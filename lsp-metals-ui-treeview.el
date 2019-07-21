;;; lsp-metals-ui-treeview.el --- Scala Client settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Darren Syzling <dsyzling@gmail.com>

;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; lsp-metals treeview ui client - handles a treeview for project tree,
;; compilation tree etc.
;; See the Metals treeview provider spec for further details:
;;  https://scalameta.org/metals/docs/editors/tree-view-protocol.html
;;
;; Current treeview interaction is:
;;   tab  key to expand/collapse nodes which is default treemacs behaviour.
;;   ret  will execute the command associated with the current node via Metals.
;;        Note you need -Dmetals.execute-client-command enabled for this to work
;;        and may require you to upgrade Metals post 0.7 for Emacs.
;;
;;   mouse left double click - will execute the command on a node.
;;
;; Metals allows classes to be expanded and the action executed on the same
;; node - metals.goto (goto definition) we can't therefore use return to
;; expand/collapse and execute actions. The existing implementation provides
;; a simple starting point to test the treeview with metals and we can evolve
;; to a Hydra like interface to provide a richer keyboard experience in future.
;;

;;; Code:

(require 'lsp-mode)
(require 'ht)
(require 'json)
(require 'dash)
(require 'treemacs)

(cl-defstruct lsp--metals-treeview-data
  (treeview-buffer nil)
  (views nil))

(defvar lsp--metals-treeview-enable nil)

(defvar-local lsp--metals-treeview-current-workspace nil
  "Associate lsp workspace with the metals treeview buffer so we can
invoke async calls to the lsp server.")

(defvar-local lsp--metals-view-id nil
  "Metals treeview id associated with the treeview buffer.")

(defconst lsp--default-metals-notification-handlers
  (ht ("metals/treeViewDidChange" #'lsp--metals-treeview-did-change)))

(defun lsp--metals-get-treeview-data (workspace)
  (gethash "metals-treeview" (lsp--workspace-metadata workspace)))

(defun lsp--metals-set-treeview-data (workspace data)
  (puthash "metals-treeview" data (lsp--workspace-metadata workspace)))

(defun lsp--metals-view-name (view-id)
  "Return a view name from the VIEW-ID."
  (replace-regexp-in-string "metals" "" view-id))

(defun lsp--metals-treeview-buffer-name (workspace view-id)
  "Return buffer name of the treeview from workspace and viewId."
  (format "*Metals %s %s*"
          (lsp--metals-view-name view-id)
          (file-name-nondirectory
           (directory-file-name (lsp--workspace-root workspace)))))

(defun lsp--metals-treemacs-path (node-uri)
  "Create a treemacs path given the node-uri which is used as the node key."
  `(:custom Build ,node-uri))

(defun lsp--metals-init-views (workspace params)
  (lsp-log "Initialising metals views for the first time %s" (lsp--workspace-root workspace))
  (let ((state (make-lsp--metals-treeview-data
                :treeview-buffer nil
                :views (mapcar
                        (lambda (node)
                          `((:view-id   .  ,(gethash "viewId" node))
                            (:view-name  .  ,(replace-regexp-in-string "metals" ""
                                                                      (gethash "viewId" node)))))
                        (gethash "nodes" params)))))
    (mapc (lambda (view-data) (lsp-log "found view %s: %s"
                                       (alist-get :view-id view-data)
                                       (alist-get :view-name view-data)))
     (lsp--metals-treeview-data-views state))
    (lsp--metals-set-treeview-data workspace state)
    (when lsp--metals-treeview-enable
      (lsp-metals-treeview-sidebar))))

(defun lsp--metals-update-node (workspace node)
  (lsp-log "in lsp--metals-update-node %s" (gethash "nodeUri" node))
  (let ((treeview-buffer-name (lsp--metals-treeview-buffer-name workspace
                                                                (gethash "viewId" node))))
    (with-current-buffer treeview-buffer-name
      (-if-let (tree-node (treemacs-find-node (lsp--metals-treemacs-path
                                               (gethash "nodeUri" node))))
          (progn
            ;; replace label in our node attached to the tree node.
            (ht-set (treemacs-button-get tree-node :node)
                    "label"
                    (ht-get node "label"))
            ;; update treeview ui - need to trigger update of parent node.
            (treemacs-do-update-node (treemacs-parent-of tree-node) t))
        (lsp-log "Failed to find node in dom")))))

(defun lsp--metals-treeview-changed (workspace params treeview-state)
  (lsp-log "treeview changed\n%s" (json-encode params))
  ;; process list of nodes that have changed
  (mapc (lambda (node)
          (lsp--metals-update-node workspace node))
        (gethash "nodes" params)))

(defun lsp--metals-treeview-did-change (workspace params)
  "Metals treeview changed notification.
Nodes that have been changed will be provided within the
PARAMS message with their viewIds.  WORKSPACE will be the current
workspace of the project."
  (lsp-log "In lsp--metals-treeview-did-change %s\n%s"
           (lsp--workspace-root workspace)
           (json-encode params))
  ;; check if metals treeview state has been stored or this is the 
  ;; first time for this project.
  (let ((state (lsp--metals-get-treeview-data workspace)))
    (lsp-log "state %s" state)
    (if (not state)
        ;; initialise state based on views sent.
        (lsp--metals-init-views workspace params)
      (lsp--metals-treeview-changed workspace params state))
    (lsp-log "state after setting %s" (lsp--metals-get-treeview-data workspace))))


;; (defun lsp--metals-send-treeview-children-sync (view-id &optional node-uri)
;;   "Query children in the view given by VIEW-ID.
;; An optional NODE-URI can be used to query children of a specific node
;; within the view. This call is synchronous and will return the response
;; from the call to metas/treeViewChildren. Under the hood lsp-request will
;; send the request asynchronously and wait for the response."
;;   (let* ((view-param `(:viewId ,view-id))
;;          (params (if node-uri
;;                      (append view-param `(:nodeUri ,node-uri))
;;                    view-param)))
;;     (lsp-request "metals/treeViewChildren" params)))

(defun lsp--metals-send-treeview-children (view-id &optional node-uri)
  "Query children in the view given by VIEW-ID.
An optional NODE-URI can be used to query children of a specific node
within the view. This call is synchronous and will return the response
from the call to metas/treeViewChildren. Under the hood lsp-request will
send the request asynchronously and wait for the response."
  (lsp-log "Sending metals/treeViewChildren")
  (lsp-request "metals/treeViewChildren"
               (append `(:viewId ,view-id)
                       (if node-uri `(:nodeUri ,node-uri) nil))))


(defun lsp--metals-send-treeview-visibility-did-change (workspace view-id visible)
  "Send metals/treeViewVisibilityDidChange to inform metals when views
are shown/hidden within the editor."
  (let ((params (list :viewId view-id
                      :visible visible)))
    (with-lsp-workspace workspace
        (lsp-request-async "metals/treeViewVisibilityDidChange" params
                           (lambda (response)
                             (lsp-log (json-encode response)))))))

(defun lsp--metals-send-treeview-node-collapse-did-change (workspace view-id node-uri collapsed?)
  "Send metals/treeViewNodeCollapseDidChange to inform Metals when a
treeview node has collapsed or expanded."
  (lsp-log "sending metals/treeViewNodeCollapseDidChange viewId %s nodeUri %s collapsed? %s"
           view-id node-uri collapsed?)
  (let ((params (list :viewId view-id
                      :nodeUri node-uri
                      :collapsed (if collapsed?
                                     t
                                   json-false))))
    (with-lsp-workspace workspace
      (lsp-request-async "metals/treeViewNodeCollapseDidChange" params
                         (lambda (response)
                           (lsp-log "metals/treeViewNodeCollapseDidChange response:\n %s"
                                    (json-encode response)))))))

(defun lsp--metals-treeview-get-children (view-id &optional node-uri)
  (with-lsp-workspace lsp--metals-treeview-current-workspace
    ;; return nodes element and convert from vector to list.
    (let ((children (ht-get (lsp--metals-send-treeview-children view-id node-uri) "nodes")))
      (lsp-log "Children returned:\n%s " (json-encode children))
      (append children nil))))

(defun lsp--metals-treeview-get-children-current-node (&rest _)
  (let ((metals-node (treemacs-button-get (treemacs-node-at-point) :node)))
    (lsp--metals-treeview-get-children (ht-get metals-node "viewId")
                                       (ht-get metals-node "nodeUri"))))
;;
;; UI tree view using treemacs
;;

(defun lsp--metals-treeview-state (item)
  ;;(lsp-log (json-encode item))
  (if (gethash "collapseState" item)
      treemacs-metals-node-closed-state
    treemacs-metals-leaf-state))

(defun lsp--metals-treeview-icon (item)
  (if (gethash "collapseState" item)
      treemacs-icon-metals-node-closed
    (treemacs-get-icon-value 'root nil "Default")))


;;TODO - refactor this to call new lsp-metals send execute function.
;; Check if node has command property and send execute to metals.
(defun lsp--metals-treeview-exec-node-action (&rest _)
  "Execute the action associated with the treeview node."
  (let* ((node (treemacs-button-get (treemacs-current-button) :node)))
    (lsp-log "exec node action %s" (json-encode node))))


(treemacs-define-leaf-node metals-leaf
  (treemacs-as-icon "• " 'face 'font-lock-builtin-face)
  :ret-action #'lsp--metals-treeview-exec-node-action
  :tab-action #'lsp--metals-treeview-exec-node-action
  :mouse1-action (lambda (&rest args)
                   (interactive)
                   (lsp--metals-treeview-exec-node-action args)))


(defun lsp--metals-on-node-collapsed (metals-node collapsed?)
  "Send metals/treeViewNodeCollapseDidChange to inform Metals
that the node has been collapsed or expanded."
  (lsp--metals-send-treeview-node-collapse-did-change lsp--metals-treeview-current-workspace
                                                      lsp--metals-view-id
                                                      (ht-get metals-node "nodeUri")
                                                      collapsed?))

;; (defun lsp--metals-toggle-node (&optional root &rest _)
;;   "Toggle collapse/expand state for metals root and
;; expandable nodes."
;;   (let* ((current-node (treemacs-current-button))
;;          (state (treemacs-button-get current-node :state)))
;;     (pcase state
;;       (`treemacs-metals-node-closed-state
;;        (lsp--metals-on-node-collapsed
;;         (treemacs-button-get (treemacs-current-button) :node) nil)
;;        (treemacs-expand-metals-node)
;;        ;;(lsp--metals-on-expand-node current-node)
;;        )
      
;;       (`treemacs-metals-node-open-state
       
;;        (lsp--metals-on-node-collapsed
;;         (treemacs-button-get (treemacs-current-button) :node) t)
;;        (treemacs-collapse-metals-node)
;;        ;;(lsp--metals-on-collapse-node current-node)
;;        )
      
;;       (`treemacs-metals-root-closed-state
;;        (treemacs-expand-metals-root)
;;        (lsp--metals-on-node-collapsed
;;         (treemacs-button-get (treemacs-current-button) :node) nil)
;;        ;;(lsp--metals-on-expand-node current-node)
;;        )
      
;;       (`treemacs-metals-root-open-state
;;        (treemacs-collapse-metals-root)
;;        (lsp--metals-on-node-collapsed
;;         (treemacs-button-get (treemacs-current-button) :node) t)
;;        ;;(lsp--metals-on-collapse-node current-node)
;;        ))))

(treemacs-define-expandable-node metals-node
  :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
  :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
  :query-function (lsp--metals-treeview-get-children-current-node)

  :ret-action 'lsp--metals-treeview-exec-node-action
  ;;:ret-action 'lsp--metals-toggle-node
  
  :after-expand (lsp--metals-on-node-collapsed
                 (treemacs-button-get node :node) nil)
  :after-collapse (lsp--metals-on-node-collapsed
                   (treemacs-button-get node :node) t)
  
  :render-action
  (treemacs-render-node
   :icon (lsp--metals-treeview-icon item)
   :label-form (gethash "label" item)
   :state treemacs-metals-node-closed-state
   ;;:state (lsp--metals-treeview-state item)
   :face 'font-lock-string-face
   :key-form (gethash "nodeUri" item)
   :more-properties (:node item :eldoc (gethash "tooltip" item))))

(treemacs-define-expandable-node metals-root
  :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
  :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
  :query-function (lsp--metals-treeview-get-children lsp--metals-view-id)

  :ret-action 'lsp--metals-treeview-exec-node-action
  
  :render-action
  (treemacs-render-node
   :icon (lsp--metals-treeview-icon item)
   :label-form (gethash "label" item)
   :state (lsp--metals-treeview-state item)
   :face 'font-lock-keyword-face
   :key-form (gethash "nodeUri" item)
   :more-properties (:node item :eldoc (gethash "tooltip" item)))
  :top-level-marker t
  :root-label (lsp--metals-view-name lsp--metals-view-id)
  :root-face 'font-lock-type-face
  :root-key-form 'Build)

;; (treemacs-define-variadic-node metals-root
;;   :query-function (progn
;;                     (lsp-log "inside query function")
;;                     (lsp--metals-treeview-get-children "metalsBuild"))
  
;;   :render-action
;;   (treemacs-render-node
;;    :icon (treemacs-get-icon-value 'root nil "Default")
;;    :label-form (gethash "label" item)
   
;;    :state (metals-treeview-state item)
;;    :key-form (gethash "label" item)
;;    :more-properties (:node item))
;;   :root-key-form 'Build)


(defun lsp--metals-show-sidebar (workspace view-id position)
  "Show or create the side window and treeview for the Metals
VIEW-ID given. WORKSPACE will be associated with the buffer
in order to invoke lsp commands/queries. The window will be
positioned as a side window by POSITION and is of the form
'((side left))"
  (let ((buffer-name (lsp--metals-treeview-buffer-name workspace (lsp--metals-view-name view-id))))
    (-if-let (buffer (get-buffer buffer-name))
        (select-window (display-buffer-in-side-window buffer position))
      (let* ((buffer (get-buffer-create buffer-name))
             (window (display-buffer-in-side-window buffer position)))

        (with-lsp-workspace workspace
          (select-window window)
          (set-window-dedicated-p window t)
          (treemacs-initialize)
          
          (setq-local lsp--metals-treeview-current-workspace workspace)
          (setq-local lsp--metals-view-id view-id)
          (treemacs-METALS-ROOT-extension))))))


(defun lsp--metals-display-views (workspace views slot)
  "Display each view returned by Metals in our sidebar."
  (when-let (view (car views))
      (lsp--metals-show-sidebar workspace
                                (alist-get :view-id view)
                                `((side . left) (slot . ,slot)))
      (lsp--metals-display-views workspace (cdr views) (+ 1 slot))
      ;; notify metals that the view has been displayed.
      (lsp--metals-send-treeview-visibility-did-change workspace (alist-get :view-id view) t)))

(defun lsp-metals-treeview-sidebar (&optional workspace)
  "Display the Metals treeview sidebar window."
  (interactive)
  (let* ((workspace (or workspace (car (lsp-workspaces))))
         (state (lsp--metals-get-treeview-data workspace)))
    ;; TODO select position from config for view id.
    (lsp--metals-display-views workspace
                               (lsp--metals-treeview-data-views
                               (lsp--metals-get-treeview-data workspace))
                               0)))

;; Testing
;; (progn
;;   (switch-to-buffer "build.sbt")
;;   (lsp-metals-treeview-sidebar))

;; (progn
;;   (switch-to-buffer "build.sbt")
;;   (vectorp (lsp--metals-treeview-get-children "metalsBuild")))

;; (progn
;;   (switch-to-buffer "build.sbt")
;;   (mapc (lambda (workspace)
;;           (mapc (lambda (buffer)
;;                   (lsp-log (buffer-name buffer)))
;;                 (lsp--workspace-buffers workspace)))
;;         (lsp-workspaces))
;;   nil)

;; (progn
;;   (switch-to-buffer "build.sbt")
;;   (lsp--metals-treeview-get-children "metalsBuild"))

;; sync
;; (progn
;;   (switch-to-buffer "build.sbt")
;;   (let ((response (lsp--metals-treeview-get-children "metalsBuild")))
;;     (lsp-log "*** response from synchronous call")
;;     (lsp-log (json-encode response))))

;; (progn
;;   (switch-to-buffer "build.sbt")
;;   (let ((response (lsp--metals-treeview-get-children "metalsBuild")))
;;     (mapc (lambda (item)
;;             (lsp-log (gethash "label" item)))
;;           response)
;;     (lsp-log "*** response from synchronous call")
;;     (lsp-log (json-encode response))))

;; (progn
;;   (switch-to-buffer "build.sbt")
;;   (let ((response (lsp--metals-treeview-get-children "metalsBuild" "projects:")))
;;     (lsp-log "*** response from synchronous call")
;;     (lsp-log (json-encode response))))

;; Debug helpers to track down issues with treemacs and aid development.
(defun lsp-metals-treemacs--debug-node ()
  (interactive)
  (-let [node (treemacs-node-at-point)]
    (message
     "Label: %s
Depth: %s
Key: %s
Path: %s
State: %s
Parent: %s
eldoc: %s
Metals Item: %s"
     (treemacs--get-label-of node)
     (treemacs-button-get node :depth)
     (treemacs-button-get node :key)
     (treemacs-button-get node :path)
     (treemacs-button-get node :state)
     (-some-> node (treemacs-button-get :parent) (treemacs--get-label-of))
     (treemacs-button-get node :eldoc)
     (-some-> node (treemacs-button-get :node)))))
(global-set-key (kbd "C-x C-ö") #'treemacs-mu4e-debug-node)


(provide 'lsp-metals-ui-treeview)
;;; lsp-metals-ui-treeview.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

