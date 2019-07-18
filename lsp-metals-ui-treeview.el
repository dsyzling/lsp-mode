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
      (lsp-log "looking for %s in treeview buffer %s"
               (gethash "nodeUri" node)
               treeview-buffer-name)
      (if (treemacs-find-in-dom (gethash "nodeUri" node))
          (lsp-log "found node in dom")
        (lsp-log "Failed to find node in dom"))
      
        ;; (treemacs-button-put (treemacs-find-in-dom (gethash "nodeUri" node))
        ;;                      :label
        ;;                      (gethash "label" node))
        
        )))

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

;; TODO
;; consider removing and using the synchronous call.
;;
;; (defun lsp--metals-send-treeview-children (view-id &optional node-uri)
;;   "Query children in the view given by VIEW-ID.
;; An optional NODE-URI can be used to query children of a specific node
;; within the view. This call is asynchronous, the metals method sent
;; in this async request is metals/treeViewChildren"
;;   (let* ((view-param `(:viewId ,view-id))
;;          (params (if node-uri
;;                      (append view-param `(:nodeUri ,node-uri))
;;                    view-param)))
;;     (lsp-request-async "metals/treeViewChildren" params
;;                         (lambda (response)
;;                           (lsp-log "Return from metals/treeViewChildren")
;;                           (lsp-log (json-encode response))))))

(defun lsp--metals-send-treeview-children-sync (view-id &optional node-uri)
  "Query children in the view given by VIEW-ID.
An optional NODE-URI can be used to query children of a specific node
within the view. This call is synchronous and will return the response
from the call to metas/treeViewChildren. Under the hood lsp-request will
send the request asynchronously and wait for the response."
  (let* ((view-param `(:viewId ,view-id))
         (params (if node-uri
                     (append view-param `(:nodeUri ,node-uri))
                   view-param)))
    (lsp-request "metals/treeViewChildren" params)))

(defun lsp--metals-send-treeview-visibility-did-change (workspace view-id visible)
  "Send metals/treeViewVisibilityDidChange to inform metals when views
are shown/hidden within the editor."
  (let ((params (list :viewId view-id
                      :visible visible)))
    (with-lsp-workspace workspace
        (lsp-request-async "metals/treeViewVisibilityDidChange" params
                           (lambda (response)
                             (lsp-log (json-encode response)))))))

(defun lsp--metals-treeview-get-children (view-id &optional node-uri)
  (lsp-log "**in lsp--metals-treeview-get-children")
  (with-lsp-workspace lsp--metals-treeview-current-workspace
    (let ((response (lsp--metals-send-treeview-children-sync view-id node-uri)))
      ;; return nodes element and convert from vector to list.
      (append (gethash "nodes" response) nil))))

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

(defun lsp--metals-treeview-query-children (&rest _)
  (lsp-log (json-encode (treemacs-button-get (treemacs-node-at-point) :node)))
  (let* ((node (treemacs-button-get (treemacs-node-at-point) :node))
         (view-id  (gethash "viewId" node))
         (node-uri (gethash "nodeUri" node)))
    (with-lsp-workspace lsp--metals-treeview-current-workspace
      (lsp--metals-treeview-get-children view-id node-uri))))

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

(defun lsp--metals-toggle-metals-node (&rest _)
  (let ((state (treemacs-button-get (treemacs-current-button) :state)))
    (if (eq treemacs-metals-node-closed-state state)
        (treemacs-expand-metals-node))
    (treemacs-collapse-metals-node)))

(treemacs-define-expandable-node metals-node
  :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
  :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
  :query-function (lsp--metals-treeview-query-children)

  ;;:ret-action 'lsp--metals-toggle-metals-node
  
  :render-action
  (treemacs-render-node
   :icon (lsp--metals-treeview-icon item)
   :label-form (gethash "label" item)
   :state treemacs-metals-node-closed-state
   ;;:state (lsp--metals-treeview-state item)
   :face 'font-lock-string-face
   :key-form (gethash "nodeUri" item)
   :more-properties (:node item)))

(treemacs-define-expandable-node metals-root
  :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
  :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
  :query-function (lsp--metals-treeview-get-children lsp--metals-view-id)
  
  :render-action
  (treemacs-render-node
   :icon (lsp--metals-treeview-icon item)
   :label-form (gethash "label" item)
   :state (lsp--metals-treeview-state item)
   :face 'font-lock-keyword-face
   :key-form (gethash "nodeUri" item)
   :more-properties (:node item))
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
;;   (setq method-reqs lsp-method-requirements))

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

;; (progn
;;   (switch-to-buffer "build.sbt")
;;   (lsp--metals-send-treeview-children "metalsBuild"))

;; (progn
;;   (switch-to-buffer "build.sbt")
;;   (lsp--metals-send-treeview-children "metalsCompile"))

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


(provide 'lsp-metals-ui-treeview)
;;; lsp-metals-ui-treeview.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

