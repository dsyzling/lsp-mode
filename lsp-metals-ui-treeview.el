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
;; expand/collapse and execute actions.  The existing implementation provides
;; a simple starting point to test the treeview with metals and we can evolve
;; to a Hydra like interface to provide a richer keyboard experience in future.
;;

;;; Code:

(require 'lsp-mode)
(require 'ht)
(require 'json)
(require 'dash)
(require 'treemacs)

(defcustom lsp-metals-treeview-enable t
  "Enable Metals treeview extension - send capability to Metals to indicate we want treeview messages."
  :group 'lsp-metals-treeview
  :type 'boolean)

(defcustom lsp-metals-treeview-show-when-views-received t
  "Automatically show the treeview when Metals sends us the list of views (compile/build). Otherwise if nil the user will have to execute lsp-metals-treeview to display the treeview explicitly."
  :group 'lsp-metals-treeview
  :type 'boolean)

(defcustom lsp-metals-treeview-logging t
  "If non nil log treeview trace/debug messages to the lsp-log for debugging."
  :group 'lsp-metals-treeview
  :type 'boolean)

(cl-defstruct lsp--metals-treeview-data
  (views nil))

(defvar-local lsp--metals-treeview-current-workspace nil
  "Associate lsp workspace with the metals treeview buffer so we can
invoke async calls to the lsp server.")

(defvar-local lsp--metals-view-id nil
  "Metals treeview id associated with the treeview buffer.")

(defun lsp--metals-treeview-log (format &rest args)
  "Log treeview tracing/debug messages to the lsp-log"
  (when lsp-metals-treeview-logging
    (apply #'lsp-log format args)))

(defun lsp--metals-treeview-get-data (workspace)
  (ht-get (lsp--workspace-metadata workspace) "metals-treeview"))

(defun lsp--metals-treeview-set-data (workspace data)
  (ht-set (lsp--workspace-metadata workspace) "metals-treeview" data))

(defun lsp--metals-view-name (view-id)
  "Return a view name from the VIEW-ID."
  (replace-regexp-in-string "metals" "" view-id))

(defun lsp--metals-treeview-log-state (view-state)
  "Log details of the views sent to us from Metals."
  (lsp--metals-treeview-log "Views received from Metals:")
  (mapc (lambda (view-data)
          (lsp--metals-treeview-log "%s: %s"
                                    (alist-get :view-id view-data)
                                    (alist-get :view-name view-data)))
        (lsp--metals-treeview-data-views view-state)))

(defun lsp--metals-treeview-buffer-name (workspace view-id)
  "Return buffer name of the treeview from WORKSPACE and VIEW-ID."
  (format "*Metals %s %s*"
          (lsp--metals-view-name view-id)
          (file-name-nondirectory
           (directory-file-name (lsp--workspace-root workspace)))))

(defun lsp--metals-treeview-waiting-message-buffer-name (workspace)
  "Return the buffer name of a temporary buffer displaying a message
informing the user that Metals has not sent any treeview information for
this WORKSPACE.  When the views arrive this buffer will be removed and
replaced with the treeviews."
  (format "*Metals %s*" (file-name-nondirectory
                         (directory-file-name (lsp--workspace-root workspace)))))

(defun lsp--metals-get-treeview-buffer-names (workspace)
  "Return the treeview buffer names associated with this WORKSPACE."
  (let* ((view-data (lsp--metals-treeview-get-data workspace))
         (views (lsp--metals-treeview-data-views view-data)))
    (-map (lambda (view)
            (lsp--metals-treeview-buffer-name workspace (alist-get :view-id view)))
          views)))

(defun lsp--metals-treeview-get-buffers (workspace)
  "Return buffers for the metals treeview associated with the WORKSPACE."
  (-map (lambda (name) (get-buffer name))
        (lsp--metals-get-treeview-buffer-names workspace)))

(defun lsp--metals-treeview-visible? (workspace)
  "Is the Metals treeview (or any of its buffers) visible in this WORKSPACE?"
  (-any (lambda (buffer) (buffer-live-p buffer))
        (lsp--metals-treeview-get-buffers workspace)))

(defun lsp--metals-treeview-delete-window (&optional workspace)
  "Delete the metals treeview window associated with the WORKSPACE.
If WORKSPACE is not provided the current treeview buffer local variable WORKSPACE
will be used."
  (interactive)
  (let ((cur-workspace (or workspace lsp--metals-treeview-current-workspace)))
    (-map (lambda (treeview-buffer)
            (switch-to-buffer treeview-buffer)
            ;; Tell metals the view is no longer visible.
            (when lsp--metals-view-id
              (lsp--metals-send-treeview-visibility-did-change lsp--metals-treeview-current-workspace
                                                               lsp--metals-view-id
                                                               nil))
            (kill-buffer treeview-buffer))
          (lsp--metals-treeview-get-buffers cur-workspace))))

;;
;; Minor mode for metals treeview window and keymap to control
;; functions such as closing window.
;;

(defvar lsp-metals-treeview-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") #'lsp--metals-treeview-delete-window)
    m)
  "Keymap for `lsp-metals-treeview-mode'.")

(define-minor-mode lsp-metals-treeview-mode "LSP Metals Treeview minor mode"
  nil nil nil
  :keymap lsp-metals-treeview-mode-map
  :group 'lsp-metals-treeview)


(defun lsp--metals-treemacs-path (node-uri)
  "Create a treemacs path given the NODE-URI which is used as the node key."
  `(:custom Build ,node-uri))

(defun lsp--metals-show-view (workspace view-id position)
  "Show or create the side window and treeview for the Metals VIEW-ID
within the current WORKSPACE.  The window will be positioned as a side
window by POSITION and is of the form '((side left))."
  (let ((buffer-name (lsp--metals-treeview-buffer-name workspace
                                                       (lsp--metals-view-name view-id))))
    ;; When opening or refreshing the view do temporarily switch focus but restore
    ;; after window has been created. User will then not be diverted away from their
    ;; current focus..
    (-if-let (buffer (get-buffer buffer-name))
        (with-selected-window (display-buffer-in-side-window buffer position)
          ;; update the root of the tree with the view.
          (lsp--metals-treeview-log "Refreshing tree %s" view-id)
          (treemacs-update-node '(:custom Build) t))
      (let* ((buffer (get-buffer-create buffer-name))
             (window (display-buffer-in-side-window buffer position)))

        (with-lsp-workspace workspace
          (with-selected-window window
            ;;(select-window window)
            (set-window-dedicated-p window t)
            (treemacs-initialize)
            
            (setq-local lsp--metals-treeview-current-workspace workspace)
            (setq-local lsp--metals-view-id view-id)
            (treemacs-METALS-ROOT-extension)
            (setq-local mode-line-format (lsp--metals-view-name view-id))
            ;; When closing other windows after splitting, prevent our treeview closing.
            (set-window-parameter window 'no-delete-other-windows t)
            (lsp-metals-treeview-mode 1)
            ;; open root of tree after initialisation.
            (treemacs-expand-metals-root)))))))

(defun lsp--metals-get-waiting-message-buffer (workspace)
  (get-buffer (lsp--metals-treeview-waiting-message-buffer-name workspace)))

(defun lsp--metals-show-waiting-message (workspace position)
  (let* ((buffer-name (lsp--metals-treeview-waiting-message-buffer-name workspace))
         (buffer (get-buffer buffer-name)))
    (if (not buffer)
        (let* ((buffer (get-buffer-create buffer-name))
               (window (display-buffer-in-side-window buffer position)))
          (set-window-dedicated-p window t)
          (with-current-buffer buffer
            (insert "Waiting for Metals Treeview information...")
            (read-only-mode))))))

(defun lsp--metals-display-views (workspace views slot)
  "Recursive function to display each view in VIEWS in the
side window based based on an increasing SLOT number position."
  (when-let ((view (car views)))
    (lsp--metals-show-view workspace
                           (alist-get :view-id view)
                           `((side . left) (slot . ,slot)))
    (lsp--metals-send-treeview-visibility-did-change workspace (alist-get :view-id view) t)
    (lsp--metals-display-views workspace (cdr views) (+ 1 slot))))

(defun lsp--metals-treeview-select-window (workspace)
  "Switch focus to the treeview window, select the first
view/buffer in the treeview window."
  (select-window (get-buffer-window
                  (car (lsp--metals-treeview-get-buffers workspace)))))

(defun lsp--metals-show-metals-views (workspace views slot &optional select-treeview-window)
  "Display each view returned by Metals in our sidebar treeview window.
Views are displayed for this WORKSPACE, VIEWS is a list of alist containing
the views taken from the lsp--metals-treeview-data structure. SLOT is a
numeric position starting from 0 where the treeview will be positioned
relative to the others. "
  (if (not (null views))
      (progn
        (lsp--metals-display-views workspace views slot)
        
        (-if-let (buffer (lsp--metals-get-waiting-message-buffer workspace))
            (kill-buffer buffer))

        (if select-treeview-window
            (lsp--metals-treeview-select-window workspace)))
    
    ;; No views are available - show temp message.
    (lsp--metals-show-waiting-message workspace `((side . left) (slot . ,slot)))))

(defun lsp--metals-treeview-refresh (workspace params)
  "Top level treeview changed - Metals has potentially given
us a new set of views."
  (lsp--metals-treeview-log "Received metals views for workspace %s"
                            (lsp--workspace-root workspace))
  (let ((state (make-lsp--metals-treeview-data
                :views (mapcar
                        (lambda (node)
                          `((:view-id    .  ,(ht-get node "viewId"))
                            (:view-name  .  ,(replace-regexp-in-string "metals" ""
                                                                       (ht-get node "viewId")))))
                        (ht-get params "nodes")))))
    
    (lsp--metals-treeview-log-state state)
    (lsp--metals-treeview-set-data workspace state)

    ;; Update views if treeview enabled or the user has decided to show the treeview
    (when (or lsp-metals-treeview-show-when-views-received
              (lsp--metals-treeview-visible? workspace))
      (lsp--metals-show-metals-views workspace
                                     (and state (lsp--metals-treeview-data-views state))
                                     0))))

(defun lsp--metals-treeview-update-node (workspace node)
  (lsp--metals-treeview-log "in lsp--metals-treeview-update-node %s" (ht-get node "nodeUri"))
  (let ((treeview-buffer-name (lsp--metals-treeview-buffer-name workspace
                                                                (ht-get node "viewId"))))
    (with-current-buffer treeview-buffer-name
      (-if-let (tree-node (treemacs-find-node (lsp--metals-treemacs-path
                                               (ht-get node "nodeUri"))))
          (progn
            ;; replace label in our node attached to the tree node.
            (ht-set (treemacs-button-get tree-node :node)
                    "label"
                    (ht-get node "label"))
            ;; update treeview ui - need to trigger update of parent node.
            (treemacs-do-update-node (treemacs-parent-of tree-node) t)
            ;;(treemacs-do-update-node tree-node nil)
            )
        (lsp--metals-treeview-log "Failed to find node in dom")))))

(defun lsp--metals-treeview-changed (workspace params)
  "The treeview nodes have changed, update our treemacs tree."
  (lsp--metals-treeview-log "treeview changed\n%s" (json-encode params))
  ;; process list of nodes that have changed
  (mapc (lambda (node)
          (lsp--metals-treeview-update-node workspace node))
        (ht-get params "nodes")))

(defun lsp--metals-views-update-message? (params)
  "When metals updates the views (build/compile) or sends us their initial
definition for it will contain a list with viewIds without any nodeUris.
PARAMS contains the hashtable of view definitions under the 'nodes' key."
  (-all? (lambda (node)
           (and (ht-get node "viewId") (not (ht-get node "nodeUri"))))
         (append (ht-get params "nodes") nil)))

(defun lsp--metals-treeview-did-change (workspace params)
  "Metals treeview changed notification.
Nodes that have been changed will be provided within the
PARAMS message with their viewIds.  WORKSPACE will be the current
workspace of the project."
  (lsp--metals-treeview-log "In lsp--metals-treeview-did-change %s\n%s"
                            (lsp--workspace-root workspace)
                            (json-encode params))
  (save-excursion
    (if (lsp--metals-views-update-message? params)
        (lsp--metals-treeview-refresh workspace params)
      (lsp--metals-treeview-changed workspace params))))

(defun lsp--metals-send-treeview-children (view-id &optional node-uri)
  "Query children in the view given by VIEW-ID.
An optional NODE-URI can be used to query children of a specific node
within the view.  This call is synchronous and will return the response
from the call to metas/treeViewChildren. Under the hood LSP-REQUEST will
send the request asynchronously and wait for the response."
  (lsp--metals-treeview-log "Sending metals/treeViewChildren")
  (lsp-request "metals/treeViewChildren"
               (append `(:viewId ,view-id)
                       (if node-uri `(:nodeUri ,node-uri) nil))))


(defun lsp--metals-send-treeview-visibility-did-change (workspace view-id visible?)
  "Send metals/treeViewVisibilityDidChange to inform metals when views
are shown/hidden within the editor. WORKSPACE is the current lsp workspace,
VIEW-ID is the view for which the visibility has changed described by the boolean
value VISIBLE - t or nil."
  (lsp--metals-treeview-log "view visibility changed %s %s" view-id visible?)
  (let ((params (list :viewId view-id
                      :visible visible?)))
    (with-lsp-workspace workspace
        (lsp-request-async "metals/treeViewVisibilityDidChange" params
                           (lambda (response)
                             (lsp--metals-treeview-log (json-encode response)))))))

(defun lsp--metals-send-treeview-node-collapse-did-change (workspace view-id node-uri collapsed?)
  "Send metals/treeViewNodeCollapseDidChange to inform Metals when a
treeview node has collapsed or expanded.  WORKSPACE is the current workspace,
VIEW-ID the id of the view containing the node with NODE-URI which has been
collapsed or expanded based on the boolean COLLAPSED? either t or nil."
  (lsp--metals-treeview-log "sending metals/treeViewNodeCollapseDidChange viewId %s nodeUri %s collapsed? %s"
                            view-id node-uri collapsed?)
  (let ((params (list :viewId view-id
                      :nodeUri node-uri
                      :collapsed (if collapsed?
                                     t
                                   json-false))))
    (with-lsp-workspace workspace
      (lsp-request-async "metals/treeViewNodeCollapseDidChange" params
                         (lambda (response)
                           (lsp--metals-treeview-log "metals/treeViewNodeCollapseDidChange response:\n %s"
                                                     (json-encode response)))))))

(defun lsp--metals-treeview-get-children (view-id &optional node-uri)
  "Retrieve children of the view given by the VIEW-ID and optionally children
of the node given by the NODE-URI.  Without a NODE-URI the top level child items
will be returned for the view. Returns a list of nodes with values converted
from json to hash tables."
  (with-lsp-workspace lsp--metals-treeview-current-workspace
    ;; return nodes element and convert from vector to list.
    (let ((children (ht-get (lsp--metals-send-treeview-children view-id node-uri) "nodes")))
      (lsp--metals-treeview-log "Children returned:\n%s " (json-encode children))
      (append children nil))))

(defun lsp--metals-treeview-get-children-current-node (&rest _)
  "Retrieve children of the currently selected node in the treeview - see
LSP--METALS-TREEVIEW-GET-CHILDREN."
  (let ((metals-node (treemacs-button-get (treemacs-node-at-point) :node)))
    (lsp--metals-treeview-get-children (ht-get metals-node "viewId")
                                       (ht-get metals-node "nodeUri"))))
;;
;; UI tree view using treemacs
;;

(defun lsp--metals-treeview-state (item)
  (if (ht-get item "collapseState")
      treemacs-metals-node-closed-state
    treemacs-metals-leaf-state))

(defun lsp--metals-treeview-icon (item)
  (if (ht-get item "collapseState")
      treemacs-icon-metals-node-closed
    (treemacs-get-icon-value 'root nil "Default")))

(defun lsp--metals-treeview-exec-node-action (&rest _)
  "Execute the action associated with the treeview node."
  (let* ((node (treemacs-button-get (treemacs-current-button) :node))
         (command (ht-get node "command")))
    (when command
      (with-lsp-workspace lsp--metals-treeview-current-workspace
        ;; Current lsp-send-execute-command is synchronous,
        ;; use our own async call.
        (lsp-request-async "workspace/executeCommand"
                           (list :command (string-remove-prefix "metals."(ht-get command "command"))
                                 :arguments (ht-get command "arguments"))
                           (lambda (response)
                             (lsp--metals-treeview-log "reply from workspace/executeCommand:\n%s"
                                                       (json-encode response))))))))


(treemacs-define-leaf-node metals-leaf
  (treemacs-as-icon "• " 'face 'font-lock-builtin-face)
  :ret-action #'lsp--metals-treeview-exec-node-action
  :tab-action #'lsp--metals-treeview-exec-node-action
  :mouse1-action (lambda (&rest args)
                   (interactive)
                   (lsp--metals-treeview-exec-node-action args)))


(defun lsp--metals-on-node-collapsed (metals-node collapsed?)
  "Send metals/treeViewNodeCollapseDidChange to inform Metals
that the node has been collapsed or expanded. METALS-NODE is a hash table
describing the metals node attached to treemacs in the :node key - passed as
item during render. COLLAPSED? either t or nil dependong on if the node has been
collapsed or expanded."
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

;;
;; Expandable node definition in the treemacs tree.
;; Can have an action associated with it - e.g. a class
;; with goto definition, or be a class that can be expanded
;; to show fields, functions etc.
;; Tab expands expandable nodes, return executes the action
;; on the node - although we will change this in future with
;; a keymap or hydra interface to allow more actions.
;;

(treemacs-define-expandable-node metals-node
  :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
  :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
  :query-function (lsp--metals-treeview-get-children-current-node)

  :ret-action 'lsp--metals-treeview-exec-node-action

  :after-expand (lsp--metals-on-node-collapsed
                 (treemacs-button-get node :node) nil)
  :after-collapse (lsp--metals-on-node-collapsed
                   (treemacs-button-get node :node) t)
  
  :render-action
  (treemacs-render-node
   :icon (lsp--metals-treeview-icon item)
   :label-form (ht-get item "label")
   :state treemacs-metals-node-closed-state
   ;;:state (lsp--metals-treeview-state item)
   :face 'font-lock-string-face
   :key-form (ht-get item "nodeUri")
   :more-properties (:node item :eldoc (ht-get item "tooltip"))))

;;
;; Root node of Metals treeview, in the first release this is either the
;; Build or Compile tree.
;; Currently disable return action for the root node. Tab expands root nodes
;; and expandable nodes.
;;

(treemacs-define-expandable-node metals-root
  :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
  :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
  :query-function (lsp--metals-treeview-get-children lsp--metals-view-id)

  ;; Ignore return action on root.
  :ret-action '(lambda(&rest _))
  
  :render-action
  (treemacs-render-node
   :icon (lsp--metals-treeview-icon item)
   :label-form (ht-get item "label")
   :state (lsp--metals-treeview-state item)
   :face 'font-lock-keyword-face
   :key-form (ht-get item "nodeUri")
   :more-properties (:node item :eldoc (ht-get item "tooltip")))
  :top-level-marker t
  :root-label (lsp--metals-view-name lsp--metals-view-id)
  :root-face 'font-lock-type-face
  :root-key-form 'Build)

(defun lsp-metals-treeview (&optional workspace)
  "Display the Metals treeview window for the WORKSPACE (optional).  If
WORKSPACE is not specified obtain the current workspace for the file in
the current buffer."
  (interactive)
  (-if-let* ((workspace (or workspace (car (lsp-workspaces)))))
      (let* ((view-state (lsp--metals-treeview-get-data workspace))
             (views (and view-state (lsp--metals-treeview-data-views view-state))))
        (lsp--metals-show-metals-views workspace views 0 t))
    (message "Current buffer is not within Metals workspace")))


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

