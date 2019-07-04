;;; lsp-metals.el --- Scala Client settings             -*- lexical-binding: t; -*-

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

(cl-defstruct lsp--metals-treeview-data
  (treeview-buffer nil)
  (views nil))

(defconst lsp--default-metals-notification-handlers
  (ht ("metals/treeViewDidChange" #'lsp--metals-treeview-did-change)))

(defun lsp--metals-get-treeview-data (workspace)
  (lsp-workspace-get-metadata "metals-treeview" workspace))

(defun lsp--metals-set-treeview-data (workspace data)
  (lsp-workspace-set-metadata "metals-treeview" data workspace))

(defun lsp--metals-init-views (workspace params)
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
    (lsp--metals-set-treeview-data workspace state)))

(defun lsp--metals-treeview-changed (workspace params treeview-state)
  (lsp-log "TODO handle treeview changed"))

(defun lsp--metals-treeview-did-change (workspace params)
  "Metals treeview changed notification.
Nodes that have been changed will be provided within the
PARAMS message with their viewIds.  WORKSPACE will be the current
workspace of the project."
  (lsp-log "In lsp--metals-treeview-did-change")
  (lsp-log (json-encode params))
  ;; check if metals treeview state has been stored or this is the 
  ;; first time for this project.
  (let (state (lsp--metals-get-treeview-data workspace))
    (if (not state)
        ;; initialise state based on views sent.
        (lsp--metals-init-views workspace params)
      (lsp--metals-treeview-changed workspace params state))))

(defun lsp--metals-send-treeview-children(view-id &optional node-uri)
  "Query children in the view given by VIEW-ID.
An optional NODE-URI can be used to query children of a specific node
within the view. This call is asynchronous, the metals method sent
in this async request is metals/treeViewChildren"
  (let* ((view-param `(:viewId ,view-id))
         (params (if node-uri
                     (append view-param `(:nodeUri ,node-uri))
                   view-param)))
    (lsp-request-async "metals/treeViewChildren" params
                        (lambda (response)
                          (lsp-log "Return from metals/treeViewChildren")
                          (lsp-log (json-encode response))))))


(defun lsp-metals-treeview-sidebar ()
  "Display the Metals treeview sidebar window."
  (interactive)
  ;; TODO)

;; Testing
;; (progn
;;   (switch-to-buffer "build.sbt")
;;   (lsp--metals-send-treeview-children "metalsBuild"))

;; (progn
;;   (switch-to-buffer "build.sbt")
;;   (lsp--metals-send-treeview-children "metalsCompile"))


(provide 'lsp-metals-ui-treeview)
;;; lsp-metals-ui-treeview.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

