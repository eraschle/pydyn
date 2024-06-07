;;; pydyn-dynamo.el --- Dynamo BIM Emacs package -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Erich Raschle
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; This module provides minor mode `pydyn-dynamo'
;;
;;; Code:

(require 'pydyn-utils)
(require 'pydyn-python)

(require 'view)

(defcustom pydyn-dynamo-keymap-prefix "C-d"
  "The prefix for pydyn-dynamo-mode key bindings."
  :type 'string
  :group 'pydyn)


(defun pydyn-dynamo-key (key)
  "Return Emacs key representation of KEY."
  (pydyn-key pydyn-dynamo-keymap-prefix key))


(defun pydyn-dynamo--mode-map-create ()
  "Define mode keymap."
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (pydyn-dynamo-key "m") #'pydyn-dynamo-mode-on)
    (define-key key-map (pydyn-dynamo-key "M") #'pydyn-dynamo-mode-off)
    (define-key key-map (pydyn-dynamo-key "n") #'pydyn-dynamo-at-point-to-python)
    (define-key key-map (pydyn-dynamo-key "s") #'pydyn-dynamo-script-to-python)
    (define-key key-map (pydyn-dynamo-key "S") #'pydyn-dynamo-folder-to-python)
    (define-key key-map (pydyn-dynamo-key "j") #'pydyn-dynamo-jump-to-node)
    (define-key key-map (pydyn-dynamo-key "g") #'pydyn-dynamo-goto-python)
    (define-key key-map (pydyn-dynamo-key "p") #'pydyn-dynamo-python-code-preview)
    (define-key key-map (pydyn-dynamo-key "o") #'pydyn-dynamo-clean-orphan-code-file)
    (define-key key-map (pydyn-dynamo-key "O") #'pydyn-dynamo-clean-orphan-code-folder)
    (define-key key-map (pydyn-dynamo-key "x") #'pydyn-dynamo-file-cache-reset)
    (define-key key-map (pydyn-dynamo-key "X") #'pydyn-dynamo-buffer-cache-reset)
    key-map))


(defvar pydyn-dynamo-mode-map (pydyn-dynamo--mode-map-create)
  "Keymap for dynpy minor mode.")

(add-to-list 'minor-mode-alist '(pydyn-dynamo-mode " pydyn-dynamo"))
(add-to-list 'minor-mode-map-alist (cons 'pydyn-dynamo-mode pydyn-dynamo-mode-map));;


(defcustom pydyn-dynamo-indent-width nil
  "Indent width for `json-mode' in open Dynamo file."
  :type 'integer
  :group 'pydyn)


(defun pydyn-dynamo-indent-width-setup ()
  "Set indent with in current buffer."
  (when pydyn-dynamo-indent-width
    (pydyn-indent-width-set pydyn-dynamo-indent-width)))


(defun pydyn-dynamo--node-info-or-error ()
  "Switch to Python file of code at current point if exists."
  (let ((node-info (pydyn-python-node-get)))
    (unless node-info
      (user-error "Current point is NOT inside a PYTHON node!!!"))
    node-info))


;;;###autoload
(defun pydyn-dynamo-goto-python ()
  "Switch to Python file of code at current point if exists."
  (interactive)
  (pydyn-is-dynamo-or-error)
  (let ((path (pydyn-export-path
               (pydyn-dynamo--node-info-or-error))))
    (unless (file-exists-p path)
      (user-error "File %s does not exists in %s"
                  (file-name-base path)
                  (file-name-parent-directory path)))
    (switch-to-buffer-other-window (pydyn-buffer-by path))))


;;;###autoload
(defun pydyn-dynamo-python-code-preview ()
  "Jump to selected node in current buffer."
  (interactive)
  (let ((buffer (pydyn-preview-buffer
                 (pydyn-dynamo--node-info-or-error))))
    (switch-to-buffer-other-window
     (with-current-buffer buffer
       (view-mode-exit)
       (if (not node-export)
           (revert-buffer nil t t)
         (python-mode)
         (ensure-empty-lines 1))
       (view-mode-enter)
       (current-buffer)))))


;;;###autoload
(defun pydyn-dynamo-at-point-to-python (switch-or-kill)
  "Export python code at point and SWITCH-OR-KILL export buffer."
  (interactive (list (pydyn-choose-switch-or-kill "Python")))
  (pydyn-is-dynamo-or-error)
  (unwind-protect
      (let ((node-info (pydyn-dynamo--node-info-or-error))
            (switch (pydyn-is-switch switch-or-kill))
            (other-win (pydyn-is-switch-other switch-or-kill))
            (kill (pydyn-is-kill switch-or-kill)))
        (pydyn-disable-lsp-clients)
        (pydyn-buffer-save (pydyn-convert-node-to-python
                            node-info 'pydyn-python-convert-clean)
                           switch other-win kill))
    (pydyn-enable-lsp-clients)
    (when (or (pydyn-is-switch switch-or-kill)
              (pydyn-is-switch-other switch-or-kill))
      (pydyn-python-mode-on))))


;;;###autoload
(defun pydyn-dynamo-script-to-python (file-path switch-or-kill)
  "Export python node in FILE-PATH and SWITCH-OR-KILL to export buffer."
  (interactive (list (pydyn-selection-get
                      (pydyn-dynamo-files-in pydyn-source-root t)
                      "Select Dynamo file:" pydyn-source-root)
                     (pydyn-choose-switch-or-kill "Python")))
  (pydyn-is-dynamo-or-error file-path)
  (unwind-protect
      (progn
        (pydyn-disable-lsp-clients)
        (let ((buffer (pydyn-convert-to-python
                       file-path 'pydyn-python-convert-clean))
              (switch (pydyn-is-switch switch-or-kill))
              (other-win (pydyn-is-switch-other switch-or-kill))
              (kill (pydyn-is-kill switch-or-kill)))
          (pydyn-buffer-save buffer switch other-win kill)))
    (pydyn-enable-lsp-clients)
    (when (or (pydyn-is-switch switch-or-kill)
              (pydyn-is-switch-other switch-or-kill))
      (pydyn-python-mode-on))))


;;;###autoload
(defun pydyn-dynamo-folder-to-python (&optional directory switch-or-kill)
  "Export all python nodes of Dynamo files in DIRECTORY.
SWITCH-OR-KILL last export buffer afterwards."
  (interactive (list (read-directory-name
                      "Export Python code to directory? "
                      pydyn-source-root)
                     (pydyn-choose-switch-or-kill "Python")))
  (unwind-protect
      (let ((buffer nil)
            (switch (pydyn-is-switch switch-or-kill))
            (other-win (pydyn-is-switch-other switch-or-kill))
            (kill (pydyn-is-kill switch-or-kill)))
        (pydyn-disable-lsp-clients)
        (dolist (file-path (pydyn-dynamo-files-in directory))
          (when (pydyn-is-dynamo? file-path)
            (when buffer (pydyn-buffer-save buffer nil nil t))
            (setq buffer (pydyn-convert-to-python
                          file-path 'pydyn-python-convert-clean))))
        (pydyn-buffer-save buffer switch other-win kill))
    (pydyn-enable-lsp-clients)
    (when (or (pydyn-is-switch switch-or-kill)
              (pydyn-is-switch-other switch-or-kill))
      (pydyn-python-mode-on))))


(defun pydyn-dynamo--node-select-of (node-info)
  "Return NODE-INFO value used for `completing-read'."
  (let ((uuid (plist-get node-info :node-id))
        (name (plist-get node-info :name)))
    (format "%-50s %s" name
            (propertize uuid 'face 'marginalia-documentation))))


(defun pydyn-dynamo--node-selections (node-infos)
  "Return list with name and node-id for all nodes in NODE-INFOS."
  (seq-map 'pydyn-dynamo--node-select-of node-infos))


(defun pydyn-dynamo--select-node ()
  "Return node-info selected by the user."
  (let* ((node-infos (pydyn-python-nodes-get :name))
         (completions-format 'vertical)
         (completions-sort 'alphabetical)
         (selected (completing-read
                    "Jump to: "
                    (pydyn-dynamo--node-selections node-infos)
                    nil t)))
    (catch 'found-it
      (dolist (node node-infos)
        (when (string-equal (pydyn-dynamo--node-select-of node) selected)
          (throw 'found-it node))))))


;;;###autoload
(defun pydyn-dynamo-jump-to-node ()
  "Jump to selected node in current buffer."
  (interactive)
  (pydyn-is-dynamo-or-error)
  (let ((node-info (pydyn-dynamo--select-node)))
    (pydyn-json-goto-line (plist-get node-info :node-id)
                          :code-line)))


(defun pydyn-dynamo--clean-orphan (file-path)
  "Delete python file where source node in Dynamo FILE-PATH does not exist anymore."
  (when (pydyn-json-nodes-exists-p file-path)
    (let ((export-path (pydyn-python-files-in
                        (pydyn-path-export-folder file-path)))
          (node-paths (pydyn-export-path-all file-path)))
      (dolist (path-wo-src (seq-difference export-path node-paths))
        (when (file-exists-p path-wo-src)
          (delete-file path-wo-src nil))))))


;;;###autoload
(defun pydyn-dynamo-clean-orphan-code-file (&optional file-path)
  "Delete python files of not existing nodes of Dynamo FILE-PATH."
  (interactive (list (pydyn-selection-get
                      (pydyn-dynamo-files-in pydyn-source-root t)
                      "Select Dynamo file:" pydyn-source-root)))
  (when (yes-or-no-p "Are you sure to delete python-files??")
    (unwind-protect
        (progn (pydyn-disable-lsp-clients)
               (pydyn-dynamo--clean-orphan file-path))
      (pydyn-enable-lsp-clients))))


;;;###autoload
(defun pydyn-dynamo-clean-orphan-code-folder (&optional directory)
  "Delete all python files of existing nodes from Dynamo files in DIRECTORY."
  (interactive (list (read-directory-name
                      "Delete python files without node in? "
                      pydyn-source-root)))
  (unwind-protect
      (progn
        (pydyn-disable-lsp-clients)
        (dolist (file-path (pydyn-dynamo-files-in directory t))
          (pydyn-dynamo--clean-orphan file-path)))
    (pydyn-enable-lsp-clients)))


;;;###autoload
(defun pydyn-dynamo-buffer-cache-reset ()
  "Reset global buffer cache."
  (interactive)
  (pydyn-buffer-cache-reset))


;;;###autoload
(defun pydyn-dynamo-file-cache-reset ()
  "Reset node cache of FILE-PATH in buffer-cache."
  (interactive)
  (pydyn-node-cache-reset buffer-file-name))


(defun pydyn-dynamo--cache-reset-h ()
  "Reset node cache for current file."
  (when (pydyn-is-dynamo?)
    (pydyn-node-cache-reset buffer-file-name)))


(defun pydyn-dynamo--cache-remove-h ()
  "Reset node cache for current file."
  (when (pydyn-is-dynamo-source?)
    (pydyn-node-cache-reset buffer-file-name))
  (unless (seq-some
           (lambda (buf)
             (pydyn-is-dynamo-source? (buffer-file-name buf)))
           (buffer-list))
    (pydyn-buffer-cache-reset)))


(define-minor-mode pydyn-dynamo-mode
  "Toggles pydyn-dynamo-mode."
  :global nil
  :group 'pydyn
  :lighter " pydyn-dynamo"
  :keymap pydyn-dynamo-mode-map

  (add-hook 'after-revert-hook 'pydyn-dynamo--cache-reset-h)
  (add-hook 'kill-buffer-hook 'pydyn-dynamo--cache-remove-h)

  (when (and pydyn-dynamo-mode
             (pydyn-not-processing?))
    (pydyn-dynamo-indent-width-setup)
    (message "ELYO DYNAMO")))


;;;###autoload
(defun pydyn-is-json-mode? ()
  "Return non-nil when active major mode is `json-mode'."
  (or (equal major-mode 'json-mode)
      (derived-mode-p 'json-mode)))


;;;###autoload
(defun pydyn-dynamo-json-config ()
  "Setup JSON file to work for minor modes."
  (when (and (pydyn-is-json-mode?)
             (pydyn-is-dynamo?))
    (setq-local require-final-newline nil
                so-long--inhibited t)))


;;;###autoload
(defun pydyn-dynamo-mode-activate ()
  "Activate and config `pydyn-dynamo-mode' if possible."
  (pydyn-dynamo-json-config)
  (when (and (pydyn-is-json-mode?)
             (pydyn-is-dynamo?)
             (pydyn-is-dynamo-source?))
    (pydyn-dynamo-mode 1)))


;;;###autoload
(defun pydyn-dynamo-mode-on ()
  "Activate `pydyn-dynamo-mode' if possible."
  (interactive)
  (pydyn-dynamo-mode-activate))


;;;###autoload
(defun pydyn-dynamo-mode-off ()
  "Deaktiviert `pydyn-dynamo-mode'."
  (interactive)
  (pydyn-dynamo-mode -1))


(provide 'pydyn-dynamo)
;;; pydyn-dynamo.el ends here
