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


(defun pydyn-dynamo--source-path-select (file-path)
  "Add source root path for FILE-PATH to `pydyn-source-config-alist'."
  (let ((path (pydyn--dir-path-of (read-directory-name
                                   "Select new source-config: "
                                   file-path))))
    (unless (s-starts-with? path file-path)
      (user-error "%S is NOT a parent-path of %S" path file-path))
    (when (pydyn-is-source? path)
      (user-error "%S is a sub-path of already existing root path"
                  (file-name-base path)))
    path))


(defun pydyn-dynamo--source-name-possible (name)
  "Return non-nil if NAME is a possible source name."
  (not (seq-contains-p (pydyn--config-names) name)))


(defun pydyn-dynamo--source-names-from (path)
  "Return possible source name from PATH."
  (let ((existing (pydyn--config-names)))
    (seq-filter (lambda (name) (not (seq-contains-p existing name)))
                (seq-rest (f-split path)))))


(defun pydyn-dynamo--source-name-ask (path)
  "Ask user for source name for PATH."
  (let ((name (completing-read
               (format "Enter or select name for %S: " path)
               (pydyn-dynamo--source-names-from path)
               nil #'pydyn-dynamo--source-name-possible)))
    (unless (pydyn-dynamo--source-name-possible name)
      (user-error "%S is used in an other source config" name))
    name))


(defun pydyn-dynamo--add-source-config (file-path)
  "Add source root path for FILE-PATH to `pydyn-source-config-alist'."
  (let* ((file-path (pydyn--dir-path-of file-path))
         (path (pydyn-dynamo--source-path-select file-path))
         (name (pydyn-dynamo--source-name-ask path)))
    (pydyn-source-config-set (list (list :path path :name name)))
    (pydyn-source-config-write)))


(defun pydyn-dynamo--ensure-source-config (&optional file-path)
  "Ensure source config for FILE-PATH exists."
  (let ((file-path (pydyn-path-get file-path)))
    (unless (pydyn-is-source? file-path)
      (pydyn-dynamo--add-source-config file-path))))


;;;###autoload
(defun pydyn-dynamo-goto-python ()
  "Switch to Python file of code at current point if exists."
  (interactive)
  (pydyn-is-dynamo-or-error)
  (pydyn-dynamo--ensure-source-config)
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
  (pydyn-dynamo--ensure-source-config)
  (pydyn-convert-convert-process-started)
  (let ((switch (pydyn-is-switch switch-or-kill))
        (other-win (pydyn-is-switch-other switch-or-kill))
        (kill (pydyn-is-kill switch-or-kill))
        (buffer nil))
    (unwind-protect
        (setq buffer (pydyn-convert-node-to-python
                      (pydyn-dynamo--node-info-or-error)
                      'pydyn-python-convert-clean))
      (pydyn-convert-convert-process-finished)
      (pydyn-buffer-save buffer switch other-win kill))))


;;;###autoload
(defun pydyn-dynamo-script-to-python (file-path switch-or-kill)
  "Export python node in FILE-PATH and SWITCH-OR-KILL to export buffer."
  (interactive (list (if (pydyn-is-dynamo? buffer-file-name)
                         (buffer-file-name)
                       (pydyn-dynamo-select-file))
                     (pydyn-choose-switch-or-kill "Python")))
  (pydyn-is-dynamo-or-error file-path)
  (pydyn-dynamo--ensure-source-config file-path)
  (pydyn-convert-convert-process-started)
  (let ((switch (pydyn-is-switch switch-or-kill))
        (other-win (pydyn-is-switch-other switch-or-kill))
        (kill (pydyn-is-kill switch-or-kill))
        (buffer nil))
    (unwind-protect
        (setq buffer (pydyn-convert-to-python
                      file-path 'pydyn-python-convert-clean))
      (pydyn-convert-convert-process-finished)
      (pydyn-buffer-save buffer switch other-win kill))))


(defun pydyn-dynamo-folder-select ()
  "Return folder with dynamo for export selected by the user."
  (interactive)
  (let ((source-root (completing-read "Select source-root: "
                                      (pydyn--source-config-paths) nil t)))
    (read-directory-name "Select directory: " source-root)))

;;;###autoload
(defun pydyn-dynamo-folder-to-python (&optional directory switch-or-kill)
  "Export all python nodes of Dynamo files in DIRECTORY.
SWITCH-OR-KILL last export buffer afterwards."
  (interactive (list (read-directory-name "Export Python of directory? "
                                          default-directory)
                     (pydyn-choose-switch-or-kill "Python")))
  (pydyn-dynamo--ensure-source-config directory)
  (pydyn-convert-convert-process-started)
  (let ((buffer nil)
        (switch (pydyn-is-switch switch-or-kill))
        (other-win (pydyn-is-switch-other switch-or-kill))
        (kill (pydyn-is-kill switch-or-kill)))
    (unwind-protect
        (dolist (file-path (pydyn-dynamo-files-in directory))
          (when buffer
            (pydyn-buffer-save buffer nil nil t))
          (setq buffer (pydyn-convert-to-python
                        file-path 'pydyn-python-convert-clean))))
    (pydyn-convert-convert-process-finished)
    (pydyn-buffer-save buffer switch other-win kill)))


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
  (interactive (list (if (pydyn-is-dynamo? buffer-file-name)
                         (buffer-file-name)
                       (pydyn-dynamo-select-file))))
  (when (yes-or-no-p "Are you sure to delete python-files??")
    (pydyn-convert-convert-process-started)
    (unwind-protect
        (pydyn-dynamo--clean-orphan file-path)
      (pydyn-convert-convert-process-finished))))


;;;###autoload
(defun pydyn-dynamo-clean-orphan-code-folder (&optional directory)
  "Delete all python files of existing nodes from Dynamo files in DIRECTORY."
  (interactive (list (read-directory-name
                      "Delete orphan python code from dynamo files in? "
                      default-directory)))
  (pydyn-convert-convert-process-started)
  (unwind-protect
      (dolist (file-path (pydyn-dynamo-files-in directory t))
        (pydyn-dynamo--clean-orphan file-path))
    (pydyn-convert-convert-process-finished)))


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


(defun pydyn-dynamo-json-config ()
  "Setup JSON file to work for minor modes."
  (setq-local require-final-newline nil
              so-long--inhibited t))


(defcustom pydyn-dynamo-can-enable-predicates
  (list 'pydyn-is-dynamo?
        'pydyn-not-processing?)
  "Symbols of functions to check `pydyn-dynamo-mode' can be enabled.
`pydyn-dynamo-mode' will be disabled if any of these functions return nil.
Functions are called with no arguments."
  :type '(repeat (symbol :tag "Function"))
  :group 'pydyn)


;;;###autoload
(defun pydyn-dynamo-can-enable? ()
  "Return non-nil if `pydyn-dynamo-mode' can be enabled."
  (when (derived-mode-p 'json-mode)
    (cl-every #'funcall pydyn-dynamo-can-enable-predicates)))


(define-minor-mode pydyn-dynamo-mode
  "Toggles pydyn-dynamo-mode."
  :global nil
  :group 'pydyn
  :lighter " pydyn-dynamo"
  :keymap pydyn-dynamo-mode-map

  (add-hook 'after-revert-hook 'pydyn-dynamo--cache-reset-h)
  (add-hook 'kill-buffer-hook 'pydyn-dynamo--cache-remove-h)

  (unless (pydyn-dynamo-can-enable?)
    (pydyn-dynamo-mode-off)))


;;;###autoload
(defun pydyn-dynamo-mode-on ()
  "Activates `pydyn-dynamo-mode'."
  (interactive)
  (unless pydyn-dynamo-mode
    (pydyn-dynamo-mode 1)))


;;;###autoload
(defun pydyn-dynamo-mode-off ()
  "Deactivates `pydyn-dynamo-mode'."
  (interactive)
  (when pydyn-dynamo-mode
    (pydyn-dynamo-mode -1)))


(defvar pydyn-dynamo-enable-predicates
  (list 'pydyn-source-config-load
        'pydyn-dynamo-indent-width-setup
        'pydyn-dynamo-json-config)
  "Symbols of functions if `pydyn-dynamo-mode' is enabled.")


(defvar pydyn-dynamo-disable-predicates
  (list 'pydyn-source-config-write)
  "Symbols of functions if `pydyn-dynamo-mode' is disabled.")


(defun pydyn-dynamo-mode-h ()
  "Function to call when `pydyn-dynamo-mode' is toggled."
  (if pydyn-dynamo-mode
      (cl-mapc #'funcall pydyn-dynamo-enable-predicates)
    (cl-mapc #'funcall pydyn-dynamo-disable-predicates)))

(add-hook 'pydyn-dynamo-mode-hook 'pydyn-dynamo-mode-h)


(provide 'pydyn-dynamo)
;;; pydyn-dynamo.el ends here
