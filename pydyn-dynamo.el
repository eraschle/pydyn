;;; pydyn-dynamo.el --- Dynamo BIM Emacs package -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Erich Raschle
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sub-license, and/or sell
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

(defcustom pydyn-dynamo-keymap-prefix "C-x C-y"
  "The prefix for pydyn-dynamo-mode key bindings."
  :type 'string
  :group 'pydyn)


(defun pydyn-dynamo-key (key)
  "Return Emacs key representation of KEY."
  (pydyn-key pydyn-dynamo-keymap-prefix key))


(defun pydyn-dynamo--mode-map-create ()
  "Define mode key-map."
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
  "Key-map for pydyn minor mode.")

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


(defun pydyn-dynamo--ensure-source-config (&optional file-path)
  "Ensure source config for FILE-PATH exists."
  (let ((file-path (pydyn-path-get file-path)))
    (unless (pydyn-config-is-known-source-p file-path)
      (pydyn-config-add-config file-path))))


(defun pydyn-is-dynamo-or-error (&optional file-path)
  "Throw user error when `pydyn-is-dynamo?' return nil with FILE-PATH argument."
  (let ((file-path (pydyn-path-get file-path)))
    (unless (pydyn-is-dynamo? file-path)
      (user-error "%s is NOT a Dynamo file" (file-name-base file-path)))))


;;;###autoload
(defun pydyn-dynamo-goto-python ()
  "Switch to Python file of code at current point if exists."
  (interactive)
  (pydyn-is-dynamo-or-error)
  (pydyn-dynamo--ensure-source-config)
  (let ((path (pydyn-export-path (pydyn-dynamo--node-info-or-error))))
    (unless (file-exists-p path)
      (pydyn-dynamo-at-point-to-python 'switch-to-buffer))
    (let ((buffer (pydyn-buffer-by path)))
      (with-current-buffer buffer
        (view-mode-exit))
      (switch-to-buffer-other-window buffer))))


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
(defun pydyn-dynamo-at-point-to-python (save-buffer-cb)
  "Export python code at point and SAVE-BUFFER-CB export buffer."
  (interactive (list (pydyn-choose-buffer-save-action "Python")))
  (pydyn-is-dynamo-or-error)
  (pydyn-dynamo--ensure-source-config)
  (pydyn-convert-process-started)
  (let ((buffer nil))
    (unwind-protect
        (setq buffer (pydyn-convert-node-to-python
                      (pydyn-dynamo--node-info-or-error)
                      'pydyn-python-convert-clean))
      (pydyn-convert-process-finished)
      (pydyn-buffer-save buffer save-buffer-cb))))



(defun pydyn-dynamo-select-file ()
  "Return selected dynamo file path by user."
  (let ((path (pydyn-config-select-config-path)))
    (pydyn-selection-get (pydyn-path-dynamo-files-in path t)
                         "Select Dynamo file: " path)))


;;;###autoload
(defun pydyn-dynamo-script-to-python (file-path delete-orphan save-buffer-cb)
  "Export python node in FILE-PATH and SAVE-BUFFER-CB to export buffer.
If DELETE-ORPHAN is non-nil delete orphan python files."
  (interactive (list (if (pydyn-is-dynamo? buffer-file-name)
                         (buffer-file-name)
                       (pydyn-dynamo-select-file))
                     (y-or-n-p "Delete orphan python files? ")
                     (pydyn-choose-buffer-save-action "Python")))
  (pydyn-is-dynamo-or-error file-path)
  (pydyn-dynamo--ensure-source-config file-path)
  (pydyn-convert-process-started)
  (when delete-orphan
    (pydyn-dynamo--clean-orphan file-path))
  (let ((buffer nil))
    (unwind-protect
        (setq buffer (pydyn-convert-to-python
                      file-path 'pydyn-python-convert-clean))
      (pydyn-convert-process-finished)
      (pydyn-buffer-save buffer save-buffer-cb))))


(defun pydyn-dynamo-select-source-for-export ()
  "Return source path to export python files from Dynamo files."
  (let ((source-root (pydyn-config-select-config-path)))
    (pydyn-config-select-directory
     "Select source to export python file from Dynamo Content? " source-root)))


;;;###autoload
(defun pydyn-dynamo-folder-to-python (directory delete-orphan save-buffer-cb)
  "Export all python nodes of Dynamo files in DIRECTORY.
SAVE-BUFFER-CB last export buffer afterwards.
If DELETE-ORPHAN is non-nil delete orphan python files."
  (interactive (list (pydyn-dynamo-select-source-for-export)
                     (y-or-n-p "Delete orphan python files? ")
                     (pydyn-choose-buffer-save-action "Python")))
  (pydyn-dynamo--ensure-source-config directory)
  (pydyn-convert-process-started)
  (let ((dynamo-files (pydyn-path-dynamo-files-in directory nil))
        (buffer nil))
    (when delete-orphan
      (pydyn-convert-delete-orphan-folder dynamo-files))
    (unwind-protect
        (dolist (file-path dynamo-files)
          (when buffer
            (pydyn-buffer-save buffer 'kill-buffer))
          (setq buffer (pydyn-convert-to-python
                        file-path 'pydyn-python-convert-clean)))
      (pydyn-convert-process-finished)
      (pydyn-buffer-save buffer save-buffer-cb))))



(defun pydyn-dynamo--node-select-of (node-info)
  "Return propertize name and node-id of NODE-INFO."
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


(defun pydyn-dynamo--clean-orphan (dynamo-path)
  "Delete python files of not existing nodes in DYNAMO-PATH."
  (when (pydyn-json-nodes-exists-p dynamo-path)
    (let* ((do-kill (not (equal dynamo-path buffer-file-name)))
           (nodes (pydyn-python-nodes-in dynamo-path do-kill)))
      (pydyn-convert-clean-orphan nodes))))


;;;###autoload
(defun pydyn-dynamo-clean-orphan-code-file (file-path)
  "Delete python files of not existing nodes of Dynamo FILE-PATH."
  (interactive (list (if (pydyn-is-dynamo? buffer-file-name)
                         (buffer-file-name)
                       (pydyn-dynamo-select-file))))
  (when (y-or-n-p "Are you sure you want to delete orphan python files? ")
    (pydyn-convert-process-started)
    (unwind-protect
        (pydyn-dynamo--clean-orphan file-path)
      (pydyn-convert-process-finished))))

(defun pydyn-dynamo--clean-orphan-in-directory (directory)
  "Delete all python files of not existing nodes from Dynamo files in DIRECTORY."
  (let ((dynamo-files (pydyn-path-dynamo-files-in directory t)))
    (pydyn-convert-delete-orphan-folder dynamo-files)
    (dolist (file-path dynamo-files)
      (pydyn-dynamo--clean-orphan file-path))))


;;;###autoload
(defun pydyn-dynamo-clean-orphan-code-folder (directory)
  "Delete all python files of existing nodes from Dynamo files in DIRECTORY."
  (interactive (list (pydyn-config-select-directory
                      "Delete orphan python code from dynamo files in? "
                      (pydyn-config-select-config-path))))
  (when (y-or-n-p "Are you sure to delete orphan python files in directory?")
    (pydyn-convert-process-started)
    (unwind-protect
        (pydyn-dynamo--clean-orphan-in-directory directory)
      (pydyn-convert-process-finished))))


;;;###autoload
(defun pydyn-dynamo-clean-orphan-code-source (source)
  "Delete all python files of not existing nodes from Dynamo files in SOURCE."
  (interactive (list (pydyn-config-select-config-path 'with-root)))
  (pydyn-dynamo-clean-orphan-code-folder
   (pydyn-config-source-path source)))


;;;###autoload
(defun pydyn-dynamo-clean-orphan-code-all-source ()
  "Delete all python files of not existing nodes from Dynamo files in all sources."
  (interactive)
  (when (y-or-n-p "Are you sure to delete orphan python files in all sources? ")
    (pydyn-convert-process-started)
    (unwind-protect
        (dolist (source (pydyn-config-sources-get 'with-root))
          (pydyn-dynamo--clean-orphan-in-directory
           (pydyn-config-source-path source)))
      (pydyn-convert-process-finished))))


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


;;;###autoload
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
  (list 'pydyn-config-load
        'pydyn-dynamo-indent-width-setup
        'pydyn-dynamo-json-config)
  "Symbols of functions if `pydyn-dynamo-mode' is enabled.")


(defvar pydyn-dynamo-disable-predicates
  (list 'pydyn-config-write)
  "Symbols of functions if `pydyn-dynamo-mode' is disabled.")


(defun pydyn-dynamo-mode-h ()
  "Function to call when `pydyn-dynamo-mode' is toggled."
  (if pydyn-dynamo-mode
      (cl-mapc #'funcall pydyn-dynamo-enable-predicates)
    (cl-mapc #'funcall pydyn-dynamo-disable-predicates)))

(add-hook 'pydyn-dynamo-mode-hook 'pydyn-dynamo-mode-h)


(provide 'pydyn-dynamo)
;;; pydyn-dynamo.el ends here
