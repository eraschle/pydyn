;;; pydyn-utils.el --- DOOM Dynamo package -*- lexical-binding: t; -*-

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
;;
;;; Commentary:
;;
;; This module provides everything else
;;
;;; Code:

(require 'lsp)
(require 'lsp-headerline)

(defcustom pydyn-keymap-prefix "C-c C-y"
  "Prefix for `pydyn-dynamo' and `pydyn-python' minor mode bindings."
  :type 'string
  :group 'pydyn)


(defun pydyn-key (&rest keys)
  "Return Emacs key representation of KEYS."
  (kbd (s-join " " (append
                    (ensure-list pydyn-keymap-prefix)
                    (ensure-list keys)))))


;;;###autoload
(defcustom pydyn-dynamo-input-regex "IN[^ -][^A-Za-z]\\(\[[0-9]+\]\\)?"
  "Regex for IN[0] variable in Dynamo Python scripts."
  :type 'string
  :group 'pydyn)


(defcustom pydyn-buffer-preview-prefix "Preview"
  "Prefix for preview buffer name."
  :type 'string
  :group 'pydyn)


(defun pydyn-buffer-preview-name (name)
  "Return preview buffer NAME with `pydyn-buffer-preview'."
  (message "Preview Name: %s" name)
  (concat (propertize (format "** %s " pydyn-buffer-preview-prefix) 'face 'marginalia-file-owner)
          (propertize (format "%s **" name) 'face 'marginalia-file-name)))


;;;###autoload
(defun pydyn-buffer-preview-name? ()
  "Return non-nil if current buffer is a preview buffer."
  (and (s-starts-with? (format "** %s " pydyn-buffer-preview-prefix)
                       (buffer-name))
       (s-ends-with? " **" (buffer-name))))


(defun pydyn-buffer-preview-get (name)
  "Return preview buffer with NAME."
  (let ((prev-name (pydyn-buffer-preview-name name)))
    (or (get-buffer prev-name)
        (get-buffer-create prev-name))))


;;;###autoload
(defun pydyn-buffer-by (path)
  "Return or create buffer of PATH."
  (or (get-file-buffer path)
      (find-file-noselect path t)
      (create-file-buffer path)))


(defun pydyn-choose-buffer-save-action (name)
  "Return User selection buffer of NAME (Python/Dynamo)."
  (pydyn-choose-get (list 'kill-buffer 'switch-to-buffer 'switch-to-buffer-other-window)
                    (format "Choose action for %s buffer?: " name)))


;;;###autoload
(defun pydyn-buffer-save (buffer-or-path &optional action-cb)
  "Save BUFFER-OR-PATH buffer if modified and call ACTION-CB if non-nil."
  (when buffer-or-path
    (let ((buffer (if (bufferp buffer-or-path)
                      buffer-or-path
                    (pydyn-buffer-by buffer-or-path))))
      (when (buffer-modified-p buffer)
        (with-current-buffer buffer
          (save-buffer 1)))
      (when (and action-cb (buffer-live-p buffer))
        (if (symbolp action-cb)
            (funcall action-cb buffer)
          (funcall (intern action-cb) buffer))))))


(defun pydyn-buffer-substring (start end &optional with-properties)
  "Return buffer substring between START and END.
WITH-PROPERTIES control if the substring contains properties or not."
  (if with-properties
      (buffer-substring start end)
    (buffer-substring-no-properties start end)))


(defun pydyn-current-line (&optional with-properties)
  "Return current line at point of the current buffer.
WITH-PROPERTIES control if the substring contains properties or not."
  (pydyn-buffer-substring (pos-bol) (pos-eol) with-properties))


(defun pydyn-while-search (search-for action-cb &optional do-action-cb ignore-case)
  "SEARCH-FOR and apply ACTION-CB & DO-ACTION-CB, IGNORE-CASE to match."
  (let ((case-fold-search ignore-case))
    (goto-char (point-min))
    (while (search-forward search-for (point-max) t)
      ;; (goto-char (match-beginning 0))
      (if (and do-action-cb (funcall do-action-cb))
          (funcall action-cb)
        (unless do-action-cb
          (funcall action-cb)))
      (end-of-line))))


(defun pydyn-while-regex (rx action-cb &optional do-action-cb ignore-case)
  "SEARCH-FOR RX and apply ACTION-CB & DO-ACTION-CB, IGNORE-CASE to match."
  (let ((case-fold-search ignore-case))
    (goto-char (point-min))
    (while (re-search-forward rx (point-max) t)
      (goto-char (match-beginning 0))
      (if (and do-action-cb (funcall do-action-cb))
          (funcall action-cb)
        (unless do-action-cb
          (funcall action-cb)))
      (end-of-line))))


(defun pydyn--remove-prefixes (path prefixes)
  "Return PATH without any PREFIXES."
  (dolist (prefix (ensure-list prefixes))
    (when (string-prefix-p prefix path)
      (setq path (string-remove-prefix prefix path))))
  path)


(defun pydyn--selection-for-path (path prefixes)
  "Return list with PATH without PREFIXES and PATH."
  (list (pydyn--remove-prefixes path prefixes) path))


(defun pydyn--select-list-for (paths prefixes)
  "Return list with PREFIXES free name and path of PATHS."
  (seq-map (lambda (path) (pydyn--selection-for-path path prefixes))
           (seq-sort #'string-lessp paths)))


(defun pydyn-selection-get (paths prompt prefixes)
  "Return path selected by the user with PROMPT displayed.
PREFIXES is removed in PATHS displayed to user."
  (let* ((select-list (pydyn--select-list-for (-flatten paths) prefixes))
         (completions-format 'vertical)
         (completions-sort 'alphabetical)
         (selected (completing-read prompt select-list nil t)))
    (seq-find (lambda (name-n-path)
                (equal (seq-first name-n-path) selected))
              select-list)))


(defun pydyn-choose-get (choose-list prompt &optional initial-input)
  "Return user selection from CHOOSE-LIST.
PROMPT is show to user and INITIAL-INPUT is pre selected if non-nil."
  (let ((completions-format 'vertical)
        (completions-sort 'alphabetical))
    (completing-read prompt choose-list nil t initial-input)))


(defvar pydyn-processing nil
  "Is non-nil during convert process.")


;;;###autoload
(defun pydyn-not-processing? ()
  "Return non-nil when no convert process is running."
  (not pydyn-processing))


(defcustom pydyn-process-lsp-client-disabled
  '(('json-mode   . (list json-ls
                          json-ls-tramp
                          json-rpc))
    ('python-mode . (list lsp-pyright
                          pylsp
                          pyls
                          ruff-lsp-tramp
                          ruff-lsp
                          ruff
                          json-rpc)))
  "List of LSP clients to disable during convert process."
  :type '(repeat (symbol))
  :safe 'listp
  :group 'pydyn)


(defcustom pydyn-process-start-hook nil
  "Hooks called before convert process starts."
  :type 'list
  :group 'pydyn)


(defcustom pydyn-process-end-hook nil
  "Hooks called after convert process ends."
  :type 'list
  :group 'pydyn)


;;;###autoload
(defun pydyn-convert-convert-process-started ()
  "Function to start convert process and run `pydyn-process-start-hook'."
  (unless pydyn-processing
    (setq lsp-disabled-clients pydyn-process-lsp-client-disabled)
    (run-hooks 'pydyn-process-start-hook)
    (setq pydyn-processing t)))


;;;###autoload
(defun pydyn-convert-convert-process-finished ()
  "Function clean up after convert process and run `pydyn-process-end-hook'."
  (setq pydyn-processing nil)
  (run-hooks 'pydyn-process-end-hook)
  (setq lsp-disabled-clients nil))


;;;###autoload
(defun pydyn-indent-width-set (width)
  "Setup line indent WIDTH for the current buffer."
  (setq-local tab-width width
              standard-indent width
              evil-shift-width width))


;;;###autoload
(defun pydyn-buffer-tabify ()
  "Convert buffer from SPACE to TABS indentation."
  (interactive)
  (unless indent-tabs-mode
    (indent-tabs-mode 1))
  (tabify (point-min) (point-max)))


;;;###autoload
(defun pydyn-buffer-untabify ()
  "Convert buffer from TAB to SPACE indentation."
  (interactive)
  (when indent-tabs-mode
    (indent-tabs-mode nil))
  (untabify (point-min) (point-max)))


(defun pydyn-buffer-breadcrumb-on ()
  "Convert buffer from TAB to SPACE indentation."
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-enable-symbol-numbers t))


(defun pydyn-buffer-breadcrumb-off ()
  "Convert buffer from TAB to SPACE indentation."
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-enable-symbol-numbers nil))


(provide 'pydyn-utils)
;;; pydyn-utils.el ends here
