;;; pydyn-python.el --- DOOM Dynamo package -*- lexical-binding: t; -*-

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
;; This module provides `pydyn-python-mode'
;;
;;; Code:

(require 'pydyn-path)
(require 'pydyn-convert)
(require 'pydyn-utils)

(require 's)
(require 'rect)


(defcustom pydyn-python-keymap-prefix "C-p"
  "The prefix for pydyn-python-mode key bindings."
  :type 'string
  :group 'pydyn)


(defun pydyn-python-key (key)
  "Return Emacs key representation of KEY."
  (kbd (pydyn-key pydyn-python-keymap-prefix key)))


(defun pydyn-python--mode-map-create ()
  "Define python keymap."
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (pydyn-python-key "m") #'pydyn-python-mode-on)
    (define-key key-map (pydyn-python-key "M") #'pydyn-python-mode-off)
    (define-key key-map (pydyn-python-key "b") #'pydyn-python-if-remove-bracket)
    (define-key key-map (pydyn-python-key "b") #'pydyn-python-backslash-ensure)
    (define-key key-map (pydyn-python-key "f") #'pydyn-python-formatter-disable)
    (define-key key-map (pydyn-python-key "F") #'pydyn-python-formatter-enable)
    (define-key key-map (pydyn-python-key "g") #'pydyn-python-goto-dynamo-node)
    (define-key key-map (pydyn-python-key "i") #'pydyn-python-ignore-to-inputs)
    (define-key key-map (pydyn-python-key "I") #'pydyn-python-ignore-to-errors)
    (define-key key-map (pydyn-python-key "h") #'pydyn-python-highlight-regex)
    (define-key key-map (pydyn-python-key "H") #'pydyn-python-unhighlight-regex)
    (define-key key-map (pydyn-python-key "n") #'pydyn-python-to-dynamo-node)
    (define-key key-map (pydyn-python-key "s") #'pydyn-python-to-dynamo-script)
    (define-key key-map (pydyn-python-key "S") #'pydyn-python-to-dynamo-folder)
    (define-key key-map (pydyn-python-key "t") #'pydyn-buffer-tabify)
    (define-key key-map (pydyn-python-key "T") #'pydyn-buffer-untabify)
    (define-key key-map (pydyn-python-key "y") #'pydyn-python-ignore-toggle)
    key-map))


(defvar pydyn-python-mode-map (pydyn-python--mode-map-create)
  "The keymap for pydyn-python-mode.")

(add-to-list 'minor-mode-alist '(pydyn-python-mode " pydyn-python"))
(add-to-list 'minor-mode-map-alist (cons 'pydyn-python-mode pydyn-python-mode-map));;

(defun pydyn-python-command-regex-get (checker-n-comment)
  "Return regex for CHECKER-N-COMMENT of comment, like type: ignore."
  (format "\\(#[ ]?%s[ ]?\\):\\([ ]?%s\\)\\(.*\\)"
          (s-trim (car checker-n-comment))
          (s-trim (cadr checker-n-comment))))


(defun pydyn-python-command-regex (comment)
  "Return regex for COMMENT, like type: ignore."
  (let ((wo-hash (s-trim (string-replace "#" "" comment))))
    (pydyn-python-command-regex-get (s-split ":" wo-hash))))


(defcustom pydyn-python-indent-width 4
  "Indent width of spaces in `python-mode'."
  :type 'integer
  :group 'pydyn)


(defun pydyn-python-indent-width-setup ()
  "Set `pydyn-python-indent-width' in current buffer."
  (when pydyn-python-indent-width
    (pydyn-indent-width-set pydyn-python-indent-width)))


(defcustom pydyn-python-line-length nil
  "Maximal line length for python code line."
  :type 'integer
  :group 'pydyn)


(defun pydyn-python-line-length-setup ()
  "Set `pydyn-python-line-length' in current buffer."
  (when pydyn-python-line-length
    (setq fill-column pydyn-python-line-length)))


(defcustom pydyn-python-type-ignore nil
  "Comment to suppress type checker error."
  :type 'string
  :group 'pydyn)


(defun pydyn-python-is-type-ignore? ()
  "Return non-nil if current line contain `pydyn-python-type-ignore'."
  (when pydyn-python-type-ignore
    (s-matches? (pydyn-python-command-regex
                 pydyn-python-type-ignore)
                (pydyn-current-line))))


(defun pydyn-python--ignore-add ()
  "Return current line with append `pydyn-python-type-ignore'."
  (when pydyn-python-type-ignore
    (concat (s-trim-right (pydyn-current-line))
            (make-string 2 ? )
            pydyn-python-type-ignore)))


;;;###autoload
(defun pydyn-python-ignore-add()
  "Add `pydyn-python-type-ignore' if not exist already."
  (interactive)
  (unless (pydyn-python-is-type-ignore?)
    (replace-string-in-region (pydyn-current-line)
                              (pydyn-python--ignore-add)
                              (pos-bol) (pos-eol))))


;;;###autoload
(defun pydyn-python-ignore-toggle()
  "Toggle `pydyn-python-type-ignore' in current line."
  (interactive)
  (when pydyn-python-type-ignore
    (save-excursion
      (goto-char (pos-bol))
      (if (re-search-forward
           (pydyn-python-command-regex pydyn-python-type-ignore)
           (pos-eol) t 1)
          (pydyn-python-ignore-remove-match)
        (pydyn-python-ignore-add)))))


(defun pydyn-python--match-replaced(matched)
  "Return current line with MATCHED replaced."
  (s-trim-right (s-replace matched "" (pydyn-current-line))))


(defun pydyn-python-ignore-remove-match ()
  "Return current line with last search match replaced."
  (let ((matched (pydyn-buffer-substring (match-beginning 0)
                                         (match-end 0))))
    (replace-string-in-region (pydyn-current-line)
                              (pydyn-python--match-replaced matched)
                              (pos-bol) (pos-eol))))


;;;###autoload
(defun pydyn-python-ignore-clean-buffer ()
  "Remove `pydyn-python-type-ignore' in current buffer."
  (interactive)
  (when pydyn-python-type-ignore
    (save-excursion
      (pydyn-while-regex (pydyn-python-command-regex pydyn-python-type-ignore)
                         'pydyn-python-ignore-remove-match))))


(defcustom pydyn-python-formatter-on nil
  "Comment for enable formatter in upcoming lines."
  :type 'string
  :group 'pydyn)


(defun pydyn-python-formatter-add-on (end-point)
  "Return point of added `pydyn-python-formatter-on' at END-POINT."
  (when pydyn-python-formatter-on
    (save-excursion
      (let ((end-point (progn (goto-char end-point)
                              (pos-bol))))
        ;; First insert the end value
        (goto-char end-point)
        (if (s-blank? (pydyn-current-line))
            (progn (insert pydyn-python-formatter-on)
                   (open-line 1))
          (ensure-empty-lines 1)
          (forward-line 1)
          (insert pydyn-python-formatter-on)))
      (point))))


(defcustom pydyn-python-formatter-off nil
  "Comment to disable formatter until `pydyn-python-formatter-off'."
  :type 'string
  :group 'pydyn)

(defun pydyn-python-formatter-set-p ()
  "Return non-nil if python formatter strings are set."
  (and pydyn-python-formatter-on pydyn-python-formatter-off))


(defun pydyn-python-formatter-set-or-error ()
  "Raise error if python formatter strings are not set."
  (unless (pydyn-python-formatter-set-p)
    (error "Python formatter comments not set")))


(defun pydyn-python-formatter-add-off (start-point)
  "Return point of added `pydyn-python-formatter-off' at START-POINT."
  (when pydyn-python-formatter-off
    (save-excursion
      (let ((start-point (progn (goto-char start-point)
                                (pos-bol))))
        ;; Otherwise start would change the end position
        (goto-char start-point)
        (if (s-blank? (pydyn-current-line))
            (progn (insert pydyn-python-formatter-off)
                   (ensure-empty-lines 1))
          (open-line 1)
          (insert pydyn-python-formatter-off)))
      (point))))


;;;###autoload
(defun pydyn-python-formatter-disable (start end)
  "Insert ON / OFF FORMATTER comment at START and END."
  (interactive "r")
  (pydyn-python-formatter-set-or-error)
  ;; First insert the end value
  (let ((end-point (pydyn-python-formatter-add-on end))
        ;; Otherwise start would change the end position
        (start-point (pydyn-python-formatter-add-off start)))
    (rectangle-forward-char (- end-point end))
    (rectangle-backward-char (- start start-point))))


(defun pydyn-python--comment-next-search (comment)
  "Search for the next COMMENT from current point.
Return point of match or nil."
  (re-search-forward (pydyn-python-command-regex comment)
                     (point-max) t 1))

(defun pydyn-python--formatter-next (comment)
  "Return next position of COMMENT."
  (save-excursion
    (pydyn-python--comment-next-search comment)
    (match-beginning 0)))


(defun pydyn-python--formatter-previous (comment)
  "Return previous position of COMMENT."
  (save-excursion
    (re-search-backward (pydyn-python-command-regex comment)
                        (point-min) t 1)
    (match-beginning 0)))


(defun pydyn-python-formatter-on-pos ()
  "Return previous and next position of `pydyn-python-formatter-on'."
  (let ((comment pydyn-python-formatter-on))
    (cons (pydyn-python--formatter-previous comment)
          (pydyn-python--formatter-next comment))))


(defun pydyn-python-formatter-off-pos ()
  "Return previous and next position of `pydyn-python-formatter-off'."
  (let ((comment pydyn-python-formatter-off))
    (cons (pydyn-python--formatter-previous comment)
          (pydyn-python--formatter-next comment))))


(defun pydyn-python-formatter-is-inside? ()
  "Return non-nil if point is between ON / OFF comment."
  (let ((off-pos (pydyn-python-formatter-off-pos))
        (on-pos (pydyn-python-formatter-on-pos)))
    (< (car off-pos) (cdr on-pos))))


;;;###autoload
(defun pydyn-python-formatter-enable ()
  "Remove formatter comment if point is between OFF / ON comment."
  (interactive)
  (pydyn-python-formatter-set-or-error)
  (when (pydyn-python-formatter-is-inside?)
    (save-excursion
      (goto-char (car (pydyn-python-formatter-off-pos)))
      (delete-line))
    (save-excursion
      (goto-char (cdr (pydyn-python-formatter-on-pos)))
      (delete-line))))


;;;###autoload
(defun pydyn-python-formatter-clean-buffer ()
  "Remove formatter comment in current buffer."
  (interactive)
  (when (pydyn-python-formatter-set-p)
    (save-excursion
      (goto-char (point-min))
      (when (pydyn-python--comment-next-search
             pydyn-python-formatter-off)
        (goto-char (point-min))
        (while (not (eobp))
          (forward-line)
          (pydyn-python-formatter-enable))))))


(defcustom pydyn-python-delete-contain nil
  "Remove if any value of this list contain in current line."
  :type 'list
  :group 'pydyn)


(defun pydyn-python-delete-comment-lines ()
  "Remove lines contain any value in `pydyn-python-delete-contain'."
  (when pydyn-python-delete-contain
    (dolist (search-for pydyn-python-delete-contain)
      (pydyn-while-search search-for 'delete-line))))


(defcustom pydyn-python-type-ignore-regex nil
  "Regex to add `pydyn-python-type-ignore' if any regex matches."
  :type 'list
  :group 'pydyn)


(defcustom pydyn-python-type-ignore-func #'pydyn-python-ignore-add
  "Function use to solve warning found by `pydyn-python-type-ignore-regex'."
  :type 'symbol
  :group 'pydyn)


;;;###autoload
(defun pydyn-python-highlight-regex ()
  "Highlight `pydyn-dynamo-input-regex' in current buffer."
  (interactive)
  (let ((hi-lock-auto-select-face t))
    (dolist (regex (completing-read-multiple
                    "Choose regex to highlight: "
                    pydyn-python-type-ignore-regex))
      (highlight-regexp regex))))


;;;###autoload
(defun pydyn-python-unhighlight-regex ()
  "Unhighlight all `pydyn-dynamo-input-regex' in current buffer."
  (interactive)
  (dolist (regex pydyn-python-type-ignore-regex)
    (unhighlight-regexp regex)))


(defcustom pydyn-python-type-ignore-contain nil
  "Add `pydyn-python-type-ignore' if current line contain any value in this."
  :type 'list
  :group 'pydyn)


;;;###autoload
(defun pydyn-python-fix-errors-at-point ()
  "Add `pydyn-python-type-ignore' to known type checker errors."
  (interactive)
  )

;;;###autoload
(defun pydyn-python-ignore-to-errors ()
  "Add `pydyn-python-type-ignore' to known type checker errors."
  (interactive)
  (when pydyn-python-type-ignore-regex
    (save-excursion
      (dolist (regex pydyn-python-type-ignore-regex)
        (pydyn-while-regex regex pydyn-python-type-ignore-func))))
  (when pydyn-python-type-ignore-contain
    (save-excursion
      (dolist (search-for pydyn-python-type-ignore-contain)
        (pydyn-while-search search-for pydyn-python-type-ignore-func)))))


(defcustom pydyn-to-python-convert-func-alist
  (list #'pydyn-python-ignore-to-inputs
        #'pydyn-python-ignore-to-errors)
  "List of functions executed in dynamo to python conversion.
The buffer contains the converted python code.
The functions are called with no arguments."
  :group 'pydyn
  :tag "Functions to convert python to dynamo"
  :type '(repeat (symbol :tag "Function")))


(defun pydyn-python-convert-clean ()
  "Remove any python comment in current buffer."
  (when (pydyn-is-python?)
    (seq-do #'funcall pydyn-to-python-convert-func-alist)))


(defun pydyn-is-python-export-or-error (&optional file-path)
  "Throw user error if FILE-PATH or current buffer is not a python-file."
  (let ((file-path (pydyn-path-get file-path)))
    (unless (pydyn-is-python-export? file-path)
      (user-error "%s is NOT a Python file" (file-name-base file-path)))))


;;;###autoload
(defun pydyn-python-ignore-to-inputs ()
  "Add `pydyn-python-type-ignore' to Dynamo Input (IN)."
  (interactive)
  (pydyn-is-python-export-or-error)
  (save-excursion
    (pydyn-while-regex pydyn-dynamo-input-regex
                       'pydyn-python-ignore-add))
  (if (and (called-interactively-p 'interactive)
           (buffer-modified-p))
      (save-buffer)))


(defcustom pydyn-convert-to-dynamo-func-alist
  (list #'pydyn-python-ignore-clean-buffer
        #'pydyn-python-formatter-clean-buffer)
  "List of functions executed in python to dynamo conversion.
The buffer contains the current python code and will converted afterwwards.
The functions are called with no arguments."
  :group 'pydyn
  :tag "Functions to clean up python code."
  :type '(repeat (symbol :tag "Function")))


;;;###autoload
(defun pydyn-python-buffer-clean (&optional not-is-python-check)
  "Remove special python comments in current buffer.
If NOT-IS-PYTHON-CHECK is non-nil, the buffer is cleaned regardless of the mode."
  (when (or not-is-python-check (pydyn-is-python?))
    (seq-do #'funcall pydyn-convert-to-dynamo-func-alist)))


(defcustom pydyn-python-inside-bracket-regex "(\\(.*?\\))"
  "Regex to find value inside bracket (group 1)."
  :type 'string
  :group 'pydyn)


(defun pydyn-python-backslash-values (match-value)
  "Return biggest backslash string found in MATCH-VALUE."
  (let ((long-slashes "\\\\\\\\")
        (one-slash "\\"))
    (when (s-contains? one-slash match-value)
      (while (and one-slash (> (seq-length long-slashes) (seq-length one-slash)))
        (if (s-contains? long-slashes match-value)
            (setq one-slash nil)
          (setq long-slashes
                (string-limit long-slashes (- (seq-length long-slashes)
                                              (seq-length one-slash)))))))
    long-slashes))


(defun pydyn-python--backslash-check ()
  "Replace `\\' with `\\\\' in region  of group 1."
  (let* ((value (match-string-no-properties 1))
         (slash (pydyn-python-backslash-values value))
         (start-r? (string-prefix-p "r" value)))
    (if (s-contains? slash value)
        (replace-string-in-region slash (if start-r? "\\" "\\\\")
                                  (match-beginning 1) (pos-eol)))))


(defun pydyn-python--backslash-contain ()
  "Return non-nil when last search contain backslashes."
  (s-contains? "\\" (match-string-no-properties 1)))


;;;###autoload
(defun pydyn-python-backslash-ensure ()
  "Start `query-replace-regexp' process to replace brackets in if-statement."
  (interactive)
  (when pydyn-python-inside-bracket-regex
    (save-excursion
      (pydyn-while-regex pydyn-python-inside-bracket-regex
                         'pydyn-python--backslash-check
                         'pydyn-python--backslash-contain)
      (unhighlight-regexp pydyn-python-inside-bracket-regex))))


(defcustom pydyn-python-if-bracket-regex "if[ ]?(\\(.*\\)):"
  "Regex to search and replace brackets in if statements."
  :type 'string
  :group 'pydyn)


;;;###autoload
(defun pydyn-python-if-remove-bracket ()
  "Start `query-replace-regexp' process to replace brackets in if-statement."
  (interactive)
  (when pydyn-python-if-bracket-regex
    (save-excursion
      (goto-char (point-min))
      (query-replace-regexp pydyn-python-if-bracket-regex "if \\1:"))))


(defun pydyn-python-dynamo-exists-or-error ()
  "Throw user error if current buffer has no Dynamo file."
  (unless node-path
    (user-error "No Dynamo file selected (Is export path %s"
                (pydyn-is-python-export? buffer-file-name)))
  (let ((file-name (file-name-base node-path)))
    (unless (file-exists-p node-path)
      (user-error "%s does not exists" file-name))
    (unless (pydyn-is-dynamo? node-path)
      (user-error "%s is NOT a Dynamo file" file-name))))


;;;###autoload
(defun pydyn-python-goto-dynamo-node ()
  "Goto to source file and try to select code at point in source."
  (interactive)
  (pydyn-python-dynamo-exists-or-error)
  (pydyn-goto-code (pydyn-convert-to-dynamo
                    (pydyn-current-line))))


(defun pydyn-python--code-clean ()
  "Return code of current buffer with removed python comments."
  (let ((code (buffer-string)))
    (with-temp-buffer
      (insert code)
      (goto-char (point-min))
      (pydyn-python-buffer-clean 'not-is-python-check)
      (buffer-string))))


(defun pydyn-python--to-dynamo-node ()
  "Replace python code of current buffer in Dynamo node."
  (condition-case error
      (pydyn-convert-python-to-dynamo (pydyn-python--code-clean))
    (error (progn (message "Error convert to dynamo: %s" error)
                  node-path))))

;;;###autoload
(defun pydyn-python-to-dynamo-node ()
  "Replace code from `current-buffer' in dynamo source file."
  (interactive)
  (pydyn-is-python-export-or-error)
  (pydyn-convert-convert-process-started)
  (let ((buffer nil)
        (save-buffer-cb (pydyn-choose-buffer-save-action "Dynamo")))
    (unwind-protect
        (setq buffer (pydyn-python--to-dynamo-node))
      (pydyn-convert-convert-process-finished)
      (pydyn-buffer-save buffer save-buffer-cb))))


(defun pydyn-python--update-message (file-path)
  "Show message for updated code in FILE-PATH."
  (let ((config (pydyn-config-by-path file-path)))
    (message "Dynamo %S updated"
             (string-remove-prefix (pydyn-config-source-path config)
                                   file-path))))


(defun pydyn-python--select-file ()
  "Return selected dynamo file path by user."
  (if (pydyn-is-python-export? buffer-file-name)
      (buffer-file-name)
    (let* ((config (pydyn-config-select-config-path))
           (export-path (pydyn-path-export-path-for config))
           (files (pydyn-path-python-files-in export-path t)))
      (pydyn-selection-get files "Select Python file: "
                           (list export-path)))))


;;;###autoload
(defun pydyn-python-to-dynamo-script (file-path save-buffer-cb)
  "Replace code from FILE-PATH of all Dynamo nodes, SAVE-BUFFER-CB buffer."
  (interactive (list (pydyn-python--select-file)
                     (pydyn-choose-buffer-save-action "Dynamo")))
  (pydyn-is-python-export-or-error file-path)
  (pydyn-convert-convert-process-started)
  (let ((directory (file-name-directory file-path))
        (buffer-before (current-buffer))
        (buffer nil)
        (dyn-path nil))
    (unwind-protect
        (dolist (python-path (pydyn-path-python-files-in directory))
          (setq buffer (pydyn-buffer-by python-path))
          (with-current-buffer buffer
            (setq dyn-path (pydyn-python--to-dynamo-node)))
          (unless (equal buffer buffer-before)
            (kill-buffer-if-not-modified buffer)))
      (pydyn-convert-convert-process-finished)
      (pydyn-python--update-message dyn-path)
      (pydyn-buffer-save dyn-path save-buffer-cb))))


(defun pydyn-python--export-search-path ()
  "Return path to search for python files based on `current-buffer'."
  (let* ((path (pydyn-path-get))
         (config (pydyn-config-by-export-path path)))
    (cond ((not (null config)) (pydyn-path-export-path-for config))
          ((pydyn-config-is-export? path) (file-name-directory path))
          (t (pydyn-path-export-path-for (pydyn-config-select-config))))))


(defun pydyn-python--select-folder ()
  "Return selected folder by user."
  (pydyn-config-select-directory
   "Replace code in Dynamo source of all python files in? "
   (pydyn-python--export-search-path)))


;;;###autoload
(defun pydyn-python-to-dynamo-folder (directory save-buffer-cb)
  "Replace code in Dynamo of python files in DIRECTORY, SAVE-BUFFER-CB last buffer."
  (interactive (list (pydyn-python--select-folder)
                     (pydyn-choose-buffer-save-action "Dynamo")))
  (pydyn-convert-convert-process-started)
  (let ((dyn-path nil)
        (buffer-before (current-buffer))
        (buffer nil))
    (unwind-protect
        (dolist (file-path (pydyn-path-python-files-in directory t))
          (setq buffer (pydyn-buffer-by file-path))
          (with-current-buffer buffer
            (let ((current-dyn (pydyn-python--to-dynamo-node)))
              (when (and dyn-path (not (string-equal current-dyn dyn-path)))
                (pydyn-python--update-message dyn-path)
                (pydyn-buffer-save dyn-path 'kill-buffer))
              (setq dyn-path current-dyn))
            (unless (equal buffer buffer-before)
              (kill-buffer-if-not-modified buffer))))
      (pydyn-convert-convert-process-finished)
      (pydyn-buffer-save dyn-path save-buffer-cb))))


(defcustom pydyn-python-can-enable-predicates
  (list 'pydyn-not-processing?
        'pydyn-is-python-export?)
  "Symbols of functions to check `pydyn-python-mode' can be enabled.
`pydyn-python-mode' will be disabled if any of these functions return nil.
Functions are called with no arguments."
  :type '(repeat (symbol :tag "Function"))
  :group 'pydyn)


;;;###autoload
(defun pydyn-python-can-enable? ()
  "Return non-nil if `pydyn-python-mode' can be activated."
  (when (derived-mode-p 'python-mode)
    (cl-every #'funcall pydyn-python-can-enable-predicates)))


(define-minor-mode pydyn-python-mode
  "Toggles pydyn-python-mode."
  :global nil
  :group 'pydyn
  :lighter " pydyn-python"
  :keymap pydyn-python-mode-map

  (unless (pydyn-python-can-enable?)
    (pydyn-python-mode-off)))


;;;###autoload
(defun pydyn-python-mode-on ()
  "Activate `pydyn-python-mode'."
  (interactive)
  (unless pydyn-python-mode
    (pydyn-python-mode 1)))


;;;###autoload
(defun pydyn-python-mode-off ()
  "Deactivate `pydyn-python-mode'."
  (interactive)
  (when pydyn-python-mode
    (pydyn-python-mode -1)))


(defvar pydyn-python-enable-predicates
  (list 'pydyn-config-load
        'pydyn-python-indent-width-setup
        'pydyn-python-line-length-setup
        'pydyn-buffer-breadcrumb-on)
  "Symbols of functions if `pydyn-python-mode' is enabled.")


(defvar pydyn-python-disable-predicates
  (list 'pydyn-config-write
        'pydyn-buffer-breadcrumb-off)
  "Symbols of functions if `pydyn-python-mode' is disabled.")


(defun pydyn-python-mode-h ()
  "Function to call when `pydyn-python-mode' is toggled."
  (if pydyn-python-mode
      (cl-mapc #'funcall pydyn-python-enable-predicates)
    (cl-mapc #'funcall pydyn-python-disable-predicates)))


(add-hook 'pydyn-python-mode-hook 'pydyn-python-mode-h)


(provide 'pydyn-python)
;;; pydyn-python.el ends here
