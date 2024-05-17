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
    (define-key key-map (pydyn-python-key "r") #'pydyn-python-node-rename)
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

(defun pydyn-python-command-regex-get(name-and-value)
  "Return regex for NAME-AND-VALUE of comment, like type: ignore."
  (format "#[ ]?%s[ ]?:[ ]?%s"
          (s-trim (car name-and-value))
          (s-trim (cadr name-and-value))))


(defun pydyn-python-command-regex(comment)
  "Return regex for COMMENT, like type: ignore."
  (let ((wo-hash (s-trim (string-replace "#" "" comment))))
    (pydyn-python-command-regex-get (s-split ":" wo-hash))))


(defcustom pydyn-python-indent-width nil
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
  (save-excursion
    (goto-char (point-min))
    (when (pydyn-python--comment-next-search
           pydyn-python-formatter-off)
      (goto-char (point-min))
      (while (not (eobp))
        (forward-line)
        (pydyn-python-formatter-enable)))))


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
(defun pydyn-python-ignore-to-errors ()
  "Add `pydyn-python-type-ignore' to known type checker errors."
  (interactive)
  (when pydyn-python-type-ignore-regex
    (save-excursion
      (dolist (regex pydyn-python-type-ignore-regex)
        (pydyn-while-regex regex 'pydyn-python-ignore-add))))
  (when pydyn-python-type-ignore-contain
    (save-excursion
      (dolist (search-for pydyn-python-type-ignore-contain)
        (pydyn-while-search search-for 'pydyn-python-ignore-add)))))


(defun pydyn-python-convert-clean ()
  "Remove any python comment in current buffer."
  (pydyn-python-delete-comment-lines)
  (pydyn-python-ignore-to-errors))


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


;;;###autoload
(defun pydyn-python-buffer-clean ()
  "Remove special python comments in current buffer."
  (interactive)
  (pydyn-python-ignore-clean-buffer)
  (pydyn-python-formatter-clean-buffer))


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


;;;###autoload
(defun pydyn-python-goto-dynamo-node ()
  "Goto to source file and try to select code at point in source."
  (interactive)
  (pydyn-is-python-export-or-error)
  (pydyn-goto-code (pydyn-convert-to-dynamo
                    (pydyn-current-line))))


(defun pydyn-python--code-clean ()
  "Return code of current buffer with removed python comments."
  (let ((code (buffer-string)))
    (with-temp-buffer
      (insert code)
      (goto-char (point-min))
      (pydyn-python-buffer-clean)
      (buffer-string))))


(defun pydyn-python--to-dynamo-node ()
  "Replace python code of current buffer in Dynamo node."
  (pydyn-convert-python-to-dynamo (pydyn-python--code-clean)))


;;;###autoload
(defun pydyn-python-to-dynamo-node (file-path switch-or-kill)
  "Replace code from FILE-PATH in source. SWITCH-OR-KILL Dynamo buffer afterwarts."
  (interactive (list buffer-file-name
                     (pydyn-choose-switch-or-kill "Dynamo")))
  (pydyn-is-python-export-or-error file-path)
  (with-current-buffer (pydyn-buffer-by file-path)
    (pydyn-buffer-save (pydyn-python--to-dynamo-node)
                       (pydyn-is-switch switch-or-kill)
                       (pydyn-is-switch-other switch-or-kill)
                       (pydyn-is-kill switch-or-kill))))


;;;###autoload
(defun pydyn-python-to-dynamo-script (file-path switch-or-kill)
  "Replace code from FILE-PATH of all Dynamo nodes, SWITCH-OR-KILL buffer."
  (interactive (list (if (pydyn-is-python-export? buffer-file-name)
                         (buffer-file-name)
                       (pydyn-selection-get (pydyn-python-files-in pydyn-export-root t)
                                            "Select python file: " pydyn-export-root))
                     (pydyn-choose-switch-or-kill "Dynamo")))
  (pydyn-is-python-export-or-error file-path)
  (unwind-protect
      (progn
        (pydyn-disable-lsp-clients)
        (let ((directory (file-name-directory file-path))
              (buffer-before (current-buffer))
              (switch (pydyn-is-switch switch-or-kill))
              (other-win (pydyn-is-switch-other switch-or-kill))
              (kill (pydyn-is-kill switch-or-kill))
              (dyn-path nil))
          (dolist (python-path (pydyn-python-files-in directory))
            (let ((buffer (pydyn-buffer-by python-path)))
              (with-current-buffer buffer
                (setq dyn-path (pydyn-python--to-dynamo-node)))
              (unless (equal buffer buffer-before)
                (kill-buffer-if-not-modified buffer))))
          (pydyn-buffer-save dyn-path switch other-win kill)
          (message "Dynamo '%s' updated" (string-remove-prefix
                                          pydyn-source-root dyn-path))))
    (pydyn-enable-lsp-clients)))


;;;###autoload
(defun pydyn-python-to-dynamo-folder (directory switch-or-kill)
  "Replace code in Dynamo of python files in DIRECTORY, SWITCH-OR-KILL last buffer."
  (interactive (list (read-directory-name "Replace code in Dynamo source of all python files in? "
                                          pydyn-export-root)
                     (pydyn-choose-switch-or-kill "Dynamo")))
  (unwind-protect
      (let ((dyn-path nil)
            (buffer-before (current-buffer))
            (switch (pydyn-is-switch switch-or-kill))
            (other-win (pydyn-is-switch-other switch-or-kill))
            (kill (pydyn-is-kill switch-or-kill)))
        (pydyn-disable-lsp-clients)
        (dolist (file-path (pydyn-python-files-in directory t))
          (let ((buffer (pydyn-buffer-by file-path)))
            (with-current-buffer buffer
              (let ((current-dyn (pydyn-python--to-dynamo-node)))
                (unless (or dyn-path (string-equal current-dyn dyn-path))
                  (when (and dyn-path (not (string-equal current-dyn dyn-path)))
                    (pydyn-buffer-save dyn-path nil t)
                    (message "Dynamo '%s' updated"
                             (string-remove-prefix
                              pydyn-source-root dyn-path)))
                  (setq dyn-path current-dyn))
                (when (not (equal buffer buffer-before))
                  (kill-buffer-if-not-modified buffer))))))
        (pydyn-buffer-save dyn-path switch other-win kill))
    (pydyn-enable-lsp-clients)))


(define-minor-mode pydyn-python-mode
  "Toggles pydyn-python-mode."
  :global nil
  :group 'pydyn
  :lighter " pydyn-python"
  :keymap pydyn-python-mode-map
  (cond
   ((and pydyn-python-mode (pydyn-not-processing?))
    (pydyn-python-indent-width-setup)
    (pydyn-python-line-length-setup)
    (pydyn-buffer-breadcrumb-on)
    (message "ELYO PYTHON on"))
   ((and pydyn-python-mode (not (pydyn-not-processing?)))
    (setq pydyn-python-mode nil)
    (message "CONVERT running"))
   (t
    (setq pydyn-python-mode nil)
    (message "ELYO PYTHON off"))))


;;;###autoload
(defun pydyn-is-python-mode? ()
  "Return non-nil if current mode is `python-mode'."
  (or (equal major-mode 'python-mode)
      (derived-mode-p 'python-mode)))


;;;###autoload
(defun pydyn-python-mode-activate ()
  "Function to activate `pydyn-python-mode'."
  (if (and (pydyn-is-python-mode?)
           (or (pydyn-is-python-export?)
               (pydyn-is-python-source?)))
      (progn (pydyn-python-indent-width-setup)
             (pydyn-python-line-length-setup)
             (if (pydyn-not-processing?)
                 (pydyn-python-mode-on)
               (pydyn-python-mode-off)))
    (pydyn-python-mode-off)))


;;;###autoload
(defun pydyn-python-mode-on ()
  "Activate `pydyn-dynamo-mode'."
  (interactive)
  (pydyn-python-mode 1))


;;;###autoload
(defun pydyn-python-mode-off ()
  "Deaktiviert `pydyn-dynamo-mode'."
  (interactive)
  (pydyn-python-mode -1))


(provide 'pydyn-python)
;;; pydyn-python.el ends here
