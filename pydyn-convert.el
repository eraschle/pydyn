;;; pydyn-convert.el --- Dynamo BIM package -*- lexical-binding: t; -*-

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
;; This module provides functionality to convert code between python and dynamo
;;
;;; Code:
(require 'pydyn-json)
(require 'pydyn-path)
(require 'pydyn-utils)

(defvar-local node-uuid nil "Unique node id in the source file.")
(put 'node-uuid 'safe-local-variable (lambda (_) t))

(defvar-local node-engine nil "Python engine to use for code.")
(put 'node-engine 'safe-local-variable (lambda (_) t))

(defvar-local node-path nil "Source path of the source file.")
(put 'node-path 'safe-local-variable (lambda (_) t))

(defvar-local node-export nil "Export path for preview buffers.")
(put 'node-export 'safe-local-variable (lambda (_) t))


(defun pydyn-local-var? ()
  "Return non-nil if buffer-local line exist."
  ;; # -*- pydyn-beg: 12716;
  (let* ((local-value "# -*-")
         (value-count (1+ (seq-length local-value))))
    (unless (<= (point-max) value-count)
      (s-starts-with-p local-value (pydyn-buffer-substring
                                    (point-min) value-count)))))


(defun pydyn--convert-file-local-set (node-info)
  "Add file local variable with value of NODE-INFO in current.buffer."
  (goto-char (point-min))
  (unless (pydyn-local-var?)
    (open-line 1))
  (add-file-local-variable-prop-line 'node-uuid   (plist-get node-info :node-id))
  (add-file-local-variable-prop-line 'node-engine (plist-get node-info :engine))
  (add-file-local-variable-prop-line 'node-path   (plist-get node-info :path)))


(defun pydyn--convert-buffer-local-set (node-info &optional export-path)
  "Set buffer-local variable with value of NODE-INFO and EXPORT-PATH."
  (setq-local node-uuid   (plist-get node-info :node-id))
  (setq-local node-engine (plist-get node-info :engine))
  (setq-local node-path   (plist-get node-info :path))
  (when (and export-path (not (file-exists-p export-path)))
    (setq-local node-export export-path)))


(defun pydyn-export-path (node-info)
  "Return export python file path for NODE-INFO."
  (concat (pydyn-path-export-folder (plist-get node-info :path))
          (pydyn-path-export-file-name node-info)))


(defun pydyn-code-in-buffer (buffer node-info &optional callback)
  "Return BUFFER with cleaned code from NODE-INFO.
CALLBACK is applied to clean exported code."
  (let ((engine (plist-get node-info :engine))
        (code (pydyn-json-decode
               (plist-get node-info :code))))
    (with-current-buffer buffer
      (let ((coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8)
            (delete-trailing-lines t)
            (require-final-newline nil))
        (read-only-mode -1)
        (coding-system-change-eol-conversion 'utf-8 'unix)
        (erase-buffer)
        (insert code)
        (when callback
          (funcall callback))
        (when (pydyn-is-python-2? engine)
          (pydyn-buffer-tabify))
        (when (pydyn-is-python-3? engine)
          (pydyn-buffer-untabify))
        (delete-trailing-whitespace (point-min)
                                    (point-max))
        ;; Go to first line of code
        (goto-char (point-min))
        (while (string-blank-p (pydyn-current-line))
          (forward-line)))
      (current-buffer))))


(defun pydyn--preview-get (export-path node-info)
  "Return convert code from NODE-INFO with CALLBACK at EXPORT-PATH if exist."
  (let ((buffer (if (file-exists-p export-path)
                    (pydyn-buffer-by export-path)
                  (pydyn-buffer-preview-get
                   (file-name-base export-path)))))
    (pydyn-code-in-buffer buffer node-info)))


(defun pydyn-preview-buffer (node-info)
  "Return BUFFER with converted code from NODE-INFO.
CALLBACK is applied to clean exported code."
  (let* ((export (pydyn-export-path node-info))
         (export-exist (file-exists-p export)))
    (with-current-buffer (pydyn--preview-get export node-info)
      (when export-exist
        (pydyn--convert-file-local-set node-info))
      (pydyn--convert-buffer-local-set node-info export)
      ;; Go to first line of code
      (goto-char (point-min))
      (when export-exist
        ;; Because of file local vars, start at second line
        (forward-line))
      (while (string-blank-p (pydyn-current-line))
        (forward-line))
      (current-buffer))))


(defun pydyn-convert-node-to-python (node-info callback)
  "Return BUFFER with converted code from NODE-INFO.
CALLBACK is applied to clean exported code."
  (let ((export-path (pydyn-export-path node-info)))
    (with-current-buffer (pydyn-code-in-buffer
                          (pydyn-buffer-by export-path) node-info callback)
      (pydyn--convert-file-local-set node-info)
      (pydyn--convert-buffer-local-set node-info nil)
      (goto-char (point-min)) ; Go to first line of code
      (forward-line) ; Because of file local vars, start at second line
      (while (string-blank-p (pydyn-current-line))
        (forward-line))
      (current-buffer))))


(defun pydyn-convert--export-paths-for (dynamo-files)
  "Return list of export folders for DYNAMO-FILES."
  (seq-map #'pydyn-path-export-folder dynamo-files))


(defun pydyn-convert--root-folder-of (dynamo-folders)
  "Return root folder of DYNAMO-FOLDERS."
  (let ((sorted-path (sort dynamo-folders #'string-greaterp)))
    (file-name-parent-directory (seq-first sorted-path))))


(defun pydyn-convert--python-folder-in (export-root)
  "Return list of python folders in export path of EXPORT-ROOT."
  (seq-uniq (seq-map #'pydyn-config-as-dir-path
                     (directory-files-recursively
                      export-root ".*py.*" nil))))


(defun pydyn-convert--orphan-items (root-path deleted)
  "Return report of DELETED items in ROOT-PATH."
  (seq-map
   (lambda (path)
     (format "-> %s" (string-remove-prefix
                      root-path path)))
   (sort deleted #'string-lessp)))


(defun pydyn-convert--orphan-report (kind root-path deleted)
  "Report DELETED orphan files or folders (KIND) in ROOT-PATH."
  (unless (seq-empty-p deleted)
    (let ((kind (if (length> deleted 1) (concat kind "s") kind))
          (names (pydyn-convert--orphan-items root-path deleted)))
      (message "Deleted orphan %s in %S:\n%s"
               kind root-path (string-join names "\n")))))

(defun pydyn-convert--delete-orphan-file (file-path)
  "Delete FILE-PATH if exist and is python file.
Return non-nil if file was deleted."
  (when (and (file-exists-p file-path) (pydyn-is-python? file-path))
    (delete-file file-path nil) t))


(defun pydyn-convert-delete-orphan-folder (dynamo-files)
  "Delete all python folders are not existing in DYNAMO-FILES."
  (let* ((export-folders (pydyn-convert--export-paths-for dynamo-files))
         (export-root (pydyn-convert--root-folder-of export-folders))
         (python-folders (pydyn-convert--python-folder-in export-root))
         (deleted nil))
    (dolist (to-delete (seq-difference python-folders export-folders))
      (dolist (file (directory-files-recursively to-delete "*.py" t))
        (pydyn-convert--delete-orphan-file file))
      (delete-directory to-delete)
      (push to-delete deleted))
    (pydyn-convert--orphan-report "folder" export-root deleted)))


(defun pydyn-convert-clean-orphan (dynamo-nodes)
  "Delete python files in FILE-PATH that are not in DYNAMO-NODES."
  (let* ((dynamo-path (plist-get (seq-first dynamo-nodes) :path))
         (export-root (pydyn-path-export-folder dynamo-path))
         (python-files (pydyn-path-python-files-in export-root))
         (node-paths (seq-map 'pydyn-export-path dynamo-nodes))
         (deleted nil))
    (dolist (to-delete (seq-difference python-files node-paths))
      (when (pydyn-convert--delete-orphan-file to-delete)
        (push to-delete deleted)))
    (pydyn-convert--orphan-report "file" export-root deleted)))


(defun pydyn-convert-to-python (dynamo-path clean-cb)
  "Export all python nodes to python files and apply CLEAN-CB in DYNAMO-PATH.
Unless last buffer,buffer will be saved and killed."
  (let* ((last-export nil)
         (do-kill (not (equal dynamo-path buffer-file-name)))
         (dyn-nodes (pydyn-python-nodes-in dynamo-path do-kill)))
    (dolist (node-info dyn-nodes)
      (when last-export
        (pydyn-buffer-save last-export 'kill-buffer))
      (setq last-export (pydyn-convert-node-to-python
                         node-info clean-cb)))
    last-export))


(defun pydyn-convert-to-dynamo (code)
  "Return CODE converted into Dynamo format."
  (with-temp-buffer
    (let ((delete-trailing-lines t)
          (require-final-newline nil))
      (insert code)
      (goto-char (point-min))
      (when (pydyn-local-var?)
        (goto-char (point-min))
        (delete-line))
      (delete-trailing-whitespace (point-min) (point-max))
      ;; Dynamo use always tabs and \n in JSON string...
      (pydyn-json-encode
       (string-trim (pydyn-buffer-substring (point-min) (point-max))
                    "[\n]+" "[\n]+")))))


(defun pydyn-convert-python-to-dynamo (code)
  "Replace python CODE in Dynamo Python NODE."
  (when (pydyn-is-dynamo? node-path)
    (let ((code (pydyn-convert-to-dynamo code))
          (node-id node-uuid))
      (save-current-buffer
        (set-buffer (pydyn-buffer-by node-path))
        (pydyn-json-code-replace node-id code)))
    node-path))


(defun pydyn-goto-code (code)
  "Select CODE in node of Python file in current buffer."
  (let ((node-id node-uuid)
        (buffer (pydyn-buffer-by node-path))
        (trim "[ \\t\\n\\r\"]+"))
    (switch-to-buffer buffer)
    (let ((scroll-conservatively 101)
          (scroll-preserve-screen-position t))
      (pydyn-json-select-code-of
       node-id (string-trim code trim trim)))
    (call-interactively
     (pydyn-utils-scroll-window-left))))


(defun pydyn-utils-scroll-window-left ()
  "Scroll window left while keeping cursor position."
  (interactive)
  (let ((original-position (point))
        (original-window-start (window-start))
        (scroll-column-number (if this-command
                                  (current-column)
                                (max 0 (- (current-column)
                                          (round (* 0.10 (window-body-width))))))))
    (scroll-left scroll-column-number)
    (goto-char original-position)
    (set-window-start (selected-window) original-window-start)))


(defun pydyn-dynamo-file-info ()
  "Return file info from Dynamo SCRIPT or CUSTOM NODE."
  (pydyn-dynamo-file-info-of node-path))


(provide 'pydyn-convert)
;;; pydyn-convert.el ends here
