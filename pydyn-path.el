;;; pydyn-path.el --- DOOM Dynamo package -*- lexical-binding: t; -*-

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
;; create, handle and check path.
;;
;;; Code:

(require 'pydyn-utils)

(defcustom pydyn-python-extension "py"
  "Extension of a python code file."
  :type 'string
  :group 'pydyn)


(defcustom pydyn-python-2-engine "IronPython2"
  "Python 2 engine name of code."
  :type 'string
  :group 'pydyn)


(defcustom pydyn-python-3-engine "CPython3"
  "Python 3 engine name of code."
  :type 'string
  :group 'pydyn)


(defcustom pydyn-dynamo-script-ext "dyn"
  "Extension of dynamo script."
  :type 'list
  :group 'pydyn)


(defcustom pydyn-dynamo-custom-ext "dyf"
  "Extension of dynamo custom node."
  :type 'list
  :group 'pydyn)


(defcustom pydyn-source-root-alist nil
  "Source root path of Dynamo library."
  :type 'list
  :group 'pydyn)


(defvar pydyn-source-root-alist-restored nil
  "Prevent multiple restore of `pydyn-source-root-alist'.")


(defcustom pydyn-source-is-file-alist nil
  "Source root path of Dynamo library."
  :type 'list
  :group 'pydyn)


(defcustom pydyn-export-root nil
  "Export root path of Python files."
  :type 'string
  :group 'pydyn)


(defcustom pydyn-config-file-path (concat user-emacs-directory "pydyn/source.pydyn")
  "File path for saving and loading source paths."
  :type 'string
  :group 'pydyn)


(defvar pydyn-config-save-and-load-buffer  " *Pydyn source file*"
  "Buffer name for saving and loading config file.")


(defun pydyn-path-get (&optional file-path)
  "Return FILE-PATH buffer if non-nil, otherwise path of current buffer."
  (or file-path buffer-file-name))


(defun pydyn-is-export? (path)
  "Return non-nil if PATH is an `pydyn-export-root'."
  (s-starts-with? pydyn-export-root path))


(defun pydyn-source-config-path ()
  "Return path to saved source root paths."
  (let ((folder-path (file-name-parent-directory pydyn-config-file-path)))
    (unless (file-exists-p folder-path)
      (make-directory folder-path t))
    pydyn-config-file-path))


(defun pydyn--source-config-insert-coding (coding)
  "Insert CODING symbol of the coding-system in which the file is encoded."
  (if (memq (coding-system-base coding) '(undecided prefer-utf-8))
      (setq coding 'utf-8-emacs))
  (insert (format ";;;; -*- coding: %S; mode: lisp-data -*-\n"
                  (coding-system-base coding))))


(defun pydyn-source-config-write ()
  "Write `pydyn-source-root-alist' to file."
  (interactive)
  (unless pydyn-source-root-alist
    (user-error "No source root's to save"))
  (let ((file (pydyn-source-config-path)))
    (with-current-buffer (get-buffer-create pydyn-config-save-and-load-buffer)
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (let ((coding-system-for-write
             (or coding-system-for-write 'utf-8-emacs))
            (print-length nil)
            (print-level nil)
            (print-circle t))
        (dolist (root pydyn-source-root-alist)
          (pp root (current-buffer))
          (insert "\n"))
        (with-coding-priority '(utf-8-emacs)
          (setq coding-system-for-write (select-safe-coding-system
                                         (point-min) (point-max)
                                         (list t coding-system-for-write))))
        (goto-char (point-min))
        (pydyn--source-config-insert-coding coding-system-for-write)
        (condition-case nil
            (write-file file)
          (file-error (message "Can't write %s" file)))
        (kill-buffer (current-buffer))))))


(defun pydyn--source-config-p (path source-paths)
  "Return non-nil if PATH is a subpath of a path in SOURCE-PATHS."
  (when (and (not (string-blank-p path))
             (file-directory-p path))
    (not (seq-some (lambda (src-path)
                     (and (not (eq src-path path))
                          (s-starts-with? src-path path)))
                   source-paths))))


(defun pydyn--config-path (path)
  "Return PATH to save in config file."
  (if (and (file-name-absolute-p path)
           (file-directory-p path))
      path
    (let ((default-directory (getenv "HOME")))
      (file-name-as-directory (file-truename path)))))


(defun pydyn--prepare-paths (root-paths)
  "Return ROOT-PATHS in config format."
  (seq-map #'pydyn--config-path
           (seq-filter
            (lambda (path)
              (pydyn--source-config-p path root-paths))
            (ensure-list root-paths))))


(defun pydyn--source-root-paths (root-paths)
  "Return ROOT-PATHS in config format."
  (seq-uniq (append (pydyn--prepare-paths root-paths)
                    (pydyn--prepare-paths pydyn-source-root-alist))))


(defun pydyn--append-and-clean-source-paths (root-paths)
  "Return paths without children path.
Checked paths a list of ROOT-PATHS and `pydyn-source-root-alist'."
  (sort (pydyn--source-root-paths root-paths) #'string-lessp))


(defun pydyn--exist-in-source-roots (root-paths)
  "Return non-nil if all ROOT-PATHS exists in `pydyn-source-root-alist'."
  (seq-every-p (lambda (path)
                 (seq-contains-p pydyn-source-root-alist path))
               root-paths))


;;;###autoload
(defun pydyn-source-root-path-set (root-paths)
  "Set `pydyn-source-root-alist' with unique ROOT-PATHS append with existing."
  (let ((root-paths (pydyn--prepare-paths root-paths)))
    (cond ((not pydyn-source-root-alist)
           (setq pydyn-source-root-alist root-paths)
           (pydyn-source-config-write))
          ((not (pydyn--exist-in-source-roots root-paths))
           (let ((source-paths (pydyn--append-and-clean-source-paths root-paths)))
             (setq pydyn-source-root-alist source-paths)
             (pydyn-source-config-write)))
          (t (message "Sources exists already %S" root-paths)))))


(defun pydyn--source-config-read-get ()
  "Return source root paths from current buffer."
  (seq-map (lambda (path) (string-trim path "^[\"]+" "[\"]+$"))
           (string-split (buffer-string) "[\n]" t)))


(defun pydyn-source-config-load (&optional force)
  "Load and restore `pydyn-source-root-alist' from file.
If FORCE is non-nil force load and set config."
  (interactive (list t))
  (when (or (not pydyn-source-root-alist-restored) force)
    (let ((file (pydyn-source-config-path)))
      (with-current-buffer (get-buffer-create pydyn-config-save-and-load-buffer)
        (goto-char (point-min))
        (delete-region (point-min) (point-max))
        (condition-case nil
            (insert-file-contents file)
          (file-error (message "Can't read %s" file)))
        (goto-char (point-min))
        (delete-line)
        (when (length> (buffer-string) 0)
          (pydyn-source-root-path-set (pydyn--source-config-read-get))
          (kill-buffer (current-buffer)))))
    (setq pydyn-source-root-alist-restored t)))


(defun pydyn-is-source? (path)
  "Return non-nil if PATH is subpath of a root-path in `pydyn-source-root-alist'."
  (unless pydyn-source-root-alist
    (pydyn-source-config-load))
  (unless pydyn-source-root-alist
    (user-error "No source root exists"))
  (seq-some (lambda (src-path) (s-starts-with? src-path path))
            pydyn-source-root-alist))

(defun pydyn-source-root-of (file-path)
  "Return source-root path of FILE-PATH."
  (seq-find (lambda (src-path) (s-starts-with? src-path file-path))
            pydyn-source-root-alist))

(defun pydyn--path-is-ext? (file-path extensions)
  "Return non-nil if FILE-PATH extension is in EXTENSIONS."
  (when file-path
    (let ((file-ext (file-name-extension file-path)))
      (seq-some (lambda (ext) (progn (s-ends-with? ext file-ext)))
                (ensure-list extensions)))))


;;;###autoload
(defun pydyn-dynamo-is-script? (&optional file-path)
  "Return non-nil when FILE-PATH is Dynamo SCRIPT."
  (pydyn--path-is-ext? (pydyn-path-get file-path)
                       pydyn-dynamo-script-ext))


;;;###autoload
(defun pydyn-dynamo-is-custom? (&optional file-path)
  "Return non-nil when FILE-PATH is Dynamo CUSTOM NODE."
  (pydyn--path-is-ext? (pydyn-path-get file-path)
                       pydyn-dynamo-custom-ext))


(defun pydyn-is-dynamo? (&optional file-path)
  "Return non-nil when FILE-PATH is Dynamo SCRIPT or CUSTOM NODE."
  (let ((file-path (pydyn-path-get file-path)))
    (or (pydyn-dynamo-is-script? file-path)
        (pydyn-dynamo-is-custom? file-path))))


;;;###autoload
(defun pydyn-is-dynamo-or-error (&optional file-path)
  "Throw user error when `pydyn-is-dynamo?' return nil with FILE-PATH argurment."
  (let ((file-path (pydyn-path-get file-path)))
    (unless (pydyn-is-dynamo? file-path)
      (user-error "%s is NOT a Dynamo file" (file-name-base file-path)))))


(defun pydyn-is-dynamo-source? (&optional file-path)
  "Return non-nil when FILE-PATH is sub-path of any source-path and is dynamo file.
`pydyn-is-source?' and `pydyn-is-dynamo?' with FILE-PATH argument
and both must return non-nil."
  (let ((file-path (pydyn-path-get file-path)))
    (and (pydyn-is-source? file-path)
         (pydyn-is-dynamo? file-path))))


(defun pydyn-dynamo-add-source-root-path (file-path)
  "Add source root path for FILE-PATH to `pydyn-source-root-alist'."
  (let ((source-root (read-directory-name "Select new pydyn source-root" file-path)))
    (unless (s-starts-with? source-root file-path)
      (user-error "%S is NOT a parent-path of %S"
                  source-root file-path))
    (when (pydyn-is-source? source-root)
      (user-error "%S is a sub-path of already existing root path"
                  (file-name-base source-root)))
    (pydyn-source-root-path-set source-root)))


(defun pydyn-dynamo-file-exists-p (file-path)
  "Return non-nil when FILE-PATH is dynamo file and exist."
  (let ((file-path (pydyn-path-get file-path)))
    (and (file-exists-p file-path)
         (pydyn-is-dynamo? file-path))))


;;;###autoload
(defun pydyn-dynamo-exists-or-error (file-path)
  "Throw user error if FILE-PATH is not a Dynamo file or not exists."
  (unless (pydyn-dynamo-file-exists-p file-path)
    (user-error "%s does not exists or is NOT a Dynamo file"
                (file-name-base file-path))))


;;;###autoload
(defun pydyn-is-python-2? (engine)
  "Return non-nil when ENGINE is PYTHON 2 engine."
  (and engine (equal engine pydyn-python-2-engine)))


;;;###autoload
(defun pydyn-is-python-3? (engine)
  "Return non-nil when ENGINE is CPython 3 engine."
  (and engine (string-equal engine pydyn-python-3-engine)))


(defun pydyn-is-python? (&optional file-path)
  "Return non-nil when FILE-PATH or current buffer is PYTHON."
  (pydyn--path-is-ext? (pydyn-path-get file-path)
                       pydyn-python-extension))


(defun pydyn-is-python-export-or-error (&optional file-path)
  "Throw user error if FILE-PATH or current buffer is not a python-file."
  (let ((file-path (pydyn-path-get file-path)))
    (unless (pydyn-is-python-export? file-path)
      (user-error "%s is NOT a Python file" (file-name-base file-path)))))


(defun pydyn--buffer-for-get (file-path)
  "Return buffer for FILE-PATH. Either `current-buffer' or new buffer."
  (if (equal buffer-file-name file-path)
      (current-buffer)
    (pydyn-buffer-by file-path)))


(defun pydyn-python-local-var-set-p (file-path)
  "Return non-nil when all buffer local variables are set.
Unless FILE-PATH is `current-buffer', new buffer will be created."
  (let ((local-vars (list 'node-uuid 'node-engine 'node-path))
        (buffer (pydyn--buffer-for-get file-path)))
    (seq-every-p (lambda (var)
                   (and (buffer-local-boundp var buffer)
                        (buffer-local-value var buffer)))
                 local-vars)))


;;;###autoload
(defun pydyn-is-python-export? (&optional file-path)
  "Return non-nil when FILE-PATH is python file and local variables are set."
  (let ((file-path (pydyn-path-get file-path)))
    (and (pydyn-python-local-var-set-p file-path)
         (pydyn-is-python? file-path))))


;;;###autoload
(defun pydyn-is-python-source? (&optional file-path)
  "Return non-nil when FILE-PATH is inside of `pydyn-export-root'."
  (let ((file-path (pydyn-path-get file-path)))
    (and (pydyn-is-source? file-path)
         (pydyn-is-python? file-path))))


(defun pydyn--files-in-directory (directory extension &optional recursive)
  "Return files of EXTENSION in DIRECTORY, RECURSIVE search if non-nil."
  (let ((files (ensure-list (list))))
    (when (file-exists-p directory)
      (dolist (ext (ensure-list extension))
        (push (directory-files-recursively
               directory (format "\.%s" ext) recursive)
              files)))
    (seq-reverse (flatten-list files))))


(defun pydyn--path-export-folder-for-source (path)
  "Return translated EXPORT directory of PATH."
  (concat pydyn-export-root
          (string-replace (pydyn-source-root-of path) ""
                          (file-name-parent-directory path))
          (file-name-base path) "/"))


(defun pydyn--path-export-folder-for (file-path)
  "Return export path for FILE-PATH."
  (if (pydyn-is-export? file-path)
      (if (file-directory-p file-path)
          file-path
        (file-name-directory file-path))
    (pydyn--path-export-folder-for-source file-path)))


(defun pydyn-python-files-in (path &optional recursive)
  "Return python files in PATH, RECURSIVE search if non-nil."
  (pydyn--files-in-directory (pydyn--path-export-folder-for path)
                             pydyn-python-extension
                             recursive))


(defun pydyn-python-select-file ()
  "Return selected dynamo file path by user."
  (let ((files (pydyn-python-files-in pydyn-export-root t)))
    (pydyn-selection-get files "Select Python file: "
                         (list pydyn-export-root))))


(defun pydyn-dynamo-files-in (directory &optional recursive)
  "Return Dynamo files in DIRECTORY, RECURSIVE search if non-nil."
  (let ((files (pydyn--files-in-directory directory
                                          (list pydyn-dynamo-custom-ext
                                                pydyn-dynamo-script-ext)
                                          recursive)))
    (when pydyn-source-is-file-alist
      (dolist (func pydyn-source-is-file-alist)
        (setq files (apply func files))))
    files))


(defun pydyn-dynamo-select-file ()
  "Return selected dynamo file path by user."
  (let ((files (seq-map (lambda (source) (pydyn-dynamo-files-in source t))
                        pydyn-source-root-alist)))
    (pydyn-selection-get files "Select Dynamo file: "
                         pydyn-source-root-alist)))


(defun pydyn-path-export-folder (node-path)
  "Return export directory path for NODE-PATH. Create directory if not exist."
  (let ((export-dir (pydyn--path-export-folder-for node-path)))
    (unless (file-exists-p export-dir)
      (make-directory export-dir t))
    export-dir))


(defvar pydyn-path-clean-lookup (list " " "<" ">" "?" "|" "*" "/" "\\" "\"")
  "Not allowed characters in for directory or file path.")


(defvar pydyn-path-name-separator "_"
  "Character to separate names and not allowed names.")


(defun pydyn--path-clean-name (value)
  "Return cleaned VALUE with all `pydyn-path-clean-lookup' replaced."
  (dolist (replace-value pydyn-path-clean-lookup)
    (setq value (string-replace replace-value
                                pydyn-path-name-separator
                                value)))
  ;; Because of multiple replacements is it
  ;; possible to have more then connected.
  (replace-regexp-in-string
   "[_]+" "_" (replace-regexp-in-string
               "[__]+" "_" value)))


(defun pydyn--path-py-abbrev-of (node-info)
  "Return abbrev from python engine in NODE-INFO."
  (let ((engine (plist-get node-info :engine)))
    (cond ((pydyn-is-python-3? engine) "py3")
          (t "py2"))))


(defun pydyn-path-export-name (node-info)
  "Return export name created from NODE-INFO."
  (s-join pydyn-path-name-separator ;; join names together with _
          (list (pydyn--path-clean-name (plist-get node-info :name))
                (pydyn--path-py-abbrev-of node-info)
                (pydyn--path-clean-name (plist-get node-info :node-id)))))


(defun pydyn-path-export-file-name (node-info)
  "Return export file name created from NODE-INFO."
  (let ((file-name (pydyn-path-export-name node-info)))
    (s-downcase (concat file-name "." pydyn-python-extension))))


(provide 'pydyn-path)
;;; pydyn-path.el ends here
