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


(defcustom pydyn--source-config-alist nil
  "Source root path of Dynamo library."
  :type 'list
  :group 'pydyn)


(defvar pydyn-source-config-restored nil
  "Prevent multiple restore of `pydyn--source-config-alist'.")


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
  "Return FILE-PATH if non-nil, otherwise path of current buffer."
  (or file-path buffer-file-name))


(defun pydyn-is-export? (&optional path)
  "Return non-nil if PATH is an `pydyn-export-root'."
  (s-starts-with? (pydyn--dir-path-of pydyn-export-root)
                  (pydyn--dir-path-of (pydyn-path-get path))))


(defun pydyn-source-config-exists ()
  "Return path to saved source root paths."
  (file-exists-p pydyn-config-file-path))


(defun pydyn-source-config-path ()
  "Return path to saved source root paths."
  (let ((folder-path (file-name-parent-directory pydyn-config-file-path)))
    (unless (file-exists-p folder-path)
      (make-directory folder-path t))
    pydyn-config-file-path))

(defun pydyn--dir-path-of (path)
  "Return absolute PATH."
  (cond ((string-prefix-p "~" path) (expand-file-name path))
        ((not (file-directory-p path)) (file-name-parent-directory path))
        ((not (directory-name-p path)) (file-name-as-directory path))
        (t path)))

(defun pydyn--config-path-get (config)
  "Return source path of CONFIG."
  (unless config
    (setq config (list :path ""))
    (message "Source root config was nil, set to empty path"))
  (pydyn--dir-path-of (plist-get config :path)))


(defun pydyn-remove-config-path (path)
  "Return PATH without path of source config."
  (let ((config (pydyn-source-config-of path)))
    (string-remove-prefix (pydyn--config-path-get config)
                          (pydyn--dir-path-of path))))


(defun pydyn--source-config-insert-coding (coding)
  "Insert CODING symbol of the coding-system in which the file is encoded."
  (if (memq (coding-system-base coding) '(undecided prefer-utf-8))
      (setq coding 'utf-8-emacs))
  (insert (format ";;;; -*- coding: %S; mode: lisp-data -*-\n\n"
                  (coding-system-base coding))))


(defun pydyn--write-config-get (config)
  "Return source root CONFIG in string format."
  (format "%s, %s, %s, %s\n"
          :path
          (string-trim (pydyn--config-path-get config))
          :name
          (string-trim (plist-get config :name))))


(defun pydyn-source-config-write ()
  "Write `pydyn--source-config-alist' to file."
  (interactive)
  (unless pydyn--source-config-alist
    (user-error "No source root's to save"))
  (let ((file (pydyn-source-config-path)))
    (with-current-buffer (get-buffer-create pydyn-config-save-and-load-buffer)
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (let ((coding-system-for-write (or coding-system-for-write
                                         'utf-8-emacs)))
        (dolist (root pydyn--source-config-alist)
          (insert (pydyn--write-config-get root)))
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


(defun pydyn--set-absolute-path-in (config)
  "Return CONFIG with absolute path."
  (let ((path (pydyn--config-path-get config)))
    (unless (equal path (plist-get config :path))
      (setq config (plist-put config :path path))))
  config)


(defun pydyn--prepare-configs (configs)
  "Return CONFIGS in config format."
  (seq-map #'pydyn--set-absolute-path-in configs))

(defun pydyn--config-same-path-p (config other)
  "Return non-nil if path in CONFIG is equal to path in OTHER."
  (equal (plist-get config :path)
         (plist-get other :path)))


(defun pydyn--config-names (&optional configs)
  "Return source names in CONFIGS or `pydyn--source-config-alist'."
  (seq-map (lambda (config) (plist-get config :name))
           (or configs pydyn--source-config-alist)))


(defun pydyn--source-config-paths ()
  "Return source paths of `pydyn--source-config-alist'."
  (seq-map #'pydyn--config-path-get pydyn--source-config-alist))


(defun pydyn--config-path-exists (config)
  "Return non-nil if path in CONFIG exists in `pydyn--source-config-alist'."
  (seq-contains-p pydyn--source-config-alist
                  config
                  #'pydyn--config-same-path-p))


(defun pydyn--config-path-not-exists (config)
  "Return non-nil if path in CONFIG not exists in `pydyn--source-config-alist'."
  (not (pydyn--config-path-exists config)))


(defun pydyn--configs-path-not-exists (configs)
  "Return CONFIGS not exists in `pydyn--source-config-alist'.
Compare path of CONFIGS with paths in `pydyn--source-config-alist'."
  (let ((configs (pydyn--prepare-configs configs)))
    (if (not pydyn--source-config-alist)
        configs
      (seq-filter #'pydyn--config-path-not-exists configs))))


(defun pydyn--config-same-name-p (config other)
  "Return non-nil if path in CONFIG is equal to path in OTHER."
  (equal (plist-get config :name)
         (plist-get other :name)))


(defun pydyn--config-name-exist (config)
  "Return non-nil if name in CONFIG exists in `pydyn--source-config-alist'."
  (seq-contains-p pydyn--source-config-alist
                  config
                  #'pydyn--config-same-name-p))


(defun pydyn--check-duplicate-names (configs)
  "Throw user error with CONFIGS name exists in `pydyn--source-config-alist'."
  (let ((dupl-names (seq-filter #'pydyn--config-name-exist configs)))
    (unless (seq-empty-p dupl-names)
      (user-error "Duplicate names exists %S"
                  (pydyn--config-names dupl-names)))))


(defun pydyn--merge-configs (configs)
  "Return CONFIGS in config format."
  (sort (seq-uniq (append pydyn--source-config-alist configs)
                  #'pydyn--config-same-path-p)
        (lambda (config other)
          (string-lessp (pydyn--config-path-get config)
                        (pydyn--config-path-get other)))))


(defun pydyn--config-changed-p (configs)
  "Return non-nil if CONFIGS is different from `pydyn--source-config-alist'."
  (or (seq-every-p #'pydyn--config-path-exists configs)
      (not (pydyn-source-config-exists))
      (not pydyn--source-config-alist)))


;;;###autoload
(defun pydyn-source-config-set (configs)
  "Set `pydyn--source-config-alist' to CONFIGS."
  (when configs
    (when (and (pydyn-source-config-exists)
               (not pydyn--source-config-alist))
      (pydyn-source-config-load))
    (let ((configs (pydyn--configs-path-not-exists configs))
          (save (not pydyn--source-config-alist)))
      (pydyn--check-duplicate-names configs)
      (when pydyn--source-config-alist
        (setq configs (pydyn--merge-configs configs)))
      (setq save (or save (pydyn--config-changed-p configs)))
      (setq pydyn--source-config-alist configs)
      (when save
        (message "Save source config")
        (pydyn-source-config-write)))))


(defun pydyn--config-plist-get (line)
  "Return plist frpom properties and values in LINE."
  (let ((plist (list))
        (prop-values (string-split line "\\(, \\)" t)))
    (cl-loop for (prop value)
             on prop-values by #'cddr
             do (let ((prop (intern prop)))
                  (when (eq prop :path)
                    (setq value (pydyn--dir-path-of value)))
                  (setq plist (plist-put plist prop value))))
    plist))


(defun pydyn--config-read ()
  "Return source root paths from current buffer."
  (seq-map #'pydyn--config-plist-get
           (string-split (buffer-string) "[\n]" t)))


(defun pydyn-source-config-load (&optional force)
  "Load and restore `pydyn--source-config-alist' from file.
If FORCE is non-nil force load and set config."
  (interactive (list t))
  (let ((file (pydyn-source-config-path)))
    (when (and (file-exists-p file)
               (or (not pydyn-source-config-restored) force))
      (with-current-buffer (get-buffer-create pydyn-config-save-and-load-buffer)
        (goto-char (point-min))
        (delete-region (point-min) (point-max))
        (condition-case nil
            (insert-file-contents file)
          (file-error (message "Can't read %s" file)))
        (goto-char (point-min))
        (delete-line)
        (when (length> (buffer-string) 0)
          (condition-case nil
              (progn
                (setq pydyn-source-config-restored t)
                (pydyn-source-config-set (pydyn--config-read)))
            (setq pydyn-source-config-restored nil))
          (kill-buffer (current-buffer)))))))


(defun pydyn--path-is-child-of-p (config path)
  "Return non-nil if PATH start with path in CONFIG."
  (s-starts-with? (pydyn--config-path-get config)
                  (pydyn--dir-path-of path)))


(defun pydyn-is-source? (file-path)
  "Return non-nil if FILE-PATH is subpath of a `pydyn--source-config-alist'."
  (unless pydyn--source-config-alist
    (pydyn-source-config-load))
  (unless pydyn--source-config-alist
    (user-error "No source config exists"))
  (seq-some (lambda (config)
              (pydyn--path-is-child-of-p config file-path))
            pydyn--source-config-alist))


(defun pydyn-source-config-of (file-path)
  "Return source config for FILE-PATH or nil if not exists."
  (seq-find (lambda (config) (pydyn--path-is-child-of-p config file-path))
            pydyn--source-config-alist))


(defun pydyn--path-is-ext? (file-path extensions)
  "Return non-nil if FILE-PATH extension exists in EXTENSIONS."
  (when file-path
    (let ((file-ext (file-name-extension file-path)))
      (seq-some (lambda (ext) (progn (s-ends-with? ext file-ext)))
                (ensure-list extensions)))))


;;;###autoload
(defun pydyn-dynamo-is-script? (&optional file-path)
  "Return non-nil if FILE-PATH is Dynamo SCRIPT file."
  (pydyn--path-is-ext? (pydyn-path-get file-path)
                       pydyn-dynamo-script-ext))


;;;###autoload
(defun pydyn-dynamo-is-custom? (&optional file-path)
  "Return non-nil if FILE-PATH is Dynamo CUSTOM NODE file."
  (pydyn--path-is-ext? (pydyn-path-get file-path)
                       pydyn-dynamo-custom-ext))


(defun pydyn-is-dynamo? (&optional file-path)
  "Return non-nil if FILE-PATH is either a Dynamo SCRIPT or CUSTOM NODE file."
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
  "Return non-nil when FILE-PATH is sub-path of any config and is dynamo file.
`pydyn-is-source?' and `pydyn-is-dynamo?' with FILE-PATH argument
and both must return non-nil."
  (let ((file-path (pydyn-path-get file-path)))
    (and (pydyn-is-dynamo? file-path)
         (pydyn-is-source? file-path))))



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
  (and engine (equal engine pydyn-python-3-engine)))


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
    (and (pydyn-is-python? file-path)
         (or (pydyn-python-local-var-set-p file-path)
             (pydyn-is-export? file-path)))))


(defun pydyn--files-in-directory (directory extension &optional recursive)
  "Return files of EXTENSION in DIRECTORY, RECURSIVE search if non-nil."
  (let ((files (ensure-list (list))))
    (when (file-exists-p directory)
      (dolist (ext (ensure-list extension))
        (push (directory-files-recursively
               directory (format "\.%s" ext) recursive)
              files)))
    (seq-reverse (flatten-list files))))


(defun pydyn--folder-name-for (path)
  "Return PATH without path of source config."
  (let ((config (pydyn-source-config-of path)))
    (format "%s/" (plist-get config :name))))


(defun pydyn--path-export-folder-for-source (path)
  "Return translated EXPORT directory of PATH."
  (concat pydyn-export-root
          (pydyn--folder-name-for path)
          (pydyn-remove-config-path
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
  (let* ((source-paths (pydyn--source-config-paths))
         (files (seq-map (lambda (source)
                           (pydyn-dynamo-files-in source t))
                         source-paths)))
    (pydyn-selection-get files "Select Dynamo file: "
                         source-paths)))


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
