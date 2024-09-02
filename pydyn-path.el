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
(require 'pydyn-config)

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


(defcustom pydyn-source-is-file-alist nil
  "Source root path of Dynamo library."
  :type 'list
  :group 'pydyn)


(defun pydyn-path-get (&optional file-path)
  "Return FILE-PATH if non-nil, otherwise path of current buffer."
  (or file-path buffer-file-name))


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


(defun pydyn-is-dynamo-source? (&optional file-path)
  "Return non-nil when FILE-PATH is sub-path of any config and is dynamo file.
`pydyn-is-source?' and `pydyn-is-dynamo?' with FILE-PATH argument
and both must return non-nil."
  (let ((file-path (pydyn-path-get file-path)))
    (and (pydyn-is-dynamo? file-path)
         (pydyn-config-by-path file-path))))


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
             (pydyn-config-is-export? file-path)))))


(defun pydyn--files-in-directory (directory extension &optional recursive)
  "Return files of EXTENSION in DIRECTORY, RECURSIVE search if non-nil."
  (let ((files (ensure-list (list))))
    (when (file-exists-p directory)
      (dolist (ext (ensure-list extension))
        (push (directory-files-recursively
               directory (format "\.%s" ext) recursive)
              files)))
    (seq-reverse (flatten-list files))))


(defun pydyn-path-python-files-in (path &optional recursive)
  "Return python files in PATH, RECURSIVE search if non-nil."
  (pydyn--files-in-directory (pydyn--path-export-folder-for path)
                             pydyn-python-extension
                             recursive))


(defun pydyn-path-dynamo-files-in (directory &optional recursive)
  "Return Dynamo files in DIRECTORY, RECURSIVE search if non-nil."
  (let ((files (pydyn--files-in-directory directory
                                          (list pydyn-dynamo-custom-ext
                                                pydyn-dynamo-script-ext)
                                          recursive)))
    (when pydyn-source-is-file-alist
      (dolist (func pydyn-source-is-file-alist)
        (setq files (apply func files))))
    files))


(defun pydyn-path-export-path-for (config)
  "Return export path for CONFIG."
  (concat pydyn-config-export-path
          (pydyn-config-source-name config) "/"))


(defun pydyn--path-export-folder-for-source (path)
  "Return translated EXPORT directory of PATH."
  (let ((config (pydyn-config-by-path path)))
    (concat (pydyn-path-export-path-for config)
            (string-remove-prefix (pydyn-config-source-path config)
                                  (file-name-parent-directory path))
            (file-name-base path) "/")))


(defun pydyn--path-export-folder-for (file-path)
  "Return export path for FILE-PATH."
  (if (pydyn-config-is-export? file-path)
      (if (file-directory-p file-path)
          file-path
        (file-name-directory file-path))
    (pydyn--path-export-folder-for-source file-path)))


(defun pydyn-path-export-folder (node-path)
  "Return export directory path for NODE-PATH. Create directory if not exist."
  (let ((export-dir (pydyn--path-export-folder-for node-path)))
    (unless (file-exists-p export-dir)
      (make-directory export-dir t))
    export-dir))


(defvar pydyn-path-regex-illegal-char "[<>:\"/\\|?* ]+"
  "Regex to match illegal characters in path.")


(defvar pydyn-path-name-separator "_"
  "Replacement for illegal characters in path.")


(defun pydyn-path--sanitize-name (name)
  "Return cleaned NAME from illegal characters."
  (let ((name (replace-regexp-in-string
               pydyn-path-regex-illegal-char
               pydyn-path-name-separator name))
        (trim (format "[%s]*" pydyn-path-name-separator)))
    (string-trim name trim trim)))


(defun pydyn--path-py-abbrev-of (node-info)
  "Return abbrev from python engine in NODE-INFO."
  (let ((engine (plist-get node-info :engine)))
    (cond ((pydyn-is-python-3? engine) "py3")
          (t "py2"))))


(defun pydyn-path-export-name (node-info)
  "Return export name created from NODE-INFO."
  (s-join pydyn-path-name-separator ;; join names together with _
          (list (pydyn-path--sanitize-name (plist-get node-info :name))
                (pydyn--path-py-abbrev-of node-info)
                (pydyn-path--sanitize-name (plist-get node-info :node-id)))))


(defun pydyn-path-export-file-name (node-info)
  "Return export file name created from NODE-INFO."
  (let ((file-name (pydyn-path-export-name node-info)))
    (s-downcase (concat file-name "." pydyn-python-extension))))


(provide 'pydyn-path)
;;; pydyn-path.el ends here
