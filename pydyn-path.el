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


(defcustom pydyn-source-root nil
  "Source root path of Dynamo library."
  :type 'string
  :group 'pydyn)


(defcustom pydyn-export-root nil
  "Export root path of Python files."
  :type 'string
  :group 'pydyn)


(defun pydyn-path-get (&optional file-path)
  "Return FILE-PATH buffer if non-nil, otherwise path of current buffer."
  (or file-path buffer-file-name))


(defun pydyn-is-export? (path)
  "Return non-nil if PATH is an `pydyn-source-root'."
  (s-starts-with? pydyn-export-root path))


(defun pydyn-is-source? (path)
  "Return non-nil if PATH is an `pydyn-export-root'."
  (s-starts-with? pydyn-source-root path))


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
  (or (pydyn-dynamo-is-script? file-path)
      (pydyn-dynamo-is-custom? file-path)))


;;;###autoload
(defun pydyn-is-dynamo-or-error (&optional file-path)
  "Throw user error if FILE-PATH is not a subpath of `pydyn-source-root'."
  (let ((file-path (pydyn-path-get file-path)))
    (unless (pydyn-is-dynamo? file-path)
      (user-error "%s is NOT a Dynamo file" (file-name-base file-path)))))


(defun pydyn-is-dynamo-source? (&optional file-path)
  "Return non-nil when FILE-PATH is inside of `pydyn-source-root'."
  (let ((file-path (pydyn-path-get file-path)))
    (and (pydyn-is-source? file-path) (pydyn-is-dynamo? file-path))))


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


;;;###autoload
(defun pydyn-is-python-export? (&optional file-path)
  "Return non-nil when FILE-PATH is inside of `pydyn-export-root'."
  (let ((file-path (pydyn-path-get file-path)))
    (and (pydyn-is-export? file-path)
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
          (string-replace pydyn-source-root ""
                          (file-name-directory path))
          (file-name-base path) "/"))


(defun pydyn--path-export-folder-for-export (path)
  "Return export path with added file-name as directory for PATH."
  (if (file-directory-p path) path (file-name-directory path)))


(defun pydyn--path-export-folder-for (file-path)
  "Return export path for FILE-PATH."
  (if (pydyn-is-export? file-path)
      (pydyn--path-export-folder-for-export file-path)
    (pydyn--path-export-folder-for-source file-path)))


(defun pydyn-python-files-in (node-path &optional recursive)
  "Return python files in NODE-PATH, RECURSIVE search if non-nil."
  (pydyn--files-in-directory (pydyn--path-export-folder-for node-path)
                             pydyn-python-extension
                             recursive))


(defun pydyn-dynamo-files-in (directory &optional recursive)
  "Return Dynamo files in DIRECTORY, RECURSIVE search if non-nil."
  (pydyn--files-in-directory directory
                             (list pydyn-dynamo-custom-ext
                                   pydyn-dynamo-script-ext)
                             recursive))


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
