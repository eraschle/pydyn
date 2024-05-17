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
;; test for `pydyn-utils`
;;
;;; Code:

(require 'ert)
(require 'pydyn-utils)

(ert-deftest pydyn-buffer-preview-name-test ()
  (should
   (string= "** Preview FILENAME **"
            (pydyn-buffer-preview-name "FILENAME")))
  (should
   (let ((pydyn-buffer-preview-prefix "VORSCHAU"))
     (string= "** VORSCHAU FILENAME **"
              (pydyn-buffer-preview-name "FILENAME"))))
  )

(ert-deftest pydyn-buffer-preview-get-test ())
(let ((preview (pydyn-buffer-preview-get "/home/user/path/to/file")))
  (should (bufferp preview))
  (should (string= (buffer-name preview)
                   "** Preview /home/user/path/to/file **")))

(ert-deftest pydyn-buffer-preview-name?-test ()
  ;; Current buffer is not a preview buffer
  (should-not (pydyn-buffer-preview-name?))
  (with-current-buffer (pydyn-buffer-preview-get "preview file")
    ;; Buffer from `pydyn-buffer-preview-get' must be a preview buffer
    (should (pydyn-buffer-preview-name?))))

(ert-deftest pydyn-save-buffer-test ()
  ;; TODO: How to test `pydyn-save-buffer'
  )

(ert-deftest pydyn-buffer-substring-test ()
  ;; TODO: How to test `pydyn-buffer-substring' useful
  )

(ert-deftest pydyn-while-search-test ()
  ;; TODO: How to test `pydyn-while-search' useful
  )

(ert-deftest pydyn-while-regex-test ()
  ;; TODO: How to test `pydyn-while-regex' useful
  )

(ert-deftest pydyn--name-get-test ()
  (let* ((dir-path "/some/path/of/directory/")
         (file-name "some-file-name.el")
         (file-path (concat dir-path file-name)))
    (should (equal (pydyn--name-get file-path)
                   (pydyn--name-get file-path
                                    dir-path)))))

(ert-deftest pydyn-not-processing-test ()
  ;; non-nil if process not is running
  (should (pydyn-not-processing?))
  ;; nil if process not is running
  (let ((pydyn-processing t))
    (should-not (pydyn-not-processing?))))
