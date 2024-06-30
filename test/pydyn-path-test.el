;;; pydyn-path-test.el --- Test for pydyn-path -*- lexical-binding: t; -*-

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
;; test for pydyn-path`
;;
;;; Code:

(require 'ert)
(require 'pydyn-path)


(describe "python version checks"
  (it "should not be python 2"
    (expect (pydyn-is-python-2? nil) :to-be nil))
  (it "with the same value as in `pydyn-python-2-engine' it should be t."
    (let ((pydyn-python-2-engine "py2"))
      (expect (pydyn-is-python-2? "py2") :to-be t))))

(ert-deftest pydyn-is-python-3-test ()
  (should-not (pydyn-is-python-3? nil))
  (let ((pydyn-python-3-engine "py3"))
    (should (pydyn-is-python-3? "py3"))))
