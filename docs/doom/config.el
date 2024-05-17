;;; elyo/dynamo/config.el -*- lexical-binding: t; -*-

;; MIT License

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

(require 'python)
(require 'json)


;;;###autoload
(defun pydyn-python-mode-setup ()
  "Hook to setup ELYO PYTHON MINOR MODE."
  (when (pydyn-not-processing?)
    (pydyn-py-activate-venv)
    (pydyn-formatter-config)
    (pydyn-pyright-config)
    (when (or (pydyn-is-python-2? node-engine)
              (pydyn-is-python-source?))
      (pydyn-py2-config))
    (when (pydyn-is-python-3? node-engine)
      (pydyn-py3-config))
    (message "PYDYN-PYTHON setup")))


;;;###autoload
(defun pydyn-file-find-uuid-in-library ()
  "Return preview completion with references of custom node."
  (interactive)
  (when (and (pydyn-is-python-export?) (pydyn-dynamo-is-custom?))
    (let ((file-info (pydyn-dynamo-file-info)))
      (+vertico-file-search
        :query (concat "\"FunctionSignature\": \""
                       (plist-get file-info :node-id) "\",")
        :in pydyn-source-root
        :all-files nil))))

(defvar pydyn--python-disable-hook nil
  "Variable to store value of other mode hook.")

(defun pydyn-python-convert-start-h ()
  "Hooks run before convert process has started."
  (setq pydyn--python-disable-hook json-mode-hook
        json-mode-hook nil)
  ;; Will be overwritten/deleted after convert file
  (add-hook 'json-mode-hook #'pydyn-dynamo-json-config))

(defun pydyn-python-convert-end-h ()
  "Hooks run after convert process is finish."
  (when pydyn--python-disable-hook
    (setq json-mode-hook pydyn--python-disable-hook
          pydyn--python-disable-hook nil)))

(defvar pydyn-source "<Pfad Hauptordner Skripts>")
(defvar pydyn-export "<Pfad Hauptordner Python Export KEIN UNTERORDNER VON PYDYN-SOURCE>")


(use-package! pydyn-python
  :hook (python-mode . pydyn-python-mode-activate)
  :init
  (add-hook 'pydyn-python-mode-hook 'pydyn-python-mode-setup)
  (add-hook 'pydyn-process-start-hook 'pydyn-python-convert-start-h)
  (add-hook 'pydyn-process-end-hook 'pydyn-python-convert-end-h)
  (pydyn-formatter-config-add)
  :config
  (setq pydyn-venv-path "<Pfad zur VENV-Ordner>"
        pydyn-stub-path "<Pfad zum Stubs-Ordner>"
        pydyn-source-root (file-truename pydyn-source)
        pydyn-source-root  (file-truename pydyn-export)
        pydyn-python-intern-path ""
        pydyn-python-formatter-off "# autopep8: off"
        pydyn-python-formatter-on "# autopep8: on"
        pydyn-python-type-ignore "# type: ignore"
        pydyn-python-indent-width 4
        pydyn-python-line-length 110

        ;; if a row of exported python code contains a value in this list will be deleted
        pydyn-python-delete-contain (list "# Load the Python Standard"
                                          "Phython-Standard- und DesignScript-Bibliotheken laden"
                                          "# The inputs to this node will be stored"
                                          "Die Eingaben für diesen Block werden in Form einer Liste in den IN-Variablen gespeichert."
                                          "dataEnteringNode = IN"
                                          "Assign your output to the OUT variable."
                                          "Weisen Sie Ihre Ausgabe der OUT-Variablen zu.")

        ;; A type: ignore is added if one regex match
        pydyn-python-type-ignore-regex (list pydyn-dynamo-input-regex
                                             "import [a-zA-z ,]*Enum"
                                             "UnwrapElement(.*)"
                                             "clr.Reference\[[a-zA-Z]+\]\(\)"
                                             "List\[[a-zA-Z]+\]\(.*\)")

        ;; A type: ignore is added if a row contain a item in this list
        pydyn-python-type-ignore-contain (list "dataEnteringNode = IN"
                                               "clr.ImportExtensions(Revit.Elements)"
                                               "clr.ImportExtensions(Revit.GeometryConversion)"
                                               "Application.DocumentManager.MdiActiveDocument"
                                               "TransactionManager.Instance."
                                               "LabelUtils.GetLabelFor"
                                               "basestring"))

  (map! :map python-mode-map
        :localleader
        (:prefix ("y" . "elyo BIM")
         :desc "Add IGNORE to IN"              :n "i" #'pydyn-python-ignore-to-inputs
         :desc "Add IGNORE to Errors"          :n "I" #'pydyn-python-ignore-to-errors
         :desc "Remove bracket if-statement"   :n "b" #'pydyn-python-if-remove-bracket
         :desc "Clean buffer comments"         :n "c" #'pydyn-python-buffer-clean
         :desc "Formatter disable in region"   :n "f" #'pydyn-python-formatter-disable
         :desc "Formatter enable"              :n "F" #'pydyn-python-formatter-enable
         :desc "Goto Dynamo script node"       :n "g" #'pydyn-python-goto-dynamo-node
         :desc "Replace python in Node"        :n "n" #'pydyn-python-to-dynamo-node
         :desc "Replace python in Script"      :n "s" #'pydyn-python-to-dynamo-script
         :desc "Replace python in Folder"      :n "S" #'pydyn-python-to-dynamo-folder
         :desc "Tabify Buffer"                 :n "t" #'pydyn-buffer-tabify
         :desc "Untabify"                      :n "T" #'pydyn-buffer-untabify
         :desc "Toggle type: ignore"           :ni "y" #'pydyn-python-ignore-toggle
         )))


(defun pydyn-dynamo-format-maybe-inhibit-h ()
  (pydyn-is-dynamo?))

(defvar pydyn--dynamo-disable-hook nil
  "Variable to store value of other mode hook.")

(defun pydyn-dynamo-convert-start-h ()
  "Hook to disable other mode hooks before convert process."
  (setq pydyn--dynamo-disable-hook python-mode-hook
        python-mode-hook nil))

(defun pydyn-dynamo-convert-end-h ()
  "Hook to enable other mode hooks after convert process."
  (when pydyn--dynamo-disable-hook
    (setq python-mode-hook pydyn--dynamo-disable-hook
          pydyn--dynamo-disable-hook nil)))

(defun pydyn-dynamo-mode-setup ()
  "Hook to setup ELYO DYNAMO MINOR MODE."
  (message "SETUP PYDYN-DYNAMO"))


(use-package! pydyn-dynamo
  :hook (json-mode . pydyn-dynamo-mode-activate)
  :config
  (setq pydyn-source-root (file-truename pydyn-source)
        pydyn-export-root (file-truename pydyn-export))

  (add-to-list 'apheleia-inhibit-functions 'pydyn-dynamo-format-maybe-inhibit-h)
  (add-hook 'pydyn-dynamo-mode-hook 'pydyn-dynamo-mode-setup)
  (add-hook 'pydyn-process-start-hook 'pydyn-dynamo-convert-start-h)
  (add-hook 'pydyn-process-end-hook 'pydyn-dynamo-convert-end-h)

  (map! :map json-mode-map
        :localleader
        (:prefix ("y" . "elyo BIM")
         :desc "Script to Python"              :n "s" #'pydyn-dynamo-script-to-python
         :desc "Folder To Python"              :n "f" #'pydyn-dynamo-folder-to-python
         :desc "Jump to node"                  :n "j" #'pydyn-dynamo-jump-to-node
         :desc "Preview node"                  :n "p" #'pydyn-dynamo-python-code-preview
         :desc "Goto Python File"              :n "g" #'pydyn-dynamo-goto-python
         :desc "To Python Code at point"       :n "n" #'pydyn-dynamo-at-point-to-python
         :desc "Script: Delete orphan code"    :n "S" #'pydyn-dynamo-clean-orphan-code-file
         :desc "Folder: Delete orphan code"    :n "F" #'pydyn-dynamo-clean-orphan-code-folder)))
