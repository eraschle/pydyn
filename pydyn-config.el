;;; pydyn-config.el --- package source configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Erich Raschle
;;
;; Author: Erich Raschle <erichraschle@gmail.com>
;; Maintainer: Erich Raschle <erichraschle@gmail.com>
;; Created: September 01, 2024
;; Modified: September 01, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/elyo/pydyn-config
;; Package-Requires: ((emacs "29.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; TODO
;;
;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defcustom pydyn-config-file-name "config.pydyn"
  "Name of the file to save source and export paths."
  :type 'string
  :group 'pydyn)


(defcustom pydyn-config-export-path nil
  "Path to export source files."
  :type 'string
  :group 'pydyn)


(defvar pydyn-config-io-buffer  " *Pydyn source file*"
  "Buffer name for saving and loading config file.")


(defvar pydyn-config--alist nil
  "List of loaded source configuration.")


(defun pydyn-config-file-exists ()
  "Return path to saved source root paths."
  (file-exists-p (pydyn-config-file-path-get)))


(defun pydyn-config-file-path-get ()
  "Return path to config and save file of this package."
  (concat pydyn-config-export-path pydyn-config-file-name))


(defun pydyn-config-as-dir-path (path)
  "Return absolute PATH."
  (let ((path (expand-file-name path)))
    (when (file-name-extension path)
      (setq path (file-name-parent-directory path)))
    (file-name-as-directory path)))


(defun pydyn-config-is-export? (path)
  "Return non-nil if PATH is an `pydyn-config-export-path'."
  (unless pydyn-config-export-path
    (user-error "No export root path defined"))
  (and path (string-prefix-p
             (pydyn-config-as-dir-path pydyn-config-export-path)
             (pydyn-config-as-dir-path (or path buffer-file-name)))))


(defun pydyn-config-source-name (config)
  "Return name of CONFIG."
  (plist-get config :name))


(defun pydyn-config-source-path (config)
  "Return name of CONFIG."
  (file-name-as-directory (plist-get config :path)))


(defun pydyn-config-valid-p (config)
  "Return non-nil if CONFIG is valid."
  (and (plist-get config :name) (plist-get config :path)))


(defun pydyn-config-source-name-equal? (config other)
  "Return non-nil if source name in CONFIG is equal to path in OTHER."
  (equal (pydyn-config-source-name config)
         (pydyn-config-source-name other)))


(defun pydyn-config-source-name-exists? (config)
  "Return non-nil if source name in CONFIG exists in `pydyn-config--alist'."
  (seq-contains-p pydyn-config--alist config
                  #'pydyn-config-source-name-equal?))


(defun pydyn-config-source-path-equal? (config other)
  "Return non-nil if path in CONFIG is equal to path in OTHER."
  (equal (pydyn-config--path-get config)
         (pydyn-config--path-get other)))


(defun pydyn-config-source-path-exists? (config)
  "Return non-nil if path in CONFIG exists in `pydyn-config--alist'."
  (seq-contains-p pydyn-config--alist config
                  #'pydyn-config-source-path-equal?))


(defun pydyn-config--source-equal? (config other)
  "Return non-nil if CONFIG is equal to OTHER."
  (and (pydyn-config-source-name-equal? config other)
       (pydyn-config-source-path-equal? config other)))


(defun pydyn-config-source-equal? (config)
  "Return non-nil if CONFIG exists in `pydyn-config--alist'."
  (seq-contains-p pydyn-config--alist config
                  #'pydyn-config--source-equal?))


;;; Save config to file

(defun pydyn-config--insert-coding (coding)
  "Insert CODING symbol of the coding-system in which the file is encoded."
  (when (memq (coding-system-base coding) '(undecided prefer-utf-8))
    (setq coding 'utf-8-emacs))
  (insert (format ";;;; -*- coding: %S; mode: lisp-data -*-\n\n"
                  (coding-system-base coding))))


(defun pydyn-config--write-config-get (config)
  "Return source root CONFIG in string format."
  (format "%s, %s, %s, %s\n"
          :path (string-trim (pydyn-config-source-path config))
          :name (string-trim (pydyn-config-source-name config))))


(defun pydyn-config-write ()
  "Write `pydyn-config--alist' to file."
  (interactive)
  (unless pydyn-config--alist
    (user-error "No config exists"))
  (let ((file (pydyn-config-file-path-get)))
    (with-current-buffer (get-buffer-create pydyn-config-io-buffer t)
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (let ((coding-system-for-write (or coding-system-for-write
                                         'utf-8-emacs)))
        (dolist (config pydyn-config--alist)
          (insert (pydyn-config--write-config-get config)))
        (with-coding-priority '(utf-8-emacs)
          (setq coding-system-for-write (select-safe-coding-system
                                         (point-min) (point-max)
                                         (list t coding-system-for-write))))
        (goto-char (point-min))
        (pydyn-config--insert-coding coding-system-for-write)
        (condition-case nil
            (progn
              (write-file file)
              (message "PYDYN config saved to %s" file))
          (file-error (message "Can't write %s" file)))
        (kill-buffer (current-buffer))))))


;;; Load config from file

(defun pydyn-config--plist-get (line)
  "Return plist from properties and values in LINE."
  (let ((plist (list))
        (prop-values (string-split line "\\(, \\)" t)))
    (cl-loop for (prop value)
             on prop-values by #'cddr
             do (let ((prop (intern prop)))
                  (when (eq prop :path)
                    (setq value (pydyn-config-as-dir-path value)))
                  (setq plist (plist-put plist prop value))))
    plist))


(defun pydyn-config--create-config ()
  "Return list of source root paths from `current-buffer'."
  (seq-map #'pydyn-config--plist-get
           (string-split (buffer-string) "[\n]" t)))


(defun pydyn-config--read-file ()
  "Return source root paths from current buffer."
  (let ((file (pydyn-config-file-path-get)))
    (goto-char (point-min))
    (delete-region (point-min) (point-max))
    (condition-case nil
        (insert-file-contents file)
      (file-error (message "Can't read %s" file)))
    (goto-char (point-min))
    (delete-line)))


(defun pydyn-config-load (&optional force)
  "Load and restore `pydyn-config--alist' from file.
If FORCE is non-nil force load and set config."
  (interactive (list t))
  (when (and (pydyn-config-file-exists)
             (or (not pydyn-config--alist) force))
    (with-current-buffer (get-buffer-create pydyn-config-io-buffer)
      (pydyn-config--read-file)
      (setq pydyn-config--alist (pydyn-config--create-config))
      (kill-buffer (current-buffer)))))


;;;###autoload
(defun pydyn-config-add (configs &optional not-report)
  "Add CONFIGS to `pydyn-config--alist' and report unless NOT-REPORT."
  (when configs
    (when (and (pydyn-config-file-exists)
               (not pydyn-config--alist))
      (pydyn-config-load))
    (setq configs (pydyn-config--check-and-report
                   (pydyn-config--prepare-configs configs)
                   not-report))
    (when (pydyn-config--changed-p configs)
      (setq pydyn-config--alist (append pydyn-config--alist configs))
      (pydyn-config-write))))


(defun pydyn-config--path-get (config)
  "Return source path of CONFIG."
  (unless config
    (setq config (list :path ""))
    (message "Source root config was nil, set to empty path"))
  (pydyn-config-as-dir-path (pydyn-config-source-path config)))


(defun pydyn-config--set-absolute-path-in (config)
  "Return CONFIG with absolute path."
  (let ((path (pydyn-config-as-dir-path
               (pydyn-config-source-path config))))
    (unless (equal path (pydyn-config-source-path config))
      (setq config (plist-put config :path path))))
  config)


(defun pydyn-config--ensure-absolute-path (configs)
  "Return CONFIGS in config format."
  (seq-map #'pydyn-config--set-absolute-path-in configs))


(defun pydyn-config--valid-configs (configs)
  "Return CONFIGS in config format."
  (seq-filter #'pydyn-config-valid-p configs))


(defun pydyn-config--prepare-configs (configs)
  "Return CONFIGS in config format.
Remove invalid config and set absolute path."
  (let ((configs (pydyn-config--valid-configs configs)))
    (seq-map #'pydyn-config--set-absolute-path-in configs)))


(defun pydyn-config-source-names (&optional configs with_root)
  "Return source names in CONFIGS or `pydyn-config--alist'.
If WITH_ROOT is non-nil add config of `pydyn-config-export-path'."
  (seq-map (lambda (config) (pydyn-config-source-name config))
           (or configs (pydyn-config--select-config-list with_root))))


(defun pydyn-config--report-message-for (config name-length)
  "Return report message for CONFIG with NAME-LENGTH of source names."
  (format "-> %s [%s]"
          (string-pad (pydyn-config-source-name config) name-length)
          (pydyn-config-source-path config)))


(defun pydyn-config--report-message (configs &optional offset)
  "Create report message for CONFIGS with optional OFFSET for source names."
  (let* ((name-length (pydyn-config--source-max-length configs offset))
         (report-lines (seq-map
                        (lambda (config)
                          (pydyn-config--report-message-for config name-length))
                        configs)))
    (string-join report-lines "\n")))


(defun pydyn-config--do-check-and-report (configs check-func report-message)
  "Execute CHECK-FUNC on CONFIGS and report with REPORT-MESSAGE."
  (let ((report-configs (seq-filter check-func configs)))
    (when (and report-message (not (seq-empty-p report-configs)))
      (message "%s:\n%s" report-message
               (pydyn-config--report-message report-configs)))
    (seq-remove check-func configs)))


(defun pydyn-config--check-and-report-equal (configs not-report)
  "Report if CONFIGS already exists in `pydyn-config--alist'.
If NOT-REPORT is non-nil do not report."
  (pydyn-config--do-check-and-report
   configs #'pydyn-config-source-equal?
   (if not-report nil "Source already exists")))


(defun pydyn-config--check-and-report-equal-name (configs not-report)
  "Report if source name in CONFIGS already exists in `pydyn-config--alist'.
If NOT-REPORT is non-nil do not report."
  (pydyn-config--do-check-and-report
   configs #'pydyn-config-source-name-exists?
   (if not-report nil "Source name already exists")))


(defun pydyn-config--check-and-report-equal-path (configs not-report)
  "Report if path in CONFIGS already exists in `pydyn-config--alist'.
If NOT-REPORT is non-nil do not report."
  (pydyn-config--do-check-and-report
   configs #'pydyn-config-source-path-exists?
   (if not-report nil "Source path already exists")))


(defun pydyn-config--check-and-report (configs not-report)
  "Check if CONFIGS can be added to `pydyn-config--alist'.
If NOT-REPORT is non-nil do not report."
  (dolist (check-func '(pydyn-config--check-and-report-equal
                        pydyn-config--check-and-report-equal-name
                        pydyn-config--check-and-report-equal-path))
    (setq configs (funcall check-func configs not-report)))
  configs)


(defun pydyn-config--changed-p (configs)
  "Return non-nil if CONFIGS is different from `pydyn-config--alist'."
  (or (not (seq-every-p #'pydyn-config-source-path-exists? configs))
      (not (pydyn-config-file-exists))
      (not pydyn-config--alist)))


(defun pydyn-config-select-directory (prompt directory)
  "Return directory selected by the user with PROMPT displayed.
DIRECTORY is the default directory."
  (pydyn-config-as-dir-path (read-directory-name prompt directory)))


(defun pydyn-config--source-path-ask (file-path)
  "Ask user for new root source path for FILE-PATH."
  (let ((path (pydyn-config-select-directory
               (format "Select new source root for %S: " file-path)
               (pydyn-config-as-dir-path file-path))))
    (unless (pydyn-config-is-subpath-p path file-path)
      (user-error "%S is not a sub-path of %S" file-path path))
    (when (pydyn-config-is-known-source-p path)
      (user-error "%S is already a source config" path))
    path))


(defun pydyn-config--source-name-possible? (name)
  "Return non-nil if NAME is a possible source name."
  (not (seq-contains-p (pydyn-config-source-names) name)))


(defun pydyn-config--source-names-from (path)
  "Return possible source name from PATH."
  (let ((existing (pydyn-config-source-names)))
    (seq-filter (lambda (name) (not (seq-contains-p existing name)))
                (seq-rest (file-name-split path)))))


(defun pydyn-config--source-name-ask (path)
  "Ask user for source name for PATH."
  (let ((name (completing-read
               (format "Select or enter new source name for %S: " path)
               (pydyn-config--source-names-from path)
               #'pydyn-config--source-name-possible?)))
    (unless (pydyn-config--source-name-possible? name)
      (user-error "%S is used in an other source config" name))
    name))


(defun pydyn-config-add-config (file-path)
  "Add source root path for FILE-PATH to `pydyn-config-alist'."
  (let* ((file-path (file-name-parent-directory file-path))
         (path (pydyn-config--source-path-ask file-path))
         (name (pydyn-config--source-name-ask path)))
    (pydyn-config-add (list (list :path path :name name)))
    (pydyn-config-write)))


(defun pydyn-config--source-max-length (&optional configs offset with_root)
  "Return max length of source names in CONFIGS with optional OFFSET.
if OFFSET is nil it is set to 5. If WITH_ROOT is non-nil add config of
`pydyn-config-export-path'."
  (+ (or offset 2)
     (seq-max (seq-map #'length (pydyn-config-source-names configs with_root)))))


(defun pydyn-config--path-for-completing (config)
  "Return source path for selection of CONFIG."
  (let ((path (pydyn-config-source-path config))
        (mount-regex "/mnt/\\([a-zA-Z]+\\)"))
    (when (string-match mount-regex path)
      (setq path (replace-match
                  (format "%s:" (upcase (match-string 1 path)))
                  t nil path)))
    path))


(defun pydyn-config--completing-read-get (config name-length)
  "Return source name and path for completion of CONFIG.
NAME-LENGTH is the length of source names."
  (concat (string-pad (pydyn-config-source-name config)
                      name-length)
          (pydyn-config--path-for-completing config)))


(defun pydyn-config-root-source ()
  "Return config for `pydyn-config-export-path'."
  (list :path (pydyn-config-as-dir-path pydyn-config-export-path)
        :name "Export-Root"))


(defun pydyn-config-is-root-source? (config)
  "Return non-nil if CONFIG is the config of `pydyn-config-export-path'."
  (pydyn-config-source-name-equal? config (pydyn-config-root-source)))


(defun pydyn-config--select-config-list (&optional with_root)
  "Return list of source config for selection.
If WITH_ROOT is non-nil add config of `pydyn-config-export-path'."
  (if with_root
      (append pydyn-config--alist (list (pydyn-config-root-source)))
    pydyn-config--alist))


(defun pydyn-config--select-list (&optional offset with_root)
  "Return list of source names with optional OFFSET.
If WITH_ROOT is non-nil add config of `pydyn-config-export-path'."
  (let ((name-length (pydyn-config--source-max-length nil offset with_root)))
    (seq-map (lambda (source)
               (pydyn-config--completing-read-get source name-length))
             (pydyn-config--select-config-list with_root))))


(defun pydyn-config-by-name (name &optional with_root)
  "Return source config plist by NAME.
If WITH_ROOT is non-nil add config of `pydyn-config-export-path'."
  (seq-find (lambda (source) (string= name (pydyn-config-source-name source)))
            (pydyn-config--select-config-list with_root)))


;;;###autoload
(defun pydyn-config-select-config (&optional with_root)
  "Return source config plist selected by the user.
If WITH_ROOT is non-nil add config of `pydyn-config-export-path'."
  (let ((config (completing-read
                 "Select source: "
                 (pydyn-config--select-list nil with_root)
                 nil t)))
    (pydyn-config-by-name (seq-first (string-split config))
                          with_root)))


;;;###autoload
(defun pydyn-config-select-config-path (&optional with_root)
  "Return source config plist selected by the user.
If WITH_ROOT is non-nil add config of `pydyn-config-export-path'."
  (let ((config (pydyn-config-select-config with_root)))
    (pydyn-config-source-path config)))


(defun pydyn-config-is-subpath-p (path subpath)
  "Return non-nil if SUBPATH is sub-path of PATH or equal."
  (or (string-prefix-p path subpath)
      (string-equal path subpath)))


(defun pydyn-config-is-source-p (config path)
  "Return non-nil if PATH start with path in CONFIG."
  (pydyn-config-is-subpath-p
   (pydyn-config-source-path config) path))


(defun pydyn-config-is-known-source-p (file-path)
  "Return non-nil if FILE-PATH is sub-path of a `pydyn-config--alist'."
  (unless pydyn-config--alist
    (pydyn-config-load))
  (unless pydyn-config--alist
    (user-error "No config exists"))
  (pydyn-config-by-path file-path))


(defun pydyn-config-by-path (file-path)
  "Return source config for FILE-PATH or nil if not exists."
  (seq-find (lambda (config)
              (pydyn-config-is-source-p config file-path))
            pydyn-config--alist nil))


(defun pydyn-config-by-export-path (export-path)
  "Return source config for EXPORT-PATH or nil if not exists."
  (when (pydyn-config-is-export? export-path)
    (let* ((subpath (string-remove-prefix pydyn-config-export-path export-path))
           (source-name (seq-first (file-name-split subpath))))
      (pydyn-config-by-name source-name))))


;;;###autoload
(defun pydyn-config-remove-source-path (file-path)
  "Remove source sub-path for FILE-PATH if exists, otherwise return FILE-PATH."
  (let ((config (pydyn-config-by-path file-path)))
    (if config
        (string-remove-prefix (pydyn-config-source-path config) file-path)
      file-path)))


(provide 'pydyn-config)
;;; pydyn-config.el ends here
