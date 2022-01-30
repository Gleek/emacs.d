;;; +sync-remote.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: January 28, 2022
;; Modified: January 28, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;;

;;; Code:
(require 'dtache)

(defcustom +sync-local-path nil
  "Local path to consider for syncing."
  :safe #'stringp
  :type '(string)
  :group '+sync-remote)
(defcustom +sync-remote-path nil
  "Remote path to sync to."
  :safe #'stringp
  :type '(string)
  :group '+sync-remote)
(defvar +sync-transfer-excludes '(".git/" "vendor/" "rendered/" "build/"))
(defvar +sync-listen-excludes '("^flycheck_.*$" ".git/" "rendered/" "build/" "^\\.#.*" "vendor/"))

(defvar +sync-rsync-exec (executable-find "rsync"))
(defvar +sync-fswatch-exec (executable-find "fswatch"))


(defun +sync-remote-start()
  "Start the syncing process."
  (interactive)
  (let* ((local-path (or +sync-local-path (read-string "Local Path: " default-directory)))
         (remote-path (or +sync-remote-path (read-string "Remote Path: ")))
         (sync-excludes (mapconcat (lambda(el) (concat "--exclude=" el))  +sync-transfer-excludes " "))
         (sync-command (mapconcat 'identity
                                  `(,+sync-rsync-exec "-zPa" ,sync-excludes "--delete" ,local-path ,remote-path)
                                  " "))
         (listen-excludes (mapconcat (lambda(el)
                                       (concat "--exclude=" el " "))
                                     +sync-listen-excludes))
         (listen-command (mapconcat 'identity
                                    `(,+sync-fswatch-exec ,listen-excludes ,local-path) " "))
         (final-command (format "%s; %s | while read f; do %s;done" sync-command listen-command sync-command)))
    (dtache-start-session final-command t)))


(provide '+sync-remote)
;;; +sync-remote.el ends here
