;;; keepass-mode-plus.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: May 15, 2022
;; Modified: May 15, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary: Enhances the default keepass-mode to add a global search capability.
;;; Also Adds the functionality to automatically expire passwords after certain period of time
;;

;;; Code:

(require 'keepass-mode)

(defvar +keepass-password-expiry (* 10 60)
  "Expire keepass password after seconds")

(defvar +keepass-completion 'consult
  "Completion framework to use for completion.
'consult and 'ivy are available values.")

(defvar keepass-password-file (expand-file-name "keepass.kbdx" "~")
  "Location of the password file to quickly jump on it")

(defun keepass-quick-switch()
  (interactive)
  ;; From core-secrets
  (let ((buf (find-file-noselect keepass-password-file)))
    (with-current-buffer buf
      (when (string= "" keepass-mode-password)
        (+keepass-set-password))
      (+keepass-start-expiry-timer)
      (+keepass-search))))

(defun +keepass-search()
  (interactive)
  (if (eq +keepass-completion 'ivy)
      (counsel-keepass)
    (consult-keepass)))

(defun counsel-keepass()
  (interactive)
  (if (not (package-installed-p 'ivy))
      (error "Ivy not installed"))
  (require 'ivy)
  (ivy-read "Search entry: "
            (+keepass-list-all)
            :require-match t
            :action '(1
                      ("p" +keepass-copy-password "Copy password")
                      ("u" +keepass-copy-username "Copy username")
                      ("o" +keepass-open-entry "Open entry"))
            :caller 'counsel-keepass))

(defun consult-keepass()
  (interactive)
  (if (not (package-installed-p 'consult))
      (error "Consult not installed"))
  (require 'consult)
  (consult-keepass-embark)
  (+keepass-copy-password
   (consult--read (+keepass-list-all)
                  :prompt "Search entry: "
                  :category 'keepass-entry
                  :require-match t)))


(defun +keepass-open-entry(entry)
  "Open entry in keepass buffer."
  (let ((buff (get-file-buffer keepass-password-file)))
    (if buff
        (with-current-buffer buff
          (let ((keepass-mode-group-path ""))
            (keepass-mode-show entry))))))

(defun +keepass-copy-password(entry)
  "Copy password for the entry"
  (let ((buff (get-file-buffer keepass-password-file)))
    (if buff
        (with-current-buffer buff
          (kill-new (keepass-mode-get "Password" entry))
          (message "Password for '%s' copied to kill-ring" entry)))))

(defun +keepass-copy-username(entry)
  "Copy username for the entry"
  (let ((buff (get-file-buffer keepass-password-file)))
    (if buff
        (with-current-buffer buff
          (kill-new (keepass-mode-get "UserName" entry))
          (message "Username for '%s' copied to kill-ring" entry)))))

(defun +keepass-list-all()
  (seq-filter
   (lambda (el)
     (not (equal "" el)))
   (+keepass-locate "/")))

(defun +keepass-locate(term)
  "Search using keepass"
  (cl-delete-if
   (lambda (k) (string-match-p "^[^/]" k))
   (split-string
    (shell-command-to-string (keepass-mode-command term "locate"))
    "\n")))

(defvar +keepass--expiry-timer nil)
(defun +keepass-start-expiry-timer()
  (if +keepass--expiry-timer
      (cancel-timer +keepass--expiry-timer))
  (setq +keepass--expiry-timer (run-with-timer +keepass-password-expiry nil #'+keepass-reset-password)))
(defun +keepass-reset-password()
  (interactive)
  (let ((buff (get-file-buffer keepass-password-file)))
    (if buff
        (with-current-buffer buff
          (setq-local keepass-mode-password "")
          (message "Keepass password reset done")))))

(defun +keepass-verify-password(password)
  (let ((buff (get-file-buffer keepass-password-file)))
    (if buff
        (with-current-buffer buff
          (if (string-match-p "Invalid credentials" (shell-command-to-string (keepass-mode-command "" "db-info")))
              nil
            t)))))

(defun +keepass-set-password()
  (let ((buff (get-file-buffer keepass-password-file)))
    (if buff
        (with-current-buffer buff
          (setq-local keepass-mode-password (keepass-mode-ask-password))
          (if (not (+keepass-verify-password keepass-mode-password))
              (progn
                (setq-local keepass-mode-password "")
                (error "Invalid password")))))))

;;;###autoload
(defun consult-keepass-embark()
  "Adds embark actions to the keepass entry"
  (if (package-installed-p 'embark)
      (eval-after-load 'embark
        '(progn
           (embark-define-keymap embark-keepass-actions
             "Keymap for actions for keepass entry"
             ("p" +keepass-copy-password)
             ("u" +keepass-copy-username)
             ("o" +keepass-open-entry))
           (add-to-list 'embark-keymap-alist '(keepass-entry . embark-keepass-actions))))))

(provide 'keepass-mode-plus)
;;; keepass-mode-plus.el ends here
