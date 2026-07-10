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

(defun +keepass-revert-buffer (&rest _ignored)
  "Refresh the current KeePass buffer after its database changes on disk."
  (set-visited-file-modtime)
  (keepass-mode-open)
  (set-buffer-modified-p nil))

(defun +keepass-mode-setup-revert ()
  "Use a KeePass-aware revert function in the current buffer."
  (setq-local revert-buffer-function #'+keepass-revert-buffer))

(add-hook 'keepass-mode-hook #'+keepass-mode-setup-revert)

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
            (keepass-plus-list)
            :require-match t
            :action '(1
                      ("p" +keepass-copy-password "Copy password")
                      ("u" +keepass-copy-username "Copy username")
                      ("o" +keepass-open-entry "Open entry"))
            :caller 'counsel-keepass))

(defun consult-keepass()
  (interactive)
  (require 'consult)
  (consult-keepass-embark)
  (+keepass-copy-password
   (consult--read (keepass-plus-list)
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

(defun keepass-plus-list()
  "List all members using keepass"
  (cl-delete-if
   (lambda (k) (or (string-match-p "^[^/]" k) (equal "" k)))
   (split-string
    (shell-command-to-string
     (keepass-mode-command "\"*\"" "search"))
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
  (let ((old-password keepass-mode-password))
    (unwind-protect
        (progn
          (setq-local keepass-mode-password password)
          (not (string-match-p
                "Invalid credentials"
                (shell-command-to-string (keepass-mode-command "" "db-info")))))
      (setq-local keepass-mode-password old-password))))

(defun +keepass-ask-valid-password(orig-fn &rest args)
  "Ask for a KeePass password using ORIG-FN and reject invalid credentials."
  (let ((password (apply orig-fn args)))
    (if (+keepass-verify-password password)
      password
      (setq-local keepass-mode-password "")
      (user-error "Invalid password"))))

(unless (advice-member-p #'+keepass-ask-valid-password 'keepass-mode-ask-password)
  (advice-add 'keepass-mode-ask-password :around #'+keepass-ask-valid-password))

(defun +keepass-set-password()
  (let ((buff (get-file-buffer keepass-password-file)))
    (if buff
        (with-current-buffer buff
          (setq-local keepass-mode-password (keepass-mode-ask-password))))))

;;;###autoload
(defun consult-keepass-embark()
  "Adds embark actions to the keepass entry"
  (eval-after-load 'embark
    '(progn
       (defvar-keymap embark-keepass-actions
         :doc "Keymap for actions for keepass entry"
         :parent embark-general-map
         "p" #'+keepass-copy-password
         "u" #'+keepass-copy-username
         "o" #'+keepass-open-entry)
       (add-to-list 'embark-keymap-alist '(keepass-entry . embark-keepass-actions)))))
(provide 'keepass-mode-plus)
;;; keepass-mode-plus.el ends here
