;;; org-mode-config.el -- Configurations for org mode
;;; Commentary:
;; This file holds various settings I use in org mode

;;; Code:

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(setq org-crypt-key nil)
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.

(setq auto-save-default nil)
;; To turn it off auto-save only locally, you can insert this:
;; # -*- buffer-auto-save-file-name: nil; -*-

(provide 'org-mode-config)
;;; org-mode-config.el ends here
