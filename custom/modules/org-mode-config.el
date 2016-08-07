;;; org-mode-config.el -- Configurations for org mode
;;; Commentary:
;; This file holds various settings I use in org mode

;;; Code:


(use-package org-crypt
  :init
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  :config (org-crypt-use-before-save-magic))

;; To turn it off auto-save only locally, you can insert this:
;; # -*- buffer-auto-save-file-name: nil; -*-

(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-wunderlist
  :init (setq org-wunderlist-file  "~/.emacs.d/Wunderlist.org"
                org-wunderlist-dir "~/.emacs.d/org-wunderlist/"))



(provide 'org-mode-config)
;;; org-mode-config.el ends here
