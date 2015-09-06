;;; company-settings.el -- Configurations for company mode
;;; Commentary:
;; This file holds various settings I use in company mode

;;; Code:

(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(provide 'company-settings)
;;; company-settings.el ends here
