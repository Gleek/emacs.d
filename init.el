;;; init.el -- Emacs configuration entry point.
;;; Commentary:
;; This file stiches all the configuration at one single place.

;;; Code:
(load (expand-file-name "bootstrap.el" user-emacs-directory))
(use-package "core-util" :ensure nil :demand t)
(use-package "core-ui" :ensure nil :demand t)
(use-package "core-ux" :ensure nil :demand t)
(use-package "core-window" :ensure nil :demand t)
(use-package "core-editing" :ensure nil :demand t)
(use-package "core-completion" :ensure nil :demand t)
(use-package "core-checker" :ensure nil :demand t)
(use-package "core-navigation" :ensure nil :demand t)
(use-package "core-metrics" :ensure nil :demand t)
(use-package "core-project" :ensure nil :demand t)
(use-package "core-debug" :ensure nil :demand t)
(use-package "core-session" :ensure nil :demand t)
(use-package "core-dired" :ensure nil :demand t)
(use-package "core-tramp" :ensure nil :demand t)
(use-package "core-vc" :ensure nil :demand t)
(use-package "core-web" :ensure nil :demand t)
(use-package "core-tools" :ensure nil :demand t)

(when IS-LINUX
  (add-to-list 'command-switch-alist
               (cons "--exwm"
                     (lambda ()
                       (use-package "core-exwm" :if IS-LINUX :ensure nil :demand t)))))

(use-package "core-shell" :ensure nil :demand t)
(use-package "core-ebook" :ensure nil :demand t)

(if (file-exists-p (expand-file-name "core/core-secrets.el" user-emacs-directory))
    (use-package "core-secrets" :ensure nil :demand t))

(use-package "lang/core-go" :ensure nil :demand t)
(use-package "lang/core-javascript" :ensure nil :demand t)
(use-package "lang/core-markdown" :ensure nil :demand t)
(use-package "lang/core-php" :ensure nil :demand t)
(use-package "lang/core-proto" :ensure nil :demand t)
(use-package "lang/core-org" :ensure nil :demand t)
(use-package "lang/core-elisp" :ensure nil :demand t)
(use-package "lang/core-diagram" :ensure nil :demand t)
(use-package "lang/core-misc" :ensure nil :demand t)

(provide 'init)
;;; init.el ends here
