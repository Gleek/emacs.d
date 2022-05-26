;;; init.el -- Emacs configuration entry point.
;;; Commentary:
;; This file stiches all the configuration at one single place.

;;; Code:

(load (expand-file-name "bootstrap.el" user-emacs-directory))
(use-package "core-util" :ensure nil :demand t)
(use-package "core-ui" :ensure nil :demand t)
(use-package "core-ux" :ensure nil :demand t
  :bind (([remap goto-line] . goto-line-with-feedback)
         ([remap keyboard-quit] . escape-quit)))

(use-package "core-window" :ensure nil :demand t
  :bind (("C-x <right>" . windmove-right)
         ("C-x <left>"  . windmove-left)
         ("C-x <up>"    . windmove-up)
         ("C-x <down>"  . windmove-down)
         ("C-x 2"       . vsplit-last-buffer)
         ("C-x 3"       . hsplit-last-buffer)
         ("C-c w r"     . rotate-windows)
         ("C-x 4 f"     . find-file-other-window)))

(use-package "core-editing" :ensure nil :demand t
  :bind (("M-;" . comment-or-uncomment-region-or-line)
         ("C-c C-d" . duplicate-current-line-or-region)
         ("C-c d" . duplicate-current-line-or-region)
         ("C-c C-;" . duplicate-and-comment-current-line-or-region)
         ("C-^" . top-join-line)
         ("C-@" . mark-inside-sexp)
         ;; ("C-M-@" . mark-non-whitespace)
         ([remap kill-whole-line] . smart-kill-whole-line)
         ([(shift return)] . smart-open-line)
         ("C-S-<return>" . open-line-above)
         ("C-x C-r" . sudo-edit)))

(use-package "core-completion" :ensure nil :demand t)

(use-package "core-checker" :ensure nil :demand t)

(use-package "core-navigation" :ensure nil :demand t
  :bind (("C-a" . beginning-of-line-or-indentation)))

(use-package "core-metrics" :ensure nil :demand t)

(use-package "core-project" :ensure nil :demand t
  ;; :chords ("nm" . switch-to-previous-buffer)
  :bind (("C-x k" . kill-current-buffer)
         ("C-x K" . kill-buffer-and-window)))

(use-package "core-debug" :ensure nil :demand t)

(use-package "core-session" :ensure nil :demand t)
(use-package "core-dired" :ensure nil :demand t)
(use-package "core-tramp" :ensure nil :demand t)
(use-package "core-vc" :ensure nil :demand t)
(use-package "core-web" :ensure nil :demand t)
(use-package "core-tools" :ensure nil :demand t
  :bind (("C-c r". rename-file-and-buffer)
         ([f5] . kmacro-edit-macro)))


(when IS-LINUX
  (add-to-list 'command-switch-alist
               (cons "--exwm"
                     (lambda ()
                       (use-package "core-exwm" :ensure nil :demand t)))))

(use-package "core-shell" :ensure nil :demand t)
(use-package "core-ebook" :ensure nil :demand t)
(use-package "core-secrets" :ensure nil :demand t)

(use-package "lang/core-go" :ensure nil :demand t)
(use-package "lang/core-javascript" :ensure nil :demand t)
(use-package "lang/core-markdown" :ensure nil :demand t)
(use-package "lang/core-php" :ensure nil :demand t)
(use-package "lang/core-proto" :ensure nil :demand t)
(use-package "lang/core-org" :ensure nil :demand t)
(use-package "lang/core-elisp" :ensure nil :demand t
  :commands remove-elc-on-save eval-and-replace
  :bind (("C-c C-e" . eval-and-replace)
         ("C-c e" . eval-and-replace))
  :config
  (add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save))

(use-package "lang/core-plantuml" :ensure nil :demand t)
(use-package "lang/core-misc" :ensure nil :demand t)

(provide 'init)
;;; init.el ends here
