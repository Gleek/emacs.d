;;; init.el -- Emacs configuration entry point.
;;; Commentary:
;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs.

;;; Code:

;; (setq debug-on-error t)
(defconst CACHE-DIR (expand-file-name "cache/" user-emacs-directory))
(defconst RES-DIR   (expand-file-name "resources/" user-emacs-directory))
(defconst IS-MAC    (eq system-type 'darwin))
(defconst IS-LINUX  (eq system-type 'gnu/linux))
(defconst IS-TERM   (not (display-graphic-p)))
(package-initialize)

(setq gc-cons-threshold 8000000) ;; collect garbage after about 100 MB
;; (run-with-idle-timer 2 t (lambda () (garbage-collect)))
(setq message-log-max 10000) ;; Debugging
;; (package-quickstart-refresh)
;; (if (file-exists-p "package-quickstart.el")
;;     (load (expand-file-name "package-quickstart.el" user-emacs-directory))
;;   (package-quickstart-refresh))


(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/"))
(setq package-native-compile t)
(setq native-comp-async-report-warnings-errors nil)

(unless IS-TERM
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-i] [C-i]))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
;; (use-package diminish)
;; (use-package use-package-chords
;;   :demand
;;   :config (key-chord-mode 1))

;; (use-package use-package-ensure-system-package)
;; (use-package benchmark-init
;;   ;; :disabled
;;   :demand
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   ;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
;;   )

(setq custom-file (concat CACHE-DIR "custom.el"))
;; (load custom-file)
(add-to-list 'load-path (expand-file-name "packages/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))

;; Reserve C-z as a prefix for mode level keymaps
(global-unset-key (kbd "C-z"))

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

(use-package "core-project" :ensure nil :demand t
  ;; :chords ("nm" . switch-to-previous-buffer)
  :bind (("C-x k" . kill-current-buffer)
         ("C-x K" . kill-buffer-and-window)))

(use-package "core-debug" :ensure nil :demand t)

(use-package "core-session" :ensure nil :demand t)
(use-package "core-dired" :ensure nil :demand t)
(use-package "core-tramp" :ensure nil :demand t)
(use-package "core-vc" :ensure nil :demand t)
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
