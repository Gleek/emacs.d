;;; core.el -- Core Editor features
;;
;;; Commentary:
;; Default global settings for Emacs

;;; Code:
(require 'server)
(unless (server-running-p)
  (server-start))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-ater-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(require 'saveplace)
(setq-default save-place t)

(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat user-emacs-directory "savehist" ))
(savehist-mode +1)

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)
(setq tab-always-indent 'complete)

(desktop-save-mode 1)
;;(setq desktop-files-not-to-save "^$")


(autoload 'magit-status "magit")

;; This some how slows down in large files
;;(delete-selection-mode)
(global-subword-mode)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave/")

(defalias 'yes-or-no-p 'y-or-n-p)


(require 'smartparens)
(smartparens-global-mode)
;; (require 'smartparens-config)
;; (setq sp-base-key-bindings 'paredit)
;; (setq sp-autoskip-closing-pair 'always)
;; (setq sp-hybrid-kill-entire-symbol nil)
;; (sp-use-paredit-bindings)
;; (show-smartparens-global-mode +1)

(yas-global-mode 1)
;; (require 'ivy)
;; (ivy-mode)
;; (require 'flx-ido)
;; Ido mode is disabled (switched to HELM)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)
;; (ido-vertical-mode 1)
;; (hlinum-activate)

(require 'helm-config)
(helm-mode 1)
(helm-adaptive-mode t)
(set-face-attribute 'helm-source-header nil :height 1)
(setq helm-display-header-line nil)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30
      helm-autoresize-min-height 30
      helm-split-window-in-side-p t
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; (global-auto-revert-mode -1)

(require 'move-text)
;; (move-text-default-bindings)

(require 'vlf-setup)

(require 'avy)
(setq avy-background t)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
;;; Remove vc support
(require 'vc)
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(remove-hook 'after-save-hook 'vc-find-file-hook)
(require 'company)
(global-company-mode)

(require 'smooth-scrolling)
(setq smooth-scroll-margin 2)
;; (require 'flycheck)
;; (global-flycheck-mode)
;; (require 'helm-dash)
;; (setq helm-dash-browser-func 'eww)

(require 'undo-tree)
(global-undo-tree-mode 1)

(require 'diminish)
;; (diminish 'ivy-mode)
(diminish 'yas-minor-mode)
(diminish 'undo-tree-mode)
(diminish 'smartparens-mode)
(diminish 'company-mode)
(diminish 'whitespace-mode)
(diminish 'global-whitespace-mode)
(diminish 'helm-mode)
(diminish 'subword-mode)
(require 'recentf)
(recentf-mode 1)

(setq gc-cons-threshold 20000000) ;; collect garbage after 20 MB
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(provide 'core)
;;; core.el ends here
