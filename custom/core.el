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
(setq-default tab-width 8)
(setq tab-always-indent 'complete)

(desktop-save-mode 1)
(setq desktop-files-not-to-save "^$")


(autoload 'magit-status "magit")
(delete-selection-mode)

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))


(global-undo-tree-mode t)
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

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

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
;; (hlinum-activate)

(global-auto-revert-mode 1)

(require 'move-text)
;; (move-text-default-bindings)

(require 'vlf-setup)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)


(require 'company)
(global-company-mode)

(require 'flycheck)
(global-flycheck-mode)
;; (require 'helm-dash)
;; (setq helm-dash-browser-func 'eww)

(require 'anzu)
(global-anzu-mode)


(require 'diminish)
(diminish 'anzu-mode)
(diminish 'yas-minor-mode)
(diminish 'undo-tree-mode)
(diminish 'smartparens-mode)
(diminish 'company-mode)
(diminish 'whitespace-mode)
(diminish 'global-whitespace-mode)


(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(provide 'core)
;;; core.el ends here
