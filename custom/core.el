;;; core.el -- Core Editor features
;;
;;; Commentary:
;; Default global settings for Emacs

;;; Code:
;; Loading UI components first
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq inhibit-startup-screen t
      initial-scratch-message "")
(set-frame-font "Source Code Pro 13")
(blink-cursor-mode -1)
(load-theme 'apropospriate-dark t)
;; mode line settings
(use-package spaceline-config
  :config
  (spaceline-emacs-theme))

;; (require 'smart-mode-line)
;; (setq sml/no-confirm-load-theme t)

;; (setq sml/theme 'respectful)
;; (add-hook 'after-init-hook #'sml/setup)


;; (powerline-default-separator)
;; (set-face-attribute 'mode-line nil :font "Sauce Code Powerline 14")
;; Powerline bug for Mac


(use-package fancy-battery
  :init (setq fancy-battery-show-percentage t)
  :config
  (display-time-mode)
  (fancy-battery-mode))

(use-package which-func
  :disabled t ;; slowing down startup of big files
  :config
  (which-function-mode)
  (set-face-foreground 'which-func "darkgrey")
  :defer t)

(use-package hl-line+
  :config
  (global-hl-line-mode t)
  (hl-line-flash)
  (hl-line-toggle-when-idle 1)
  (hl-line-when-idle-interval 1))


(use-package simple
  :config
  (size-indication-mode t)
  (line-number-mode t)
  (column-number-mode t))


(use-package linum
  :config
  (defun linum-format-func (line)
    "Defines the format for the linum mode for specific LINE."
    (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
      (propertize (format (format " %%%dd " w) line) 'face 'linum)))
  (setq linum-format 'linum-format-func))


(use-package smooth-scroll
  :config
  (smooth-scroll-mode t)
  :diminish smooth-scroll-mode)

;; (use-package smooth-scrolling
;;   :init
;;   (setq smooth-scroll-margin 2))



(use-package whitespace
  :init
  (setq whitespace-line -1)
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  :config
  (global-whitespace-mode)
  :diminish global-whitespace-mode)

;; Find my cursor
(use-package beacon
  :config
  (beacon-mode)
  :diminish beacon-mode)


(use-package server
  :config
  (unless (server-running-p)
  (server-start))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

(use-package god-mode
  :bind ([escape] . god-local-mode))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))


(use-package saveplace
  :init
  (setq-default save-place t))

(use-package savehist
  :init (setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat user-emacs-directory "savehist" ))
  :config
  (savehist-mode 1))

;;(delete-selection-mode)
(use-package magit
  :bind ("C-x m" . magit-status)
  :config
  (setq magit-refresh-status-buffer nil))


(use-package subword
  :config (global-subword-mode)
  :diminish subword-mode
  :defer t)

(use-package tramp
  :init
  (setq tramp-default-method "ssh")
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave/")
  :config
  (add-to-list 'tramp-default-proxies-alist
               '("scripts" nil "/ssh:entry:"))
  :defer t)


(use-package flyspell
  :init (setq ispell-program-name "aspell")
  )

(use-package paren
  :config
  (show-paren-mode))

(use-package smartparens
  :config
  (smartparens-global-mode)
  ;; (require 'smartparens-config)
  ;; (setq sp-base-key-bindings 'paredit)
  ;; (setq sp-autoskip-closing-pair 'always)
  ;; (setq sp-hybrid-kill-entire-symbol nil)
  ;; (sp-use-paredit-bindings)
  ;; (show-smartparens-global-mode +1)
  :diminish smartparens-mode)


(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-$" . mc/mark-more-like-this-extended)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c m" . mc/mark-all-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (global-unset-key (kbd "M-<down-mouse-1>")))

(use-package term
  :config
  (yas-minor-mode -1)
  :defer t)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-c a" . counsel-ag)))

(use-package paradox
  :init (setq paradox-automatically-star t))


;; (require 'flx-ido)

;; (use-package counsel
;;   :bind ("M-x" . counsel-M-x))
(use-package ido
  :disabled t
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  disable ido faces to see flx highlights.
  (ido-vertical-mode 1)
  :defer t)

(use-package imenu
  :bind ("C-x c i" . helm-imenu))


(use-package autorevert
  :disabled t
  :config (global-auto-revert-mode -1))


(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  :diminish
  drag-stuff-mode
  :defer t)

;; (require 'drag-stuff)
;; (drag-stuff-global-mode 1)

(use-package simple-http
  :config (setq httpd-root "~/Development/testing")
  :defer t)


(use-package vlf :defer t)

(use-package avy
  :init
  (setq avy-background t)
  :bind (("C-\"". avy-goto-word-or-subword-1)
         ("C-'" . avy-goto-char-timer))
  :defer t)
;; (require 'avy)
;; (setq avy-background t)

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode)
  :bind ("M-p" . projectile-find-file)
  :diminish projectile-mode)


(use-package vc
  :init
  (setq vc-handled-backends nil)
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
  (remove-hook 'find-file-hook 'vc-refresh-state)
  (remove-hook 'after-save-hook 'vc-find-file-hook))

(use-package volatile-highlights
  :disabled t
  :config
  (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)


(use-package flycheck
  :config
  (global-flycheck-mode)
  :diminish flycheck-mode)

;; (require 'helm-dash)
;; (setq helm-dash-browser-func 'eww)

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  :diminish undo-tree-mode)


(use-package recentf
  :config
  (recentf-mode 1))

(use-package er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package which-key
  :config
  (which-key-mode 1)
  :diminish which-key-mode)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode)


(use-package hippie-expand
  :init
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-line
                                           try-expand-list
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol))
  :bind ("M-/" . hippie-expand))

(use-package company
  :disabled t
  :init
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 3)
  (setq company-backends
        (quote
         (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                       (company-dabbrev-code company-keywords)
                       company-oddmuse company-dabbrev)))
  :config
  (global-company-mode)
  ;; (defvar company-mode/enable-yas t
  ;;   "Enable yasnippet for all backends.")



  ;; (defun company-mode/backend-with-yas (backend)
  ;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
  ;;       backend
  ;;     (append (if (consp backend) backend (list backend))
  ;;             '(:with company-yasnippet))))


  ;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  :defer t)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))
(use-package recentf
  :disabled t
  :config (recentf-mode))

(use-package isearch
  :bind (("C-r"   . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward)))
(use-package swiper
  :bind ("C-s" . swiper))
(use-package anzu
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(use-package fold-this
  :bind (("C-c C-F" . fold-this-all)
         ("C-c C-f" . fold-this)
         ("C-c M-f" . fold-this-unfold-all)))

(use-package zop-to-char
  :bind ("M-z" . zop-to-char))

(use-package desktop
  :config (desktop-save-mode 1))


;; Loading functions and variables

(set-default 'imenu-auto-rescan t)
(setq-default frame-title-format '(buffer-file-name "%b"))
(setq-default line-spacing 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq select-enable-clipboard t
      select-enable-primary t
      display-time-default-load-average nil
      save-interprogram-paste-before-kill t
      apropos-do-all t
      ring-bell-function 'ignore
      mouse-yank-at-point t
      require-final-newline t
      ns-use-srgb-colorspace 'nil
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)
(setq tab-always-indent 'complete)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)



(provide 'core)
;;; core.el ends here
