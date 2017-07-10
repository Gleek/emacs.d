;;; core.el -- Core Editor features
;;
;;; Commentary:
;; Default global settings for Emacs

;;; Code:

;;;;;;;;;;;;;;;;;;;
;; Look and Feel ;;
;;;;;;;;;;;;;;;;;;;
;; Remove Clutter
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq inhibit-startup-screen t
      initial-scratch-message "")
(blink-cursor-mode -1)

;; Font/Themes
(set-frame-font "Fira Mono 11")
(load-theme 'spacemacs-dark t)
(set-cursor-color "#FFFFCC")
(use-package all-the-icons :ensure t)

;; mode line settings
(use-package spaceline-config
  :init
  (setq powerline-default-separator "wave")
  (setq spaceline-window-numbers-unicode t)
  (setq spaceline-workspace-numbers-unicode t)
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme))

(use-package spaceline-all-the-icons
  :disabled t
  :init
  (defvar spaceline-all-the-icons-separator-type)
  (setq spaceline-all-the-icons-separator-type 'arrow))

(use-package fancy-battery
  :ensure t
  :init
  (defvar fancy-battery-show-percentage)
  (setq fancy-battery-show-percentage t)
  :config
  (display-time-mode)
  (fancy-battery-mode))

(use-package which-func
  :disabled t ;; slowing down startup of big files
  :config
  (which-function-mode)
  (set-face-foreground 'which-func "darkgrey")
  :defer t)

(use-package simple
  :config
  (size-indication-mode t)
  (line-number-mode t)
  (column-number-mode t))


(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))


;; Editing
(use-package hl-line+
  :ensure t
  :config
  (global-hl-line-mode t)
  (hl-line-flash)
  (hl-line-toggle-when-idle 1)
  (hl-line-when-idle-interval 1))

(use-package whitespace
  :init
  (setq whitespace-line -1)
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  :config
  (global-whitespace-mode)
  :diminish global-whitespace-mode)

;; Find my cursor
(use-package beacon
  :disabled
  :config
  (beacon-mode)
  :diminish beacon-mode)

(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode t)
  (setq smooth-scroll/vscroll-step-size 5)
  :diminish smooth-scroll-mode)

(use-package linum
  :ensure t
  :config
  (defun linum-format-func (line)
    "Defines the format for the linum mode for specific LINE."
    (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
      (propertize (format (format " %%%dd " w) line) 'face 'linum)))
  (setq linum-format 'linum-format-func))

(use-package nlinum
  :ensure nlinum
  :ensure nlinum-relative
  :defer t)

(use-package paren
  :config
  (show-paren-mode))

(use-package volatile-highlights
  :disabled t
  :config
  (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

(use-package which-key
  :ensure t
  :disabled t
  :config
  (which-key-mode 1)
  :diminish which-key-mode)

(use-package indent-guide :ensure t :disabled t)
;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-c s s" . counsel-rg)
         ("M-y" . counsel-yank-pop)
         ("M-." . counsel-gtags-dwim)
         ("C-c t u" . counsel-gtags-update-tags)
         ("C-x c i" . counsel-imenu)
         ("C-M-." . counsel-gtags-find-definition)))

(use-package ivy
  :init
  (ivy-mode t)
  :bind ("C-x b" . ivy-switch-buffer)
  :diminish ivy-mode)

(use-package ido
  :disabled t
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (ido-vertical-mode 1)
  :defer t)

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
  :ensure t
  :ensure company-web
  :ensure company-quickhelp
  ;; :disabled te
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

(use-package yasnippet
  :disabled t
  ;; :ensure t
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode)

;;;;;;;;;;;;;
;; Editing ;;
;;;;;;;;;;;;;

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package subword
  :init (global-subword-mode t)
  :diminish subword-mode
  :defer t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-$" . mc/mark-more-like-this-extended)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c m" . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package drag-stuff
  :disabled t
  ;; :ensure t
  :config
  (drag-stuff-global-mode 1)
  :diminish
  drag-stuff-mode
  :defer t)

(use-package fold-this
  :ensure t
  :bind (("C-c C-F" . fold-this-all)
         ("C-c C-c" . fold-this)
         ("C-c M-f" . fold-this-unfold-all)))

(use-package origami
  :disabled t
  :config (global-origami-mode 1)
  :bind (("C-c C-c" . origami-toggle-node)))

(use-package er/expand-region
  :ensure expand-region
  :init (setq shift-select-mode nil)
  :bind ("C-=" . er/expand-region))


(use-package smartparens
  :init (smartparens-global-mode t)
  :ensure t
  :config
  (require 'smartparens-config)
  ;; (setq sp-base-key-bindings 'paredit)
  ;; (setq sp-autoskip-closing-pair 'always)
  ;; (setq sp-hybrid-kill-entire-symbol nil)
  ;; (sp-use-paredit-bindings)
  ;; (show-smartparens-global-mode +1)
  :bind (("M-[" . sp-backward-unwrap-sexp)
         ("M-]" . sp-unwrap-sexp))
  :diminish smartparens-mode)

(use-package evil
  :ensure t
  :disabled t
  :config (evil-mode 1))

(use-package god-mode
  :ensure t
  :bind ([escape] . god-local-mode))

;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :init
  (setq avy-background t)
  :bind (("C-\"". avy-goto-word-or-subword-1)
         ("C-'" . avy-goto-char-timer))
  :defer t)

(use-package ace-window
  :bind ("C-:" . ace-window))

(use-package isearch
  :bind (("C-r"   . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward)))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package anzu
  :ensure t
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(use-package zop-to-char
  :ensure t
  :bind ("M-z" . zop-to-char))

(use-package imenu-anywhere :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-file Management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package neotree
  :ensure t
  :init (setq neo-theme 'icons))

(use-package magit
  :ensure t
  :bind ("C-x m" . magit-status)
  :config
  (setq magit-refresh-status-buffer nil)
  (setq magit-auto-revert-mode nil)
  (magit-auto-revert-mode nil))

(use-package projectile
  :ensure projectile
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (projectile-mode 1)
  :config
  :bind (("M-p" . projectile-find-file)
         ("C-c s p" . projectile-ripgrep))
  :diminish projectile-mode)

(use-package vc
  :init
  ;; Slows down opening large files.
  (setq vc-handled-backends nil)
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
  (remove-hook 'find-file-hook 'vc-refresh-state)
  (remove-hook 'after-save-hook 'vc-find-file-hook))

(use-package recentf
  :config
  (recentf-mode 1))
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))
(use-package recentf
  :disabled t
  :config (recentf-mode))
(use-package desktop
  :disabled t
  :config (desktop-save-mode 1))

(use-package xref
  :config (add-to-list 'xref-backend-functions 'gxref-xref-backend))

(use-package ggtags :ensure t)
(use-package ecb :disabled t :defer t)

(use-package ag :ensure t :defer t)
;;;;;;;;;;;;;
;; Checker ;;
;;;;;;;;;;;;;
(use-package flyspell
  :init
  (defvar ispell-program-name)
  (defvar flyspell-issue-message-flag)
  (setq ispell-program-name "aspell")
  (setq flyspell-issue-message-flag nil)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  (set-face-attribute 'flyspell-incorrect nil :underline '(:color "#6666ff"))
  (set-face-attribute 'flyspell-duplicate nil :underline '(:color "#6666ff"))
  :diminish flyspell-mode)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  :diminish flycheck-mode)


;;;;;;;;;;;;;;;;;;;;;
;; Emacs Utilities ;;
;;;;;;;;;;;;;;;;;;;;;
(use-package server
  :config
  (unless (server-running-p)
  (server-start))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

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
      savehist-file (concat () user-emacs-directory "savehist" ))
  :config
  (savehist-mode 1))

(use-package tramp
  :init
  (setq tramp-default-method "ssh")
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave/")
  :config
  (add-to-list 'tramp-default-proxies-alist
               '("jobs" nil "/ssh:entry:"))
  (add-to-list 'tramp-default-proxies-alist
               '("scripts" nil "/ssh:entry:"))
  (add-to-list 'tramp-default-proxies-alist
               '("aa102" nil "/ssh:entry:"))
  :defer t)
(use-package paradox
  :ensure t
  :init
  (defvar paradox-automatically-star)
  (defvar paradox-execute-asynchronously)
  (setq paradox-automatically-star t)
  (setq paradox-execute-asynchronously t))

(use-package simple-http
  :config (setq httpd-root "~/Development/testing")
  :defer t)

(use-package zeal-at-point
  :config (setq zeal-at-point-zeal-version "0.3.1"))

(use-package dashboard
  :ensure t
  :init (setq dashboard-items '((recents  . 7)
                                (projects . 8)
                                (bookmarks . 5)
                                (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package howdoi :disabled t)
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  :diminish undo-tree-mode)

(use-package vlf :defer t)

(use-package autorevert
  :disabled t
  :config (global-auto-revert-mode -1))

(use-package term
  :config
  (yas-minor-mode -1)
  :defer t)

(use-package ansi-term
  :ensure shell-toggle
  :init (setq-default bidi-display-reordering nil)
  :bind ("C-`" . shell-toggle)
  :config
  (defun term-handle-more-ansi-escapes (proc char)
    "Handle additional ansi escapes."
    (cond
     ;; \E[nG - Cursor Horizontal Absolute, e.g. move cursor to column n
     ((eq char ?G)
      (let ((col (min term-width (max 0 term-terminal-parameter))))
        (term-move-columns (- col (term-current-column)))))
     (t)))
  (advice-add 'term-handle-ansi-escape :before #'term-handle-more-ansi-escapes))

(use-package eshell
  :ensure eshell-git-prompt
  ;;   https://github.com/ekaschalk/dotspacemacs/blob/master/.spacemacs
  :init (eshell-git-prompt-use-theme 'robbyrussell))

(use-package persistent-scratch
  :ensure t
  :config (persistent-scratch-setup-default))

(use-package alert
  :ensure t
  :init
  (defvar alert-default-style)
  (setq alert-default-style 'libnotify))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Languages / Syntax Major Mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package apache-mode)
(use-package csv-mode)
(use-package php-mode :ensure t)
(use-package js2-mode :ensure t)
;; (use-package js-doc :ensure t)
;; (use-package jsx-mode :defer t)
(use-package less-css-mode)
(use-package phpcbf
  :config
  (setq phpcbf-standard "~/Development/phpcs.xml"))

(use-package go-mode
  :init
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("M-*" . pop-tag-mark)))

(use-package markdown-mode
  :ensure t
  :init
  (defvar markdown-command)
  (setq markdown-command "/usr/bin/pandoc"))


;; Org mode settings
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

;; (use-package org-wunderlist
;;   :init (setq org-wunderlist-file  "~/.emacs.d/Wunderlist.org"
;;                 org-wunderlist-dir "~/.emacs.d/org-wunderlist/"))

(use-package org-alert
  :config (org-alert-enable))

(use-package org
  :init
  (setq org-ellipsis "…"
        ;; org-agenda-files '("~/Dropbox/org-files")
        org-log-done 'time
        org-startup-with-inline-images t
        ;; org-fontify-whole-heading-line t
        ;; org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"))


;; Loading functions and variables

(set-default 'imenu-auto-rescan t)
(setq-default frame-title-format '(buffer-file-name "%b"))
(setq-default line-spacing 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq select-enable-clipboard t ;; Enabled emacs to use system clipboard
      select-enable-primary nil ;; Disable Copy on selection
      display-time-default-load-average nil
      save-interprogram-paste-before-kill t
      apropos-do-all t
      use-dialog-box nil
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
(put 'narrow-to-region 'disabled nil)
(provide 'core)
;;; core.el ends here
