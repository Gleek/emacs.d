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
(set-frame-font "Source Code Pro 13" 'keepsize t)
;; (set-face-font 'variable-pitch "Baskerville 15")
(set-face-font 'variable-pitch "Georgia 15")
(set-face-font 'fixed-pitch "Source Code Pro 13")


(use-package doom-themes
  :init
  (defvar doom-themes-treemacs-enable-variable-pitch)
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  :demand
  :config (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))



(use-package all-the-icons)
;; mode line settings
;; (use-package spaceline-config
;;   :defer 1
;;   :init
;;   (setq powerline-default-separator "utf-8")
;;   (setq powerline-height 20)
;;   (setq spaceline-minor-modes-separator " ")
;;   (defadvice load-theme (after run-after-load-theme-hook activate)
;;     "Run `powerline-reset'."
;;     (powerline-reset))
;;   :ensure spaceline
;;   :config
;;   (spaceline-spacemacs-theme))


(use-package doom-modeline
  :defer 1
  :config
  (setq doom-modeline-vcs-max-length 20)
  (doom-modeline-mode +1))


(use-package which-func
  :disabled t ;; slowing down startup of big files
  :config
  (which-function-mode)
  (set-face-foreground 'which-func "darkgrey"))

(use-package simple
  :ensure nil
  :config
  (size-indication-mode t)
  (line-number-mode t)
  (column-number-mode t))

(use-package display-line-numbers
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))


;; Editing
(use-package hl-line+
  :disabled t
  :ensure t
  :config
  (global-hl-line-mode t)
  (hl-line-flash)
  (hl-line-toggle-when-idle 1)
  (hl-line-when-idle-interval 1))

(use-package whitespace
  :hook (after-change-major-mode . sane-whitespace)
  :init
  (defun sane-whitespace()
    (unless (or (eq major-mode 'fundamental-mode)
              buffer-read-only
              (bound-and-true-p global-whitespace-mode)
              (null buffer-file-name))
      (whitespace-mode +1)))
  (setq whitespace-style '(face empty trailing space-after-tab space-before-tab))
  :diminish whitespace-mode)

;; Find my cursor
(use-package beacon
  :disabled
  :config
  (beacon-mode)
  :diminish beacon-mode)

(use-package smooth-scroll
  :defer 1
  :ensure t
  :config
  (smooth-scroll-mode t)
  ;; (setq scroll-conservatively 101)
  ;; (setq inhibit-compacting-font-caches t)
  (setq smooth-scroll/vscroll-step-size 5)
  :diminish smooth-scroll-mode)


(use-package paren
  :defer 10
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery nil)
  (show-paren-mode))


(use-package which-key
  :defer 1
  :config
  (which-key-mode 1)
  (which-key-setup-minibuffer)
  :diminish which-key-mode)

(use-package zoom
  :defer 3
  :diminish
  :config
  ;; (zoom-mode t)
  (setq zoom-ignored-buffer-name-regexps '("^\*ansi-term.*"))
  (setq zoom-size '(0.618 0.618)))

(use-package writeroom-mode)

;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;
(use-package counsel
  :ensure t
  :init
  (setq counsel-rg-base-command "rg -S --no-heading --line-number -M 500 --color never %s .")
  :bind (("M-x"     . counsel-M-x)
         ("C-c s s" . counsel-rg)
         ("C-x B"   . counsel-switch-buffer-other-window)
         ("C-c SPC" . counsel-mark-ring)
         ("M-y"     . counsel-yank-pop)
         ("C-x c i" . counsel-imenu)
         ("C-x C-f" . counsel-find-file)
         ("C-h v" . counsel-describe-variable)))

(use-package ivy
  :init
  (setq ivy-sort-max-size 7500
        ivy-height 15
        ivy-wrap nil
        ivy-use-selectable-prompt t
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ivy-use-selectable-prompt t)
  :config
  (ivy-mode t)
  ;; (all-the-icons-ivy-setup)
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c b r" . ivy-resume))
  :diminish ivy-mode)

(use-package all-the-icons-ivy-rich
  :defer 1
  :config
  (all-the-icons-ivy-rich-mode 1))

(use-package ivy-posframe
  :disabled
  :diminish
  :config
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  ;; (setq ivy-posframe-parameters '())
  (setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)
        (parent-frame nil)))
  (ivy-posframe-mode 1))

(use-package ivy-rich
  :defer 1
  :after ivy
  :config
  (ivy-rich-mode t))

;; (use-package all-the-icons-ivy-rich
;;   :defer 1
;;   :after ivy-rich
;;   :config (all-the-icons-ivy-rich-mode t))

(use-package hippie-exp
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
  :defer 5
  :ensure company-web
  :ensure company-quickhelp
  ;; :disabled t
  :bind ("C-." . company-complete)
  :init

  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        ;; company-backends
        ;; (quote
        ;;  (company-semantic company-clang company-capf company-files
        ;;                    (company-dabbrev-code company-keywords)
        ;;                    company-dabbrev))
        company-backends '(company-capf)
        
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  :config
  (global-company-mode)
  :diminish "Ⓒ")

(use-package company-box
  :diminish
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-images))


(use-package yasnippet
  ;; :disabled t
  :defer 5
  :config
  (yas-global-mode 1)
  :diminish (yas-minor-mode . "Ⓨ"))

;;;;;;;;;;;;;;;;
;; Editing ;;
;;;;;;;;;;;;;;;;

(use-package move-text
  :defer 3
  :config
  (move-text-default-bindings))


(use-package auto-indent-mode
  :init
  (setq auto-indent-assign-indent-level nil)
  :hook php-mode
  :diminish)

(use-package subword
  :init (global-subword-mode t)
  :diminish subword-mode)

(use-package multiple-cursors
  :ensure t
  :config
  (define-key mc/keymap (kbd "C-s") #'phi-search)
  (define-key mc/keymap (kbd "C-r") #'phi-search-backward)
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-$" . mc/mark-more-like-this-extended)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c m" . mc/mark-all-dwim)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package string-inflection
  :bind (("M-_" . string-inflection-all-cycle)
         ("C-c c s" . string-inflection-underscore)
         ("C-c c k" . string-inflection-kebab-case)
         ("C-c c M" . string-inflection-camelcase)
         ("C-c c m" . string-inflection-lower-camelcase)
         ("C-c c c" . capitalize-word)
         ("C-c c u" . upcase-word)
         ("C-c c l" . downcase-word)))


(use-package align
  :bind (("C-x a a" . align)
         ("C-x a c" . align-current)))

(use-package origami
  :config (global-origami-mode 1)
  :bind (("C-c C-c" . origami-recursively-toggle-node)))

(use-package er/expand-region
  :ensure expand-region
  :init (setq shift-select-mode nil)
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

;; (use-package easy-kill
;;   :init
;;   (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package smartparens
  :hook ((prog-mode text-mode conf-mode) . smartparens-mode)
  :ensure t
  :config
  ;; (smartparens-global-mode t)
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  ;; https://github.com/Fuco1/smartparens/issues/80 get reindent on curly brackets.
  (dolist (mode '(prog-mode))
  (sp-local-pair mode "{" nil :post-handlers
                 '((radian-enter-and-indent-sexp "RET")
                   (radian-enter-and-indent-sexp "<return>"))))
  (defun radian-enter-and-indent-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  :bind (("M-[" . sp-backward-unwrap-sexp)
         ("M-]" . sp-unwrap-sexp))
  :diminish smartparens-mode)


(use-package evil
  :ensure t
  :disabled t
  :config (evil-mode 1))

(use-package vimrc-mode)

(use-package god-mode
  :ensure t
  ;; :bind ([escape] . god-local-mode)
  :config (define-key god-local-mode-map (kbd ".") 'repeat))

;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :init
  (setq avy-background t)
  :bind (("C-\"". avy-goto-word-or-subword-1)
         ("C-'" . avy-goto-char-timer)))

(use-package ace-window
  :bind ("C-:" . ace-window))

(use-package winner
  :config (winner-mode t)
  :bind (("s-<tab>" . winner-undo)
         ("s-<iso-lefttab>" . winner-redo)))

(use-package isearch
  :ensure nil
  :bind (("C-r"   . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward)))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
         ("C-c s m" . swiper-mc)))

(use-package phi-search :ensure t
  :init (setq phi-search-limit 10000))

(use-package anzu
  :ensure t
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package imenu-anywhere :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-file Management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treemacs
  :bind ("C-c p t" . treemacs))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package lsp-treemacs
  :after treemacs lsp
  :hook (treemacs-select-hook . lsp-treemacs-sync-mode))

(use-package magit
  :bind (("C-x m" . magit-status)
         ("C-c g b" . magit-blame-addition)
         ("C-c g l" . magit-log-buffer-file))
  :config
  (setq magit-refresh-status-buffer nil)
  (setq magit-auto-revert-mode nil)
  (setq magit-diff-refine-hunk "all")
  (magit-auto-revert-mode nil))

(use-package browse-at-remote)

(use-package git-gutter
  :defer 3
  :diminish git-gutter-mode
  :bind (("C-c g d" . git-gutter:popup-hunk)
         ("C-c g r" . git-gutter:revert-hunk))
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:modified-sign " "
        git-gutter:added-sign " "
        git-gutter:deleted-sign " ")
  (set-face-attribute
   'git-gutter:modified nil
   :family "Arial"
   :background "SandyBrown"
   :height 70)
  (set-face-attribute
   'git-gutter:added nil
   :family "Arial"
   :background "DarkGreen"
   :height 70)
  (set-face-attribute
   'git-gutter:deleted nil
   :family "Arial"
   :background "DarkRed"
   :height 70))

(use-package git-timemachine)

(use-package projectile
  :ensure projectile
  :init
  (defvar projectile-mode-line)
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-enable-caching t) ;; Projectile turbo!!
  :config
  (setq projectile-mode-line-prefix "")
  (projectile-mode 1)
  :bind (("C-c p z" . counsel-fzf)
         ("C-c p k" . projectile-kill-buffers)
         ("C-c p s" . projectile-save-project-buffers)))

(use-package counsel-projectile
  :defer 1
  :bind (("M-p" . counsel-projectile-find-file)
         ("C-c p f" . counsel-projectile-find-file)
         ("C-c p b" . counsel-projectile-switch-to-buffer)
         ("C-c p p" . counsel-projectile-switch-project)))


(use-package vc
  :init
  ;; Slows down opening large files.
  (setq vc-handled-backends '(Git))
  ;; (remove-hook 'find-file-hooks 'vc-find-file-hook)
  (add-hook 'find-file-hook 'vc-refresh-state)
  ;; (remove-hook 'after-save-hook 'vc-find-file-hook)
  )

(use-package recentf
  :config
  (recentf-mode 1))
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))
(use-package ibuffer-projectile
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package recentf
  :disabled t
  :config (recentf-mode))

;; (use-package xref
;;   :config
;;   (add-to-list 'xref-backend-functions 'gxref-xref-backend))

(use-package ivy-xref
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ecb :disabled t )

(use-package ag :ensure t )

(use-package dumb-jump
  :ensure t
  ;; :bind (("M-g o" . dumb-jump-go-other-window)
  ;;        ("M-g j" . dumb-jump-go)
  ;;        ("M-g i" . dumb-jump-go-prompt)
  ;;        ("M-g h" . dumb-jump-back)
  ;;        ("M-g q" . dumb-jump-quick-look)
  ;;        ("M-g x" . dumb-jump-go-prefer-external)
  ;;        ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-rg-cmd "rg")
  (setq dumb-jump-max-find-time 5)
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-selector 'ivy))

(use-package rg
  :ensure rg
  :ensure wgrep-ag
  :config
  (add-hook 'ripgrep-search-mode-hook 'wgrep-ag-setup))

(use-package smart-jump
  :ensure t
  :defer 4
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-?" . smart-jump-references))
  :config (smart-jump-setup-default-registers)
  (smart-jump-register :modes 'go-mode))
;;;;;;;;;;;;;
;; Checker ;;
;;;;;;;;;;;;;
(use-package flyspell-lazy
  :config (flyspell-lazy-mode 1))
(use-package flyspell
  :defer 10
  :init
  (defvar ispell-program-name)
  (defvar flyspell-issue-message-flag)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq flyspell-issue-message-flag nil)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  (set-face-attribute 'flyspell-incorrect nil :underline '(:color "#6666ff"))
  (set-face-attribute 'flyspell-duplicate nil :underline '(:color "#6666ff"))
  (eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))
  :diminish flyspell-mode)

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  ;; (global-flycheck-mode -1)
  (setq flycheck-protoc-import-path
        '("~/Development/zomato/zomato-event-registry/"
          "~/Development/gocode/src/github.com/Zomato/delivery-composite-order-service/"))
  (setq flycheck-check-syntax-automatically '(save mode-enabled new-line idle-change))
  ;; (setq flycheck-buffer-switch-check-intermediate-buffers nil)
  (setq flycheck-display-errors-delay 0.25)
  :diminish flycheck-mode)


(use-package flycheck-phpstan
  :init
  (setq phpstan-memory-limit "1G")
)



(use-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  ;; (flycheck-posframe-configure-pretty-defaults)
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ ")
  (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p))

(use-package flymake)
(use-package flymake-diagnostic-at-point
  :after flymake
  :init
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Emacs Utilities ;;
;;;;;;;;;;;;;;;;;;;;;
(use-package server
  :defer 5
  :config
    (unless (server-running-p)
    (server-start))
    (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

(use-package key-chord
  :defer 10
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay .015
        key-chord-one-key-delay .040)
  (key-chord-define-global "df" 'isearch-forward)
  (key-chord-define-global "jk" 'avy-goto-word-or-subword-1)
  (key-chord-define-global "nm" 'switch-to-previous-buffer))

(use-package saveplace
  :init
  (save-place-mode 1)
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
  ;; (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
  :config
  (add-to-list 'tramp-default-proxies-alist
               '("jobs" nil "/ssh:entry:"))
  (add-to-list 'tramp-default-proxies-alist
               '("scripts" nil "/ssh:entry:"))
  (add-to-list 'tramp-default-proxies-alist
               '("aa102" nil "/ssh:entry:")))

(use-package paradox
  :ensure t
  :init
  (defvar paradox-automatically-star)
  (defvar paradox-execute-asynchronously)
  (setq paradox-automatically-star t)
  (setq paradox-execute-asynchronously t))

(use-package simple-http
  :ensure nil
  :config (setq httpd-root "~/Development/testing")
  )

(use-package zeal-at-point
  :config (setq zeal-at-point-zeal-version "0.3.1"))

(use-package dashboard
  :demand
  :ensure t
  :init (setq dashboard-items '((recents  . 7)
                                (projects . 8)
                                (bookmarks . 5)
                                ;; (agenda . 5)
                                ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 3)
  (setq dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook))

(use-package page-break-lines :diminish page-break-lines-mode)

(use-package howdoi :disabled t)
(use-package undo-tree
  :defer 2
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :diminish undo-tree-mode)

(use-package vlf )

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  :diminish (auto-revert-mode . "Ⓐ"))

(use-package osx-dictionary
  :bind (("C-c s d" . osx-dictionary-search-input)))

(use-package term
  :ensure shell-toggle
  :init
  (setq-default bidi-display-reordering nil)
  ;; (defun term-handle-more-ansi-escapes (proc char)
  ;;   "Handle additional ansi escapes."
  ;;   (cond
  ;;    ;; \E[nG - Cursor Horizontal Absolute, e.g. move cursor to column n
  ;;    ((eq char ?G)
  ;;     (let ((col (min term-width (max 0 term-terminal-parameter))))
  ;;       (term-move-columns (- col (term-current-column)))))
  ;;    (t)))
  ;; (advice-remove 'term-handle-ansi-escape :before #'term-handle-more-ansi-escapes)
  :config
  (yas-minor-mode -1))

(use-package so-long
  :defer 2
  :config
  (setq so-long-threshold 400)
  ; Add some font locking
  (setq so-long-minor-modes (delq 'font-lock-mode so-long-minor-modes))
  (add-to-list 'so-long-minor-modes 'lsp-mode)
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))

  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  (add-to-list 'so-long-target-modes 'text-mode)
  (setq so-long-variable-overrides (delq (assoc 'buffer-read-only so-long-variable-overrides) so-long-variable-overrides))
  (global-so-long-mode))

(use-package shell-pop
  :ensure t
  :bind ("C-`" . shell-pop)
  :init
  (setq shell-pop-window-position "bottom"
        shell-pop-window-size     30
        shell-pop-term-shell      "/bin/zsh"
        shell-pop-internal-mode   "ansi-term"))

(use-package eshell
  :ensure eshell-git-prompt
  ;;   https://github.com/ekaschalk/dotspacemacs/blob/master/.spacemacs
  :init (eshell-git-prompt-use-theme 'robbyrussell))

(use-package persistent-scratch
  :init
  :defer 1
  :config (persistent-scratch-restore)
  (persistent-scratch-setup-default))

(use-package alert
  :defer 10
  :init
  (defvar alert-default-style)
  (setq alert-default-style 'libnotify))


(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(use-package helpful
  :init
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(use-package desktop
  :defer 5
  :init
  (setq desktop-dirname "~/.emacs.d/")
  (defun +desktop-save()
    (interactive)
    (let ((desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name)))))
             (desktop-save desktop-dirname t)))
  (defun restore-from-desktop(&rest _)
    (interactive)
    (desktop-read desktop-dirname))
  (add-hook 'kill-emacs-hook #'+desktop-save))

(use-package restart-emacs
  :after desktop
  :init
  (add-to-list 'command-switch-alist (cons "--restore" #'restore-from-desktop))
  (defun restart-and-restore()
    (interactive)
    (+desktop-save)
    (restart-emacs '("--restore"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Languages / Syntax Major Mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package apache-mode :ensure t)
(use-package csv-mode :ensure t)
(use-package php-mode
  :ensure t
  :bind (:map php-mode-map
              ("C-c C-c" . nil)
              ("C-." . nil))
  :config
  (setq c-auto-align-backslashes nil)
  (add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)
  (setq c-basic-offset 4))

(use-package geben
  :config (setq geben-path-mappings '(("~/Development/zomato/application/" "/ssh:zdev:/var/www/zomato9/application"))))

(use-package abbrev
  :ensure nil
  :diminish "🆎")

(setq read-process-output-max (* 1024 1024)) ;; 1mb
(use-package lsp-mode
  :hook ((js-mode js2-mode js3-mode rjsx-mode go-mode rust-mode php-mode) . lsp)
  :init
  (setq lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  (setq lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)
  (setq lsp-intelephense-files-max-size 2000000))

(use-package dap-mode)

(use-package lsp-ivy
  :defer 2
  :bind (:map lsp-mode ("M-<return>" . lsp-ivy-workspace-symbol)))

(use-package lua-mode)
(use-package dockerfile-mode)
(use-package docker-compose-mode)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :ensure t
  :config

  (setq lsp-prefer-flymake nil
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil)

    (add-hook 'lsp-ui-doc-frame-hook
            (lambda (frame _w)
              (set-face-attribute 'default frame :font "Hack" :height 100)))

  )

(use-package lsp-imenu
  :ensure lsp-mode
  :config
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

;; (use-package lsp-php
;;   :disabled t
;;   :defer 1
;;   :config
;;   ;; (setq lsp-php-server-install-dir "/mnt/data/Development/")
;;   (setq lsp-php-language-server-command (quote ("/usr/bin/node" "/home/umar/.config/yarn/global/node_modules/intelephense-server/lib/server.js" "--stdio")))
;;   (add-hook 'php-mode-hook #'lsp-php-enable))

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-enable-sql-detection t)
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (flycheck-add-mode 'php 'web-mode)
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode)) ;; Using rjsx for that
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it)))

(use-package js-mode
  :ensure nil
  :init (setq js-indent-level 4))
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-basic-offset 4)
  ;; (load "lsp-javascript")
  ;; (add-hook 'js2-mode-hook (lambda ()
  ;;                            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  :bind (:map js2-mode-map ("M-." . nil))
  :ensure t)
(use-package rjsx-mode :mode ("\\.jsx\\'" . rjsx-mode) )
(use-package tern                       ; Javascript IDE backend
  :disabled t
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'tern-mode)
  :config
  ;; Don't generate port files
  (add-to-list 'tern-command "--no-port-file" 'append)
  :bind (:map tern-mode-keymap
              ("M-." . nil)
              ("M-," . nil))
  :diminish "🕊")

;; (use-package company-tern               ; Auto-completion for javascript
;;   :ensure t
;;   :after company
;;   :config (add-to-list 'company-backends 'company-tern))

;; (use-package js-doc :ensure t)
;; (use-package jsx-mode )
;; (use-package lsp-javascript-typescript
;;   :defer 1
;;   :hook ((js-mode . lsp-javascript-typescript-enable)
;;          (js2-mode . lsp-javascript-typescript-enable)
;;          (typescript-mode . lsp-javascript-typescript-enable)
;;          (js3-mode . lsp-javascript-typescript-enable)
;;          (rjsx-mode . lsp-javascript-typescript-enable)))

(use-package indium
  :config
  (setq indium-chrome-executable "google-chrome"))

(use-package json-mode
  :bind (:map json-mode-map ("C-c C-f" . json-pretty-print)))

(use-package less-css-mode)
(use-package rainbow-mode :diminish "🌈")
(use-package phpcbf
  :config
  (setq phpcbf-standard "~/Development/zomato/phpcs.xml"))

(use-package reformatter :disabled)

(use-package protobuf-mode
  :init
  ;; TODO: make a proper utility instead of this Macro
  (fset 'renumber-proto-message
   [?\C-\M-u ?\C-\M-s ?= ?\C-b ?\C-  ?\C-f ?\C-c ?m ?\C-  ?\C-k ?  ?\M-x ?m ?c ?/ ?i ?n ?s ?e ?r ?\C-n return C-S-up ?\; return])
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))

  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t)))
  (add-hook 'protobuf-mode-hook
          (function (lambda ()
                      (setq tab-width 2))))
  (defvar prototool-command)
  (setq prototool-command "/usr/local/bin/prototool")
  (defun prototool-format()
    (interactive)
    (message "Formatting proto file")
    (shell-command (concat
                    prototool-command
                    " format"
                    " -w "
                    (buffer-file-name)))
    (revert-buffer t t))
  (defun prototool-format-after-save()
    (interactive)
    (when (eq major-mode 'protobuf-mode)
      (prototool-format)))
  ;; (add-hook 'protobuf-mode
  ;;         (lambda ()
  ;;            (add-hook 'after-save-hook 'prototool-format-after-save nil t)))
  ;; (add-hook 'after-save-hook 'prototool-format-after-save)
  )

(use-package go-mode
  :ensure go-mode
  :ensure company-go
  :init
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  )

(use-package rust-mode
  :init
  (setq rust-format-on-save t)
  :config
  (flycheck-mode -1))

(use-package robe
  :ensure t
  :config
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby))
  (add-hook 'ruby-mode-hook 'robe-mode)
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package markdown-mode
  :ensure t
  :init
  (defvar markdown-command)
  (setq markdown-command "/usr/bin/pandoc"))

(use-package restclient :mode ("\\.rest\\'" . restclient-mode) )
(use-package company-restclient
  :after company
  :after restclient
  :config (add-to-list 'company-backends 'company-restclient))

;; PDF View
(use-package pdf-tools
  :defer 5
  :init
  (defvar pdf-annot-highlight-colors '(("blue" . "#40e0d0") ("yellow" . "#face50") ("red" . "#ff69b4")))
  (defun pdf-annot-choose-highlight-color()
    (interactive)
    (pdf-annot-update-highilight-color
     (cdr (assoc (completing-read "Choose highlight color: " pdf-annot-highlight-colors nil t) pdf-annot-highlight-colors))))
  ;; (defun pdf-annot-update-highilight-color(color)
  ;;   (setf (alist-get 'highlight pdf-annot-default-annotation-properties nil t) (cons 'color color)))
  (defun pdf-annot-update-highilight-color (color)
    (setq pdf-annot-default-annotation-properties
          (let* ((temp-props)
                 (acolor (cons 'color color))
                 (highlight-prop '()))
            (push acolor highlight-prop)
            (push 'highlight highlight-prop)
            (dolist (element pdf-annot-default-annotation-properties)
              (when (eq (eq (car element) 'highlight) nil)
                (push element temp-props)))
            (push highlight-prop  temp-props)
            temp-props)))
  ;; (pdf-view-midnight-colors '("#839496" . "#002b36"))
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (pdf-annot-update-highilight-color "#face50")
              ))
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("C-c C-a H" . pdf-annot-choose-highlight-color)
              ("D" . pdf-annot-delete)
              ("m" . pdf-view-midnight-minor-mode)
              ("t" . pdf-annot-add-text-annotation))

  :config
  (pdf-tools-install))

(use-package saveplace-pdf-view
  :hook (pdf-view-mode . (lambda ()
                           (require 'saveplace-pdf-view)
                           (save-place-mode t)
                           )))

;; W3M browser
(use-package w3m
  :init (setq w3m-search-default-engine "duckduckgo"))

(use-package olivetti)

;; Org mode settings
(use-package org-crypt
  :ensure nil
  :defer 4
  :init
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setf epa-pinentry-mode 'loopback)
  (setq org-crypt-key nil)
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  :config (org-crypt-use-before-save-magic))

;; To turn it off auto-save only locally, you can insert this:
;; # -*- buffer-auto-save-file-name: nil; -*-

(use-package org-bullets
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (use-package org-wunderlist
;;   :init (setq org-wunderlist-file  "~/.emacs.d/Wunderlist.org"
;;                 org-wunderlist-dir "~/.emacs.d/org-wunderlist/"))

(use-package org-alert
  :config (org-alert-enable))

(use-package org-pomodoro
  :config
  ;; '(org-pomodoro-finished-sound "/home/umar/.emacs.d/resources/doorbell.wav")
  ;; '(org-pomodoro-long-break-sound "/home/umar/.emacs.d/resources/doorbell.wav")
  ;; '(org-pomodoro-short-break-sound "/home/umar/.emacs.d/resources/doorbell.wav")
  ;; '(org-pomodoro-start-sound "/home/umar/.emacs.d/resources/doorbell.wav")
  )

(use-package org
  :init
  (defvar org-plantuml-jar-path)
  ;; helper function
  (defun my-org-confirm-babel-evaluate (lang body)
    "Do not ask for confirmation to evaluate code for specified languages."
    (member lang '("plantuml")))
  ;; trust certain code as being safe
  (setq org-ellipsis "…"
        org-agenda-files '("~/Dropbox/org-files/Tasks.org")
        org-log-done 'time
        org-startup-with-inline-images t
        org-hide-emphasis-markers t
        org-html-checkbox-type 'html
        org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar")
        org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "|" "DONE" "DELEGATED" "STALE"))
        ;; org-fontify-whole-heading-line t
        ;; org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-confirm-babel-evaluate nil
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  (font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)

  (custom-set-faces
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  :config
  ;; Support for plantuml
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

  ;; automatically show the resulting image
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  :bind (("C-c o a" . org-agenda)
         ("C-c o e" . org-export-dispatch)))


(use-package org-alert
  :init
  (setq alert-default-style 'osx-notifier))

;; Loading functions and variables
(delete-selection-mode +1)
(set-default 'imenu-auto-rescan t)
(setq-default frame-title-format '(buffer-file-name "Emacs - %b"))
(setq-default line-spacing 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq select-enable-clipboard t ;; Enabled emacs to use system clipboard
      select-enable-primary nil ;; Disable Copy on selection
      display-time-default-load-average nil
      save-interprogram-paste-before-kill t
      kill-ring-max 200
      kill-do-not-save-duplicates t
      apropos-do-all t
      use-dialog-box nil
      x-underline-at-descent-line t
      ring-bell-function 'ignore
      mouse-yank-at-point t
      require-final-newline t
      fast-but-imprecise-scrolling t
      frame-inhibit-implied-resize t
      highlight-nonselected-windows nil
      ediff-window-setup-function 'ediff-setup-windows-plain)

(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)
(setq enable-recursive-minibuffers t)
;; (setq redisplay-dont-pause t)
;; Backup
(setq version-control t
      kept-new-versions 10
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      backup-directory-alist '(("" . "~/.emacs.d/backups/per-save"))
      browse-url-browser-function 'browse-url-default-browser)


(setq-default cursor-in-non-selected-windows nil)
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)
(setq tab-always-indent t)
(put 'narrow-to-region 'disabled nil)
(provide 'core)
;;; core.el ends here
