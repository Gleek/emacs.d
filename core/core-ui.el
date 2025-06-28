(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(menu-bar-mode -1)
(setq inhibit-startup-screen t
      initial-scratch-message "")
(blink-cursor-mode -1)
(setq frame-inhibit-implied-resize t)
;; Font/Themes
(defvar default-font "Iosevka Term")
(set-frame-font (concat default-font " 14") 'keepsize t)
;; (set-face-font 'variable-pitch "Baskerville 15")
;; (set-face-font 'variable-pitch "ETBembo 17")

;; (if IS-MAC
;;     (set-face-attribute 'variable-pitch nil :family "Cusvenir Next" :height 1.2)
;;   (set-face-attribute 'variable-pitch nil :family "Helvetica" :height 1.1))
(set-face-attribute 'variable-pitch nil :family "Roboto Mono" :height 1.0)
(set-face-attribute 'fixed-pitch nil :family default-font :height 1.0)
(set-fontset-font t 'arabic "KFGQPC Uthmanic Script Hafs 25")

(defun +italic-comments(&rest _)
  (set-face-attribute 'font-lock-comment-face nil :inherit 'italic))

(defun +bold-function-def(&rest _)
  "Increase of function definitions to make it pop out."
  (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
  ;; resetting the weight of faces that inherit it directly.
  (eval-after-load 'consult
    '(set-face-attribute 'consult-file nil :weight 'normal))
  (set-face-attribute 'font-lock-function-call-face nil :weight 'normal))

(add-hook 'enable-theme-functions #'+italic-comments)
(add-hook 'enable-theme-functions #'+bold-function-def)


(defvar +checker-line-style 'wave) ;; line / wave



(setq-default frame-title-format
              '(""
                "%b"
                (:eval
                 (let ((project-name
                        (if (fboundp 'projectile-project-name)
                            (projectile-project-name)
                          "-")))
                   (unless (string= "-" project-name)
                     (format " [%s]" project-name))))
                " - Emacs"))

;; Space out a little
(setq-default line-spacing 0.1)
(defun set-bigger-spacing ()
  (setq-local default-text-properties '(line-spacing 0.10 line-height 1.10)))
(add-hook 'text-mode-hook 'set-bigger-spacing)
(add-hook 'prog-mode-hook 'set-bigger-spacing)
(add-hook 'conf-mode-hook 'set-bigger-spacing)


(setq x-underline-at-descent-line nil)
(setq x-use-underline-position-properties t)


(setq resize-mini-windows 'grow-only)

(when IS-MAC
  (setq ns-auto-hide-menu-bar nil)
  (setq ns-use-srgb-colorspace t)
  (set-fontset-font
   t 'symbol (font-spec :family "Apple Color Emoji") nil)
  (add-to-list
   'default-frame-alist'(ns-transparent-titlebar . t))
  (use-package ns-auto-titlebar
    :defer 5
    :ensure t
    :config
    (ns-auto-titlebar-mode)))


(use-package ligature
  :defer 1
  :config
  (ligature-set-ligatures 't '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "++" "+++"))
  (global-ligature-mode t))

(use-package solaire-mode
  :demand t
  :config
  (solaire-global-mode t))

(use-package hide-mode-line
  :bind ("C-c t M" . hide-mode-line-mode)
  :ensure (:wait t))

(use-package nerd-icons :ensure (:wait t))

(use-package doom-themes
  ;; :ensure solaire-mode
  :init
  (defvar doom-themes-treemacs-enable-variable-pitch)
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  :bind ("C-c t T" . +switch-theme-type)
  :demand t
  :config
  (defvar +theme-type 'light)
  (when IS-MAC
    (setq +theme-type ns-system-appearance))
  (defvar +light-theme 'doom-one-light)
  (defvar +dark-theme 'doom-one)

  (defun +disable-all-themes()
    (interactive)
    (mapc #'disable-theme custom-enabled-themes))

  (defun +apply-theme (theme-type)
    (+disable-all-themes)
    (setq +theme-type theme-type)
    (disable-theme 'solaire-swap-bg-theme)
    (let ((theme (if (eq theme-type 'light) +light-theme +dark-theme)))
      (load-theme theme t t)
      (enable-theme theme))
    (solaire-global-mode t)
    (set-frame-parameter nil 'background-mode +theme-type))

  (defun +switch-theme-type()
    (interactive)
    (+apply-theme (if (eq +theme-type 'light) 'dark 'light)))

  (+apply-theme +theme-type)
  (doom-themes-org-config)
  (add-hook 'ns-system-appearance-change-functions #'+apply-theme))

;; (use-package ef-themes)
(use-package spacious-padding
  :demand t
  :config
  (spacious-padding-mode t))

(use-package doom-modeline
  ;; :hook (after-init . doom-modeline-mode)
  :init
  (doom-modeline-mode t)
  :config
  (line-number-mode -1)
  (setq doom-modeline-vcs-max-length 20)
  (setq doom-modeline-height 19)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-lsp nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project))


(use-package emacs
  :ensure nil
  :config
  (size-indication-mode t)
  ;; (line-number-mode t)
  (column-number-mode t))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :bind ("C-c t l". display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-widen t)
  (setq display-line-numbers-width-start t))

(use-package hl-line
  :ensure nil
  ;; Highlights the current line
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode))

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;; Highlight long numbers to make them easy to read
(use-package num3-mode
  :hook ((prog-mode text-mode conf-mode) . num3-mode)
  :config
  (setq num3-group-size 3)
  (setq num3-threshold 6)
  (defun num3-face-setup(&rest _)
    (set-face-attribute 'num3-face-even nil :underline t :weight 'bold :background nil))
  (add-hook 'enable-theme-functions #'num3-face-setup)
  (num3-face-setup))

(use-package highlight-indent-guides
  :bind ("C-c t h" . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package indent-bars
  ;; Even though this has stipples, so no breaking lines but they are too dark.
  ;; Changing the color isn't working for stipples, might be an emacs bug.
  :disabled
  ;; :bind ("C-c t h" . +toggle-indent-bars)
  :ensure (:fetcher github :repo "jdtsmith/indent-bars")
  :config
  ;; Workaround until https://github.com/jdtsmith/indent-bars/issues/31#issuecomment-2031605144 is fixed
  (remove-hook 'enable-theme-functions #'indent-bars-reset)
  (defun +toggle-indent-bars()
    (interactive)
    (if indent-bars-mode
        (progn
          (indent-bars-mode -1)
          (remove-hook 'enable-theme-functions #'indent-bars-reset))
      (indent-bars-mode t)
      (add-hook 'enable-theme-functions #'indent-bars-reset)))

  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.2)
   ;; ideally should be nil but the blend doesn't work on my emacs.
   ;; Though stipple lines are visually better but dark black lines are too distracting.
   indent-bars-prefer-character nil
   indent-bars-pattern "."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth nil
   indent-bars-highlight-current-depth nil
   indent-bars-display-on-blank-lines nil))

(use-package doom-dashboard :ensure nil :demand t
  :bind (:map +doom-dashboard-mode-map
              ([left-margin mouse-1]  .  nil)
              ([remap forward-button] .  #'+doom-dashboard/forward-button)
              ([remap backward-button].  #'+doom-dashboard/backward-button)
              ("n"      .  #'forward-button)
              ("p"      .  #'backward-button)
              ("C-n"    .  #'forward-button)
              ("C-p"    .  #'backward-button)
              ([down]   .  #'forward-button)
              ([up]     .  #'backward-button)
              ([tab]    .  #'forward-button)
              ([backtab].  #'backward-button))
  :config
  (setq +doom-dashboard-banner-dir RES-DIR))


;; (use-package doom-dashboard
;;   :ensure (:fetcher github :repo "gleek/doom-dashboard"))


(use-package olivetti
  :init
  (setq-default olivetti-body-width 130)
  :bind ("C-c t z" . +focus-mode)
  :config

  (defun +focus-mode()
    (interactive)
    (if (and (fboundp 'olivetti-mode) olivetti-mode)
        (progn
          ;; (display-line-numbers-mode +1)
          (olivetti-mode -1))
      ;; (display-line-numbers-mode -1)
      (olivetti-mode +1)))

  (defun olivetti-custom-width()
    (interactive)
    (setq-default olivetti-body-width
                  (string-to-number
                   (completing-read "Width" '("200" "160" "150" "100" "70") nil t)))
    (olivetti-reset-all-windows)))

(use-package centered-window)


(use-package mini-frame
  :bind ("C-c t m" . +toggle-mini-frame)
  :config
  (defun +toggle-mini-frame()
    (interactive)
    (if mini-frame-mode
        (mini-frame-mode -1)
      (mini-frame-mode 1)))

  (setq mini-frame-resize t)
  (setq mini-frame-show-parameters
        '((left . 0.5)
          (top . 0.3)
          (left-fringe . 12)
          (child-frame-border-width . 3)
          (internal-border-width . 1)
          (right-fringe . 12)
          (width . 0.80)))
  (setq mini-frame-ignore-functions '(y-or-n-p yes-or-no-p))
  (add-to-list 'mini-frame-ignore-commands 'completion-at-point)
  (add-to-list 'mini-frame-ignore-commands 'git-gutter:revert-hunk)
  (mini-frame-mode 1))

(use-package treesit
  :ensure nil
  :init
  (defvar treesit-language-available-p-memoized-cache (make-hash-table :test 'equal)
    "Cache for `treesit-language-available-p' results.")
  ;; (clrhash treesit-language-available-p-memoized-cache)  ; To clear
  (advice-add 'treesit-language-available-p :around
              (lambda (orig-fn &rest args)
                (let* ((key (cons (car args) (cadr args)))
                       (cached (gethash key treesit-language-available-p-memoized-cache)))
                  (or cached
                      (puthash key (apply orig-fn args) treesit-language-available-p-memoized-cache)))))
  (setq treesit-extra-load-path (list (concat RES-DIR "treesit/")))
  ;; signifies the level to render treesit-font-lock-feature-list.
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :defer 2
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; (use-package tree-sitter
;;   :ensure tree-sitter
;;   :ensure tree-sitter-langs
;;   :defer 2
;;   :config
;;   ;; (require 'tree-sitter-langs)
;;   ;; (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package hl-todo
  :hook ((prog-mode yaml-mode) . hl-todo-mode)
  :config
  (defun +hl-todo-regex()
    (concat "\\(\\b" (mapconcat 'car hl-todo-keyword-faces "\\b\\|\\b") "\\b\\)"))
  (defun +hl-todo-project()
    (interactive)
    (consult-ripgrep nil (+hl-todo-regex))))

(defun toggle-cursor-type()
  (interactive)
  (setq cursor-type
        (cond
         ((eq cursor-type t) 'bar)
         (t 't))))

(defun reading-mode()
  (interactive)
  (setq cursor-type 'bar)
  (electric-quote-local-mode t)
  (setq bidi-paragraph-direction nil)
  ;; (variable-pitch-mode t) ; disabling for now. Doesn't support all symbols
  (display-line-numbers-mode -1)
  (setq left-margin-width 5)
  (setq right-margin-width 5))


(provide 'core-ui)
