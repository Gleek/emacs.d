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
(defvar default-font "Jetbrains Mono")
(set-frame-font (concat default-font " 13") 'keepsize t)
;; (set-face-font 'variable-pitch "Baskerville 15")
;; (set-face-font 'variable-pitch "ETBembo 17")

;; (if IS-MAC
;;     (set-face-attribute 'variable-pitch nil :family "Cusvenir Next" :height 1.2)
;;   (set-face-attribute 'variable-pitch nil :family "Helvetica" :height 1.1))
(set-face-attribute 'variable-pitch nil :family "Roboto Mono" :height 1.1)
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
;; ligatures
(let ((alist
       '(
         (33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
         (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
         (36 . ".\\(?:>\\)")
         (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
         (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
         (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
         (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
         (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
         (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
         (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
         (48 . ".\\(?:x[a-zA-Z]\\)")
         (58 . ".\\(?:::\\|[:=]\\)")
         (59 . ".\\(?:;;\\|;\\)")
         (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
         (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
         (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
         (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
         (91 . ".\\(?:]\\)")
         (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
         (94 . ".\\(?:=\\)")
         (119 . ".\\(?:ww\\)")
         (123 . ".\\(?:-\\)")
         (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
         (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

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
(setq-default line-spacing 0.2)
(defun set-bigger-spacing ()
  (setq-local default-text-properties '(line-spacing 0.20 line-height 1.20)))
(add-hook 'text-mode-hook 'set-bigger-spacing)
(add-hook 'prog-mode-hook 'set-bigger-spacing)
(add-hook 'conf-mode-hook 'set-bigger-spacing)


(setq x-underline-at-descent-line nil)
(setq x-use-underline-position-properties t)


(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

(when IS-MAC
  (setq ns-auto-hide-menu-bar nil)
  (setq ns-use-srgb-colorspace t)
  (set-fontset-font
   t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (add-to-list
   'default-frame-alist'(ns-transparent-titlebar . t))
  (use-package ns-auto-titlebar
    :defer 5
    :ensure t
    :config
    (ns-auto-titlebar-mode)))



(use-package doom-themes
  :ensure doom-themes
  :ensure solaire-mode
  :init
  (defvar doom-themes-treemacs-enable-variable-pitch)
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  :bind ("C-c t T" . +switch-theme-type)
  :demand
  :config
  (defvar +theme-type 'light)
  (when IS-MAC
    (setq +theme-type ns-system-appearance))
  (defvar +light-theme 'doom-one-light)
  (defvar +dark-theme 'doom-one)

  (defun +switch-theme-type(&optional theme)
    (interactive)
    (let ((rev-theme (pcase theme ('light 'dark) ('dark 'light))))
      (setq +theme-type (if rev-theme rev-theme +theme-type)))
    (disable-theme 'solaire-swap-bg-theme)
    (if (eq +theme-type 'light)
        (progn
          (disable-theme +light-theme)
          (load-theme +dark-theme t)
          (setq +theme-type 'dark))
      (disable-theme +dark-theme)
      (load-theme +light-theme t)
      (setq +theme-type 'light))
    (set-frame-parameter nil 'background-mode +theme-type))
  (solaire-global-mode +1)
  (load-theme (if (eq +theme-type 'dark) +dark-theme +light-theme) t)
  (+italic-comments)
  (+bold-function-def)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  ;; (add-hook 'ns-system-appearance-change-functions
  ;;           #'(lambda (appearance)
  ;;               (+switch-theme-type appearance)
  ;;               ;; (mapc #'disable-theme custom-enabled-themes)
  ;;               ;; (pcase appearance
  ;;               ;;   ('light (progn (load-theme 'doom-one-light t) (solaire-global-mode +1)))
  ;;               ;;   ('dark (progn (load-theme 'doom-one t) (solaire-global-mode +1))))
  ;;               ))
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-vcs-max-length 20)
  (setq doom-modeline-height 25)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-checker-simple-format nil))

(use-package hide-mode-line
  :bind ("C-c t M" . hide-mode-line-mode))

(use-package emacs
  :ensure nil
  :config
  (size-indication-mode t)
  (line-number-mode t)
  (column-number-mode t))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :bind ("C-c t l". display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-widen t))

(use-package hl-line
  :ensure nil
  ;; Highlights the current line
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode))

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package highlight-indent-guides
  :disabled
  ;; :bind ("C-c t h" . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package indent-bars
  :vc (:fetcher github :repo jdtsmith/indent-bars)
  :bind ("C-c t h" . +toggle-indent-bars)
  :ensure nil
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
   indent-bars-prefer-character t
   indent-bars-pattern "."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth nil
   indent-bars-highlight-current-depth nil
   indent-bars-display-on-blank-lines nil))



(use-package doom-dashboard :ensure nil :demand t
  :bind (:map +doom-dashboard-mode-map
              ([left-margin mouse-1]  .  #'ignore)
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
  (add-to-list 'mini-frame-ignore-commands 'swiper)
  (add-to-list 'mini-frame-ignore-commands 'swiper-from-isearch)
  (add-to-list 'mini-frame-ignore-commands 'counsel-rg)
  (add-to-list 'mini-frame-ignore-commands 'completion-at-point)
  (add-to-list 'mini-frame-ignore-commands 'git-gutter:revert-hunk)
  (mini-frame-mode 1))


(use-package treesit
  :ensure nil
  :init
  (setq treesit-extra-load-path (list (concat RES-DIR "treesit/")))
  ;; signifies the level to render treesit-font-lock-feature-list.
  (setq treesit-font-lock-level 4))

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
  (variable-pitch-mode t)
  (display-line-numbers-mode -1)
  (setq left-margin-width 5)
  (setq right-margin-width 5))


(provide 'core-ui)
