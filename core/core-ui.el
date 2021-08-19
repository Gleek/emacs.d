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
(set-frame-font (concat default-font " 12") 'keepsize t)
;; (set-face-font 'variable-pitch "Baskerville 15")
;; (set-face-font 'variable-pitch "ETBembo 17")

(if IS-MAC
    (set-face-attribute 'variable-pitch nil :family "Cusvenir Next" :height 1.2)
  (set-face-attribute 'variable-pitch nil :family "Helvetica" :height 1.1))
(set-face-attribute 'fixed-pitch nil :family default-font :height 1.0)
(set-fontset-font t 'arabic "KFGQPC Uthmanic Script Hafs 25")

;; ligatures
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
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


(setq-default frame-title-format '(buffer-file-name "%b - Emacs"))

;; Space out a little
(setq-default line-spacing 0.2)
(setq-default default-text-properties '(line-spacing 0.20 line-height 1.20))
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
  (add-to-list
   'default-frame-alist'(ns-appearance . light)))


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
  (defun +switch-theme-type()
    (interactive)
    (disable-theme 'solaire-swap-bg-theme)
    (if (eq +theme-type 'light)
        (progn
          (disable-theme +light-theme)
          (load-theme +dark-theme t)
          (setq-local +theme-type 'dark))
      (disable-theme +dark-theme)
      (load-theme +light-theme t)
      (setq-local +theme-type 'light)))

  (solaire-global-mode +1)
  (load-theme (if (eq +theme-type 'dark) +dark-theme +light-theme) t)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  ;; (add-hook 'ns-system-appearance-change-functions
  ;;           #'(lambda (appearance)
  ;;               (mapc #'disable-theme custom-enabled-themes)
  ;;               (pcase appearance
  ;;                 ('light (progn (load-theme 'doom-one-light t) (solaire-global-mode +1)))
  ;;                 ('dark (progn (load-theme 'doom-one t) (solaire-global-mode +1))))))
  )

(use-package posframe
  :defer 5
  :config
  ;; Switching buffers with keychords keeps the posframe open.
  ;; Adding a custom hook and running this fixes this
  (add-hook '+quick-switch-buffer-hook 'posframe-delete-all))

(use-package all-the-icons
  :defer 1)
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
  :demand
  :config
  (setq doom-modeline-vcs-max-length 20)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-checker-simple-format t)
  (doom-modeline-mode +1))

(use-package hide-mode-line
  :bind ("C-c t M" . hide-mode-line-mode))

(use-package simple
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
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode)
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package highlight-indent-guides
  :bind ("C-c t h" . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

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
              ([backtab].  #'backward-button)))

(use-package dashboard
  :disabled
  :after all-the-icons
  :demand
  :bind (:map dashboard-mode-map
              ("C-n" . widget-forward)
              ("C-p" . widget-backward)
              ("A"   . +switch-to-agenda)
              ("S"   . +switch-to-scratch)
              ("R"   . restore-from-desktop))
  :init (setq dashboard-items '((recents  . 5)
                                (projects . 10)
                                ;; (bookmarks . 5)
                                ;; (agenda . 5)
                                ))
  ;; (setq dashboard-items nil)

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner (expand-file-name "banner.png" user-emacs-directory))
  (setq dashboard-center-content t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)
  (setq dashboard-navigator-buttons
        `(
          ((,(all-the-icons-octicon "sync" :height 1.1 :v-adjust 0.0)
            "Restore (R)"
            "Restore last session"
            (lambda (&rest _) (restore-from-desktop)) nil "" "\t")

           (,(all-the-icons-octicon "calendar" :height 1.1 :v-adjust 0.0)
            "Agenda (A) "
            "Open agenda"
            (lambda (&rest _) (+switch-to-agenda)) nil "" "\t")

           (,(all-the-icons-octicon "pencil" :height 1.1 :v-adjust 0.0)
            "Scratch (S) "
            "Open scratch"
            (lambda (&rest _) (+switch-to-scratch)) nil "" ""))))
  :config

  (defun +recenter-large-banner(f &rest arg)
    (face-spec-set
     'dashboard-text-banner
     '((t :inherit font-lock-keyword-face :font "Fira Mono 2"))
     'face-defface-spec)
    (let ((dashboard-banner-length (* 7 (window-width)))
          (title dashboard-banner-logo-title))
      (apply f arg)
      (insert "\n\n")))
  (advice-add 'dashboard-insert-ascii-banner-centered :around '+recenter-large-banner)

  (defun +switch-to-scratch()
    (interactive)
    (switch-to-buffer "*scratch*"))

   (dashboard-setup-startup-hook))

;; (use-package minimap
;;   :bind ("C-c t m" . minimap-mode)
;;   :config
;;   (add-hook 'minimap-sb-mode-hook (lambda()
;;                                     (setq-local default-text-properties '(line-spacing 0.0 line-height 1.0))))
;;   (setq minimap-window-location 'right
;;         minimap-update-delay 0
;;         minimap-width-fraction 0.09
;;         minimap-minimum-width 15))

(use-package olivetti
  :init
  (setq-default olivetti-body-width 130)
  :bind ("C-c t z" . +focus-mode)
  :config
  (defun +focus-mode()
    (interactive)
    (if olivetti-mode
        (progn
          (display-line-numbers-mode +1)
          (olivetti-mode -1))
      (display-line-numbers-mode -1)
      (olivetti-mode +1)))
  (defun olivetti-custom-width()
    (interactive)
    (setq olivetti-body-width
          (string-to-number
           (completing-read "Width" '("160" "150" "100" "70") nil t)))
    (olivetti-set-buffer-windows)))

(use-package mini-frame
  :defer 1
  :config
  (setq mini-frame-show-parameters
        '((left . 0.5)
          (top . 0.3)
          (width . 0.6)))
  (mini-frame-mode 1)
  (add-to-list 'mini-frame-ignore-commands 'swiper)
  (add-to-list 'mini-frame-ignore-commands 'git-gutter:revert-hunk))


(use-package tree-sitter
  :ensure tree-sitter
  :ensure tree-sitter-langs
  :defer 2
  :config
  ;; (require 'tree-sitter-langs)
  ;; (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defun toggle-cursor-type()
  (interactive)
  (setq cursor-type
        (cond
         ((eq cursor-type t) 'bar)
         (t 't))))

(defun reading-mode()
  (interactive)
  (setq cursor-type 'bar)
  (electric-quote-mode t)
  (setq bidi-paragraph-direction nil)
  (variable-pitch-mode t)
  (display-line-numbers-mode -1)
  (setq left-margin-width 5)
  (setq right-margin-width 5))


(provide 'core-ui)
