(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(setq inhibit-startup-screen t
      initial-scratch-message "")
(blink-cursor-mode -1)
(setq frame-inhibit-implied-resize t)
;; Font/Themes
(defvar default-font "Fira Code 12")
(setq default-font "Fira Code 12")
(set-frame-font default-font 'keepsize t)
;; (set-face-font 'variable-pitch "Baskerville 15")
(set-face-font 'variable-pitch "ETBembo 17")
(set-face-font 'fixed-pitch default-font)

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
  :init
  (defvar doom-themes-treemacs-enable-variable-pitch)
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  :demand
  :config (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))



(use-package all-the-icons
  :demand)
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
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-checker-simple-format t)
  (doom-modeline-mode +1))

(use-package hide-mode-line)

(use-package simple
  :ensure nil
  :config
  (size-indication-mode t)
  (line-number-mode t)
  (column-number-mode t))

(use-package display-line-numbers
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :bind ("C-c t l". display-line-numbers-mode))


(use-package hl-line
  ;; Highlights the current line
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode)
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar doom--hl-line-mode nil))

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package highlight-indent-guides
  :bind ("C-c t h" . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package dashboard
  :after all-the-icons
  :demand
  :bind (:map dashboard-mode-map
              ("C-n" . widget-forward)
              ("C-p" . widget-backward))
  :init (setq dashboard-items '((recents  . 5)
                                (projects . 10)
                                ;; (bookmarks . 5)
                                ;; (agenda . 5)
                                ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 3)
  (setq dashboard-center-content t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)
  (setq dashboard-navigator-buttons
        `(
          ((,(all-the-icons-octicon "sync" :height 1.1 :v-adjust 0.0)
            "Restore"
            "Restore last session"
            (lambda (&rest _) (restore-from-desktop)) nil "" ""))))
  :config
  (dashboard-setup-startup-hook))

(setq-default frame-title-format '(buffer-file-name "Emacs - %b"))
(setq-default line-spacing 0)

(use-package minimap
  :defer t
  :bind ("C-c t m" . minimap-mode)
  :config
  (setq minimap-window-location 'right
        minimap-update-delay 0
        minimap-width-fraction 0.09
        minimap-minimum-width 15))

(use-package olivetti
  :bind ("C-c t z" . olivetti-mode)
  :config
  (defun olivetti-custom-width()
    (interactive)
    (setq olivetti-body-width
          (string-to-number
           (completing-read "Width" '("160" "150" "100" "70") nil t)))
    (olivetti-set-buffer-windows)))

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


(provide 'core-ui)
