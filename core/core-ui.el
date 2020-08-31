(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq inhibit-startup-screen t
      initial-scratch-message "")
(blink-cursor-mode -1)
(setq frame-inhibit-implied-resize t)
;; Font/Themes
(defvar default-font "Source Code Pro 13")

(set-frame-font default-font 'keepsize t)
;; (set-face-font 'variable-pitch "Baskerville 15")
(set-face-font 'variable-pitch "ETBembo 17")
(set-face-font 'fixed-pitch default-font)

(when IS-MAC
  (setq mac-command-modifier 'meta
        ;;       mac-option-modifier 'control
        ns-option-modifier 'super)
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
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
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
  (setq doom-modeline-checker-simple-format nil)
  (doom-modeline-mode +1))

(use-package hide-mode-line)

(use-package simple
  :ensure nil
  :config
  (size-indication-mode t)
  (line-number-mode t)
  (column-number-mode t))

(use-package display-line-numbers
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode))


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
          ((,(all-the-icons-octicon "history" :height 1.1 :v-adjust 0.0)
            "Restore"
            "Restore last session"
            (lambda (&rest _) (restore-from-desktop)) nil "" ""))))
  :config
  (dashboard-setup-startup-hook))

(setq-default frame-title-format '(buffer-file-name "Emacs - %b"))
(setq-default line-spacing 0)

(use-package olivetti
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

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)


(provide 'core-ui)
