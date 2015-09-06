;;; ui.el -- UI Properties file of Emacs
;;; Commentary:
;; Defines the commands considering the UI aspect
;;; Code:
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(size-indication-mode t)
(set-frame-font "Sauce Code Powerline 14")
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(load-theme 'apropospriate-dark t)
(show-paren-mode)
(setq-default line-spacing 0)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(global-hl-line-mode t)
;; hl-line+ feature
(hl-line-flash)
(hl-line-toggle-when-idle 1)
(hl-line-when-idle-interval 1)
(setq-default frame-title-format '(buffer-file-name "%b"))

;; (defun linum-format-func (line)
;;   "Defines the format for the linum mode for specific LINE."
;; (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
;;     (propertize (format (format " %%%dd " w) line) 'face 'linum)))

;; (setq linum-format 'dynamic)
;; (global-linum-mode -1)
;; (fringe-mode -1)
;; (setq-default cursor-type '(bar . 1))
(blink-cursor-mode -1)
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'respectful)
(add-hook 'after-init-hook #'sml/setup)
;; (set-face-attribute 'mode-line nil :font "Sauce Code Powerline 14")


(setq ring-bell-function 'ignore)

(require 'whitespace)
(setq whitespace-line -1)

(setq whitespace-style '(face tabs empty trailing lines-tail))
(global-whitespace-mode)
(provide 'ui)
;;; ui.el ends here
