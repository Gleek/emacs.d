(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(size-indication-mode t)
(set-frame-font "Sauce Code Powerline 14")
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(load-theme 'zenburn t)
(show-paren-mode)
(setq-default line-spacing 0)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq-default frame-title-format '(buffer-file-name "%b"))
;; (defun linum-format-func (line)
;;   (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
;;     (propertize (format (format " %%%dd " w) line) 'face 'linum)))
;; (setq linum-format 'linum-format-func)
;; (global-linum-mode 1)
;; (setq-default cursor-type '(bar . 1))

(blink-cursor-mode -1)

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'respectful)
(add-hook 'after-init-hook #'sml/setup)



(require 'whitespace)
(setq whitespace-line -1)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(global-whitespace-mode)
;; (setq whitespace-display-mappings '((tab-mark 9 [9655 9] [92 9])))
;; (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
;; (newline-mark 10 [182 10]) ; 10 LINE FEED
;; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」

(provide 'ui)
;;; ui.el ends here
