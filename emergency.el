;;; init.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: May 05, 2022
;; Modified: May 05, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; 

;;; Code:
(defconst CACHE-DIR (expand-file-name "cache/" user-emacs-directory))
(defconst RES-DIR   (expand-file-name "resources/" user-emacs-directory))
(defconst IS-MAC    (eq system-type 'darwin))
(defconst IS-LINUX  (eq system-type 'gnu/linux))
(defconst IS-TERM   (not (display-graphic-p)))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(blink-cursor-mode -1)

(electric-pair-mode t)
(size-indication-mode t)
(line-number-mode t)
(column-number-mode t)

(global-set-key (kbd "M-p") 'project-find-file)

(global-set-key (kbd "M-/") 'hippie-expand)


(when IS-MAC
  (setq mac-command-modifier 'meta
        ;;       mac-option-modifier 'control
        ns-option-modifier 'super))

(fido-mode t)
(fido-vertical-mode t)

(load-theme 'modus-operandi t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq select-enable-clipboard t ;; Enabled emacs to use system clipboard
      select-enable-primary nil ;; Disable Copy on selection
      display-time-default-load-average nil
      save-interprogram-paste-before-kill t
      kill-ring-max 200
      kill-do-not-save-duplicates t
      apropos-do-all t
      use-dialog-box nil
      ring-bell-function 'ignore
      mouse-yank-at-point t
      require-final-newline t
      ;; scrolling
      fast-but-imprecise-scrolling t
      auto-window-vscroll nil
      scroll-preserve-screen-position t
      kill-do-not-save-duplicates t
      highlight-nonselected-windows nil
      ediff-window-setup-function 'ediff-setup-windows-plain)
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)
(setq enable-recursive-minibuffers t)

;;; init.el ends here
