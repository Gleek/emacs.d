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

;; (setq modus-themes-subtle-line-numbers nil)
(setq modus-themes-mode-line '((padding . 4)))
(load-theme 'modus-operandi t)

(set-face-attribute 'default nil :height 130)
(setq-default line-spacing 0.2)


;; Keybindings
(global-set-key (kbd "M-p") 'project-find-file)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c s s") 'project-find-regexp)
(global-set-key (kbd "C-=") 'smart-mark-sexp)
(global-set-key (kbd "C-+") 'mark-this-sexp)
(when IS-MAC
  (setq mac-command-modifier 'meta
        ;; mac-option-modifier 'control
        ns-option-modifier 'super))


(fido-mode t)
(fido-vertical-mode t)



(setq project-list-file (expand-file-name "projects" CACHE-DIR))

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
      x-underline-at-descent-line t
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

(setq search-highlight t)
(setq search-whitespace-regexp ".*?")
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace nil)
(setq isearch-lazy-highlight t)
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format " (%s/%s)")
(setq isearch-yank-on-move 'shift)
(setq isearch-allow-scroll 'unlimited)


;; Utility functions
(defun comment-or-uncomment-region-or-line ()
  "Comment a line or region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))

(defun mark-inside-sexp ()
  "Mark inside a sexp."
  (interactive)
  (let (beg end)
    (backward-up-list 1 t nil)
    (setq beg (1+ (point)))
    (forward-sexp)
    (setq end (1- (point)))
    (goto-char beg)
    (push-mark)
    (goto-char end))
  (activate-mark))


(defun smart-mark-sexp()
  "Poor man's expand region"
  (interactive)
  (if (region-active-p)
      (expand-sexp)
    (mark-this-sexp)))

(defun expand-sexp()
  (interactive)
  (catch 'ex (deactivate-mark)
	 (let (beg start end inner )
	   (setq beg (point))
	   (setq inner t)
	   (condition-case nil (backward-up-list 1 t nil)
	     (error (progn
		      (mark-whole-buffer)
		      (throw 'ex 1))))
	   (if (eq beg (1+ (point)))
		 (setq inner nil))
	   (setq start (if inner (1+ (point)) (point)))
	   (forward-sexp)
	   (setq end (if inner (1- (point)) (point)))
	   (goto-char end)
	   (push-mark)
	   (goto-char start))
	 (activate-mark)))

(defun mark-this-sexp()
  (interactive)
  (ignore-errors (backward-sexp))
  (mark-sexp))



;;; init.el ends here
