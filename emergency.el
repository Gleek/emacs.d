;;; init.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: May 05, 2022
;; Modified: May 05, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:
;; This file is never loaded in normal use and is meant to be used as a minimal init.el
;; in another Emacs where I don't have the liberty to clone my full config.
;; These are the principles for this file:
;; - Single file: So it's easy to copy paste - email to someone
;; - Only uses Emacs defaults - no fluff packages
;; - Assumes the latest stable Emacs version
;; - No dependency on extra packages unless absolutely necessary (for eg language major modes - but only when visiting those files)
;; - Future: can include some helpful utilities like flmake chekers and dumb jump on request basis.
;;

;;; Code:
(defconst CACHE-DIR (expand-file-name "cache/" user-emacs-directory))
(defconst RES-DIR   (expand-file-name "resources/" user-emacs-directory))
(defconst IS-MAC    (eq system-type 'darwin))
(defconst IS-LINUX  (eq system-type 'gnu/linux))
(defconst IS-TERM   (not (display-graphic-p)))

(unless IS-TERM
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-i] [C-i]))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(blink-cursor-mode -1)

(electric-pair-mode t)
(size-indication-mode t)
(line-number-mode t)
(column-number-mode t)
(display-line-numbers-mode t)

;; (setq modus-themes-subtle-line-numbers nil)
(setq modus-themes-mode-line '((padding . 4)))
(setq inhibit-startup-screen t
      initial-scratch-message "")

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
(global-set-key (kbd "C-:") 'other-window)
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)
(global-set-key (kbd "M-[") 'unwrap-sexp)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "C-^") 'top-join-line)
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-`") 'pop-eshell)
(global-set-key (kbd "<C-m> a") 'rename-all-occurences)

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
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
                 (message "Line copied") (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))


(defun comment-or-uncomment-region-or-line ()
  "Comment a line or region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))


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
                      (push-mark (point-min))
                      (goto-char (point-max))
                      (activate-mark)
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

(defun unwrap-sexp()
  (interactive)
  (save-excursion
    (let (start end)
      (catch 'ex
        (condition-case nil (backward-up-list 1 t nil)
          (error (throw 'ex 1)))
        (setq start (point))
        (forward-sexp)
        (backward-delete-char 1)
        (goto-char start)
        (delete-char 1)))
    (indent-sexp)))


(defun beginning-of-line-or-indentation ()
  "Move to beginning of line, or indentation."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(defun move-line(&optional dir)
  (let (line-content)
    (setq line-content (thing-at-point 'line t))
    (delete-current-line)
    (forward-line (if (eq dir 'up) -1 1))
    (insert line-content)
    (forward-line -1)
    (indent-for-tab-command)))

(defun move-line-up()
  (interactive)
  (move-line 'up))

(defun move-line-down()
  (interactive)
  (move-line 'down))


(defun top-join-line()
  (interactive)
  (delete-indentation -1))

(defun delete-current-line ()
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun duplicate-current-line-or-region ()
  "Duplicates the current line or region.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive)
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (goto-char end)
    (newline)
    (insert region)
    (setq end (point))))

(defun pop-eshell()
  (interactive)
  (require 'eshell)
  (if (eq major-mode 'eshell-mode)
      (progn (bury-buffer)
             (delete-window))
    (let ((c-directory default-directory))
      (split-window-below)
      (other-window 1 nil)
      (eshell)
      (message "Switching to %s" c-directory)
      (eshell/cd c-directory))))

(defun rename-all-occurences()
  (interactive)
  (let ((word (thing-at-point 'symbol t))
        (final ())
        (case-fold-search nil))
    (when word
      (setq final (read-string (format "Replace %s with : " word)))
      (query-replace word final t (point-min) (point-max)))))



;; Only install very critical packages on need basis
(setq file-packages
      '(("php" . (php-mode))
        ("go" . (go-mode))))

(defun check-essential-packages()
  (let (file-name extension ipackages)
    (setq file-name (buffer-file-name (current-buffer)))
    (when file-name
      (setq extension (file-name-extension file-name))
      (mapc (lambda(o)
              (when (equal extension (car o))
                (setq ipackages (cdr o))
                (mapc (lambda(p)
                        (if (not (package-installed-p p))
                            (progn
                              (message "Installing package %S" p)
                              (package-install p))))
                      ipackages)))
            file-packages))))
(add-hook 'find-file-hook 'check-essential-packages)

;;; init.el ends here
