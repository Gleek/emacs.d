;;; init.el -- Emacs configuration entry point.
;;; Commentary:
;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs.

;;; Code:
(setq gc-cons-threshold 100000000) ;; collect garbage after about 100 MB
(require 'package)
(package-initialize)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq custom-file "~/.emacs.d/custom/emacs-custom.el")
(load custom-file)
(add-to-list 'load-path "~/.emacs.d/custom/")
(use-package personal
  :bind (("C-x k"                 . kill-this-buffer)
         ("M-;"                   . comment-or-uncomment-region-or-line)
         ("C-a"                   . beginning-of-line-or-indentation)
         ("C-x a r"               . align-regexp)
         ("C-c n"                 . cleanup-buffer)
         ("C-x <right>"           . windmove-right)
         ("C-x <left>"            . windmove-left)
         ("C-x <up>"              . windmove-up)
         ("C-x <down>"            . windmove-down)
         ("C-c r"                 . rename-file-and-buffer)
         ("C-x C-r"               . sudo-edit)
         ("C-x 2"                 . vsplit-last-buffer)
         ("C-x 3"                 . hsplit-last-buffer)
         ("C-c d"                 . duplicate-current-line-or-region)
         ("C-c c"                 . duplicate-and-comment-current-line-or-region)
         ("C-^"                   . top-join-line)
         ("C-c C-t x"             . TeX-toggle-escape)
         ("C-c s d"               . lucky)
         ("C-c e"                 . eval-and-replace)
         ([remap kill-whole-line] . smart-kill-whole-line)
         ([(shift return)]        . smart-open-line)
         ([remap goto-line]       . goto-line-with-feedback)))

(load "core")
(load "osx")
(load "web-settings")
(load "nonsvn")
(load "xwidconf")
;; Load individual modules
;; (add-to-list 'load-path "~/.emacs.d/custom/modules")
;; (load "helm-settings")
;; (load "ivy-config")
;; (load "org-mode-config")
(provide 'init)
;;; init.el ends here
