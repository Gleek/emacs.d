;;; init.el -- Emacs configuration entry point.
;;; Commentary:
;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs.

;;; Code:

;; (setq debug-on-error t)
(setq gc-cons-threshold 100000000) ;; collect garbage after about 100 MB
(run-with-idle-timer 2 t (lambda () (garbage-collect)))
(setq message-log-max 10000) ;; Debugging
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(use-package diminish)

(setq custom-file "~/.emacs.d/custom/emacs-custom.el")
(load custom-file)
(add-to-list 'load-path "~/.emacs.d/custom/")

(use-package personal
  :ensure nil
  :bind (("C-x k"                 . kill-current-buffer)
         ("M-;"                   . comment-or-uncomment-region-or-line)
         ("C-a"                   . beginning-of-line-or-indentation)
         ("C-x a r"               . align-regexp)
         ("s-u"                   . revert-buffer)
         ("C-c n"                 . cleanup-buffer)
         ("C-x F"                 . ido-find-file-other-window)
         ("C-x <right>"           . windmove-right)
         ("C-x <left>"            . windmove-left)
         ("C-x <up>"              . windmove-up)
         ("C-x <down>"            . windmove-down)
         ("C-c r"                 . rename-file-and-buffer)
         ("C-x C-r"               . sudo-edit)
         ("C-x 2"                 . vsplit-last-buffer)
         ("C-x 3"                 . hsplit-last-buffer)
         ("C-c d"                 . duplicate-current-line-or-region)
         ("C-c ;"                 . duplicate-and-comment-current-line-or-region)
         ("C-^"                   . top-join-line)
         ("C-c C-t x"             . TeX-toggle-escape)
         ("C-c s d"               . duck)
         ("C-c s l"               . lucky)
         ("C-c e"                 . eval-and-replace)
         ("C-S-<up>"              . change-number-at-point)
         ("C-S-<down>"            . subtract-number-at-point)
         ([f5]                    . kmacro-edit-macro)
         ([remap kill-whole-line] . smart-kill-whole-line)
         ([(shift return)]        . smart-open-line)
         ([remap goto-line]       . goto-line-with-feedback)))

;; (load "firacode") ;; Font hack to get ligatures
(load "core")

(load "init-exwm")
(load "osx")
;; (load "web-settings")
(load "nonsvn")
;; (load "xwidconf")
;; Load individual modules
;; (add-to-list 'load-path "~/.emacs.d/custom/modules")
;; (load "helm-settings")
;; (load "ivy-config")
;; (load "org-mode-config")
(provide 'init)
;;; init.el ends here
