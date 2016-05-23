;;; init.el -- Emacs configuration entry point.
;;; Commentary:
;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs.

;;; Code:
(require 'package)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/custom/")
(load "ui")
(load "personal")
(load "core")
(load "osx")
(load "web-settings")
(load "keybindings")
;; Load individual modules
(add-to-list 'load-path "~/.emacs.d/custom/modules")
(load "helm-settings")
(load "org-wunderlist-settings")
(load "nonsvn")
(load "org-mode-config")
(load "company-settings")
(load "god-mode-config")
;;; init.el ends here
