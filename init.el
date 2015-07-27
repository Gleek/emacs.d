;;; init.el -- Prelude's configuration entry point.
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

;;; init.el ends here
