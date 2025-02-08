;;; core-org.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Umar Ahmad
;; Created: February 08, 2025
;; Modified: February 08, 2025
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; Main file that loads all org-files

;;; Code:
(defvar +org-directory "~/Dropbox/org-files/")
(use-package org)
(use-package "org/core-org-tools.el" :demand t :ensure nil)
(use-package "org/core-org-ui.el" :demand t :ensure nil)
(use-package "org/core-gtd.el" :demand t :ensure nil)
(use-package "org/core-org-notes.el" :demand t :ensure nil)

(provide 'core-org)
;;; core-org.el ends here
