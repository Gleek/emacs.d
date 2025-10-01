;;; core-python.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Umar Ahmad
;; Created: October 02, 2025
;; Modified: October 02, 2025
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; 

;;; Code:

(use-package pyvenv)
(use-package pipenv)
(use-package pip-requirements)
(use-package pyimport)
(use-package py-isort)
(use-package uv-mode
  :hook (python-ts-mode))

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright")
  :hook ((python-mode python-ts-mode) . +init-lsp-pyright)
  :config
  (defun +init-lsp-pyright()
    (require 'lsp)
    (require 'lsp-pyright)
    (lsp-deferred)))

(provide 'core-python)
;;; core-python.el ends here
