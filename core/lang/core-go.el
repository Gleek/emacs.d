(use-package go-mode
  :ensure go-mode
  :ensure go-eldoc
  :ensure go-guru
  :ensure gorepl-mode
  :ensure go-tag
  :ensure go-gen-test
  :ensure flycheck-golangci-lint
  :bind (:map go-mode-map
              ("C-c a" . go-tag-add))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook #'go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'flycheck-golangci-lint-setup)
  (setq go-tag-args (list "-transform" "camelcase")))

(provide 'core-go)
