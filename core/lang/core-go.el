;;  go get -u golang.org/x/tools/gopls
;;  go get -u github.com/fatih/gomodifytags
;;
;;  go get -v github.com/rogpeppe/godef
;;  go get -u github.com/motemen/gore/cmd/gore
;;  DEPRECATED: go get -u github.com/stamblerre/gocode
;;  go get -u golang.org/x/tools/cmd/godoc
;;  go get -u golang.org/x/tools/cmd/goimports
;;  go get -u golang.org/x/tools/cmd/gorename
;;  DEPRECATED: go get -u golang.org/x/tools/cmd/guru
;;
;;
;; https://golangci-lint.run/usage/install/#local-installation
;; brew install golangci/tap/golangci-lint
;; curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.31.0


(use-package go-mode
  :ensure go-mode
  :ensure gorepl-mode
  :ensure go-tag
  :ensure go-gen-test
  :ensure flycheck-golangci-lint
  :bind (:map go-mode-map
              ("C-c a" . go-tag-add)
              ("C-c C-d" . nil))
  :config
  (setq gofmt-command "goimports")
  ;; (add-hook 'go-mode-hook #'go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'flycheck-golangci-lint-setup)
  (setq go-tag-args (list "-transform" "camelcase")))

(provide 'core-go)
