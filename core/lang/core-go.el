;;  go install golang.org/x/tools/gopls@latest
;;  go install github.com/fatih/gomodifytags@latest
;;  go install github.com/go-delve/delve/cmd/dlv@latest
;;  go install github.com/rogpeppe/godef@latest
;;  go install github.com/motemen/gore/cmd/gore@latest
;;  DEPRECATED: go get -u github.com/stamblerre/gocode
;;  go install golang.org/x/tools/cmd/godoc@latest
;;  go install golang.org/x/tools/cmd/goimports@latest
;;  go install golang.org/x/tools/cmd/gorename@latest
;;  DEPRECATED: go get -u golang.org/x/tools/cmd/guru
;;
;;
;; https://golangci-lint.run/usage/install/#local-installation
;; brew install golangci-lint
;; curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.31.0

;;; Code:

;; Disabling go-ts-mode because of inferior highlighting and indentation. To revisit again later.
;; (setq auto-mode-alist (delete '("\\.go\\'" . go-ts-mode) auto-mode-alist))

(push '(go-dot-mod-mode . go-mod-ts-mode) major-mode-remap-alist)
;; (push '(go-mode . go-ts-mode) major-mode-remap-alist)

(defun +gofmt-before-save()
  (interactive)
  (require 'go-mode)
  (when (or (eq major-mode 'go-mode)
            (eq major-mode 'go-ts-mode))
    (gofmt)))


(use-package go-tag
  :commands (+go-tag-add)
  :config
  (defun +go-tag-add(arg)
    (interactive "P")
    (if (eq arg nil)
        (let ((go-tag-args '("-transform" "camelcase")))
          (call-interactively 'go-tag-add))
      (let ((go-tag-args nil))
        (call-interactively 'go-tag-add)))))




(use-package flycheck-golangci-lint
  :hook ((go-mode go-ts-mode) . +go-setup-checkers)
  :config
  (setq flycheck-golangci-lint-enable-linters
        '("bodyclose" "depguard" "dupl" "errcheck" "exhaustive" "funlen" "gochecknoinits" "goconst"
          "gocritic" "gocognit" "gofmt" "goimports" "revive" "gosec" "gosimple" "govet" "ineffassign" "lll"
          "misspell" "noctx" "rowserrcheck" "staticcheck" "typecheck" "unparam" "unused"
          "whitespace" "gomodguard" "sqlclosecheck" "errcheck"))

  (defun +flycheck-parse-checkstyle-warnings-only (output checker buffer)
    "Parse Checkstyle errors from OUTPUT and return only warnings.

    This function converts all errors to warnings for the specified CHECKER (golangci-lint).
    See `flycheck-parse-checkstyle' for more details on the parameters"
    (let ((original-errors (flycheck-parse-checkstyle output checker buffer)))
      (mapcar (lambda (error)
                (when (eq (flycheck-error-level error) 'error)
                  (setf (flycheck-error-level error) 'warning))
                error)
              original-errors)))

  (defun +go-setup-checkers()
    (flycheck-golangci-lint-setup)

    ;; Demote golangci errors to info
    (dolist (patt (get 'golangci-lint 'flycheck-error-patterns))
      (setcdr patt 'warning))

    (setf (flycheck-checker-get 'golangci-lint 'error-parser)
          #'+flycheck-parse-checkstyle-warnings-only)

    (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :bind (:map go-ts-mode-map
              ("C-z a" . +go-tag-add))
  :config
  (setq-default go-ts-mode-indent-offset tab-width))

(use-package go-mode
  :ensure go-gen-test
  :ensure flycheck-golangci-lint
  :bind (:map go-mode-map
              ("C-z a" . +go-tag-add)
              ("C-c C-d" . nil))
  :config
  ;; (setq dap-go-debug-program `("node" ,(concat dap-go-debug-path "/extension/dist/debugAdapter.js")))
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook '+gofmt-before-save)
  (set-popup-rule! "^\\*go-guru-output\\*" :size 0.4 :quit t))


(use-package gorepl-mode
  :hook ((go-mode go-ts-mode) . gorepl-mode)
  :bind (:map gorepl-mode-map
              ("C-c C-g" . nil)))

(use-package go-playground
  :init
  (setq go-playground-basedir (expand-file-name "src/example.com/playground/" (getenv "GOPATH"))))

(provide 'core-go)
