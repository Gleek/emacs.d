(use-package js2-mode
  ;; :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-basic-offset 2
        js-chain-indent t
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-highlight-external-variables t
        js2-idle-timer-delay 0.1)
  ;; (load "lsp-javascript")
  ;; (add-hook 'js2-mode-hook (lambda ()
  ;;                            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  :bind (:map js2-mode-map ("M-." . nil))
  :ensure t)


(use-package json-ts-mode
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode)))
(use-package jq-mode
  :after (json-ts-mode)
  :bind (:map json-ts-mode-map ("C-z s" . jq-interactively))
  :config
  (setq jq-interactive-font-lock-mode 'json-ts-mode)
  (setq jq-interactive-delay 0))

;; (use-package rjsx-mode :mode ("\\.jsx\\'" . rjsx-mode) )

(use-package js
  :ensure nil
  :mode ("\\.jsx?\\'" . js-ts-mode)
  :bind (:map js-ts-mode-map ("M-." . nil))
  :config
  (setq js-indent-level 2)
  (add-hook 'js-ts-mode-hook (lambda() (setq-local format-all-formatters '(("Javascript" prettier))))))

(use-package typescript-ts-mode
  :ensure nil
  :mode ("\\.ts\\'" . typescript-ts-mode))

(provide 'core-javascript)
