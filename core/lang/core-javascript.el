(use-package js-mode
  :ensure nil
  :init (setq js-indent-level 2))
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
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


(use-package rjsx-mode :mode ("\\.jsx\\'" . rjsx-mode) )
;; (use-package tern                       ; Javascript IDE backend
;;   :disabled t
;;   :ensure t
;;   :init
;;   (add-hook 'js2-mode-hook #'tern-mode)
;;   :config
;;   ;; Don't generate port files
;;   (add-to-list 'tern-command "--no-port-file" 'append)
;;   :bind (:map tern-mode-keymap
;;               ("M-." . nil)
;;               ("M-," . nil))
;;   :diminish "🕊")

;; (use-package company-tern               ; Auto-completion for javascript
;;   :ensure t
;;   :after company
;;   :config (add-to-list 'company-backends 'company-tern))

;; (use-package js-doc :ensure t)
;; (use-package jsx-mode )
;; (use-package lsp-javascript-typescript
;;   :defer 1
;;   :hook ((js-mode . lsp-javascript-typescript-enable)
;;          (js2-mode . lsp-javascript-typescript-enable)
;;          (typescript-mode . lsp-javascript-typescript-enable)
;;          (js3-mode . lsp-javascript-typescript-enable)
;;          (rjsx-mode . lsp-javascript-typescript-enable)))


;; (use-package indium
;;   :init
;;   (setq indium-chrome-data-dir (concat CACHE-DIR "indium-chrome-profile"))
;;   :config
;;   (setq indium-chrome-executable "google-chrome"))

(provide 'core-javascript)
