;;; core-debug -- Debugger packages and config
;;; Code:

(use-package dap-mode
  :hook (dap-mode . dap-tooltip-mode)
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode)
  :bind ("C-c p d" . dap-hydra)
  :init
  (setq dap-breakpoints-file (concat CACHE-DIR "dap-breakpoints")
        dap-utils-extension-path (concat CACHE-DIR "dap-extension"))
  :config
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))


(use-package dape
  :config
  (setq dape-cwd-fn 'projectile-project-root))


(use-package emacs
  :ensure nil
  :bind ("C-c t D" . toggle-debug-on-error))


(provide 'core-debug)
;;; core-debug.el ends here
