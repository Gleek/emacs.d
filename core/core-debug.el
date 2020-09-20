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
  (dap-mode 1)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))



(provide 'core-debug)
;;; core-debug.el ends here
