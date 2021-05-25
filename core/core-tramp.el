(use-package tramp
  :ensure nil
  :init
  (setq tramp-default-method "ssh")
  (setq tramp-auto-save-directory (concat CACHE-DIR "tramp-autosave/"))
  (setq tramp-persistency-file-name (concat CACHE-DIR "tramp"))
  ;; (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
  :config
  (add-to-list 'tramp-default-proxies-alist
               '("jobs" nil "/ssh:entry:"))
  (add-to-list 'tramp-default-proxies-alist
               '("scripts" nil "/ssh:entry:"))
  (add-to-list 'tramp-default-proxies-alist
               '("aa102" nil "/ssh:entry:")))

(use-package docker-tramp)
(provide 'core-tramp)
