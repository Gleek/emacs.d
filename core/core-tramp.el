(use-package tramp
  :ensure nil
  :init
  (setq tramp-default-method "ssh")
  (setq tramp-auto-save-directory (concat CACHE-DIR "tramp-autosave/"))
  (setq tramp-persistency-file-name (concat CACHE-DIR "tramp"))
  ;; (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
  :config
  (setq remote-file-name-inhibit-cache 60
        tramp-verbose 1
        vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp
                                     "[/\\\\]node_modules"))
  (add-to-list 'tramp-default-proxies-alist
               '("jobs" nil "/ssh:entry:"))
  (add-to-list 'tramp-default-proxies-alist
               '("scripts" nil "/ssh:entry:"))
  (add-to-list 'tramp-default-proxies-alist
               '("aa102" nil "/ssh:entry:")))

(use-package docker-tramp)
(provide 'core-tramp)
