(use-package tramp
  :ensure nil
  :init
  (setq tramp-default-method "ssh")
  (setq tramp-auto-save-directory (concat CACHE-DIR "tramp-autosave/"))
  (setq tramp-persistency-file-name (concat CACHE-DIR "tramp"))
  ;; (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
  :config
  (setq remote-file-name-inhibit-cache 60
        remote-file-name-inhibit-locks t
        remote-file-name-inhibit-auto-save-visited t
        tramp-copy-size-limit (* 1024 1024) ;; 1MB
        tramp-verbose 1
        vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp
                                     "[/\\\\]node_modules"))
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty))

;; (use-package tramp-container)
(provide 'core-tramp)
