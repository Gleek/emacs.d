(setq bookmark-file (concat CACHE-DIR "bookmarks"))
(setq auto-save-list-file-prefix (concat CACHE-DIR "auto-save-list/.saves-")
      auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t)))
(use-package persistent-scratch
  :init
  :defer 1
  :config
  (setq persistent-scratch-save-file (concat CACHE-DIR ".persistent-scratch"))
  (persistent-scratch-restore)
  (persistent-scratch-setup-default))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-save-file (concat CACHE-DIR "recentf"))
  (setq recentf-filename-handlers
        '(substring-no-properties    ; strip out lingering text properties
          abbreviate-file-name)))

(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start))
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

;; (use-package key-chord
;;   :defer 1
;;   :config
;;   (key-chord-mode 1)
;;   (setq key-chord-two-keys-delay .015
;;         key-chord-one-key-delay .040)
;;   (key-chord-define-global "df" 'isearch-forward)
;;   (key-chord-define-global "jk" 'avy-goto-word-or-subword-1)
;;   (key-chord-define-global "nm" 'switch-to-previous-buffer))

(use-package saveplace
  :ensure nil
  :defer 1
  :config
  (setq save-place-file (concat CACHE-DIR "places"))
  (save-place-mode 1)
  (setq-default save-place t))

(use-package savehist
  :ensure nil
  :init (setq savehist-additional-variables
              ;; search entries
              '(search-ring regexp-search-ring)
              ;; save every minute
              savehist-autosave-interval 60
              ;; keep the home clean
              savehist-file (concat () CACHE-DIR "savehist" ))
  :config
  (savehist-mode 1))

(use-package desktop
  :ensure nil
  :init
  (add-to-list 'command-switch-alist (cons "--restore" #'restore-from-desktop))
  (setq desktop-dirname CACHE-DIR)
  (defun quick-desktop-save()
    (interactive)
    (let ((desktop-file-modtime (nth 5 (file-attributes (concat desktop-dirname ".emacs.desktop")))))
      (desktop-save desktop-dirname t)))
  (defun restore-from-desktop(&rest _)
    (interactive)
    (desktop-read desktop-dirname))
  (add-hook 'kill-emacs-hook #'quick-desktop-save))


(use-package restart-emacs
  :init
  (defun restart-and-restore()
    (interactive)
    (quick-desktop-save)
    (restart-emacs '("--restore"))))

(setq confirm-kill-emacs 'yes-or-no-p)

(provide 'core-session)
