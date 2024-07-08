(setq auto-save-list-file-prefix (concat CACHE-DIR "auto-save-list/.saves-")
      auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t)))
(use-package persistent-scratch
  :init
  :bind ("<f6>" . persistent-scratch-quick-open)
  :config
  (eval-after-load '+popup
    '(set-popup-rule! "\\^*scratch:" :vslot -4 :autosave t :size 0.35 :select t :quit nil :ttl nil :modeline t))
  (setq persistent-scratch-save-file (concat CACHE-DIR ".persistent-scratch"))
  ;; (persistent-scratch-restore)
  ;; (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode)
  (defun persistent-scratch-buffer-identifier()
    (string-match "^*scratch:" (buffer-name)))
  (defun persistent-scratch-get-scratches()
    (let ((scratch-buffers)
          (save-data
           (read
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8-unix))
                (insert-file-contents persistent-scratch-save-file))
              (buffer-string)))))
      (dolist (saved-buffer save-data)
        (push (substring (aref saved-buffer 0) (length "*scratch:")) scratch-buffers))
      scratch-buffers))

  (defun persistent-scratch-restore-this(&optional file)
    (interactive)
    (let ((current-buf (buffer-name (current-buffer)))
          (save-data
           (read
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8-unix))
                (insert-file-contents (or file persistent-scratch-save-file)))
              (buffer-string)))))
      (dolist (saved-buffer save-data)
        (when (string= current-buf (aref saved-buffer 0))
          (with-current-buffer (get-buffer-create (aref saved-buffer 0))
            (erase-buffer)
            (insert (aref saved-buffer 1))
            (funcall (or (aref saved-buffer 3) #'ignore))
            (let ((point-and-mark (aref saved-buffer 2)))
              (when point-and-mark
                (goto-char (car point-and-mark))
                (set-mark (cdr point-and-mark))))
            (let ((narrowing (aref saved-buffer 4)))
              (when narrowing
                (narrow-to-region (car narrowing) (cdr narrowing))))
            ;; Handle version 2 fields if present.
            (when (>= (length saved-buffer) 6)
              (unless (aref saved-buffer 5)
                (deactivate-mark))))))))

  (defun persistent-scratch-quick-open()
    (interactive)
    (let* ((scratch-buffers (persistent-scratch-get-scratches))
          (chosen-scratch (concat "*scratch:"
                                  (completing-read
                                   "Choose a scratch: "
                                   scratch-buffers nil nil nil nil
                                   (random-alnum 4))))
          (buffer-exists-p (get-buffer chosen-scratch)))
      (switch-to-buffer chosen-scratch)
      (unless buffer-exists-p
        (persistent-scratch-restore-this))
      (persistent-scratch-mode)))
  (setq persistent-scratch-scratch-buffer-p-function 'persistent-scratch-buffer-identifier))

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

(use-package saveplace
  :ensure nil
  :defer 1
  :config
  (setq save-place-file (concat CACHE-DIR "places"))
  (save-place-mode 1)
  (setq-default save-place t)
  ;; Add a fail safe in case of a crash
  (advice-add 'save-place-alist-to-file :around #'inhibit-message-a)
  (run-with-idle-timer 20 t 'save-place-alist-to-file))

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

(use-package multisession
  :ensure nil
  :init
  (setq multisession-directory (concat CACHE-DIR "multisession/")))

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
    (restart-emacs '("--restore")))
  :bind (("C-c R n"  . restart-emacs-start-new-emacs)
         ("C-c R r"  . restart-emacs)
         ("C-c R R"  . restart-and-restore)))

(setq confirm-kill-emacs 'yes-or-no-p)

(provide 'core-session)
