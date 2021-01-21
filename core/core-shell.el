(use-package shell-pop
  :ensure t
  :bind (("C-`" . +shellpop-eshell)
         ("C-~" . +shellpop-vterm)
         ("C-c t v" . +shellpop-vterm))
  :config

  (defun shell-pop--cd-to-cwd-vterm(cwd)
    (interactive)
    (+vterm-run-command (concat "cd " (shell-quote-argument cwd)))
    (setq default-directory cwd))

  ;; shell-pop--cd-to-cwd
  (defun +shell-pop-cd(o &rest args)
    (let ((abspath (expand-file-name (car args))))
      (cond ((string= shell-pop-internal-mode "vterm")
             (shell-pop--cd-to-cwd-vterm abspath))
            (t (apply o args)))))
  (advice-add 'shell-pop--cd-to-cwd :around '+shell-pop-cd)

  (defun +shellpop-eshell()
    (interactive)
    (let ((shell-pop-internal-mode-buffer "*eshell*")
          (shell-pop-internal-mode "eshell")
          (shell-pop-internal-mode-func '(lambda () (eshell)))
          (shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell)))))
      (shell-pop nil)))

  (defun +shellpop-vterm()
    (interactive)
    (let ((shell-pop-internal-mode-buffer "*vterm*")
          (shell-pop-internal-mode "vterm")
          (shell-pop-internal-mode-func '(lambda () (vterm)))
          (shell-pop-shell-type '("vterm" "*vterm*" (lambda nil (vterm)))))
      (shell-pop nil)))

  (setq shell-pop-window-position "bottom"
        shell-pop-window-size     40
        shell-pop-internal-mode   "eshell"
        shell-pop-internal-mode-buffer "*eshell*"
        shell-pop-internal-mode-func '(lambda () (eshell))
        shell-pop-full-span t
        shell-pop-autocd-to-working-dir t
        shell-pop-restore-window-configuration t
        shell-pop-cleanup-buffer-at-process-exit t
        shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell)))))

(use-package eshell
  :ensure nil
  ;; :ensure eshell-git-prompt
  ;;   https://github.com/ekaschalk/dotspacemacs/blob/master/.spacemacs
  ;; :init (eshell-git-prompt-use-theme 'robbyrussell)
  :config
  ;; (setq eshell-banner-message
  ;;       '(format "%s %s\n"
  ;;                (propertize (format " %s " (string-trim (buffer-name)))
  ;;                            'face 'mode-line-highlight)
  ;;                (propertize (current-time-string)
  ;;                            'face 'font-lock-keyword-face)))
  (defun cdp (&rest args)
    (apply #'cd (projectile-project-root) args))
  (setq eshell-directory-name (concat CACHE-DIR "eshell/"))
  (setq eshell-banner-message "")
  (setq eshell-history-file-name (concat CACHE-DIR "eshell/history"))
  (setq eshell-last-dir-ring-file-name (concat CACHE-DIR "eshell/lastdir")))

(use-package vterm
  :preface (setq vterm-install t)
  :hook (vterm-mode . hide-mode-line-mode)
  :bind (:map vterm-mode-map
              ("C-c C-a" . +vterm-screen-session))
  :config
  (defun +vterm-run-command(command)
    (vterm-send-C-S-a)
    (vterm-send-string (concat " " command " #") t)
    (vterm-send-return))
  (defun +vterm-screen-session()
    "Start vterm in default session screen"
    (interactive)
    (+vterm-run-command "screen -dR \"session\""))
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  ;; (add-hook 'vterm-mode-hook (lambda() (+vterm-screen-session)))
  (add-hook 'vterm-mode-hook
            (lambda()
              (setq confirm-kill-processes nil))))

(provide 'core-shell)
