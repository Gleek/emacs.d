(use-package shell-pop
  :ensure t
  :bind (("C-`" . +shellpop-eshell)
         ;; ("C-~" . +shellpop-vterm)
         ("C-c t v" . +shellpop-vterm))
  :config
  (defun shell-pop--cd-to-cwd-vterm(cwd)
    (interactive)
    (+vterm-run-command
     (concat "cd " (shell-quote-argument
                    (if (projectile-project-root cwd)
                        (projectile-project-root cwd)
                      cwd))))
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
    (let ((display-buffer-alist display-buffer-alist)
          ;; Have local copy of `display-buffer-alist' to make eshell take full window to avoid
          ;; flicker wrong window positioning on start
          (shell-pop-internal-mode-buffer "*eshell*")
          (shell-pop-internal-mode "eshell")
          (shell-pop-internal-mode-func '(lambda () (eshell)))
          (shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell)))))
      (push (cons "^\\*eshell" display-buffer--same-window-action) display-buffer-alist)
      (shell-pop nil)))

  (defun +shellpop-vterm()
    (interactive)
    (let ((shell-pop-internal-mode-buffer "*vterm*")
          (shell-pop-internal-mode "vterm")
          (shell-pop-internal-mode-func '(lambda () (vterm)))
          (shell-pop-shell-type '("vterm" "*vterm*" (lambda nil (vterm)))))
      (shell-pop nil)))

  (defun run-eat()
    (eat--1 (funcall eat-default-shell-function) nil 'display-buffer))

  (defun +shellpop-eat()
    ;; FIXME: Doesn't work
    (interactive)
    (let ((shell-pop-internal-mode-buffer "*eat*")
          (shell-pop-internal-mode "eat")
          (shell-pop-internal-mode-func '(lambda () (run-eat)))
          (shell-pop-shell-type '("eat" "*eat*" (lambda nil (run-eat)))))
      (shell-pop nil)))

  (setq shell-pop-window-position "bottom"
        shell-pop-window-size     45
        shell-pop-internal-mode   "eshell"
        shell-pop-internal-mode-buffer "*eshell*"
        shell-pop-internal-mode-func '(lambda () (eshell))
        shell-pop-full-span t
        shell-pop-autocd-to-working-dir t
        shell-pop-restore-window-configuration t
        shell-pop-cleanup-buffer-at-process-exit t
        shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell)))))

(use-package shrink-path)

(use-package bash-completion
  :after eshell
  (bash-completion-setup))


(use-package eat
  :after (eshell)
  :demand t
  :bind (("C-~" . eat-project-or-default)
         (:map eat-mode-map
               (("C-`" . bury-buffer)
                ("C-~" . bury-buffer))))
  :config
  (defun eat-project-or-default()
    (interactive)
    (if (project-current)
        (eat-project)
      (eat)))
  (setq eat-kill-buffer-on-exit t)
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  (set-popup-rule! "^\\*.*eat\\*" :size 0.4 :quit nil :select t))

(use-package eshell
  :init
  (defvar eshell-hist-mode-map (make-sparse-keymap))
  :ensure nil
  :bind ((:map eshell-hist-mode-map ("M-r" . consult-history)))
  :config
  (set-popup-rule! "^\\*eshell" :ignore t)
  (setq eshell-banner-message ""
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-history-size 10000
        eshell-hist-ignoredups t
        ;; don't record command in history if prefixed with whitespace
        eshell-input-filter 'eshell-input-filter-initial-space
        ;; em-prompt
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-default-prompt-fn
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)


  (defface +eshell-prompt-pwd '((t (:inherit font-lock-constant-face)))
    "TODO"
    :group 'eshell)
  (defun +eshell-default-prompt-fn ()
    "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
    (require 'shrink-path)
    (concat (let ((pwd (eshell/pwd)))
              (propertize (if (equal pwd "~")
                              pwd
                            (abbreviate-file-name (shrink-path-file pwd)))
                          'face '+eshell-prompt-pwd))
            (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
            " "))

  ;; Courtesy: Doom
  (defun +eshell/search-history ()
    "Search the eshell command history with helm, ivy or `eshell-list-history'."
    (interactive)
    (require 'em-hist)
    (let* ((ivy-completion-beg (eshell-bol))
           (ivy-completion-end (point-at-eol))
           (input (buffer-substring-no-properties
                   ivy-completion-beg
                   ivy-completion-end)))
      ;; Better than `counsel-esh-history' because that doesn't
      ;; pre-populate the initial input or selection.
      (ivy-read "Command: "
                (delete-dups
                 (when (> (ring-size eshell-history-ring) 0)
                   (ring-elements eshell-history-ring)))
                :initial-input input
                :action #'ivy-completion-in-region-action)))

  (defun eshell/cdp()
    (interactive)
    (eshell/cd (projectile-project-root)))
  (setq eshell-directory-name (concat CACHE-DIR "eshell/"))
  (setq eshell-aliases-file (expand-file-name "eshell-aliases" user-emacs-directory))
  (setq eshell-banner-message "")
  (setq eshell-history-file-name (concat CACHE-DIR "eshell/history"))
  (setq eshell-last-dir-ring-file-name (concat CACHE-DIR "eshell/lastdir")))

(use-package eshell-up
  :commands eshell-up eshell-up-peek)

(use-package eshell-vterm
  :disabled t
  :after eshell
  :hook (eshell-mode . eshell-vterm-mode)
  :config
  (defalias 'eshell/v 'eshell-exec-visual))

(use-package esh-help
  :hook (eshell-mode . +esh-load-helpdoc)
  :config
  (defvar +esh-help-loaded nil)
  (defun +esh-load-helpdoc()
    (if (not +esh-help-loaded)
        (setup-esh-help-eldoc))
    (setq +esh-help-loaded t)))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package vterm
  :preface (setq vterm-install t)
  :hook (vterm-mode . hide-mode-line-mode)
  :bind (:map vterm-mode-map
              ("C-c C-a" . +vterm-screen-session)
              ("C-c C-d" . +vterm-cd-root-or-current))
  :config
  (defun +vterm-cd-default-directory()
    (interactive)
    (+vterm-run-command (concat "cd " default-directory)))
  (defun +vterm-cd-root-or-current(&optional prefix)
    (interactive "P")
    (let (jump-directory)
      (if (and (not prefix) (projectile-project-root default-directory))
          (setq jump-directory (projectile-project-root default-directory))
        (setq jump-directory default-directory))
      (+vterm-run-command (concat "cd " jump-directory))))
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


(use-package emacs :ensure nil
  :config
  (defun launch-kitty ()
    "Launch Kitty terminal in the current default directory or project root."
    (interactive)
    (let* ((default-directory
            (or (when (fboundp 'projectile-project-root)
                  (projectile-project-root))
                (when (fboundp 'project-root)
                  (project-root))
                default-directory))
           (kitty-command (concat "kitty --directory " (shell-quote-argument default-directory))))
      (start-process "kitty" "*kitty*" "sh" "-c" kitty-command))))

(use-package detached
  :init
  (setq detached-db-directory CACHE-DIR
        detached-session-directory (concat CACHE-DIR "sessions"))
  ;; (detached-init)
  :bind (("C-c x x" . detached-shell-command)
         ("C-c x t" . detached-tail-session)
         ("C-c x o" . detached-consult-session)
         ;; ("C-c x k" . +detached-kill-and-delete-session)
         ("C-c x K" . detached-delete-sessions))
  :config

  (if IS-MAC
      (setq detached-terminal-data-command "script -F -q /dev/null %s"))
  (setq detached-notification-function 'detached-state-transitionion-echo-message)

  (defun +quick-sshuttle()
    (interactive)
    (setq sshuttle-endpoints (secret-get sshuttle-endpoints))
    (let ((command (cdr (assoc-string (completing-read "Server" sshuttle-endpoints) sshuttle-endpoints))))
      (dtache-start-session (concat "sshuttle -l 0.0.0.0 -r " command) nil))))



(provide 'core-shell)
