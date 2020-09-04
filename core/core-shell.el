(use-package term
  :ensure shell-toggle
  :init
  (setq-default bidi-display-reordering nil)
  ;; (defun term-handle-more-ansi-escapes (proc char)
  ;;   "Handle additional ansi escapes."
  ;;   (cond
  ;;    ;; \E[nG - Cursor Horizontal Absolute, e.g. move cursor to column n
  ;;    ((eq char ?G)
  ;;     (let ((col (min term-width (max 0 term-terminal-parameter))))
  ;;       (term-move-columns (- col (term-current-column)))))
  ;;    (t)))
  ;; (advice-remove 'term-handle-ansi-escape :before #'term-handle-more-ansi-escapes)
  :config
  (yas-minor-mode -1))



(use-package shell-pop
  :ensure t
  :bind ("C-`" . shell-pop)
  :config
  ;; (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

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
  (setq eshell-directory-name (concat CACHE-DIR "eshell/"))
  (setq eshell-banner-message "")
  (setq eshell-history-file-name (concat CACHE-DIR "eshell/history"))
  (setq eshell-last-dir-ring-file-name (concat CACHE-DIR "eshell/lastdir")))

(provide 'core-shell)
