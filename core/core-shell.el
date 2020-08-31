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
  :init
  (setq shell-pop-window-position "bottom"
        shell-pop-window-size     30
        shell-pop-term-shell      "/bin/zsh"
        shell-pop-internal-mode   "ansi-term"))

(use-package eshell
  ;; :ensure eshell-git-prompt
  ;;   https://github.com/ekaschalk/dotspacemacs/blob/master/.spacemacs
  ;; :init (eshell-git-prompt-use-theme 'robbyrussell)
  :config
  (setq eshell-history-file-name (concat CACHE-DIR "eshell/history"))
  (setq eshell-last-dir-ring-file-name (concat CACHE-DIR "eshell/lastdir")))

(provide 'core-shell)
