;;; keybindings.el --- Emacs keybindings.
;;
;;; Commentary:
;; Keybindings used in Emacs

;;; Code:
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)
(add-hook 'LaTeX-mode-hook
          (lambda nil
            (local-set-key (kbd "C-c C-t x") 'TeX-toggle-escape)))
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "M-z") 'zop-to-char)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-$")'mc/mark-more-like-this-extended)

(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-M-<up>") 'move-text-up)
(global-set-key (kbd "C-M-<down>") 'move-text-down)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(provide 'keybindings)
;;; keybindings.el ends here