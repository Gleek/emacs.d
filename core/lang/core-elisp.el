;;;###autoload
(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;;;###autoload
(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(use-package emacs :ensure nil
  :bind (("C-c C-e" . eval-and-replace)
         ("C-c e" . eval-and-replace))
  :config
  (add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save))

(use-package eros
  :hook (emacs-lisp-mode . eros-mode))

(provide 'core-elisp)
