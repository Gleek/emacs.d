;;; personal.el -- personal functions
;;
;;; Commentary:
;; Many Useful functions
;;; Code:
(defun comment-or-uncomment-region-or-line ()
  "Comment a line or region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))


(defun beginning-of-line-or-indentation ()
  "Move to beginning of line, or indentation."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))


(defun TeX-toggle-escape nil (interactive)
       "Toggle Shell Escape"
       (setq LaTeX-command
             (if (string= LaTeX-command "latex") "latex -shell-escape"
               "latex"))
       (message (concat "shell escape "
                        (if (string= LaTeX-command "latex -shell-escape")
                            "enabled"
                          "disabled"))
                ))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))


(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun file-owner-uid (filename)
  "Return the UID of the FILENAME as an integer.
See `file-attributes' for more info."
  (nth 2 (file-attributes filename 'integer)))

(defun file-owned-by-user-p (filename)
  "Return t if file FILENAME is owned by the currently logged in user."
  (equal (file-owner-uid filename)
         (user-uid)))

(defun find-alternate-file-as-root (filename)
  "Wraps `find-alternate-file' with opening a file as root."
  (find-alternate-file (concat "/sudo:root@localhost:" filename)))

(defun reopen-as-root ()
  "Find file as root if necessary."
  (unless (or (tramp-tramp-file-p buffer-file-name)
              (equal major-mode 'dired-mode)
              (not (file-exists-p (file-name-directory buffer-file-name)))
              (file-writable-p buffer-file-name)
              (file-owned-by-user-p buffer-file-name))
    (find-alternate-file-as-root buffer-file-name)))
;; (add-hook 'find-file-hook 'reopen-as-root)


(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

;;helm multi occur
;; (eval-after-load "helm-regexp"
;;   '(setq helm-source-moccur
;;          (helm-make-source "Moccur"
;;                            'helm-source-multi-occur :follow 1)))

;; (defun my-helm-multi-all ()
;;   "multi-occur in all buffers backed by files."
;;   (interactive)
;;   (helm-multi-occur
;;    (delq nil
;;          (mapcar (lambda (b)
;;                    (when (buffer-file-name b) (buffer-name b)))
;;                  (buffer-list)))))


(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))

(provide 'personal)
;;; personal.el ends here
