(use-package protobuf-mode
  :bind (:map protobuf-mode-map
              ("C-c C-d" . nil)
              ("C-c a n" . +proto-renumber))
  :config
  ;; For some reason protobuf is not running prog-mode-hook
  (add-hook 'protobuf-mode-hook (lambda()
                                  (run-hooks 'prog-mode-hook)))
  (setq flycheck-protoc-import-path
        `(,(concat (expand-file-name "~/Development/") (rot13 "mbzngb/mbzngb-rirag-ertvfgel/"))))
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))

  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t)))
  (add-hook 'protobuf-mode-hook
            (function (lambda ()
                        (setq tab-width 2))))

  (defun +proto-buf-set-lint-root ()
    "Point `flycheck-buf-lint-root' at the buf workspace/module root.
buf refuses to lint a file via its own directory when that directory
is inside a module (e.g. a v1 workspace with `buf.work.yaml'); it must
run from the workspace root and filter with `--path'.  Prefer the
`buf.work.yaml' directory, falling back to the top-level `buf.yaml',
then to the projectile project root."
    (when buffer-file-name
      (let ((root (or (locate-dominating-file buffer-file-name "buf.work.yaml")
                      (locate-dominating-file buffer-file-name "buf.yaml")
                      (and (fboundp 'projectile-project-root)
                           (projectile-project-root)))))
        (when root
          (setq-local flycheck-buf-lint-root (expand-file-name root))))))
  (add-hook 'protobuf-mode-hook #'+proto-buf-set-lint-root)

  (with-eval-after-load 'flycheck
    (flycheck-buf-setup))

  (defun +proto-renumber()
    "Reunumber protos in these steps
    - get current proto out in a separate buffer
    - for every line ignore if its a comment or an empty line
    - if there's a { somewhere jump to the end
    - Goto the point by
        jumping to = if present
        if = not present jump to end of line add = and type in current number and ;
        if = present then delete the line after = and add current number and ;"
    (interactive)
    (require 'subr-x)
    (save-excursion
      (c-beginning-of-defun)
      (let ((proto-message-type (thing-at-point 'word t)))
      (c-end-of-defun)
      (backward-char 1)
      (let ((rbeg (point)))
        (backward-list)
        (let ((text (buffer-substring-no-properties rbeg (point)))
              (current-num (if (string= proto-message-type "message") 1 0))
              (final-string "")
              (line-string ""))
          (with-temp-buffer
            (insert text)
            (goto-char (point-min))
            (delete-region (line-beginning-position) (line-end-position))
            (delete-char 1)
            (goto-char (point-max))
            (delete-region (line-beginning-position) (line-end-position))
            (delete-char -1)
            (goto-char (point-min))
            (while (not (eobp))
              (setq line-string (string-trim (thing-at-point 'line t)))
              (unless (or (string-prefix-p "//" line-string)
                          (= (length line-string) 0))
                (if (string-match-p (regexp-quote "{") line-string)
                    (forward-list)
                  (if (string-match-p (regexp-quote "=") line-string)
                      (progn (search-forward "=")
                             (delete-region (point) (line-end-position)))
                    (end-of-line)
                    (insert " ="))
                  (insert (format " %d;" current-num))
                  (setq current-num (1+ current-num))))
              (forward-line 1))
            (setq final-string (buffer-string)))
          (let ((beg (point)))
            (forward-list)
            (forward-line -1)
            (goto-char (line-end-position))
            (delete-region (+ beg 2) (point))
            (insert final-string))))))))

(use-package flycheck-buf
  :ensure (:host github :repo "gleek/flycheck-buf"))
