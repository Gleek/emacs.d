(defvar +quick-switch-buffer-hook nil)
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1))
  (run-hook-with-args-until-success '+quick-switch-buffer-hook))

(use-package projectile
  :ensure projectile
  :init
  (setq projectile-cache-file (concat CACHE-DIR "projectile.cache"))
  (setq projectile-known-projects-file (concat CACHE-DIR "projectile-bookmarks.eld"))
  (defvar projectile-mode-line)
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-enable-caching t) ;; Projectile turbo!!
  :config
  (defun +copy-project-file-name()
    (interactive)
    (let ((filename (file-relative-name buffer-file-name (projectile-project-root))))
      (when filename
        (kill-new filename)
        (message "Copied project file name '%s' to the clipboard." filename))))
  (projectile-load-known-projects)
  (setq projectile-mode-line-prefix "")
  (projectile-mode 1)
  :bind (("C-c p z" . counsel-fzf)
         ("C-c p w" . +copy-project-file-name)
         ("C-c p k" . projectile-kill-buffers)
         ("C-c p s" . projectile-save-project-buffers)))

(use-package counsel-projectile
  :bind (("M-p" . counsel-projectile)
         ("C-c p f" . counsel-projectile-find-file)
         ("C-c p b" . counsel-projectile-switch-to-buffer)
         ("C-x B" . counsel-projectile-switch-to-buffer)
         ("C-c p p" . counsel-projectile-switch-project))
  :config
  (defvar +ivy-project-sort-min-length 1)
  (defvar +ivy-project-sort-max-candidates 200)
  (defun +ivy-project-sort--exact-match-file-base-name(name x y)
    (cond ((string= (file-name-nondirectory x) name) 1)
          ((string= (file-name-nondirectory y) name) 2)
          (t nil)))

  (defun +ivy-project-sort--exact-match-root-name(name x y)
    (cond ((string= (file-name-base x) name) 1)
          ((string= (file-name-base y) name) 2)
          (t nil)))

  (defun +ivy-project-sort--prefix-match-file-base-name(name x y)
    (cond ((string-match-p (concat "\\`" (funcall ivy--regex-function name)) (file-name-nondirectory x)) 1)
          ((string-match-p (concat "\\`" (funcall ivy--regex-function name)) (file-name-nondirectory y)) 2)
          (t nil)))

  (defun +ivy-project-sort--match-file-base-name(name x y)
    (cond ((string-match-p (regexp-quote name) (file-name-nondirectory x)) 1)
          ((string-match-p (regexp-quote name) (file-name-nondirectory y)) 2)
          (t nil)))

  (defun +ivy-project-sort-files(name candidates)
    "Assumes all candidates already match name"
    (if (and (>= (length name) +ivy-project-sort-min-length)
             (<= (length candidates) +ivy-project-sort-max-candidates))
        (cl-sort (copy-sequence candidates)
                 (lambda (x y)
                   (if (eq (or (+ivy-project-sort--exact-match-file-base-name name x y)
                               (+ivy-project-sort--exact-match-root-name name x y)
                               (+ivy-project-sort--prefix-match-file-base-name name x y)
                               (+ivy-project-sort--match-file-base-name name x y)) 2) nil t)))
      candidates))
  (defun +counsel-projectile-find-file-matcher(regexp candidates)
    (+ivy-project-sort-files regexp (counsel--find-file-matcher regexp candidates)))
  (setq counsel-projectile-find-file-matcher '+counsel-projectile-find-file-matcher))


(use-package project
  :config
  (setq project-list-file (concat CACHE-DIR "projects")))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))
(use-package ibuffer-projectile
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))


(use-package ivy-xref
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ecb :disabled t )

(use-package ag :ensure t )

(use-package dumb-jump
  :ensure t
  ;; :bind (("M-g o" . dumb-jump-go-other-window)
  ;;        ("M-g j" . dumb-jump-go)
  ;;        ("M-g i" . dumb-jump-go-prompt)
  ;;        ("M-g h" . dumb-jump-back)
  ;;        ("M-g q" . dumb-jump-quick-look)
  ;;        ("M-g x" . dumb-jump-go-prefer-external)
  ;;        ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-rg-cmd "rg")
  (setq dumb-jump-max-find-time 5)
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-selector 'ivy))

(use-package rg)

(use-package wgrep
  :config (setq wgrep-auto-save-buffer t))

(use-package smart-jump
  :ensure t
  :defer 1
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-?" . smart-jump-references))
  :config (smart-jump-setup-default-registers)
  (smart-jump-register :modes 'php-mode
                       :jump-fn 'xref-find-definitions
                       :pop-fn 'xref-pop-marker-stack
                       :refs-fn 'xref-find-references
                       :should-jump t
                       :heuristic 'point
                       :async 500
                       :order 1)
  (setq smart-jump-find-references-fallback-function 'smart-jump-find-references-with-rg)
  (smart-jump-register :modes 'go-mode))


(use-package treemacs
  :bind (("C-c t t" . +treemacs-toggle)
         ("C-c p t" . +show-treemacs))
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (concat CACHE-DIR "treemacs-persist")
        treemacs-last-error-persist-file (concat CACHE-DIR "treemacs-last-error-persist"))
  :config
  (defun +show-treemacs()
    (interactive)
    (if (and (projectile-project-root)
             t)
        ;; Modified treemacs-add-and-display-current-project to focus on
        ;; the current file instead of project root
        (treemacs-block
         (treemacs-unless-let (root (treemacs--find-current-user-project))
             (treemacs-error-return-if (null root)
               "Not in a project.")
           (let* ((path (treemacs--canonical-path root))
                  (name (treemacs--filename path)))
             (unless (treemacs-current-workspace)
               (treemacs--find-workspace))
             (if (treemacs-workspace->is-empty?)
                 (progn
                   (treemacs-do-add-project-to-workspace path name)
                   (treemacs-select-window)
                   (treemacs-pulse-on-success))
               (treemacs-select-window)
               (if (treemacs-is-path path :in-workspace)
                   (treemacs-select-window)
                 (treemacs-add-project-to-workspace path name))))))
      (treemacs)))
  (defun +treemacs-toggle ()
    "Initialize or toggle treemacs.
Ensures that only the current project is present and all other projects have
been removed.
Use `treemacs' command for old functionality."
    (interactive)
    (pcase (treemacs-current-visibility)
      (`visible (delete-window (treemacs-get-local-window)))
      (_ (+show-treemacs))))
  (treemacs-follow-mode -1)
  (setq treemacs-width 30))

(use-package treemacs-projectile
  :ensure t)

(use-package lsp-treemacs
  :hook (treemacs-select-hook . lsp-treemacs-sync-mode)
  :bind (:map lsp-mode-map ("C-c p e" . lsp-treemacs-errors-list)))

(use-package lsp-mode
  :hook ((js-mode js2-mode js3-mode rjsx-mode go-mode rust-mode php-mode) . lsp)
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-session-file (concat CACHE-DIR ".lsp-session-v1"))
  :config
  (setq lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  (setq lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil))

(use-package lsp-ivy
  :bind (:map lsp-mode-map ("M-<return>" . lsp-ivy-workspace-symbol)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :ensure t
  :config

  (setq lsp-prefer-flymake nil
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil)

  (remove-hook 'lsp-ui-doc-frame-hook
               (lambda (frame _w)
                 (set-face-attribute 'default frame :font default-font :height 100))))

(use-package lsp-imenu
  :ensure lsp-mode
  :config
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))


(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))


(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(defun find-file-at-point-with-line()
  "If file has an attached line num goto that line, ie boom.rb:12."
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
        (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  ;; (find-file-at-point)
  (find-file (ffap-guesser))
  (if (not (equal line-num 0))
      (goto-line line-num)))



(provide 'core-project)
