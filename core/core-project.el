(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(use-package projectile
  :ensure projectile
  :init
  (setq projectile-cache-file (concat CACHE-DIR "projectile.cache"))
  (setq projectile-known-projects-file (concat CACHE-DIR "projectile-bookmarks.eld"))
  (defvar projectile-mode-line)
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-enable-caching t) ;; Projectile turbo!!
  :config
  (projectile-load-known-projects)
  (setq projectile-mode-line-prefix "")
  (projectile-mode 1)
  :bind (("C-c p z" . counsel-fzf)
         ("C-c p k" . projectile-kill-buffers)
         ("C-c p s" . projectile-save-project-buffers)))

(use-package counsel-projectile
  :bind (("M-p" . counsel-projectile-find-file)
         ("C-c p f" . counsel-projectile-find-file)
         ("C-c p b" . counsel-projectile-switch-to-buffer)
         ("C-c p p" . counsel-projectile-switch-project)))


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

(use-package rg
  :ensure rg
  :ensure wgrep-ag
  :config
  (add-hook 'ripgrep-search-mode-hook 'wgrep-ag-setup))

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
  :bind ("C-c p t" . treemacs)
  :config
  (setq treemacs-width 30)
  (setq treemacs-persist-file (concat CACHE-DIR "treemacs-persist")))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package lsp-treemacs
  :after treemacs lsp
  :hook (treemacs-select-hook . lsp-treemacs-sync-mode))

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
  :defer 1
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
