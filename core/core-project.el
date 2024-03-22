(defvar +quick-switch-buffer-hook nil)
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1))
  (run-hook-with-args-until-success '+quick-switch-buffer-hook))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(use-package projectile
  :ensure projectile
  :init
  (setq projectile-cache-file (concat CACHE-DIR "projectile.cache"))
  (setq projectile-known-projects-file (concat CACHE-DIR "projectile-bookmarks.eld"))
  :config
  (defun +copy-project-file-name(arg)
    ;; Copy file name relative to the project. But expanded if arg is passed.
    (interactive "P")
    (let ((filename (if arg
                        buffer-file-name
                      (file-relative-name buffer-file-name (projectile-project-root)))))
      (when filename
        (kill-new filename)
        (message "Copied project file name '%s' to the clipboard." filename))))
  ;; (projectile-load-known-projects)
  (setq projectile-mode-line-prefix "")
  (projectile-mode 1)
  :bind (("C-c p z" . counsel-fzf)
         ("C-c p w" . +copy-project-file-name)
         ("C-c p k" . projectile-kill-buffers)
         ("C-c p c" . projectile-compile-project)
         ("C-c p s" . projectile-save-project-buffers)))

;; (use-package "+projectile-find-file"
;;   :ensure nil
;;   :bind* (("s-o" . +projectile-find-file-dynamic)
;;           ("s-p" . +projectile-find-file-dynamic))
;;   :config

;;   (eval-after-load "all-the-icons-ivy"
;;     '(progn (let ((all-the-icons-ivy-file-commands
;;                    '(counsel-projectile
;;                      counsel-projectile-find-file
;;                      +projectile-find-file-dynamic
;;                      +projectile-find-file
;;                      counsel-projectile-find-dir)))
;;               (all-the-icons-ivy-setup))
;;             ))

;;   (advice-add 'counsel-projectile-find-file :override '+projectile-find-file-dynamic))

(use-package consult-projectile
  :bind*
  (("C-c p p" . consult-projectile-switch-project)
   ("C-c p f" . consult-projectile-find-file)
   ("C-c p D" . consult-projectile-find-dir)
   ("s-p" . consult-projectile))
  :config
  (require 'consult-projectile-plus)
  (consult-projectile-plus-init))

(use-package project
  :ensure nil
  :config
  (setq project-list-file (concat CACHE-DIR "projects")))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; Courtesy doom
  (set-popup-rule! "^\\*Ibuffer\\*$" :ignore t)
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face '(:inherit (success bold))
        ibuffer-formats
        `((mark modified read-only locked
                ,@`(;; Here you may adjust by replacing :right with :center
                    ;; or :left According to taste, if you want the icon
                    ;; further from the name
                    " " (icon 2 2 :left :elide)
                    ,(propertize " " 'display `(space :align-to 8)))
                (name 18 18 :left :elide)
                " " (size 9 -1 :right)
                " " (mode 16 16 :left :elide)
                ,@(when (require 'ibuffer-vc nil t)
                    '(" " (vc-status 12 :left)))
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename)))
  ;; Display buffer icons on GUI
  (define-ibuffer-column icon (:name "  ")
    (let ((icon (if (and (buffer-file-name)
                         (nerd-icons-auto-mode-match?))
                    (nerd-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                  (nerd-icons-icon-for-mode major-mode :v-adjust -0.05))))
      (if (symbolp icon)
          (setq icon (nerd-icons-faicon "nf-fa-file_o" :face 'nerd-icons-dsilver :height 0.8 :v-adjust 0.0))
        icon)))

  ;; Redefine size column to display human readable size
  (define-ibuffer-column size
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

(use-package ibuffer-vc
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))


(use-package ivy-xref
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ag :ensure t )
(use-package rg)

(use-package dumb-jump
  :ensure t
  :commands (dumb-jump-xref-activate)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-rg-cmd "rg")
  (setq dumb-jump-max-find-time 5)
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-selector 'ivy))


(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package smart-jump
  :ensure t
  :after xref
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-?" . smart-jump-references))
  :config
  ;; (smart-jump-setup-default-registers)
  (smart-jump-register :modes 'php-mode
                       :jump-fn 'xref-find-definitions
                       :pop-fn 'xref-pop-marker-stack
                       :refs-fn 'xref-find-references
                       :should-jump t
                       :heuristic 'point
                       :order 1)
  (setq smart-jump-find-references-fallback-function 'smart-jump-find-references-with-rg)
  (defun +smart-jump-go-mode-register()
    ;; Prefer lsp(gopls) over guru and godef for go
    (smart-jump-register :modes 'go-mode
                         :jump-fn 'xref-find-definitions
                         :pop-fn 'xref-pop-marker-stack
                         :refs-fn 'xref-find-references
                         :should-jump t
                         :heuristic 'point
                         :order 1))
  ;; Disable initialization of guru and def
  (advice-add 'smart-jump-go-mode-register :override '+smart-jump-go-mode-register))


(use-package treemacs
  :bind (("C-c t t" . +treemacs-toggle)
         ("C-c p t" . +show-treemacs))
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-space-between-root-nodes nil
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
  (treemacs-follow-mode t)
  (setq treemacs-width 30))

(use-package treemacs-projectile
  :ensure t)

(use-package lsp-treemacs
  :hook (treemacs-select-hook . lsp-treemacs-sync-mode)
  :bind (:map lsp-mode-map ("C-c p e" . lsp-treemacs-errors-list)))

(use-package lsp-mode
  :hook ((js-mode js2-mode js3-mode rjsx-mode go-mode rust-mode php-mode go-ts-mode) . lsp-deferred)
  :commands lsp
  :bind ((:map lsp-mode-map
               ("C-c p r" . lsp-rename)
               ("M-'" . lsp-goto-implementation)))
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  (setq lsp-server-install-dir (concat CACHE-DIR "lsp/"))
  (setq lsp-session-file (concat CACHE-DIR ".lsp-session-v1"))
  :config
  (defun +lsp-set-priority (server priority)
    (setf (lsp--client-priority (gethash server lsp-clients)) priority))

  ;; (company-backend-for-hook 'lsp-mode-hook '((company-capf :with company-yasnippet)))
  (company-backend-for-hook 'lsp-completion-mode-hook '((company-capf :with company-yasnippet)))
  (setq lsp-modeline-code-actions-segments '(name icon))
  ;; (setq lsp-signature-function 'lsp-signature-posframe)

  ;; (setq lsp-disabled-clients '(intelephense))
  (eval-after-load '+popup
    '(set-popup-rule! "^\\*lsp-help" :size 0.35 :ttl 0 :quit t))
  (setq lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-text-document-color nil
        lsp-log-io nil
        lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting nil
        lsp-keep-workspace-alive nil
        ;; lsp-enable-on-type-formatting nil
        lsp-ui-doc-enable nil
        ;; lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
        )
  (setq lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil))

;; (use-package lsp-ivy
;;   :after lsp-mode
;;   :bind (:map lsp-mode-map ("M-<return>" . lsp-ivy-workspace-symbol)))
(use-package consult-lsp
    :after lsp-mode
    :bind (:map lsp-mode-map ("M-<return>" . consult-lsp-symbols)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-mode-map
              ("C-c t u" . lsp-ui-mode))
  :ensure t
  :config

  (setq lsp-ui-doc-max-height 13
        lsp-ui-doc-max-width 50
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-completion-provider :none)

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
  "Copy current line in file to clipboard as '</path/to/file>::<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) "::" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(defun find-file-at-point-with-line()
  "If file has an attached line num goto that line, ie boom.rb:12."
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]::" (point-max) t)
    (if (looking-at "[0-9]+")
        (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  ;; (find-file-at-point)
  (find-file (ffap-guesser))
  (if (not (equal line-num 0))
      (goto-line line-num)))



(provide 'core-project)
