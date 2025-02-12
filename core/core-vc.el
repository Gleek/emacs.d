(use-package magit
  :commands (magit-status magit-dispatch)
  :bind (("C-x m" . magit-status)
         ("C-c g b" . magit-blame-addition)
         ("C-c g f" . magit-diff-buffer-file)
         ("C-c g l" . magit-log-buffer-file))
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (defun magit-remove-git-lock-file ()
    "Remove git's index lock file, if it exists."
    (interactive)
    (let ((base (magit-toplevel)))
      (delete-file (concat base "/.git/index.lock"))))

  (setq magit-refresh-status-buffer nil)
  (setq magit-auto-revert-mode nil)
  (setq magit-save-repository-buffers nil)

  (setq magit-diff-refine-hunk t)

  (setopt magit-format-file-function #'magit-format-file-nerd-icons)

  (setq magit-status-headers-hook '(magit-insert-head-branch-header))
  ;; This gives some performance boost to magit
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode))


(use-package ediff
  :ensure nil
  :bind ("C-c g s" . ediff-current-file)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w"))


(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("C-c C-s m" . smerge-keep-mine)
              ("C-c C-s o" . smerge-keep-other)
              ("C-c C-s b" . smerge-keep-base)
              ("C-c C-s a" . smerge-keep-all)
              ("C-c C-s RET" . smerge-keep-current)))

;; (use-package magit-delta
;;   :hook (magit-mode . magit-delta-mode))

;; For some reason it takes a lot of time to run with package-quickstart-enable
;; (use-package forge
;;   :after magit
;;   :config
;;   (setq forge-database-file (concat CACHE-DIR "forge-database.sqlite")))


;; (use-package code-review
;;   :ensure forge
;;   :bind ("C-c g R" . code-review-start)
;;   :config
;;   (setq code-review-log-file (concat CACHE-DIR "code-review-error.log"))
;;   (setq code-review-db-database-file (concat CACHE-DIR "code-review-db-file.sqlite")))

(use-package browse-at-remote
  :bind (("C-c g w" . browse-at-remote-kill)
         ("C-c g o" . browse-at-remote))
  :config
  (setq browse-at-remote-add-line-number-if-no-region-selected nil))

(use-package git-gutter-fringe)
(use-package git-gutter
  :diminish git-gutter-mode
  :bind (("C-c g d" . git-gutter:popup-hunk)
         ("C-c g r" . git-gutter:revert-hunk))
  :init
  ;; (defvar vc-gutter-in-remote-files nil)

  (defun vc-gutter-init-maybe()
    (let ((file-name (buffer-file-name (buffer-base-buffer))))
      (if file-name (unless (file-remote-p file-name)
                      (git-gutter-mode t)))))

  (add-hook 'find-file-hook 'vc-gutter-init-maybe)
  :config
  (set-popup-rule! "^\\*git-gutter" :select nil :size '+popup-shrink-to-fit)

  (require 'git-gutter-fringe)
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps courtesy doom emacs
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  ;; (global-git-gutter-mode t)
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  (eval-after-load 'magit
    '(progn
       (advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
       (advice-add #'magit-unstage-file :after #'+vc-gutter-update-h)))


  ;; Backup for git gutter without fringe. Tries to replicate the fringes
  (setq git-gutter:modified-sign " "
        git-gutter:added-sign " "
        git-gutter:deleted-sign " ")
  (set-face-attribute
   'git-gutter:modified nil
   :family "Arial"
   :background "SandyBrown"
   :height 70)
  (set-face-attribute
   'git-gutter:added nil
   :family "Arial"
   :background "DarkGreen"
   :height 70)
  (set-face-attribute
   'git-gutter:deleted nil
   :family "Arial"
   :background "DarkRed"
   :height 70))

(use-package diff-hl
  ;; :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  ;; :hook (find-file . diff-hl-mode)
  :config
  ;; Courtesy: Doom Emacs
  (set-popup-rule! "^\\*diff-hl" :select nil :size '+popup-shrink-to-fit)
  (setq vc-git-diff-switches '("--histogram"))
  (setq diff-hl-flydiff-delay 0.5)
  (setq diff-hl-show-staged-changes nil)
  (defun +vc-diff-hl-define-bitmaps-h()
    (define-fringe-bitmap 'diff-hl-bmp-middle [224] nil nil '(center repeated))
    (define-fringe-bitmap 'diff-hl-bmp-delete [240 224 192 128] nil nil 'top))
  (defun +vc-gutter-type-face-fn (type _pos)
    (intern (format "diff-hl-%s" type)))
  (defun +vc-gutter-type-at-pos-fn (type _pos)
    (if (eq type 'delete)
        'diff-hl-bmp-delete
      'diff-hl-bmp-middle))
  (defun +vc-gutter-fix-diff-hl-faces-h ()
    (set-face-background 'diff-hl-insert nil)
    (set-face-background 'diff-hl-delete nil)
    (set-face-background 'diff-hl-change nil))
  ;; FIXME: doesn't shrink to fit properly
  (defun +vc-gutter--shrink-popup-a (fn &rest args)
    (cl-letf (((symbol-function 'diff-refine-hunk)
               (lambda ()
                 (funcall diff-refine-hunk)
                 (shrink-window-if-larger-than-buffer))))
      (apply fn args)))
  (advice-add #'diff-hl-revert-hunk-1 :around #'+vc-gutter--shrink-popup-a)
  (add-hook 'diff-hl-mode-hook #'+vc-gutter-fix-diff-hl-faces-h)
  (advice-add #'diff-hl-fringe-bmp-from-pos  :override #'+vc-gutter-type-at-pos-fn)
  (advice-add #'diff-hl-fringe-bmp-from-type :override #'+vc-gutter-type-at-pos-fn)
  (advice-add #'diff-hl-define-bitmaps :override #'+vc-diff-hl-define-bitmaps-h)
  (setq diff-hl-draw-borders nil))


(use-package git-timemachine)

(use-package gitignore-templates)

(use-package vc
  :ensure nil
  :hook (find-file . vc-refresh-state))

(use-package autorevert
  :ensure nil
  :defer 1
  :bind ("s-u" . revert-buffer)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))
  (global-auto-revert-mode 1)
  :diminish (auto-revert-mode . "Ⓐ"))

(use-package backup-walker)

(setq version-control t
      kept-new-versions 10
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      backup-directory-alist `(("" . ,(concat CACHE-DIR "backups/per-save")))
      browse-url-browser-function 'browse-url-default-browser)
(provide 'core-vc)
