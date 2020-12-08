(use-package magit
  :bind (("C-x m" . magit-status)
         ("C-c g b" . magit-blame-addition)
         ("C-c g l" . magit-log-buffer-file))
  :custom
  (magit-git-executable "/usr/bin/git")
  (magit-status-headers-hook '(magit-insert-head-branch-header))
  :config
  (setq magit-refresh-status-buffer nil)
  (setq magit-auto-revert-mode nil)
  (setq magit-save-repository-buffers nil)
  (setq magit-diff-refine-hunk t)
  ;; This gives some performance boost to magit
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode))

(use-package browse-at-remote
  :bind (("C-c g w" . browse-at-remote-kill)
         ("C-c g o" . browse-at-remote))
  :init
  (setq browse-at-remote-add-line-number-if-no-region-selected nil))

(use-package git-gutter
  :defer 1
  :after popup
  :ensure git-gutter
  :ensure git-gutter-fringe
  :diminish git-gutter-mode
  :bind (("C-c g d" . git-gutter:popup-hunk)
         ("C-c g r" . git-gutter:revert-hunk))
  ;; :init
  ;; (defvar vc-gutter-in-remote-files nil)

  ;;   (defun vc-gutter-init-maybe ()
  ;;     "Enable `git-gutter-mode' in the current buffer.

  ;; If the buffer doesn't represent an existing file, `git-gutter-mode's activation
  ;; is deferr1 until the file is saved. Respects `git-gutter:disabled-modes'."
  ;;     (let ((file-name (buffer-file-name (buffer-base-buffer))))
  ;;       (when (or vc-gutter-in-remote-files
  ;;                 (not (file-remote-p (or file-name default-directory))))
  ;;         (if (null file-name)
  ;;             (add-hook 'after-save-hook #'vc-gutter-init-maybe nil 'local)
  ;;           (when (and (vc-backend file-name)
  ;;                      (progn
  ;;                        (require 'git-gutter)
  ;;                        (not (memq major-mode git-gutter:disabled-modes))))
  ;;             (if (and (display-graphic-p)
  ;;                      (require 'git-gutter-fringe nil t))
  ;;                 (setq-local git-gutter:init-function      #'git-gutter-fr:init
  ;;                             git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
  ;;                             git-gutter:clear-function     #'git-gutter-fr:clear
  ;;                             git-gutter:window-width -1)
  ;;               (setq-local git-gutter:init-function      'nil
  ;;                           git-gutter:view-diff-function #'git-gutter:view-diff-infos
  ;;                           git-gutter:clear-function     #'git-gutter:clear-diff-infos
  ;;                           git-gutter:window-width 1))
  ;;             (git-gutter-mode +1)
  ;;             (remove-hook 'after-save-hook #'vc-gutter-init-maybe 'local))))))
  ;;   (add-hook 'find-file-hook 'vc-gutter-init-maybe)
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
  (global-git-gutter-mode t)
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  (advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
  (advice-add #'magit-unstage-file :after #'+vc-gutter-update-h)

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

;; (use-package git-gutter-fringe
;;   :)

(use-package git-timemachine)

(use-package gitignore-templates)

(use-package vc
  :ensure nil
  :defer 5
  :config
  (add-hook 'find-file-hook 'vc-refresh-state))


(use-package autorevert
  :ensure nil
  :defer 1
  :bind ("s-u" . revert-buffer)
  :config
  (define-globalized-minor-mode global-autorevert-mode auto-revert-mode
    (lambda ()
      (auto-revert-mode -1)))
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))
  (global-auto-revert-mode 1)
  :diminish (auto-revert-mode . "Ⓐ"))

(setq version-control t
      kept-new-versions 10
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      backup-directory-alist `(("" . ,(concat CACHE-DIR "backups/per-save")))
      browse-url-browser-function 'browse-url-default-browser)

(provide 'core-vc)
