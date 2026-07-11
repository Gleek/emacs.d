(use-package magit
  :commands (magit-status magit-dispatch)
  :bind (("C-x m" . magit-status)
         ("C-c g b" . magit-blame-addition)
         ("C-c g f" . magit-diff-buffer-file)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g P" . +magit-pull-request))
  :init
  (setq magit-define-global-key-bindings nil)
  (setq magit-diff-specify-hunk-foreground nil)
  :config
  (setq magit-diff-fontify-hunk t) ;; Too slow currently but I do some custom optimisations
  (setq magit-diff-refine-hunk t)

  (defcustom +magit-diff-refine-max-pair-chars 30000
    "Maximum combined characters in a changed pair to refine.
This limits pathological fine-diff work without rejecting a large hunk
made up of many small delete+insert pairs.  Nil means no limit."
    :type '(choice (const :tag "No limit" nil)
                   integer)
    :group 'magit-diff)

  (defcustom +magit-diff-refine-max-pair-line-ratio 2.0
    "Maximum line-count ratio between the sides of a changed pair.
For example, 2.0 skips refinement when one side has more than twice as
many lines as the other.  This avoids misleading word matches between
structurally different replacements.  Nil means no limit."
    :type '(choice (const :tag "No limit" nil)
                   number)
    :group 'magit-diff)

  (defun +magit-diff-refine--poor-pair-p (beg1 end1 beg2 end2)
    "Return non-nil when a changed pair is a poor refinement candidate.
BEG1..END1 and BEG2..END2 are the deleted and inserted regions passed
by `diff--refine-hunk' to `smerge-refine-regions'."
    (let ((lines1 (count-lines beg1 end1))
          (lines2 (count-lines beg2 end2))
          (chars1 (- end1 beg1))
          (chars2 (- end2 beg2)))
      (or (and +magit-diff-refine-max-pair-chars
               (> (+ chars1 chars2)
                  +magit-diff-refine-max-pair-chars))
          (and +magit-diff-refine-max-pair-line-ratio
               (> (max (/ (float lines1) (max lines2 1))
                       (/ (float lines2) (max lines1 1)))
                  +magit-diff-refine-max-pair-line-ratio)))))

  (defun +magit-diff-refine--remove-syntax-overlays (beg end)
    (dolist (ov (overlays-in beg end))
      (when (+magit-diff-refine--syntax-overlay-p ov)
        (delete-overlay ov))))

  (defun +magit-diff-refine--font-lock-face-p (face)
    (cond
     ((symbolp face)
      (string-prefix-p "font-lock-" (symbol-name face)))
     ((consp face)
      (catch 'found
        (dolist (part face)
          (when (+magit-diff-refine--font-lock-face-p part)
            (throw 'found t)))))))

  (defun +magit-diff-refine--syntax-overlay-p (ov)
    (and (not (overlay-get ov 'diff-mode))
         (or (overlay-get ov '+magit-diff-hunk-syntax)
             (+magit-diff-refine--font-lock-face-p
              (overlay-get ov 'face)))))

  (defun +magit-diff-refine--remove-refined-syntax-overlays (beg end)
    (dolist (ov (overlays-in beg end))
      (when (eq (overlay-get ov 'diff-mode) 'fine)
        (+magit-diff-refine--remove-syntax-overlays
         (overlay-start ov)
         (overlay-end ov)))))

  (defun +magit-diff-tag-hunk-syntax-overlays-a (fn &rest args)
    (let ((syntax-buffer (current-buffer))
          (make-overlay-fn (symbol-function 'make-overlay)))
      (cl-letf (((symbol-function 'make-overlay)
                 (lambda (&rest overlay-args)
                   (let ((ov (apply make-overlay-fn overlay-args)))
                     (when (eq (overlay-buffer ov) syntax-buffer)
                       (overlay-put ov '+magit-diff-hunk-syntax t))
                     ov))))
        (prog1 (apply fn args)
          (+magit-diff-refine--remove-refined-syntax-overlays
           (point-min) (point-max))))))

  (defun +magit-diff-refine-pair-limit-a
      (fn beg1 end1 beg2 end2 props-c &optional preproc props-r props-a)
    (if (and (eq preproc #'diff-refine-preproc)
             (+magit-diff-refine--poor-pair-p beg1 end1 beg2 end2))
        nil
      (prog1 (funcall fn beg1 end1 beg2 end2 props-c preproc props-r props-a)
        (when (eq preproc #'diff-refine-preproc)
          (+magit-diff-refine--remove-refined-syntax-overlays beg1 end1)
          (+magit-diff-refine--remove-refined-syntax-overlays beg2 end2)))))

  (with-eval-after-load 'smerge-mode
    (advice-remove #'smerge-refine-regions
                   #'+magit-diff-refine-pair-limit-a)
    (advice-add #'smerge-refine-regions
                :around #'+magit-diff-refine-pair-limit-a))

  (advice-remove #'magit-diff--update-hunk-syntax
                 #'+magit-diff-tag-hunk-syntax-overlays-a)
  (advice-add #'magit-diff--update-hunk-syntax
              :around #'+magit-diff-tag-hunk-syntax-overlays-a)

  (defun magit-remove-git-lock-file ()
    "Remove git's index lock file, if it exists."
    (interactive)
    (let ((base (magit-toplevel)))
      (delete-file (concat base "/.git/index.lock"))))

  (defun +git-remote-github-parts (remote)
    "Return (OWNER . REPO) parsed from the URL of git REMOTE."
    (let ((url (or (magit-get "remote" remote "url")
                   (user-error "Remote %S has no URL" remote))))
      (if (string-match "[:/]\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?/?\\'" url)
          (cons (match-string 1 url) (match-string 2 url))
        (user-error "Cannot parse owner/repo from remote URL: %s" url))))

  (defun +magit-pull-request ()
    "Open the GitHub PR comparison for the current branch in a browser.
The PR base is the upstream branch (@{upstream}, set via
`branch.<name>.merge'/`remote'); the head is the push branch
(@{push}, set via `push.default'/`remote.pushDefault'/
`branch.<name>.pushRemote').  Handles fork workflows where the
two live on different remotes."
    (interactive)
    (let* ((upstream (or (magit-get-upstream-branch)
                         (user-error "No upstream branch is configured")))
           (push (or (magit-get-push-branch)
                     (user-error "No push branch is configured")))
           (base-parts (magit-split-branch-name upstream))
           (head-parts (magit-split-branch-name push))
           (base-repo (+git-remote-github-parts (car base-parts)))
           (head-repo (+git-remote-github-parts (car head-parts)))
           (base-branch (cdr base-parts))
           ;; Cross-fork comparisons need an "owner:branch" head ref.
           (head-ref (if (equal (car base-repo) (car head-repo))
                         (cdr head-parts)
                       (format "%s:%s" (car head-repo) (cdr head-parts)))))
      (browse-url
       (format "https://github.com/%s/%s/compare/%s...%s?expand=1"
               (car base-repo) (cdr base-repo) base-branch head-ref))))

  (setq magit-refresh-status-buffer nil)
  (setq magit-auto-revert-mode nil)
  (setq magit-save-repository-buffers nil)
  (setq magit-repository-directories "~/Development/")


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
  :bind (("C-c g s" . +ediff-current-file)
         ("C-c g B" . ediff-buffers-dwim))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")

  ;; Courtesy: https://emacs.stackexchange.com/a/17089/2144
  (defvar +ediff-last-windows nil)
  (defun +store-pre-ediff-winconfig ()
    (setq +ediff-last-windows (current-window-configuration)))

  (defun +ediff-current-file()
    "`ediff-current-file' but without checking for autosave"
    (interactive)
    (let ((buffer-auto-save-file-name nil))
      (ediff-current-file)))

  (defun +restore-pre-ediff-winconfig ()
    (set-window-configuration +ediff-last-windows))

  (add-hook 'ediff-before-setup-hook #'+store-pre-ediff-winconfig)
  (add-hook 'ediff-quit-hook #'+restore-pre-ediff-winconfig)

  ;; kill Ediff buffers on quit to avoid leftover control panels
  (defun +ediff-cleanup-buffers ()
    "Kill Ediff control and merge buffers when quitting Ediff."
    (dolist (b (list ediff-control-buffer
                     ;; ediff-buffer-A
                     ;; ediff-buffer-B
                     ))
      (when (buffer-live-p b)
        (kill-buffer b))))
  (add-hook 'ediff-quit-hook #'+ediff-cleanup-buffers)

  (defun ediff-buffers-dwim ()
    "Smart function for ediff buffers.
     If exactly 2 buffers are visible, runs ediff-buffers on them.
     If exactly 3 buffers are visible, runs ediff-buffers3.
     Otherwise falls back to interactive ediff-buffers."
    (interactive)
    (let* ((visible-buffers (mapcar #'window-buffer (window-list)))
           (visible-count (length visible-buffers)))
      (cond
       ((= visible-count 2)
        (ediff-buffers (car visible-buffers)
                       (cadr visible-buffers)))
       ((= visible-count 3)
        (ediff-buffers3 (nth 0 visible-buffers)
                        (nth 1 visible-buffers)
                        (nth 2 visible-buffers)))
       (t
        (call-interactively #'ediff-buffers))))))


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
  (+popup-rule "^\\*git-gutter" :regexp t :align below :size 0.25)

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
  (+popup-rule "^\\*diff-hl" :regexp t :align below :size 0.25)
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

(use-package consult-gh
  :config
  (setopt consult-gh-default-clone-directory "~/Development/")
  (setopt consult-gh-repo-maxnum 200))

(use-package consult-gh-embark
  :demand t
  :after consult-gh
  :config
  (consult-gh-embark-mode +1))

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
