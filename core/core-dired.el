(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump)
         (:map dired-mode-map
               ("f" . nil)))
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delete-by-moving-to-trash t
        dired-dwim-target t
        dired-auto-revert-buffer t
        dired-mouse-drag-files t
        dired-hide-details-hide-symlink-targets nil)

  (when IS-MAC
    ;; brew install coreutils
    (setq insert-directory-program (executable-find "gls")))

  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (set-popup-rule! "^\\*image-dired"
    :slot 20 :size 0.8 :select t :quit nil :ttl 0)
  (setq image-dired-dir (concat CACHE-DIR "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        image-dired-thumb-size 150))

(use-package wdired
  :after dired
  :demand t
  :ensure nil
  :bind (:map dired-mode-map
              ("W" . wdired-change-to-wdired-mode)
              ("e" . wdired-change-to-wdired-mode))
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))


(use-package peep-dired
  :after dired
  :demand t
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-cleanup-eagerly nil)
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-ignored-extensions
        '("mkv" "webm" "mp4" "mp3" "ogg" "iso" "mov" "wav"))
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package dired-posframe
  :disabled t
  :bind (:map dired-mode-map
              ("p" . dired-posframe-show)))


(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-aux
  :ensure nil
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

(use-package dired-hist
  :load-path "packages/dired-hist.el"
  :after dired
  :demand t
  :bind (:map dired-mode-map
              ("l" . dired-hist-go-back)
              ("f" . dired-hist-go-forward))
  :config
  (dired-hist-mode 1))


(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ("//" . dired-toggle-omit))
  :demand t
  :ensure nil
  :config
  (defun dired-toggle-omit()
    (interactive)
    (if dired-omit-mode
        (dired-omit-mode -1)
      (dired-omit-mode 1)))

  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^.\\(svn\\|git\\)\\'"
                "\\|^.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; Courtesy : Doom
  (when-let (cmd (cond (IS-MAC "open")
                       (IS-LINUX "xdg-open")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.xlsx\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.wav\\'" ,(if IS-MAC "afplay" "aplay"))
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

(use-package dirvish
  :after dired
  :demand t
  :bind (:map dired-mode-map
              ("<tab>" . dirvish-subtree-toggle)
              ("z" . dirvish-history-jump)
              ("M-m" . dirvish-mark-menu))
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-attributes '(subtree-state vc-state nerd-icons collapse))
  (setq dirvish-cache-dir (expand-file-name "dirvish/" CACHE-DIR))
  (setq dirvish-subtree-always-show-state t)
  (setq dirvish-header-line-height doom-modeline-height)
  (setq dirvish-use-header-line t)
  (setq dirvish-hide-cursor nil)
  (setq dirvish-mode-line-height doom-modeline-height)
  (setq dirvish-reuse-session t)
  (setq dirvish-subtree-state-style 'nerd)
  (set-face-attribute 'dirvish-hl-line nil :inherit hl-line-face)
  (define-advice dirvish-data-for-dir (:before (_dired _buffer setup))
    (when (and setup (memq 'vc-state dirvish-attributes))
      (set-window-fringes nil 5 1))))

(use-package dired-filter
  :disabled t
  :hook ((dired-mode . dired-filter-mode)
         (dired-mode . dired-filter-by-dot-files)
         (dired-mode . dired-filter-by-omit))
  :bind (:map dired-mode-map ("/?" . +toggle-dired-filter-view))
  :config
  ;; Hide filter bar by default
  (setq dired-filter-show-filters nil)
  (defun +toggle-dired-filter-view()
    (interactive)
    (setq dired-filter-show-filters (if (eq dired-filter-show-filters t) nil t))
    (dired-filter-mode -1)
    (dired-filter-mode +1)))

(use-package disk-usage
  :after dired
  :demand t
  :bind (:map dired-mode-map
              ("C-c d S" . disk-usage-here))
  :init
  (setq disk-usage--format-files 'file-name-nondirectory))


(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(provide 'core-dired)
