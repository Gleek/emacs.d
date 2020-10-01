(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delete-by-moving-to-trash t
        dired-dwim-target t
        dired-auto-revert-buffer t
        dired-hide-details-hide-symlink-targets nil)

  (when IS-MAC
    ;; brew install coreutils
    (setq insert-directory-program "/usr/local/bin/gls"))

  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")

  (setq image-dired-dir (concat CACHE-DIR "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        image-dired-thumb-size 150)

  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode)))

(use-package wdired
  :ensure nil
  :bind (:map dired-mode-map
              ("W" . wdired-change-to-wdired-mode))
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))


(use-package peep-dired
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-cleanup-eagerly nil)
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-ignored-extensions
        '("mkv" "webm" "mp4" "mp3" "ogg" "iso" "mov"))
  :bind (:map dired-mode-map
              ("P" . peep-dired)))


(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-subtree
  :ensure
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))

(use-package dired-du
  :bind (:map dired-mode-map
              ("C-c d S" . dired-du-mode)))


(use-package dired-aux
  :ensure nil
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
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
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

(use-package all-the-icons-dired
  ;; messes up subtree
  ;; :disabled t
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (defadvice dired-subtree-toggle (after dired-icons-refreash ())
    "Insert an empty line when moving up from the top line."
    (revert-buffer))
  (ad-activate 'dired-subtree-toggle))

(use-package fd-dired
  :init
  (global-set-key [remap find-dired] #'fd-dired)
  (set-popup-rule! "^\\*F\\(?:d\\|ind\\)\\*$" :ignore t))

(when IS-MAC
  (use-package osx-trash
    ;; brew install trash
    :defer 1
    :config
    (osx-trash-setup)))

(provide 'core-dired)
