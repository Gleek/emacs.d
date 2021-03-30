(use-package dired
  :ensure nil
  :defer 1
  :bind (:map dired-mode-map
              ("f" . nil))
  :commands (dired-open-with-dragger)
  :bind (:map dired-mode-map
              ("rd" . dired-open-with-dragger))
  :config
  (defun dired-open-with-dragger()
    (interactive)
    (start-process-shell-command "dragger" nil (concat "dragger " (string-join (dired-get-marked-files) " "))))
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
  (set-popup-rule! "^\\*image-dired"
    :slot 20 :size 0.8 :select t :quit nil :ttl 0)
  (setq image-dired-dir (concat CACHE-DIR "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        image-dired-thumb-size 150)

  :hook (dired-mode . dired-hide-details-mode))

(use-package wdired
  :after dired
  :ensure nil
  :bind (:map dired-mode-map
              ("W" . wdired-change-to-wdired-mode))
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))


(use-package peep-dired
  :after dired
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-cleanup-eagerly nil)
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-ignored-extensions
        '("mkv" "webm" "mp4" "mp3" "ogg" "iso" "mov"))
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package dired-posframe
  :disabled t
  :bind (:map dired-mode-map
              ("p" . dired-posframe-show)))


(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; (use-package dired-rainbow
;;   :hook (dired-mode . dired-rainbow-mode))

(use-package dired-subtree
  :ensure
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))

;; (use-package dired-du
;;   :bind (:map dired-mode-map
;;               ("C-c d S" . dired-du-mode)))


(use-package dired-aux
  :ensure nil
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

(use-package dired-x
  :ensure nil
  :bind ("C-x C-j" . dired-jump)
  :commands (dired-jump)
  ;; :hook (dired-mode . dired-omit-mode)
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
  ;; :bind (:map dired-mode-map
  ;;             ("C-c d s" . fd-dired))
  :config
  (set-popup-rule! "^\\*F\\(?:d\\|ind\\)\\*$" :ignore t))

(use-package dired-filter
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

;; (use-package dired-ranger)
;; (use-package ranger)

(use-package disk-usage
  :after dired
  :bind (:map dired-mode-map
              ("C-c d S" . disk-usage-here))
  :init
  (setq disk-usage--format-files 'file-name-nondirectory))

;; https://github.com/Fuco1/dired-hacks/issues/126
;; (use-package dired-collapse
;;   :hook (dired-mode . dired-collapse-mode))

(use-package counsel-fd
  :bind (:map dired-mode-map
              ("ff". counsel-fd-file-jump)
              ("fd". counsel-fd-dired-jump)))

(when IS-MAC
  (use-package osx-trash
    ;; brew install trash
    :defer 1
    :config
    (osx-trash-setup)))

(provide 'core-dired)
