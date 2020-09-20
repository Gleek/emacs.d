(use-package flyspell-lazy
  :after flyspell
  :hook ((flyspell-mode . flyspell-lazy-mode)
         (flyspell-prog-mode . flyspell-lazy-mode)))

(use-package flyspell
  ;; :defer 5
  :disabled
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'conf-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (defvar ispell-program-name)
  (defvar flyspell-issue-message-flag)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq flyspell-issue-message-flag nil)
  (set-face-attribute 'flyspell-incorrect nil :underline '(:color "#6666ff"))
  (set-face-attribute 'flyspell-duplicate nil :underline '(:color "#6688ff"))
  (eval-after-load "flyspell"
    '(define-key flyspell-mode-map (kbd "C-.") nil))
  (setq flyspell-prog-text-faces (delq 'font-lock-string-face flyspell-prog-text-faces))
  :diminish flyspell-mode)

;; TODO: checkout proselint

(use-package spell-fu
  :hook (text-mode . spell-fu-mode)
  :init
  (setq spell-fu-directory (concat CACHE-DIR "spell-fu"))
  :config
  (set-face-attribute 'spell-fu-incorrect-face nil :underline '(:color "#6666ff")))

(use-package flycheck
  :defer 1
  :config
  ;; (global-flycheck-mode -1)
  (add-hook 'prog-mode-hook 'flycheck-mode)

  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))
  ;; (setq flycheck-buffer-switch-check-intermediate-buffers nil)
  (setq flycheck-display-errors-delay 0.25)

  ;; Courtesy - Doom Emacs
  (setq flycheck-indication-mode 'right-fringe)
  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)

  (set-face-attribute 'flycheck-error nil :underline '(:style line :color "#a52a2a"))
  (set-face-attribute 'flycheck-warning nil :underline '(:style line :color "#ca9532"))
  (set-face-attribute 'flycheck-info nil :underline '(:style line :color "#98be65"))
  :diminish flycheck-mode)

;; (flycheck-add-next-checker 'lsp '(warning . php-phpmd))
;; (use-package flycheck-phpstan
;;   :init
;;   (setq phpstan-memory-limit "1G")
;; )

(use-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  ;; (flycheck-posframe-configure-pretty-defaults)
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-prefix "··· "
        flycheck-posframe-error-prefix "✕ ")
  (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p))

(use-package flymake)
(use-package flymake-diagnostic-at-point
  :after flymake
  :init
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(provide 'core-checker)
