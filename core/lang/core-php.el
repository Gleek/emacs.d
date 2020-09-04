(setq lsp-intelephense-files-max-size 2000000)
(setq lsp-intelephense-licence-key intelephense-key)
(setq lsp-intelephense-storage-path (concat CACHE-DIR "lsp-intelephense/"))
(use-package php-mode
  :init
  :ensure php-mode
  :ensure php-boris
  ;; :ensure phpactor
  ;; :ensure php-refactor-mode
  :bind (:map php-mode-map
              ("C-c C-c" . nil)
              ("C-." . nil))
  :config
  (setq php-template-compatibility nil)
  (setq c-auto-align-backslashes nil)
  (add-hook 'php-mode-hook (lambda() (setq sp-max-pair-length 5)))
  (add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)
  (setq c-basic-offset 4))

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-enable-sql-detection t)
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (flycheck-add-mode 'php 'web-mode)
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode)) ;; Using rjsx for that
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(use-package phpcbf
  :config
  (setq phpcbf-standard "~/Development/zomato/phpcs.xml"))

(use-package geben
  :init
  (setq geben-temporary-file-directory (concat CACHE-DIR "geben")))

(provide 'core-php)
