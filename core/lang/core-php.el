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
              ("C-c C-d" . nil)
              ("C-." . nil))
  :config
   ;; Makes typing smooth with very little affect on syntax
   ;; highlighting. We're using tree-sitter anyway.
  (advice-add 'php-syntax-propertize-function :override #'return-false)
  ;; Applying syntax propertize on extended region is slow. Disable
  ;; that and instead depend on tree-sitter to do the highlighting.
  (advice-add 'php-syntax-propertize-extend-region :override #'return-false)
  (remove-hook 'syntax-propertize-extend-region-functions #'php-syntax-propertize-extend-region)
  (setq flycheck-phpmd-rulesets '("cleancode" "codesize" "controversial" "design" "naming"))
  ;; (require 'dap-php)
  ;; (dap-php-setup)
  (setq php-template-compatibility nil)
  (setq c-auto-align-backslashes nil)
  (setq flycheck-phpcs-standard "~/.config/phpcs/phpcs.xml")
  (add-hook 'php-mode-hook (lambda() (setq sp-max-pair-length 5)))
  (add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)

  ;; Fix phpmd to not return error code on invalid syntax
  (let* ((phpmd-checker (flycheck-checker-get 'php-phpmd 'command))
         (ex (car phpmd-checker))
         (rest (cdr phpmd-checker)))
    (push "--ignore-errors-on-exit" rest)
    (setf (flycheck-checker-get 'php-phpmd 'command) (cons ex rest)))

  ;; Demote phpmd errors to info
  (defun +flycheck-phpmd-parse(f &rest args)
    (let ((out (apply f args)))
      (mapcar
       (lambda(el)
         (setf (flycheck-error-level el) 'info)
         el)
       out)))
  (advice-add 'flycheck-parse-phpmd :around #'+flycheck-phpmd-parse)


  (defun php-local-checkers()
    (setq flycheck-local-checkers '((lsp . ((next-checkers . ((warning . phpstan)))))
      (phpstan . ((next-checkers . ((warning . php-phpmd))))))))

  (add-hook 'php-mode-hook 'php-local-checkers)
  (setq c-basic-offset 4))

(use-package web-mode
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


(use-package flycheck-phpstan
  :init
  (add-hook 'php-mode-hook (lambda()(require 'flycheck-phpstan)))
  :config
  (setq-default phpstan-level 8)
  (setq-default phpstan-memory-limit "4G")
  ;; Demote phpstan errors to warnings. Errors should be sytanctical errors only.
  (setcdr (rassoc 'error (flycheck-checker-get 'phpstan 'error-patterns)) 'warning))

(use-package geben
  :init
  (setq geben-temporary-file-directory (concat CACHE-DIR "geben")))

(use-package php-doc-block :ensure nil
  :commands (php-doc-block))

(provide 'core-php)
