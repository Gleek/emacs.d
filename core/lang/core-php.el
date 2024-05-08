(setq lsp-intelephense-files-max-size 2500000)
(setq lsp-intelephense-licence-key "")
(setq lsp-intelephense-licence-key (secret-get intelephense-key))
(setq lsp-intelephense-storage-path (concat CACHE-DIR "lsp-intelephense/"))

(use-package php-mode
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
  (setq php-mode-template-compatibility nil)
  (setq c-auto-align-backslashes nil)
  (setq flycheck-phpcs-standard "~/.config/phpcs/phpcs.xml")
  (add-hook 'php-mode-hook (lambda() (setq sp-max-pair-length 5)))
  (add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)
  ;; (phpmd-ignore-error)
  (setq c-basic-offset 4))

(use-package php-ts-mode
  :mode "\\.php\\'"
  :ensure (:fetcher github :repo "emacs-php/php-ts-mode")
  :config
  (defun php-local-checkers()
    (setq flycheck-local-checkers '((lsp . ((next-checkers . ((warning . phpstan)))))
                                    (phpstan . ((next-checkers . ((warning . php-phpmd))))))))
  (set-face-attribute 'php-function-call nil :inherit 'font-lock-function-call-face)

  (add-hook 'php-ts-mode-hook 'php-local-checkers))


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
  :after (php-ts-mode flycheck)
  :demand t
  :config
  (defun +flycheck-phpmd-parse(f &rest args)
    (let ((out (apply f args)))
      (mapcar
       (lambda(el)
         (setf (flycheck-error-level el) 'info)
         el)
       out)))
  (defun phpmd-ignore-error()
    ;; Fix phpmd to not return error code on invalid syntax
    (let* ((phpmd-checker (flycheck-checker-get 'php-phpmd 'command))
           (ex (car phpmd-checker))
           (rest (cdr phpmd-checker))
           cmd)
      (push "--ignore-errors-on-exit" rest)
      (message "hello")
      (setq cmd (cons ex rest))
      (setf (flycheck-checker-get 'php-phpmd 'command) cmd))
    (advice-add 'flycheck-parse-phpmd :around #'+flycheck-phpmd-parse))

  (phpmd-ignore-error)
  (setq-default phpstan-level 8)
  (setq-default phpstan-memory-limit "4G")
  ;; Demote phpstan errors to warnings. Errors should be sytanctical errors only.
  ;; This doesn't work currently, since there's a custom parser. changed the output of the `flycheck-phpstan-parse-json' from error to warning works.
  ;; TODO: Do this from outside the package.
  (setf (flycheck-checker-get 'phpstan 'modes) '(php-mode php-ts-mode))
  (let ((checker (rassoc 'error (flycheck-checker-get 'phpstan 'error-patterns))))
    (if checker (setcdr checker 'warning))))

(use-package geben
  :init
  (setq geben-temporary-file-directory (concat CACHE-DIR "geben")))

(use-package php-doc-block :ensure nil
  :commands (php-doc-block))

(provide 'core-php)
