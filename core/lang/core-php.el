(setq lsp-intelephense-files-max-size 2500000)
(setq lsp-intelephense-licence-key (secret-get intelephense-key))
(setq lsp-intelephense-format-braces "k&r")
(setq lsp-intelephense-storage-path (concat CACHE-DIR "lsp-intelephense/"))
(setq flycheck-phpcs-standard "~/.config/phpcs/phpcs.xml")


(use-package php-ts-mode
  :init
  (add-to-list 'major-mode-remap-alist '(php-mode . php-ts-mode))
  (add-to-list 'major-mode-remap-alist '(php-mode-maybe . php-ts-mode))
  :mode "\\.php\\'"
  :ensure nil
  :config
  (add-hook 'php-ts-mode-hook (lambda() (setq-local format-all-formatters '(("PHP" (prettier "--brace-style=1tbs"))))))
  (defun php-local-checkers()
    (setq-local flycheck-local-checkers-chain '((lsp . phpstan)
                                                (phpstan . php-phpmd))))
  (defun php-disable-lsp-imenu()
    "The imenu produced by php-ts-mode is better than what lsp intelephense outputs"
    (setq-local lsp-enable-imenu nil))
  ;; (set-face-attribute 'php-function-call nil :inherit 'font-lock-function-call-face)
  (add-hook 'php-ts-mode-hook 'php-local-checkers)
  (add-hook 'php-ts-mode-hook 'php-disable-lsp-imenu))

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
      (setq cmd (cons ex rest))
      (setf (flycheck-checker-get 'php-phpmd 'command) cmd))
    (advice-add 'flycheck-parse-phpmd :around #'+flycheck-phpmd-parse))

  (phpmd-ignore-error)
  (setq-default phpstan-level 8)
  (setq-default phpstan-memory-limit "4G")
  ;; Demote phpstan errors to warnings. Errors should be sytanctical errors only.
  ;; This doesn't work currently, since there's a custom parser. changed the output of the `flycheck-phpstan-parse-json' from error to warning works.
  ;; TODO: Do this from outside the package.
  (setf (flycheck-checker-get 'phpstan 'modes) '(php-mode php-ts-mode)))

(use-package geben
  :init
  (setq geben-temporary-file-directory (concat CACHE-DIR "geben")))

(use-package php-doc-block :ensure nil
  :commands (php-doc-block))

(provide 'core-php)
