;;; web-settings.el -- Custom Web Settings File
;;; Commentary:
;; Settings for the modes of web php js & html
;;; Code:
(require 'flycheck)
;; flycheck for web mode-line
(eval-after-load 'flycheck
  '(progn
     (flycheck-define-checker web-mode-php
       "This is the same as the default php checker except just for web-mode.It continues checking for javascript errors if there are no more PHP errors."
       :command ("phpt" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1" "-d" "log_errors=0" source)
       :error-patterns ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " " (message) " in " (file-name) " on line " line line-end))
       :modes (web-mode))
     (add-to-list 'flycheck-checkers 'web-mode-php)))
(eval-after-load 'flycheck
  '(progn
     (flycheck-define-checker web-mode-phpcs
        "A PHP style checker using PHP_CodeSniffer.
See URL `http://pear.php.net/package/PHP_CodeSniffer/'."
       :command ("phpcs" "--report=checkstyle"
                 (option "--standard=" flycheck-phpcs-standard concat)
                 source)
       :error-parser flycheck-parse-checkstyle
       :modes (web-mode))
     (add-to-list 'flycheck-checkers 'web-mode-phpcs)))

(setq js2-basic-offset 4)
;; (defun my-setup-php ()
;; enable web mode
;; (web-mode)
;; make these variables local
;; (make-local-variable 'web-mode-code-indent-offset)
;; (make-local-variable 'web-mode-markup-indent-offset)
;; (make-local-variable 'web-mode-css-indent-offset)

;; set indentation, can set different indentation level for different code type
;; (setq web-mode-code-indent-offset 4)
;; (setq web-mode-css-indent-offset 2)
;; (setq web-mode-markup-indent-offset 2)
;; )

;; (add-to-list 'auto-mode-alist '("\\.php$" . my-setup-php))
(provide 'web-settings)
;;; web-settings.el ends here
