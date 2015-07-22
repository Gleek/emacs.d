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
