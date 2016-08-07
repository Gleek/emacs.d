;;; php-boris-minor-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "php-boris-minor-mode" "php-boris-minor-mode.el"
;;;;;;  (22367 64630 0 0))
;;; Generated autoloads from php-boris-minor-mode.el

(autoload 'php-boris-minor-mode "php-boris-minor-mode" "\
PHP boris minor mode.
Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When the minor mode is enabled, it adds several commands to
interact with the Boris PHP REPL.

The keys largely correspond to what nrepl uses, see
`https://github.com/clojure-emacs/nrepl.el'

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; php-boris-minor-mode-autoloads.el ends here
