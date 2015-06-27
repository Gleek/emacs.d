;;; howdoi-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "howdoi" "howdoi.el" (21901 12371 0 0))
;;; Generated autoloads from howdoi.el

(autoload 'howdoi-query-line-at-point "howdoi" "\
Take a line at point, make the search using that line as a
query and pop up a buffer displaying the answer.

\(fn)" t nil)

(autoload 'howdoi-query-line-at-point-replace-by-code-snippet "howdoi" "\
Take a line at the point, make the search using that line as a
query and replace the line by a found code snippet.

\(fn)" t nil)

(autoload 'howdoi-query "howdoi" "\
Prompts for the QUERY and performs the search for an answer.
Pop up a buffer displaying an answer.

\(fn QUERY)" t nil)

(autoload 'howdoi-query-insert-code-snippet-at-point "howdoi" "\
Prompt for the QUERY and perform the search for an answer.
Insert a found code snippet at point.

\(fn QUERY)" t nil)

(autoload 'howdoi-minor-mode "howdoi" "\
Toggle howdoi minor mode. 

With a prefix argument ARG, enable Line Number mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This minor mode provides a set of key bindings for easy access to
the howdoi.

The following keys are available in `howdoi-minor-mode':

  key             binding
-------         -----------

C-c C-o n       howdoi-show-next-question
C-c C-o p       howdoi-show-previous-question
C-c C-o c       howdoi-show-current-question
C-c C-o b       howdoi-browse-current-question
C-c C-o u       howdoi-query
C-c C-o l       howdoi-query-line-at-point
C-c C-o r       howdoi-query-line-at-point-replace-by-code-snippet
C-c C-o i       howdoi-query-insert-code-snippet-at-point

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("howdoi-pkg.el") (21901 12371 751978 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; howdoi-autoloads.el ends here
