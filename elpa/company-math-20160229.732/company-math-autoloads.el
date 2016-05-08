;;; company-math-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "company-math" "company-math.el" (22272 55582
;;;;;;  0 0))
;;; Generated autoloads from company-math.el

(autoload 'company-latex-commands "company-math" "\
Company backend for latex commands.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'company-math-symbols-latex "company-math" "\
Company backend for LaTeX mathematical symbols.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'company-math-symbols-unicode "company-math" "\
Company backend for insertion of Unicode mathematical symbols.
See the unicode-math page [1] for a list of fonts that have a
good support for mathematical symbols.

 [1] http://ftp.snt.utwente.nl/pub/software/tex/help/Catalogue/entries/unicode-math.html

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; company-math-autoloads.el ends here
