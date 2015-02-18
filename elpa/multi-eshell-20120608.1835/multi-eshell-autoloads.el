;;; multi-eshell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "multi-eshell" "multi-eshell.el" (21732 15135
;;;;;;  514868 319000))
;;; Generated autoloads from multi-eshell.el

(autoload 'multi-eshell-go-back "multi-eshell" "\
Switch to buffer multi-eshell-last-buffer.

\(fn &optional IGNORED)" t nil)

(autoload 'multi-eshell-switch "multi-eshell" "\
If current buffer is not an multi-eshell, switch to current multi-eshell buffer. Otherwise, switch to next multi-eshell buffer.

\(fn &optional IGNORED)" t nil)

(autoload 'multi-eshell "multi-eshell" "\
Creates a shell buffer. If one already exists, this creates a new buffer, with the name '*shell*<n>', where n is chosen by the function generate-new-buffer-name.

\(fn &optional NUMSHELLS)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; multi-eshell-autoloads.el ends here
