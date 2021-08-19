(setenv
 "LIBRARY_PATH"
 "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin19/11.1.0/")

(setq package-enable-at-startup t
      package-quickstart nil
      load-prefer-newer t)
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)



;; Courtesy: Doom Emacs
(unless (daemonp)
  (defvar file-name-handler-alist-backup file-name-handler-alist)
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' later, because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because changes to `file-name-handler-alist'
    ;; since startup ought to be preserved.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'file-name-handler-alist-backup handler))
    (setq file-name-handler-alist file-name-handler-alist-backup))
  (add-hook 'emacs-startup-hook #'reset-file-handler-alist-h))


(defvar gc-cons-threshold-backup gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun reset-gc-cons-threshold()
  (setq gc-cons-threshold gc-cons-threshold-backup))
(add-hook 'emacs-startup-hook #'reset-gc-cons-threshold)
;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)
