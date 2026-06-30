;;; core-util.el ---                                  -*- lexical-binding: t; -*-

(defun inhibit-message-a(orig-fun &rest args)
  (let ((inhibit-message t))
    (apply orig-fun args)))

;;Courtesy: db48x https://stackoverflow.com/a/6541072
(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))


(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun string-multi-replace(replacers string)
  (dolist (combination replacers)
    (setq string (string-replace (car combination) (car (cdr combination)) string)))
  string)

(defun ensure-dir(dir)
  "Create directory if doesn't exist"
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defmacro secret-get (key)
  "Utility to get secret value from secret vars.

This checks if the variable is set."
  `(let ((var (intern ,(concat "secret/" (symbol-name key)))))
     (if (boundp var)
         (symbol-value var)
       'nil)))

(defmacro secret-set (&rest key-value-pairs)
  `(progn
     ,@(cl-loop for (key value) on key-value-pairs by #'cddr
                collect `(set (intern (concat "secret/" ,(symbol-name key))) ,value))))


(defun +debug(obj &optional buffer)
  (let ((buffer (or buffer "*debug*")))
    (with-output-to-temp-buffer buffer
      (princ (pp-to-string obj)))
    (with-current-buffer buffer
      (emacs-lisp-mode)
      (goto-char (point-min)))
    (display-buffer (get-buffer buffer))))


(defun get-primary-monitor-resolution()
  "Get the resolution for the primary monitor."
  (let* ((monitors (display-monitor-attributes-list))
         (primary-monitor (car monitors))
         (geometry (alist-get 'geometry primary-monitor))
         (width (nth 2 geometry))
         (height (nth 3 geometry)))
    (list width height)))

(provide 'core-util)
;;; core-util ends here
