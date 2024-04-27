;;; core-util.el ---                                  -*- lexical-binding: t; -*-

(defmacro keep-region (command)
  "Wrap command in code that saves and restores the region"
  (letrec ((command-name (symbol-name command))
           (advice-name (concat command-name "-keep-region")))
    `(progn
       (defadvice ,command (around ,(intern advice-name))
         (let ((deactivate-mark nil)
               (transient-mark-mode transient-mark-mode))
           (save-excursion
             ad-do-it)))
       (ad-activate (quote ,command)))))

(defun nshuffle (sequence)
  "Knuth shuffle for a list"
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun return-false(&rest _)
  "Return nil no matter what the inputs here.
Useful to override functions to become empty"
  nil)


(defun string-length<(str1 str2)
  "Retuns non-nil if STR1 is less in length than STR2."
  (< (length str1) (length str2)))


(defun random-alnum (&optional length)
  (let ((times (or length 1))
        (random ""))
    (setq-local random "")
    (dotimes (_ times)
      (setq random (concat random (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
                                         (i (% (abs (random)) (length alnum))))
                                    (substring alnum i (1+ i))))))
    random))

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


(provide 'core-util)
;;; core-util ends here
