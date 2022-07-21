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

(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))


(defun ensure-dir(dir)
  "Create directory if doesn't exist"
  (unless (file-directory-p dir)
    (make-directory dir t)))

(provide 'core-util)
;;; core-util ends here
