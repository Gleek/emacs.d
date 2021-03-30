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
  (let ((times (or length 5))
        (random ""))
    (setq-local random "")
    (dotimes (_ times)
      (setq random (concat random (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
             (i (% (abs (random)) (length alnum))))
                       (substring alnum i (1+ i))))))
    random))


(provide 'core-util)
;;; core-util ends here
