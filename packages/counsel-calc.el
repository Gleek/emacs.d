;;; counsel-calc.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Umar Ahmad
;; Created: September 01, 2021
;; Modified: September 01, 2021
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary: A small utility to do quick calculations and show output in an ivy window

;; 

;;; Code:


(defun counsel-calc--eval(str)
  (require 'calc)
  (ignore-errors (if (> (length str) 0)
      (let* ((str (format "usimplify(%s)" str))
             (out (calc-eval str)))
        (if (listp out)
            `(,(format  "%s at %s" (car (cdr out)) (- (car out) 10)))
          `(,out))))))

(defun counsel-calc--push-to-calc(str)
  (require 'calc-aent)
  ;; (calc-evaluate str)
  (calc)
  (calc-wrapper
   (calc-alg-entry str)))


(defun completing-read-calc()
  (interactive)
  (kill-new
   (completing-read
    "Expression: "
    (lambda(string predicate action)
      (let (out)
        (setq string (minibuffer-contents))
        (setq out (if (> (length string) 0) (counsel-calc--eval string) '("")))
        ;; FIXME: moving cursor on prompt removes.
        (complete-with-action action out (car out) predicate))))))


(defun vertico-calc()
  (interactive)
  (let ((vertico-count 1))
    (completing-read-calc)))


(defun counsel-calc()
  (interactive)
  (if (not (package-installed-p 'ivy))
      (user-error "Ivy not present"))
  (ivy-read "Expression: "
            #'counsel-calc--eval
            :action '(1
                      ("k" kill-new "copy")
                      ("c" counsel-calc--push-to-calc "Open in cal"))
            :dynamic-collection t
            :caller 'counsel-calc))
(provide 'counsel-calc)
;;; counsel-calc.el ends here
