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
  (if (> (length str) 0)
      (let* ((str (format "usimplify(%s)" str))
             (out (calc-eval str)))
        (if (listp out)
            `(,(format  "%s at %s" (car (cdr out)) (- (car out) 10)))
          `(,out)))))

(defun counsel-calc()
  (interactive)
  (ivy-read "Expression: "
            #'counsel-calc--eval
            :action (lambda (x)
                      (kill-new x))
            :dynamic-collection t
            :caller 'counsel-calc))
(provide 'counsel-calc)
;;; counsel-calc.el ends here
