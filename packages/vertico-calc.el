;;; vertico-calc.el ---                                  -*- lexical-binding: t; -*-

;;; Code:

;;; vertico-calc.el --- eval calc interactively, using vertico


;;; Commentary:
;;

;;; Code:
(require 'vertico)

(defun vertico-calc--eval(str)
  "Eval calc for with STR."
  (require 'calc)
  (ignore-errors (if (> (length str) 0)
      (let* ((str (format "usimplify(%s)" str))
             (out (calc-eval str)))
        (if (listp out)
            `(,(format  "%s at %s" (car (cdr out)) (- (car out) 10)))
          `(,out))))))

(defun vertico-calc-completion-table()
  "List all func of calc."
  (let (all-calc-func)
    (mapatoms (lambda (f)
		(when (and (functionp f)
			   (string-prefix-p "calcFunc-" (symbol-name f)))
		  (setq all-calc-func (append `(,(substring (symbol-name f) (length "calcFunc-"))) all-calc-func)))))
    all-calc-func))

(defun vertico-calc-completion-at-point ()
  "This is the function to be used for the hook `completion-at-point-functions'."
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'word))
         (start (car bds))
         (end (cdr bds)))
    (list start end (vertico-calc-completion-table) . nil )))


(defvar-keymap calc-completion-map
  :parent minibuffer-mode-map
  "TAB" #'completion-at-point)

(defun vertico-calc-read-calc()
  "Read input and calc."
  (interactive)
  (kill-new
     (let (out)
       (completing-read
	"Expression: "
	(lambda(string predicate action)
          (setq string (minibuffer-contents))
          (setq out (if (> (length string) 0) (vertico-calc--eval string) '("")))
          (complete-with-action action out (car out) predicate))
	nil
	nil
	nil
	'vertico-calc-history)
       (car out))))


(defun vertico-calc()
  "Read input and cal, with `TAB' as completion trigger."
  (interactive)
  (minibuffer-with-setup-hook
      (:append
       (lambda ()
	 (use-local-map calc-completion-map)
	 (add-hook 'completion-at-point-functions
		   #'vertico-calc-completion-at-point nil t)))
    (vertico-calc-read-calc)))


(provide 'vertico-calc)

;;; vertico-calc.el ends here
