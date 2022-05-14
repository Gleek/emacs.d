;;; mx-metrics.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: May 13, 2022
;; Modified: May 13, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary: Track the usages of commands run through M-x. Assumes counsel-M-x is used.

;; 

;;; Code:

(defgroup mx-metrics nil
  "Customization group for mx-metrics."
  :group 'local
  :prefix "mx-metrics")

(defvar mx-metrics-table (make-hash-table :test 'equal :size 128)
  "Hash table storing how many times each command is called.")

(defcustom mx-metric-buffer "*M-x metrics*"
  "Buffer to display the mx-metrics data."
  :group 'mx-metrics
  :type 'string)

(defcustom mx-metrics-file "~/.emacs-mx-metrics"
  "File to save `mx-metrics-table'."
  :group 'mx-metrics
  :type 'file)


(defun mxm--obj()
  (mapcar (lambda(k)
            `(,k . ,(gethash k mx-metrics-table)))
          (mxm--sort-table)))

(defun mx-metrics-show-simple()
  (interactive)
  (when (and mx-metrics-table (hash-table-p mx-metrics-table))
    (with-current-buffer (get-buffer-create mx-metric-buffer)
      (erase-buffer)
      (mapc (lambda (obj)
              (insert (format "%s -> %d\n" (car obj) (cdr obj))))
            (mxm--obj)))
    (pop-to-buffer mx-metric-buffer)))

(defun mx-metrics-show()
  (interactive)
  (require 'vtable)
  (when (and mx-metrics-table (hash-table-p mx-metrics-table))
    (with-current-buffer (get-buffer-create mx-metric-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (make-vtable
         :columns '("Name" (:name "Frequency" :width 10))
         :objects (mxm--obj)
         :keymap (define-keymap
                   "q" #'kill-buffer-and-window)
         :getter (lambda (object column vtable)
                   (pcase (vtable-column vtable column)
                     ("Name" (car object))
                     ("Frequency" (cdr object))))))
      (read-only-mode t)
      (pop-to-buffer mx-metric-buffer))))

(defun mx-metrics-save()
  (interactive)
  (unless (file-exists-p mx-metrics-file)
      (make-empty-file mx-metrics-file))
  (write-region (format "%S" mx-metrics-table) nil mx-metrics-file))

(defun mxm--command-exec-rec(cmd)
  (let (count)
    (setq count (gethash cmd mx-metrics-table))
    (puthash cmd (1+ (if count count 0)) mx-metrics-table)))


(defun mxm--advice-mx()
  (advice-add 'counsel-M-x-action :before 'mxm--command-exec-rec))

(defun mxm--unadvice-mx()
  (advice-remove 'counsel-M-x-action 'mxm--command-exec-rec))


(defun mxm--sort-table()
  (let (keys)
    (maphash (lambda (k v) (push k keys)) mx-metrics-table)
    (sort keys (lambda (k1 k2)
                 (> (gethash k1 mx-metrics-table)
                    (gethash k2 mx-metrics-table))))
    keys))

(defun mxm--load()
  (with-temp-buffer
    (insert-file-contents mx-metrics-file)
    (setq mx-metrics-table (read (current-buffer)))))


(defvar mxm--timer)
(defun mxm--start-timer()
  (setq mxm--timer (run-with-timer 600 t
                                   'mx-metrics-save))
  (add-hook 'kill-emacs-hook 'mx-metrics-save))


(defun mxm--stop-timer()
  (if mxm--timer (cancel-timer mxm--timer))
  (remove-hook 'kill-emacs-hook 'mx-metrics-save))

(define-minor-mode mx-metrics-mode
  "Mx metrics mode records the number of times a command was run through M-x."
  :global t
  :init-value t
  :lighter nil
  :keymap nil
  :group 'mx-metrics

  (if mx-metrics-mode
      (progn (mxm--load)
             (mxm--start-timer)
             (mxm--advice-mx))
    (mxm--unadvice-mx)
    (mx-metrics-save)
    (mxm--stop-timer)))

(provide 'mx-metrics)
;;; mx-metrics.el ends here
