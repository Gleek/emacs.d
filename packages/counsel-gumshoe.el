;;; counsel-gumshoe.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Umar Ahmad
;; Created: August 03, 2021
;; Modified: August 03, 2021
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;;

;;; Code:

(require 'counsel)
(require 'gumshoe)

(defvar counsel-gumshoe-calling-mark (make-marker))
(defun gumshoe-open-mark(mark)
  (when (markerp mark)
    (let (buffer position)
      (setq buffer (marker-buffer mark))
      (setq position (marker-position mark))
      (when buffer
        (set-buffer buffer)
        (or (and (>= position (point-min))
                 (<= position (point-max)))
            (widen))
        (goto-char position)
        (switch-to-buffer buffer)))))

(defun counsel-gumshoe-action(cand)
  (let ((mark (get-text-property 0 'mark cand)))
    (when mark
      (gumshoe-open-mark mark))))

(defun marker-almost-equal(m1 m2)
  (and (eq (marker-buffer m1) (marker-buffer m2))
       (eq (marker-position m1) (marker-position m2))))

(defun counsel-gumshoe-update-fn ()
  "Show preview by candidate."
  (let ((mark (get-text-property 0 'mark (ivy-state-current ivy-last))))
    (when mark
      (counsel--mark-ring-delete-highlight)
      (with-ivy-window
        (gumshoe-open-mark mark)
        (counsel--mark-ring-add-highlight)))))


(defun counsel-gumshoe-get-candidates (marks)
  (when marks
    (save-excursion
      (save-restriction
        ;; Widen, both to save `line-number-at-pos' the trouble
        ;; and for `buffer-substring' to work.
        (widen)
        (let* (buffer
               (max-buffer-name-width 25))
          (mapcar (lambda (mark)
                    (setq buffer (marker-buffer mark))
                    (when buffer
                      (set-buffer buffer)
                      (goto-char (marker-position mark))
                      (let ((linum (line-number-at-pos))
                            (line  (buffer-substring
                                    (line-beginning-position) (line-end-position)))
                            (buf (buffer-name buffer)))
                        (propertize
                         (format
                          "%s : %s"
                          (string-pad (string-limit (format "%s (%d)" buf linum)
                                                    max-buffer-name-width)
                                      max-buffer-name-width)
                          line)
                         'mark mark))))
                  marks))))))

(defun counsel-gumshoe-unwind()
  (gumshoe-open-mark counsel-gumshoe-calling-mark)
  (counsel--mark-ring-delete-highlight))

(defun counsel-gumshoe-ring ()
  "Browse `mark-ring' interactively.
Obeys `widen-automatically', which see."
  (interactive)
  (let* ((counsel-gumshoe-calling-mark (point-marker))
         (marks (copy-sequence (ring-elements (oref gumshoe--global-backlog log))))
         (marks (delete-dups marks))
         (marks
          ;; mark-marker is empty?
          (if (equal (mark-marker) (make-marker))
              marks
            (cons (copy-marker (mark-marker)) marks)))
         (candidates (counsel-gumshoe-get-candidates marks)))
    (if candidates
        (ivy-read "Mark: " candidates
                  :require-match t
                  :update-fn #'counsel-gumshoe-update-fn
                  :action #'counsel-gumshoe-action
                  :unwind #'counsel-gumshoe-unwind
                  :caller 'counsel-gumshoe-ring)
      (message "Mark ring is empty"))))
(ivy-configure 'counsel-gumshoe-ring
  :update-fn #'counsel-gumshoe-update-fn
  :unwind-fn #'counsel-gumshoe-unwind)


(provide 'counsel-gumshoe)
;;; counsel-gumshoe.el ends here
