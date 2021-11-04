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

(defvar counsel-gumshoe-candidates nil)

(defun gumshoe-open-mark(candidate)
  (message "Opening mark with %s " (cdr counsel-gumshoe-candidates))
  (gumshoe--jump (cadr (assoc candidate counsel-gumshoe-candidates))))

(defun open-arbit-mark(mark)
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


(defun counsel-gumshoe-update-fn ()
  "Show preview by candidate."
  (let ((mark (ivy-state-current ivy-last)))
    (when mark
      (counsel--mark-ring-delete-highlight)
      (with-ivy-window
        (gumshoe-open-mark mark)
        (counsel--mark-ring-add-highlight)))))

(defun counsel-gumshoe-unwind()
  ;; (unless (eq ivy-exit 'done) (open-arbit-mark counsel-gumshoe-calling-mark))
  (counsel--mark-ring-delete-highlight))

(defun counsel-gumshoe-peruse (recs slot-spec &optional entry-filter)
    "Peruse SLOT-SPEC fields of RECS.
Pre-filter results with ENTRY-FILTER."
    (let* ((counsel-gumshoe-calling-mark (point-marker))
           (entries recs)
           (format-schema (string-join (mapcar #'symbol-name slot-spec) "|"))
           (prompt (concat "(" format-schema "): "))
           (format-components (mapcar #'(lambda (_) "%s") slot-spec))
           (format-string (string-join format-components "|"))
           (filtered-entries (if entry-filter
                                 (seq-filter entry-filter entries)
                               entries))
           (entry-strings (gumshoe--format-records filtered-entries format-string slot-spec))
           (counsel-gumshoe-candidates (cl-mapcar #'list entry-strings filtered-entries)))
      (ivy-read "Mark: " counsel-gumshoe-candidates
                :require-match t
                :update-fn #'counsel-gumshoe-update-fn
                :unwind #'counsel-gumshoe-unwind
                :action #'gumshoe-open-mark
                :caller 'counsel-gumshoe-peruse)))
(ivy-configure 'counsel-gumshoe-peruse
  :update-fn #'counsel-gumshoe-update-fn
  :unwind-fn #'counsel-gumshoe-unwind)


(provide 'counsel-gumshoe)
;;; counsel-gumshoe.el ends here
