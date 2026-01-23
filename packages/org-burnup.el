;;; org-burnup.el --- Burn-up snapshots for Org subtrees  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Umar Ahmad
;; Created: January 23, 2026
;; Modified: January 23, 2026
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; This package implements a simple "burn-up" history for an Org subtree based
;; on checkbox statistics.
;;
;; Usage (typical):
;;
;;   * Project Foo
;;     #+BEGIN: org-burnup
;;     #+END:
;;
;;     | current_date | total items | completed items |
;;     |--------------+-------------+-----------------|
;;
;; Running `org-update-dblock' (or `org-update-all-dblocks') on the dynamic
;; block:
;;
;; - Counts all checkboxes ([ ], [-], [X]) in the current subtree.
;; - Appends a new row with today's inactive timestamp plus TOTAL/DONE counts to
;;   the history table that lives immediately after a maintained #+PLOT line
;;   following the block.
;; - Fits least-squares lines TOTAL~time and DONE~time, then computes the
;;   intersection to project a completion date (or "∞" when not meaningful).
;; - Writes exactly one line of output inside the block:
;;
;;     Projected completion: [YYYY-MM-DD Ddd]
;;
;; Notes:
;; - The history table is treated as data storage; the dynamic block itself only
;;   displays the projection.
;; - The projection is naive (linear trend over time); it does not account for
;;   scope changes or non-linear progress.

;;; Code:

(require 'org)
(require 'subr-x) ;; string-trim
(require 'cl-lib)

(defgroup org-burnup nil
  "Burn-up snapshots for Org subtrees."
  :group 'org)

(defun org-burnup--subtree-end ()
  "Return end position of the current subtree."
  (save-excursion (org-end-of-subtree t t)))

(defun org-burnup--count-checkboxes (beg end)
  "Return (TOTAL DONE) checkboxes between BEG and END."
  (let ((total 0)
        (done 0))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\(\\[[- X]\\]\\)" end t)
        (cl-incf total)
        (when (string= (match-string 1) "[X]")
          (cl-incf done))))
    (list total done)))

(defun org-burnup--today-inactive ()
  "Return today's date as an inactive org timestamp."
  (format-time-string "[%Y-%m-%d %a]"))

(defun org-burnup--ensure-header (rows)
  "Return table header lines plus ROWS (list of row strings)."
  (append
   (list "| current_date | total items | completed items |"
         "|--------------+-------------+-----------------|")
   rows))

(defun org-burnup--date->days (inactive-ts)
  "Convert an inactive org timestamp string INACTIVE-TS to days since epoch.

Accepts strings like \"[2026-01-23 Fri]\".
Returns a float (so it can be used in regressions)."
  (let* ((s (string-trim inactive-ts "\\[" "\\]"))
         (ymd (car (split-string s " ")))
         (parts (mapcar #'string-to-number (split-string ymd "-")))
         (y (nth 0 parts))
         (m (nth 1 parts))
         (d (nth 2 parts)))
    (/ (float-time (encode-time 0 0 0 d m y)) 86400.0)))

(defun org-burnup--days->inactive (days)
  "Convert DAYS since epoch (float) to an inactive org timestamp."
  (format-time-string "[%Y-%m-%d %a]" (seconds-to-time (* days 86400.0))))

(defun org-burnup--linear-fit (xs ys)
  "Return (A B) least-squares fit for y = A + B*x.

XS and YS are lists of numbers with equal length.
Return nil if a fit is not possible."
  (let* ((n (length xs)))
    (when (>= n 2)
      (let* ((mx (/ (apply #'+ xs) (float n)))
             (my (/ (apply #'+ ys) (float n)))
             (sxx 0.0)
             (sxy 0.0))
        (cl-mapc (lambda (x y)
                   (let ((dx (- x mx)))
                     (setq sxx (+ sxx (* dx dx)))
                     (setq sxy (+ sxy (* dx (- y my))))))
                 xs ys)
        (when (> sxx 0.0)
          (let ((b (/ sxy sxx))
                (a (- my (* (/ sxy sxx) mx))))
            (list a b)))))))

(defconst org-burnup--plot-directive
  "#+PLOT: ind:1 deps:(2 3) type:2d with:linespoints set:\"xdata time\" set:\"timefmt '%Y-%m-%d'\" set:\"format x '%Y-%m-%d'\" set:\"xtics rotate by -45\""
  "Org-plot directive inserted above the burn-up history table.

This expects the first column to be a date-like string; we currently store an
inactive timestamp like [YYYY-MM-DD Ddd], and `org-burnup--date->days' parses
out the YYYY-MM-DD portion for computation.")

(defun org-burnup--parse-burnup-table-at-point ()
  "Parse an org-burnup table at point.

Point must be at the first table line (starting with |).
Returns a plist:
  (:beg :end :rows)
Where :rows is a list of (days total done), one per data row."
  (unless (looking-at "^[ \t]*|")
    (error "org-burnup: expected table at point"))
  (let ((beg (line-beginning-position))
        (rows '()))
    (save-excursion
      (goto-char beg)
      (while (and (not (eobp)) (looking-at "^[ \t]*|"))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          ;; Data rows look like: | [2026-01-23 Fri] | ... | ... |
          (when (string-match "^|[ \t]*\\(\\[[^]]+\\]\\)[ \t]*|[ \t]*\\([0-9]+\\)[ \t]*|[ \t]*\\([0-9]+\\)[ \t]*|" line)
            (let* ((ts (match-string 1 line))
                   (total (string-to-number (match-string 2 line)))
                   (done (string-to-number (match-string 3 line)))
                   (days (org-burnup--date->days ts)))
              (push (list days total done) rows))))
        (forward-line 1))
      (list :beg beg
            :end (point)
            :rows (nreverse rows)))))

(defun org-burnup--projected-completion (rows)
  "Return projected completion as either \"∞\" or an inactive timestamp.

ROWS is a list of (DAYS TOTAL DONE).
Fit separate least-squares lines for TOTAL and DONE vs DAYS, then solve
for their intersection."
  (let* ((n (length rows)))
    (cond
     ((< n 2) "∞")
     (t
      (let* ((xs (mapcar #'car rows))
             (totals (mapcar (lambda (r) (nth 1 r)) rows))
             (dones (mapcar (lambda (r) (nth 2 r)) rows))
             (fit-t (org-burnup--linear-fit xs totals))
             (fit-d (org-burnup--linear-fit xs dones)))
        (if (or (null fit-t) (null fit-d))
            "∞"
          (let* ((at (nth 0 fit-t)) (bt (nth 1 fit-t))
                 (ad (nth 0 fit-d)) (bd (nth 1 fit-d))
                 (den (- bd bt)))
            (if (<= (abs den) 1e-9)
                "∞"
              (let* ((x* (/ (- at ad) den))
                     (x-last (car (last xs))))
                (if (or (<= bd 0.0) (< x* x-last))
                    "∞"
                  (org-burnup--days->inactive (ceiling x*))))))))))))

(defun org-dblock-write:org-burnup (_params)
  "Dynamic block writer for burn-up snapshots.

- Counts checkbox items in the current subtree.
- Appends a new row for today to the history table stored *after* the block.
- Computes a linear projection from the history table and inserts exactly one
  line inside the block:

  Projected completion: [YYYY-MM-DD Ddd]

PARAMS currently ignored (reserved for future options)."
  (let* ((subtree-beg (save-excursion (org-back-to-heading t) (point)))
         (subtree-end (save-excursion (org-burnup--subtree-end)))
         (cb (org-burnup--count-checkboxes subtree-beg subtree-end))
         (total (nth 0 cb))
         (done (nth 1 cb))
         (date (org-burnup--today-inactive))
         (new-row (format "| %s | %11d | %15d |" date total done))
         (after-end (make-marker))
         (table-beg (make-marker))
         (projection "∞"))

    ;; Find this block's BEGIN line, then its corresponding END.
    (save-excursion
      (unless (re-search-backward "^[ \t]*#\\+BEGIN: +org-burnup\\b" nil t)
        (error "org-burnup: couldn't find #+BEGIN: org-burnup"))
      (unless (re-search-forward "^[ \t]*#\\+END:?\\s-*$" nil t)
        (error "org-burnup: couldn't find #+END for this dynamic block"))
      (set-marker after-end (line-end-position)))

    ;; Side-effect outside block: append/update the history table right after END.
    (save-excursion
      (goto-char after-end)
      (forward-line 1)

      ;; Remove blank lines right after #+END:
      (while (and (not (eobp)) (looking-at "^[ \t]*$"))
        (delete-region (line-beginning-position)
                       (min (1+ (line-end-position)) (point-max))))

      (let ((insert-pos (point)))
        (cond
         ;; Expected layout is:
         ;;   #+END:
         ;;   #+PLOT: ...
         ;;   | table ...
         ((looking-at "^[ \t]*#\\+PLOT:")
          ;; Ensure the plot directive is exactly what we expect.
          (let ((bol (line-beginning-position))
                (eol (line-end-position)))
            (delete-region bol (min (1+ eol) (point-max)))
            (insert org-burnup--plot-directive "\n"))

          (forward-line 1)
          (unless (looking-at "^[ \t]*|")
            (error "org-burnup: expected a table after #+PLOT"))

          ;; Existing table: remember its beginning, append new row.
          (set-marker table-beg (point))

          (goto-char (org-table-end))
          (while (and (not (eobp)) (looking-at "^[ \t]*|"))
            (forward-line 1))

          (insert new-row "\n")
          (forward-line -1)
          (org-table-align)

          ;; Ensure next two lines after the table are empty.
          (goto-char (org-table-end))
          (beginning-of-line)
          (forward-line 1)
          (dotimes (_ 2)
            (unless (looking-at "^[ \t]*$")
              (insert "\n")
              (forward-line -1))
            (unless (looking-at "^[ \t]*$")
              (end-of-line)
              (insert "\n"))
            (forward-line 1)))

         ((looking-at "^[ \t]*|")
          ;; Backwards-compat: table exists but no #+PLOT line.
          ;; Insert the plot directive above the table.
          (set-marker table-beg (point))
          (goto-char table-beg)
          (insert org-burnup--plot-directive "\n")

          ;; Append the new row at the end of the table.
          (goto-char (org-table-end))
          (while (and (not (eobp)) (looking-at "^[ \t]*|"))
            (forward-line 1))
          (insert new-row "\n")
          (forward-line -1)
          (org-table-align)

          ;; Ensure next two lines after the table are empty.
          (goto-char (org-table-end))
          (beginning-of-line)
          (forward-line 1)
          (dotimes (_ 2)
            (unless (looking-at "^[ \t]*$")
              (insert "\n")
              (forward-line -1))
            (unless (looking-at "^[ \t]*$")
              (end-of-line)
              (insert "\n"))
            (forward-line 1)))

         (t
          ;; No table: create plot directive + table and remember its beginning.
          (goto-char insert-pos)
          (insert org-burnup--plot-directive "\n")
          (set-marker table-beg (point))

          (insert (mapconcat #'identity
                             (org-burnup--ensure-header (list new-row))
                             "\n")
                  "\n")
          (forward-line -1)
          (org-table-align)

          ;; Ensure two blank lines after the table.
          (goto-char (org-table-end))
          (delete-region (point) (progn (skip-chars-forward "\n\t ") (point)))
          (insert "\n\n"))))

      ;; Compute projection from the (now-updated) history table.
      (when (marker-position table-beg)
        (goto-char table-beg)
        (let* ((parsed (org-burnup--parse-burnup-table-at-point))
               (rows (plist-get parsed :rows)))
          (setq projection (org-burnup--projected-completion rows)))))

    ;; Clean up markers.
    (set-marker after-end nil)
    (set-marker table-beg nil)

    ;; Insert the single line inside the dynamic block.
    (insert (format "Projected completion: %s\n" projection))

    ;; Remove Org's extra blank line between our output line and #+END:.
    ;; Use Org's dblock navigation instead of relying on markers (Org may move
    ;; or invalidate markers during the update).
    (save-excursion
      (org-beginning-of-dblock)
      (let ((beg (point)))
        (unless (re-search-forward org-dblock-end-re nil t)
          (error "org-burnup: couldn't find #+END for this dynamic block"))
        (beginning-of-line)
        ;; Point is at the beginning of the #+END line.
        (let ((end-bol (point)))
          (forward-line -1)
          (when (and (> (point) beg)
                     (looking-at "^[ \t]*$"))
            (delete-region (line-beginning-position) end-bol)))))
    nil))

(provide 'org-burnup)
;;; org-burnup.el ends here
