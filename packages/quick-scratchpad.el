;;; quick-scratchpad.el --- Quick scratchpad in a floating frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Umar Ahmad
;; Created: March 29, 2026
;; Version: 0.0.1
;; Author: Umar Ahmad

;;; Commentary:

;; Opens a floating org-mode scratchpad frame for quick note-taking.
;; Designed to be invoked via emacsclient for a quick-capture workflow.

;;; Code:

(defvar quick-scratchpad-file nil
  "Path to the scratchpad org file.")

(defvar quick-scratchpad-font nil
  "Font string for the scratchpad frame (e.g. \"Iosevka Term-16\").")

(defvar quick-scratchpad-width-ratio 0.4
  "Frame width as a ratio of screen width.")

(defvar quick-scratchpad-height-ratio 0.4
  "Frame height as a ratio of screen height.")

(defvar quick-scratchpad-alpha-background 85
  "Background transparency for the scratchpad frame.")

(defvar quick-scratchpad-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-z C-z") #'quick-scratchpad-save-and-close)
    (define-key map (kbd "C-z k") #'quick-scratchpad-close)
    (define-key map (kbd "C-z n") #'quick-scratchpad-new-entry)
    map)
  "Keymap active in the quick scratchpad frame.")

(define-minor-mode quick-scratchpad-mode
  "Minor mode for the quick scratchpad frame."
  :lighter " Scratchpad"
  :keymap quick-scratchpad-keymap)

(defun quick-scratchpad-save-and-close ()
  "Save the scratchpad buffer and close the frame."
  (interactive)
  (save-buffer)
  (quick-scratchpad-close))

(defun quick-scratchpad-close ()
  "Close the quick scratchpad frame."
  (interactive)
  (widen)
  (quick-scratchpad-mode -1)
  (when (frame-live-p (selected-frame))
    (delete-frame (selected-frame) t)))

(defun quick-scratchpad-new-entry ()
  "Create a new top-level heading with inactive timestamp and narrow to it."
  (interactive)
  (widen)
  (goto-char (point-max))
  (unless (bolp) (newline))
  (insert "* ")
  (org-insert-time-stamp (current-time) t t)
  (newline)
  (org-narrow-to-subtree)
  (goto-char (point-max)))

(defun quick-scratchpad-open ()
  "Open a quick scratchpad frame."
  (interactive)
  (unless quick-scratchpad-file
    (user-error "`quick-scratchpad-file' is not set"))
  (let* ((resolution (get-primary-monitor-resolution))
         (sw (car resolution))
         (sh (cadr resolution))
         (width (floor (* quick-scratchpad-width-ratio sw)))
         (height (floor (* quick-scratchpad-height-ratio sh)))
         (left (floor (* (/ (- 1.0 quick-scratchpad-width-ratio) 2.0) sw)))
         (top (floor (* 0.25 sh)))
         (params `((name . "quick-scratchpad")
                   (width . ,(cons 'text-pixels width))
                   (height . ,(cons 'text-pixels height))
                   (left . ,left)
                   (top . ,top)
                   (internal-border-width . 15)
                   (undecorated-round . t)
                   (no-focus-on-map . t)
                   (no-other-frame . t)
                   ,@(when quick-scratchpad-font
                       `((font . ,quick-scratchpad-font)))
                   (alpha-background . ,quick-scratchpad-alpha-background)))
         (frame (make-frame params)))
    (with-selected-frame frame
      (select-frame-set-input-focus frame)
      (switch-to-buffer (find-file-noselect quick-scratchpad-file))
      (goto-char (point-max))
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil)
      (quick-scratchpad-mode 1))))

(provide 'quick-scratchpad)
;;; quick-scratchpad.el ends here
