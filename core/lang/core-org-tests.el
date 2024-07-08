;;; core-org-tests.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Umar Ahmad
;; Created: July 08, 2024
;; Modified: July 08, 2024
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; Tests for various functions in core-org.el

;;; Code:

(require 'ert)



(ert-deftest test-org-timestamp-has-repeater-p ()
  "Test cases for org-timestamp-has-repeater-p function."
  (should (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 ++1d>"))
  (should (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 .+1w>"))
  (should (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 +2m>"))
  (should (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 .+3y>"))
  (should-not (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45>"))
  (should-not (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 +1d"))
  (should-not (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 +1w"))
  (should-not (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 +2m"))
  (should-not (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 .+3y"))
  (should-not (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 +d>"))
  (should-not (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 .+w>"))
  (should-not (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 +m>"))
  (should-not (org-timestamp-has-repeater-p "<2024-07-05 Fri 13:30-13:45 .+y>")))



(ert-deftest test-agenda-skip-projects ()
  "Test +agenda-skip-projects function with multiple cases."
  (let ((test-cases
         `((,(concat "* TODO Project A\n"
                    "** TODO Subtask 1\n"
                    "** TODO Subtask 2\n")
            . 18)
           (,(concat "* TODO Project A\n"
                    "** DONE Subtask 1\n"
                    "** TODO Subtask 2\n")
            . 18)
           (,(concat "* TODO Project A\n"
                    "** DONE Subtask 1\n"
                    "** DONE Subtask 2\n")
            . nil))))
    (dolist (case test-cases)
      (with-temp-buffer
        (org-mode)
        (insert (car case))
        (goto-char (point-min))
        (should (equal (+agenda-skip-projects) (cdr case)))))))






(provide 'core-org-tests)
;;; core-org-tests.el ends here
