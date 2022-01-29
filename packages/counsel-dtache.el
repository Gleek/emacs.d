;;; counsel-dtache.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: January 29, 2022
;; Modified: January 29, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:
;; Counsel extension for the dtache.el.  Primarily adds a few ivy actions that can help trigger
;; actions from a single view instead of depending on separate commands.

;;; Code:
(require 'dtache)
(require 'ivy)

(defun counsel-dtache()
  (interactive)
  (ivy-read "Select session: "
            (counsel-dtache--get-sessions)
            :require-match t
            :action '(1
                      ("o" counsel-dtache--open-session "Open")
                      ("k" counsel-dtache--kill-and-delete "Kill")
                      ("s" counsel-dtache--kill "Stop")
                      ("t" counsel-dtache--tail "Tail")
                      ("R" counsel-dtache--rerun "Rerun")
                      ("d" counsel-dtache--delete "Delete"))
            :caller 'counsel-dtache)
  )

(defun counsel-dtache--get-sessions()
  (dtache-session-candidates (dtache-get-sessions)))

(defun counsel-dtache--decode(cand)
  (cdr cand))

(defun counsel-dtache--open-session(cand)
  (dtache-open-session (counsel-dtache--decode cand)))

(defun counsel-dtache--kill(cand)
  (dtache-kill-session (counsel-dtache--decode cand)))

(defun counsel-dtache--tail(cand)
  (dtache-tail-session (counsel-dtache--decode cand)))

(defun counsel-dtache--delete(cand)
  (dtache-delete-session (counsel-dtache--decode cand))
  (ivy-update-candidates (counsel-dtache--get-sessions)))


(defun counsel-dtache--rerun(cand)
  (dtache-rerun-session (counsel-dtache--decode cand)))

(defun counsel-dtache--kill-and-delete(cand)
  (counsel-dtache--kill cand)
  (counsel-dtache--delete cand))

(provide 'counsel-dtache)
;;; counsel-dtache.el ends here
