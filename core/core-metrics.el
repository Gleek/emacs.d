;;; core-metrics.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: May 07, 2022
;; Modified: May 07, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; 

;;; Code:

(use-package keyfreq
  :init
  (setq keyfreq-file (expand-file-name "keyfreq" CACHE-DIR))
  (setq keyfreq-file-lock (expand-file-name "keyfreq.lock" CACHE-DIR))
  :defer 2
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          org-self-insert-command
          next-line
          previous-line
          mwheel-scroll
          lsp-ui-doc--handle-mouse-movement)))


;; Requires https://activitywatch.net/
;; brew install activitywatch
(use-package activity-watch-mode
  :defer 2
  :config
  (global-activity-watch-mode t)
  (defun +activity-watch-show()
    (interactive)
    (+browse-url "http://localhost:5600/#/timeline"))

  (defun active-watch--save-override()
    (ignore-errors
      (save-match-data
        (activity-watch--call))))
  (advice-add 'activity-watch--save :override 'active-watch--save-override))

(use-package explain-pause-mode
  :disabled t
  :ensure nil
  :bind ("<f12>" . explain-pause-top)
  :config
  (explain-pause-mode))

(use-package profiler
  :bind (("<C-f12>" . profiler-start)
         ("<C-S-f12>" . profiler-report)
         ("<M-f12>" . profiler-stop)))


(use-package mx-metrics
  :ensure nil
  :defer 2
  :config
  (setq mx-metrics-file (expand-file-name "mx-metrics" CACHE-DIR))
  (mx-metrics-mode t))

(use-package esup)

(provide 'core-metrics)
;;; core-metrics.el ends here
