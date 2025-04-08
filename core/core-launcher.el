;;; core-launcher.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Umar Ahmad
;; Created: December 22, 2024
;; Modified: December 22, 2024
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; Attempt to use Emacs as a replacement for Spotlight / Alfred / Raycast / Rofi, etc.

;;; Code:

;; (use-package yequake
;;   :config
;;   (add-to-list 'yequake-frames '("emacs-launcher"
;;                                  (buffer-fns . #'consult-omni-multi)
;;                                  (width . 0.4)
;;                                  (height . 0.5)
;;                                  (top . 0.3)
;;                                  (frame-parameters . ((name . "emacs-launcher")
;;                                                       (minibuffer . only)
;;                                                       (autoraise . t)
;;                                                       (undecorated . t)
;;                                                       (window-system . ns))))))


(use-package consult-omni
  :ensure (:fetcher github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el"))
  :commands (+launch-default-launcher consult-omni +launcher-org-quick-clock-in)
  :config
  (setq consult-omni-show-preview t)
  (setq consult-omni-preview-key "C-o")
  (require 'consult-omni-sources)
  (require 'consult-omni-embark)

  (defun launcher-creator(f &rest args)
    "Create a launcher for the given function F with ARGS.

F is expected to show up in the minibuffer.
Currently the frame is shown on the primary monitor.
Should be updated to show on the active monitor."
    (let* ((vertico-count 15)
           (width (floor (* 0.45 (car (get-primary-monitor-resolution)))))
           (height (floor (* 0.5 (cadr (get-primary-monitor-resolution)))))
           (left  (floor (* 0.25 (car (get-primary-monitor-resolution)))))
           (top (floor (* 0.3 (cadr (get-primary-monitor-resolution)))))
           (params `((name . "emacs-launcher")
                     (width . ,(cons 'text-pixels width))
                     (height . ,(cons 'text-pixels height))
                     (left . ,left)
                     (top . ,top)
                     (internal-border-width . 15)
                     (undecorated-round . t)
                     (no-focus-on-map . t)
                     (delete-before-next-focus . t)
                     (no-other-frame . t)
                     (font . ,(concat default-font "-16"))
                     (alpha-background . 70)
                     (minibuffer . only)))
           (frame (make-frame params)))
      (with-selected-frame frame
        (select-frame-set-input-focus frame)
        (unwind-protect
            (progn (apply f args)
                   nil)
          (progn
            (when (frame-live-p frame) (delete-frame frame t))
            nil)))))
  (defun +launch-consult-omni()
    (interactive)
    (launcher-creator 'consult-omni "" (propertize "\n   " 'face 'consult-omni-path-face)))

  (defun +launch--update-source-prop (source-key prop value)
    "Externally update the property PROP of SOURCE-KEY with VALUE.

Primarily used in the +launch-default-launcher to change the min-value for all the sources."
    (let* ((source-cons (assoc source-key consult-omni--sources-alist))
           (source (cdr source-cons))
           (updated-source (plist-put source prop value)))
      (setf (cdr source-cons) updated-source)))


  (defun +launcher-org-quick-clock-in()
    (interactive)
    (launcher-creator 'org-quick-clock-in))

  (defun +launch-default-launcher()
    (interactive)
    (let* ((consult-omni-multi-sources
            '("Numi" "Apps" "Org Agenda" "Buffer" "Static launcher" "Web search")))
      (+launch-consult-omni)))



  (defun +launch-emoji-completing-read()
    (interactive)
    (require 'emoji-search)
    (let ((vertico-multiform-commands '((vertico-directory-enter grid)))) ; since I usually enter it via vertico only
      (launcher-creator 'emoji-search-completing-read)))

  (defun +launch-gptel()
    (interactive)
    (launcher-creator 'consult-omni-gptel-static))


  (defun +launch-file()
    (interactive)
    (let ((consult-omni-multi-sources '("fd" "mdfind")))
      (+launch-consult-omni)))

  (defun +launch-killer()
    (interactive)
    (require 'dwim-shell-commands)
    (launcher-creator 'dwim-shell-commands-kill-process))

  (defun +launch-zoom()
    (interactive)
    (let ((zoomlink (secret-get zoomlink)))
      (when zoomlink
        (start-process "Zoom" nil "open" zoomlink)
        (kill-new (format "%s" zoomlink)))))


  (defvar consult-omni-launcher-entries
    '(("Emoji" . +launch-emoji-completing-read)
      ("Find File" . +launch-file)
      ("Kill Process" . +launch-killer)
      ("Start Zoom call" . +launch-zoom)
      ("Ask GPT" . +launch-gptel))
    "List of launcher entries and their associated functions.")

  (cl-defun consult-omni--launcher-fetch-results (input &rest args &key callback &allow-other-keys)
    "Return hardcoded entries matching INPUT."
    (let ((candidates (mapcar #'car consult-omni-launcher-entries)))
      (let ((filtered-candidates (cl-remove-if-not
                                  (lambda (cand) (string-match-p (regexp-quote input) cand))
                                  candidates)))
        (mapcar (lambda (cand) (propertize cand
                                           :source "Launch"
                                           :title cand
                                           :url nil
                                           :query input))
                filtered-candidates))))

  (defun consult-omni--launcher-execute (cand)
    "Execute function associated with CAND."
    (let ((entry (assoc cand consult-omni-launcher-entries)))
      (when entry
        (funcall (cdr entry)))))



  (defvar consult-omni-web-searches-entries
    '(("DuckDuckGo" . "https://duckduckgo.com/?q=%s")
      ("devdocs.io" . "https://devdocs.io/#q=%s")
      ("Youtube" . "https://www.youtube.com/results?search_query=%s"))
    "List of fixed entries and their associated functions.")

  (cl-defun consult-omni--web-searches-results(input &rest args &key callback &allow-other-keys)
    "Return hardcoded entries matching INPUT."
    (defvar consult-omni--fixed-timer nil)
    (when consult-omni--fixed-timer
      (cancel-timer consult-omni--fixed-timer))
    (let ((entries (reverse (mapcar (lambda (cand)
                                      (propertize (format "Search for \"%s\" on %s" input (car cand))
                                                  :source "Web Search"
                                                  :title (car cand)
                                                  :url nil
                                                  :query input))
                                    consult-omni-web-searches-entries))))
      (setq consult-omni--fixed-timer (run-with-timer 1 nil callback  entries))
      entries))


  (defun consult-omni--web-search-execute(cand)
    (let* ((query (get-text-property 0 :query cand))
           (engine (get-text-property 0 :title cand))
           (entry (assoc engine consult-omni-web-searches-entries)))
      (when entry
        (browse-url (format (cdr entry) query)))))

  (setq consult-omni-numi-regexp-pattern "\\(.*[[:digit:]\/\*\+-=%^&$\(\{\[].*\\)")
  (setq consult-omni-multi-sources '("Numi" "Apps" "Org Agenda" "Buffer" "Static launcher" "DuckDuckGo API" "Web search"))
  (setq consult-omni-sources-modules-to-load
        (list 'consult-omni-apps
              'consult-omni-buffer
              'consult-omni-dict
              'consult-omni-fd
              'consult-omni-git-grep
              'consult-omni-gptel
              'consult-omni-grep
              'consult-omni-duckduckgo
              'consult-omni-browser-history
              'consult-omni-line-multi
              'consult-omni-locate
              'consult-omni-man
              'consult-omni-mdfind
              'consult-omni-notes
              'consult-omni-numi
              'consult-omni-org-agenda
              'consult-omni-projects
              'consult-omni-ripgrep
              'consult-omni-ripgrep-all
              'consult-omni-wikipedia))
  (consult-omni-sources-load-modules)
  (consult-omni-define-source "Static launcher"
                              :narrow-char ?l
                              :category 'consult-omni-static-launcher
                              :type 'sync
                              :require-match t
                              :face 'default
                              :request #'consult-omni--launcher-fetch-results
                              :on-return #'ignore
                              :on-preview #'ignore
                              :on-callback #'consult-omni--launcher-execute
                              :preview-key consult-omni-preview-key
                              :search-hist 'consult-omni--search-history
                              :select-hist 'consult-omni--selection-history
                              :enabled (lambda () t)
                              :group #'consult-omni--group-function
                              :sort t
                              :interactive consult-omni-intereactive-commands-type
                              :annotate nil
                              :min-input 0)

  (consult-omni-define-source "Web search"
                              :narrow-char ?w
                              :category 'consult-omni-web-search
                              :type 'dynamic
                              :require-match t
                              :face 'default
                              :request #'consult-omni--web-searches-results
                              :on-return #'ignore
                              :on-preview #'ignore
                              :on-callback #'consult-omni--web-search-execute
                              :preview-key consult-omni-preview-key
                              :search-hist 'consult-omni--search-history
                              :select-hist 'consult-omni--selection-history
                              :enabled (lambda () t)
                              :group #'consult-omni--group-function
                              :sort nil
                              :interactive consult-omni-intereactive-commands-type
                              :annotate nil))



(provide 'core-launcher)
;;; core-launcher.el ends here
