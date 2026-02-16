;;; core-gtd.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Umar Ahmad
;; Created: February 08, 2025
;; Modified: February 08, 2025
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; has all the code related to gtd (getting things done).
;; Contains capture flow and agenda management
;; Also contains all things related to agenda

;;; Code:
(defvar +agenda-directory (concat +org-directory "gtd/"))


(defun +capture-inbox()
  (interactive)
  (org-capture nil "i"))
;; trust certain code as being safe

(defun +org-capture-template-from-file ()
  "Return the capture template read from a file."
  (with-temp-buffer
    (insert-file-contents (concat +agenda-directory "project_t.org"))
    (buffer-string)))

(defun +open-worklog()
  (interactive)
  (let ((buffer (find-file-noselect (concat +agenda-directory "worklog.org"))))
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (re-search-backward "^\\*\\*\\* Week" nil t)
    (org-fold-show-entry)
    (goto-char (point-max))))

(defun add-property-with-date-captured ()
  "Add CAPTURED property to the current item."
  (interactive)
  (org-set-property "CAPTURED" (format-time-string "[%F %a %R]")))

(setq org-capture-templates
      `(("i" "inbox" entry (file ,(concat +agenda-directory "inbox.org"))
         "* TODO %?")
        ("l" "link" entry (file ,(concat +agenda-directory "inbox.org"))
         "* TODO %(org-cliplink-capture)" :immediate-finish t)
        ("c" "org-protocol-capture" entry (file ,(concat +agenda-directory "inbox.org"))
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
        ("p" "project" entry (file ,(concat +agenda-directory "next.org"))
         (function +org-capture-template-from-file))))


(defun org-entry-waiting-hook ()
  "Custom function to handle WAITING state transitions."
  (require 'org-edna)
  (when (string= org-state "WAITING")
    (let ((entry-id (org-id-get-create)))
      (save-window-excursion
        (let* ((consult-after-jump-hook nil)
               (target (consult-org-agenda))
               (current-trigger (org-entry-get nil "TRIGGER"))
               (current-trigger-ids ""))
          (when target
            (setq current-trigger-ids
                  (when (and current-trigger (string-match "ids(\\(.*?\\))" current-trigger))
                    (concat (match-string 1 current-trigger) " ")))
            (org-set-property
             "TRIGGER"
             (format "ids(%s) todo!(TODO)" (concat current-trigger-ids entry-id)))))))))

(defun org-entry-delegated-hook()
  "Custom function to handle DELEGATED state transitions."
  (when (string= org-state "DELEGATED")
    (let* ((existing-tags (org-get-tags nil t))
           (crm-separator "[ \t]*:[ \t]*")
           (delegated-to
            (mapconcat #'identity
                       (completing-read-multiple
                        "Delegate to: "
                        (org-global-tags-completion-table
                         (org-agenda-files))
                        nil nil "@" 'org-delegated-history)
                       ":"))
           (tracking-link (read-string "Tracking link: ")))
      (setq existing-tags (delete-dups (cons delegated-to existing-tags)))
      (org-set-tags existing-tags)
      (if (> (length tracking-link) 0)
          (org-entry-put nil "DELEGATED_TRACKING_LINK" tracking-link)))))


(defun post-org-clock-in()
  (let ((state (org-get-todo-state)))
    (when (and state (not (equal state "DOING")))
      (org-todo "DOING")
      (org-set-property "ORIG_STATE" state))))

(defun post-org-clock-out()
  (let ((cur-state (org-get-todo-state))
        (fin-state (org-entry-get nil "ORIG_STATE")))
    (when (and cur-state fin-state (equal cur-state "DOING"))
      (org-todo fin-state)
      (org-delete-property "ORIG_STATE"))))

(defun +switch-to-agenda()
  (interactive)
  (org-agenda nil " "))

(defun +org-insert-date-tree ()
  "Update existing date tree or insert a new one in the current Org buffer with inactive timestamps and checkbox counter."
  (interactive)
  (let* ((today (current-time))
         (year (format-time-string "%Y" today))
         (month (format-time-string "%B" today))
         (week (format-time-string "%W" today))
         (date-stamp (format-time-string "[%Y-%m-%d %a]" today))
         (day-entry-point nil))
    (org-with-wide-buffer
     (goto-char (point-min))
     (unless (re-search-forward (format "^\\* %s" year) nil t)
       (goto-char (point-max))
       (insert (format "\n* %s" year)))
     (org-narrow-to-subtree)
     (goto-char (point-min))
     (unless (re-search-forward (format "^\\*\\* %s" month) nil t)
       (goto-char (point-max))
       (insert (format "\n** %s" month)))
     (org-narrow-to-subtree)
     (goto-char (point-min))
     (unless (re-search-forward (format "^\\*\\*\\* Week %s" week) nil t)
       (goto-char (point-max))
       (insert (format "\n*** Week %s" week)))
     (org-narrow-to-subtree)
     (goto-char (point-min))
     (if (re-search-forward (format "^\\*\\*\\*\\* %s" (regexp-quote date-stamp)) nil t)
         (progn
           (when (looking-at " \\[/\\]")
             (delete-region (match-beginning 0) (match-end 0))
             (insert " [0/0]"))
           (end-of-line)
           (setq day-entry-point (point)))
       (goto-char (point-max))
       (insert (format "\n**** %s [0/0]" date-stamp))
       (newline)
       (setq day-entry-point (point)))
     (widen))
    (org-update-statistics-cookies nil)
    (when day-entry-point
      (goto-char day-entry-point)
      (indent-according-to-mode))))

(use-package org
  :ensure nil
  :bind (("C-c o c" . +capture-inbox)
         ("C-c o C" . org-capture)
         ("C-c o O". org-clock-out)
         ("C-c o W" . +open-worklog)
         ("<f5>" . +capture-inbox)
         ("C-c o g" . consult-org-agenda)
         :map org-mode-map
         ("C-z i d" . +org-insert-date-tree))
  :config
  (add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)
  (add-hook 'org-after-todo-state-change-hook 'org-entry-waiting-hook)
  (add-hook 'org-after-todo-state-change-hook 'org-entry-delegated-hook)
  (add-hook 'org-clock-in-hook #'post-org-clock-in 'append)
  (add-hook 'org-clock-out-hook #'post-org-clock-out 'append)
  (setq org-bookmark-names-plist nil)
  (setq org-archive-location (concat +org-directory "archive.org::* From %s")
        org-log-done 'time
        org-todo-keywords '((sequence "TODO(t)" "DOING(o)"  "|" "DONE(d)")
                            (sequence "BLOCKED(b@/!)" "DELEGATED(e!)" "WAITING(w!)" "|" "CANCELLED(c@/!)")))
  (setq org-refile-targets
        '((nil :maxlevel . 8)
          (org-agenda-files :maxlevel . 8))
        ;; Without this, completers like ivy/helm are only given the first level of
        ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
        ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
        ;; target! e.g. FILE/Tasks/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil))

(use-package org-protocol
  :ensure nil
  :defer 2
  :config
  (add-to-list 'org-protocol-protocol-alist
               '("org-id" :protocol "org-id"
                 :function org-id-protocol-goto-org-id))

  (defun org-id-protocol-goto-org-id (info)
    "This handler simply goes to the org heading with given id using emacsclient.

    INFO is an alist containing additional information passed by the protocol URL.
    It should contain the id key, pointing to the path of the org id.

      Example protocol string:
      org-protocol://org-id?id=abcd"
    (when-let ((id (plist-get info :id)))
      (org-id-goto id))
    nil))

(use-package org-agenda
  :ensure nil
  :init

  :bind (("C-c o A" . org-agenda)
         ("C-c o a" . +switch-to-agenda)
         ("<f1>" . +switch-to-agenda)
         (:map org-agenda-mode-map
               ("i" . org-agenda-clock-in)
               ("c" . +capture-inbox)
               ("M-*" . nil)
               ("o" . +agenda-toggle-skips)
               ("W" . +agenda-item-to-worklog)
               ("E" . +agenda-item-to-lifelog)
               ("r" . +org-agenda-process-inbox-item)
               ("P" . +org-process-inbox)
               ("R" . org-agenda-refile)
               ("C-z C-w" . org-agenda-roam-refile)))
  :config
  ;; Courtesy: https://stackoverflow.com/a/41273964
  (defun org-agenda-bulk-copy-subtree ()
    "Copy marked entries on agenda"
    (interactive)
    (or (eq major-mode 'org-agenda-mode) (error "Not in agenda"))
    (let* ((marker (or (org-get-at-bol 'org-marker) (org-agenda-error)))
           (buffer (marker-buffer marker))
           (pos (marker-position marker))
           (output-buf (get-buffer-create "*RESULTS*")))
      (with-current-buffer buffer
        (goto-char pos)
        (org-back-to-heading t)
        (org-copy-subtree))
      (with-current-buffer output-buf
        (insert org-subtree-clip "\n"))
      (unless (get-buffer-window output-buf)
        (display-buffer output-buf t))))

  (defun org-copy-marked-entries ()
    "Copy marked entries in the Org agenda."
    (interactive)
    (let ((entries ""))
      (dolist (entry (reverse org-agenda-bulk-marked-entries))
        (with-current-buffer (marker-buffer entry)
          (save-excursion
            (goto-char entry)
            (setq entries (concat entries (buffer-substring-no-properties (point-at-bol) (point-at-eol)) "\n")))))
      (kill-new entries)
      (message "Marked entries copied to kill ring.")))

  (defun +agenda-item-to-lifelog()
    (interactive)
    (+agenda-item-to-worklog "lifelog.org"))

  (defun +agenda-item-to-worklog (&optional file)
    "Copies the link to the current item inside agenda and pushes it to worklog.org with a link."
    (interactive)
    (unless file
      (setq file "worklog.org"))
    (let* ((marker (or (org-get-at-bol 'org-marker)
                       (org-agenda-error)))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char pos)
          (org-id-get-create)
          (let ((l (org-store-link nil)))
            (with-current-buffer (find-file-noselect (concat +agenda-directory file))
              (+org-insert-date-tree)
              (goto-char (point-max))
              (insert (format "- [ ] %s\n" l))
              (org-update-statistics-cookies nil)
              (message "Pushed %s to %s"
                       ;; Remove the id: link format and only keep the formatted description
                       (string-trim-right
                        (replace-regexp-in-string "\\[\\[id:[^\]]+\\]\\[" "" l)
                        "\]\]")
                       file)))))))

  ;; Courtesy: https://emacs.stackexchange.com/a/59883
  (defun org-agenda-bulk-mark-regexp-category (regexp)
    "Mark entries whose category matches REGEXP for future agenda bulk action."
    (interactive "sMark entries with category matching regexp: ")
    (let ((entries-marked 0) txt-at-point)
      (save-excursion
        (goto-char (point-min))
        (goto-char (next-single-property-change (point) 'org-hd-marker))
        (while (and (re-search-forward regexp nil t)
                    (setq category-at-point
                          (get-text-property (match-beginning 0) 'org-category)))
          (if (get-char-property (point) 'invisible)
              (beginning-of-line 2)
            (when (string-match-p regexp category-at-point)
              (setq entries-marked (1+ entries-marked))
              (call-interactively 'org-agenda-bulk-mark)))))
      (unless entries-marked
        (message "No entry matching this regexp."))))

  ;; Courtesy Jethro Kuan.
  ;; https://blog.jethro.dev/posts/org_mode_workflow_preview/
  (defun +org-process-inbox ()
    "Called in org-agenda-mode, processes all inbox items."
    (interactive)
    (org-agenda-bulk-mark-regexp-category "inbox")
    (+bulk-process-entries))


  (defun +bulk-process-entries ()
    (if (not (null org-agenda-bulk-marked-entries))
        (let ((entries (reverse org-agenda-bulk-marked-entries))
              (processed 0)
              (skipped 0))
          (dolist (e entries)
            (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
              (if (not pos)
                  (progn (message "Skipping removed entry at %s" e)
                         (cl-incf skipped))
                (goto-char pos)
                (let (org-loop-over-headlines-in-active-region) (funcall '+org-agenda-process-inbox-item))
                ;; `post-command-hook' is not run yet.  We make sure any
                ;; pending log note is processed.
                (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                          (memq 'org-add-log-note post-command-hook))
                  (org-add-log-note))
                (cl-incf processed))))
          (org-agenda-redo)
          (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
          (message "Acted on %d entries%s%s"
                   processed
                   (if (= skipped 0)
                       ""
                     (format ", skipped %d (disappeared before their turn)"
                             skipped))
                   (if (not org-agenda-persistent-marks) "" " (kept marked)")))))


  (defun +bulk-process-entry-set-tags ()
    "Set tags for current entry."
    (interactive)
    (call-interactively #'org-agenda-set-tags)
    (transient-resume))

  (defun +bulk-process-entry-set-priority ()
    "Set priority for current entry."
    (interactive)
    (call-interactively #'org-agenda-priority)
    (transient-resume))

  (defun +bulk-process-entry-refile ()
    "Refile current entry and exit transient."
    (interactive)
    (call-interactively #'org-agenda-refile)
    (transient-quit-one))

  (transient-define-prefix +bulk-process-entry-menu ()
    "Transient menu for processing a single org entry."
    ["Actions"
     ("t" "Set Tags" +bulk-process-entry-set-tags)
     ("p" "Set Priority" +bulk-process-entry-set-priority)
     ("RET" "Refile and continue" +bulk-process-entry-refile)
     ("q" "Skip entry" transient-quit-one)])

  (defun +bulk-process-entries-v2 ()
    "Process marked entries in agenda with transient menu."
    (interactive)
    (if (not (null org-agenda-bulk-marked-entries))
        (let ((entries (reverse org-agenda-bulk-marked-entries))
              (processed 0)
              (skipped 0))
          (dolist (e entries)
            (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
              (if (not pos)
                  (progn (message "Skipping removed entry at %s" e)
                         (cl-incf skipped))
                (goto-char pos)
                (+bulk-process-entry-menu)
                (cl-incf processed))))
          (org-agenda-redo)
          (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
          (message "Acted on %d entries%s%s"
                   processed
                   (if (= skipped 0)
                       ""
                     (format ", skipped %d (disappeared before their turn)"
                             skipped))
                   (if (not org-agenda-persistent-marks) "" " (kept marked)")))))
  ;; Courtesy: https://orgmode.org/worg/org-hacks.html
  (defun org-agenda-log-mode-colorize-block ()
    "Set different line spacing based on clock time duration."
    (save-excursion
      (let ((colors `(,(doom-color 'selection) ,(doom-lighten (doom-color 'selection) 0.2)))
            pos
            duration)
        (goto-char (point-min))
        (while (setq pos (next-single-property-change (point) 'duration))
          (goto-char pos)
          (when (and (not (equal pos (point-at-eol)))
                     (setq duration (org-get-at-bol 'duration)))
            ;; larger duration bar height
            (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 60))))
                  (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
              (overlay-put ov 'face `(:background ,(car colors) :foreground "white"))
              (setq colors (append (cdr colors) (cons (car colors) ())))
              (overlay-put ov 'line-height line-height)
              (overlay-put ov 'line-spacing (1- line-height))))))))

  (defun remove-mouse-face ()
    "Remove mouse face in the current buffer."
    (remove-text-properties (point-min) (point-max) '(mouse-face t)))

  (defun disable-solaire()
    (setq-local solaire-mode-real-buffer-fn #'return-true))

  (add-hook 'org-agenda-finalize-hook #'remove-mouse-face)
  (add-hook 'org-agenda-finalize-hook #'hl-line-mode)
  ;; (add-hook 'org-agenda-finalize-hook #'disable-solaire)
  ;; (add-hook 'org-agenda-finalize-hook #'org-agenda-log-mode-colorize-block)


  (defvar +org-current-effort "1:00"
    "Current effort for agenda items.")
  (defun +org-agenda-set-effort (effort)
    "Set the effort property for the current headline."
    (interactive
     (list (read-string (format "Effort [%s]: " +org-current-effort) nil nil +org-current-effort)))
    (setq +org-current-effort effort)
    (org-agenda-check-no-diary)
    (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error)))
           (buffer (marker-buffer hdmarker))
           (pos (marker-position hdmarker))
           (inhibit-read-only t)
           newhead)
      (org-with-remote-undo buffer
        (with-current-buffer buffer
          (widen)
          (goto-char pos)
          (org-show-context 'agenda)
          (funcall-interactively 'org-set-effort nil +org-current-effort)
          (end-of-line 1)
          (setq newhead (org-get-heading)))
        (org-agenda-change-all-lines newhead hdmarker))))

  (defun +org-agenda-process-inbox-item ()
    "Process a single item in the org-agenda."
    (interactive)
    (org-with-wide-buffer
     (org-agenda-set-tags)

     ;; Prompt for project category codes if it's tagged as a project
     ;; This is so that the subtasks have categories that show some context about the project
     ;; Better way to solve this is to show the project name somehow along with the tasks
     (let (tags category marker)
       (setq marker (org-get-at-bol 'org-marker))
       (setq tags (org-with-point-at marker (org-get-tags)))
       (if (member "project" tags)
           (progn
             (setq category (read-string "Project code: "))
             (org-with-point-at marker
               (org-set-property "CATEGORY" category)))))
     (org-agenda-priority)
     ;; (call-interactively '+org-agenda-set-effort)
     (org-agenda-refile nil nil t)))


  (defun org-id-protocol-link-copy ()
    (interactive)
    (save-window-excursion
      (org-agenda-switch-to)
      (org-kill-new (concat "org-protocol://org-id?id="
                            (org-id-get nil 'create)))
      (message "Link copied to clipboard")))

  (defun org-agenda-roam-refile()
    (interactive)
    (save-window-excursion
      (org-agenda-switch-to)
      (call-interactively 'org-roam-refile)))

  ;; Automatically save all org agenda buffers at regular intervals. Earlier I achieved this with
  ;; autosave, which created a lot of orphan files everywhere.
  (defun +org-save-all-agenda-files ()
    "Save all Org agenda files that haven't been saved in the last 10 seconds."
    (interactive)
    (dolist (file (org-agenda-files))
      (let ((buffer (find-buffer-visiting file)))
        (if buffer
            (with-current-buffer buffer
              (when (and buffer-file-name (buffer-modified-p))
                (save-buffer)))))))
  (setq +org-agenda-save-timer
        (run-with-idle-timer 5 t '+org-save-all-agenda-files))
  ;; To stop: (cancel-timer +org-agenda-save-timer)


  ;; Courtesy: https://stackoverflow.com/a/36830367
  (defun org-random-cmp (a b)
    "Return -1,0 or 1 randomly"
    (- (mod (random) 3) 1))

  (defun +agenda-skip-projects ()
    "Skip items with subtasks, unless they are in WAITING or BLOCKED state."
    (let ((has-subtasks nil)
          (current-state (org-get-todo-state)))
      (org-narrow-to-subtree)
      (save-excursion
        (org-back-to-heading t)
        (when (org-goto-first-child)
          (setq has-subtasks (member (org-get-todo-state) org-not-done-keywords))
          (while (and (not has-subtasks) (org-goto-sibling))
            (when (member (org-get-todo-state) org-not-done-keywords)
              (setq has-subtasks t)))))
      (widen)
      (if (and has-subtasks (not (member current-state '("WAITING" "BLOCKED"))))
          (or (outline-next-heading) (point-max))
        nil)))

  (defun +agenda-skip-if-parent-blocked ()
    "Skip items if their parent is in WAITING or BLOCKED state.
   Does not skip if the item has no parent or is itself in WAITING or BLOCKED state."
    (let ((parent-state nil)
          (current-state (org-get-todo-state)))
      (save-excursion
        (when (org-up-heading-safe)  ; Returns nil if there's no parent
          (setq parent-state (org-get-todo-state))))
      (if parent-state
          ;; If there's a parent, skip if parent is WAITING or BLOCKED
          (when (member parent-state '("WAITING" "BLOCKED"))
            (org-end-of-subtree t))
        ;; If there's no parent, don't skip
        nil)))


  (defun +agenda-skip-errands-worktime()
    (let ((tags (org-get-tags)))
      (when (and (or (member "errand" tags) (member "business" tags))
                 (let ((current-time (decode-time (current-time))))
                   (and (<= 1 (nth 6 current-time) 5) ; Monday to Friday
                        (<= 11 (nth 2 current-time) 17)))) ; 11 AM to 6 PM
        (save-excursion (or (outline-next-heading) (point-max))))))

  (defun org-timestamp-has-repeater-p (timestamp-str)
    "Check if TIMESTAMP-STR contains a repeater."
    (and (string-match-p (concat "\\(" org-ts-regexp "\\)") timestamp-str)
         (string-match-p "\\([.+][0-9]+[hdwmy]\\)" timestamp-str)))

  (defun +org-agenda-skip-if-timestamp-today-or-future ()
    "Skip entries with any timestamp that is today or in the future."
    (let ((end (save-excursion (org-end-of-subtree t)))
          (skip nil))
      (save-excursion
        (while (and (not skip) (re-search-forward org-ts-regexp end t))
          (let* ((timestamp (match-string 0))
                 (timestamp-date (org-time-string-to-time timestamp))
                 (today (org-time-string-to-time (format-time-string "%Y-%m-%d"))))
            (when (and timestamp-date
                       (or (time-less-p today timestamp-date)
                           (org-timestamp-has-repeater-p timestamp)))
              (setq skip t))))
        (when skip
          (goto-char end)
          (point)))))

  (defvar +agenda-disable-skips nil
    "Lot of tasks are skipped from agenda to give a clearer view `+agenda-skip'. But this variable can be toggled to show all")
  (defun +agenda-toggle-skips()
    (interactive)
    (setq +agenda-disable-skips (not +agenda-disable-skips))
    (org-agenda-redo-all))

  (defun +agenda-skip()
    "Collect all skip functions and return the point to skip to."
    (if +agenda-disable-skips
        nil
      (or
       (+agenda-skip-projects)
       (+agenda-skip-if-parent-blocked)
       (org-agenda-skip-entry-if 'scheduled 'deadline)
       (+org-agenda-skip-if-timestamp-today-or-future) ; All entries that have timestamps passed should show up in the agenda.
       (+agenda-skip-errands-worktime))))



  (setq org-agenda-custom-commands
        `((" " "Agenda"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-deadline-warning-days 30)))
            (alltodo ""
                     ((org-agenda-overriding-header "To Refile")
                      (org-agenda-files '(,(concat +agenda-directory "inbox.org")
                                          ,(concat +agenda-directory "inbox_phone.org")))))
            (todo "DOING"
                  ((org-agenda-overriding-header "In Progress")
                   (org-agenda-files '(,(concat +agenda-directory "someday.org")
                                       ,(concat +agenda-directory "next.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "One-off Tasks")
                   (org-agenda-files '(,(concat +agenda-directory "next.org")))
                   (org-agenda-skip-function '(+agenda-skip))))
            (todo "BLOCKED"
                  ((org-agenda-overriding-header "Blocked Tasks")
                   (org-agenda-files '(,(concat +agenda-directory "someday.org")
                                       ,(concat +agenda-directory "next.org")))
                   (org-agenda-skip-function '(+agenda-skip))))
            (todo "DELEGATED"
                  ((org-agenda-overriding-header "Delegated Tasks")
                   (org-agenda-files '(,(concat +agenda-directory "someday.org")
                                       ,(concat +agenda-directory "next.org")))
                   (org-agenda-skip-function '(+agenda-skip))))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting on")
                   (org-agenda-files '(,(concat +agenda-directory "someday.org")
                                       ,(concat +agenda-directory "next.org")))
                   (org-agenda-skip-function '(+agenda-skip))))

            (todo "TODO"
                  ((org-agenda-overriding-header "Someday")
                   (org-agenda-cmp-user-defined #'org-random-cmp)
                   (org-agenda-files '(,(concat +agenda-directory "someday.org")))
                   (org-agenda-sorting-strategy '(user-defined-up))
                   (org-agenda-max-entries (if +agenda-disable-skips nil 10))))
            nil))))





  (setq org-agenda-files (mapcar (lambda(file) (concat +agenda-directory file)) '("inbox.org" "inbox_phone.org" "next.org" "someday.org" "trackers.org"))
        org-agenda-window-setup 'current-window
        org-agenda-skip-unavailable-files t
        org-agenda-tags-column 'auto
        org-agenda-skip-scheduled-if-done t
        org-agenda-span 10
        org-agenda-block-separator ?─; (aref "━" 0)
        org-agenda-start-on-weekday nil
        org-agenda-start-day nil
        org-agenda-time-grid `((daily today require-timed remove-match)
                               ,(number-sequence 800 2000 200)
                               "......"
                               "----------------")
        org-agenda-inhibit-startup t))

(use-package org-edna
  :hook org-mode
  :config
  (org-edna-load))

(use-package org-alert
  :disabled t
  :config
  (setopt org-alert-notify-cutoff 5)
  (setopt org-alert-interval 150)
  (setopt org-alert-notify-after-event-cutoff 0)
  (setopt org-alert-notification-title "Agenda")
  ;; TODO: alert for non scheduled entries as well: https://github.com/spegoraro/org-alert/issues/27
  (org-alert-enable))

(use-package org-wild-notifier
  :defer 5
  :config
  (setq org-wild-notifier-alert-time '(5))
  (setq org-wild-notifier-keyword-whitelist nil)
  (org-wild-notifier-mode t))


(use-package calfw)
(use-package calfw-org
  :ensure calfw calfw-org
  :commands (+open-calendar)
  :bind (:map cfw:calendar-mode-map ("?" . +calendar/show-keys))
  :config
  (set-popup-rule! "^\\*cfw:details\\*$")
  (setq cfw:org-overwrite-default-keybinding t)
  (defun +calendar/show-keys()
    (interactive)
    (which-key-show-full-keymap 'cfw:calendar-mode-map))
  (defun +open-calendar()
    (interactive)
    (cfw:open-calendar-buffer
     ;; :custom-map cfw:my-cal-map
     :contents-sources
     (list
      (cfw:org-create-source (doom-color 'fg)))))
  ;; Courtesy: fuxialexander
  (setq cfw:face-item-separator-color nil
        cfw:render-line-breaker 'cfw:render-line-breaker-none
        cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)
  (setq cfw:display-calendar-holidays nil))

(use-package calfw-blocks
  :after (calfw)
  :demand t
  :ensure (:fetcher github :repo "ml729/calfw-blocks"))

(use-package org-timeline
  :ensure nil
  :commands (org-timeline-insert-timeline)
  ;; :hook (org-agenda-finalize . +org-insert-timeline)
  :config
  ;; Check if org agenda has scheduled items with timestamps
  (defun +org-agenda-has-scheduled()
    (save-excursion
      (goto-char (point-min))
      (if (next-single-property-change (point) 'duration)
          t
        nil)))
  (defun +org-insert-timeline()
    (if (and (+org-agenda-has-scheduled)
             (or (not (boundp 'org-ql-view-buffers-files))
                 (not org-ql-view-buffers-files)))
        (org-timeline-insert-timeline)))
  (setq org-timeline-space-out-consecutive t)
  (setq org-timeline-overlap-in-new-line t)
  (setq org-timeline-show-title-in-blocks t)
  (set-face-attribute 'org-timeline-block nil :inherit 'highlight :background nil))



(use-package org-timeblock
  :after (org-agenda)
  :demand t
  :bind (("C-c a c" . org-timeblock)
         (:map org-agenda-mode-map
               (("C" . org-timeblock)))
         (:map org-timeblock-mode-map
               (("c" . +capture-inbox))))
  :config
  (setq org-timeblock-tag-colors
        '(("errand" . org-timeblock-blue)
          ("work" . org-timeblock-magenta)
          ("meeting" . org-timeblock-red)))
  (setq org-timeblock-inbox-file (concat +agenda-directory "inbox.org"))
  (setq org-timeblock-files org-agenda-files)
  (setq org-timeblock-show-future-repeats t))



(use-package org-ql
  :after (org org-agenda)
  :demand t
  :commands (+org-archive-archivable +org-show-archivable +org-show-pending org-dblock-write:org-ql-list)
  :bind (("C-c o G" . org-ql-find-agenda)
         (:map org-agenda-mode-map
               (("Vp" . +org-show-project-overview)
                ("VP" . +org-show-pending)
                ("VA" . +org-show-archivable)
                ("VV" . org-ql-view))))
  :config
  (set-popup-rule! "^\\*Org QL View:"  :slot -1 :vslot -1 :size 0.5 :ttl 0)
  ;; (add-to-list 'org-ql-views
  ;;              (list "Projects overview"
  ;;                    :buffer-files #'org-agenda-files
  ;;                    :query '(and (todo) (category) (tags "project")
  ;;                                 (ancestors
  ;;                                  (and (todo)
  ;;                                       (not (parent)))))
  ;;                    :sort '(todo date priority)
  ;;                    :super-groups '((:auto-parent t))
  ;;                    :title "Projects overview"))
  (defun org-ql-find-agenda()
    (interactive)
    (inhibit-message-a 'org-ql-find org-agenda-files))

  (org-ql-defpred captured (&key from to _on)
    "Return non-nil if current entry was captured in given period.
Without arguments, return non-nil if entry is captured."
    :normalizers ((`(,predicate-names ,(and num-days (pred numberp)))
                   (let* ((from-day (* -1 num-days))
                          (rest (list :from from-day)))
                     (org-ql--normalize-from-to-on
                       `(captured :from ,from))))
                  (`(,predicate-names . ,rest)
                   (org-ql--normalize-from-to-on
                     `(captured :from ,from :to ,to))))
    :body
    (let ((org-captured-time (org-entry-get (point) "captured")))
      (when org-captured-time
        (and (if from (ts> (ts-parse org-captured-time) from) t)
             (if to (ts< (ts-parse org-captured-time) to) t)))))

  (defun +org-show-pending(&optional arg)
    (interactive "P")
    (let ((days (if arg arg 30)))
      (org-ql-search
        org-agenda-files
        `(and (todo "TODO" "DELEGATED" "BLOCKED" "WAITING" "DOING") (captured :to ,(ts-format (ts-adjust 'day (* -1 days) (ts-now)))))
        :sort '(reverse))))

  (defun +org-show-project-overview()
    (interactive)
    (org-ql-search
      org-agenda-files
      '(and (todo) (category) (tags "project")
            (ancestors
             (and (todo)
                  (not (parent)))))
      :sort '(reverse todo date priority)
      :super-groups '((:auto-parent t))
      :title "Projects overviews"))

  (defun +org-show-archivable()
    (interactive)
    (org-ql-search
      org-agenda-files
      `(closed :to ,(ts-format (ts-adjust 'day -90 (ts-now))))))
  (defun +org-archive-archivable()
    "Automatically archive all items that were closed 60 days ago or before"
    (interactive)
    (org-ql-select
      org-agenda-files
      `(closed :to ,(ts-format (ts-adjust 'day -90 (ts-now))))
      :action 'org-archive-subtree-default))
  (cl-defun org-dblock-write:org-ql-list (params)
    "Modified version of org-ql dblock to show all headings in a list form.
TODO: Add checkbox based on todo state. If in todo show [ ] if done show [X] in none then only the heading"
    (-let* (((&plist :query) params)
            (query (cl-etypecase query
                     (string (org-ql--query-string-to-sexp query))
                     (list  ;; SAFETY: Query is in sexp form: ask for confirmation, because it could contain arbitrary code.
                      (org-ql--ask-unsafe-query query)
                      query)))
            (formatter-fn (lambda (element)
                            (cond
                             ((and org-id-link-to-org-use-id
                                   (org-element-property :ID element))
                              (org-make-link-string (format "id:%s" (org-element-property :ID element))
                                                    (org-element-property :raw-value element)))
                             ((org-element-property :file element)
                              (org-make-link-string (format "file:%s::*%s"
                                                            (org-element-property :file element)
                                                            (org-element-property :raw-value element))
                                                    (org-element-property :raw-value element)))
                             (t (org-make-link-string (org-element-property :raw-value element)
                                                      (org-link-display-format
                                                       (org-element-property :raw-value element)))))))
            (elements (org-ql-query :from (org-agenda-files)
                                    :where query
                                    :select '(org-element-put-property (org-element-headline-parser (line-end-position)) :file (buffer-file-name)))))
      (dolist (element elements)
        (insert "- " (funcall formatter-fn element) "\n"))
      (delete-char -1))))



(use-package org-mru-clock
  :commands (org-quick-clock-in)
  :bind ("s-c" . org-quick-clock-in)
  :config
  (defun org-quick-clock-in()
    "Start a new clock but clock out any running clock first"
    (interactive)
    (if (org-clocking-p) (org-clock-out))
    (org-mru-clock-in))
  (setq org-mru-clock-how-many 100)
  (setq org-mru-clock-keep-formatting t)
  (setq org-mru-clock-persist-file (concat CACHE-DIR "org-mru-clock")))



(use-package org-burnup
  :load-path "packages/org-burnup.el"
  :ensure (gnuplot)
  :after org
  :demand t)



(provide 'core-gtd)
;;; core-gtd.el ends here
