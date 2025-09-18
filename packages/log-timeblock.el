;;; log-timeblock.el --- Org-log timeblock calendar visualization -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'timeblock)

(defgroup log-timeblock nil
  "Visual calendar for org timeblocked log files."
  :group 'calendar)

(defcustom log-timeblock-files nil
  "List of org log files to aggregate and visualize."
  :type '(repeat file))

(defcustom log-timeblock-last-duration 30
  "Default duration in minutes for the last timeblock task each day."
  :type 'integer)

;; No longer user-set, now calculated dynamically.
(defvar log-timeblock-visible-days 4
  "Number of days to show in the calendar view (auto-calculated, min 1).")

(defvar log-timeblock-end-date nil)
(defvar log-timeblock-date-list nil)
(defvar log-timeblock-buffer-name "*log-timeblock*")

(defun log-timeblock-get-date-list (end-date n)
  "Return a list of N date strings [YYYY-MM-DD DDD] ending with END-DATE. Replace with file aggregation in prod."
  (let ((base (date-to-time end-date)))
    (cl-loop for i from (1- n) downto 0
             collect (format-time-string "%Y-%m-%d %a" (time-subtract base (days-to-time i))))))

(defun log-timeblock-extract-date-blocks (dates)
  "For each date in DATES, collect block-strings from all log files. Returns alist (date . ((file . block-string)...))."
  (cl-loop for date in dates
           collect
           (cons date
                 (cl-loop for file in log-timeblock-files
                          if (file-readable-p file)
                          collect
                          (with-temp-buffer
                            (insert-file-contents file)
                            (let ((heading (concat "^\\*\\{4\\} \\[" date "\\]"))
                                  (start) (end))
                              (goto-char (point-min))
                              (when (re-search-forward heading nil t)
                                (setq start (line-beginning-position))
                                (if (re-search-forward "^\\*\\{4\\} " nil t)
                                    (setq end (match-beginning 0))
                                  (setq end (point-max)))
                                (cons file (buffer-substring-no-properties start end)))))))))

(defun log-timeblock-shift-time (dt minutes)
  "Return new decoded time list from DT (decoded time) shifted by MINUTES."
  (let* ((new-time (time-add (apply #'encode-time dt) (seconds-to-time (* minutes 60)))))
    (decode-time new-time)))

(defun log-timeblock-parse-date-block (date block-string &optional last-duration)
  "Parse tasks under a **** [DATE] heading block. Returns list of plists: (:start ... :status ... :title ...)."
  (let* ((results '())
         (date-ymd (car (split-string date)))
         (default-last (or last-duration (and (boundp 'log-timeblock-last-duration) log-timeblock-last-duration) 30)))
    (with-temp-buffer
      (insert block-string)
      (goto-char (point-min))
      (forward-line 1)
      (while (re-search-forward "^[ \t]*[-+] \\[\\([ Xx]\\)\\] \\([0-9][0-9]:[0-9][0-9]\\)\\*? \\(.*\\)$" nil t)
        (let ((status (string= (match-string 1) "X"))
              (hhmm (match-string 2))
              (title (match-string 3)))
          (push (list :start (parse-time-string (format "%s %s" date-ymd hhmm))
                      :status status
                      :title title)
                results)))
      (nreverse results))))

(defun log-timeblock--dedup-tasks (tasks)
  "Deduplicate list of tasks (each plist) by start time, keeping the last occurrence."
  (let ((seen (make-hash-table :test 'equal)) dedup)
    (dolist (task (reverse tasks))
      (let* ((start (plist-get task :start)))
        (unless (gethash start seen)
          (puthash start t seen)
          (push task dedup))))
    (nreverse dedup)))

(defun log-timeblock--aggregate-day-tasks (date blocks &optional last-duration)
  "Aggregate all task entries for DATE given BLOCKS ((file . block-string) ...).
Returns merged, deduped plist list, without :end annotation (populated after merge)."
  (let (all)
    (dolist (fb blocks)
      (let ((block-string (cdr fb)))
        (when block-string
          (setq all (append all (log-timeblock-parse-date-block date block-string))))))
    (log-timeblock--dedup-tasks all)))

(defun log-timeblock--populate-end (tasks &optional last-duration)
  "Sorts tasks by :start, then populates :end for each by next :start or default duration."
  (let* ((default-last (or last-duration (and (boundp 'log-timeblock-last-duration) log-timeblock-last-duration) 30))
         (sorted (sort tasks (lambda (a b)
                              (time-less-p (apply #'encode-time (plist-get a :start))
                                           (apply #'encode-time (plist-get b :start))))))
         (l sorted)
         (out nil))
    (while l
      (let* ((cur (car l))
             (next (cadr l))
             (end (if next (plist-get next :start)
                    (log-timeblock-shift-time (plist-get cur :start) default-last))))
        (push (plist-put cur :end end) out))
      (setq l (cdr l)))
    (nreverse out)))

(defun log-timeblock-render-columns (date-list)
  "Render N columns in `*log-timeblock*` buffer. Pulls fresh data from org files."
  (let ((buf (get-buffer-create log-timeblock-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (let* ((width (max 40 (floor (/ (- (window-pixel-width (get-buffer-window buf)) 15) log-timeblock-visible-days))))g
       (win-height (- (window-pixel-height (get-buffer-window buf)) 30)))
        (dolist (date date-list)
          (let* ((blocks (cdr (assoc date (log-timeblock-extract-date-blocks (list date)))))
                 (raw-tasks (log-timeblock--aggregate-day-tasks date blocks))
                 (tasks (log-timeblock--populate-end raw-tasks))
                 (entries
                  (mapcar (lambda (task)
                            (list (cons 'start (plist-get task :start))
                                  (cons 'end (plist-get task :end))
                                  (cons 'title (log-timeblock--clean (plist-get task :title)))
                                  (cons 'status (plist-get task :status))))
                          tasks)))
            (timeblock-insert-column
             entries
             (parse-time-string (concat (car (split-string date)) " 00:00"))
             width win-height
             :show-date t
             :show-current-time t ))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;; Workaround for timeblock.el bug: strip &, < from titles
(defun log-timeblock--clean (title)
  "Return TITLE with all & characters removed."
  (let ((out title))
    (setq out (replace-regexp-in-string "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" out)) ;; org link markup
    (setq out (replace-regexp-in-string "[&<]" "" out)) ;; & and <
    out))

(defun log-timeblock--calculate-visible-days ()
  "Return number of columns to display: ceil((window-pixel-width)/200), min 1."
  (max 1 (ceiling (/ (window-pixel-width (selected-window)) 200.0))))

(defun log-timeblock-update-view ()
  "Update columns and buffer for the current end date."
  (setq log-timeblock-visible-days (log-timeblock--calculate-visible-days))
  (setq log-timeblock-date-list
        (log-timeblock-get-date-list log-timeblock-end-date log-timeblock-visible-days))
  (log-timeblock-render-columns log-timeblock-date-list))

(defun log-timeblock-move-window (days)
  "Move end date D+days for the window, then update. No global aggregation."
  (let* ((cur-time (date-to-time log-timeblock-end-date))
         (new-time (time-add cur-time (days-to-time days))))
    (setq log-timeblock-end-date (format-time-string "%Y-%m-%d %a" new-time))))

(defun log-timeblock-next-day ()
  "Move end date window +1 within date range and update."
  (interactive)
  (log-timeblock-move-window 1)
  (log-timeblock-update-view))

(defun log-timeblock-prev-day ()
  "Move end date window -1 within date range and update."
  (interactive)
  (log-timeblock-move-window -1)
  (log-timeblock-update-view))

(defun log-timeblock-reset ()
  "Reset calendar window to today as the end date."
  (interactive)
  (setq log-timeblock-end-date (format-time-string "%Y-%m-%d %a" (current-time)))
  (log-timeblock-update-view))

(defun log-timeblock-quit ()
  "Quit the main calendar buffer."
  (interactive)
  (kill-buffer log-timeblock-buffer-name))

(defun log-timeblock-calendar ()
  "Entrypoint: Open the timeblock visualization buffer for the window ending at today."
  (interactive)
  (log-timeblock-reset)
  (with-current-buffer log-timeblock-buffer-name
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'log-timeblock-next-day)
      (define-key map (kbd "p") #'log-timeblock-prev-day)
      (define-key map (kbd "f") #'log-timeblock-next-day)
      (define-key map (kbd "b") #'log-timeblock-prev-day)
      (define-key map (kbd "g") #'log-timeblock-reset)
      (define-key map (kbd "q") #'log-timeblock-quit)
      (use-local-map map))))

(provide 'log-timeblock)
;;; log-timeblock.el ends here
