;;; core-gptel-tools.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Umar Ahmad
;; Created: May 27, 2025
;; Modified: May 27, 2025
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:
;;
;; This library provides GPTel tools for integrating with gptel.
;; It registers a suite of commands via `gptel-make-tool'
;; covering buffer operations, file manipulation, diff application,
;; search, and shell command execution.
;; Tools defined here empower GPT-driven workflows inside Emacs (edit made via new edit_buffer tool).
;;
;;; Code:

(require 'subr-x)

(require 'gptel)
(require 'projectile nil t)  ;; Load projectile optionally
(require 'flycheck nil t)    ;; Load flycheck optionally
(require 'xref)
(require 'elisp-mode)  ; for elisp--xref-backend
(require 'lsp-mode nil t)
(require 'diff nil t)

(require 'gptel-memory nil t)

(defvar gptel-tools-auto-pilot nil
  "If non-nil, don't wait for user prompt where usually prompts are required.")

(defun gptel-tool--make-xref-formatter (callback)
  "Create an xref formatter that will send results to CALLBACK.
Returns a function suitable for use as `xref-show-xrefs-function'."
  (lambda (fetcher _alist)
    (when-let ((xrefs (funcall fetcher)))
      (funcall callback
               (mapconcat
                (lambda (xref)
                  (let* ((loc (xref-item-location xref))
                         (group (xref-location-group loc))
                         ;; Resolve real file path
                         (real-file (file-truename group))
                         ;; Get correct line number
                         (line (or (with-current-buffer (find-file-noselect real-file)
                                     (save-excursion
                                       (goto-char (xref-location-marker loc))
                                       (line-number-at-pos)))
                                   (xref-location-line loc)
                                   0)))
                    ;; Return plain text without properties
                    (substring-no-properties
                     (format "%s:%d: %s"
                             real-file
                             line
                             (xref-item-summary xref)))))
                xrefs
                "\n")))))

(defun gptel-tool-find-definitions (callback symbol buffer-name line-number)
  "Find definitions of SYMBOL in BUFFER-NAME at LINE-NUMBER.
Returns file:line: summary format for each definition via CALLBACK."
  (cond
   ((or (null symbol) (string-empty-p symbol))
    (funcall callback "Error: Symbol cannot be empty"))
   ((< line-number 1)
    (funcall callback "Error: Line number must be positive"))
   ((not (buffer-live-p (gptel-resolve-buffer-name buffer-name)))
    (funcall callback (format "Error: Buffer %s not found" buffer-name)))
   (t
    (let ((buf (gptel-resolve-buffer-name buffer-name)))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line-number))
          (let ((search-result (gptel-tool--smart-text-match symbol)))
            (if (not search-result)
                (funcall callback
                         (format "Error: Symbol '%s' not found on line %d" symbol line-number))
              (when-let* ((backend (or (xref-find-backend)
                                       (user-error "No xref backend found")))
                          (identifier (xref-backend-identifier-at-point backend)))
                (with-temp-message (format "Finding definitions of %s..." symbol)
                  (condition-case err
                      (let* ((formatter (gptel-tool--make-xref-formatter callback))
                             (xref-show-xrefs-function formatter)
                             (xref-show-definitions-function formatter))
                        (xref-find-definitions identifier))
                    (error (funcall callback
                                    (format "Error finding definitions: %s" (error-message-string err)))))))))))))))

(defun gptel-tool-find-references (callback symbol buffer-name line-number)
  "Find all references to SYMBOL in BUFFER-NAME at LINE-NUMBER.
Returns file:line: summary format for each reference via CALLBACK."
  (cond
   ((or (null symbol) (string-empty-p symbol))
    (funcall callback "Error: Symbol cannot be empty"))
   ((< line-number 1)
    (funcall callback "Error: Line number must be positive"))
   ((not (buffer-live-p (gptel-resolve-buffer-name buffer-name)))
    (funcall callback (format "Error: Buffer %s not found" buffer-name)))
   (t
    (let ((buf (gptel-resolve-buffer-name buffer-name)))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line-number))
          (let ((search-result (gptel-tool--smart-text-match symbol)))
            (if (not search-result)
                (funcall callback
                         (format "Error: Symbol '%s' not found on line %d" symbol line-number))
              (when-let* ((backend (or (xref-find-backend)
                                       (user-error "No xref backend found")))
                          (identifier (xref-backend-identifier-at-point backend)))
                (with-temp-message (format "Finding references to %s..." symbol)
                  (condition-case err
                      (let ((xref-show-xrefs-function (gptel-tool--make-xref-formatter callback)))
                        (xref-find-references identifier))
                    (error (funcall callback
                                    (format "Error finding references: %s" (error-message-string err)))))))))))))))


(defun gptel-tool-find-apropos (callback pattern buffer-name)
  "Find all symbols matching PATTERN in BUFFER-NAME using xref-find-apropos.
Returns results in file:line: context format via CALLBACK."
  (cond
   ((or (null pattern) (string-empty-p pattern))
    (funcall callback "Error: Search pattern cannot be empty"))
   ((not (buffer-live-p (gptel-resolve-buffer-name buffer-name)))
    (funcall callback (format "Error: Buffer %s not found" buffer-name)))
   (t
    (let ((buf (gptel-resolve-buffer-name buffer-name)))
      (with-current-buffer buf
        (with-temp-message (format "Finding symbols matching '%s'..." pattern)
          (condition-case err
              (let ((xref-show-xrefs-function (gptel-tool--make-xref-formatter callback)))
                (xref-find-apropos pattern))
            (error (funcall callback
                            (format "Error finding symbols: %s" (error-message-string err)))))))))))

(defun gptel-filter-list-regex (pattern lst)
  "Return list elements in LST that match regexp PATTERN, case-insensitive. If PATTERN is nil/empty, return LST."
  (let ((case-fold-search t))
    (if (and pattern (not (string-empty-p pattern)))
        (cl-remove-if-not (lambda (x) (string-match-p pattern x)) lst)
      lst)))

(defun gptel-tool-get-recent-files (&optional pattern)
  "Return a list of recently accessed files from `recentf-list' optional regexp PATTERN (case-insensitive).
If PATTERN is non-nil/nonempty, only files matching the regexp are included.
Requires the recentf package to be enabled."
  (if (and (boundp 'recentf-list) recentf-list)
      (let ((filtered (gptel-filter-list-regex pattern recentf-list)))
        (if filtered
            (mapconcat #'identity filtered "\n")
          "No files match the pattern."))
    "No recent files found. Make sure recentf-mode is enabled."))

(defun gptel-tool--imenu-flatten (index)
  "Recursively flatten imenu alist INDEX into a list of leaf items."
  (cl-labels
      ((flat (lst)
         (cl-loop for el in lst
                  if (imenu--subalist-p el)
                  append (flat (cdr el))
                  else collect el)))
    (flat index)))

(defun gptel-tool-get-imenu (buffer-name)
  "Return all items of an imenu after recomputing it for BUFFER-NAME.
Get the result as string with line number: text"
  (let* ((buf (gptel-resolve-buffer-name buffer-name))
         (imenu-use-markers t))
    (if (not (buffer-live-p buf))
        (format "Error: Buffer %s which resolved to %s not found" buffer-name buf)
      (with-current-buffer buf
        (let* ((imenu-alist (imenu--truncate-items
                             (save-excursion
                               (without-restriction
                                 (funcall imenu-create-index-function)))))
               (flat-items (gptel-tool--imenu-flatten imenu-alist)))
          (mapconcat
           (lambda (el)
             (let* ((marker (cdr el))
                    (text (car el))
                    (line-num (save-excursion
                                (goto-char marker)
                                (line-number-at-pos)))
                    (stripped-text (substring-no-properties text)))
               (format "%s: %s" line-num stripped-text)))
           flat-items
           "\n"))))))

(defun gptel-tool-read-buffer(buffer)
  "Read the contents of BUFFER and return it as a string."
  (with-temp-message (format "Reading buffer: %s" buffer)
    (condition-case err
        (let ((buf (gptel-resolve-buffer-name buffer)))
          (if (buffer-live-p buf)
              (with-current-buffer buf
                (buffer-substring-no-properties (point-min) (point-max)))
            (format "Error: buffer %s is not live." buffer)))
      (error (format "Error reading buffer %s: %s"
                     buffer (error-message-string err))))))

(defun gptel-tool-append-to-buffer (buffer text)
  "Append TEXT to the end of BUFFER.  Return a success or error message."
  (with-temp-message (format "Appending to buffer: %s" buffer)
    (condition-case err
        (let ((buf (gptel-resolve-buffer-name buffer)))
          (if (buffer-live-p buf)
              (with-current-buffer buf
                (goto-char (point-max))
                (insert text)
                (format "Successfully appended text to buffer %s." buffer))
            (format "Error: buffer %s is not live or does not exist." buffer)))
      (error (format "Error appending to buffer %s: %s"
                     buffer (error-message-string err))))))

(defun gptel-read-documentation (symbol)
  "Read the documentation for SYMBOL, which can be a function or variable."
  (with-temp-message (format "Reading documentation for: %s" symbol)
    (condition-case err
        (let ((sym (intern symbol)))
          (cond
           ((fboundp sym)
            (documentation sym))
           ((boundp sym)
            (documentation-property sym 'variable-documentation))
           (t
            (format "No documentation found for %s" symbol))))
      (error (format "Error reading documentation for %s: %s"
                     symbol (error-message-string err))))))

(defun gptel-tool-echo-message (text)
  "Send TEXT as a message to the *Messages* buffer."
  (with-temp-message (format "Sending message: %s" text)
    (message "%s" text)
    (format "Message sent: %s" text)))

(defun gptel-tool-delete-file (filepath)
  "Delete FILEPATH by moving it to trash.
   Returns a status message indicating success or failure."
  (with-temp-message (format "Moving file to trash: %s" filepath)
    (condition-case err
        (if (file-exists-p filepath)
            (progn
              (delete-file filepath t)
              (format "Successfully moved file to trash: %s" filepath))
          (format "File does not exist: %s" filepath))
      (error (format "Error deleting file %s: %s"
                     filepath (error-message-string err))))))

(defun gptel-tool-create-file (path filename content)
  "Create a file named FILENAME in PATH with CONTENT.
   If the file already exists, returns the buffer name and suggests using edit_buffer."
  (with-temp-message (format "Creating file: %s in %s" filename path)
    (condition-case err
        (let ((full-path (expand-file-name filename path)))
          (if (file-exists-p full-path)
              (let ((buf (find-file-noselect full-path)))
                (format "File already exists. Use edit_buffer with buffer name '%s' to modify it."
                        (buffer-name buf)))
            (with-temp-buffer
              (insert content)
              (write-file full-path)
              (format "Created file %s in %s" filename path))))
      (error (format "Error creating file %s in %s: %s"
                     filename path (error-message-string err))))))

(defun gptel-tool-open-file-in-background (file_name)
  "Open FILE_NAME without displaying it to the user.
For use when intermediary file access is needed without user visibility.
Creates a buffer with file contents but does not display it to the user."
  (if (or (null file_name)
          (string-empty-p file_name)
          (not (file-exists-p file_name)))
      (error "Error: file_name must be a path to an existing file for open_file_in_background")
    (with-temp-message (format "Opening file in background: %s" file_name)
      (condition-case err
          (find-file-noselect file_name t)
        (error (format "Error opening file in background: %s - %s" file_name (error-message-string err)))))))

(defun gptel-tool-open-file-on-line (file_name line_number)
  "Open FILE_NAME in another window and optionally go to LINE_NUMBER.
When LINE_NUMBER is nil or 0, just open the file for viewing.
Otherwise, position cursor at the specified LINE_NUMBER."
  ;; First check if buffer exists
  (if (or (null file_name)
          (string-empty-p file_name)
          (not (file-exists-p file_name)))
      (error "Error: file_name must be a path to an existing file for open_file_on_line")
    (let* ((buf (gptel-resolve-buffer-name file_name)))
      (if (and buf (buffer-live-p buf))
          (switch-to-buffer-other-window buf)
        (find-file-other-window file_name))
      (when (and line_number (not (zerop line_number)))
        (goto-char (point-min))
        (forward-line (1- line_number))))))

(defun gptel-tool-make-directory (parent name)
  "Create directory NAME in PARENT directory."
  (with-temp-message (format "Creating directory: %s in %s" name parent)
    (condition-case err
        (progn
          (make-directory (expand-file-name name parent) t)
          (format "Directory %s created/verified in %s" name parent))
      (error (format "Error creating directory %s in %s: %s"
                     name parent (error-message-string err))))))

(defun gptel-tool-list-directory (directory)
  "List the contents of DIRECTORY."
  (with-temp-message (format "Listing directory: %s" directory)
    (condition-case err
        (mapconcat #'identity
                   (directory-files directory)
                   "\n")
      (error (format "Error listing directory: %s - %s" directory (error-message-string err))))))


(defun gptel-tool--record-run-command-history (command working-dir)
  "Record COMMAND run in WORKING-DIR to history file."
  (let* ((history-file (expand-file-name "gptel-tool-run-command/history" CACHE-DIR))
         (dir (file-name-directory history-file))
         (timestamp (format-time-string "%Y%m%d%H%M%S")))
    (make-directory dir t)
    (with-temp-buffer
      (insert (format "%s\t%s\t%s\n" timestamp (or working-dir "") command))
      (append-to-file (point-min) (point-max) history-file))))

(defun gptel-tool--run-get-id (command working-directory)
  (let* ((project-name
          (or (and (fboundp 'projectile-project-p)
                   (projectile-project-p)
                   (projectile-project-name))
              (file-name-nondirectory (directory-file-name working-directory))))
         (hash (substring (md5 (concat command working-directory)) 0 7)))
    (format "gptel-tool-run-%s-%s" project-name hash)))

(defun gptel-tool--run-command-poll (id callback command buffer proc attempts)
  "Poll the command process and return output and status via CALLBACK."
  (let ((status (and proc (process-status proc)))
        output
        (bufname (buffer-name buffer)))
    (with-current-buffer buffer
      (setq output (substring-no-properties (buffer-string))))
    (cond
     ((memq status '(exit signal))
      (let* ((exit-code (process-exit-status proc))
             (status-str (if (zerop exit-code)
                             "Status: complete"
                           (format "Status: failed with exit code %d" exit-code))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (funcall callback (concat output "\n" status-str))))
     ((> attempts 0)
      ;; Still running, reschedule poll with decremented attempts
      (run-at-time 0.5 nil #'gptel-tool--run-command-poll id callback command buffer proc (1- attempts)))
     (t ;; Timeout reached while still running: report running status once
      (funcall callback (concat
                         output
                         (format
                          "\nStatus: running\nBuffer: %s\nOutput is still being generated, check the buffer `%s', with read_file, later to see the full output."
                          bufname bufname)))))))

(defun gptel-tool-run-command (callback command &optional working-directory)
  (require 'eat)
  (let* ((dir (expand-file-name (or working-directory default-directory) default-directory))
         (id (gptel-tool--run-get-id command dir))
         buffer proc)
    (let ((default-directory dir)
          (eat-kill-buffer-on-exit nil))
      (setq buffer (eat-make id "/bin/bash" nil "-c" command)))
    (setq proc (get-buffer-process buffer))
    (gptel-tool--record-run-command-history command dir)
    (run-at-time 0.5 nil #'gptel-tool--run-command-poll id callback command buffer proc 20)
    nil))

(defun gptel-tool--build-ripgrep-command (query files)
  "Build ripgrep command for QUERY in FILES."
  (format "rg --line-number %s%s"
          (shell-quote-argument query)
          (if (and files (not (string= files "")))
              (format " -g %s" (shell-quote-argument files))
            "")))

(defun gptel-tool--get-search-directory (directory)
  "Get expanded search DIRECTORY or default-directory."
  (if (and directory (not (string= directory "")))
      (expand-file-name directory)
    default-directory))

(defun gptel-tool--in-known-project-p (search-dir)
  "Check if SEARCH-DIR is in a known project."
  (when (fboundp 'projectile-relevant-known-projects)
    (let ((known-projects (seq-filter
                           (lambda (proj)
                             (not (string= (expand-file-name proj)
                                           (expand-file-name "~"))))
                           (projectile-known-projects))))
      (or (member search-dir known-projects)
          (seq-some (lambda (project-dir)
                      (or (string-prefix-p
                           (directory-file-name (expand-file-name project-dir))
                           (expand-file-name search-dir))))
                    known-projects)))))

(defun gptel-tool--make-process-sentinel (callback)
  "Create a process sentinel that calls CALLBACK with results."
  (lambda (proc _event)
    (when (eq (process-status proc) 'exit)
      (let ((buf (process-buffer proc)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((output (buffer-string)))
              (funcall callback output)
              (kill-buffer buf))))))))

(defun gptel-tool-search-with-ripgrep (callback query files directory)
  "Search for QUERY in FILES within DIRECTORY using ripgrep and call CALLBACK with results."
  (let* ((search-dir (gptel-tool--get-search-directory directory))
         (expanded-search-dir (expand-file-name search-dir)))
    (cond
     ((not (file-directory-p expanded-search-dir))
      (funcall callback (format "Error: Directory does not exist: %s" expanded-search-dir)))
     (t
      (let* ((default-directory expanded-search-dir)
             (output-buffer (generate-new-buffer "*gptel-ripgrep*"))
             (cmd (gptel-tool--build-ripgrep-command query files))
             (in-known-project (gptel-tool--in-known-project-p expanded-search-dir)))
        (if (and (not (or in-known-project gptel-tools-auto-pilot))
                 (not (y-or-n-p
                       (format "Directory %s is not in a known projects (%s). Run ripgrep anyway? "
                               expanded-search-dir (length (projectile-relevant-known-projects))))))
            (funcall callback
                     (format "Search aborted: directory %s is not in known projects"
                             expanded-search-dir))
          (with-temp-message (format "Searching for: %s in %s buffer %s"
                                     query expanded-search-dir output-buffer)
            (make-process
             :name "gptel-ripgrep"
             :buffer output-buffer
             :command (list shell-file-name shell-command-switch cmd)
             :sentinel (gptel-tool--make-process-sentinel callback)))))))))


(defun gptel-tool-replace-buffer (buffer_name content)
  "Replace the contents of BUFFER_NAME with CONTENT."
  (with-temp-message (format "Replacing buffer contents: `%s`" buffer_name)
    (let ((buf (gptel-resolve-buffer-name buffer_name)))
      (if (buffer-live-p buf)
          (with-current-buffer buf
            (erase-buffer)
            (insert content)
            (format "Buffer contents replaced: %s" buffer_name))
        (format "Error: Buffer '%s' not found" buffer_name)))))


(defun gptel-tool-read-unified (name-or-path &optional start-line end-line)
  "Read contents from buffer or file. Gets resolved buffer, or opens file if not found. Optionally read from START-LINE to END-LINE (inclusive, 1-based).
If more than 5000 lines, returns only a window of ±2500 lines around point.
If the path is relative and not found, tries to resolve relative to current project root.
Returns a formatted string with buffer metadata (buffer_name, buffer_directory, project, total_lines, start_line, end_line) and content."
  (let ((live-buf (gptel-resolve-buffer-name name-or-path)))
    (if (not (buffer-live-p live-buf))
        (format "Error: Could not resolve buffer or file for '%s'" name-or-path)
      (with-current-buffer live-buf
        (save-excursion
          (let* ((buffer_name (buffer-name))
                 (buffer_directory (or (and (buffer-file-name)
                                            (file-name-directory (buffer-file-name)))
                                       default-directory))
                 (project (when (fboundp 'projectile-project-root)
                            (ignore-errors (projectile-project-root))))
                 (total-lines (count-lines (point-min) (point-max)))
                 (sl (or start-line 1))
                 (el (or end-line total-lines)))
            (setq sl (max 1 sl))
            (setq el (min total-lines el))
            (let ((nlines (1+ (- el sl)))
                  content)
              (cond
               ((<= nlines 5000)
                (goto-char (point-min))
                (forward-line (1- sl))
                (setq content (buffer-substring-no-properties
                               (point)
                               (progn (forward-line nlines) (point)))))
               (t
                (let* ((cur-line (line-number-at-pos (or (point) (point-min))))
                       (low (max 1 (- cur-line 2500)))
                       (high (min total-lines (+ cur-line 2500))))
                  (goto-char (point-min))
                  (forward-line (1- low))
                  (setq content (buffer-substring-no-properties
                                 (point)
                                 (progn (forward-line (1+ (- high low))) (point)))))))
              (format "buffer_name: %s\nbuffer_directory: %s\nproject: %s\ntotal_lines: %d\nstart_line: %d\nend_line: %d\n---\n%s"
                      buffer_name (or buffer_directory "") (or project "") total-lines sl el content))))))))


(defun gptel-tool-read-file (filepath)
  "Read and return the contents of FILEPATH."
  (with-temp-message (format "Reading file: %s" filepath)
    (condition-case err
        (let ((expanded-path (expand-file-name filepath)))
          (if (file-exists-p expanded-path)
              (progn
                (find-file-noselect expanded-path t)
                (with-temp-buffer
                  (insert-file-contents expanded-path)
                  (buffer-string)))
            (format "Error: File does not exist: %s" expanded-path)))
      (error (format "Error reading file: %s - %s" filepath (error-message-string err))))))


(defun gptel-tool-list-projects (&optional pattern)
  "Return a list of all project paths from projectile, filtered by PATTERN (case-insensitive). If PATTERN is nil/empty, return all."
  (require 'projectile)
  (gptel-filter-list-regex pattern (projectile-relevant-known-projects)))

(defun gptel-tool-change-default-directory(dir)
  "Change the default directory for file operations to DIR."
  (let ((expanded-dir (expand-file-name dir)))
    (if (file-directory-p expanded-dir)
        (progn
          (setq default-directory expanded-dir)
          (format "Changed directory to: %s" expanded-dir))
      (format "Error: Directory does not exist: %s" expanded-dir))))

(defun gptel-tool-get-buffer-directory (buffer)
  "Get the directory of the specified BUFFER."
  (let ((buf (gptel-resolve-buffer-name buffer)))
    (if (buffer-live-p buf)
        (with-current-buffer buf
          default-directory)
      (format "Error: Buffer %s is not live." buffer))))

(defun gptel-tool-list-project-files (pattern)
  "List files in project that match PATTERN.
PATTERN is a regular expression to filter files.

Return value is a *string* formatted like:

Project directory: /full/path/to/project
Current directory: /full/path/to/`default-directory'

file1
file2

If PATTERN is nil/empty, return an empty listing (with directories)."
  (let* ((project-root (when (fboundp 'projectile-project-root)
                         (ignore-errors (projectile-project-root))))
         (project-dir (file-truename (or project-root default-directory)))
         (current-dir (file-truename default-directory))
         (default-directory project-dir)
         (files (if (and pattern (not (string-empty-p pattern)))
                    (split-string
                     (string-trim
                      (shell-command-to-string
                       (format "fd -t f --full-path %s" (shell-quote-argument pattern))))
                     "\n" t)
                  nil)))
    (concat
     "Project directory: " project-dir "\n"
     "Current directory: " current-dir "\n\n"
     (mapconcat #'identity files "\n"))))

;; Courtesy: gfredericks
(defun gptel-tool-read-lines (buffer-name start-line end-line)
  "Read lines from BUFFER-NAME between START-LINE and END-LINE, inclusive.
Each line is prefixed with its line number.
Returns the lines as a concatenated string."
  (cond
   ((< start-line 1)
    (format "Error: Start line must be positive, got %d" start-line))
   ((< end-line start-line)
    (format "Error: End line (%d) must be greater than or equal to start line (%d)" end-line start-line))
   ((not (buffer-live-p (gptel-resolve-buffer-name buffer-name)))
    (format "Error: Buffer %s not found" buffer-name))
   (t
    (condition-case err
        (let ((lines '())
              (buf (gptel-resolve-buffer-name buffer-name)))
          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- start-line))
              (while (and (<= start-line end-line) (not (eobp)))
                (let ((line-content (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
                  (push (format "%d: %s" start-line line-content) lines))
                (forward-line 1)
                (setq start-line (1+ start-line)))))
          (mapconcat 'identity (nreverse lines) "\n"))
      (error (format "Error: %S" err))))))

(defun gptel-tool-replace-lines (buffer-name start-line end-line replacement-string)
  "In BUFFER-NAME, replace lines from START-LINE to END-LINE with REPLACEMENT-STRING.
START-LINE and END-LINE are inclusive line numbers.
Returns a status message indicating success or error."
  (message "Replacing lines %d to %d in buffer=%s"
           start-line end-line buffer-name)
  (condition-case err
      (let ((buf (gptel-resolve-buffer-name buffer-name)))
        (if (buffer-live-p buf)
            (with-current-buffer buf
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- start-line))
                (let ((start-point (point)))
                  ;; Go to the beginning of the line after the last line to replace
                  (forward-line (- (1+ end-line) start-line))
                  (let ((end-point (point)))
                    (delete-region start-point end-point)
                    (goto-char start-point)
                    ;; Only add newline if the replacement string doesn't end with one
                    ;; and we're not at the end of the buffer
                    (insert replacement-string)
                    (unless (or (string-suffix-p "\n" replacement-string)
                                (eobp))
                      (insert "\n"))))))
          (error "Buffer %s not found" buffer-name)))
    (error (format "Error: %S" err))))

(defun gptel-tool-list-buffers(&optional pattern)
  "List all open buffers in Emacs filtered by PATTERN (case-insensitive, optional). If PATTERN is nil/empty, return all."
  (mapconcat #'identity (gptel-filter-list-regex pattern (mapcar #'buffer-name (buffer-list))) ", "))

(defun gptel-tool-list-visible-buffers()
  "List all visible buffers in Emacs.
Returns a comma-separated string of buffer names that are currently displayed
in Emacs windows, excluding the buffer that called this function."
  (let* ((current-buf (current-buffer))
         (visible-bufs (delq nil
                             (mapcar (lambda (w)
                                       (let ((buf (window-buffer w)))
                                         (unless (eq buf current-buf)
                                           (buffer-name buf))))
                                     (window-list)))))
    (mapconcat #'identity visible-bufs ", ")))

(defun gptel-tool--smart-text-match (text)
  "Find TEXT in current buffer with flexible whitespace handling.
Tries exact match first, then tries with flexible whitespace matching.
Returns non-nil if a match is found, nil otherwise.
The match can be used with `replace-match' afterwards.

Searching is done in this order:
1. Exact match from current position
2. Search with any leading whitespace from current position
3. From buffer beginning: exact match
4. From buffer beginning: with any leading whitespace"
  (when text
    (or (search-forward text nil t)
        ;; Split into lines, escape each line as regex, then handle whitespace
        (when-let* ((lines (split-string text "\n"))
                    (regex (mapconcat
                            (lambda (line)
                              (concat "\\s-*" (regexp-quote (string-trim-left line))))
                            lines
                            "\n")))
          (or (re-search-forward regex nil t)
              (save-excursion
                (goto-char (point-min))
                (or (search-forward text nil t)
                    (re-search-forward regex nil t))))))))

(defun gptel-tool--copy-buffer-for-edit (buffer)
  "Create a temp buffer with same content and mode as BUFFER.
Assumes BUFFER is valid."
  (when-let ((existing (get-buffer "*edit-file*")))
    (kill-buffer existing))
  (with-current-buffer (get-buffer-create "*edit-file*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (with-current-buffer buffer
                (buffer-substring-no-properties (point-min) (point-max))))
      (funcall (with-current-buffer buffer major-mode))
      (if (eq (with-current-buffer buffer major-mode) 'org-mode)
          (org-show-all)))
    (current-buffer)))

(defun gptel-tool--calculate-diff (content1 content2)
  "Calculate the diff between CONTENT1 and CONTENT2, returning a clean diff as a string."
  (with-temp-buffer
    (let ((buf1 (current-buffer)))
      (insert content1)
      (with-temp-buffer
        (let ((buf2 (current-buffer)))
          (insert content2)
          (with-temp-buffer
            (let ((diff-buf (current-buffer)))
              (diff-no-select buf1 buf2 nil 'noasync diff-buf)
              (with-current-buffer diff-buf
                (goto-char (point-min))
                (forward-line 3)
                (let ((start (point)))
                  (goto-char (point-max))
                  (forward-line -1)
                  (buffer-substring-no-properties start (point)))))))))))

(defun gptel-tool--compare-and-patch (callback buffer temp-buffer)
  "Run ediff between BUFFER and TEMP-BUFFER with cleanup hooks.
Calls CALLBACK when complete. Verifies if all changes were accepted."
  (letrec ((cleanup-hook
            (lambda ()
              (remove-hook 'ediff-quit-hook cleanup-hook)
              (let ((final-content (with-current-buffer temp-buffer
                                     (buffer-substring-no-properties (point-min) (point-max)))))
                ;; First kill the temp buffer since we're done with it
                (when (buffer-live-p temp-buffer)
                  (kill-buffer temp-buffer))
                ;; Then cleanup any ediff-related buffers
                (dolist (buf (buffer-list))
                  (when (and (buffer-live-p buf)
                             (string-match-p "\\*ediff-\\|\\*Ediff" (buffer-name buf)))
                    (ignore-errors (kill-buffer buf))))
                (funcall callback
                         (if (string= (with-current-buffer buffer
                                        (buffer-substring-no-properties (point-min) (point-max)))
                                      final-content)
                             "All edits complete"
                           (concat "Edits NOT accepted by user. Here are the edits that were rejected by the user in diff format:\n"
                                   (gptel-tool--calculate-diff
                                    (with-current-buffer buffer
                                      (buffer-substring-no-properties (point-min) (point-max)))
                                    final-content))))))))
    (add-hook 'ediff-quit-hook cleanup-hook)
    (ediff-buffers buffer temp-buffer)
    (unless (frame-focus-state)
      (alert "GPTel Diff is ready for review" :title "GPTel"))))

(defun gptel-tool--replace-buffer(callback buffer temp-buffer)
  "Replace BUFFER contents with TEMP-BUFFER contents and call CALLBACK."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (cur-point (point)))
      (erase-buffer)
      (insert (with-current-buffer temp-buffer
                (buffer-substring-no-properties (point-min) (point-max))))
      (save-buffer)
      (goto-char cur-point)))
  (funcall callback "All edits complete"))


(defun gptel-tool--flycheck-errors (buffer)
  "Get the list of Flycheck errors as strings for BUFFER (buffer object).
Returns a list of error strings."
  (with-current-buffer buffer
    (let ((max-errors 100)
          errors-list)
      (if (bound-and-true-p flycheck-mode)
          (setq errors-list
                (seq-take
                 (mapcar
                  (lambda (err)
                    (let ((err-type (flycheck-error-level err))
                          (line (flycheck-error-line err))
                          (column (flycheck-error-column err))
                          (message (flycheck-error-message err)))
                      (format "%s at line %d, col %d: %s"
                              (or err-type "unknown")
                              (or line 0)
                              (or column 0)
                              (or message "no message"))))
                  (flycheck-overlay-errors-in (point-min) (point-max)))
                 max-errors))
        (error "Flycheck mode is not enabled in this buffer"))
      errors-list)))

(defun gptel-tool--wait-for-flycheck (cb buffer &optional max-retries)
  "Wait for flycheck to finish in BUFFER (buffer object), always starting with a 0.5s delay.
Calls callback CB when finished. Polls at 0.5s intervals, up to MAX-RETRIES. If flycheck is not enabled, returns immediately."
  (with-current-buffer buffer
    (if (not (bound-and-true-p flycheck-mode))
        (funcall cb)
      (let ((retries (or max-retries 10)))
        (let ((run
               (lambda ()
                 (with-current-buffer buffer
                   (let ((status (when (boundp 'flycheck-last-status-change) flycheck-last-status-change)))
                     (if (and (eq status 'running) (> retries 0))
                         (gptel-tool--wait-for-flycheck cb buffer (1- retries))
                       (funcall cb)))))))
          (run-at-time 0.5 nil run))))))

(defun gptel-tool--append-new-errors (callback buffer edit-fn)
  "Run EDIT-FN with a callback, then append new flycheck errors and call CALLBACK. If flycheck is not available, skip error diff and call as normal."
  (with-current-buffer buffer
    (if (not (bound-and-true-p flycheck-mode))
        (funcall edit-fn callback)
      (let ((old-errors (gptel-tool--flycheck-errors buffer)))
        (funcall edit-fn
                 (lambda (result-str)
                   (gptel-tool--wait-for-flycheck
                    (lambda ()
                      (let* ((after-errors (gptel-tool--flycheck-errors buffer))
                             (new-errors (cl-remove-if (lambda (err) (member err old-errors)) after-errors))
                             (report (concat (if new-errors
                                                 (concat "\n\nNew errors post edit:\n" (mapconcat #'identity new-errors "\n"))
                                               "\n\nNew errors post edit:\nNone")
                                             "\nFor full list use tool `list_errors'")))
                        (funcall callback (concat result-str report))))
                    buffer)))))))

(defun gptel-tool-edit-buffer (callback buffer-name buffer-edits)
  "Edit buffer named BUFFER-NAME by applying BUFFER-EDITS.
Shows ediff and calls CALLBACK when complete."
  (let ((buffer (gptel-resolve-buffer-name buffer-name)))
    (unless buffer
      (error "Buffer does not exist: %s" buffer-name))
    (let* ((temp-buffer (gptel-tool--copy-buffer-for-edit buffer))
           (sorted-edits (sort (seq-into buffer-edits 'list)
                               (lambda (a b)
                                 (let ((ln-a (or (plist-get a :line_number) 0))
                                       (ln-b (or (plist-get b :line_number) 0)))
                                   (> ln-a ln-b)))))
           (success t)
           (fail-msg nil))
      (with-current-buffer temp-buffer
        (dolist (edit sorted-edits)
          (when success
            (let ((line-number (or (plist-get edit :line_number) 0))
                  (old-string (plist-get edit :old_string))
                  (new-string (plist-get edit :new_string)))
              (goto-char (point-min))
              (forward-line (1- line-number))
              (if (string= old-string "")
                  (progn
                    (move-to-column 0)
                    (insert new-string)
                    (when (and (char-after) (= (char-after) ?\n))
                      (insert "\n")))
                (if (gptel-tool--smart-text-match old-string)
                    (replace-match new-string t t)
                  ;; Fail fast: stop processing further edits and do not proceed to ediff.
                  (setq success nil)
                  (setq fail-msg
                        (format "Could not find '%s' in line %d or anywhere else in the buffer. Please use read_file again to read relevant portions"
                                old-string line-number))))))))
      (if (not success)
          (progn
            (when (buffer-live-p temp-buffer)
              (kill-buffer temp-buffer))
            (funcall callback fail-msg))
        (gptel-tool--append-new-errors
         callback buffer
         (lambda (cb)
           (if gptel-tools-auto-pilot
               (gptel-tool--replace-buffer cb buffer temp-buffer)
             (gptel-tool--compare-and-patch cb buffer temp-buffer))))))))


(defun gptel-tool-read-buffer-with-lines (buffer-name)
  "Read the contents of BUFFER-NAME and return it with line numbers.
Line numbers are prepended to each line, similar to the output of `cat -n'.
This is useful when you need to refer to specific line numbers in the buffer."
  (with-temp-buffer
    (let ((temp-file (make-temp-file "gptel-buffer-")))
      (insert (gptel-tool-read-buffer buffer-name))
      (write-file temp-file)
      (with-temp-buffer
        (insert (shell-command-to-string (format "cat -n %s" temp-file)))
        (buffer-string)))))

(defun gptel-tool-count-lines-buffer (buffer-name)
  "Count the number of lines in BUFFER-NAME."
  (with-temp-message (format "Counting lines in buffer: %s" buffer-name)
    (let ((buf (gptel-resolve-buffer-name buffer-name)))
      (if (buffer-live-p buf)
          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (let ((line-count 0))
                (while (not (eobp))
                  (setq line-count (1+ line-count))
                  (forward-line 1))
                line-count)))
        (format "Error: Buffer '%s' not found" buffer-name)))))

(defun gptel-resolve-buffer-name (buffer-name-or-path)
  "Find the correct buffer object for BUFFER-NAME-OR-PATH.

Tries multiple strategies to locate the right buffer:
1. If it looks like a file path, find buffer visiting that file
2. Look for exact buffer name match
3. Find buffers with partial name matches
4. If multiple matches, prefer buffers in current project
5. Fall back to most recently used buffer

Returns the buffer object if found, or nil if no buffer is found."
  (cond
   ;; Case 1: Already a buffer object
   ((bufferp (get-buffer buffer-name-or-path))
     (get-buffer buffer-name-or-path))

   ;; Case 2: Nil or empty string
   ((not (and buffer-name-or-path (stringp buffer-name-or-path)))
     nil)

   ;; ;; Case 3: Exact buffer name match
   ;; ((get-buffer buffer-name-or-path))

   ;; Case 4: Might be a file path
   ((and (string-match-p "/" buffer-name-or-path)
         (file-exists-p buffer-name-or-path))
     (find-file-noselect buffer-name-or-path t))

   ;; Case 5: Try to find matching buffers
   ((let* ((name-regexp (regexp-quote buffer-name-or-path))
           (matching-buffers
            (seq-filter (lambda (buf)
                          (string-match-p name-regexp (buffer-name buf)))
                        (buffer-list))))
      (and matching-buffers (not (null matching-buffers))))
    (let* ((name-regexp (regexp-quote buffer-name-or-path))
           (matching-buffers
            (seq-filter (lambda (buf)
                          (string-match-p name-regexp (buffer-name buf)))
                        (buffer-list))))
      (if (= (length matching-buffers) 1)
           (car matching-buffers)
        (if (fboundp 'projectile-project-root)
            (let* ((project-root (projectile-project-root))
                   (project-buffers
                    (seq-filter (lambda (buf)
                                  (when project-root
                                    (projectile-project-buffer-p buf project-root)))
                                matching-buffers)))
              (if (= (length project-buffers) 1)
                   (car project-buffers)
                 (car matching-buffers)))
          (car matching-buffers)))))
   ;; Case 6: No match, try searching project files
   (t
    (when (and (fboundp 'gptel-tool-list-project-files)
               (fboundp 'find-file-noselect))
      (let* ((matches (gptel-tool-list-project-files buffer-name-or-path)))
        (when (and matches (> (length matches) 0))
          (let* ((file-to-open (car matches))
                   (project-root (when (fboundp 'projectile-project-root) (projectile-project-root)))
                   (full-path (if (and project-root (not (file-name-absolute-p file-to-open)))
                                  (expand-file-name file-to-open project-root)
                                file-to-open)))
             (find-file-noselect full-path t))))))))

(defun gptel-tool-git-log (callback &optional file-path count)
  "Get git log for the current project or FILE-PATH.
Shows last COUNT commits (defaults to 100).
Returns formatted string with commit hash, date, author, and message."
  (let* ((count (or count 100))
         (git-cmd
          (if file-path
              (format "git log -n %d --pretty=format:'%%h||%%ad||%%an||%%s||%%b' --date=short %s"
                      count (shell-quote-argument file-path))
            (format "git log -n %d --pretty=format:'%%h||%%ad||%%an||%%s||%%b' --date=short"
                    count)))
         (git-output (shell-command-to-string git-cmd)))
    (if (string-empty-p git-output)
        (if file-path
            (format "No git history found for file: %s" file-path)
          "No git history found in current directory")
      (mapconcat
       (lambda (line)
         (when (string-match "\\([^|]+\\)||\\([^|]+\\)||\\([^|]+\\)||\\([^|]+\\)||\\(.*\\)" line)
           (format "Commit: %s\nDate: %s\nAuthor: %s\nSubject: %s\nDescription: %s\n"
                   (match-string 1 line)
                   (match-string 2 line)
                   (match-string 3 line)
                   (match-string 4 line)
                   (string-trim (or (match-string 5 line) "")))))
       (split-string git-output "\n")
       "\n"))))

(defun gptel-tool-show-commit (callback commit-hash)
  "Show the changes in the specified COMMIT-HASH.
Returns the diff content showing what changed in that commit."
  (let ((git-cmd (format "git show --color=never %s" (shell-quote-argument commit-hash))))
    (let ((output (shell-command-to-string git-cmd)))
      (if (string-empty-p output)
          (format "No changes found for commit: %s" commit-hash)
        output))))

(defun gptel-tool-list-flycheck-errors (buffer-name)
  "Get flycheck errors for BUFFER-NAME, truncated to maximum 100 errors.
Returns a formatted string with error type, line, column, and message."
  (condition-case err
      (let* ((buf (gptel-resolve-buffer-name buffer-name))
             (max-errors 100)
             (errors-list nil))
        (if (buffer-live-p buf)
            (with-current-buffer buf
              (if (bound-and-true-p flycheck-mode)
                  (progn
                    (setq errors-list
                          (seq-take
                           (mapcar
                            (lambda (err)
                              (let* ((err-type (flycheck-error-level err))
                                     (line (flycheck-error-line err))
                                     (column (flycheck-error-column err))
                                     (message (flycheck-error-message err)))
                                (format "%s at line %d, col %d: %s"
                                        (or err-type "unknown")
                                        (or line 0)
                                        (or column 0)
                                        (or message "no message"))))
                            (flycheck-overlay-errors-in (point-min) (point-max)))
                           max-errors))
                    (if errors-list
                        (mapconcat 'identity errors-list "\n")
                      "No errors found in buffer"))
                "Flycheck mode is not enabled in this buffer. This tool can't be used"))
          (format "Error: Buffer %s not found" buffer-name)))
    (error (format "Error getting errors: %S" err))))

(defun gptel-tool-buffer-details (buffer-name)
  "Get detailed information about a buffer named BUFFER-NAME.
  Returns a formatted string with various buffer properties."
  (let* ((buf (gptel-resolve-buffer-name buffer-name)))
    (if (not (buffer-live-p buf))
        (format "Error: Buffer '%s' not found" buffer-name)
      (with-current-buffer buf
        (let* ((buf-file (buffer-file-name))
               (visible-windows (get-buffer-window-list buf nil t))
               (size (buffer-size))
               (line-count (line-number-at-pos (point-max)))
               (encoding (coding-system-plist buffer-file-coding-system))
               (project-root (and (fboundp 'projectile-project-root)
                                 (projectile-project-root))))
          (format "Buffer: %s\nFile: %s\nVisible: %s\nSize: %d bytes\nLines: %d\nEncoding: %s\nMajor Mode: %s\nProject: %s\nSaved: %s \nDefault Directory: %s"
                  (buffer-name)
                  (or buf-file "none")
                  (if visible-windows "Yes" "No")
                  size
                  line-count
                  (or (plist-get encoding :name) "unknown")
                  major-mode
                  (or project-root "none")
                   (if (buffer-modified-p) "No" "Yes")
                   default-directory))))))

(defun gptel-tool-eval-elisp (elisp-form)
  "Evaluate ELISP-FORM and return the result as a string."
  (condition-case err
      (let ((result (eval (read (format "%s" elisp-form)) t)))
        (format "%S" result))
    (error (format "Error evaluating elisp: %s" (error-message-string err)))))

(defun gptel-tool-delete-directory (directory recursive)
  "Delete DIRECTORY. If RECURSIVE is non-nil, delete contents recursively.
   Move to trash when possible. Returns a status message indicating success or failure."
  (with-temp-message (format "Deleting directory: %s" directory)
    (condition-case err
        (let ((expanded-dir (expand-file-name directory)))
          (if (file-directory-p expanded-dir)
              (progn
                (delete-directory expanded-dir recursive t) ; t means move to trash
                (format "Successfully deleted directory: %s" expanded-dir))
            (format "Directory does not exist: %s" expanded-dir)))
      (error (format "Error deleting directory %s: %s"
                     directory (error-message-string err))))))



(defun gptel-tool--fetch-with-timeout (url url-cb tool-cb failed-msg &rest args)
  "Fetch URL and call URL-CB in the result buffer.

Call TOOL-CB if there is an error or a timeout.  TOOL-CB and ARGS are
passed to URL-CB.  FAILED-MSG is a fragment used for messaging.  Handles
cleanup."
  (let* ((timeout 30) timer done
         (inherit-process-coding-system t)
         (proc-buffer
          (url-retrieve
           url (lambda (status)
                 (setq done t)
                 (when timer (cancel-timer timer))
                 (if-let* ((err (plist-get status :error)))
                     (funcall tool-cb
                              (format "Error: %s failed with error: %S" failed-msg err))
                   (apply url-cb tool-cb args))
                 (kill-buffer (current-buffer)))
           args 'silent)))
    (setq timer
          (run-at-time
           timeout nil
           (lambda (buf cb)
             (unless done
               (setq done t)
               (let ((kill-buffer-query-functions)) (kill-buffer buf))
               (funcall
                cb (format "Error: %s timed out after %d seconds."
                           failed-msg timeout))))
           proc-buffer tool-cb))
    proc-buffer))

;;;; Web search
(defun gptel-tool--shr-next-link ()
  "Jump to the next SHR link in the buffer.  Return jump position."
  (let ((current-prop (get-char-property (point) 'shr-url))
        (next-pos (point)))
    (while (and (not (eobp))
                (setq next-pos
                      (or (next-single-property-change (point) 'shr-url)
                          (point-max)))
                (let ((next-prop (get-char-property next-pos 'shr-url)))
                  (or (equal next-prop current-prop)
                      (equal next-prop nil))))
      (goto-char next-pos))
    (goto-char next-pos)))
(defvar gptel-tool--web-search-active nil)

(defun gptel-tool--web-search-eww (tool-cb query &optional count)
  "Search the web using eww's default search engine (usually DuckDuckGo).

Call TOOL-CB with the results as a string.  QUERY is the search string.
COUNT is the number of results to return (default 5)."
  ;; No more than two active searches at one time
  (require 'eww)
  (setq gptel-tool--web-search-active
        (cl-delete-if-not
         (lambda (buf) (and (buffer-live-p buf)
                       (process-live-p (get-buffer-process buf))))
         gptel-tool--web-search-active))
  (if (>= (length gptel-tool--web-search-active) 2)
      (progn (message "Web search: waiting for turn")
             (run-at-time 5 nil #'gptel-tool--web-search-eww
                          tool-cb query count))
    (push (gptel-tool--fetch-with-timeout
           (concat eww-search-prefix (url-hexify-string query))
           #'gptel-tool--web-search-eww-callback
           tool-cb (format "Web search for \"%s\"" query))
          gptel-tool--web-search-active)))

(defun gptel-tool--web-fix-unreadable ()
  "Replace invalid characters from point to end in current buffer."
  (while (and (skip-chars-forward "\0-\x3fff7f")
              (not (eobp)))
    (display-warning
     '(gptel gptel-tool-tools)
     (format "Invalid character in buffer \"%s\"" (buffer-name)))
    (delete-char 1) (insert "?")))

(defun gptel-tool--web-search-eww-callback (cb)
  "Extract website text and run callback CB with it."
  (let* ((count 5) (results))
    (goto-char (point-min))
    (goto-char url-http-end-of-headers)
    ;; (gptel-tool--web-fix-unreadable)
    (let* ((dom (libxml-parse-html-region (point) (point-max)))
           (result-count 0))
      (eww-score-readability dom)
      ;; (erase-buffer) (buffer-disable-undo)
      (with-temp-buffer
        (shr-insert-document (eww-highest-readability dom))
        (goto-char (point-min))
        (while (and (not (eobp)) (< result-count count))
          (let ((pos (point))
                (url (get-char-property (point) 'shr-url))
                (next-pos (gptel-tool--shr-next-link)))
            (when-let* (((stringp url))
                        (idx (string-search "http" url))
                        (url-fmt (url-unhex-string (substring url idx))))
              (cl-incf result-count)
              (push (concat url-fmt "\n\n"
                            (string-trim
                             (buffer-substring-no-properties pos next-pos))
                            "\n\n----\n")
                    results))))))
    (funcall cb (apply #'concat (nreverse results)))))

;;;; Read URLs
(defun gptel-tool--is-pdf-response-p ()
  "Check if current buffer contains a PDF response.
Returns t if Content-Type header indicates PDF, nil otherwise."
  (goto-char (point-min))
  (re-search-forward "^Content-Type:.*application/pdf" nil t))

(defun gptel-tool--save-pdf-to-temp ()
   "Save PDF content from current buffer to a temporary file.
Returns the path to the temporary file."
  (let ((temp-file (make-temp-file "gptel-pdf-" nil ".pdf")))
    (goto-char url-http-end-of-headers)
    (write-region (point) (point-max) temp-file nil 'silent)
    temp-file))

(defun gptel-tool--extract-pdf-with-pdftotext (pdf-file)
  "Extract text from PDF-FILE using pdftotext with layout preservation.
Returns the extracted text or an error message if pdftotext is not available
or extraction fails."
  (condition-case err
      (let ((output (shell-command-to-string
                     (format "pdftotext -layout %s -" (shell-quote-argument pdf-file)))))
        (if (string-empty-p output)
            "Error: pdftotext produced no output. PDF may be empty or unreadable."
          output))
    (error
     (format "Error: pdftotext not found or failed: %s. Install poppler-utils package."
             (error-message-string err)))))

(defun gptel-tool--read-url (tool-cb url)
  "Fetch URL text and call TOOL-CB with it.

Handles both HTML pages and PDF files:
- For HTML: parses DOM and extracts readable content
- For PDF: saves to temp file and uses pdftotext -layout to extract text"
  (require 'eww)
  (gptel-tool--fetch-with-timeout
   url
   (lambda (cb)
     (goto-char (point-min))
     (forward-paragraph)
     (condition-case errdata
         (if (gptel-tool--is-pdf-response-p)
             ;; PDF handling
             (let ((pdf-file (gptel-tool--save-pdf-to-temp))
                   extracted-text)
               (unwind-protect
                   (progn
                     (setq extracted-text (gptel-tool--extract-pdf-with-pdftotext pdf-file))
                     (funcall cb extracted-text))
                 ;; Cleanup temp file
                 (when (file-exists-p pdf-file)
                   (delete-file pdf-file))))
           ;; HTML handling
           (let ((dom (libxml-parse-html-region (point) (point-max))))
             (with-temp-buffer
               (eww-score-readability dom)
               (shr-insert-document (eww-highest-readability dom))
               (decode-coding-region (point-min) (point-max) 'utf-8)
               (funcall
                cb (buffer-substring-no-properties
                    (point-min) (point-max))))))
       (error (funcall cb (format "Error: Request failed with error data:\n%S"
                                  errdata)))))
   tool-cb (format "Fetch for \"%s\"" url)))


;;;;;;;;;

(gptel-make-tool :name "read_buffer_old"
                 :function #'gptel-tool-read-buffer
                 :description "Return the contents of an Emacs buffer. This will also give the modified contents if the buffer has been modified but not saved. For the file on disk use read_file. Avoid doing this for large buffers. Use read_lines for large buffers instead. To estimate the size of the buffer use count_lines_buffer."
                 :args (list '(:name "buffer"
                                     :type string
                                     :description "The name of the buffer whose contents are to be retrieved"))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "append_to_buffer"
                 :function #'gptel-tool-append-to-buffer
                 :description "Append the given text to the end of an Emacs buffer. Returns a success or error message."
                 :args (list
                        '(:name "buffer"
                                :type string
                                :description "The name of the buffer to append to.")
                        '(:name "text"
                                :type string
                                :description "The text to append to the buffer."))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "read_documentation"
                 :function #'gptel-read-documentation
                 :description "Read the documentation for a given elisp function or variable"
                 :args (list '(:name "name"
                                     :type string
                                     :description "The name of the function or variable whose documentation is to be retrieved"))
                 :category "elisp"
                 :include t)

(gptel-make-tool :name "read_file"
                 :function #'gptel-tool-read-unified
                 :description "Read the contents of a buffer or file (by name or path). Optionally read only a line range (1-based). Output is capped at 5000 lines. Returns basic metadata (buffer/file identity, directory/project when available, total lines, returned range) followed by the content."
                 :args (list '(:name "name-or-path"
                                    :type string
                                    :description "Buffer name or file path")
                               '(:name "start-line"
                                    :type integer
                                    :optional t
                                    :description "Start line (optional)")
                               '(:name "end-line"
                                    :type integer
                                    :optional t
                                    :description "End line (optional)"))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "echo_message"
                 :function #'gptel-tool-echo-message
                 :description "Send a message to the *Messages* buffer"
                 :args (list '(:name "text"
                                     :type string
                                     :description "The text to send to the messages buffer"))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "delete_file"
                 :function #'gptel-tool-delete-file
                 :description "Delete a file by moving it to trash. Requires user confirmation."
                 :args (list '(:name "filepath"
                                     :type string
                                     :description "The full path of the file to delete"))
                 :category "emacs"
                 :confirm t
                 :include t)

(gptel-make-tool :name "create_file"
                 :function #'gptel-tool-create-file
                 :description "Create a new file with the specified content"
                 :args (list '(:name "path"
                                     :type string
                                     :description "The directory where to create the file")
                             '(:name "filename"
                                     :type string
                                     :description "The name of the file to create")
                             '(:name "content"
                                     :type string
                                     :description "The content to write to the file"))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "open_file_in_background"
                 :function #'gptel-tool-open-file-in-background
                 :description "Open a file without displaying it to the user. Creates a buffer with the file contents but doesn't show it."
                 :args (list '(:name "file_name"
                                     :type string
                                     :description "The file to open in the background without displaying"))
                 :category "emacs"
                 :include t)


(gptel-make-tool :name "open_file_on_line"
                 :function #'gptel-tool-open-file-on-line
                 :description "Open the file for the user and optionally focus on a specific line number. Potentially disrupts users flow by popping up the window. Use only when the user has specifically asked to open and see the file and do it once. In all other cases use open_file_in_background instead. For reading the file use read_file. If you want the user to open multiple files, then print the the file name and line number in the org mode link format, e.g. [[file:<full_file_name>::<line_number>][<file_base_name>::<line_number>]]. User will then open the links manually."
                 :args (list '(:name "file_name"
                                     :type string
                                     :description "The file path or buffer name to open. This opens in another window")
                             '(:name "line_number"
                                     :type number
                                     :description "The line number which should be focused. Use 0 or nil to just open the file."))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "make_directory"
                 :function #'gptel-tool-make-directory
                 :description "Create a new directory with the given name in the specified parent directory"
                 :args (list '(:name "parent"
                                     :type string
                                     :description "The parent directory where the new directory should be created, e.g. /tmp")
                             '(:name "name"
                                     :type string
                                     :description "The name of the new directory to create, e.g. testdir"))
                 :category "emacs"
                 :confirm t
                 :include t)

(gptel-make-tool :name "list_directory"
                 :function #'gptel-tool-list-directory
                 :description "List the contents of a given directory"
                 :args (list '(:name "directory"
                                     :type string
                                     :description "The path to the directory to list"))
                 :category "emacs"
                 :include t)


(gptel-make-tool :name "run_command"
                 :function #'gptel-tool-run-command
                 :description "Run a shell command (requires confirmation). Returns output plus a status. If still running, also returns the buffer name so you can read it later with `read_file`."
                 :args (list
                        '(:name "command"
                                :type string
                                :description "The complete shell command to execute.")
                        '(:name "working_dir"
                                :type string
                                :description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
                 :category "command"
                 :confirm t
                 :async t
                 :include t)

(gptel-make-tool :name "search_with_ripgrep"
                 :function #'gptel-tool-search-with-ripgrep
                 :description "Search for text patterns in files using ripgrep (rg). Use this instead of grep for better performance."
                 :args (list
                        '(:name "query"
                                :type string
                                :description "The search pattern to find")
                        '(:name "files"
                                :type string
                                :description "Optional: File patterns to include (e.g., \"*.py\" or leave empty for all files)"
                                :optional t)
                        '(:name "directory"
                                :type string
                                :description "Directory to search in"
                                :optional t))
                 :category "emacs"
                 :async t
                 :include t)

(gptel-make-tool :name "replace_buffer"
                 :function #'gptel-tool-replace-buffer
                 :description "Completely overwrites buffer contents with the provided content. IMPORTANT: This tool will erase all existing content in the specified buffer; user confirmation will be required."
                 :args (list
                        '(:name "buffer_name"
                                :type string
                                :description "The name of the buffer whose contents will be replaced.")
                        '(:name "content"
                                :type string
                                :description "The new content to write to the buffer, replacing all existing content."))
                 :category "emacs"
                 :confirm t
                 :include t)

(gptel-make-tool :name "read_file_old"
                 :function #'gptel-tool-read-file
                 :description "Read and display the contents of a file. This shows the saved file on disk. If the file is edited on buffer read_buffer will need to be used to get the updated content"
                 :args (list '(:name "filepath"
                                     :type string
                                     :description "Path to the file to read. Supports relative paths and ~."))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "list_projects"
                 :function #'gptel-tool-list-projects
                 :description (concat "Returns a list of all the project paths. "
                                      "Every element of the list is in quotes signifies the directory for the project."
                                      " All commands for the project might be run in that directory. Accepts an optional regex pattern to filter results."
                                      " Use this sparingly, when you're unsure of what project is being talked about."
                                      " If user refers to the current project assume you're in the right project "
                                      " and use list_project_files instead to get files directly if needed.")
                 :args (list '(:name "pattern"
                                    :type string
                                    :description "Optional regexp to filter projects. Empty returns all. Case-insensitive." :optional t))
                 :category "emacs"
                 :confirm t
                 :include t)

(gptel-make-tool :name "change_directory"
                 :function #'gptel-tool-change-default-directory
                 :description "Change the current directory for file operations to a specified directory. Should be done before running any file operations that depend on the current directory."
                 :args (list '(:name "dir"
                                     :type string
                                     :description "The new default directory to set. Supports relative paths and ~."))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "get_buffer_directory"
                 :function #'gptel-tool-get-buffer-directory
                 :description "Get the directory of a specified buffer. Returns the directory path or an error message if the buffer is not live."
                 :args (list '(:name "buffer"
                                     :type string
                                     :description "The name of the buffer whose directory is to be retrieved"))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "find_references"
                 :function #'gptel-tool-find-references
                 :description "Find all references to specified symbol on given line in buffer. Returns file:line: content format showing the entire line where the symbol appears, without affecting the user's view or cursor position. This will be more accurate the running a simple search commands as it will use language heuristics to get the references. It also fallbacks to search automatically if language heuristics are not present."
                 :args (list '(:name "symbol"
                                     :type string
                                     :description "The symbol to search for and find references to")
                             '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer containing the symbol")
                             '(:name "line-number"
                                     :type integer
                                     :description "The line number where to search for the symbol"))
                 :category "emacs"
                 :async t
                 :include t)

(gptel-make-tool :name "find_definitions"
                 :function #'gptel-tool-find-definitions
                 :description "Find definitions of specified symbol on given line in buffer. Returns results in file:line: context format via callback. This will be more accurate the running a simple search commands as it will use language heuristics to get the declaration. It also fallbacks to search automatically in case those language heuristics are not present."
                 :args (list '(:name "symbol"
                                     :type string
                                     :description "The symbol to search for and find definitions of")
                             '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer containing the symbol")
                             '(:name "line-number"
                                     :type integer
                                     :description "The line number where to search for the symbol"))
                 :category "emacs"
                 :include t
                 :async t)

(gptel-make-tool :name "search_symbols"
                 :function #'gptel-tool-find-apropos
                 :description "Search for symbol names matching PATTERN across the current codebase (fuzzy/approximate match). Use this when you’re not sure of the exact symbol or its location. Returns matches as =file:line: summary=."
                 :args (list '(:name "pattern"
                                     :type string
                                     :description "Pattern to match symbol names")
                             '(:name "buffer-name"
                                     :type string
                                     :description "Any buffer from the relevant project/language (used to scope the search)"))
                 :category "emacs"
                 :include t
                 :async t)

(gptel-make-tool :name "list_project_files"
                 :function #'gptel-tool-list-project-files
                 :description "List files in the current project matching regex pattern."
                 :args (list '(:name "pattern"
                                     :type string
                                     :description "Regex pattern to match filenames (non empty)."))
                 :category "emacs"
                 :include t)


(gptel-make-tool :name "read_lines"
                 :function #'gptel-tool-read-lines
                 :description "Read lines from a specified buffer and prefix each line with its line number. Always prefer this over reading full buffer but only if you have high confidence that you only need context of specific lines. Use read_buffer if full buffer is needed"
                 :args (list '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer to read from.")
                             '(:name "start-line"
                                     :type integer
                                     :description "The starting line number (inclusive).")
                             '(:name "end-line"
                                     :type integer
                                     :description "The ending line number (inclusive)."))
                 :include t
                 :category "emacs")

(gptel-make-tool :name "replace_lines"
                 :function #'gptel-tool-replace-lines
                 :description "Replace a range of lines in a specified buffer with a provided string."
                 :args (list '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer to modify.")
                             '(:name "start-line"
                                     :type integer
                                     :description "The starting line number to replace.")
                             '(:name "end-line"
                                     :type integer
                                     :description "The ending line number to replace.")
                             '(:name "replacement-string"
                                     :type string
                                     :description "The string to replace the specified lines."))
                 :confirm t
                 :include t
                 :category "emacs")

(gptel-make-tool :name "count_lines_buffer"
                 :function #'gptel-tool-count-lines-buffer
                 :description "Count the number of lines in a specified buffer. This is specially useful when combined with read_lines tool to get specific lines from the bottom of the buffer."
                 :args (list '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer to count lines in."))
                 :category "emacs"
                 :include t)



(gptel-make-tool :name "list_buffers"
                 :function #'gptel-tool-list-buffers
                 :description "List current Emacs buffers. Accepts an optional case-insensitive regex pattern to filter buffer names."
                 :args (list '(:name "pattern"
                                    :type string
                                    :description "Optional regexp to filter buffer names. Empty returns all. Case-insensitive." :optional t))
                 :category "emacs"
                 :confirm t
                 :include t)



(gptel-make-tool :name "list_visible_buffers"
                 :function #'gptel-tool-list-visible-buffers
                 :description "List buffers currently shown in Emacs windows. Useful when user refers to “this/current/here”."
                 :args nil
                 :category "emacs"
                 :include t)


(gptel-make-tool :name "read_buffer_with_lines"
                 :function #'gptel-tool-read-buffer-with-lines
                 :description "Read the contents of an Emacs buffer and return it with line numbers. Similar to how cat -n would return. This is useful for debugging or when you want to refer to specific lines in the buffer. Also useful for when you want to refer to specific lines in the buffer to make edits later."
                 :args (list '(:name "buffer"
                                     :type string
                                     :description "The name of the buffer whose contents are to be retrieved"))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "edit_buffer"
                 :function #'gptel-tool-edit-buffer
                 :description "Edit an Emacs buffer by applying a list of small text edits, then wait for the user to approve the changes.
Each edit object supports:
- line_number (optional): where to start searching (helps disambiguate)
- old_string: text to replace (use \"\" to insert)
- new_string: replacement text

Prefer this tool for editing files the user is actively working on (it uses the latest buffer contents).
Batch multiple small edits in a single call.

Returns:
- \"All edits complete\" on success
- \"Could not find ...\" if an edit didn\"t match
- \"Edits NOT accepted by user\" if the user rejected some changes (includes a diff)
Also appends any new Flycheck errors introduced by the edits (if available)."
                 :args (list '(:name "buffer-name"
                                     :type string
                                     :description "Buffer to edit")
                             `(:name "buffer-edits"
                                     :type array
                                     :items (:type object
                                                   :properties
                                                   (:line_number
                                                    (:type integer :description "Optional line hint (1-based) to disambiguate matches." :optional t)
                                                    :old_string
                                                    (:type string
                                                           :description "Text to replace (\"\" = insert).")
                                                    :new_string
                                                    (:type string :description "Replacement/insert text.")))
                                     :description "Edits (apply in order)"))
                 :category "emacs"
                 :include t
                 :async t)

(gptel-make-tool :name "list_errors"
                 :function #'gptel-tool-list-flycheck-errors
                 :description "Check errors, warnings and other issues in a buffer. Returns up to 100 errors by default."
                 :args (list '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer to check for errors."))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "get_outline"
                 :function #'gptel-tool-get-imenu
                 :description "Return a code outline (symbols/sections) for a buffer, with line numbers."
                 :args (list '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer to get the imenu for."))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "git_log"
                 :function #'gptel-tool-git-log
                 :description "Get git log for current project or specific file. Shows commit hash, date, author, subject and description for each commit."
                 :args (list '(:name "file-path"
                                     :type string
                                     :description "Optional: Path to file to get history for. If not provided, shows project history."
                                     :optional t)
                             '(:name "count"
                                     :type integer
                                     :description "Optional: Number of commits to show (defaults to 100)"
                                     :optional t))
                 :category "git"
                 :async t
                 :include t)

(gptel-make-tool :name "get_recent_files"
                 :function #'gptel-tool-get-recent-files
                 :description "Returns a list of files that were recently opened or modified. The list of files the user has interacted with is sorted from most recent to oldest. Each file is returned with its full path. If a regex PATTERN is given, only files matching it (case-insensitive) are returned. Empty or nil returns the full list."
                 :args (list '(:name "pattern"
                                     :type string
                                     :description "Optional regexp to filter results. Empty returns all. Case-insensitive." :optional t))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "buffer_details"
                 :function #'gptel-tool-buffer-details
                 :description "Get detailed information about a buffer including its name, associated file, visibility status, size, line count, encoding, major mode, project status, and whether it has been saved. Useful for debugging or getting a complete overview of a buffer's state."
                 :args (list '(:name "buffer-name"
                             :type string
                             :description "The name of the buffer to get details for"))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "eval_elisp"
                 :function #'gptel-tool-eval-elisp
                 :description "Evaluates an Emacs Lisp expression and returns the result. Use this to build and evaluate small Lisp programs for calculations, checking conditions, or querying Emacs state. For example: (+ 1 2 3) for calculations. IMPORTANT: This tool can execute arbitrary code and should be used with caution."
                 :args (list '(:name "elisp-form"
                                     :type string
                                     :description "The Emacs Lisp form to evaluate"))
                 :category "elisp"
                 :confirm t
                 :include t)

(gptel-make-tool :name "show_commit"
                 :function #'gptel-tool-show-commit
                 :description "Show the complete diff of changes made in a specific commit."
                 :args (list '(:name "commit-hash"
                                     :type string
                                     :description "The commit hash to show changes for"))
                 :category "git"
                 :async t
                 :include t)

(gptel-make-tool :name "delete_directory"
                 :function #'gptel-tool-delete-directory
                 :description "Delete a directory, with optional recursive deletion. Moves to trash when possible. Requires user confirmation."
                 :args (list '(:name "directory"
                                     :type string
                                     :description "The path of the directory to delete")
                             '(:name "recursive"
                                     :type boolean
                                     :description "Whether to recursively delete the directory and its contents"))
                 :category "emacs"
                 :confirm t
                 :include t)



;; Courtesy: karthink
(gptel-make-tool :name "web_search"
                 :function 'gptel-tool--web-search-eww
                 :description "Search the web for the first five results to a query.  The query can be an arbitrary string.  Returns the top five results from the search engine as a list of plists.  Each object has the keys `:url` and `:excerpt` for the corresponding search result.

This tool uses the Emacs web browser (eww) with its default search engine (typically DuckDuckGo) to perform searches. No API key is required.

If required, consider using the url as the input to the `web_fetch` tool to get the contents of the url.  Note that this might not work as the `web_fetch` tool does not handle javascript-enabled pages."
                 :args '((:name "query"
                                :type string
                                :description "The natural language search query, can be multiple words.")
                         (:name "count"
                                :type integer
                                :description "Number of results to return (default 5)"
                                :optional t))
                 :include t
                 :async t
                 :category "web")

(gptel-make-tool :name "web_fetch"
                 :function #'gptel-tool--read-url
                 :description "Fetch and read the contents of a URL.

- Returns the text of the URL (not HTML) formatted for reading.
- Request times out after 30 seconds."
                 :args '(( :name "url"
                           :type "string"
                           :description "The URL to read"))
                 :async t
                 :include t
                 :category "web")


(provide 'core-gptel-tools)
;;; core-gptel-tools.el ends here
