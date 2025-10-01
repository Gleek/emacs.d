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

(require 'gptel)
(require 'projectile nil t)  ;; Load projectile optionally
(require 'flycheck nil t)    ;; Load flycheck optionally
(require 'xref)
(require 'elisp-mode)  ; for elisp--xref-backend
(require 'lsp-mode nil t)
(require 'diff nil t)


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

(defun gptel-tool-get-recent-files ()
  "Return a list of recently accessed files from `recentf-list'.
Requires the recentf package to be enabled."
  (if (and (boundp 'recentf-list) recentf-list)
      (mapconcat #'identity recentf-list "\n")
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
         (hash (substring (md5 command) 0 4)))
    (format "gptel-tool-run-%s-%s" project-name hash)))

(defun gptel-tool-run-command (command &optional working-directory)
  (let* ((dir (expand-file-name (or working-directory default-directory) default-directory))
         (id (gptel-tool--run-get-id command dir))
         (buffer (get-buffer-create id))
         proc output status done)
    (with-current-buffer buffer
      (erase-buffer))
    (let ((default-directory dir))
      (setq proc (start-process-shell-command id buffer command)))
    (gptel-tool--record-run-command-history command dir)
    (let ((attempts 20))
      (while (and (not done) (> attempts 0))
        (sit-for 0.5)
        (setq status (process-status proc))
        (when (memq status '(exit signal))
          (setq done t))
        (setq attempts (1- attempts))))
    (with-current-buffer buffer
      (setq output (buffer-string)))
    (setq status (process-status proc))
    (cond
     ((memq status '(exit signal))
      (let* ((exit-code (process-exit-status proc))
             (status-str (if (zerop exit-code)
                             "Status: complete"
                           (format "Status: failed with exit code %d" exit-code))))
        (concat output "\n" status-str)))
     ((eq status 'run)
      (concat
       output
       (format
        "\nStatus: running\nBuffer: %s\nOutput is still being generated, check the buffer `%s' later to see the full output."
        id id))))))



(defun gptel-tool--build-ripgrep-command (query files)
  "Build ripgrep command for QUERY in FILES."
  (format "rg --line-number %s%s"
          (shell-quote-argument query)
          (if (and files (not (string= files "")))
              (format " -g %s" (shell-quote-argument files))
            ""))
)

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
                           (projectile-relevant-known-projects))))
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
         (default-directory search-dir)
         (output-buffer (generate-new-buffer "*gptel-ripgrep*"))
         (cmd (gptel-tool--build-ripgrep-command query files))
         (in-known-project (gptel-tool--in-known-project-p search-dir)))

    (if (and (not in-known-project)
             (not (y-or-n-p
                   (format "Directory %s is not in a known project. Run ripgrep anyway? "
                           search-dir))))
        (funcall callback
                 (format "Search aborted: directory %s is not in known projects"
                         search-dir))

      (with-temp-message (format "Searching for: %s in %s buffer %s"
                                 query search-dir output-buffer)
        (make-process
         :name "gptel-ripgrep"
         :buffer output-buffer
         :command (list shell-file-name shell-command-switch cmd)
         :sentinel (gptel-tool--make-process-sentinel callback))))))


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

(defun gptel-tool-list-projects ()
  "Return a list of all project paths from projectile."
  (require 'projectile)
  (projectile-relevant-known-projects))

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
Returns a list of matching file paths or empty list if no matches
or empty pattern."
  (when (and pattern (not (string-empty-p pattern)))
    (let ((default-directory (if (fboundp 'projectile-project-root)
                                (or (projectile-project-root) default-directory)
                              default-directory)))
      (split-string
       (string-trim
        (shell-command-to-string
         (format "fd -t f --full-path %s" (shell-quote-argument pattern))))
       "\n" t))))

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

(defun gptel-tool-list-buffers()
  "List all open buffers in Emacs."
  (mapconcat #'buffer-name (buffer-list) ", "))

(defun gptel-tool-list-matching-buffers(buffer-name-regex)
  "List all open buffers whose names match BUFFER-NAME-REGEX."
  (let ((matching-buffers (seq-filter
                           (lambda (buf)
                             (string-match-p buffer-name-regex (buffer-name buf)))
                           (buffer-list))))
    (if matching-buffers
        (mapconcat #'buffer-name matching-buffers ", ")
      "No matching buffers found.")))

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
  (letrec ((final-content (with-current-buffer temp-buffer
                            (buffer-substring-no-properties (point-min) (point-max))))
           (cleanup-hook
            (lambda ()
              (remove-hook 'ediff-quit-hook cleanup-hook)
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
                                  final-content)))))))
    (add-hook 'ediff-quit-hook cleanup-hook)
    (ediff-buffers buffer temp-buffer)))

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
           (success t))
      (with-current-buffer temp-buffer
        (dolist (edit sorted-edits)
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
                (setq success nil)
                (funcall callback (format "Could not find '%s' in line %d or anywhere else in the buffer" old-string line-number)))))))
      (if success
          (gptel-tool--compare-and-patch callback buffer temp-buffer)))))

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
   ;; ;; Case 1: Already a buffer object
   ((bufferp (get-buffer buffer-name-or-path)) (get-buffer buffer-name-or-path))

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
   (t
    (let* ((name-regexp (regexp-quote buffer-name-or-path))
           (matching-buffers
            (seq-filter (lambda (buf)
                          (string-match-p name-regexp (buffer-name buf)))
                        (buffer-list))))
      (cond
       ;; Case 5.1: No matches
       ((null matching-buffers)
        nil)

       ;; Case 5.2: Single match
       ((= (length matching-buffers) 1)
        (car matching-buffers))

       ;; Case 5.3: Multiple matches, try to find in project
       (t
        (if (fboundp 'projectile-project-root)
            (let* ((project-root (projectile-project-root))
                   (project-buffers
                    (seq-filter (lambda (buf)
                                  (when project-root
                                    (projectile-project-buffer-p buf project-root)))
                                matching-buffers)))
              (if (= (length project-buffers) 1)
                  (car project-buffers)
                ;; Case 5.4: Fall back to most recent buffer
                (car matching-buffers)))
          ;; Case 5.5: No projectile, just use most recent
          (car matching-buffers))))))))

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
                      "No flycheck errors found in buffer"))
                "Flycheck mode is not enabled in this buffer"))
          (format "Error: Buffer %s not found" buffer-name)))
    (error (format "Error getting flycheck errors: %S" err))))

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

;;;;;;;;;

(gptel-make-tool :name "read_buffer"
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
                 :description "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run. The response will be the output of the command execution with status of the current command. If the status is Running, it will also return a buffer name which can be checked later for the output till that time."
                 :args (list
                        '(:name "command"
                                :type string
                                :description "The complete shell command to execute.")
                        '(:name "working_dir"
                                :type string
                                :description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
                 :category "command"
                 :confirm t
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

(gptel-make-tool :name "read_file"
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
                                      " All commands for the project might be run in that directory.")
                 :category "emacs"
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

(gptel-make-tool :name "find_apropos"
                 :function #'gptel-tool-find-apropos
                 :description "Find all symbols matching pattern in specified buffer using xref-find-apropos. Returns results in file:line: context format. This will be more accurate the running a simple search commands as it will use language heuristics to get the declaration. It also fallbacks to search automatically in case those language heuristics are not present. This is also a general search and only uses the buffer name to focus on the project. Any buffer in the project for that language can be used. This does a fuzzy search mostly and lists all possible matches. Is better than find_definition if we don't have the exact line for the symbol to search for"
                 :args (list '(:name "pattern"
                                     :type string
                                     :description "Pattern to match symbol names")
                             '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer to search in"))
                 :category "emacs"
                 :include t
                 :async t)

(gptel-make-tool :name "list_project_files"
                 :function #'gptel-tool-list-project-files
                 :description "List files in the current project that match a regular expression pattern."
                 :args (list '(:name "pattern"
                                     :type string
                                     :description "Regular expression pattern to match filenames. Empty pattern returns no results."))
                 :category "emacs"
                 :include t)


(gptel-make-tool :name "read_lines"
                 :function #'gptel-tool-read-lines
                 :description "Read lines from a specified buffer and prefix each line with its line number. Always prefer this over reading full buffer but only if you have high confidence that you only need context of specific lines"
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
                 :description "List current Emacs buffers"
                 :args nil
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "list_matching_buffers"
                 :function #'gptel-tool-list-matching-buffers
                 :description "List buffers whose names match a given regular expression. This is useful for finding specific buffers when you have a pattern in mind. Or if read_buffer or similar tools are not working for you. To figure out the correct buffer name."
                 :args (list '(:name "buffer-name-regex"
                                     :type string
                                     :description "The regular expression to match buffer names. Use an empty string to match all buffers."))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "list_visible_buffers"
                 :function #'gptel-tool-list-visible-buffers
                 :description "List currently visible buffers in Emacs, excluding the buffer that made this call. This is a great way to find out what other buffers the user is looking at. Use it if you're unsure about what the user is referring to or user is specifically reffering to some file by \"this\" or similar words."
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
                 :description "Edit an Emacs buffer with a list of edits. Each edit contains:
- line-number: Where to apply the edit
- old-string: Text to replace (can be empty for insertions)
- new-string: Text to insert instead

Prefer this tool for editing files the user is working on as it shows the latest buffer content.
Make multiple small edits IN A SINGLE CALL rather than large block replacements.
If multiple calls are needed, re-read the buffer to get updated line numbers.

Returns (with recommended actions):
- \"Couldn't find '<text>' in line <n>\" - Recheck the text and try again, or break edit into smaller parts
- \"All edits complete\" - Success, no further action needed
- \"Edits NOT accepted by user\" - Provides the diff for unaccepted changes; consider a different approach or consult with the user"
                 :args (list '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer to edit")
                             `(:name "buffer-edits"
                                     :type array
                                     :items (:type object
                                                   :properties
                                                   (:line_number
                                                    (:type integer :description "The line number where edit starts. If text is not found on that line then the whole buffer is searched")
                                                    :old_string
                                                    (:type string
                                                           :description "Text to replace. Can be empty for pure insertions.")
                                                    :new_string
                                                    (:type string :description "Text to insert instead of old-string.")))
                                     :description "List of edits to apply"))
                 :category "emacs"
                 :include t
                 :async t)

(gptel-make-tool :name "list_flycheck_errors"
                 :function #'gptel-tool-list-flycheck-errors
                 :description "Check flycheck errors, warnings and other issues in a buffer. Returns up to 100 errors by default."
                 :args (list '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer to check for flycheck errors."))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "get_imenu"
                 :function #'gptel-tool-get-imenu
                 :description "Get the imenu listings for a buffer. Which is a list of important positions of a buffer.
This could be places where all declarations are present for a buffer -- Global variables, structs, function definitions, etc.
Good to understand relevant portions of the buffer without reading the full buffer"
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
                 :description "Returns a list of files that were recently opened or modified in Emacs. This uses Emacs' built-in file history tracking (recentf) which maintains a list of files the user has interacted with, sorted from most recent to oldest. Each file is returned with its full path. The list updates automatically as users open and save files in Emacs."
                 :args nil
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
                 :description "Evaluates an Emacs Lisp expression and returns the result. Use this to build and evaluate small Lisp programs for calculations, checking conditions, or querying Emacs state. For example: (length (buffer-list)) to count buffers, (buffer-modified-p (get-buffer \"foo.el\")) to check if a buffer is modified, or (+ 1 2 3) for calculations. IMPORTANT: This tool can execute arbitrary code and should be used with caution."
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

(provide 'core-gptel-tools)
;;; core-gptel-tools.el ends here
