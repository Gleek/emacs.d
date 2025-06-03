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
  "Create a file named FILENAME in PATH with CONTENT."
  (with-temp-message (format "Creating file: %s in %s" filename path)
    (condition-case err
        (let ((full-path (expand-file-name filename path)))
          (with-temp-buffer
            (insert content)
            (write-file full-path))
          (format "Created file %s in %s" filename path))
      (error (format "Error creating file %s in %s: %s"
                     filename path (error-message-string err))))))

(defun gptel-tool-open-file-in-background (file_name)
  "Open FILE_NAME without displaying it to the user.
For use when intermediary file access is needed without user visibility.
Creates a buffer with file contents but does not display it to the user."
  (with-temp-message (format "Opening file in background: %s" file_name)
    (condition-case err
        (find-file-noselect file_name t)  ; t = nowarn
      (error (format "Error opening file in background: %s - %s"
                     file_name (error-message-string err))))))

(defun gptel-tool-open-file-on-line (file_name line_number)
  "Open FILE_NAME in another window and optionally go to LINE_NUMBER.
When LINE_NUMBER is nil or 0, just open the file for viewing.
Otherwise, position cursor at the specified LINE_NUMBER."
  (find-file-other-window file_name)
  (if (and line_number (not (zerop line_number)))
      (progn (goto-char (point-min))
             (forward-line (1- line_number)))))

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

(defun gptel-tool-run-command (command &optional working_dir)
  "Execute shell COMMAND in WORKING_DIR and return the output."
  (with-temp-message (format "Executing command: `%s`" command)
    (let ((default-directory (if (and working_dir (not (string= working_dir "")))
                                 (expand-file-name working_dir)
                               default-directory)))
      (shell-command-to-string command))))

(defun gptel-tool--build-ripgrep-command (query files)
  "Build ripgrep command for QUERY in FILES."
  (format "rg --line-number %s %s"
          (shell-quote-argument query)
          (if (and files (not (string= files "")))
              (shell-quote-argument files) "")))

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
                      (string-prefix-p
                       (expand-file-name project-dir)
                       search-dir))
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
        (with-temp-buffer
          (insert-file-contents (expand-file-name filepath))
          (buffer-string))
      (error (format "Error reading file: %s - %s" filepath (error-message-string err))))))

(defun gptel-tool-list-projects ()
  "Return a list of all project paths from projectile."
  (require 'projectile)
  (projectile-relevant-known-projects))

(defun gptel-tool-change-default-directory(dir)
  "Change the default directory for file operations to DIR."
  (setq default-directory (expand-file-name dir)))

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
    (require 'projectile)
    (let ((project-root (projectile-project-root)))
      (if project-root
          (mapcar (lambda (file) (concat project-root file))
                  (seq-filter (lambda (file) (string-match-p pattern file))
                              (projectile-project-files project-root)))
        (format "Error: Not in a projectile project")))))

(defun gptel-tool-apply-diff (file_path diff_content &optional patch_options working_dir)
  "Apply DIFF_CONTENT to FILE_PATH with PATCH_OPTIONS in WORKING_DIR.
This is an internal implementation function used by the `apply_diff` tool."
  (let ((original-default-directory default-directory)
        (user-patch-options (if (and patch_options (not (string-empty-p patch_options)))
                                (split-string patch_options " " t)
                              nil))
        ;; Combine user options with -N, ensuring -N is there.
        ;; If user provides -N or --forward, use their version. Otherwise, add -N.
        (base-options '("-N"))
        (effective-patch-options '()))

    (if user-patch-options
        (if (or (member "-N" user-patch-options) (member "--forward" user-patch-options))
            (setq effective-patch-options user-patch-options)
          (setq effective-patch-options (append user-patch-options base-options)))
      (setq effective-patch-options base-options))

    (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
           (err-buf-name (generate-new-buffer-name "*patch-stderr*"))
           (target-file nil)
           (exit-status -1) ; Initialize to a known non-zero value
           (result-output "")
           (result-error ""))
      (unwind-protect
          (progn
            (when (and working_dir (not (string-empty-p working_dir)))
              (setq default-directory (expand-file-name working_dir)))

            (setq target-file (expand-file-name file_path))

            (unless (file-exists-p target-file)
              ;; Use error to signal failure, which gptel should catch.
              (error "File to patch does not exist: %s" target-file))

            (with-temp-message (format "Applying diff to: `%s` with options: %s" target-file effective-patch-options)
              (with-temp-buffer
                (insert diff_content)
                (unless (eq (char-before (point-max)) ?\n)
                  (goto-char (point-max))
                  (insert "\n"))

                ;; Pass buffer *names* to call-process-region
                (setq exit-status (apply #'call-process-region
                                         (point-min) (point-max)
                                         "patch"       ; Command
                                         nil           ; delete region (no)
                                         (list out-buf-name err-buf-name) ; stdout/stderr buffer names
                                         nil           ; display (no)
                                         (append effective-patch-options (list target-file))))))

            ;; Retrieve content from buffers using their names
            (let ((stdout-buf (get-buffer out-buf-name))
                  (stderr-buf (get-buffer err-buf-name)))
              (when stdout-buf
                (with-current-buffer stdout-buf
                  (setq result-output (buffer-string))))
              (when stderr-buf
                (with-current-buffer stderr-buf
                  (setq result-error (buffer-string)))))

            (if (= exit-status 0)
                (format "Diff successfully applied to %s.\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
                        target-file effective-patch-options result-output result-error)
              ;; Signal an Elisp error, which gptel will catch and display.
              ;; The arguments to 'error' become the error message.
              (error "Failed to apply diff to %s (exit status %s).\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
                     target-file exit-status effective-patch-options result-output result-error)))
        ;; Cleanup clause of unwind-protect
        (setq default-directory original-default-directory)
        (let ((stdout-buf-obj (get-buffer out-buf-name))
              (stderr-buf-obj (get-buffer err-buf-name)))
          (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj))
          (when (buffer-live-p stderr-buf-obj) (kill-buffer stderr-buf-obj)))))))

;; Courtesy: gfredericks
(defun gptel-tool-read-lines (buffer-name start-line end-line)
  "Read lines from BUFFER-NAME between START-LINE and END-LINE, inclusive.
Each line is prefixed with its line number.
Returns the lines as a concatenated string."
  (condition-case err
      (let ((lines '())
            (buf (gptel-resolve-buffer-name buffer-name)))
        (if (buffer-live-p buf)
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
          (error "Buffer %s not found" buffer-name))
        (mapconcat 'identity (nreverse lines) "\n"))
    (error (format "Error: %S" err))))

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
                ;; NOCOMMIT: I think this has an off-by-one error
                ;; where it deletes too many lines; we might also
                ;; want to support a pure insert, maybe just by
                ;; changing end-line to be exclusive
                (goto-char (point-min))
                (forward-line (1- start-line))
                (let ((start-point (line-beginning-position)))
                  (forward-line (1+ (- end-line start-line)))
                  (let ((end-point (line-end-position)))
                    (delete-region start-point end-point)
                    (goto-char start-point)
                    (insert replacement-string "\n")))))
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
in Emacs windows."
  (mapconcat #'(lambda (w)
                 (buffer-name (window-buffer w)))
             (window-list)
             ", "))

(defun gptel-tool-edit-file (target file-edits &optional target-is-buffer)
  "Edit TARGET by applying a sequence of line-specific edits defined in FILE-EDITS. Each edit specifies a line number and text replacement.

FILE-EDITS should be a list of plists with :line_number, :old_string, and :new_string.
Shows a diff of changes with ediff and returns a status message indicating success or failure."
  (let ((logs (list (format "Starting edit for %s (%d edits)" target (length file-edits)))))
    (with-current-buffer (get-buffer-create "*edit-file*")
      (let* ((inhibit-read-only t)
             (case-fold-search nil)
             (edit-success nil)
             (target-buffer nil)
             (file-dir nil)
             (abs-path nil))
        ;; Handle buffer or file based on target-is-buffer flag
        (if target-is-buffer
            (progn
              (setq target-buffer (get-buffer target))
              (unless target-buffer
                (let ((err-msg (format "Buffer does not exist: %s" target)))
                  (push (format "Error: %s" err-msg) logs)
                  (error "%s" (mapconcat #'identity (reverse logs) "\n"))))
              (setq file-dir (with-current-buffer target-buffer default-directory))
              (push (format "Using buffer: %s with directory: %s" target file-dir) logs))
          ;; File path handling
          (progn
            (setq abs-path (if (file-name-absolute-p target)
                               target
                             (expand-file-name target default-directory)))
            (setq file-dir (file-name-directory abs-path))
            (push (format "Using directory: %s for file: %s" file-dir abs-path) logs)))

        (setq default-directory file-dir)

        ;; Clear the existing buffer content
        (erase-buffer)

        ;; For file targets, check if file exists
        (when (and (not target-is-buffer) (not (file-exists-p abs-path)))
          (let ((err-msg (format "File does not exist: %s (resolved from %s)" abs-path target)))
            (push (format "Error: %s" err-msg) logs)
            (error "%s" (mapconcat #'identity (reverse logs) "\n"))))

        ;; Load content from the target (file or buffer)
        (if target-is-buffer
            (progn
              (push "Reading buffer contents" logs)
              (insert (with-current-buffer target-buffer
                        (buffer-substring-no-properties (point-min) (point-max))))
              ;; Use the original buffer's major mode
              (let ((target-mode (with-current-buffer target-buffer major-mode)))
                (funcall target-mode)
                (push (format "Using mode: %s from buffer" major-mode) logs)))
          (progn
            (push "Reading file contents" logs)
            (insert-file-contents abs-path)
            ;; Set major mode to match the original file
            (let ((buffer-file-name abs-path)) ; Temporarily set buffer-file-name to help mode detection
              (set-auto-mode)
              (setq buffer-file-name nil))
            (push (format "Using mode: %s from file" major-mode) logs)))

        ;; Sort edits by line number in descending order to avoid line number shifting
        (let ((sorted-edits (sort (seq-into file-edits 'list)
                                  (lambda (a b) (> (plist-get a :line_number)
                                                   (plist-get b :line_number))))))

          (push (format "Applying %d edits" (length sorted-edits)) logs)

          ;; Apply changes from bottom to top to preserve line numbers
          (dolist (file-edit sorted-edits)
            (when-let ((line-number (plist-get file-edit :line_number))
                       (old-string (plist-get file-edit :old_string))
                       (new-string (plist-get file-edit :new_string)))
              (push (format "Processing edit at line %d" line-number) logs)
              (goto-char (point-min))
              (forward-line (1- line-number))
              (let ((line-content (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))
                (push (format "Line content: %s" line-content) logs)
                (if (string= old-string "")
                    ;; For empty old string, just insert the new string at the line position
                    (progn
                      (push (format "Inserting new content at line %d" line-number) logs)
                      (move-to-column 0)
                      (insert new-string)
                      (when (= (char-after) ?\n)
                        (insert "\n"))
                      (setq edit-success t))
                  ;; For non-empty old string, replace it with new string
                  ;; Don't restrict search to line-end-position to support multi-line strings
                  (if (search-forward old-string nil t)
                      (progn
                        (push (format "Replacing '%s' with '%s'" old-string new-string) logs)
                        (replace-match new-string t t)
                        (setq edit-success t))
                    (push (format "Warning: Could not find '%s' in line %d"
                                  old-string line-number) logs))))))

          ;; Show diffs and return result
          (if edit-success
              (progn
                (push "Successfully applied edits" logs)
                ;; Set up cleanup of temporary buffer when ediff quits
                ;; Set up global hook to clean up the edit buffer
                (defun gptel-tool--cleanup-edit-file-buffer ()
                  "Clean up the *edit-file* buffer after ediff sessions."
                  (when (get-buffer "*edit-file*")
                    (kill-buffer "*edit-file*")))

                ;; Add to both hooks to maximize chances of cleanup
                (add-hook 'ediff-quit-hook #'gptel-tool--cleanup-edit-file-buffer)
                (add-hook 'ediff-cleanup-hook #'gptel-tool--cleanup-edit-file-buffer)

                ;; Start ediff session
                (if target-is-buffer
                    (ediff-buffers target-buffer (current-buffer))
                  (ediff-buffers (find-file-noselect abs-path) (current-buffer)))
                ;; (kill-buffer (current-buffer))
                (mapconcat #'identity (reverse logs) "\n"))
            (let ((err-msg (format "Failed to edit %s" (if target-is-buffer target abs-path))))
              (push err-msg logs)
              (error "%s" (mapconcat #'identity (reverse logs) "\n")))))))))

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

Returns the buffer object if found, nil otherwise."
  (cond
   ;; ;; Case 1: Already a buffer object
   ;; ((bufferp buffer-name-or-path) buffer-name-or-path)

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
                 :description "Read the documentation for a given function or variable"
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
                 :category "filesystem"
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
                 :category "filesystem"
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
                 :description "Open the file for the user and optionally focus on a specific line number. Potentially disrupts users flow by popping up the window. Use only when the user has specifically asked to open and see the file and do it once. In all other cases use open_file_in_background instead. For reading the file use read_file."
                 :args (list '(:name "file_name"
                                     :type string
                                     :description "The file to open. This file opens in another window")
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
                 :category "filesystem"
                 :confirm t
                 :include t)

(gptel-make-tool :name "list_directory"
                 :function #'gptel-tool-list-directory
                 :description "List the contents of a given directory"
                 :args (list '(:name "directory"
                                     :type string
                                     :description "The path to the directory to list"))
                 :category "filesystem"
                 :include t)


(gptel-make-tool :name "run_command"
                 :function #'gptel-tool-run-command
                 :description "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run."
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
                 :category "search"
                 :async t
                 :include t)

(gptel-make-tool :name "ReplaceBuffer"
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
                 :category "filesystem"
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
                 :category "filesystem"
                 :include t)

(gptel-make-tool :name "get_buffer_directory"
                 :function #'gptel-tool-get-buffer-directory
                 :description "Get the directory of a specified buffer. Returns the directory path or an error message if the buffer is not live."
                 :args (list '(:name "buffer"
                                     :type string
                                     :description "The name of the buffer whose directory is to be retrieved"))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "list_project_files"
                 :function #'gptel-tool-list-project-files
                 :description "List files in the current project that match a regular expression pattern."
                 :args (list '(:name "pattern"
                                     :type string
                                     :description "Regular expression pattern to match filenames. Empty pattern returns no results."))
                 :category "project"
                 :include t)


(gptel-make-tool :name "apply_diff"
                 :description (concat
                               "Applies a diff (patch) to a specified file. "
                               "USE THIS TOOL FOR ALL LLMs EXCEPT GEMINI (use apply_diff_fenced for Gemini). "
                               "The diff must be in the unified format (output of 'diff -u original_file new_file'). "
                               "The LLM should generate the diff such that the file paths within the diff "
                               "(e.g., '--- a/filename' '+++ b/filename') are appropriate for the 'file_path' argument and chosen 'patch_options'. "
                               "Common 'patch_options' include: '' (empty, if paths in diff are exact or relative to current dir of file_path), "
                               "'-p0' (if diff paths are full or exactly match the target including prefixes like 'a/'), "
                               "'-p1' (if diff paths have one leading directory to strip, e.g., diff has 'a/src/file.c' and you want to patch 'src/file.c' from project root). "
                               "Default options are '-N' (ignore already applied patches).")
                 :args (list
                        '(:name "file_path"
                                :type string
                                :description "The path to the file that needs to be patched.")
                        '(:name "diff_content"
                                :type string
                                :description "The diff content in unified format (e.g., from 'diff -u').")
                        '(:name "patch_options"
                                :type string
                                :optional t
                                :description "Optional: Additional options for the 'patch' command (e.g., '-p1', '-p0', '-R'). Defaults to '-N'. Prepend other options if needed, e.g., '-p1 -N'.")
                        '(:name "working_dir"
                                :type string
                                :optional t
                                :description "Optional: The directory in which to interpret file_path and run patch. Defaults to the current buffer's directory if not specified."))
                 :category "filesystem"
                 :function #'gptel-tool-apply-diff
                 :confirm t
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
                 :description "List currently visible buffers in Emacs. This is a great way to find out what the user is talking about. Use it if you're unsure about what the query."
                 :args nil
                 :category "emacs"
                 :include t)


(gptel-make-tool :name "edit_file"
                 :function (lambda (file-path file-edits)
                             (gptel-tool-edit-file file-path file-edits nil))
                 :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line. Change directory before running this tool using change_directory if needed."
                 :args (list '(:name "file-path"
                                     :type string
                                     :description "The full path of the file to edit")
                             `(:name "file-edits"
                                     :type array
                                     :items (:type object
                                                   :properties
                                                   (:line_number
                                                    (:type integer :description "The line number of the file where edit starts.")
                                                    :old_string
                                                    (:type string
                                                           :description ,(concat "The old-string to be replaced. This can't be blank as this is searched for replacement."
                                                                                 " If a new line is to be added where nothing old is available then still have something "
                                                                                 "here that you can keep it in the new string as well. For empty buffers use append_to_buffer instead."))
                                                    :new_string
                                                    (:type string :description "The new-string to replace old-string.")))
                                     :description "The list of edits to apply on the file"))
                 :category "filesystem"
                 :include t
                 :confirm t)

(gptel-make-tool :name "read_buffer_with_lines"
                 :function #'gptel-tool-read-buffer-with-lines
                 :description "Read the contents of an Emacs buffer and return it with line numbers. Similar to how cat -n would return. This is useful for debugging or when you want to refer to specific lines in the buffer. Also useful for when you want to refer to specific lines in the buffer to make edits later."
                 :args (list '(:name "buffer"
                                     :type string
                                     :description "The name of the buffer whose contents are to be retrieved"))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "edit_buffer"
                 :function (lambda (buffer-name buffer-edits)
                             (gptel-tool-edit-file buffer-name buffer-edits t))
                 :description "Edit an Emacs buffer with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line.
Similar to edit_file, but operates on buffer contents rather than files on disk."
                 :args (list '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer to edit")
                             `(:name "buffer-edits"
                                     :type array
                                     :items (:type object
                                                   :properties
                                                   (:line_number
                                                    (:type integer :description "The line number of the buffer where edit starts.")
                                                    :old_string
                                                    (:type string
                                                           :description ,(concat "The old-string to be replaced. This can't be blank as this is searched for replacement."
                                                                                 " If a new line is to be added where nothing old is available then still have something "
                                                                                 "here that you can keep it in the new string as well. For empty buffers use append_to_buffer instead."))
                                                    :new_string
                                                    (:type string :description "The new-string to replace old-string.")))
                                     :description "The list of edits to apply on the buffer"))
                 :category "emacs"
                 :include t
                 :confirm t)

(gptel-make-tool :name "list_flycheck_errors"
                 :function #'gptel-tool-list-flycheck-errors
                 :description "Check flycheck errors, warnings and other issues in a buffer. Returns up to 100 errors by default."
                 :args (list '(:name "buffer-name"
                                     :type string
                                     :description "The name of the buffer to check for flycheck errors."))
                 :category "emacs"
                 :include t)

(provide 'core-gptel-tools)
;;; core-gptel-tools.el ends here
