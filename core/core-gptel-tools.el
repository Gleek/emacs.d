;;; core-gptel-tools.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Umar Ahmad
;; Created: May 27, 2025
;; Modified: May 27, 2025
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:
;;
;; This library provides GPTel tools for integrating language models
;; seamlessly with Emacs. It registers a suite of commands via `gptel-make-tool'
;; covering buffer operations, file manipulation, diff application,
;; search, and shell command execution.
;; Tools defined here empower GPT-driven workflows inside Emacs.
;;
;;; Code:

(require 'gptel)

(defun gptel-tool-read-buffer(buffer)
  "Read the contents of BUFFER and return it as a string."
  (with-temp-message (format "Reading buffer: %s" buffer)
    (condition-case err
        (if (buffer-live-p (get-buffer buffer))
            (with-current-buffer buffer
              (buffer-substring-no-properties (point-min) (point-max)))
          (format "Error: buffer %s is not live." buffer))
      (error (format "Error reading buffer %s: %s"
                     buffer (error-message-string err))))))

(defun gptel-tool-append-to-buffer (buffer text)
  "Append TEXT to the end of BUFFER. Return a success or error message."
  (with-temp-message (format "Appending to buffer: %s" buffer)
    (condition-case err
        (if (buffer-live-p (get-buffer buffer))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert text)
              (format "Successfully appended text to buffer %s." buffer))
          (format "Error: buffer %s is not live or does not exist." buffer))
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
Creates a buffer with the file contents but doesn't show it."
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
      (goto-char (point-min))
    (forward-line (1- line_number))))

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

(defun gptel-tool-apply-diff (file_path diff_content &optional patch_options working_dir)
  "Apply DIFF_CONTENT to FILE_PATH with PATCH_OPTIONS in WORKING_DIR.
The diff must be in the unified format (output of 'diff -u original_file new_file').
FILE_PATH should be the path to the file that needs to be modified.
PATCH_OPTIONS can include flags like -p0, -p1, etc. to handle path stripping.
WORKING_DIR can specify a different directory to run the patch command from."
  (let ((original-default-directory default-directory)
        (temp-diff-file (make-temp-file "gptel-patch-"))
        (options (if (and patch_options (not (string-empty-p patch_options)))
                     (split-string patch_options " " t)
                   '("-N")))  ;; Default to -N (ignore already applied)
        (exit-status -1)
        (stdout-str "")
        (stderr-str ""))

    (unwind-protect
        (progn
          ;; Change to specified working directory if provided
          (when (and working_dir (not (string-empty-p working_dir)))
            (setq default-directory (expand-file-name working_dir)))

          ;; Expand target file path
          (let ((target-file (expand-file-name file_path)))

            ;; Check that target file exists
            (unless (file-exists-p target-file)
              (error "File to patch does not exist: %s" target-file))

            ;; Write diff content to temporary file
            (with-temp-file temp-diff-file
              (insert diff_content))

            ;; Run patch command
            (with-temp-message (format "Applying diff to: `%s` with options: %s" target-file options)
              (let ((result (apply #'call-process
                                   "patch"                  ;; command
                                   nil                      ;; infile
                                   (list t t)               ;; collect both stdout and stderr
                                   nil                      ;; no display
                                   (append options (list target-file "-i" temp-diff-file)))))
                (setq exit-status result)
                (with-current-buffer standard-output
                  (setq stdout-str (buffer-string)))))

            ;; Return result based on exit status
            (if (eq exit-status 0)
                (format "Patch successfully applied to %s.\nOutput:\n%s"
                        target-file stdout-str)
              (error "Failed to apply patch to %s (exit status %s).\nOutput:\n%s"
                     target-file exit-status stdout-str))))

      ;; Cleanup
      (setq default-directory original-default-directory)
      (when (file-exists-p temp-diff-file)
        (delete-file temp-diff-file)))))

(defun gptel-tool-run-command (command &optional working_dir)
  "Execute shell COMMAND in WORKING_DIR and return the output."
  (with-temp-message (format "Executing command: `%s`" command)
    (let ((default-directory (if (and working_dir (not (string= working_dir "")))
                                 (expand-file-name working_dir)
                               default-directory)))
      (shell-command-to-string command))))

(defun gptel-tool-search-with-ripgrep (callback query files directory)
  "Search for QUERY in FILES within DIRECTORY using ripgrep and call CALLBACK with results."
  (let ((cmd (format "rg --line-number %s %s"
                     (shell-quote-argument query)
                     (if (and files (not (string= files "")))
                         (shell-quote-argument files)
                       "")))
        (default-directory (if (and directory (not (string= directory "")))
                               (expand-file-name directory)
                             default-directory))
        (output-buffer (generate-new-buffer "*gptel-ripgrep*"))
        (in-known-project (and (fboundp 'projectile-relevant-known-projects)
                               (member (expand-file-name default-directory)
                                       (projectile-relevant-known-projects)))))
    (if (and (not in-known-project)
             (not (y-or-n-p (format "Directory %s is not in projectile-relevant-known-projects. Run ripgrep anyway? " default-directory))))
        (progn
          (kill-buffer output-buffer)
          (funcall callback (format "Search aborted: directory %s is not in known projects" default-directory)))
      (with-temp-message (format "Searching for: %s in %s" query directory)
        (make-process
         :name "gptel-ripgrep"
         :buffer output-buffer
         :command (list shell-file-name shell-command-switch cmd)
         :sentinel (lambda (process _event)
                     (when (eq (process-status process) 'exit)
                       (with-current-buffer output-buffer
                         (let ((output (buffer-string)))
                           (funcall callback output)
                           (kill-buffer output-buffer))))))))))

(defun gptel-tool-replace-buffer (buffer_name content)
  "Replace the contents of BUFFER_NAME with CONTENT."
  (with-temp-message (format "Replacing buffer contents: `%s`" buffer_name)
    (if (get-buffer buffer_name)
        (with-current-buffer buffer_name
          (erase-buffer)
          (insert content)
          (format "Buffer contents replaced: %s" buffer_name))
      (format "Error: Buffer '%s' not found" buffer_name))))

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
  (if (buffer-live-p (get-buffer buffer))
      (with-current-buffer buffer
        default-directory)
    (format "Error: Buffer %s is not live." buffer)))

(defun gptel-tool-list-project-files (pattern)
  "List files in project that match PATTERN.
PATTERN is a regular expression to filter files.
Returns a list of matching file paths or empty list if no matches or empty pattern."
  "List files in project that match PATTERN.
PATTERN is a regular expression to filter files.
Returns a list of matching file paths or empty list if no matches or empty pattern."
  (when (and pattern (not (string-empty-p pattern)))
    (require 'projectile)
    (let ((project-root (projectile-project-root))
          (files nil))
      (if project-root
          (setq files (mapcar (lambda (file) (concat project-root file))
                              (seq-filter (lambda (file) (string-match-p pattern file))
                                          (projectile-project-files project-root))))
        (format "Error: Not in a projectile project")))))

;; (defun gptel-tool-apply-diff-to-buffer (diff_content buffer_name &optional working_dir)
;;   "Apply DIFF_CONTENT to BUFFER_NAME interactively.
;; If WORKING_DIR is provided, change to that directory before applying the diff.
;; Shows diff in a temporary buffer and prompts user to apply changes."
;;   (require 'diff-mode)
;;   (let ((original-default-directory default-directory)
;;         (diff-buffer-name "*gptel-diff-preview*"))
;;     (unless (buffer-live-p (get-buffer buffer_name))
;;       (error "Target buffer %s is not live" buffer_name))

;;     (with-current-buffer (get-buffer-create diff-buffer-name)
;;       (erase-buffer)
;;       (insert diff_content)
;;       (diff-mode)
;;       (goto-char (point-min))
;;       (when (and working_dir (not (string-empty-p working_dir)))
;;         (setq default-directory (concat (expand-file-name working_dir) "/")))
;;       (display-buffer (current-buffer)))

;;     (if (y-or-n-p (format "Apply this diff to buffer %s? " buffer_name))
;;         (progn
;;           (with-current-buffer diff-buffer-name
;;             (condition-case err
;;                 (diff-apply-hunk)
;;               (error (format "Error applying diff to buffer %s: %s" buffer_name (error-message-string err))))))
;;       (progn
;;         ;; (kill-buffer diff-buffer-name)
;;         (format "Diff application canceled by user.")))))

;; (defun gptel-tool-apply-diff-to-buffer (diff-content buffer-name &optional working-dir)
;;   "Apply a single unified-diff hunk DIFF-CONTENT to BUFFER-NAME.
;; Recalculate hunk offsets from matching context, show a preview, prompt the user,
;; and on header‐apply failure fall back to a pure context‐based apply."
;;   (require 'diff-mode)
;;   (require 'cl-lib)
;;   (let* ((buf (or (get-buffer buffer-name)
;;                   (when (file-exists-p buffer-name)
;;                     (find-file-noselect buffer-name t))))
;;          (preview-buf (get-buffer-create "*gptel-diff-preview*"))
;;          ;; these will be filled in when parsing the hunk:
;;          context-line
;;          orig-old-start
;;          old-count
;;          real-new-start
;;          new-count)
;;     (unless (buffer-live-p buf)
;;       (error "Target buffer %s is not live or visitable" buffer-name))

;;     ;; Prepare the preview buffer
;;     (with-current-buffer preview-buf
;;       (setq default-directory
;;             (or working-dir
;;                 (with-current-buffer buf default-directory)))
;;       (erase-buffer)
;;       (insert diff-content)
;;       (diff-mode))

;;     ;; Parse the hunk header, extract context and recompute counts & offsets
;;     (with-current-buffer preview-buf
;;       (goto-char (point-min))
;;       (when (re-search-forward
;;              "^@@ -\\([0-9]+\\),[0-9]+ +\\+\\([0-9]+\\),[0-9]+ @@"
;;              nil t)
;;         ;; stash the original old-start
;;         (setq orig-old-start (string-to-number (match-string 1)))
;;         ;; locate the body of this one hunk
;;         (let* ((hunk-beg   (match-beginning 0))
;;                (body-start (progn (forward-line 1) (point)))
;;                (body-end   (progn
;;                              (goto-char body-start)
;;                              (while (and (not (eobp))
;;                                          (not (looking-at "^@@ ")))
;;                                (forward-line 1))
;;                              (point)))
;;                (hunk-lines (split-string
;;                             (buffer-substring-no-properties
;;                              body-start body-end)
;;                             "\n" t)))
;;           ;; first non-+/-/@@ line is our context
;;           (setq context-line
;;                 (cl-find-if
;;                  (lambda (ln)
;;                    (and (not (string-prefix-p "+" ln))
;;                         (not (string-prefix-p "-" ln))
;;                         (not (string-prefix-p "@@" ln))))
;;                  hunk-lines))
;;           ;; recalc counts
;;           (setq old-count
;;                 (cl-count-if (lambda (ln) (not (string-prefix-p "+" ln)))
;;                              hunk-lines)
;;                 new-count
;;                 (cl-count-if (lambda (ln) (not (string-prefix-p "-" ln)))
;;                              hunk-lines))
;;           ;; recalc new-start by searching context in the real buffer
;;           (setq real-new-start
;;                 (with-current-buffer buf
;;                   (goto-char (point-min))
;;                   (if (and context-line
;;                            (search-forward context-line nil t))
;;                       (line-number-at-pos (match-beginning 0))
;;                     ;; fallback to original if not found
;;                     (string-to-number (match-string 2)))))
;;           ;; rewrite header
;;           (goto-char hunk-beg)
;;           (kill-line)
;;           (insert (format "@@ -%d,%d +%d,%d @@"
;;                           orig-old-start old-count
;;                           real-new-start new-count)))))

;;     ;; Show & prompt
;;     (display-buffer preview-buf)
;;     (with-current-buffer preview-buf
;;       (if (y-or-n-p (format "Apply this diff to buffer %s? " buffer-name))
;;           (condition-case err
;;               (diff-apply-hunk)
;;             (error
;;              (message "Header apply failed: %s — retrying by context…" err)
;;              (goto-char (point-min))
;;              (when (and context-line
;;                         (search-forward context-line nil t))
;;                (diff-apply-hunk))))
;;         (message "Diff application canceled")))))

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
      (let ((lines '()))
        (with-current-buffer buffer-name
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
    (error (format "Error: %S" err))))

(defun gptel-tool-replace-lines (buffer-name start-line end-line replacement-string)
  "In BUFFER-NAME, replace lines from START-LINE to END-LINE with REPLACEMENT-STRING.
START-LINE and END-LINE are inclusive line numbers.
Returns a status message indicating success or error."
  (message "Replacing lines %d to %d in buffer=%s"
           start-line end-line buffer-name)
  (condition-case err
      (with-current-buffer buffer-name
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
    (error (format "Error: %S" err))))

(defun gptel-tool-list-buffers()
  "List all open buffers in Emacs."
  (mapconcat #'buffer-name (buffer-list) ", "))

(defun gptel-tool-list-visible-buffers()
  "List all visible buffers in Emacs.
Returns a comma-separated string of buffer names that are currently displayed
in Emacs windows."
  (mapconcat #'(lambda (w)
                 (buffer-name (window-buffer w)))
             (window-list)
             ", "))

(defun gptel-tool-edit-file (file-path file-edits)
  "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing.
  Edit FILE-PATH by applying the edits in FILE-EDITS.
FILE-EDITS should be a list of plists with :line_number, :old_string, and :new_string.
Shows a diff of changes with ediff and returns a status message indicating success or failure."
      (with-current-buffer (get-buffer-create "*edit-file*")
        (insert-file-contents (expand-file-name file-path))
        (let ((inhibit-read-only t)
              (case-fold-search nil)
              (file-name (expand-file-name file-path))
              (edit-success nil))
          ;; apply changes
          (dolist (file-edit (seq-into file-edits 'list))
            (when-let ((line-number (plist-get file-edit :line_number))
                       (old-string (plist-get file-edit :old_string))
                       (new-string (plist-get file-edit :new_string))
                       (is-valid-old-string (not (string= old-string ""))))
              (goto-char (point-min))
              (forward-line (1- line-number))
              (when (search-forward old-string nil t)
                (replace-match new-string t t)
                (setq edit-success t))))
          ;; return result to gptel
          (if edit-success
              (progn
                ;; show diffs
                (ediff-buffers (find-file-noselect file-name) (current-buffer))
                (format "Successfully edited %s" file-name))
            (format "Failed to edited %s" file-name))))
    (format "Failed to edited %s" file-path)))

(defun gptel-tool-read-buffer-with-lines (buffer-name)
  "Read the contents of BUFFER-NAME and return it with line numbers.
Line numbers are prepended to each line, similar to the output of 'cat -n'.
This is useful when you need to refer to specific line numbers in the buffer."
  ;; Take buffer content and put it in a temp file. cat -n that file and return the result as string
  (with-temp-buffer
    (let ((temp-file (make-temp-file "gptel-buffer-")))
      (insert (gptel-tool-read-buffer buffer-name))
      (write-file temp-file)
      (with-temp-buffer
        (insert (shell-command-to-string (format "cat -n %s" temp-file)))
        (buffer-string)))))

(defun gptel-tool-apply-unified-diff (diff-text buffer)
  "Apply unified DIFF-TEXT to BUFFER.
The patch must be standard unified format ("@@ -l,s +l,s @@" hunks).
If a hunk fails to apply cleanly, Ediff will pop up so you can
resolve it interactively—much nicer than blind `patch(1)` failures."
  (let* ((target buffer)
         (patch-buf (generate-new-buffer "*unified-diff*")))
    (with-current-buffer patch-buf
      (insert diff-text)
      (diff-mode))
    (save-excursion
      (ediff-patch-buffer patch-buf target))
    (kill-buffer patch-buf)))


;;;;;;;;;

(gptel-make-tool :name "read_buffer"
                 :function #'gptel-tool-read-buffer
                 :description "Return the contents of an Emacs buffer. This will also give the modified contents if the buffer has been modified but not saved. For the file on disk use read_file."
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
                 :description "Open the file for the user and optionally focus on a specific line number. Potentially disrupts users flow by popping up the window. Use only when the user has specifically asked to open and see the file. In all other cases use open_file_in_background instead. For reading the file use read_file."
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

;; (gptel-make-tool
;;  :name "apply_diff"
;;  :description (concat
;;                "Applies a diff (patch) to a specified file. "
;;                "The diff must be in the unified format (output of 'diff -u original_file new_file'). "
;;                "The LLM should generate the diff such that the file paths within the diff "
;;                "(e.g., '--- a/filename' '+++ b/filename') are appropriate for the 'file_path' argument and chosen 'patch_options'. "
;;                "Common 'patch_options' include: '' (empty, if paths in diff are exact or relative to current dir of file_path), "
;;                "'-p0' (if diff paths are full or exactly match the target including prefixes like 'a/'), "
;;                "'-p1' (if diff paths have one leading directory to strip, e.g., diff has 'a/src/file.c' and you want to patch 'src/file.c' from project root). "
;;                "Default options are '-N' (ignore already applied patches).")
;;  :args (list
;;         '(:name "file_path"
;;                 :type string
;;                 :description "The path to the file that needs to be patched.")
;;         '(:name "diff_content"
;;                 :type string
;;                 :description "The diff content in unified format (e.g., from 'diff -u').")
;;         '(:name "patch_options"
;;                 :type string
;;                 :optional t
;;                 :description "Optional: Additional options for the 'patch' command (e.g., '-p1', '-p0', '-R'). Defaults to '-N'. Prepend other options if needed, e.g., '-p1 -N'.")
;;         '(:name "working_dir"
;;                 :type string
;;                 :optional t
;;                 :description "Optional: The directory in which to interpret file_path and run patch. Defaults to the current buffer's directory if not specified."))
;;  :category "filesystem"
;;  :confirm t
;;  :function #'gptel-tool-apply-diff
;;  :include t)

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

;; (gptel-make-tool :name "apply_diff_to_buffer"
;;                  :function #'gptel-tool-apply-diff-to-buffer
;;                  :description "Apply diff content to a buffer interactively. Shows a preview and prompts for confirmation. Note: final buffer isn’t saved to disk and must be saved manually.
;; When generating diffs for this tool, emit exactly one unified-diff hunk in this format:
;; --- filename
;; +++ filename
;; @@ -oldStart,oldCount +newStart,newCount @@
;; -context line(s)
;; -removed line(s)
;; +added line(s)
;;  context line(s)
;; No extra text."
;;                  :args (list
;;                         '(:name "diff_content"
;;                                 :type string
;;                                 :description "The diff content in unified format (e.g., from 'diff -u')")
;;                         '(:name "buffer_name"
;;                                 :type string
;;                                 :description "The name of the buffer to apply the diff to")
;;                         '(:name "working_dir"
;;                                 :type string
;;                                 :optional t
;;                                 :description "Optional: The directory in which to interpret relative paths in the diff"))
;;                  :category "emacs"
;;                  :confirm t
;;                  :include t)

;; (gptel-make-tool :name "apply_diff_to_buffer"
;;                  :function #'gptel-tool-apply-diff-to-buffer
;;                  :description
;;                  "Apply a single unified-diff hunk to an Emacs buffer by recalculating
;;  hunk offsets from matching context. Opens a preview in *gptel-diff-preview*,
;;  prompts the user, and falls back to pure context matching on header failure."
;;                  :args (list
;;                         ;; Required diff content
;;                         '(:name "diff_content"
;;                                 :type string
;;                                 :description "The diff hunk in unified format (one hunk only).")
;;                         ;; Target buffer (or file path) to apply the diff into
;;                         '(:name "buffer_name"
;;                                 :type string
;;                                 :description "Name of the live buffer or file path to patch.")
;;                         ;; Optional working directory
;;                         '(:name "working_dir"
;;                                 :type string
;;                                 :optional t
;;                                 :description "Directory to use as default-directory when applying."))
;;                  :category "emacs"
;;                  :confirm t
;;                  :include t)

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
                 :include t)

(gptel-make-tool :name "read_lines"
                 :function #'gptel-tool-read-lines
                 :description "Read lines from a specified buffer and prefix each line with its line number."
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


(gptel-make-tool :name "list_buffers"
                 :function #'gptel-tool-list-buffers
                 :description "List current Emacs buffers"
                 :args nil
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "list_visible_buffers"
                 :function #'gptel-tool-list-visible-buffers
                 :description "List currently visible buffers in Emacs. This is a great way to find out what the user is talking about. Use it if you're unsure about what the query."
                 :args nil
                 :category "emacs"
                 :include t)


;; (gptel-make-tool :name "edit_file"
;;                  :function #'gptel-tool-edit-file
;;                  :description "Edit file with a list of edits, each edit contains a line-number,
;; a old-string and a new-string, new-string will replace the old-string at the specified line."
;;                  :args (list '(:name "file-path"
;;                                      :type string
;;                                      :description "The full path of the file to edit")
;;                              '(:name "file-edits"
;;                                      :type array
;;                                      :items (:type object
;;                                                    :properties
;;                                                    (:line_number
;;                                                     (:type integer :description "The line number of the file where edit starts.")
;;                                                     :old_string
;;                                                     (:type string
;;                                                            :description (concat "The old-string to be replaced. This can't be blank as this is searched for replacement."
;;                                                                                 " If a new line is to be added where nothing old is available then still have something "
;;                                                                                 "here that you can keep it in the new string as well. For empty buffers use append_to_buffer instead."))
;;                                                     :new_string
;;                                                     (:type string :description "The new-string to replace old-string.")))
;;                                      :description "The list of edits to apply on the file"))
;;                  :category "filesystem"
;;                  :include t
;;                  :confirm t)

(gptel-make-tool :name "read_buffer_with_lines"
                 :function #'gptel-tool-read-buffer-with-lines
                 :description "Read the contents of an Emacs buffer and return it with line numbers. Similar to how cat -n would return. This is useful for debugging or when you want to refer to specific lines in the buffer. Also useful for when you want to refer to specific lines in the buffer to make edits later."
                 :args (list '(:name "buffer"
                                     :type string
                                     :description "The name of the buffer whose contents are to be retrieved"))
                 :category "emacs"
                 :include t)

(gptel-make-tool :name "apply_unified_diff"
                 :function #'gptel-tool-apply-unified-diff
                 :description "Apply a unified diff string to a buffer using Ediff"
                 :args (list '(:name "diff-text"
                                     :type string
                                     :description "The unified diff text to apply.")
                             '(:name "buffer"
                                     :type string
                                     :description "The buffer to apply the diff to."))
                 :category "emacs"
                 :include t)


(provide 'core-gptel-tools)
;;; core-gptel-tools.el ends here
