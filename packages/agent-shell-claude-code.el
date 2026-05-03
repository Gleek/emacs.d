;;; agent-shell-claude-code.el --- Claude Code specific extensions for agent-shell  -*- lexical-binding: t; -*-

;; Author: Umar Ahmad

;;; Commentary:
;;
;; Provides Claude Code specific functionality for agent-shell that
;; relies on Claude Code's internal session storage format.
;;
;; WARNING: This depends on undocumented Claude Code internals
;; (JSONL session files) and may break if the format changes.

;;; Code:

(require 'json)
(eval-when-compile
  (require 'cl-lib))

(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--clean-up "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--current-shell "agent-shell")
(declare-function agent-shell-reload "agent-shell")
(declare-function map-elt "map")
(declare-function map-nested-elt "map")

(defun agent-shell-claude-code--session-jsonl-path (&optional shell-buffer)
  "Return the JSONL file path for the session in SHELL-BUFFER.
SHELL-BUFFER defaults to the current buffer."
  (let* ((buf (or shell-buffer (current-buffer)))
         (state (buffer-local-value 'agent-shell--state buf))
         (session-id (map-nested-elt state '(:session :id)))
         (cwd (buffer-local-value 'default-directory buf)))
    (unless session-id
      (user-error "No active session"))
    (let* ((cwd-clean (directory-file-name (expand-file-name cwd)))
           (cwd-dashed (replace-regexp-in-string "/" "-" cwd-clean))
           (jsonl-dir (expand-file-name cwd-dashed "~/.claude/projects/"))
           (jsonl-file (expand-file-name (concat session-id ".jsonl") jsonl-dir)))
      (unless (file-exists-p jsonl-file)
        (user-error "Session file not found: %s" jsonl-file))
      jsonl-file)))

(defun agent-shell-claude-code--parse-turns (jsonl-path)
  "Parse JSONL-PATH and return a list of prompt turns.
Each turn is an alist with keys :prompt-id, :line-start, :line-end, :preview."
  (let ((lines (split-string (with-temp-buffer
                               (insert-file-contents jsonl-path)
                               (buffer-string))
                             "\n" t))
        (turns nil)
        (current-prompt-id nil)
        (current-preview nil)
        (current-line-start nil))
    (cl-loop for line in lines
             for line-num from 1
             do (let* ((obj (json-parse-string line :object-type 'alist))
                       (type (alist-get 'type obj))
                       (prompt-id (alist-get 'promptId obj))
                       (role (alist-get 'role (alist-get 'message obj)))
                       (is-meta (eq (alist-get 'isMeta obj) t))
                       (content (alist-get 'content (alist-get 'message obj)))
                       (text (if (stringp content)
                                 content
                               (or (alist-get 'text (and (arrayp content)
                                                         (> (length content) 0)
                                                         (aref content 0)))
                                   "")))
                       ;; Strip XML tags to get the actual content
                       (clean-text (when (stringp text)
                                     (string-trim
                                      (replace-regexp-in-string
                                       "[\n\r]+" " "
                                       (replace-regexp-in-string "<[^>]+>" "" text))))))
                  ;; Detect start of a new user prompt turn
                  (when (and (equal type "user")
                             (equal role "user")
                             (not is-meta)
                             prompt-id
                             (not (equal prompt-id current-prompt-id))
                             clean-text
                             (not (string-empty-p clean-text)))
                    ;; Close previous turn
                    (when current-prompt-id
                      (push (list :prompt-id current-prompt-id
                                  :line-start current-line-start
                                  :preview current-preview)
                            turns))
                    ;; Start new turn — but prefer non-XML content for preview
                    (setq current-prompt-id prompt-id
                          current-line-start line-num
                          current-preview
                          (truncate-string-to-width clean-text 80 nil nil "...")))
                  ;; Update preview if we find a better (non-XML) message in same turn
                  (when (and (equal type "user")
                             (equal role "user")
                             (not is-meta)
                             (equal prompt-id current-prompt-id)
                             (stringp text)
                             (not (string-match-p "\\`<" text))
                             (not (string-empty-p (string-trim text))))
                    (setq current-preview
                          (truncate-string-to-width
                           (string-trim (replace-regexp-in-string "[\n\r]+" " " text))
                           80 nil nil "...")))))
    ;; Close last turn
    (when current-prompt-id
      (push (list :prompt-id current-prompt-id
                  :line-start current-line-start
                  :preview current-preview)
            turns))
    (nreverse turns)))

(defun agent-shell-claude-code--truncate-jsonl (jsonl-path line-before)
  "Truncate JSONL-PATH keeping only lines up to LINE-BEFORE (exclusive).
Creates a backup at JSONL-PATH.bak before truncating."
  (let ((backup (concat jsonl-path ".bak")))
    (copy-file jsonl-path backup t)
    (let ((lines (split-string (with-temp-buffer
                                 (insert-file-contents jsonl-path)
                                 (buffer-string))
                               "\n" t)))
      (with-temp-file jsonl-path
        (dolist (line (seq-take lines (1- line-before)))
          (insert line "\n"))))))

;;;###autoload
(defun agent-shell-claude-code-rewind ()
  "Rewind the current Claude Code session to an earlier turn.
Shows a list of user prompts and rewinds the session to just
before the selected one.  The session is then reloaded.

This modifies the JSONL session file on disk and restarts
the session, so Claude loses all context after the rewind point."
  (interactive)
  (let* ((shell-buffer (if (derived-mode-p 'agent-shell-mode)
                           (current-buffer)
                         (or (agent-shell--current-shell)
                             (user-error "Not in a shell"))))
         (jsonl-path (agent-shell-claude-code--session-jsonl-path shell-buffer))
         (turns (agent-shell-claude-code--parse-turns jsonl-path)))
    (unless (> (length turns) 1)
      (user-error "Not enough turns to rewind (need at least 2)"))
    ;; Build candidates: all turns except the first (can't rewind before first prompt)
    (let* ((candidates (nreverse
                        (mapcar (lambda (turn)
                                  (cons (format "Turn %d: %s"
                                                (cl-position turn turns)
                                                (plist-get turn :preview))
                                        turn))
                                (cdr turns))))  ; skip first turn
           (candidate-strings (mapcar #'car candidates))
           (selection (completing-read "Rewind to before: "
                                       (lambda (string pred action)
                                         (if (eq action 'metadata)
                                             '(metadata (display-sort-function . identity))
                                           (complete-with-action action candidate-strings string pred)))
                                       nil t))
           (turn (cdr (assoc selection candidates)))
           (rewind-line (plist-get turn :line-start)))
      ;; Find the line to truncate at: we need to also remove any
      ;; queue-operation lines that precede the user message
      (let ((lines (split-string (with-temp-buffer
                                   (insert-file-contents jsonl-path)
                                   (buffer-string))
                                 "\n" t))
            (truncate-at rewind-line))
        ;; Walk backwards from rewind-line to skip preceding queue-operation entries
        (cl-loop for i downfrom (1- rewind-line) above 0
                 for line = (nth (1- i) lines)
                 for obj = (json-parse-string line :object-type 'alist)
                 while (equal (alist-get 'type obj) "queue-operation")
                 do (setq truncate-at i))
        (when (yes-or-no-p
               (format "Rewind session to before turn %d? (removes %d lines, backup created)"
                       (cl-position turn turns)
                       (- (length lines) (1- truncate-at))))
          ;; Truncate JSONL on disk, then let agent-shell-reload
          ;; restart the buffer/agent against the same session-id
          ;; (it'll re-read the truncated JSONL via session/load).
          (agent-shell-claude-code--truncate-jsonl jsonl-path truncate-at)
          (with-current-buffer shell-buffer
            (agent-shell-reload))
          (message "Session rewound. Backup at %s.bak" jsonl-path))))))

(declare-function agent-shell--session-choice-label "agent-shell")
(declare-function agent-shell--session-selection-columns "agent-shell")
(declare-function agent-shell--session-column-value "agent-shell")
(declare-function agent-shell-cwd "agent-shell")
(declare-function agent-shell--resolve-path "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function acp-make-session-list-request "acp")
(declare-function acp-send-request "acp")

(defun agent-shell-claude-code--project-sessions-dir (&optional directory)
  "Return the Claude Code sessions directory for DIRECTORY.
DIRECTORY defaults to `default-directory'."
  (let* ((cwd (directory-file-name (expand-file-name (or directory default-directory))))
         (cwd-dashed (replace-regexp-in-string "/" "-" cwd)))
    (expand-file-name cwd-dashed "~/.claude/projects/")))

(defun agent-shell-claude-code--delete-session-by-id (session-id)
  "Delete the JSONL file, transcript, and kill any buffer for SESSION-ID."
  (let* ((sessions-dir (agent-shell-claude-code--project-sessions-dir))
         (jsonl-path (expand-file-name (concat session-id ".jsonl") sessions-dir))
         transcript-path)
    (unless (file-exists-p jsonl-path)
      (user-error "Session file not found: %s" jsonl-path))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (buffer-local-value 'agent-shell--state buf))
        (let ((buf-session-id (map-nested-elt
                               (buffer-local-value 'agent-shell--state buf)
                               '(:session :id))))
          (when (equal buf-session-id session-id)
            (setq transcript-path
                  (buffer-local-value 'agent-shell--transcript-file buf))
            (with-current-buffer buf
              (agent-shell--clean-up))
            (kill-buffer buf)))))
    (delete-file jsonl-path)
    (when (and transcript-path (file-exists-p transcript-path))
      (delete-file transcript-path))
    (message "Deleted session %s" session-id)))

;;;###autoload
(defun agent-shell-claude-code-delete-this-session ()
  "Delete the current session's JSONL file and kill the buffer."
  (interactive)
  (let* ((shell-buffer (if (derived-mode-p 'agent-shell-mode)
                           (current-buffer)
                         (or (agent-shell--current-shell)
                             (user-error "Not in a shell"))))
         (state (buffer-local-value 'agent-shell--state shell-buffer))
         (session-id (map-nested-elt state '(:session :id))))
    (unless session-id
      (user-error "No active session"))
    (let* ((jsonl-path (agent-shell-claude-code--session-jsonl-path shell-buffer))
           (transcript-path (buffer-local-value 'agent-shell--transcript-file shell-buffer))
           (turn-count (length (agent-shell-claude-code--parse-turns jsonl-path))))
      (when (yes-or-no-p (format "Delete current session with %d turns?" turn-count))
        (with-current-buffer shell-buffer
          (agent-shell--clean-up))
        (kill-buffer shell-buffer)
        (delete-file jsonl-path)
        (when (and transcript-path (file-exists-p transcript-path))
          (delete-file transcript-path))
        (message "Deleted session %s" session-id)))))

;;;###autoload
(defun agent-shell-claude-code-delete-session ()
  "Delete a Claude Code session's JSONL file.
Uses an existing agent-shell's ACP client to list sessions
with the same format as the agent-shell session picker.
If the session is active in a buffer, that buffer is also killed."
  (interactive)
  (let* ((shell-buffer (or (car (agent-shell-buffers))
                           (user-error "No active agent-shell to query sessions from")))
         (state (buffer-local-value 'agent-shell--state shell-buffer))
         (client (map-elt state :client))
         (cwd (with-current-buffer shell-buffer
                (agent-shell--resolve-path (agent-shell-cwd))))
         (response (acp-send-request
                    :client client
                    :request (acp-make-session-list-request :cwd cwd)
                    :sync t))
         (acp-sessions (append (or (map-elt response 'sessions) '()) nil)))
    (unless acp-sessions
      (user-error "No sessions found"))
    (let* ((columns (agent-shell--session-selection-columns))
           (max-widths (mapcar (lambda (col)
                                 (cons col (apply #'max
                                                  (mapcar (lambda (s)
                                                            (length (agent-shell--session-column-value col s)))
                                                          acp-sessions))))
                               columns))
           (session-choices (mapcar (lambda (s)
                                      (cons (agent-shell--session-choice-label
                                             :acp-session s :max-widths max-widths)
                                            s))
                                    acp-sessions))
           (candidates (mapcar #'car session-choices))
           (this-command 'agent-shell)
           (selection (completing-read "Delete session: "
                                       (lambda (string pred action)
                                         (if (eq action 'metadata)
                                             '(metadata (display-sort-function . identity))
                                           (complete-with-action action candidates string pred)))
                                       nil t))
           (selected (cdr (assoc selection session-choices)))
           (session-id (map-elt selected 'sessionId)))
      (let* ((sessions-dir (agent-shell-claude-code--project-sessions-dir))
             (jsonl-path (expand-file-name (concat session-id ".jsonl") sessions-dir))
             (turn-count (if (file-exists-p jsonl-path)
                             (length (agent-shell-claude-code--parse-turns jsonl-path))
                           0)))
        (when (yes-or-no-p (format "Delete session with %d turns?" turn-count))
          (agent-shell-claude-code--delete-session-by-id session-id))))))

(provide 'agent-shell-claude-code)
;;; agent-shell-claude-code.el ends here
