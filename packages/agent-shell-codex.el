;;; agent-shell-codex.el --- Codex specific extensions for agent-shell  -*- lexical-binding: t; -*-

;; Author: Umar Ahmad

;;; Commentary:
;;
;; Provides Codex specific functionality for agent-shell using the
;; local Codex app-server protocol.
;;
;; WARNING: The Codex app-server API used here is experimental.  It is
;; less brittle than editing Codex JSONL files directly, but method
;; names and payloads may still change across Codex releases.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(eval-when-compile
  (require 'cl-lib))

(declare-function agent-shell--current-shell "agent-shell")
(declare-function agent-shell-reload "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell--clean-up "agent-shell")
(declare-function shell-maker-busy "shell-maker")
(declare-function map-elt "map")
(declare-function map-nested-elt "map")

(defgroup agent-shell-codex nil
  "Codex specific extensions for agent-shell."
  :group 'agent-shell
  :prefix "agent-shell-codex-")

(defcustom agent-shell-codex-app-server-command
  '("codex" "app-server" "--listen" "stdio://")
  "Command used to start a local Codex app-server over stdio."
  :type '(repeat string)
  :group 'agent-shell-codex)

(defcustom agent-shell-codex-app-server-timeout 10
  "Seconds to wait for a Codex app-server JSON-RPC response."
  :type 'number
  :group 'agent-shell-codex)

(defvar agent-shell-codex--request-id 0)

(defun agent-shell-codex--next-request-id ()
  "Return the next Codex app-server JSON-RPC request id."
  (cl-incf agent-shell-codex--request-id))

(defun agent-shell-codex--current-shell ()
  "Return the current agent-shell buffer."
  (if (derived-mode-p 'agent-shell-mode)
      (current-buffer)
    (or (agent-shell--current-shell)
        (user-error "Not in a shell"))))

(defun agent-shell-codex--session-id (&optional shell-buffer)
  "Return the Codex session id for SHELL-BUFFER."
  (let* ((buf (or shell-buffer (agent-shell-codex--current-shell)))
         (state (buffer-local-value 'agent-shell--state buf))
         (session-id (map-nested-elt state '(:session :id))))
    (unless session-id
      (user-error "No active session"))
    session-id))

(defun agent-shell-codex--ensure-ready (shell-buffer)
  "Signal an error unless SHELL-BUFFER is ready for history mutation."
  (unless (with-current-buffer shell-buffer
            (if (fboundp 'agent-shell-status)
                (eq (agent-shell-status :shell-buffer shell-buffer) 'ready)
              (not (shell-maker-busy))))
    (user-error "Codex session is busy; try again when the turn finishes")))

(defun agent-shell-codex--json-rpc-request (method &optional params)
  "Return a JSON-RPC request object for METHOD and PARAMS."
  `((id . ,(agent-shell-codex--next-request-id))
    (method . ,method)
    ,@(when params `((params . ,params)))))

(defun agent-shell-codex--send-json-rpc (process request)
  "Send REQUEST to PROCESS as newline-delimited JSON."
  (process-send-string process (concat (json-encode request) "\n")))

(defun agent-shell-codex--parse-json-line (line)
  "Parse LINE as JSON, returning nil on parse failure."
  (condition-case nil
      (json-parse-string line :object-type 'alist :array-type 'list)
    (json-parse-error nil)))

(defun agent-shell-codex--wait-response (process buffer request-id)
  "Wait for REQUEST-ID response from PROCESS using BUFFER."
  (let ((deadline (+ (float-time) agent-shell-codex-app-server-timeout))
        (read-pos (point-min))
        response)
    (with-current-buffer buffer
      (while (and (not response)
                  (< (float-time) deadline))
        (accept-process-output process 0.05)
        (goto-char read-pos)
        (while (and (not response)
                    (not (eobp))
                    (search-forward "\n" nil t))
          (let* ((line (string-trim
                        (buffer-substring-no-properties read-pos (1- (point)))))
                 (obj (and (not (string-empty-p line))
                           (agent-shell-codex--parse-json-line line))))
            (setq read-pos (point))
            (when (and obj (equal (alist-get 'id obj) request-id))
              (setq response obj))))
        (when (and (not response)
                   (not (process-live-p process))
                   (>= read-pos (point-max)))
          (error "Codex app-server exited before response %s" request-id)))
      (unless response
        (error "Timed out waiting for Codex app-server response %s" request-id))
      (when-let ((err (alist-get 'error response)))
        (user-error "Codex app-server error: %s"
                    (or (alist-get 'message err)
                        (format "%S" err))))
      (alist-get 'result response))))

(defmacro agent-shell-codex--with-app-server (&rest body)
  "Start a short-lived Codex app-server and evaluate BODY.
Inside BODY, `process' and `buffer' are bound to the server process and
its output buffer."
  (declare (indent 0) (debug t))
  `(let* ((buffer (generate-new-buffer " *agent-shell-codex-app-server*"))
          (process (make-process
                    :name "agent-shell-codex-app-server"
                    :buffer buffer
                    :command agent-shell-codex-app-server-command
                    :connection-type 'pipe
                    :noquery t)))
     (unwind-protect
         (progn
           (let* ((request (agent-shell-codex--json-rpc-request
                            "initialize"
                            '((clientInfo
                               (name . "agent-shell-codex")
                               (version . "0.1"))
                              (capabilities
                               (experimentalApi . t)))))
                  (request-id (alist-get 'id request)))
             (agent-shell-codex--send-json-rpc process request)
             (agent-shell-codex--wait-response process buffer request-id))
           ,@body)
       (when (process-live-p process)
         (delete-process process))
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(defun agent-shell-codex--request (method params)
  "Synchronously call Codex app-server METHOD with PARAMS."
  (agent-shell-codex--with-app-server
    (agent-shell-codex--request-using process buffer method params)))

(defun agent-shell-codex--request-using (process buffer method params)
  "Synchronously call Codex app-server METHOD with PARAMS.
Use existing app-server PROCESS and BUFFER."
  (let* ((request (agent-shell-codex--json-rpc-request method params))
         (request-id (alist-get 'id request)))
    (agent-shell-codex--send-json-rpc process request)
    (agent-shell-codex--wait-response process buffer request-id)))

(defun agent-shell-codex--turn-text (turn)
  "Return a displayable user message preview for TURN."
  (let* ((items (alist-get 'items turn))
         (user-item (seq-find (lambda (item)
                                (equal (alist-get 'type item) "userMessage"))
                              items))
         (content (alist-get 'content user-item))
         (text-items (seq-filter (lambda (item)
                                   (equal (alist-get 'type item) "text"))
                                 content))
         (text (mapconcat (lambda (item)
                            (or (alist-get 'text item) ""))
                          text-items
                          "\n")))
    (truncate-string-to-width
     (string-trim (replace-regexp-in-string "[\n\r]+" " " text))
     80 nil nil "...")))

(defun agent-shell-codex--list-turns (session-id)
  "Return turns for SESSION-ID ordered oldest first."
  (let* ((result (agent-shell-codex--request
                  "thread/turns/list"
                  `((threadId . ,session-id)
                    (limit . 100)
                    (sortDirection . "desc")
                    (itemsView . "summary"))))
         (turns (alist-get 'data result)))
    (nreverse turns)))

(defun agent-shell-codex--thread-path (thread)
  "Return rollout path from THREAD, or signal a user error."
  (let ((path (alist-get 'path thread)))
    (unless (and (stringp path) (file-exists-p path))
      (user-error "Codex rollout file not found: %S" path))
    path))

(defun agent-shell-codex--turn-start-line (jsonl-path turn-id)
  "Return 1-based line number where TURN-ID starts in JSONL-PATH."
  (let ((line-number 0)
        found)
    (with-temp-buffer
      (insert-file-contents jsonl-path)
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (cl-incf line-number)
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
               (obj (agent-shell-codex--parse-json-line line))
               (payload (alist-get 'payload obj)))
          (when (and (equal (alist-get 'type obj) "event_msg")
                     (equal (alist-get 'type payload) "task_started")
                     (equal (alist-get 'turn_id payload) turn-id))
            (setq found line-number)))
        (forward-line 1)))
    found))

(defun agent-shell-codex--truncate-jsonl-before-turn (jsonl-path turn-id)
  "Truncate JSONL-PATH before TURN-ID.
Creates a backup at JSONL-PATH.bak before truncating."
  (let ((line-before (agent-shell-codex--turn-start-line jsonl-path turn-id))
        (backup (concat jsonl-path ".bak")))
    (unless line-before
      (user-error "Could not find Codex turn %s in %s" turn-id jsonl-path))
    (copy-file jsonl-path backup t)
    (let ((lines (split-string (with-temp-buffer
                                 (insert-file-contents jsonl-path)
                                 (buffer-string))
                               "\n" t)))
      (with-temp-file jsonl-path
        (dolist (line (seq-take lines (1- line-before)))
          (insert line "\n"))))
    backup))

;;;###autoload
(defun agent-shell-codex-rewind ()
  "Rewind the current Codex session to before an earlier turn.
This uses Codex's local app-server `thread/rollback' method and then
reloads the agent-shell buffer so codex-acp reloads the mutated
history."
  (interactive)
  (let* ((shell-buffer (agent-shell-codex--current-shell))
         (session-id (agent-shell-codex--session-id shell-buffer)))
    (agent-shell-codex--ensure-ready shell-buffer)
    (agent-shell-codex--with-app-server
      (let* ((result (agent-shell-codex--request-using
                      process buffer
                      "thread/turns/list"
                      `((threadId . ,session-id)
                        (limit . 100)
                        (sortDirection . "desc")
                        (itemsView . "summary"))))
             (turns (nreverse (alist-get 'data result))))
        (unless (> (length turns) 1)
          (user-error "Not enough turns to rewind (need at least 2)"))
        (let* ((candidates
                (nreverse
                 (cl-loop for turn in (cdr turns)
                          for index from 1
                          collect (cons (format "Turn %d: %s"
                                                index
                                                (agent-shell-codex--turn-text turn))
                                        (list :turn turn :index index)))))
               (candidate-strings (mapcar #'car candidates))
               (selection (completing-read
                           "Rewind to before: "
                           (lambda (string pred action)
                             (if (eq action 'metadata)
                                 '(metadata (display-sort-function . identity))
                               (complete-with-action action candidate-strings string pred)))
                           nil t))
               (selected (cdr (assoc selection candidates)))
               (turn-index (plist-get selected :index))
               (turn-id (alist-get 'id (plist-get selected :turn)))
               (num-turns (- (length turns) turn-index)))
          (when (yes-or-no-p
                 (format "Rewind Codex session to before turn %d? (drops %d turns)"
                         turn-index num-turns))
            ;; `thread/rollback' currently only works on a loaded app-server
            ;; thread, while `thread/turns/list' can read directly from disk.
            (let* ((resume-result
                    (agent-shell-codex--request-using
                     process buffer
                     "thread/resume"
                     `((threadId . ,session-id)
                       (excludeTurns . t))))
                   (jsonl-path
                    (agent-shell-codex--thread-path
                     (alist-get 'thread resume-result)))
                   ;; Resolve this before rollback so a missing physical
                   ;; boundary fails without mutating Codex history.
                   (_line-before
                    (or (agent-shell-codex--turn-start-line jsonl-path turn-id)
                        (user-error "Could not find Codex turn %s in %s"
                                    turn-id jsonl-path))))
            (agent-shell-codex--request-using
             process buffer
             "thread/rollback"
             `((threadId . ,session-id)
               (numTurns . ,num-turns)))
            (agent-shell-codex--truncate-jsonl-before-turn jsonl-path turn-id)
            (with-current-buffer shell-buffer
              (agent-shell-reload))
            (message "Codex session rewound"))))))))

;;;###autoload
(defun agent-shell-codex-archive-this-session ()
  "Archive the current Codex session and kill its agent-shell buffer.
This is Codex's official-ish delete equivalent; it hides the session
from normal session lists without directly deleting JSONL or SQLite
state."
  (interactive)
  (let* ((shell-buffer (agent-shell-codex--current-shell))
         (session-id (agent-shell-codex--session-id shell-buffer)))
    (agent-shell-codex--ensure-ready shell-buffer)
    (when (yes-or-no-p "Archive current Codex session? ")
      (agent-shell-codex--request
       "thread/archive"
       `((threadId . ,session-id)))
      (with-current-buffer shell-buffer
        (agent-shell--clean-up))
      (kill-buffer shell-buffer)
      (message "Archived Codex session %s" session-id))))

(provide 'agent-shell-codex)
;;; agent-shell-codex.el ends here
