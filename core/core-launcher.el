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
  (setq consult-omni-apps-use-cache t)
  (require 'consult-omni-sources)
  (require 'consult-omni-embark)

  (defvar +launcher-space nil
    "Space identifier for the current launcher invocation.")

  (defvar +launcher-active nil
    "Non-nil while a launcher minibuffer is active.")

  (defun +launcher-quit ()
    "Quit the launcher, including all recursive minibuffers."
    (interactive)
    (top-level))

  (defun +launcher-minibuffer-setup ()
    "Install launcher-specific bindings in the current minibuffer."
    (when +launcher-active
      ;; `abort-recursive-edit' only leaves the innermost Embark prompt.
      ;; `top-level' unwinds Embark and the outer Consult launcher together.
      (local-set-key (kbd "C-g") #'+launcher-quit)))

  (defvar +launcher-frames (make-hash-table :test #'equal)
    "Launcher frames indexed by their macOS Space identifier.")

  (defvar +launcher-sources
    '("Qalc" "Developer" "Static launcher" "Apps" "Org Agenda"
      "Buffer" "Projects" "Browser Tabs" "Browser History"
      "Agent" "Web search")
    "Ordered Consult Omni sources used by the default launcher.")

  (defun launcher-creator(f &rest args)
    "Create a launcher for the given function F with ARGS.

F is expected to show up in the minibuffer.
Currently the frame is shown on the primary monitor.
Should be updated to show on the active monitor."
    (let* ((pgtk-p (eq window-system 'pgtk))
           (vertico-count 15)
           ;; Dynamic minibuffer frames work well under PGTK, but the NS frame
           ;; moves as it resizes.  Keep the original fixed frame on macOS.
           (vertico-resize pgtk-p)
           (resize-mini-frames pgtk-p)
           (max-mini-window-height 0.5)
           (monitor (car (display-monitor-attributes-list)))
           (geometry (alist-get 'workarea monitor))
           ;; PGTK reports physical pixels plus a scale factor, while NS/macOS
           ;; already reports its work area in logical points.
           (scale (if pgtk-p
                      (or (alist-get 'scale-factor monitor) 1.0)
                    1.0))
           (monitor-width (/ (nth 2 geometry) scale))
           (monitor-height (/ (nth 3 geometry) scale))
           (width (floor (* (if pgtk-p 0.55 0.45) monitor-width)))
           (height (floor (* 0.5 monitor-height)))
           (left (floor (* (if pgtk-p 0.225 0.25) monitor-width)))
           (top (floor (* (if pgtk-p 0.15 0.3) monitor-height)))
           (params `((name . "emacs-launcher")
                     (width . ,(cons 'text-pixels width))
                     (height . ,(if pgtk-p
                                    1
                                  (cons 'text-pixels height)))
                     (left . ,left)
                     (top . ,top)
                     (internal-border-width . 15)
                     (undecorated-round . t)
                     (no-focus-on-map . ,(eq window-system 'ns))
                     (no-other-frame . t)
                     (font . ,(concat default-font "-16"))
                     (alpha-background . 70)
                     (minibuffer . only)))
           (cached-frame (and +launcher-space
                              (gethash +launcher-space +launcher-frames)))
           (frame
            (if (frame-live-p cached-frame)
                cached-frame
              (let ((new-frame (make-frame params)))
                (when +launcher-space
                  (puthash +launcher-space new-frame +launcher-frames))
                new-frame))))
      (with-selected-frame frame
        (when +launcher-space
          ;; Frames cached for another Space are never touched here.  Showing
          ;; only the frame created on the current Space avoids macOS Space
          ;; switching and window-moving races.
          (modify-frame-parameters frame params)
          (make-frame-visible frame))
        (select-frame-set-input-focus frame)
        (unwind-protect
            (let ((+launcher-active t))
              ;; This dynamically scoped hook also applies to recursive
              ;; minibuffers opened by Embark from inside the launcher.
              (minibuffer-with-setup-hook #'+launcher-minibuffer-setup
                (apply f args))
              nil)
          (progn
            (when (frame-live-p frame)
              (if +launcher-space
                  (make-frame-invisible frame)
                (delete-frame frame t)))
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
    (launcher-creator 'org-clock-in-any))

  (defun +launcher-sort-by-source (candidates)
    "Sort CANDIDATES by source, then with Vertico's normal sorter."
    (let ((groups (make-hash-table :test #'equal))
          seen-sources
          ungrouped)
      (dolist (candidate candidates)
        (if-let ((source (get-text-property 0 :source candidate)))
            (progn
              (cl-pushnew source seen-sources :test #'equal)
              (push candidate (gethash source groups)))
          (push candidate ungrouped)))
      (nconc
       (cl-loop for source in consult-omni-multi-sources
                append (funcall vertico-sort-function
                                (nreverse (gethash source groups))))
       ;; Never discard candidates from a source whose display name differs
       ;; from its configured Consult Omni name.
       (cl-loop for source in (nreverse seen-sources)
                unless (member source consult-omni-multi-sources)
                append (funcall vertico-sort-function
                                (nreverse (gethash source groups))))
       (funcall vertico-sort-function (nreverse ungrouped)))))

  (defun +launch-default-launcher(&optional space)
    (interactive)
    (let* ((+launcher-space space)
           (vertico-sort-override-function #'+launcher-sort-by-source)
           (consult-omni-multi-sources +launcher-sources))
      (+launch-consult-omni)))



  (defun +launch-emoji-completing-read()
    (interactive)
    (require 'emoji-search)
    ;; Vertico multiform selects per-command layouts from `this-command'.
    ;; Preserve the underlying command identity even though the launcher calls
    ;; it indirectly, so this gets the same grid as `C-c s e'.
    (let ((this-command 'emoji-search-completing-read))
      (launcher-creator 'emoji-search-completing-read)))

  (defun +launcher-schedule-emoji ()
    "Open the emoji picker after the Consult launcher has closed."
    (run-at-time 0 nil #'+launch-emoji-completing-read))

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

  (defun +launcher-system-command (action)
    "Run the allowlisted system ACTION on macOS or Linux."
    (pcase (list system-type action)
      (`(darwin lock) (start-process "launcher-lock" nil
                                     "/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession"
                                     "-suspend"))
      (`(darwin display-sleep) (start-process "launcher-display-sleep" nil "pmset" "displaysleepnow"))
      (`(darwin sleep) (start-process "launcher-sleep" nil "osascript" "-e"
                                      "tell application \"System Events\" to sleep"))
      (`(darwin mute) (start-process "launcher-mute" nil "osascript" "-e"
                                     "set volume output muted not (output muted of (get volume settings))"))
      (`(darwin appearance) (start-process "launcher-appearance" nil "osascript" "-e"
                                           "tell application \"System Events\" to tell appearance preferences to set dark mode to not dark mode"))
      (`(gnu/linux lock) (start-process "launcher-lock" nil "loginctl" "lock-session"))
      (`(gnu/linux display-sleep) (start-process "launcher-display-sleep" nil "xset" "dpms" "force" "off"))
      (`(gnu/linux sleep) (start-process "launcher-sleep" nil "systemctl" "suspend"))
      (`(gnu/linux mute) (start-process "launcher-mute" nil "pactl" "set-sink-mute" "@DEFAULT_SINK@" "toggle"))
      (_ (user-error "System action unavailable on %s" system-type))))


  (defvar consult-omni-launcher-entries
    '(("Emoji" . +launcher-schedule-emoji)
      ("Find File" . +launch-file)
      ("Clock In" . +launcher-org-quick-clock-in)
      ("Kill Process" . +launch-killer)
      ("System: Lock screen" . (lambda () (+launcher-system-command 'lock)))
      ("System: Sleep display" . (lambda () (+launcher-system-command 'display-sleep)))
      ("System: Sleep computer" . (lambda () (+launcher-system-command 'sleep)))
      ("System: Toggle mute" . (lambda () (+launcher-system-command 'mute)))
      ("System: Toggle light/dark mode" . (lambda () (+launcher-system-command 'appearance)))
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
                                           :source "Static launcher"
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
      ("Google AI" . "https://www.google.com/search?udm=50&source=searchlabs&q=%s")
      ("devdocs.io" . "https://devdocs.io/#q=%s")
      ("Youtube" . "https://www.youtube.com/results?search_query=%s"))
    "List of fixed entries and their associated functions.")

  (cl-defun consult-omni--web-searches-results(input &rest args &key callback &allow-other-keys)
    "Return hardcoded entries matching INPUT."
    (let ((entries (reverse (mapcar (lambda (cand)
                                      (propertize (format "Search for \"%s\" on %s" input (car cand))
                                                  :source "Web Search"
                                                  :title (car cand)
                                                  :url nil
                                                  :query input))
                                    consult-omni-web-searches-entries))))
      (when (functionp callback)
        (funcall callback entries))
      nil))


  (defun consult-omni--web-search-execute(cand)
    (let* ((query (get-text-property 0 :query cand))
           (engine (get-text-property 0 :title cand))
           (entry (assoc engine consult-omni-web-searches-entries)))
      (when entry
        (browse-url (format (cdr entry) query)))))

  (defconst +launcher-agent-directory
    (expand-file-name "~/junk/claude-chats/")
    "Working directory for agents started from the launcher.")

  (defun consult-omni--agent-prompt (input)
    "Return an agent prompt from INPUT, or nil when it is blank.

The optional `ai ' prefix is removed, but is no longer required."
    (when (stringp input)
      (let ((prompt (string-trim input)))
        (when (string-match "\\`ai[[:space:]]+\\(.+\\)\\'" prompt)
          (setq prompt (string-trim (match-string 1 prompt))))
        (unless (string-empty-p prompt)
          prompt))))

  (defun consult-omni--agent-fetch-results (input &rest _args)
    "Offer a fresh agent-shell for INPUT."
    (when-let ((prompt (consult-omni--agent-prompt input)))
      (let ((candidate
             (propertize (format "Ask agent: %s" prompt)
                         :source "Agent"
                         :title prompt
                         :query prompt)))
        (list candidate))))

  (defun +launcher-start-agent (prompt)
    "Start a new agent-shell in the launcher directory and send PROMPT."
    (require 'agent-shell)
    (make-directory +launcher-agent-directory t)
    (let* ((default-directory +launcher-agent-directory)
           (shell-buffer
            (agent-shell--start
             :config (or (agent-shell--auto-preferred-config)
                         agent-shell-preferred-agent-config
                         (user-error "No preferred agent-shell config found"))
             :no-focus t
             :new-session t
             :session-strategy 'new)))
      (agent-shell--display-buffer shell-buffer)
      (with-current-buffer shell-buffer
        (agent-shell-queue-request prompt))))

  (defun consult-omni--agent-callback (candidate)
    "Start an agent for the prompt stored on CANDIDATE."
    (let ((prompt (get-text-property 0 :query candidate)))
      ;; Let Consult unwind and hide the launcher frame before displaying the
      ;; agent buffer.  This also avoids selecting the minibuffer-only frame.
      (run-at-time 0 nil #'+launcher-start-agent prompt)))

  (defvar +launcher-developer-tools
    `(("uuid" :description "generate a UUID" :label "UUID"
       :function ,(lambda ()
                    (downcase (string-trim
                               (shell-command-to-string "uuidgen")))))
      ("timestamp" :aliases ("unix") :description "current Unix timestamp"
       :label "Unix timestamp"
       :function ,(lambda () (number-to-string (floor (float-time)))))
      ("urlencode" :argument t :description "encode a URL component"
       :label "URL encoded" :function url-hexify-string)
      ("urldecode" :argument t :description "decode a URL component"
       :label "URL decoded" :function url-unhex-string)
      ("base64" :argument t :description "encode text as Base64"
       :label "Base64"
       :function ,(lambda (text) (base64-encode-string text t)))
      ("base64decode" :argument t :description "decode Base64 text"
       :label "Base64 decoded" :function base64-decode-string)
      ("sha256" :argument t :description "hash text with SHA-256"
       :label "SHA-256"
       :function ,(lambda (text) (secure-hash 'sha256 text))))
    "Developer tools exposed by the launcher.

Each entry is (COMMAND . PROPERTIES).  Set `:argument' to t when the
tool requires text after its command; omit it for no-argument tools.")

  (defun +launcher-developer--commands (tool)
    "Return the command and aliases belonging to TOOL."
    (cons (car tool) (plist-get (cdr tool) :aliases)))

  (defun +launcher-developer--find-tool (command)
    "Return the developer tool matching COMMAND or one of its aliases."
    (seq-find (lambda (tool)
                (member command (+launcher-developer--commands tool)))
              +launcher-developer-tools))

  (defun +launcher-developer-results (input)
    "Return developer utility results appropriate for INPUT."
    (let* ((normalized (downcase (string-trim input)))
           (space (string-match-p "[[:space:]]" normalized))
           (parts (and (string-match
                        "\\`\\([^[:space:]]+\\)\\(?:[[:space:]]+\\(.*\\)\\)?\\'"
                        normalized)
                       (list (match-string 1 normalized)
                             (match-string 2 input))))
           (command (car parts))
           (argument (cadr parts))
           (tool (and command (+launcher-developer--find-tool command)))
           results)
      (cl-labels
          ((add-result (label value)
             (push (propertize (format "%s  %s" label value)
                               :source "Developer" :title value :query input)
                   results))
           (add-command (name properties)
             (let ((argument-p (plist-get properties :argument)))
               (push (propertize name
                                 :source "Developer" :title name :query input
                                 :developer-command t
                                 :annotation
                                 (if argument-p
                                     (format "%s <text> — %s" name
                                             (plist-get properties :description))
                                   (plist-get properties :description)))
                     results))))
        (when (and tool
                   (if (plist-get (cdr tool) :argument)
                       (not (string-empty-p (or argument "")))
                     (not space)))
          (condition-case nil
              (add-result (plist-get (cdr tool) :label)
                          (if (plist-get (cdr tool) :argument)
                              (funcall (plist-get (cdr tool) :function) argument)
                            (funcall (plist-get (cdr tool) :function))))
            (error nil)))
        (unless space
          (dolist (candidate-tool +launcher-developer-tools)
            (dolist (name (+launcher-developer--commands candidate-tool))
              (when (and (not (string-empty-p normalized))
                         (string-prefix-p normalized name)
                         (not (and (not (plist-get (cdr candidate-tool) :argument))
                                   (string= normalized name))))
                (add-command name (cdr candidate-tool))))))
        (nreverse results))))

  (defun consult-omni--developer-fetch-results (input &rest _args)
    "Return developer transformations for INPUT."
    (+launcher-developer-results (string-trim input)))

  (defun consult-omni--developer-callback (candidate)
    "Copy the transformed value from CANDIDATE."
    (kill-new (get-text-property 0 :title candidate)))

  (defun consult-omni--developer-annotate (candidate)
    "Annotate developer command CANDIDATE with its usage."
    (when-let ((annotation (get-text-property 0 :annotation candidate)))
      (concat "  " (propertize annotation 'face 'completions-annotations))))

  (defun consult-omni--browser-tabs-query (input)
    "Return the query from a `tab QUERY' INPUT, or nil."
    (when (and (stringp input)
               (string-match "\\`tab[[:space:]]+\\(.+\\)\\'" input))
      (string-trim (match-string 1 input))))

  (defun consult-omni--browser-tabs-valid-input-p (input)
    "Return INPUT when it has the `tab QUERY' form expected by the source."
    (and (consult-omni--browser-tabs-query input) input))

  (defun +launcher-chrome-tabs ()
    "Return Chrome tabs as title, URL, tab index and window id fields."
    (process-lines
     "osascript" "-e"
     (concat
      "tell application \"Google Chrome\"\n"
      "set output to \"\"\n"
      "repeat with w in windows\n"
      "set tabIndex to 0\n"
      "repeat with t in tabs of w\n"
      "set tabIndex to tabIndex + 1\n"
      "set output to output & (title of t) & (ASCII character 9) & (URL of t) & (ASCII character 9) & tabIndex & (ASCII character 9) & (id of w) & linefeed\n"
      "end repeat\nend repeat\nreturn output\nend tell")))

  (defun consult-omni--browser-tabs-fetch-results (input &rest _args)
    "Return open Chrome tabs matching INPUT's `tab ' query."
    (when-let ((query (consult-omni--browser-tabs-query input)))
      (let ((case-fold-search t))
        (delq
         nil
         (mapcar
          (lambda (line)
            (pcase-let ((`(,title ,url ,tab-index ,window-id)
                         (split-string line "\t")))
              (when (and window-id
                         (string-match-p (regexp-quote query)
                                         (concat title " " url)))
                (propertize (format "%s  %s" title url)
                            :source "Browser Tabs" :title title :url url
                            :tab-index tab-index :window-id window-id))))
          (+launcher-chrome-tabs))))))

  (defun consult-omni--browser-tabs-callback (candidate)
    "Focus the Chrome tab represented by CANDIDATE."
    (let ((tab-index (get-text-property 0 :tab-index candidate))
          (window-id (get-text-property 0 :window-id candidate)))
      (start-process
       "launcher-focus-chrome-tab" nil "osascript" "-e"
       (format (concat "tell application \"Google Chrome\"\n"
                       "activate\nset targetWindow to first window whose id is %s\n"
                       "set active tab index of targetWindow to %s\n"
                       "set index of targetWindow to 1\nend tell")
               window-id tab-index))))

  (defun +launcher-org-agenda-add-embark-marker (candidates)
    "Expose Consult Omni agenda markers in the form Embark expects.

Consult Omni stores the destination as `:marker', while Embark's
`org-heading' actions look for the conventional `org-marker' text
property.  Without it, Org actions run in the launcher minibuffer."
    (mapcar
     (lambda (candidate)
       ;; Consult can retain candidates between dynamic updates.  Work on a
       ;; copy rather than modifying a string it may already have protected.
       (if (not (stringp candidate))
           candidate
         (let ((copy (copy-sequence candidate)))
           (when-let ((marker (get-text-property 0 :marker copy)))
             (add-text-properties 0 (length copy)
                                  `(org-marker ,marker) copy))
           copy)))
     candidates))

  (consult-omni-define-source "Qalc"
                              :narrow-char ?Q
                              :category 'consult-omni-calc
                              :type 'async
                              :require-match t
                              :face 'consult-omni-engine-title-face
                              :request (lambda (input &rest _)
                                         (when-let ((command (qalc-command input)))
                                           (cons command
                                                 (cdr
                                                  (consult--default-regexp-compiler
                                                   input 'basic t)))))
                              :valid-input (lambda (input)
                                             (cond
                                              ((string-prefix-p "=" input)
                                               (string-trim (substring input 1)))
                                              ((string-match-p "[[:digit:]]" input)
                                               input)))
                              :filter #'qalc-filter-results
                              :on-preview #'ignore
                              :on-return #'identity
                              :on-callback #'qalc-run-result-action
                              :preview-key consult-omni-preview-key
                              :search-hist 'consult-omni--search-history
                              :select-hist 'consult-omni--selection-history
                              :group #'consult-omni--group-function
                              :enabled #'qalc-available-p
                              :sort t
                              :interactive consult-omni-intereactive-commands-type
                              :annotate nil)

  (consult-omni-define-source "Agent"
                              :narrow-char ?a
                              :category 'consult-omni-agent
                              :type 'sync
                              :require-match t
                              :face 'consult-omni-engine-title-face
                              :request #'consult-omni--agent-fetch-results
                              :valid-input #'consult-omni--agent-prompt
                              :on-preview #'ignore
                              :on-return #'identity
                              :on-callback #'consult-omni--agent-callback
                              :preview-key consult-omni-preview-key
                              :search-hist 'consult-omni--search-history
                              :select-hist 'consult-omni--selection-history
                              :group #'consult-omni--group-function
                              :enabled (lambda () t)
                              :sort nil
                              :interactive consult-omni-intereactive-commands-type
                              :annotate nil
                              :min-input 3)

  (consult-omni-define-source "Developer"
                              :narrow-char ?d
                              :category 'consult-omni-developer
                              :type 'sync
                              :require-match t
                              :request #'consult-omni--developer-fetch-results
                              :on-preview #'ignore
                              :on-return #'identity
                              :on-callback #'consult-omni--developer-callback
                              :preview-key consult-omni-preview-key
                              :search-hist 'consult-omni--search-history
                              :select-hist 'consult-omni--selection-history
                              :group #'consult-omni--group-function
                              :enabled (lambda () t)
                              :sort nil
                              :interactive consult-omni-intereactive-commands-type
                              :annotate #'consult-omni--developer-annotate
                              :min-input 1)

  (consult-omni-define-source "Browser Tabs"
                              :narrow-char ?T
                              :category 'url
                              :type 'sync
                              :require-match t
                              :request #'consult-omni--browser-tabs-fetch-results
                              :valid-input #'consult-omni--browser-tabs-valid-input-p
                              :on-preview #'ignore
                              :on-return #'identity
                              :on-callback #'consult-omni--browser-tabs-callback
                              :preview-key consult-omni-preview-key
                              :search-hist 'consult-omni--search-history
                              :select-hist 'consult-omni--selection-history
                              :group #'consult-omni--group-function
                              :enabled (lambda ()
                                         (and (eq system-type 'darwin)
                                              (file-directory-p "/Applications/Google Chrome.app")))
                              :sort t
                              :interactive consult-omni-intereactive-commands-type
                              :annotate nil
                              :min-input 5)

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
              'consult-omni-org-agenda
              'consult-omni-projects
              'consult-omni-ripgrep
              'consult-omni-ripgrep-all
              'consult-omni-wikipedia))
  (consult-omni-sources-load-modules)
  (unless (advice-member-p #'+launcher-org-agenda-add-embark-marker
                           'consult-omni--org-agenda-items)
    (advice-add 'consult-omni--org-agenda-items
                :filter-return #'+launcher-org-agenda-add-embark-marker))
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
