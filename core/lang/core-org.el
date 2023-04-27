;; Org mode settings
(defvar +org-directory "~/Dropbox/org-files/")


(use-package org-crypt
  :ensure nil
  :commands org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry
  :init
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setf epg-pinentry-mode 'loopback)
  (setq org-crypt-key nil)
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  :config (org-crypt-use-before-save-magic))

;; To turn it off auto-save only locally, you can insert this:
;; # -*- buffer-auto-save-file-name: nil; -*-

;; (use-package org-bullets
;;   :init (remove-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


(use-package org-pomodoro
  :after org-agenda
  :bind (:map org-agenda-mode-map
              ("I" . org-pomodoro))
  :config
  (setq org-pomodoro-finished-sound (concat RES-DIR "bell.wav"))
  (setq org-pomodoro-long-break-sound (concat RES-DIR "bell.wav"))
  (setq org-pomodoro-short-break-sound (concat RES-DIR "bell.wav"))
  (setq org-pomodoro-start-sound (concat RES-DIR "bell.wav")))

(use-package org
  ;; :defer 2
  :config
  ;; Strange bug causing org-version to be empty. Breaking nov.el
  (setq org-version (if (string= org-version "")
                        (let ((org-full-dir (file-name-directory (locate-library "org"))))
                          (save-match-data
                            (and (string-match "-\\([0-9.]+\\)/" org-full-dir)) (match-string 1 org-full-dir)))
                      org-version))
  (defvar org-plantuml-jar-path)
  ;; helper function
  (defun my-org-confirm-babel-evaluate (lang body)
    "Do not ask for confirmation to evaluate code for specified languages."
    (member lang '("plantuml")))

  (defun +org-refile-to-pos (file headline &optional arg)
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (org-refile arg nil (list headline file nil pos)))
    (switch-to-buffer (current-buffer)))

  ;; Courtesy: johnkitchin
  ;; TODO: Can't paste no macos. Have to use copyq
  ;; Alternative is to directly use pandoc on org text
  ;; pandoc -f org -t rtf -s -
  (defun org-formatted-copy ()
    "Export region to HTML, and copy it to the clipboard."
    (interactive)
    (save-window-excursion
      (let* ((org-export-with-toc nil)
             (org-export-with-sub-superscripts nil)
             (buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
             (html (with-current-buffer buf (buffer-string))))
        (with-current-buffer buf
          (shell-command-on-region
           (point-min)
           (point-max)
           "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
        (kill-buffer buf))))

  (defun anonymous-pomodoro(&optional arg)
    "Start a 25 minute work or 5 min rest timer based on the prefix arg.
    Helpful in quickly starting a pomodoro for work and rest"
    (interactive "P")
    (if (and (boundp 'org-timer-countdown-timer) org-timer-countdown-timer)
        (org-timer-stop)
      (org-timer-set-timer (if arg "5" "25"))))

  (defun variable-pitch-for-notes ()
    (interactive)
    (when (string-match "\\(.*Notes.org\\|roam.*org\\)" (format "%s" buffer-file-name))
      (reading-mode)))

  (defun copy-current-line-link-for-org ()
    "Copy current line in file to clipboard as an org file link"
    (interactive)
    (let ((org-link
           (format "[[file:%s::%d][%s::%d]]"
                   (buffer-file-name)
                   (line-number-at-pos)
                   (file-name-nondirectory (buffer-file-name))
                   (line-number-at-pos))))
      (kill-new org-link)
      (message (format "%s::%d link copied to clipboard"
                       (file-name-nondirectory (buffer-file-name))
                       (line-number-at-pos)))))

  (defun org-beautify-checkbox ()
    "Beautify Org Checkbox Symbol"
    (push '("[ ]" .  "☐") prettify-symbols-alist)
    (push '("[X]" . "☒" ) prettify-symbols-alist)
    (push '("[-]" . "❍" ) prettify-symbols-alist)
    (prettify-symbols-mode))
  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")

  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)
  (add-hook 'org-mode-hook 'org-beautify-checkbox)


  (defun +capture-inbox()
    (interactive)
    (org-capture nil "i"))
  ;; trust certain code as being safe

  (setq org-capture-templates
        `(("i" "inbox" entry (file ,(concat +org-directory "inbox.org"))
           "* TODO %?")
          ("l" "link" entry (file ,(concat +org-directory "inbox.org"))
           "* TODO %(org-cliplink-capture)" :immediate-finish t)
          ("c" "org-protocol-capture" entry (file ,(concat +org-directory "inbox.org"))
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))


  (defun add-property-with-date-captured ()
    "Add CAPTURED property to the current item."
    (interactive)
    (org-set-property "CAPTURED" (format-time-string "[%F %a %R]")))

  (add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)

  (setq org-capture-bookmark nil)

  (setq org-ellipsis "⤶"
        ;; org-agenda-files `(,+org-directory)

        org-archive-location (concat +org-directory "archive.org::* From %s")
        org-startup-align-all-table t
        org-log-done 'time
        org-return-follows-link t
        org-startup-with-inline-images t
        org-display-remote-inline-images 'download
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-id-locations-file (concat CACHE-DIR ".org-id-locations")
        org-generic-id-locations-file (concat CACHE-DIR ".org-generic-id-locations-file")
        org-image-actual-width 500
        org-startup-folded t
        org-imenu-depth 8
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))
        org-export-with-sub-superscripts nil ;; "{}"
        org-html-checkbox-type 'html
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-clock-sound (concat RES-DIR "bell.wav")
        org-id-link-to-org-use-id 'create-if-interactive
        org-todo-keywords '((sequence "TODO(t)" "DOING(o)"  "|" "DONE(d)")
                            (sequence "BLOCKED(b@/!)" "DELEGATED(e@/!)" "WAITING(w@/!)" "|" "CANCELLED(c@/!)"))
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-confirm-babel-evaluate nil
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

  (setq org-format-latex-options
        (list :foreground 'auto
              :background 'auto
              :scale 1.5
              :html-foreground "Black"
              :html-background "Transparent"
              :html-scale 1.0
              :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))

  (setq help-at-pt-display-when-idle t)

  (setq org-refile-targets
        '((nil :maxlevel . 8)
          (org-agenda-files :maxlevel . 8))
        ;; Without this, completers like ivy/helm are only given the first level of
        ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
        ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
        ;; target! e.g. FILE/Tasks/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (setq org-src-preserve-indentation t  ; use native major-mode indentation
        org-link-elisp-confirm-function nil
        ;; Show src buffer in popup, and don't monopolize the frame
        org-src-window-setup 'other-window)

  ;; (defadvice org-edit-src-exit (after restore-window-config activate disable)
  ;;   (winner-undo))

  (setq org-display-remote-inline-images 'download) ; TRAMP urls
  (org-link-set-parameters "http"  :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "https" :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "img"   :image-data-fun #'+org-inline-image-data-fn)

  ;; (font-lock-remove-keywords 'org-mode
  ;;                       '(("^ *\\((-)\\) "
  ;;                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (add-hook 'org-mode-hook 'visual-line-mode)
  ;; (add-hook 'org-mode-hook 'toggle-truncate-lines)
  (company-backend-for-hook 'org-mode-hook '((company-capf company-yasnippet company-dabbrev)))
  (add-hook 'org-mode-hook 'variable-pitch-for-notes)

  (setq org-modules
        '(;; ol-w3m
          ;; ol-bbdb
          ol-bibtex
          ;; ol-docview
          ;; ol-gnus
          ;; ol-info
          ;; ol-irc
          ;; ol-mhe
          ;; ol-rmail
          ;; ol-eww
          org-habit
          ))
  ;; Save target buffer after archiving a node.
  ;; (setq org-archive-subtree-save-file-p t)

  (setq org-habit-completed-glyph ?•) ; bullet point
  ;; (setq org-habit-today-glyph 0215) ; multiplication sign
  ;; For some reason the above glyph comes up as \327 in agenda
  ;; using a similar gliph
  (setq org-habit-today-glyph ?⨯) ; vector cross product sign
  (setq org-habit-following-days 1)

  ;; Prevent modifications made in invisible sections of an org document, as
  ;; unintended changes can easily go unseen otherwise.
  (setq org-catch-invisible-edits 'smart)

  (custom-set-faces
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  (set-face-attribute 'org-ellipsis nil
                      :inherit '(font-lock-comment-face default)
                      :weight 'normal)


  ;; Support for plantuml
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))


  (advice-add #'org-babel-execute:plantuml
              :override #'+plantuml-org-babel-execute:plantuml-a)
  (add-to-list 'org-babel-default-header-args:plantuml
               '(:cmdline . "-charset utf-8"))

  ;; automatically show the resulting image
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-clock-in-hook (lambda() (org-todo "DOING")) 'append)

  ;; Courtesy: doom emacs (popup/+hacks.el)
  (defun +popup--supress-delete-other-windows-a (origin-fn &rest args)
    (if +popup-mode
        (cl-letf (((symbol-function #'delete-other-windows) #'ignore)
                  ((symbol-function #'delete-window)        #'ignore))
          (apply origin-fn args))
      (apply origin-fn args)))
  (advice-add #'org-add-log-note :around #'+popup--supress-delete-other-windows-a)
  (advice-add #'org-capture-place-template :around #'+popup--supress-delete-other-windows-a)
  (advice-add #'org-export--dispatch-ui :around #'+popup--supress-delete-other-windows-a)
  (advice-add #'org-agenda-get-restriction-and-command :around #'+popup--supress-delete-other-windows-a)
  (advice-add #'org-goto-location :around #'+popup--supress-delete-other-windows-a)
  (advice-add #'org-fast-tag-selection :around #'+popup--supress-delete-other-windows-a)
  (advice-add #'org-fast-todo-selection :around #'+popup--supress-delete-other-windows-a)

  ;; Ensure todo, agenda, and other minor popups are delegated to the popup system.
  (defun +popup--org-pop-to-buffer-a(orig-fn buf &optional norecord)
    (if +popup-mode
        (pop-to-buffer buf nil norecord)
      (funcall orig-fn buf norecord)))

  (advice-add #'org-switch-to-buffer-other-window :around #'+popup--org-pop-to-buffer-a)

  ;; Courtesy: doomemacs
  ;; HACK `pop-to-buffer-same-window' consults `display-buffer-alist', which is
  ;;      what our popup manager uses to manage popup windows. However,
  ;;      `org-src-switch-to-buffer' already does its own window management
  ;;      prior to calling `pop-to-buffer-same-window', so there's no need to
  ;;      _then_ hand off the buffer to the pop up manager.
  (defun +popup--org-src-switch-to-buffer-a (orig-fn &rest args)
    (cl-letf (((symbol-function #'pop-to-buffer-same-window) #'switch-to-buffer))
      (apply orig-fn args)))
  (advice-add #'org-src-switch-to-buffer :around #'+popup--org-src-switch-to-buffer-a)

  (set-popup-rules!
    '(("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
      ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
       :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
      ("^\\*Org \\(?:Select\\|Attach\\)" :slot -1 :vslot -2 :ttl 0 :size 0.25)
      ("^\\*Org Agenda"     :ignore t)
      ("^\\*Org Src"        :size 0.4  :quit nil :select t :autosave t :modeline t :ttl nil)
      ("^\\*Org-Babel")
      ("^CAPTURE-.*\\.org$" :size 0.25 :quit nil :select t :autosave t)))


  :bind (("C-c o e" . org-export-dispatch)
         ("C-c o c" . +capture-inbox)
         ("C-c o w" . copy-current-line-link-for-org)
         ("C-c o T" . anonymous-pomodoro)
         ("<f5>" . +capture-inbox)
         ("C-c o g" . consult-org-agenda)
         :map org-mode-map
         ("C-s-q" . org-fill-paragraph)
         ("C-<tab>" . nil)))

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
    nil)
  )

(use-package org-agenda
  ;; :after org
  :ensure nil
  :init
  (defun +switch-to-agenda()
    (interactive)
    (org-agenda nil " "))
  :bind (("C-c o A" . org-agenda)
         ("C-c o a" . +switch-to-agenda)
         ("<f1>" . +switch-to-agenda)
         (:map org-agenda-mode-map
               ("i" . org-agenda-clock-in)
               ("c" . +capture-inbox)
               ("M-*" . nil)
               ("W" . org-id-protocol-link-copy)
               ("r" . +org-agenda-process-inbox-item)
               ("R" . org-agenda-refile)))
  :config
  ;; Courtesy Jethro Kuan.
  ;; https://blog.jethro.dev/posts/org_mode_workflow_preview/
  (defvar +org-agenda-bulk-process-key ?f
    "Default key for bulk processing inbox items.")

  (defun +org-process-inbox ()
    "Called in org-agenda-mode, processes all inbox items."
    (interactive)
    (org-agenda-bulk-mark-regexp "inbox:")
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
     (org-agenda-priority)
     (call-interactively '+org-agenda-set-effort)
     (org-agenda-refile nil nil t)))


  (defun org-id-protocol-link-copy ()
    (interactive)
    (save-window-excursion
      (org-agenda-switch-to)
      (org-kill-new (concat "org-protocol://org-id?id="
                            (org-id-get nil 'create)))
      (message "Link copied to clipboard")))

  ;; Courtesy: https://stackoverflow.com/a/36830367
  ;; (defun org-random-cmp (a b)
  ;;   "Return -1,0 or 1 randomly"
  ;;   (- (mod (random) 3) 1))

  ;; (setq org-agenda-som-count 0)
  (setq org-agenda-custom-commands
        `((" " "Agenda"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-deadline-warning-days 365)))
            (todo "TODO"
                  ((org-agenda-overriding-header "To Refile")
                   (org-agenda-files '(,(concat +org-directory "inbox.org")
                                       ,(concat +org-directory "inbox_phone.org")))))
            (todo "DOING"
                  ((org-agenda-overriding-header "In Progress")
                   (org-agenda-files '(,(concat +org-directory "someday.org")
                                       ,(concat +org-directory "next.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "One-off Tasks")
                   (org-agenda-files '(,(concat +org-directory "next.org")))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
            (todo "BLOCKED"
                  ((org-agenda-overriding-header "Blocked Tasks")
                   (org-agenda-files '(,(concat +org-directory "someday.org")
                                       ,(concat +org-directory "next.org")))))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting on")
                   (org-agenda-files '(,(concat +org-directory "someday.org")
                                       ,(concat +org-directory "next.org")))))
            (todo "DELEGATED"
                  ((org-agenda-overriding-header "Delegated Tasks")
                   (org-agenda-files '(,(concat +org-directory "someday.org")
                                       ,(concat +org-directory "next.org")))))
            ;; (todo "TODO"
            ;; ;; Idea is to sort randomly and pick up top 10 results from the someday list. Doesn't work right now.
            ;;       ((org-agenda-overriding-header "Someday")
            ;;        (org-agenda-cmp-user-defined #'org-random-cmp)
            ;;        (org-agenda-files '(,(concat +org-directory "someday.org")))
            ;;        (org-agenda-sorting-strategy '(user-defined-up))
            ;;        (org-agenda-som-count 0)
            ;;        ;; (org-agenda-skip-function
            ;;        ;;  (org-agenda-skip-entry-if
            ;;        ;;   (> (setq org-agenda-som-count (1+ org-agenda-som-count)) 10)))
            ;;        ))
            nil))))





  (setq org-agenda-files (mapcar (lambda(file) (concat +org-directory file)) '("inbox.org" "inbox_phone.org" "next.org" "projects.org" "someday.org" "schedule.org" "repeaters.org"))
        org-agenda-window-setup 'current-window
        org-agenda-skip-unavailable-files t
        org-agenda-span 10
        org-agenda-block-separator (aref "━" 0)
        org-agenda-start-on-weekday nil
        org-agenda-start-day nil
        org-agenda-time-grid `((daily today require-timed remove-match)
                               ,(number-sequence 800 2000 200)
                               "......"
                               "----------------")
        org-agenda-inhibit-startup t))

(use-package org-gcal
  :commands (org-gcal-sync org-gcal-post-at-point)
  :after org-agenda
  :init
  (defvar org-gcal--running-timer nil)
  (unless (eq org-gcal--running-timer nil)
    (setq org-gcal--running-timer (run-with-timer 300 300 (lambda () (org-gcal-sync t t)))))
  :bind (:map org-agenda-mode-map
              ("S" . org-gcal-sync)
              ("P" . org-gcal-post-at-point))
  :config
  (defvar org-gcal-file)
  (setq org-gcal-file (concat +org-directory "schedule.org")
        org-gcal-dir (concat CACHE-DIR "org-gcal/")
        persist--directory-location (concat CACHE-DIR "persist")
        org-gcal-token-file (expand-file-name ".org-gcal-token" org-gcal-dir)
        org-gcal-notify-p nil)
  ;; Depends on core-secrets entry
  ;; (setq org-gcal-client-id "my-app.apps.googleusercontent.com"
  ;;       org-gcal-client-secret "secret"
  ;;       org-gcal-calendars '("calendar1" "calendar2" "calendar3"))

  (setq org-gcal-fetch-file-alist (mapcar (lambda(x) `(,x . ,org-gcal-file)) org-gcal-calendars)))


(use-package org-wild-notifier
  :defer 5
  :config
  (setq org-wild-notifier-alert-time '(5))
  (setq org-wild-notifier-keyword-whitelist nil)
  (org-wild-notifier-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))
(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚑" "⇧" "⇩" "☕")))

(use-package org-download
  :bind (("C-c o d c" . org-download-clipboard)
         ("C-c o d d" . org-download-image)
         ("C-c o d x" . org-download-delete)
         ("C-c o d s" . org-download-screenshot)
         ("C-c o d y" . org-download-yank)
         :map org-mode-map
         ("C-y" . +org-yank))

  :init
  (setq-default org-download-image-dir (concat +org-directory "resource/downloads"))
  (when IS-MAC (setq org-download-screenshot-method "screencapture -i %s"))
  :config
  (defun +org-yank()
    "Yanks image or text in the buffer"
    (interactive)
    (if (or (not IS-MAC) (> (length (shell-command-to-string "pngpaste -b | cut -b 1")) 2))
        (org-yank nil)
      (org-download-clipboard nil))))

(use-package org-roam
  :ensure org-roam
  ;; :ensure company-org-roam
  :init
  (setq org-roam-directory (concat +org-directory "org-roam/"))
  (setq org-roam-db-location (concat CACHE-DIR "org-roam.db"))
  :bind (("C-c o n n" . +org-roam-node-find)
         ("C-c o m" . org-roam-buffer-toggle)
         ("C-c o r d" . org-roam-dailies-goto-date)
         ("C-c o r r" . org-roam-dailies-goto-today)
         ("C-c o r m" . org-roam-dailies-goto-tomorrow)
         ("C-c o r y" . org-roam-dailies-goto-yesterday)
         (:map org-mode-map
               ("C-z r t" . org-roam-tag-add)
               ("C-z r T" . org-roam-tag-remove)
               ("C-c o n b" . org-roam-switch-to-buffer)
               ("C-c o n g" . org-roam-graph)
               ("C-c o n u" . org-roam-unlinked-references)
               ("C-c o m"   . org-roam-buffer-toggle)
               ("C-c o n a" . org-roam-alias-add)
               ("C-c o n A" . org-roam-alias-remove)
               ("C-c o n r" . org-roam-ref-add)
               ("C-c o n R" . org-roam-ref-remove)))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (add-to-list 'org-roam-file-exclude-regexp "logseq/")
  (defvar org-roam-capture-immediate-template
    (append (car org-roam-capture-templates) '(:immediate-finish t)))

  (defun org-roam-company-insert()
    "Hacky way to quickly initiate a similar functionality to
org-roam-insert-immediate, but using company.

Unused as of now as it does not create new nodes if existing ones are not found."
    (interactive)
    (require 'company)
    (insert "[[roam:")
    (save-excursion (insert "]]"))
    (call-interactively 'company-capf))

  (cl-defun +org-roam-node-find (&optional other-window initial-input filter-fn pred &key templates)
    "Copy of org-roam-node-find with the only change being the goto in
the capture call.

This causes the buffer to be ready and open
the capture popup."
    (interactive current-prefix-arg)
    (let ((node (org-roam-node-read initial-input filter-fn pred)))
      (if (org-roam-node-file node)
          (org-roam-node-visit node other-window)
        (org-roam-capture-
         :goto '(4)
         :node node
         :templates templates
         :props '(:finalize find-file)))))

  (defun org-roam-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (push arg args))
          (org-roam-capture-templates (list org-roam-capture-immediate-template)))
      (apply #'org-roam-node-insert args)))
  (defun +do-org-roam-bindings()
    (when (let ((file-name (buffer-file-name (buffer-base-buffer))))
            (and file-name (org-roam-file-p file-name)))
      (local-set-key (kbd "<C-i>") 'org-roam-insert-immediate)))

  (defun +org-roam-open-with-buffer-maybe-h ()
    (and (not org-roam-capture--node) ; don't proc for capture buffers
         (not (eq 'visible (org-roam-buffer--visibility)))
         (one-window-p t nil)
         (org-roam-buffer-toggle)))

  (defun +roam-preview-fetcher ()
    "Org roam preview to preview only the paragraph containing the link.
    Instead of the text till next heading or full file."
    (let* ((elem (org-element-context))
           (parent (org-element-property :parent elem)))
      ;; TODO: alt handling for non-paragraph elements
      (string-trim-right (buffer-substring-no-properties
                          (org-element-property :begin parent)
                          (org-element-property :end parent)))))

  ;; (add-hook 'org-roam-find-file-hook  '+org-roam-open-with-buffer-maybe-h :append)
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (require 'org-roam-protocol)
  ;; (add-hook 'find-file-hook 'open-org-roam)
  (add-hook 'org-mode-hook '+do-org-roam-bindings)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))


  (setq org-roam-preview-function #'+roam-preview-fetcher)

  (setq org-roam-completion-everywhere t)
  (setq org-roam-verbose nil)
  (setq org-roam-mode-section-functions '(org-roam-backlinks-section
                                          org-roam-reflinks-section
                                          org-roam-unlinked-references-section))
  (setq org-roam-graph-viewer (lambda(url) (+browse-url url))))

(use-package org-roam-ui
  :after org-roam
  :commands (org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package calfw-org
  :ensure calfw
  :ensure calfw-org
  :commands (+open-calendar)
  :bind (:map org-agenda-mode-map ("C" . +open-calendar)
              :map cfw:calendar-mode-map ("?" . +calendar/show-keys))
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


(use-package org-timeline
  :ensure nil
  :commands (org-timeline-insert-timeline)
  :hook (org-agenda-finalize . +org-insert-timeline)
  :config
  (defun +org-insert-timeline()
    (if (or (not (boundp 'org-ql-view-buffers-files))
            (not org-ql-view-buffers-files))
        (org-timeline-insert-timeline)))
  (setq org-timeline-space-out-consecutive t)
  (setq org-timeline-overlap-in-new-line t)
  (setq org-timeline-show-title-in-blocks t)
  (set-face-attribute 'org-timeline-block nil :inherit 'highlight :background nil))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-pretty-table :ensure nil
  :hook ((org-mode orgtbl-mode) . org-pretty-table-mode))


;; (use-package org-noter)
;; (use-package ox-clip
;;   :bind ("s-w". ox-clip-formatted-copy))
;; (use-package org-cliplink
;;   :bind ("C-c o y" . org-cliplink))

(use-package ox-pandoc
  :after ox)


(use-package org-web-tools)


(use-package anki-editor
  :after org
  :init
  (defvar anki-editor-mode-map (make-sparse-keymap) "anki-editor-mode keymap.")
  (add-to-list 'minor-mode-map-alist (cons 'anki-editor-mode
                                           anki-editor-mode-map))
  :bind (("C-c o k k" . +org-capture-anki-basic)
         ("C-c o k c" . +org-capture-anki-cloze)
         :map anki-editor-mode-map
         ("C-c C-c" . anki-editor-push-tree)
         ("<C-i>" . anki-editor-insert-note)
         ("C-c {" . 'anki-editor-cloze-region-auto-incr)
         ("C-c [" . 'anki-editor-cloze-region-dont-incr)
         ("C-c 0" . 'anki-editor-reset-cloze-number))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number)
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t)

  (defvar org-my-anki-file)
  (setq org-my-anki-file (concat +org-directory "anki.org"))

  (defvar +anki-editor-cloze-number 1)
  ;; Courtesy : yiufung
  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region +anki-editor-cloze-number "")
    (setq +anki-editor-cloze-number (1+ +anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- +anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq +anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number)
    (anki-refile-all))

  (defun anki-refile-all()
    "Refile all tree elements to Exported"
    (let ((notes (anki-editor-map-note-entries (lambda () (point-marker)) nil 'tree)))
      (mapc (lambda (marker)
              (goto-char marker)
              (+org-refile-to-pos org-my-anki-file "Exported"))
            notes)))

  (defun +init-anki()
    (when (string-match "\\(anki.org\\)" (format "%s" buffer-file-name))
      (progn
        (anki-editor-mode t))))

  (add-hook 'org-mode-hook '+init-anki)
  (add-to-list 'org-capture-templates
               '("a" "Anki basic"
                 entry
                 (file+headline org-my-anki-file "Dispatch Shelf")
                 "* %<%c> \n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Default\n:END:\n** Front\n%?\n** Back\n%x\n"))
  (add-to-list 'org-capture-templates
               '("A" "Anki cloze"
                 entry
                 (file+headline org-my-anki-file "Dispatch Shelf")
                 "* %<%c> \n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Default\n:END:\n** Text\n%x\n** Extra\n"))

  (defun anki-insert-new-note()
    (interactive)
    (anki-editor-reset-cloze-number)
    (anki-editor-insert-note))
  (defun +org-capture-anki-basic()
    (interactive)
    (org-capture nil "a"))

  (defun +org-capture-anki-cloze()
    (interactive)
    (org-capture nil "A")))

(use-package incremental-reading
  :commands (incremental-reading-extract-basic incremental-reading-extract-cloze +incremental-reading-extract-basic)
  :ensure nil
  :after org
  :bind (:map org-mode-map
              ("C-z k k" . +incremental-reading-extract-basic)
              ("C-z k p" . incremental-reading-parse-cards))
  :config
  (defvar incremental-reading-local-tags nil)
  (setq-default incremental-reading-local-tags nil)
  (defvar incremental-reading-basic-template-back-front
    ":ANKI-CARD:
#+ATTR_DECK: %s
#+ATTR_TYPE: Basic
#+ATTR_TAGS: %s
#+BEGIN_ANKI org
#+ATTR_FIELD: Front
#+BEGIN_FIELD
%s
#+END_FIELD

#+ATTR_FIELD: Back
#+BEGIN_FIELD
%s
#+END_FIELD
#+END_ANKI
:END:
")
  (defun +incremental-reading-extract-basic (prefix)
    "Extract current region into a basic anki card.  Differs from the
    default basic card creation by Asking for a question and
    using that as the front. And uses the selection as the answer.
    Also hides the block so as to keep the content clean."
    (interactive "P")
    (let* ((element (org-element-at-point))
           (selection-start (region-beginning))
           (selection-end (region-end))
           (tags (if (or (eq incremental-reading-local-tags nil) prefix)
                     (read-string "tags: ")
                   incremental-reading-local-tags))
           (question (read-string "Question : ")))
      (goto-char (org-element-property :end element))
      (setq-local incremental-reading-local-tags tags)
      (insert (format incremental-reading-basic-template-back-front
                      incremental-reading-default-deck
                      tags
                      question
                      (incremental-reading--extract-text selection-start
                                                         selection-end))))
    ;; Close the block as well
    (save-excursion
      (forward-line -1)
      (org-cycle))))


(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-ql
  :commands (+org-archive-archivable +org-show-archivable +org-show-pending)
  :config

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

  (defun +org-show-archivable()
    (interactive)
    (org-ql-search
      org-agenda-files
      `(closed :to ,(ts-format (ts-adjust 'day -60 (ts-now))))))
  (defun +org-archive-archivable()
    "Automatically archive all items that were closed 60 days ago or before"
    (interactive)
    (org-ql-select
      org-agenda-files
      `(closed :to ,(ts-format (ts-adjust 'day -60 (ts-now))))
      :action 'org-archive-subtree-default)))

(use-package org-tree-slide
  :after org
  :commands org-tree-slide-mode
  :bind (:map org-mode-map
              ("C-z P" . org-tree-slide-mode))
  :config
  (org-tree-slide-simple-profile)
  (setq org-tree-slide-activate-message " "
        org-tree-slide-deactivate-message " "
        org-tree-slide-modeline-display nil
        org-tree-slide-heading-emphasis t)
  (add-hook 'org-tree-slide-after-narrow-hook #'org-display-inline-images)
  (add-hook 'org-tree-slide-mode-hook #'+org-present-prettify-slide-h)
  (add-hook 'org-tree-slide-play-hook #'+org-present-hide-blocks-h)
  (defvar +org-present-text-scale 5
    "The `text-scale-amount' for `org-tree-slide-mode'.")
  (defun +org-present-hide-blocks-h ()
    "Hide org #+ constructs."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*\\(#\\+\\)\\(\\(?:BEGIN\\|END\\|ATTR\\)[^[:space:]]+\\).*" nil t)
        (org-flag-region (match-beginning 1)
                         (match-end 0)
                         org-tree-slide-mode
                         t))))
  (defvar +org-present--last-wconf nil)
  (defvar +org-present--last-line-num nil)
  (defun +org-present-prettify-slide-h ()
    "Set up the org window for presentation."
    (let ((arg (if org-tree-slide-mode +1 -1)))
      (if (not org-tree-slide-mode)
          (progn
            (when +org-present--last-wconf
              (set-window-configuration +org-present--last-wconf))
            (when +org-present--last-line-num
              (display-line-numbers-mode +org-present--last-line-num)))
        (setq +org-present--last-wconf (current-window-configuration))
        (delete-other-windows)
        (setq +org-present--last-line-num display-line-numbers-mode)
        (display-line-numbers-mode -1))
      (when (fboundp 'centered-window-mode)
        (setq-local cwm-use-vertical-padding t)
        (setq-local cwm-frame-internal-border 100)
        (setq-local cwm-left-fringe-ratio -10)
        (setq-local cwm-centered-window-width 300)
        (centered-window-mode arg))
      (hide-mode-line-mode arg)

      (cond (org-tree-slide-mode
             (set-window-fringes nil 0 0)
             (when (bound-and-true-p flyspell-mode)
               (flyspell-mode -1))
             ;; (add-hook 'kill-buffer-hook #'+org-present--cleanup-org-tree-slides-mode
             ;;           nil 'local)
             (text-scale-set +org-present-text-scale)
             (ignore-errors (org-latex-preview '(4))))
            (t
             (text-scale-set 0)
             (set-window-fringes nil fringe-mode fringe-mode)
             (org-clear-latex-preview)
             (org-remove-inline-images)
             (org-mode)))
      (redraw-display)))
  (defun +org-present--cleanup-org-tree-slides-mode ()
    (unless (cl-loop for buf in (doom-buffers-in-mode 'org-mode)
                     if (buffer-local-value 'org-tree-slide-mode buf)
                     return t)
      (org-tree-slide-mode -1)
      (remove-hook 'kill-buffer-hook #'+org-present--cleanup-org-tree-slides-mode
                   'local))))

(use-package org-excalidraw
  :ensure nil
  :commands (org-excalidraw-create-drawing)
  :config
  (setq org-excalidraw-directory (concat +org-directory "resource/excalidraw")))

(provide 'core-org)
;;; core-org.el ends here
