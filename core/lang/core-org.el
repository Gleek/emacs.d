;; Org mode settings
(defvar +org-directory "~/Dropbox/org-files/")
(defvar +roam-directory (concat +org-directory "org-roam/"))
(defvar +agenda-directory (concat +org-directory "gtd/"))
(defvar +ekg-directory (concat +org-directory "ekg/"))

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

  (defun +org-refile-to-pos (file headline &optional arg)
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (org-refile arg nil (list headline file nil pos)))
    (switch-to-buffer (current-buffer)))

  (defun org-formatted-copy (arg)
    "Export region to HTML, and copy it to the clipboard.
    Earlier used a textutil implementation to convert html to rtf
    and also a version which used pandoc to convert from org to
    rtf directly.  But rtf text is not easily supported
    everywhere.  This updated version uses a custom swift
    script (pbcopy-html) which transforms html to
    NSAttributedString. This seems like, can be pasted
    everywhere that supports some decent formatting."
    (interactive "P")
    (save-window-excursion
      (let* ((org-export-with-toc nil)
             (org-export-with-sub-superscripts nil)
             (org-html-checkbox-type 'unicode)
             ;; (org-export-smart-quotes-alist nil)
             (org-export-with-smart-quotes t)
             (export-type (if arg "rtf" "attributed"))
             (buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
             (html (with-current-buffer buf (buffer-string)))
             ;; Remove electric quotes as they get messed up in some applications
             (html (string-multi-replace '(("”" "\"") ("“" "\"") ("’" "'")) html)))
        (shell-command (format "pbcopy-html --type=%s %s" export-type (shell-quote-argument html)))
        (kill-buffer buf))))

  (defun org-formatted-paste()
    "Clipboard content in html is converted to org using pandoc and inserted to the buffer."
    (interactive)
    ;; pbpaste-html is generated using the trick at https://stackoverflow.com/a/36109230
    (shell-command "pbpaste-html | pandoc -f html -t org" (current-buffer)))

  (defun anonymous-pomodoro(&optional arg)
    "Start a 25 minute work or 5 min rest timer based on the prefix arg.
    Helpful in quickly starting a pomodoro for work and rest"
    (interactive "P")
    (if (and (boundp 'org-timer-countdown-timer) org-timer-countdown-timer)
        (org-timer-stop)
      (org-timer-set-timer (if arg "5" "25"))))

  (defun variable-pitch-for-notes ()
    (interactive)
    (when (string-match "\\(.*Notes.org\\|roam.*org\\|denote.*org\\)" (format "%s" buffer-file-name))
      (reading-mode)))

  (defun copy-file-link-for-org (file)
    "Copy current line in file to clipboard as an org file link"
    (interactive "f")
    (let ((org-link
           (format "[[file:%s][%s]]"
                   file
                   (file-name-nondirectory file))))
      (kill-new org-link)
      (message (format "%s file copied to clipboard"
                       (file-name-nondirectory file)))))

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

  (defun org-beautify-chars ()
    "Beautify Org Checkbox Symbol"
    ;; (push '("[ ]" .  "☐") prettify-symbols-alist)
    ;; (push '("[X]" . "☒" ) prettify-symbols-alist)
    ;; (push '("[-]" . "❍" ) prettify-symbols-alist)
    ;; (push '("->" . "⟶" ) prettify-symbols-alist)
    ;; (push '("=>" . "⟹") prettify-symbols-alist)
    (push '("--" . "—") prettify-symbols-alist)
    (prettify-symbols-mode))
  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")

  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)
  (add-hook 'org-mode-hook 'org-beautify-chars)


  (defun +capture-inbox()
    (interactive)
    (org-capture nil "i"))
  ;; trust certain code as being safe

  (defun +org-capture-template-from-file ()
    "Return the capture template read from a file."
    (with-temp-buffer
      (insert-file-contents (concat +agenda-directory "project_t.org"))
      (buffer-string)))

  (setq org-capture-templates
        '(("t" "Todo with project and category" entry
           (file+headline "~/path/to/inbox.org" "TODO")
           (function my/org-capture-template-from-file))))

  (setq org-capture-templates
        `(("i" "inbox" entry (file ,(concat +agenda-directory "inbox.org"))
           "* TODO %?")
          ("l" "link" entry (file ,(concat +agenda-directory "inbox.org"))
           "* TODO %(org-cliplink-capture)" :immediate-finish t)
          ("c" "org-protocol-capture" entry (file ,(concat +agenda-directory "inbox.org"))
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
          ("p" "project" entry (file ,(concat +agenda-directory "inbox.org"))
           (function +org-capture-template-from-file))))



  (defun +org-yank(arg)
    "Yanks image or text in the buffer"
    (interactive "P")
    (unless arg
      (let* ((is-image (and IS-MAC
                            (eq (call-process "pngpaste" nil nil nil "-b") 0)))
             (is-html (and IS-MAC
                           (not is-image)
                           (eq (call-process "pbpaste-html") 0))))

        (if is-image
            (progn
              (require 'org-download)
              (org-download-clipboard nil))
          (if is-html
              (org-formatted-paste)
            (org-yank nil)))))
    (if arg (org-yank nil)))


  (defun insert-org-link-to-lorien-file ()
    "Choose a file in resource/lorien interactively and insert its org file link in the current buffer."
    (interactive)
    (let ((file-path (read-file-name "Choose a file in resource/lorien: " "resource/lorien/")))
      (if (file-exists-p file-path)
          (let ((file-name (file-name-nondirectory file-path)))
            (insert (format "[[file:%s][%s]]" file-path file-name)))
        (message "File does not exist"))))



  (defun add-property-with-date-captured ()
    "Add CAPTURED property to the current item."
    (interactive)
    (org-set-property "CAPTURED" (format-time-string "[%F %a %R]")))

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

  (add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)
  (add-hook 'org-after-todo-state-change-hook 'org-entry-waiting-hook)
  (add-hook 'org-after-todo-state-change-hook 'org-entry-delegated-hook)
  (setq org-capture-bookmark nil)

  (setq org-ellipsis "⤶"
        ;; org-agenda-files `(,+agenda-directory)

        org-archive-location (concat +org-directory "archive.org::* From %s")
        org-startup-align-all-tables t
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
        org-auto-align-tags nil
        org-tags-column 0
        org-imenu-depth 8
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))
        org-export-with-sub-superscripts nil ;; "{}"
        org-use-sub-superscripts nil
        org-html-checkbox-type 'html
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-clock-sound (concat RES-DIR "bell.wav")
        org-id-link-to-org-use-id 'create-if-interactive
        org-todo-keywords '((sequence "TODO(t)" "DOING(o)"  "|" "DONE(d)")
                            (sequence "BLOCKED(b@/!)" "DELEGATED(e!)" "WAITING(w!)" "|" "CANCELLED(c@/!)"))
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-confirm-babel-evaluate nil
        org-pretty-entities t
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

  ;; (custom-set-faces
  ;;  '(org-block ((t (:inherit fixed-pitch))))
  ;;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-document-info ((t (:foreground "dark orange"))))
  ;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
  ;;  '(org-link ((t (:foreground "royal blue" :underline t))))
  ;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
  ;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  ;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  ;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  ;; (set-face-attribute 'org-ellipsis nil
  ;;                     :inherit '(font-lock-comment-face default)
  ;;                     :weight 'normal)


  ;; Support for plantuml
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t)
                                 (emacs-lisp . t)
                                 (shell . t)))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
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
         ("C-c o C" . org-capture)
         ("C-c o w" . copy-current-line-link-for-org)
         ("C-c o T" . anonymous-pomodoro)
         ("<f5>" . +capture-inbox)
         ("C-c o g" . consult-org-agenda)
         :map org-mode-map
         ("C-s-q" . org-fill-paragraph)
         ("s-w" . org-formatted-copy)
         ("C-<tab>" . nil))
  :bind* (:map org-mode-map
               ("C-y" . +org-yank)))

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
      (org-roam-refile)))

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

  (defun +agenda-skip-projects()
    "Every entry that has a sub TODO item is a project."
    (let ((has-subtasks nil))
      (org-narrow-to-subtree)
      (save-excursion
        (org-back-to-heading t)
        (when (org-goto-first-child)
          (setq has-subtasks (member (org-get-todo-state) org-not-done-keywords))
          (while (and (not has-subtasks) (org-goto-sibling))
            (when (member (org-get-todo-state) org-not-done-keywords)
              (setq has-subtasks t)))))
      (widen)
      (if has-subtasks
          (or (outline-next-heading) (point-max))
        nil)))

  (defun +agenda-skip-errands-worktime()
    (let ((tags (org-get-tags)))
      (when (and (member "errand" tags)
                 (let ((current-time (decode-time (current-time))))
                   (and (<= 1 (nth 6 current-time) 5) ; Monday to Friday
                        (<= 10 (nth 2 current-time) 17)))) ; 10 AM to 6 PM
        (save-excursion (or (outline-next-heading) (point-max))))))

  (defun +agenda-skip()
    (or (+agenda-skip-projects)
        (org-agenda-skip-entry-if 'scheduled 'deadline)
        (+agenda-skip-errands-worktime)))



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
                   (org-agenda-max-entries 10)))
            nil))))





  (setq org-agenda-files (mapcar (lambda(file) (concat +agenda-directory file)) '("inbox.org" "inbox_phone.org" "next.org" "someday.org"))
        org-agenda-window-setup 'current-window
        org-agenda-skip-unavailable-files t
        org-agenda-skip-scheduled-if-done t
        org-agenda-span 10
        org-agenda-block-separator (aref "━" 0)
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

;; (use-package org-gcal
;;   :commands (org-gcal-sync org-gcal-post-at-point)
;;   :after org-agenda
;;   :init
;;   (defvar org-gcal--running-timer nil)
;;   (unless (eq org-gcal--running-timer nil)
;;     (setq org-gcal--running-timer (run-with-timer 300 300 (lambda () (org-gcal-sync t t)))))
;;   :bind (:map org-agenda-mode-map
;;               ("S" . org-gcal-sync)
;;               ("P" . org-gcal-post-at-point))
;;   :config
;;   (defvar org-gcal-file)
;;   (setq org-gcal-file (concat +agenda-directory "schedule.org")
;;         org-gcal-dir (concat CACHE-DIR "org-gcal/")
;;         persist--directory-location (concat CACHE-DIR "persist")
;;         org-gcal-token-file (expand-file-name ".org-gcal-token" org-gcal-dir)
;;         org-gcal-notify-p nil)
;;   ;; Depends on core-secrets entry
;;   ;; (setq org-gcal-client-id "my-app.apps.googleusercontent.com"
;;   ;;       org-gcal-client-secret "secret"
;;   ;;       org-gcal-calendars '("calendar1" "calendar2" "calendar3"))

;;   (setq org-gcal-fetch-file-alist (mapcar (lambda(x) `(,x . ,org-gcal-file)) org-gcal-calendars)))


(use-package org-wild-notifier
  :defer 5
  :config
  (setq org-wild-notifier-alert-time '(5))
  (setq org-wild-notifier-keyword-whitelist nil)
  (org-wild-notifier-mode))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka") ;; package's recommended font
  (setq org-modern-label-border 0.2)
  ;; (defface solaire-org-modern-label
  ;;   `((t :inherit org-modern-label
  ;;        :box (:color ,(face-attribute 'solaire-default-face :background nil t))))
  ;;   "Alternative for org-modern-label")
  ;; (add-to-list 'solaire-mode-remap-alist '(org-modern-label . solaire-org-modern-label))


  (defface org-modern-priority-A
    '((t :inherit (org-modern-priority)))
    "Face for org priority A label")
  (defface org-modern-priority-B
    '((t :inherit (org-modern-priority)))
    "Face for org priority B label")
  (defface org-modern-priority-C
    '((t :inherit (org-modern-priority)))
    "Face for org priority C label")
  (setq org-modern-priority-faces
        (quote ((?A org-modern-priority-A)
                (?B org-modern-priority-B)
                (?C org-modern-priority-C))))
  (defvar priority-colors
    '((?A . (:light "OrangeRed3" :dark "orange red"))
      (?B . (:light "MediumPurple3" :dark "MediumPurple1"))
      (?C . (:light "RoyalBlue" :dark "DeepSkyBlue2")))
    "Colors for priority in org modern")

  (defun +reset-org-priority-colors(&rest _)
    "Resets the priority colors according to current them and priority-colors.

Works by changing `org-modern-priority-A/B/C' faces dynamically."
    (mapcar (lambda (pr)
              (set-face-attribute
               (intern (concat "org-modern-priority-" (char-to-string  pr)))
               nil
               :foreground (plist-get
                            (cdr (assoc pr priority-colors))
                            (intern (concat ":" (symbol-name +theme-type))))))
            '(?A ?B ?C))
    t)
  (add-hook 'enable-theme-functions '+reset-org-priority-colors)
  (+reset-org-priority-colors))

;; (use-package org-superstar
;;   :hook (org-mode . org-superstar-mode))
;; (use-package org-fancy-priorities
;;   :hook (org-mode . org-fancy-priorities-mode)
;;   :config
;;   (setq org-fancy-priorities-list '("⚑" "⇧" "⇩" "☕")))

(use-package org-download
  :bind (("C-c o d c" . org-download-clipboard)
         ("C-c o d d" . org-download-image)
         ("C-c o d x" . org-download-delete)
         ("C-c o d s" . org-download-screenshot)
         ("C-c o d y" . org-download-yank))
  :init
  (setq org-download-abbreviate-filename-function (lambda (str) str))

  (setq-default org-download-image-dir (concat +roam-directory "resource/downloads"))
  (when IS-MAC (setq org-download-screenshot-method "screencapture -i %s")))

;; Trying out ekg
(use-package ekg
  :init
  (setq ekg-db-file (concat +ekg-directory "ekg.db"))
  :bind (("C-c o u" . ekg-show-notes-with-all-tags)
         ("C-c o U" . ekg-capture)
         (:map ekg-capture-mode-map
               (("C-c C-c" . org-ctrl-c-ctrl-c)
                ("C-x C-s" . +ekg-capture-save)))
         (:map ekg-edit-mode-map
               (("C-c C-c" . org-ctrl-c-ctrl-c)
                ("C-x C-s" . +ekg-edit-save)))
         (:map ekg-notes-mode-map
               (("<return>" . ekg-notes-open)
                ("C-c C-o" . org-open-at-point))))
  :commands (ekg-capture ekg-capture-url ekg-show-notes-with-all-tags)
  :config
  (require 'org)
  (defun +ekg-logseq-sync(&rest args)
    (require 'ekg-logseq)
    (setq ekg-logseq-dir (concat +ekg-directory "logseq/"))
    (ekg-logseq-sync))
  (defun ekg-variable-pitch()
    (solaire-mode -1)
    (reading-mode))
  (defun +ekg-edit-save()
    (interactive)
    (cl-letf (((symbol-function #'kill-buffer) #'ignore))
      (call-interactively 'ekg-edit-finalize)))
  (defun +ekg-capture-save()
    (interactive)
    (cl-letf (((symbol-function #'kill-buffer) #'ignore))
      (call-interactively 'ekg-capture-finalize)))
  (defun +ekg-format-notes()
    (let ((inhibit-read-only t))
      (org-pretty-table-mode t)))
  (add-hook 'ekg-notes-mode-hook 'ekg-variable-pitch)
  (add-hook 'ekg-capture-mode-hook 'ekg-variable-pitch)
  (add-hook 'ekg-edit-mode-hook 'ekg-variable-pitch)
  ;; Formatting the org buffer with some missing formats
  (add-hook 'ekg-notes-mode-hook '+ekg-format-notes)
  (add-hook 'ekg-note-save-hook '+ekg-logseq-sync)


  (setq ekg-metadata-separator-text "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  ;; Force fixed-pitch for tags
  (set-face-attribute 'ekg-tag nil :inherit 'fixed-pitch)
  (set-face-attribute 'ekg-notes-mode-title nil :inherit 'fixed-pitch)
  (set-face-attribute 'ekg-metadata nil :inherit 'fixed-pitch))

;; Trying denote
;; (use-package denote
;;   :bind ("C-c n n" . denote-open-or-create)
;;   :init
;;   (setq xref-search-program 'ripgrep)
;;   (setq denote-directory (concat +org-directory "denote/"))
;;   :config
;;   (setq denote-known-keywords nil)
;;   (add-hook 'org-mode-hook '+denote-binding)
;;   (defun +denote-binding()
;;     (when (denote-file-is-note-p (buffer-file-name))
;;       (local-set-key (kbd "<C-i>") 'denote-link-or-create)
;;       (local-set-key (kbd "C-c r") 'denote-rename-file))))

;; (use-package consult-notes
;;   :bind ("C-c n n" . consult-notes)
;;   :config
;;   (require 'denote)
;;   (consult-notes-denote-mode))


(use-package org-roam
  :ensure org-roam
  ;; :ensure company-org-roam
  :init
  (setq org-roam-directory +roam-directory)
  (setq org-roam-dailies-directory +roam-directory)
  (setq org-roam-db-location (concat CACHE-DIR "org-roam.db"))
  :bind* (("C-c n n" . +org-roam-node-find))
  :bind (("C-c o m" . org-roam-buffer-toggle)
         ("C-c o s" . consult-org-roam-search)
         ("C-c o r d" . org-roam-dailies-goto-date)
         ("C-c o r r" . org-roam-dailies-goto-today)
         ("C-c n d" . org-roam-dailies-goto-today)
         ("C-c o r m" . org-roam-dailies-goto-tomorrow)
         ("C-c o r y" . org-roam-dailies-goto-yesterday)
         (:map org-mode-map
               ("C-z r t" . org-roam-tag-add)
               ("C-z C-w" . org-roam-refile)
               ("C-z C-W" . org-roam-extract-subtree)
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
  (add-to-list 'org-roam-file-exclude-regexp "logseq/")
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:50}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (defvar org-roam-capture-immediate-template
    (append (car org-roam-capture-templates) '(:immediate-finish t)))

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+datetree "journal.org" week))))


  (defun org-dblock-write:org-roam-backlinks-list (params)
    (let* ((id (plist-get params :id))
           (id (if id id (org-roam-node-id (org-roam-node-at-point))))
           (backlinks (org-roam-db-query
                       [:select [nodes:title nodes:id]
                                :from links
                                :inner :join nodes
                                :on (= nodes:id links:source)
                                :where (= dest $s1)
                                :and (= type "id")]
                       id))
           (fmt-fn (lambda (backlink)
                     (let ((link (car (cdr backlink)))
                           (name (car backlink)))
                       (org-make-link-string (format "id:%s" link)
                                             name)))))
      (dolist (backlink backlinks)
        (insert "- " (funcall fmt-fn backlink) "\n"))
      (delete-char -1)))



  (defun org-roam-node-graph()
    "Open the org-roam graph"
    (interactive)
    (org-roam-graph 1 (org-roam-node-at-point 'assert)))

  (defun org-roam-company-insert()
    "Hacky way to quickly initiate a similar functionality to
org-roam-insert-immediate, but using company.

Unused as of now as it does not create new nodes if existing ones are not found."
    (interactive)
    (require 'company)
    (insert "[[roam:")
    (save-excursion (insert "]]"))
    (call-interactively 'company-capf))

  (defun +org-roam-node-find ()
    "Copy of org-roam-node-find with the only change being the goto in
the capture call and use mtime to sort

This causes the buffer to be ready and open
the capture popup."
    (interactive current-prefix-arg)
    (let ((node (org-roam-node-read nil nil 'org-roam-node-read-sort-by-file-mtime)))
      (if (org-roam-node-file node)
          (org-roam-node-visit node)
        (org-roam-capture-
         :goto '(4)
         :node node
         :props '(:finalize find-file)))))

  (defun org-roam-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (push arg args))
          (org-roam-capture-templates (list org-roam-capture-immediate-template)))
      (apply #'org-roam-node-insert args)))
  (defun +do-org-roam-bindings()
    (when (let ((file-name (buffer-file-name (buffer-base-buffer))))
            (and file-name (org-roam-file-p file-name)))
      (local-set-key (kbd "<C-i>") 'org-roam-insert-immediate)
      (local-set-key (kbd "C-z t") 'org-roam-tag-add)))

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
  ;; (setq org-roam-graph-viewer (lambda(url) (+browse-url url)))
  (setq org-roam-graph-viewer (lambda(file) (display-buffer (find-file-noselect file))))
  (setq org-roam-graph-extra-config '(("rankdir" . "LR"))))

(use-package org-roam-ui
  :after org-roam
  :commands (org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-transclusion)

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
  :hook (org-agenda-finalize . +org-insert-timeline)
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
  :bind (:map org-agenda-mode-map
              ("C" . org-timeblock)))

(use-package org-appear
  ;; (setq org-hide-emphasis-markers t)
  :hook (org-mode . org-appear-mode))

(use-package org-pretty-table :ensure nil
  :hook (orgtbl-mode . org-pretty-table-mode))


;; (use-package org-noter)
;; (use-package ox-clip
;;   :bind ("s-w". ox-clip-formatted-copy))
;; (use-package org-cliplink
;;   :bind ("C-c o y" . org-cliplink))

(use-package ox-pandoc
  :demand t
  :after (ox)
  :config
  (defun org-pandoc-export-to-custom (&optional a s v b e)
    "Export to custom."
    (interactive)
    (let* ((format (intern (read-string "Enter the export format: ")))
           (options-var (intern (format "org-pandoc-options-for-%s" format)))
           (options (if (boundp options-var) (symbol-value options-var) nil)))
      (org-pandoc-export format a s v b e t)))
  (defun org-pandoc-export-to-custom-and-open (&optional a s v b e)
    "Export to custom and open."
    (interactive)
    (let* ((format (intern (read-string "Enter the export format: ")))
           (options-var (intern (format "org-pandoc-options-for-%s" format))))
      (unless (boundp options-var)
        (set options-var nil))
      (org-pandoc-export format a s v b e 0)))
  (add-to-list 'org-pandoc-menu-entry '(?. "to custom" org-pandoc-export-to-custom))
  (add-to-list 'org-pandoc-menu-entry '(?, "to custom and open" org-pandoc-export-to-custom-and-open)))


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
      `(closed :to ,(ts-format (ts-adjust 'day -60 (ts-now))))))
  (defun +org-archive-archivable()
    "Automatically archive all items that were closed 60 days ago or before"
    (interactive)
    (org-ql-select
      org-agenda-files
      `(closed :to ,(ts-format (ts-adjust 'day -60 (ts-now))))
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

(use-package org-re-reveal
  :commands (+org-re-reveal-run)
  :config
  (setq org-re-reveal-root  (concat CACHE-DIR "reveal.js")
        org-re-reveal-revealjs-version "4"
        +org-re-reveal-exact-working-version "5.1.0")

  (defun +org-re-reveal-run()
    (interactive)
    (org-re-reveal-setup (lambda ()
                           (org-re-reveal-export-to-html-and-browse))))

  (defun org-re-reveal-setup(callback)
    ;; Check if directory org-re-reveal-root exists
    (if (file-exists-p org-re-reveal-root)
        ;; If it exists, directly call the callback
        (funcall callback)
      ;; Otherwise, download the repository
      (progn
        (message "Reveal.js directory not found. Downloading...")
        (let ((process (start-process-shell-command
                        "git-clone-reveal.js"
                        "*git-clone-reveal.js*"
                        (format "git clone --depth 1 --branch %s %s %s"
                                +org-re-reveal-exact-working-version
                                "https://github.com/hakimel/reveal.js"
                                org-re-reveal-root))))
          ;; Set a sentinel to run the callback when the process finishes
          (set-process-sentinel process
                                (lambda (proc event)
                                  (when (string= event "finished\n")
                                    (funcall callback)))))))))



(use-package org-tree-slide
  :after org
  :commands org-tree-slide-mode
  :bind (:map org-mode-map
              ("C-z P" . org-tree-slide-mode))
  :config
  (org-tree-slide-presentation-profile)
  (setq org-tree-slide-activate-message " "
        org-tree-slide-deactivate-message " "
        org-tree-slide-modeline-display nil
        org-tree-slide-header t
        org-tree-slide-heading-emphasis t
        +org-present-hide-first-heading t)
  (add-hook 'org-tree-slide-after-narrow-hook #'org-display-inline-images)
  (add-hook 'org-tree-slide-mode-hook #'+org-present-prettify-slide-h)
  ;; (add-hook 'org-tree-slide-play-hook #'+org-present-hide-blocks-h)
  (defvar +org-present-text-scale 3
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
             (spell-fu-mode -1)
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

  (defun +org-tree-slide--set-slide-header (blank-lines)
    "Set the header with overlay.
Some number of BLANK-LINES will be shown below the header."
    (org-tree-slide--hide-slide-header)
    (setq org-tree-slide--header-overlay
          (make-overlay (point-min) (+ 1 (point-min))))
    (overlay-put org-tree-slide--header-overlay 'after-string " ")
    (overlay-put org-tree-slide--header-overlay
                 'face
                 'org-tree-slide-header-overlay-face)
    (if org-tree-slide-header
        (overlay-put org-tree-slide--header-overlay 'display
                     (concat (when org-tree-slide-breadcrumbs
                               (concat (org-tree-slide--get-parents
                                        org-tree-slide-breadcrumbs)))
                             (org-tree-slide--get-blank-lines blank-lines)))
      (overlay-put org-tree-slide--header-overlay 'display
                   (org-tree-slide--get-blank-lines blank-lines))))
  ;; (advice-add 'org-tree-slide--set-slide-header :override #'+org-tree-slide--set-slide-header)
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
