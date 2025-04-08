(setq url-configuration-directory (concat CACHE-DIR "url"))
(setq gamegrid-user-score-file-directory (concat CACHE-DIR "games"))
(setq request-storage-directory (concat CACHE-DIR "request"))
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun html2text ()
  "Replacement for standard html2text using shr."
  (interactive)
  (shr-render-region (point-min) (point-max))
  (goto-char (point-min)))


(defun capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

(defun insert-uuid ()
  (interactive)
  (shell-command "echo -n \"$(uuidgen)\"" t))

(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun open-with-dragger(file)
  (interactive "f")
  ;; Utility to drag and drop files to outside world
  ;; Details: https://github.com/gleek/dragger
  (start-process-shell-command "dragger" nil (concat "dragger " file)))

(defun open-this-with-dragger()
  (interactive)
  (open-with-dragger (buffer-file-name)))

(defun remind (time message)
  "Set a reminder to notify after TIME with MESSAGE.
TIME is a string consisting of a number followed by 's', 'm', or 'h'. (e.g., 10s, 5m, 1h)"
  (interactive "sTime: \nsMessage: ")
  (let* ((unit (substring time -1))
         (number (string-to-number (substring time 0 -1)))
         (seconds (cond
                   ((string= unit "s") number)
                   ((string= unit "m") (* 60 number))
                   ((string= unit "h") (* 3600 number))
                   (t (error "Invalid time format. Use 's' for seconds, 'm' for minutes, or 'h' for hours.")))))
    (run-at-time seconds
                 nil
                 (lambda (msg)
                   (alert msg :title "Reminder")
                   (play-sound-file (concat RES-DIR "bell.wav")))
                 message)))

(use-package emacs :ensure nil
  :bind (("C-c r". rename-file-and-buffer)
         ([f5] . kmacro-edit-macro)))

(use-package image-mode :ensure nil
  :init
  (defun +scale-image()
    (when (eq major-mode 'image-mode)
      (scale-image)))
  (defun scale-image-register-hook ()
    "Register the image scaling hook."
    (add-hook 'text-scale-mode-hook '+scale-image))

  :hook (image-mode . scale-image-register-hook)
  :bind (:map image-mode-map
              ("C-f" . image-forward-hscroll-small)
              ("C-b" . image-backward-hscroll-small)
              ("M-f" . image-forward-hscroll-large)
              ("M-b" . image-backward-hscroll-large)
              ("C-n" . image-forward-vscroll-small)
              ("C-p" . image-backward-vscroll-small)
              ("t" . image-get-ocr-text)
              ("T" . image-get-ocr-pdf))
  :config
  (defun image-get-ocr-text()
    (interactive)
    (async-shell-command (format "tesseract %s -" (shell-quote-argument (buffer-file-name)))
                         (format "*tesseract-ocr-%s*"
                                 (file-name-nondirectory (buffer-file-name)))))

  (defun image-get-ocr-pdf()
    (interactive)
    (let ((file (make-temp-file "ocr-")))
      (message "pdf file %s" file)
      (shell-command (format "tesseract %s %s --psm 12 pdf" (shell-quote-argument (buffer-file-name)) file)
                     nil nil)
      (display-buffer (find-file-noselect (format "%s.pdf" file)))))

  (defun image-forward-hscroll-small()
    (interactive)
    (image-forward-hscroll 5))

  (defun image-backward-hscroll-small()
    (interactive)
    (image-backward-hscroll 5))

  (defun image-forward-hscroll-large()
    (interactive)
    (image-forward-hscroll 30))

  (defun image-backward-hscroll-large()
    (interactive)
    (image-backward-hscroll 30))

  (defun image-forward-vscroll-small()
    (interactive)
    (image-next-line 5))

  (defun image-backward-vscroll-small()
    (interactive)
    (image-previous-line 5))

  (defun image-forward-vscroll-large()
    (interactive)
    (image-next-line 30))

  (defun image-backward-vscroll-large()
    (interactive)
    (image-previous-line 30))

  (defun scale-image ()
    "Scale the image by the same factor specified by the text scaling."
    (image-transform-set-scale
     (expt text-scale-mode-step
           text-scale-mode-amount))))

(use-package eimp
  :hook (image-mode . eimp-mode)
  :bind (:map eimp-minor-mode-map
              (("<S-down-mouse-1>" . eimp-crop-mouse)))
  :config
  (defun +eimp-remove-background (arg)
    (interactive "P")
    (require 'eimp)
    (let* ((file-name (buffer-file-name))
           (is-jpg (and file-name (or (string= (file-name-extension file-name) "jpg")
                                      (string= (file-name-extension file-name) "jpeg"))))
           (convert-to-png (and is-jpg (y-or-n-p "Convert JPG to PNG before background removal?")))
           (png-file (when convert-to-png (concat (file-name-sans-extension file-name) ".png")))
           ;; Get top 5 colors
           (color-list-command (format "convert %s -format '%%c' -colors 5 histogram:info:- | sort -r | grep -o '#[0-9A-Fa-f]\\{6\\}' | sed 's/(standard input)://'"
                                       (shell-quote-argument file-name)))
           (color-list (split-string (shell-command-to-string color-list-command) "\n" t))
           (chosen-color (if arg
                             (completing-read "Choose a color: " color-list)
                           (car color-list))) ; Use completing-read if arg is provided, otherwise choose the top candidate
           (fuzz-input (if arg
                           (read-string "Enter fuzz percentage: " "20")
                         "20"))
           (fuzz (format "%s%%" fuzz-input))
           (mogrify-args (list "-fuzz" fuzz "-transparent" chosen-color)))
      (if (and is-jpg (not convert-to-png))
          (error "Cannot make JPG images transparent without converting to PNG"))
      (when convert-to-png
        (shell-command (format "convert %s %s" (shell-quote-argument file-name) (shell-quote-argument png-file)))
        (find-file png-file))
      (eimp-mogrify-image mogrify-args)))

  (defun eimp-crop-mouse(event)
    ;; FIXME: co-ordinates and mouse position don't match
    (interactive "e")
    (let* ((window (posn-window (event-start event)))
           (event-start (event-start event))
           end
           message-log-max
           image-size image-width image-height
           width-ratio height-ratio ratio
           dx dy dx-dy x-y start-x-y)
      (mouse-set-point event)
      ;; Image at or just before point
      (unless (eimp-get-display-property)
        (backward-char))
      (cond
       ((not (posn-image event-start))
        (message "No image at mouse"))
       (t
        (setq image-size (image-size (eimp-get-image) t)
              image-width (car image-size)
              image-height (cdr image-size))
        (setq start-x-y (eimp-frame-relative-coordinates event-start)
              dx-dy (posn-object-x-y event-start))
        (setq start-x-y (cons (- (car start-x-y) (car dx-dy))
                              (- (cdr start-x-y) (cdr dx-dy))))
        (track-mouse
          (while (progn
                   (setq event (read-event))
                   (or (mouse-movement-p event)
                       (memq (car-safe event) '(switch-frame select-window))))

            (if (memq (car-safe event) '(switch-frame select-window))
                nil
              (setq end (event-end event))
              (if (numberp (posn-point end))
                  (progn
                    (setq x-y (eimp-frame-relative-coordinates end)
                          dx (- (car x-y) (car start-x-y))
                          dy (- (cdr x-y) (cdr start-x-y))))
                (setq dx -1 dy -1))
              (if (or (< dx 0) (< dy 0))
                  (message "Not cropping image")
                (message "Cropping image from %dx%d to %dx%d"
                         image-width image-height dx dy)))))
        (when (and (> dx 0) (> dy 0))
          (eimp-mogrify-image
           `("-crop" ,(concat (format "%dx%d+%d+%d" image-width image-height dx dy))))))))))


(use-package transient
  :init
  (setq transient-levels-file (concat CACHE-DIR "transient/levels.el")
        transient-values-file (concat CACHE-DIR "transient/values.el")
        transient-history-file (concat CACHE-DIR "transient/history.el")))

(use-package simple-http
  :ensure nil
  :config
  (defun httpd-start-here (directory port)
    (interactive (list (read-directory-name "Root directory: " default-directory nil t)
                       (read-number "Port: " 8017)))
    (setq httpd-root directory)
    (setq httpd-port port)
    (httpd-start)
    (browse-url (concat "http://localhost:" (number-to-string port) "/")))

  (setq httpd-root "~/Development/testing"))

;; (use-package zeal-at-point
;;   :config (setq zeal-at-point-zeal-version "0.3.1"))
;; (use-package howdoi :disabled t)

(when IS-MAC
  (use-package osx-dictionary
    :config
    (set-popup-rule! "^\\*osx-dictionary\\*" :size 0.4 :quit t)
    :bind (("C-c s D" . osx-dictionary-search-input))))


(use-package alert
  :init
  (defvar alert-default-style)
  (setq alert-default-style (if IS-MAC 'notifier 'libnotify)))



(use-package helpful
  :init
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))


(use-package async
  :init
  (setq async-byte-compile-log-file (concat CACHE-DIR "async-bytecomp.log")))

;; (use-package scratch)

(use-package restclient
  :hook (restclient-mode . display-line-numbers-mode)
  :mode ("\\.rest\\'" . restclient-mode)
  :bind (:map restclient-mode-map (("C-c C-c" . restclient-http-send-current-stay-in-window)
                                   ("C-c C-v" . restclient-http-send-current)
                                   ("C-c n n" . nil)))
  :config
  (set-popup-rule! "^\\*HTTP Response" :size 0.4 :quit 'other)
  (set-popup-rule! "^\\*Restclient Info" :size 0.4 :quit 'other)
  (add-hook 'restclient-mode-hook
            (lambda()
              (setq imenu-generic-expression '((nil "^[A-Z]+\s+.+" 0)))))

  (defvar +restclient-debug nil)
  (setq +restclient-debug t)
  (setq restclient-inhibit-cookies t)

  (defun +restclient-copy-curl-command ()
    "Formats the request as a curl command and copies the command to the clipboard."
    (interactive)
    (restclient-http-parse-current-and-do
     '(lambda (method url headers entity)
        (let* ((header-args
                (apply 'append
                       (mapcar (lambda (header)
                                 (list "-H" (format "\"%s: %s\"" (car header) (cdr header))))
                               headers)))
               (header-parsed (mapconcat 'identity header-args " "))
               (method-arg (concat "-X" " " method))
               (entity-arg (if (> 0 (string-width entity)) ""
                             (format "-d \x27%s\x27" entity)))
               (curl-command (format "curl %s %s %s %s" header-parsed method-arg url entity-arg)))
          (kill-new curl-command)
          (message "curl command copied to clipboard.")))))

  ;; (defun +rest-client-http-call (orig-fn &rest args)
  ;;   "Make a few modifications before making the call"
  ;;   (let ((gnutls-verify-error t)
  ;;         ;; TODO fix
  ;;         (url-debug +restclient-debug))
  ;;     (message "URL debug => %s" url-debug)
  ;;     (apply orig-fn args)))
  ;; (advice-add '+rest-client-http-call :around #'restclient-http-do)
  )

(use-package verb
  :after org
  :demand t
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))


(use-package speed-type
  :config
  (defun +speed-type-setup (&rest _)
    ;; (variable-pitch-mode t)
    (setq cursor-type 'bar)
    (olivetti-mode)
    (text-scale-set 1))
  (defun +speed-type-skill (wpm)
    "Updated skill according to my targets!"
    (cond ((< wpm 55) "Beginner")
          ((< wpm 60) "Intermediate")
          ((< wpm 70) "Average")
          ((< wpm 85) "Pro")
          ((< wpm 110) "Master")
          (t          "Racer")))
  (advice-add 'speed-type--skill :override '+speed-type-skill)
  (advice-add 'speed-type--setup :after '+speed-type-setup)
  (setq speed-type-default-lang 'English)
  (setq speed-type-gb-dir (concat CACHE-DIR "speed-type/")))


(when IS-MAC
  (use-package exec-path-from-shell
    :demand
    :config
    ;; don't load interactive shell
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "LC_ALL" "LANG"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

(use-package system-packages
  :bind (("C-c P s" . system-packages-search)
         ("C-c P i" . system-packages-install)
         ("C-c P u" . system-packages-uninstall)
         ("C-c P U" . system-packages-update)
         ("C-c P l" . system-packages-list-installed-packages))
  :config
  (set-popup-rule! "^\\*system-packages\\*" :size 0.4 :quit 'other))

(use-package bongo
  :bind (("C-c b s" . bongo-seek)
         ("C-c b b" . +jump-to-music)
         ("C-c b P" . bongo-playlist)
         ("C-c b i" . bongo-show)
         ("C-c b n" . bongo-play-next)
         ("C-c b x" . +bongo-playlist-terminate)
         ("C-c b R" . +bongo-playlist-random-toggle)
         ("C-c b r" . bongo-play-random)
         ("C-c b p" . bongo-play-previous)
         ("C-c b SPC" . bongo-pause/resume))
  :config
  ;; Courtesy: Protesilaos
  (defun +bongo-playlist-reset ()
    "Stop playback and reset `bongo' playlist marks.
To reset the playlist is to undo the marks produced by non-nil
`bongo-mark-played-tracks'."
    (interactive)
    (when (bongo-playlist-buffer-p)
      (bongo-stop)
      (bongo-reset-playlist)))
  (defun +bongo-playlist-random-toggle ()
    "Toggle `bongo-random-playback-mode' in playlist buffers."
    (interactive)
    (with-bongo-playlist-buffer
     (if (eq bongo-next-action 'bongo-play-random-or-stop)
         (bongo-progressive-playback-mode)
       (bongo-random-playback-mode))))
  (defun +bongo-playlist-play-random()
    (interactive)
    (unless (bongo-playlist-buffer)
      (bongo-playlist-buffer))
    (when (or (bongo-playlist-buffer-p)
              (bongo-library-buffer-p))
      (unless (bongo-playing-p)
        (with-current-buffer (bongo-playlist-buffer)
          (bongo-play-random)
          (bongo-random-playback-mode)
          (bongo-recenter)))))
  (defun +bongo-playlist-terminate ()
    "Stop playback and clear the entire `bongo' playlist buffer.
Contrary to the standard `bongo-erase-buffer', this also removes
the currently playing track."
    (interactive)
    (when (bongo-playlist-buffer-p)
      (bongo-stop)
      (bongo-erase-buffer)))
  (setq bongo-default-directory "~/Music")
  (setq bongo-prefer-library-buffers nil)
  (setq bongo-mark-played-tracks t)
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-display-inline-playback-progress t)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-header-line-mode nil)
  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)

  (setq bongo-enabled-backends '(mpv))
  (setq bongo-vlc-program-name "vlc")

  ;; Courtesy: Protesilaos
  (defun +bongo-playlist-buffer-no-banner ()
    "Set up a Bongo playlist buffer without its header commentary.
To be advised as override for `bongo-default-playlist-buffer'.
To actually enable this, evaluate `+bongo-remove-headers'."
    (with-current-buffer (get-buffer-create bongo-default-playlist-buffer-name)
      (unless (derived-mode-p 'bongo-playlist-mode)
        (bongo-playlist-mode))
      (current-buffer)))
  (defun +bongo-library-buffer-no-banner ()
    "Set up a Bongo library buffer without its header commentary.
To be advised as override for `bongo-default-library-buffer'.

To actually enable this, evaluate `+bongo-remove-headers'."
    (with-current-buffer (get-buffer-create bongo-default-library-buffer-name)
      (unless (derived-mode-p 'bongo-library-mode)
        (bongo-library-mode))
      (current-buffer)))
  (defun +bongo-remove-headers ()
    "Remove comment headers from Bongo buffers."
    (advice-add 'bongo-default-playlist-buffer :override #'+bongo-playlist-buffer-no-banner)
    (advice-add 'bongo-default-library-buffer :override #'+bongo-library-buffer-no-banner))
  (+bongo-remove-headers)

  (defun +jump-to-music()
    (interactive)
    (dired bongo-default-directory)
    (bongo-dired-library-mode)
    (setq-local bongo-playlist-buffer bongo-default-playlist-buffer-name)))


(use-package keepass-mode)
(use-package keepass-mode-plus
  :ensure nil
  :bind (("C-c s p" . keepass-quick-switch)
         (:map keepass-mode-map
               ("s" . +keepass-search)))
  :config
  (setq keepass-password-file (secret-get keepass-password-file)))

(use-package totp
  :commands (totp-copy-pin-as-kill totp-display)
  :bind ("C-c s P" . totp-display)
  :config
  (add-to-list 'auth-sources (secret-get auth-source-file))
  ;; epg version >= 2.4.1 doesn't work with emacs without it
  ;; (fset 'epg-wait-for-status 'ignore)
  ;; This shouldn't be done as it might corrupt files
  (defvar auth-source-timer nil)
  ;; Courtesy : Mickey Peterson
  (defun totp-display (auth)
    "Select a TOTP AUTH from `auth-sources' and display its TOTP."
    (interactive
     (list
      (let ((candidates (mapcar
                         (lambda (auth)
                           (cons (format "User '%s' on %s"
                                         (propertize (plist-get auth :user) 'face 'font-lock-keyword-face)
                                         ;; removes "TOTP:" prefix from the host name
                                         (propertize (substring (plist-get auth :host) 5) 'face 'font-lock-string-face))
                                 auth))
                         (seq-filter (lambda (auth) (string-prefix-p "TOTP:" (plist-get auth :host)))
                                     (auth-source-search :max 10000)))))
        (if auth-source-timer
            (cancel-timer auth-source-timer))
        (setq auth-source-timer (run-with-timer 100 nil (lambda () (auth-source-forget-all-cached))))
        (cdr (assoc (completing-read "Pick a TOTP> " candidates) candidates)))))
    (let ((code (totp (funcall (plist-get auth :secret)))))
      (message "Your TOTP for '%s' is: %s (sent to kill ring)"
               (propertize (plist-get auth :host) 'face font-lock-keyword-face)
               (propertize code 'face 'font-lock-string-face))
      (kill-new code)
      code)))

(use-package proced
  :ensure nil
  :commands proced
  :config
  (setq-default proced-auto-update-flag t)
  (setq proced-auto-update-interval 1)
  (setq proced-descend t)
  (setq proced-filter 'user))
(use-package proced-narrow
  :after proced
  :diminish
  :bind (:map proced-mode-map
              ("/" . proced-narrow)))

(use-package calc
  :ensure nil
  :bind ("C-*" . calc)
  :config
  ;; TODO: fixme error("Eager macro-expansion failure: %S" (void-variable ))
  ;; (defmath tzconv(dt tz &optional tz2)
  ;;   ;; if only tz is present assume that the dt is in local timezone and convert to tz
  ;;   ;; if both tz and tz2 are present assume dt is in tz timezone and convert to tz2
  ;;   (if tz2
  ;;       :"unixtime(unixtime(dt) + tzone(tz) - tzone(tz2))"
  ;;       :"unixtime(unixtime(dt)+tzone()-tzone(tz))"))
  (setq math-additional-units
        '(
          (GiB "1024 * MiB" "Giga Byte")
          (MiB "1024 * KiB" "Mega Byte")
          (KiB "1024 * B" "Kilo Byte")
          (B nil "Byte")
          (Gib "1024 * Mib" "Giga Bit")
          (Mib "1024 * Kib" "Mega Bit")
          (Kib "1024 * b" "Kilo Bit")
          (b "B / 8" "Bit"))
        math-units-table nil))

(use-package vertico-calc :ensure nil
  :after vertico
  :bind ("M-*" . vertico-calc))

(use-package calc-currency
  :ensure (:fetcher github :repo "jws85/calc-currency")
  :commands (calc-currency-load)
  :after (calc)
  :demand t
  :config
  ;; (calc-currency-update-file)
  (calc-currency-load))


(use-package shortdoc :ensure nil
  :bind ("C-h s" . shortdoc-display-group))

(use-package impostman :ensure nil
  :commands (impostman-import-file impostman-import-string +impostman-import-only-collection)
  :config
  (defun +impostman-import-only-collection()
    (interactive)
    (let* ((collection (impostman-read-collection-filename))
           (output-name "restclient")
           (output-alist (impostman--get-output-alist output-name)))
      (impostman-parse-file collection nil output-alist))))


;; (use-package package-safe-delete)

(use-package artist
  :ensure nil
  :hook (artist-mode . reset-line-spacing)
  :config
  (defun reset-line-spacing()
    (setq-local line-spacing 0.0)))
(use-package ascii-art-to-unicode)

(use-package epaint :ensure nil
  :load-path "packages/epaint/"
  :commands (epaint))

(use-package +sync-remote
  :defer 2
  ;; Need to load early so that the custom variables can be marked safe to be used in .dir-locals.
  :bind ("C-c x a z" . +sync-remote-start)
  :ensure nil)

(use-package salahtimes
  :ensure nil
  :bind ("C-c a s" . salah-times)
  :commands (salah-times)
  :config
  (setq salah-city (secret-get city))
  (setq salah-country (secret-get country))
  (setq salah-school (secret-get salah-school))
  (setq salah-next-days 2)
  (set-popup-rule! "^ \\*salah-times\\*" :select nil :size '+popup-shrink-to-fit))


(use-package gptel
  :commands (gptel gptel-send)
  :bind (("C-c q s" . gptel-send)
         ("C-c q r" . gptel-rewrite)
         ("C-c q c" . gptel)
         ("C-c q m" . gptel-menu))
  :config
  (setq gptel-model 'gemini-2.0-flash-exp)
  (setq gptel-default-mode 'org-mode)
  (setq-default gptel-org-branching-context nil)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*** @me")
  (setq gptel-api-key (secret-get openai-key))
  (setq gptel-backend (gptel-make-gemini "Gemini" :key (secret-get gemini-key) :stream t))

  (gptel-make-openai "TogetherAI"
    :host "api.together.xyz"
    :key (secret-get together-ai-key)
    :stream t
    :models '(;; has many more, check together.ai
              mistralai/Mixtral-8x7B-Instruct-v0.1
              codellama/CodeLlama-13b-Instruct-hf
              codellama/CodeLlama-34b-Instruct-hf))
  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key (secret-get groq-key)
    :models '(llama-3.3-70b-versatile
              llama3-70b-8192
              deepseek-r1-distill-qwen-32b
              deepseek-r1-distill-llama-70b)))

(use-package aidermacs
  :bind ("C-c q a" . aidermacs-transient-menu)
  :ensure (:fetcher github :repo "MatthewZMD/aidermacs")
  :config
  ;; (setq aidermacs-default-model "anthropic/claude-3-5-sonnet-20241022")
  (setq aidermacs-default-model "o3-mini")
  (setq aidermacs-editor-model "o3-mini")
  (setq aidermacs-architect-model "o1-mini")
  (setenv "OPENAI_API_KEY" (secret-get openai-key))
  (setenv "AIDER_CHAT_LANGUAGE" "english")
  (setq aidermacs-use-architect-mode t))


(when IS-MAC
  (defun play-sound-mac(sound)
    (start-process "org-clock-play-notification" nil
                   "afplay" (plist-get (cdr sound) :file)))
  (advice-add 'play-sound :override 'play-sound-mac))

(use-package emacs
  :ensure nil
  :init
  (setq nsm-settings-file (concat CACHE-DIR "network-security.data")))

(use-package keycast
  :bind ("C-c t k" . +toggle-keycast)
  :config
  (setq keycast-mode-line-insert-after '(:eval (doom-modeline-format--main)))
  (defun +toggle-keycast()
    (interactive)
    (if (member '("" keycast-mode-line) global-mode-string)
        (progn (setq global-mode-string (delete '("" keycast-mode-line) global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast disabled"))
      (add-to-list 'global-mode-string '("" keycast-mode-line))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast enabled"))))


(provide 'core-tools)
;;; core-tools.ends here
