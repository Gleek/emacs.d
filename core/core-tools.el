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
              ("C-p" . image-backward-vscroll-small))
  :config

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
  :config
  (defun +eimp-remove-background(arg)
    (interactive "P")
    (let ((color (+choose-color "Background color: "))
          (fuzz (if arg (format "%d%%" arg) "15%")))
      (eimp-mogrify-image (list "-fuzz" fuzz "-transparent" color))))

  (defun +choose-color(prompt)
    (let* ((colors
            (delete nil
                    (mapcar (lambda (cell)
                              (let* ((name (car cell))
                                     (dups (cdr cell))
                                     (hex (counsel-colors--name-to-hex name)))
                                (when hex
                                  (propertize name 'hex hex 'dups dups))))
                            (list-colors-duplicates))))
           (counsel--colors-format
            (format "%%-%ds %%s %%s%%s"
                    (apply #'max 0 (mapcar #'string-width colors))))
           (chosen-color (ivy-read prompt colors
                                   :require-match t
                                   :history 'counsel-colors-emacs-history
                                   :caller 'counsel-colors-emacs)))
      (get-text-property 0 'hex chosen-color))))


(use-package paradox
  :ensure t
  :init
  (defvar paradox-automatically-star)
  (defvar paradox-execute-asynchronously)
  (setq paradox-automatically-star t)
  (setq paradox-execute-asynchronously t))

;; (use-package debbugs)

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
  (global-set-key [remap describe-symbol]   #'helpful-symbol)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))


(use-package async
  :init
  (setq async-byte-compile-log-file (concat CACHE-DIR "async-bytecomp.log")))

;; (use-package scratch)

(use-package restclient
  :ensure restclient
  :ensure company-restclient
  :hook (restclient-mode . display-line-numbers-mode)
  :mode ("\\.rest\\'" . restclient-mode)
  :bind (:map restclient-mode-map (("C-c C-c" . restclient-http-send-current-stay-in-window)
                                   ("C-c C-v" . restclient-http-send-current)))
  :config
  (set-popup-rule! "^\\*HTTP Response" :size 0.4 :quit 'other)
  (set-popup-rule! "^\\*Restclient Info" :size 0.4 :quit 'other)
  (company-backend-for-hook 'restclient-mode-hook '((company-restclient company-yasnippet)))
  (add-hook 'restclient-mode-hook
            (lambda()
              (setq imenu-generic-expression '((nil "^[A-Z]+\s+.+" 0)))))

  (defvar +restclient-debug nil)
  (setq +restclient-debug t)

  (defun +rest-client-http-call (orig-fn &rest args)
    "Make a few modifications before making the call"
    (let ((gnutls-verify-error t)
          ;; TODO fix
          (url-debug +restclient-debug))
      (message "URL debug => %s" url-debug)
      (apply orig-fn args)))
  (advice-add '+rest-client-http-call :around #'restclient-http-do))


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
               ("s" . +keepass-search))))


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
  :bind ("C-*" . calc)
  :config
  (defmath tzconv(dt tz &optional tz2)
    ;; if only tz is present assume that the dt is in local timezone and convert to tz
    ;; if both tz and tz2 are present assume dt is in tz timezone and convert to tz2
    (if tz2
        :"unixtime(unixtime(dt) + tzone(tz) - tzone(tz2))"
        :"unixtime(unixtime(dt)+tzone()-tzone(tz))"))
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

(use-package counsel-calc :ensure nil
  :bind ("M-*" . completing-read-calc))

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


(use-package package-safe-delete)

(use-package artist
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
  (setq salah-next-days 2)
  (set-popup-rule! "^\\*salah-times\\*" :select nil :size '+popup-shrink-to-fit))

(when IS-MAC
  (use-package spotlight
    :bind ("C-c s S" . spotlight)))


(provide 'core-tools)
;;; core-tools.ends here
