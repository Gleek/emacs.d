(setq url-configuration-directory (concat CACHE-DIR "url"))
(setq gamegrid-user-score-file-directory (concat CACHE-DIR "games"))
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


(use-package paradox
  :ensure t
  :init
  (defvar paradox-automatically-star)
  (defvar paradox-execute-asynchronously)
  (setq paradox-automatically-star t)
  (setq paradox-execute-asynchronously t))

(use-package debbugs)

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

(use-package zeal-at-point
  :config (setq zeal-at-point-zeal-version "0.3.1"))
(use-package howdoi :disabled t)

(when IS-MAC
  (use-package osx-dictionary
    :bind (("C-c s D" . osx-dictionary-search-input))))


(use-package alert
  :init
  (defvar alert-default-style)
  (setq alert-default-style 'libnotify))



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

(use-package scratch)

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
    (variable-pitch-mode t)
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

(use-package w3m
  :init (setq w3m-search-default-engine "duckduckgo"))

(when IS-MAC
  (use-package exec-path-from-shell
    :demand
    :config
    ;; don't load interactive shell
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

(use-package system-packages
  :bind (("C-c P s" . system-packages-search)
         ("C-c P i" . system-packages-install)
         ("C-c P u" . system-packages-uninstall)
         ("C-c P l" . system-packages-list-installed-packages))
  :config
  (set-popup-rule! "^\\*system-packages\\*" :size 0.4 :quit 'other))


(use-package "web-search" :ensure nil :demand t
  :bind (("C-c s w" . duck)
         ("C-c s l" . lucky)
         ("C-c s d" . devdocs)))

;; (use-package keepass-mode)

(provide 'core-tools)
