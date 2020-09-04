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

(use-package image-mode :ensure nil
  :init
  (defun scale-image-register-hook ()
    "Register the image scaling hook."
    (add-hook 'text-scale-mode-hook 'scale-image))

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
  :defer 1
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



(use-package restclient :mode ("\\.rest\\'" . restclient-mode) )
(use-package company-restclient
  :after company
  :after restclient
  :config (add-to-list 'company-backends 'company-restclient))

(use-package w3m
  :init (setq w3m-search-default-engine "duckduckgo"))

(when IS-MAC
  (use-package exec-path-from-shell
    :demand
    :config (exec-path-from-shell-initialize)))


(use-package "web-search" :ensure nil :demand t
  :bind (("C-c s w" . duck)
         ("C-c s l" . lucky)
         ("C-c s d" . devdocs)))
;; (use-package keepass-mode)

(provide 'core-tools)
