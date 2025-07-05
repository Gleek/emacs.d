;;; core-org-tools.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Umar Ahmad
;; Created: February 08, 2025
;; Modified: February 08, 2025
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;;

;;; Code:

(require 'mod-number nil t)

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
    (let ((org-export-with-toc nil)
          (org-export-with-sub-superscripts nil)
          (org-html-checkbox-type 'unicode)
          ;; (org-export-smart-quotes-alist nil)
          (org-export-with-smart-quotes t)
          (export-type (if arg "rtf" "attributed")))
      (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
             (html (with-current-buffer buf (buffer-string)))
             ;; Remove electric quotes as they get messed up in some applications
             (html (string-multi-replace '(("”" "\"") ("“" "\"") ("’" "'")) html)))
        (shell-command (format "pbcopy-html --type=%s %s" export-type (shell-quote-argument html)))
        (kill-buffer buf)))))


(defun org-formatted-paste()
  "Clipboard content in html is converted to org using pandoc and inserted to the buffer."
  (interactive)
  ;; pbpaste-html is generated using the trick at https://stackoverflow.com/a/36109230
  (shell-command "pbpaste-html | pandoc -f html -t org" (current-buffer)))

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

(defun anonymous-pomodoro(&optional arg)
  "Start a 25 minute work or 5 min rest timer based on the prefix arg.
    Helpful in quickly starting a pomodoro for work and rest"
  (interactive "P")
  (if (and (boundp 'org-timer-countdown-timer) org-timer-countdown-timer)
      (org-timer-stop)
    (org-timer-set-timer (if arg "5" "25"))))


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

(defun +org-refile-to-pos (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))


(defun +org-increase-number-at-point(n)
  (interactive "P")
  (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
      (org-shiftcontrolup n)
    (change-number-at-point n)))

(defun +org-decrease-number-at-point(n)
  (interactive "p")
  (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
      (org-shiftcontroldown n)
    (change-number-at-point (- n))))


(use-package org
  :bind (("C-c o e" . org-export-dispatch)
         ("C-c o w" . copy-current-line-link-for-org)
         ("C-c o T" . anonymous-pomodoro)
         :map org-mode-map
         ("C-s-q" . org-fill-paragraph)
         ("s-w" . org-formatted-copy)
         ("C-S-<up>" . +org-increase-number-at-point)
         ("C-S-<down>" . +org-decrease-number-at-point)
         ("C-<tab>" . nil))
  :bind* (:map org-mode-map
               ("C-y" . +org-yank))
  :ensure nil
  :config
  (setq org-version (if (string= org-version "")
                        (let ((org-full-dir (file-name-directory (locate-library "org"))))p
                             (save-match-data
                               (and (string-match "-\\([0-9.]+\\)/" org-full-dir)) (match-string 1 org-full-dir)))
                      org-version))
  (setq org-catch-invisible-edits 'smart)
  (setq org-id-locations-file (concat CACHE-DIR "org-id-locations"))
  (setq org-export-with-broken-links t)
  (setq org-format-latex-options
        (list :foreground 'auto
              :background 'auto
              :scale 1.5
              :html-foreground "Black"
              :html-background "Transparent"
              :html-scale 1.0
              :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))
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
  ;; Support for plantuml
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t)
                                 (emacs-lisp . t)
                                 (shell . t)))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (advice-add #'org-babel-execute:plantuml
              :override #'+plantuml-org-babel-execute:plantuml-a)
  (add-to-list 'org-babel-default-header-args:plantuml
               '(:cmdline . "-charset utf-8")))


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


(use-package org-pomodoro
  :after org-agenda
  :bind (:map org-agenda-mode-map
              ("I" . org-pomodoro))
  :config
  (setq org-pomodoro-finished-sound (concat RES-DIR "bell.wav"))
  (setq org-pomodoro-long-break-sound (concat RES-DIR "bell.wav"))
  (setq org-pomodoro-short-break-sound (concat RES-DIR "bell.wav"))
  (setq org-pomodoro-start-sound (concat RES-DIR "bell.wav")))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-re-reveal
  :after org
  :demand t
  :commands (+org-re-reveal-run)
  :bind (:map org-mode-map
              ("C-z p o" . +org-re-reveal-run))
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
              ("C-z p p" . org-tree-slide-mode))
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
  )



(provide 'core-org-tools)
;;; core-org-tools.el ends here
