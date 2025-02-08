;;; core-org-notes.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Umar Ahmad
;; Created: February 08, 2025
;; Modified: February 08, 2025
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;;

;;; Code:
(defvar +roam-directory (concat +org-directory "org-roam/"))
(defvar +ekg-directory (concat +org-directory "ekg/"))


(defun insert-org-link-to-lorien-file ()
    "Choose a file in resource/lorien interactively and insert its org file link in the current buffer."
    (interactive)
    (let ((file-path (read-file-name "Choose a file in resource/lorien: " "resource/lorien/")))
      (if (file-exists-p file-path)
          (let ((file-name (file-name-nondirectory file-path)))
            (insert (format "[[file:%s][%s]]" file-path file-name)))
        (message "File does not exist"))))

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
  :bind (("C-c o s" . org-roam-search)
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
  (setq org-roam-node-display-template (concat "${type:15} ${title:*} " (propertize "${tags:50}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)


  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+datetree "journal.org" week))))
  (setq org-roam-capture-templates
        '(("b" "brain" plain
           "%?"
           :if-new (file+head "brain/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("p" "post" plain "%?"
           :if-new
           (file+head "posts/${title}.org" "#+title: ${title}\n#+filetags: :post:\n")
           :immediate-finish t
           :unnarrowed t)))

  (defvar org-roam-brain-capture-immediate-template
    (car org-roam-capture-templates))
  (defvar org-roam-ref-capture-immediate-template
    (cadr org-roam-capture-templates))

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))


  (defun org-roam-search ()
    (interactive)
    (require 'consult)
    (consult-ripgrep org-roam-directory))
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
    (let* ((is-brain (string-suffix-p "brain/" (file-name-directory (or (buffer-file-name) default-directory))))
           (template (if is-brain
                         org-roam-brain-capture-immediate-template
                       org-roam-ref-capture-immediate-template))
           (args (push arg args))
           (org-roam-capture-templates (list template)))
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

(use-package org-excalidraw
  :after org
  :demand t
  :bind (:map org-mode-map
              ("<mouse-4>" . +excalidraw-draw))
  :ensure (:fetcher github :repo "gleek/org-excalidraw") ; Maintain my own fork since the original has bugs and not updated.
  :commands (+excalidraw-draw)
  :config
  (defvar +excalidraw-initialized nil)
  (defun +excalidraw-draw()
    (interactive)
    (if (not +excalidraw-initialized)
        (org-excalidraw-initialize))
    (setq +excalidraw-initialized t)
    (org-excalidraw-create-drawing))
  (setq org-excalidraw-directory (concat +roam-directory "resource/excalidraw")))

(provide 'core-org-notes)
;;; core-org-notes.el ends here
