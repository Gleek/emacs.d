;;; core-org-ui.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Umar Ahmad
;; Created: February 08, 2025
;; Modified: February 08, 2025
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;;

;;; Code:

(defun variable-pitch-for-notes ()
  (interactive)
  (when (string-match "\\(.*Notes.org\\|roam.*org\\|denote.*org\\)" (format "%s" buffer-file-name))
    (reading-mode)))

(defun org-beautify-chars ()
  "Beautify Org Checkbox Symbol"
  ;; (push '("[ ]" .  "☐") prettify-symbols-alist)
  ;; (push '("[X]" . "☒" ) prettify-symbols-alist)
  ;; (push '("[-]" . "❍" ) prettify-symbols-alist)
  (push '("->" . "⟶" ) prettify-symbols-alist)
  ;; (push '("=>" . "⟹") prettify-symbols-alist)
  (push '("--" . "—") prettify-symbols-alist)
  (prettify-symbols-mode))

(defun echo-area-tooltips ()
  "Show tooltips in the echo area automatically for current buffer.
Useful to checking the link under point."
  (setq-default help-at-pt-display-when-idle 'never)
  (setq-local help-at-pt-display-when-idle t
              help-at-pt-timer-delay 0)
  (help-at-pt-cancel-timer)
  (help-at-pt-set-timer))


(use-package org
  :ensure nil
  :config
  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)
  (add-hook 'org-mode-hook 'org-beautify-chars)
  (add-hook 'org-mode-hook #'echo-area-tooltips)
  (add-hook 'org-mode-hook 'visual-line-mode)
  ;; (add-hook 'org-mode-hook 'toggle-truncate-lines)
  (add-hook 'org-mode-hook 'variable-pitch-for-notes)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)


  (setq help-at-pt-display-when-idle t)
  (setq org-ellipsis " "       ; Setting to space since org modern already modifies the heading stars to arrows.
        ;; org-agenda-files `(,+agenda-directory)
        org-startup-align-all-tables t
        org-return-follows-link t
        org-startup-with-inline-images t
        org-display-remote-inline-images 'download
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-image-actual-width 500
        org-startup-folded t
        org-auto-align-tags nil
        org-tags-column 0
        org-imenu-depth 8
        org-startup-shrink-all-tables t
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))
        org-export-with-sub-superscripts nil ;; "{}"
        org-export-with-section-numbers nil
        org-html-validation-link nil
        org-use-sub-superscripts nil
        org-html-checkbox-type 'html
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-clock-sound (concat RES-DIR "bell.wav")
        org-agenda-clockreport-parameter-plist '(:maxlevel 3)
        org-id-link-to-org-use-id 'create-if-interactive
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-confirm-babel-evaluate nil
        org-pretty-entities t
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

  (setq org-habit-completed-glyph ?•) ; bullet point
  ;; (setq org-habit-today-glyph 0215) ; multiplication sign
  ;; For some reason the above glyph comes up as \327 in agenda
  ;; using a similar gliph
  (setq org-habit-today-glyph ?⨯) ; vector cross product sign
  (setq org-habit-following-days 1)
  (setq org-display-remote-inline-images 'download)

  (setq org-src-preserve-indentation t  ; use native major-mode indentation
        org-link-elisp-confirm-function nil
        ;; Show src buffer in popup, and don't monopolize the frame
        org-src-window-setup 'other-window)


  (dolist (face-attrs '(
                        ;; (org-block :inherit 'fixed-pitch)
                        ;; (org-code :inherit '(shadow fixed-pitch))
                        ;; (org-document-info :foreground "dark orange")
                        ;; (org-document-info-keyword :inherit '(shadow fixed-pitch))
                        ;; (org-indent :inherit '(org-hide fixed-pitch))
                        ;; (org-link :foreground "royal blue" :underline t)
                        ;; (org-meta-line :inherit (font-lock-comment-face fixed-pitch))
                        (org-property-value :inherit 'fixed-pitch)
                        (org-special-keyword :inherit (font-lock-comment-face fixed-pitch))
                        ;; (org-table :inherit 'fixed-pitch)
                        ;; (org-tag :inherit (shadow fixed-pitch) :weight bold :height 0.8)
                        ;; (org-ellipsis :inherit '(font-lock-comment-face default) :weight 'normal)
                        (org-verbatim :inherit (shadow fixed-pitch))
                        ))
    (let ((face (car face-attrs))
          (attributes (cdr face-attrs)))
      (apply #'set-face-attribute face nil attributes)))


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
      ("^CAPTURE-.*\\.org$" :size 0.25 :quit nil :select t :autosave t))))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . +org-modern-agenda))
  :config
  ;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka") ;; package's recommended font
  (setq org-modern-label-border 1)
  (add-to-list 'consult-preview-allowed-hooks 'global-org-modern-mode)

  ;; Courtesy: connormclaud (https://github.com/minad/org-modern/pull/209)
  ;; The reason for this not merged is the performance hit `string-pixel-width' is supposed to cause
  ;; Also it isn't backward compatible with older emacs versions
  ;; If this slows down my agenda as well. Disabling and setting  `org-agenda-tags-column' to 0 will be the solution.
  (defun org-modern-align-tags (&optional line)
    "Align all tags in agenda items to `org-agenda-tags-column'.
When optional argument LINE is non-nil, align tags only on the
current line."
    (let ((inhibit-read-only t)
          (org-agenda-tags-column (if (eq 'auto org-agenda-tags-column)
                                      (-(window-max-chars-per-line))
                                    org-agenda-tags-column))
          (end (and line (line-end-position)))
          len col pixel-width)
      (org-fold-core-ignore-modifications
        (save-excursion
          (goto-char (if line (line-beginning-position) (point-min)))
          (while (re-search-forward org-tag-group-re end 'noerror)
            (setq len (length (match-string 1))
                  pixel-width (string-pixel-width (match-string 1))
                  col (if (< org-agenda-tags-column 0)
                          (- (abs org-agenda-tags-column) len)
                        org-agenda-tags-column))
            (goto-char (match-beginning 1))
            (delete-region (save-excursion (skip-chars-backward " \t") (point))
                           (point))
            (let ((spaces (make-string (max 1 (- col (current-column) 2)) ?\s)))
              (insert spaces)
              (insert (propertize " " 'display `(space :align-to (- right (,pixel-width))))))
            (goto-char (line-end-position)))))))

  (defun +org-modern-agenda()
    (org-modern-agenda)
    (org-modern-align-tags))



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

(use-package org-transclusion)

(use-package org-appear
  ;; (setq org-hide-emphasis-markers t)
  :hook (org-mode . org-appear-mode))

(use-package org-pretty-table :ensure nil
  :hook (orgtbl-mode . org-pretty-table-mode))

(provide 'core-org-ui)
;;; core-org-ui.el ends here
