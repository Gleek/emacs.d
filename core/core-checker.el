(use-package spell-fu
  :hook (text-mode . spell-fu-mode)
  :init
  (setq spell-fu-directory (concat CACHE-DIR "spell-fu"))
  :bind (("C-M-i" . +spell-fu-fix-last-error)
         (:map text-mode-map
               ("C-M-i" . +spell-fu-fix-last-error)))
  :config
  ;; Courtesy: https://emacs.stackexchange.com/a/55545 (ideasman42)
  ;; rotation logic is also in the link
  (defun +ispell-word-immediate ()
    "Run `ispell-word', using the first suggestion."
    (interactive)
    (cl-letf
        (((symbol-function 'ispell-command-loop)
          (lambda (miss _guess _word _start _end) (car miss))))
      (ispell-word)))

  (defun +spell-fu-fix-last-error(options)
    (interactive "P")
    (save-excursion
      (ispell-set-spellchecker-params)
      (ispell-accept-buffer-local-defs)
      (if (or (not (thing-at-point 'word))
              (ispell-correct-p))
          (spell-fu-goto-previous-error)
        (backward-char))
      (if options
          (+spell/correct)
        (+ispell-word-immediate))))
  ;; Courtesy: Doom
  (defun +spell-correct-fn (candidates word)
      (completing-read (format "Corrections for %S: " word) candidates))

  (defun +spell/correct ()
    "Correct spelling of word at point."
    (interactive)
    ;; spell-fu fails to initialize correctly if it can't find aspell or a similar
    ;; program. We want to signal the error, not tell the user that every word is
    ;; spelled correctly.
    (unless (;; This is what spell-fu uses to check for the aspell executable
             or (and ispell-really-aspell ispell-program-name)
             (executable-find "aspell"))
      (user-error "Aspell is required for spell checking"))

    (ispell-set-spellchecker-params)
    (save-current-buffer
      (ispell-accept-buffer-local-defs))

    (cl-destructuring-bind (start . end)
        (or (bounds-of-thing-at-point 'word)
            (user-error "No word at point"))
      (let ((word (thing-at-point 'word t))
            (orig-pt (point))
            poss ispell-filter)
        (ispell-send-string "%\n")
        (ispell-send-string (concat "^" word "\n"))
        (while (progn (accept-process-output ispell-process)
                      (not (string= "" (car ispell-filter)))))
        ;; Remove leading empty element
        (setq ispell-filter (cdr ispell-filter))
        ;; ispell process should return something after word is sent. Tag word as
        ;; valid (i.e., skip) otherwise
        (unless ispell-filter
          (setq ispell-filter '(*)))
        (when (consp ispell-filter)
          (setq poss (ispell-parse-output (car ispell-filter))))
        (cond
         ((or (eq poss t) (stringp poss))
          ;; don't correct word
          (message "%s is correct" (funcall ispell-format-word-function word))
          t)
         ((null poss)
          ;; ispell error
          (error "Ispell: error in Ispell process"))
         (t
          ;; The word is incorrect, we have to propose a replacement.
          (setq res (+spell-correct-fn (nth 2 poss) word))
          ;; Some interfaces actually eat 'C-g' so it's impossible to stop rapid
          ;; mode. So when interface returns nil we treat it as a stop.
          (unless res (setq res (cons 'break word)))
          (cond
           ((stringp res)
            (+spell--correct res poss word orig-pt start end))
           ((let ((cmd (car res))
                  (wrd (cdr res)))
              (unless (or (eq cmd 'skip)
                          (eq cmd 'break)
                          (eq cmd 'stop))
                (+spell--correct cmd poss wrd orig-pt start end)
                (unless (string-equal wrd word)
                  (+spell--correct wrd poss word orig-pt start end))))))
          (ispell-pdict-save t))))))


  (defun +spell--correct (replace poss word orig-pt start end)
    (cond ((eq replace 'ignore)
           (goto-char orig-pt)
           nil)
          ((eq replace 'save)
           (goto-char orig-pt)
           (ispell-send-string (concat "*" word "\n"))
           (ispell-send-string "#\n")
           (setq ispell-pdict-modified-p '(t)))
          ((or (eq replace 'buffer) (eq replace 'session))
           (ispell-send-string (concat "@" word "\n"))
           (add-to-list 'ispell-buffer-session-localwords word)
           (or ispell-buffer-local-name ; session localwords might conflict
               (setq ispell-buffer-local-name (buffer-name)))
           (if (null ispell-pdict-modified-p)
               (setq ispell-pdict-modified-p
                     (list ispell-pdict-modified-p)))
           (goto-char orig-pt)
           (if (eq replace 'buffer)
               (ispell-add-per-file-word-list word)))
          (replace
           (let ((new-word (if (atom replace)
                               replace
                             (car replace)))
                 (orig-pt (+ (- (length word) (- end start))
                             orig-pt)))
             (unless (equal new-word (car poss))
               (delete-region start end)
               (goto-char start)
               (insert new-word))))
          ((goto-char orig-pt)
           nil)))

  (setq-default spell-fu-faces-exclude
                '(org-block org-block-begin-line
                org-block-end-line org-code org-date org-footnote
                org-formula org-latex-and-related org-link
                org-meta-line org-property-value
                org-ref-cite-face org-special-keyword org-tag
                org-todo org-todo-keyword-done
                org-todo-keyword-habt org-todo-keyword-kill
                org-todo-keyword-outd org-todo-keyword-todo
                org-todo-keyword-wait org-verbatim
                markdown-code-face markdown-html-attr-name-face
                markdown-html-attr-value-face
                markdown-html-tag-name-face
                markdown-inline-code-face markdown-link-face
                markdown-markup-face markdown-plain-url-face
                markdown-reference-face markdown-url-face
                font-latex-math-face font-latex-sedate-face
                font-lock-function-name-face
                font-lock-keyword-face
                font-lock-variable-name-face))

  (defun +spell-fu-set-face()
    (set-face-attribute 'spell-fu-incorrect-face nil :underline '(:style wave :color "#6666ff")))
  (+spell-fu-set-face)
  (add-hook '+theme-toggle-hook '+spell-fu-set-face))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  ;; (global-flycheck-mode -1)
  ;; (add-hook 'prog-mode-hook 'flycheck-mode)

  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))
  ;; (setq flycheck-buffer-switch-check-intermediate-buffers nil)
  (setq flycheck-display-errors-delay 0.9)

  (set-popup-rules!
    '(("^\\*Flycheck error messages\\*" :select nil)
      ("^\\*Flycheck errors\\*" :size 0.25)))

  ;; Courtesy - Doom Emacs
  (setq flycheck-indication-mode 'right-fringe)
  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)

  (defun +flycheck-set-face()
    (set-face-attribute 'flycheck-error nil :underline '(:style wave :color "#a52a2a"))
    (set-face-attribute 'flycheck-warning nil :underline '(:style wave :color "#ca9532"))
    (set-face-attribute 'flycheck-info nil :underline '(:style wave :color "#98be65")))
  (+flycheck-set-face)
  (add-hook '+theme-toggle-hook '+flycheck-set-face)


  ;; Using mode level flycheck checkers instead of chaining them.
  ;; So that single flycheck checker can used in multiple modes..such as `lsp'
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint))

(use-package consult-flycheck
  :after (flycheck consult)
  :bind (:map flycheck-mode-map
              ("C-c ! l" . consult-flycheck)))




;; (flycheck-add-next-checker 'lsp '(warning . php-phpmd))
;; (use-package flycheck-phpstan
;;   :init
;;   (setq phpstan-memory-limit "1G")
;; )
(unless IS-TERM
  (use-package flycheck-posframe
    :hook (flycheck-mode . flycheck-posframe-mode)
    :config
    ;; (flycheck-posframe-configure-pretty-defaults)
    (setq flycheck-posframe-warning-prefix "⚠ "
          flycheck-posframe-info-prefix "··· "
          flycheck-posframe-prefix "··· "
          flycheck-posframe-error-prefix "✕ ")
    (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p)))

(use-package flycheck-popup-tip
  :disabled t
  :hook (flycheck-mode . flycheck-popup-tip-mode))

;; (use-package flymake :ensure nil)
;; (use-package flymake-diagnostic-at-point
;;   :after flymake
;;   :init
;;   (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(provide 'core-checker)
