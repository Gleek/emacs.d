(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

;; Courtesy: oantolin
(defun mark-inside-sexp ()
  "Mark inside a sexp."
  (interactive)
  (let (beg end)
    (backward-up-list 1 t t)
    (setq beg (1+ (point)))
    (forward-sexp)
    (setq end (1- (point)))
    (goto-char beg)
    (push-mark)
    (goto-char end))
  (activate-mark))


(defmacro def-thing-marker (fn-name things forward-thing &rest extra)
  `(defun ,fn-name (&optional arg allow-extend)
     ,(format "Mark ARG %s starting with the current one. If ARG is negative,
mark -ARG %s ending with the current one.
Interactively (or if ALLOW-EXTEND is non-nil), if this command is
repeated or (in Transient Mark mode) if the mark is active, it
marks the next ARG %s after the ones already marked." things things things)
     (interactive "p\np")
     (unless arg (setq arg 1))
     (if (and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (,forward-thing arg)
            (point)))
       ,(plist-get extra :pre)
       (,forward-thing arg)
       ,(plist-get extra :post)
       (push-mark nil t t)
       (,forward-thing (- arg)))))


(defun forward-to-whitespace (arg)
  "Move forward to the end of the next sequence of non-whitespace
characters. With argument, do this that many times."
  (interactive "^p")
  (re-search-forward
   (if (> arg 0)
       "[^[:blank:]\n]\\(?:[[:blank:]\n]\\|\\'\\)"
     "\\(?:[[:blank:]\n]\\|\\`\\)[^[:blank:]\n]")
   nil t arg)
  (unless (= (point) (if (> arg 0) (point-max) (point-min)))
    (forward-char (if (> arg 0) -1 1))))

(def-thing-marker mark-non-whitespace "vim WORDS"
                  forward-to-whitespace)


(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun open-line-above ()
  (interactive)
  (previous-line)
  (smart-open-line))


(defun comment-or-uncomment-region-or-line ()
  "Comment a line or region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))





(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))


(defun unescape-region (start end)
  "Unescape special characters in the region between START and END."
  (interactive "r")
  (let ((text (buffer-substring start end)))
    (delete-region start end)
    (insert (format "(insert \"%s\")" text))
    (save-excursion (eval-last-sexp nil))
    (backward-kill-sexp)))



(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file "/sudo:root@localhost:")
    (let ((pos (point)))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
      (goto-char pos))))

(defun get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank).
https://emacs.stackexchange.com/a/12124/2144"
  (interactive)
  (kill-new (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org | sed 's/ / /g'"))
  (yank))


(use-package emacs :ensure nil
  :bind (("M-;" . comment-or-uncomment-region-or-line)
         ("C-c C-d" . duplicate-current-line-or-region)
         ("C-c d" . duplicate-current-line-or-region)
         ("C-c C-;" . duplicate-and-comment-current-line-or-region)
         ("C-^" . top-join-line)
         ("C-@" . mark-inside-sexp)
         ;; ("C-M-@" . mark-non-whitespace)
         ([remap kill-whole-line] . smart-kill-whole-line)
         ([(shift return)] . smart-open-line)
         ("C-S-<return>" . open-line-above)
         ("C-x C-r" . sudo-edit)))

(use-package selected
  :defer 1
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("d" . downcase-region)
              ("u" . upcase-region)
              ("r" . reverse-region)
              ;; ("s" . sort-lines)
              ("x" . eval-region))
  :config
  (selected-global-mode 1))

(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down))
  :config
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))


(use-package auto-indent-mode
  :disabled t ;; makes stuff super slow
  :init
  (setq auto-indent-assign-indent-level nil)
  :hook prog-mode
  :config
  (advice-remove 'beginning-of-visual-line #'ad-Advice-move-beginning-of-line)
  :diminish)

(use-package subword
  :ensure nil
  :init (global-subword-mode t)
  :diminish subword-mode)

(use-package multiple-cursors
  ;; C-v M-v to go through cursors
  ;; C-' to only show places where cursor is. Again to unhide
  ;; RET to switch from rrm to multiple cursors
  :ensure t
  :after selected
  :config
  (define-key mc/keymap (kbd "C-s") #'phi-search)
  (define-key mc/keymap (kbd "C-r") #'phi-search-backward)
  :bind (("C-S-c C-S-c". mc/edit-lines)
         ("C-$".         mc/mark-more-like-this-extended)
         ("C->".         mc/mark-next-like-this)
         ("C-<".         mc/mark-previous-like-this)
         ("<C-m> C-.".   mc/mark-next-like-this)
         ("<C-m> C-,".   mc/mark-previous-like-this)
         ;; Courtesy: jwiegley
         ("C-c m"       . mc/mark-all-dwim)
         ("<C-m> ^"     . mc/edit-beginnings-of-lines)
         ("<C-m> `"     . mc/edit-beginnings-of-lines)
         ("<C-m> $"     . mc/edit-ends-of-lines)
         ("<C-m> '"     . mc/edit-ends-of-lines)
         ("<C-m> R"     . mc/reverse-regions)
         ("<C-m> S"     . mc/sort-regions)
         ("<C-m> W"     . mc/mark-all-words-like-this)
         ("<C-m> Y"     . mc/mark-all-symbols-like-this)
         ("<C-m> a"     . mc/mark-all-like-this-dwim)
         ("<C-m> c"     . mc/mark-all-dwim)
         ("<C-m> l"     . mc/insert-letters)
         ("<C-m> n"     . mc/insert-numbers)
         ("<C-m> r"     . mc/mark-all-in-region)
         ("<C-m> C-s"     . set-rectangular-region-anchor)
         ("<C-m> %"     . mc/mark-all-in-region-regexp)
         ("<C-m> t"     . mc/mark-sgml-tag-pair)
         ("<C-m> w"     . mc/mark-next-like-this-word)
         ("<C-m> x"     . mc/mark-more-like-this-extended)
         ("<C-m> y"     . mc/mark-next-like-this-symbol)
         ("<C-m> C-x"   . reactivate-mark)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> ("     . mc/mark-all-symbols-like-this-in-defun)
         ("<C-m> C-("   . mc/mark-all-words-like-this-in-defun)
         ("<C-m> M-("   . mc/mark-all-like-this-in-defun)
         ("<C-m> ["     . mc/vertical-align-with-space)
         ("<C-m> {"     . mc/vertical-align)

         ("M-<down-mouse-1>" . mc/add-cursor-on-click)
         :map selected-keymap
         ("c"   . mc/edit-lines)
         ("."   . mc/mark-next-like-this)
         ("<"   . mc/unmark-previous-like-this)
         ("C->" . mc/skip-to-next-like-this)
         (","   . mc/mark-previous-like-this)
         (">"   . mc/unmark-next-like-this)
         ("C-<" . mc/skip-to-previous-like-this)
         ("y"   . mc/mark-next-symbol-like-this)
         ("Y"   . mc/mark-previous-symbol-like-this)
         ("w"   . mc/mark-next-word-like-this)
         ("W"   . mc/mark-previous-word-like-this))
  :config
  (defun reactivate-mark ()
    (interactive)
    (activate-mark)))
;; Courtesy jwiegley
(use-package mc-extras
  :bind (("<C-m> M-C-f" . mc/mark-next-sexps)
         ("<C-m> M-C-b" . mc/mark-previous-sexps)
         ("<C-m> <"     . mc/mark-all-above)
         ("<C-m> >"     . mc/mark-all-below)
         ("<C-m> f"     . mc/freeze-fake-cursors-dwim)
         ("<C-m> C-d"   . mc/remove-current-cursor)
         ("<C-m> C-k"   . mc/remove-cursors-at-eol)
         ("<C-m> M-d"   . mc/remove-duplicated-cursors)
         ("<C-m> ]"     . mc/rect-rectangle-to-multiple-cursors)
         ("<C-m> |"     . mc/move-to-column)
         ("<C-m> ~"     . mc/compare-chars)))

(use-package string-inflection
  :bind (("M-_" . string-inflection-all-cycle)
         ("C-c c s" . string-inflection-underscore)
         ("C-c c k" . string-inflection-kebab-case)
         ("C-c c M" . string-inflection-camelcase)
         ("C-c c m" . string-inflection-lower-camelcase)
         ("C-c c c" . capitalize-word)
         ("C-c c u" . upcase-word)
         ("C-c c l" . downcase-word)))


(use-package align
  :ensure nil
  :bind (("C-x a a" . align)
         ("C-x a r" . align-regexp)
         ("C-x a c" . align-current)))


(use-package er/expand-region
  :ensure expand-region
  :init (setq shift-select-mode nil)
  :bind (("C-=" . er/expand-region)
         ("C-c SPC d" . er/mark-defun)
         ("C-c SPC f" . er/mark-paragraph)
         ("C-c SPC '" . er/mark-inside-quotes)
         ("C-c SPC (" . er/mark-inside-pairs)
         ("C-c SPC SPC" . er/expand-region)
         ("C-+" . er/contract-region)))

;; (use-package easy-kill
;;   :init
;;   (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package smartparens
  :defer 1
  :ensure t
  :config
  (smartparens-global-mode t)
  ;; (add-hook 'prog-mode-hook 'smartparens-mode)
  ;; (add-hook 'org-mode-hook 'smartparens-mode)
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  ;; https://github.com/Fuco1/smartparens/issues/80 get reindent on curly brackets.
  (dolist (mode '(prog-mode))
    (sp-local-pair mode "{" nil :post-handlers
                   '((radian-enter-and-indent-sexp "RET")
                     (radian-enter-and-indent-sexp "<return>"))))
  (defun radian-enter-and-indent-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  :bind (("M-[" . sp-backward-unwrap-sexp)
         ("M-]" . sp-unwrap-sexp))
  :diminish smartparens-mode)

(use-package undo-tree
  :disabled t ; Trying out vundo and undo-fu for sometime
  :defer 1
  :ensure t
  :bind (:map undo-tree-map
              ("M-_" . nil))
  :config
  ;; Quiting undo tree resets undo-tree-visualizer-diff value.
  ;; This masks it so that it can’t be changed
  (defun +undo-tree-visualizer-quit(f &rest args)
    (cl-letf ((undo-tree-visualizer-diff))
      (apply f args)))
  (advice-add 'undo-tree-visualizer-quit :around '+undo-tree-visualizer-quit)

  (defun +undo-tree-save-history (undo-tree-save-history &rest args)
    "Removes Wrote undo tree messages from Message buffer"
    (let ((message-log-max nil)
          (inhibit-message t))
      (apply undo-tree-save-history args)))
  (advice-add 'undo-tree-save-history :around '+undo-tree-save-history)

  (global-undo-tree-mode 1)

  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
        ;; truncating the undo history and corrupting the tree. See
        ;; https://github.com/syl20bnr/spacemacs/issues/12110
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000)

  (add-hook 'escape-hook (lambda()
                           (when (eq major-mode 'undo-tree-visualizer-mode)
                             (undo-tree-visualizer-quit))))

  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,(concat CACHE-DIR "undo"))))
  :diminish undo-tree-mode)


(use-package undo-fu
  :defer 1
  :config
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap undo-redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t)

  (undo-fu-mode t))

(use-package undo-fu-session
  :after (undo-fu)
  :demand t
  :config
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-directory (expand-file-name "undo-fu-session" CACHE-DIR))
  (when (executable-find "zstd")
    (setq undo-fu-session-compression 'zst))
  (undo-fu-session-global-mode t))


(use-package vundo
  :bind (("C-x u" . vundo)
         (:map vundo-mode-map
               ("C-g" . vundo-quit)
               ("D" . vundo-live-diff-mode)))
  :config
  (defun vundo-live-diff-post-command ()
    "Post command hook function for live diffing."
    (when (not (memq this-command '(vundo-quit vundo-confirm)))
      (progn
        (vundo-diff-mark (vundo-m-parent (vundo--current-node vundo--prev-mod-list)))
        (vundo-diff))))
  (define-minor-mode vundo-live-diff-mode
    "Shows live diff between the current node and its parent."
    :lighter nil
    (if vundo-live-diff-mode
        (add-hook 'post-command-hook #'vundo-live-diff-post-command 0 t)
      (remove-hook 'post-command-hook #'vundo-live-diff-post-command t)))

  (add-hook 'vundo-mode-hook (lambda () (vundo-live-diff-mode t)))
  (set-face-attribute 'vundo-default nil :font "FiraCode Nerd Font Mono" :family "FiraCode Nerd Font")
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t))


(use-package so-long
  :ensure nil
  :defer 1
  :config
  (setq so-long-threshold 10000)
  ;; Add some font locking
  (setq so-long-minor-modes (delq 'font-lock-mode so-long-minor-modes))
  (add-to-list 'so-long-minor-modes 'lsp-mode)
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))

  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  (add-to-list 'so-long-target-modes 'text-mode)
  (setq so-long-variable-overrides (delq (assoc 'buffer-read-only so-long-variable-overrides) so-long-variable-overrides))
  (global-so-long-mode))

(use-package ws-butler
  :hook (find-file . ws-butler-mode))

(use-package whitespace
  :ensure nil
  :hook (after-change-major-mode . sane-whitespace)
  :init
  (defun sane-whitespace()
    (unless (or (eq major-mode 'fundamental-mode)
                buffer-read-only
                (bound-and-true-p global-whitespace-mode)
                (null buffer-file-name))
      (whitespace-mode +1)))
  (setq whitespace-style '(face empty space-after-tab space-before-tab trailing))
  :diminish whitespace-mode)

(use-package format-all
  :bind (("C-c f" . +format)
         ("C-c C-f" . +format))
  :config

  (defun +format(&optional arg)
    "Format buffer or region using lsp or format-all or cleanup-buffer.

Prefers region if active.
With prefix ARG, prompt for the formatter to use."
    (interactive "P")
    (let* ((chosen (and arg (completing-read "Format using: " (list "lsp" "format-all" "cleanup-buffer") nil t)))
           (no-choose-or-lsp (or (not chosen) (string= chosen "lsp")))
           (no-choose-or-fa (or (not chosen) (string= chosen "format-all")))
           (no-choose-or-cb (or (not chosen) (string= chosen "cleanup-buffer")))
           (lsp-enabled (bound-and-true-p lsp-mode))
           (lsp-region-p (and lsp-enabled (lsp-feature? "textDocument/rangeFormatting")))
           (lsp-formatter-p (and lsp-enabled (lsp-feature? "textDocument/formatting")))
           (fa-formatter (gethash (format-all--language-id-buffer) format-all--language-table))
           (fa-region-p (let ((region-p nil))
                          (if arg
                              (dolist (formatter fa-formatter region-p)
                                (when (and formatter (eq (car (gethash formatter format-all--features-table)) 'region))
                                  (setq region-p t)))
                            (let ((first-formatter (car fa-formatter)))
                              (setq region-p (and first-formatter (eq (car (gethash first-formatter format-all--features-table)) 'region)))))
                          region-p)))

      (cond ((and (region-active-p) lsp-region-p no-choose-or-lsp)
             (message "Formatting region with lsp")
             (lsp-format-region (region-beginning) (region-end)))

            ((and (region-active-p) fa-region-p no-choose-or-fa)
             (message "Formatting region with format-all")
             (format-all-ensure-formatter)
             (format-all-region (region-beginning) (region-end) (if arg 'always nil)))

            ((and lsp-formatter-p no-choose-or-lsp)
             (message "Formatting buffer with lsp")
             (lsp-format-buffer))

            ((and fa-formatter no-choose-or-fa)
             (message "Formatting buffer with format-all")
             (format-all-ensure-formatter)
             (format-all-buffer (if arg 'always nil)))

            (no-choose-or-cb
             (message "Cleaning buffer")
             (cleanup-buffer)))))

  (defun indent-defun ()
    "Indent the current defun."
    (interactive)
    (save-excursion
      (mark-defun)
      (indent-region (region-beginning) (region-end))))

  (defun indent-buffer ()
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun untabify-buffer ()
    (interactive)
    (untabify (point-min) (point-max)))

  (defun cleanup-buffer-safe ()
    "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a `before-save-hook', and that
might be bad."
    (interactive)
    (untabify-buffer)
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8))

  (defun cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
    (interactive)
    (cleanup-buffer-safe)
    (indent-buffer)))

(use-package cc-mode
  :ensure nil
  ;; Use hungry delete everywhere
  :bind ("C-<backspace>" . c-hungry-delete-backwards))

(use-package "mod-number" :ensure nil
  :bind (("C-S-<up>" . change-number-at-point)
         ("C-S-<down>" . subtract-number-at-point)))


(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode))


(use-package emoji-search
  :ensure nil
  :bind ("C-c s e" . emoji-search-completing-read)
  :config
  (setq emoji-search-download-dir CACHE-DIR)
  (add-to-list 'vertico-multiform-commands '(emoji-search-completing-read grid)))

;; (delete-selection-mode +1)
(setq backward-delete-char-untabify-method 'untabify)
(setq-default cursor-in-non-selected-windows nil)
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)
(setq-default fill-column 100)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right) ;; To render bidirectional text

(setq tab-always-indent 'complete)
(put 'narrow-to-region 'disabled nil)


(provide 'core-editing)
