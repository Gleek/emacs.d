(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))


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

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

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
  (indent-buffer))


(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(defun find-alternate-file-as-root (filename)
  "Wraps `find-alternate-file' with opening a file as root."
  (find-alternate-file (concat "/sudo:root@localhost:" filename)))

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



(use-package move-text
  :defer 1
  :config
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice)
  (move-text-default-bindings))


(use-package auto-indent-mode
  :init
  (setq auto-indent-assign-indent-level nil)
  :hook prog-mode
  :config
  (advice-remove 'beginning-of-visual-line #'ad-Advice-move-beginning-of-line)
  :diminish)

(use-package subword
  :init (global-subword-mode t)
  :diminish subword-mode)

(use-package multiple-cursors
  :ensure t
  :config
  (define-key mc/keymap (kbd "C-s") #'phi-search)
  (define-key mc/keymap (kbd "C-r") #'phi-search-backward)
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-$" . mc/mark-more-like-this-extended)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c m" . mc/mark-all-dwim)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

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
  :bind (("C-x a a" . align)
         ("C-x a r" . align-regexp)
         ("C-x a c" . align-current)))


(use-package er/expand-region
  :ensure expand-region
  :init (setq shift-select-mode nil)
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

;; (use-package easy-kill
;;   :init
;;   (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package smartparens
  :defer 1
  :ensure t
  :config
  ;; (smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'smartparens-mode)
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


(use-package evil
  :ensure t
  :disabled t
  :config (evil-mode 1))

(use-package vimrc-mode)

(use-package god-mode
  :ensure t
  ;; :bind ([escape] . god-local-mode)
  :config (define-key god-local-mode-map (kbd ".") 'repeat))

(use-package undo-tree
  :defer 1
  :ensure t
  :bind (:map undo-tree-map
              ("M-_" . nil))
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-diff nil
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
        ;; truncating the undo history and corrupting the tree. See
        ;; https://github.com/syl20bnr/spacemacs/issues/12110
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000)
  (remove-hook 'undo-tree-mode-hook 'hide-mode-line-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,(concat CACHE-DIR "undo"))))
  :diminish undo-tree-mode)


(use-package so-long
  :defer 1
  :config
  (setq so-long-threshold 400)
                                        ; Add some font locking
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
  :hook (after-change-major-mode . sane-whitespace)
  :init
  (defun sane-whitespace()
    (unless (or (eq major-mode 'fundamental-mode)
                buffer-read-only
                (bound-and-true-p global-whitespace-mode)
                (null buffer-file-name))
      (whitespace-mode +1)))
  (setq whitespace-style '(face empty space-after-tab space-before-tab))
  :diminish whitespace-mode)

(use-package reformatter :disabled)

(use-package "mod-number" :ensure nil :demand t
  :bind (("C-S-<up>" . change-number-at-point)
         ("C-S-<down>" . subtract-number-at-point)))

(delete-selection-mode +1)
(setq backward-delete-char-untabify-method 'hungry)
(setq-default cursor-in-non-selected-windows nil)
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)
(setq tab-always-indent t)
(put 'narrow-to-region 'disabled nil)
(provide 'core-editing)
