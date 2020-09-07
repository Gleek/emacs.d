(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (let ((line-number-already-enabled display-line-numbers))
    (unwind-protect
        (progn
          (when (eq line-number-already-enabled nil) (display-line-numbers-mode 1))
          (goto-line (read-number "Goto line: ")))
      (when (eq line-number-already-enabled nil)(display-line-numbers-mode -1)))))


(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
                 (message "Line copied") (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Avoid getting read only error with focus on minibuffer
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(when IS-MAC
  (setq mac-command-modifier 'meta
        ;;       mac-option-modifier 'control
        ns-option-modifier 'super))

;; Find my cursor
(use-package beacon
  :disabled
  :config
  (beacon-mode)
  :diminish beacon-mode)

(use-package smooth-scroll
  :defer 1
  :ensure t
  :config
  (smooth-scroll-mode t)
  ;; (setq scroll-conservatively 101)
  ;; (setq inhibit-compacting-font-caches t)
  (setq smooth-scroll/vscroll-step-size 5)
  :diminish smooth-scroll-mode)


(use-package paren
  :defer 1
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery nil)
  (show-paren-mode))


(use-package which-key
  :defer 1
  :config
  (which-key-mode 1)
  ;; (which-key-setup-minibuffer)
  :diminish which-key-mode)

(use-package zoom
  :defer 1
  :diminish
  :config
  ;; (zoom-mode t)
  (setq zoom-ignored-buffer-name-regexps '("^\*ansi-term.*"))
  (setq zoom-size '(0.618 0.618)))

(use-package writeroom-mode)

(use-package which-func
  :disabled t ;; slowing down startup of big files
  :config
  (which-function-mode)
  (set-face-foreground 'which-func "darkgrey"))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq select-enable-clipboard t ;; Enabled emacs to use system clipboard
      select-enable-primary nil ;; Disable Copy on selection
      display-time-default-load-average nil
      save-interprogram-paste-before-kill t
      kill-ring-max 200
      kill-do-not-save-duplicates t
      apropos-do-all t
      use-dialog-box nil
      x-underline-at-descent-line t
      ring-bell-function 'ignore
      mouse-yank-at-point t
      require-final-newline t
      fast-but-imprecise-scrolling t
      kill-do-not-save-duplicates t
      highlight-nonselected-windows nil
      ediff-window-setup-function 'ediff-setup-windows-plain)
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)
(setq enable-recursive-minibuffers t)

(use-package page-break-lines :diminish page-break-lines-mode)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


;; Courtesy: doom emacs
;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).
More specifically, when `escape-quit' is pressed.   If any hook returns non-nil,
all hooks after it are ignored.")

(defun escape-quit ()
  "Run `escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))



(provide 'core-ux)
