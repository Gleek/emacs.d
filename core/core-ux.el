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
;; (use-package beacon
;;   :disabled
;;   :config
;;   (beacon-mode)
;;   :diminish beacon-mode)

(use-package smooth-scroll
  :defer 1
  :ensure t
  :config
  (smooth-scroll-mode t)
  ;; (setq scroll-conservatively 101)
  ;; (setq inhibit-compacting-font-caches t)
  (setq smooth-scroll/vscroll-step-size 5)
  :diminish smooth-scroll-mode)


;; (use-package good-scroll)


(use-package paren
  :ensure nil
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
  ;; Courtesy: doom emacs
  (when (eq which-key-popup-type 'side-window)
  (setq which-key-popup-type 'custom
        which-key-custom-popup-max-dimensions-function
        (lambda (_) (which-key--side-window-max-dimensions))
        which-key-custom-hide-popup-function #'which-key--hide-buffer-side-window
        which-key-custom-show-popup-function
        (lambda (act-popup-dim)
          (cl-letf ((symbol-function (defun display-buffer-in-side-window (buffer alist)
                                       (+popup-display-buffer-stacked-side-window-fn
                                        buffer (append '((vslot . -9999)) alist)))))
                 ;; HACK Fix #2219 where the which-key popup would get cut off.
                 (setcar act-popup-dim (1+ (car act-popup-dim)))
                 (which-key--show-buffer-side-window act-popup-dim)))))
  ;; (which-key-setup-minibuffer)
  :diminish which-key-mode)

(use-package keycast
  :bind ("C-c t k" . +toggle-keycast)
  :config
 (defun +toggle-keycast()
    (interactive)
    (if (member '("" mode-line-keycast " ") global-mode-string)
        (progn (setq global-mode-string (delete '("" mode-line-keycast " ") global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast disabled"))
      (add-to-list 'global-mode-string '("" mode-line-keycast " "))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast enabled"))))



(use-package zoom
  ;; :defer 1
  :diminish
  :init
  (setq zoom-ignored-buffer-name-regexps '("^\*ansi-term.*"))
  (setq zoom-size '(0.618 0.618))
  :config
  ;; (zoom-mode t)
  )

(use-package writeroom-mode)

(use-package which-func
  ;; :disabled t ;; slowing down startup of big files
  :ensure nil
  :defer 5
  :config
  (which-function-mode)
  (advice-add 'which-function :filter-return
            (lambda (s) (when s (truncate-string-to-width s 30 nil nil t))))

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
      ;; scrolling
      fast-but-imprecise-scrolling t
      auto-window-vscroll nil
      scroll-preserve-screen-position t

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
;; 2. Deactivate mark if present.
;; 3. Close popup windows remotely (if it is allowed to)
;; 4. Refresh buffer indicators, like git-gutter and flycheck
;; 5. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).
More specifically, when `escape-quit' is pressed.   If any hook returns non-nil,
all hooks after it are ignored.")

(defun escape-quit (&optional interactive)
  "Run `escape-hook'."
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))

        ;; Deactivate active mark
        ((if (region-active-p)
             (or (let (select-active-regions)
                   (deactivate-mark))
                 t)))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

(provide 'core-ux)
