(use-package ivy
  ;; :defer 1
  :init
  (setq ivy-sort-max-size 7500
        ivy-height 17
        ivy-wrap nil
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ivy-read-action-function 'ivy-read-action-by-key
        ivy-use-selectable-prompt t)

  :config
  ;; (ivy-mode t)
  (eval-after-load '+popup
    '(set-popup-rule! "^\\*ivy-occur" :size 0.35 :ttl 0 :quit nil))


  ;; Ivy debounces all inputs when ivy-dynamic-exhibit-delay-ms is set to some value The following
  ;; code snippet only debounces the ivy--exhibit if there is a change in input This helps in
  ;; dynamic filtering by not debouncing any commands such as up and down arrow keys while at the
  ;; same time running filter functions only if some actual filtering is required.
  (defvar +ivy--queue-last-input nil)
  (defun +ivy-queue-exhibit-a(f &rest args)
    (if (equal +ivy--queue-last-input (ivy--input))
        (ivy--exhibit)
      (apply f args))
    (setq +ivy--queue-last-input (ivy--input)))
  (advice-add 'ivy--queue-exhibit :around #'+ivy-queue-exhibit-a)

  ;; Force change line spacing in ivy. There's a bug which makes ivy
  ;; hide few candidates if the spacing is made too large, but works
  ;; fine for smaller values.

  (defun +ivy-shrink-after-dispatching-a(f &rest a)
    (unless mini-frame-mode
      (apply f a)))
  (advice-add 'ivy-shrink-after-dispatching :around #'+ivy-shrink-after-dispatching-a)
  :diminish ivy-mode)


(use-package vertico
  :defer 1
  :bind (("C-c v" . vertico-repeat)
         (:map vertico-map
               ("<return>" . vertico-directory-enter)
               ("<backspace>" . vertico-directory-delete-char)
               ("M-<backspace>" . vertico-directory-delete-word)
               ("C-c C-c" . embark-act)
               ("C-c C-o" . embark-export)))
  :hook (minibuffer-setup . vertico-repeat-save)
  :config
  (vertico-mode t)
  (setq vertico-resize nil
        vertico-count 15)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package embark
  :bind* (("M-o" . embark-act)
          ("C-M-o" . embark-act-noquit)
          (:map minibuffer-local-map
                ("C-," . embark-become)))
  :config
  (setq embark-help-key "?")
  ;; (defvar +embark-become-keymap (define-keymap))
  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))

  (defvar +embark-buffer-keymap (define-keymap))
  (define-key +embark-buffer-keymap "p" #'consult-projectile)
  (define-key +embark-buffer-keymap "P" #'consult-projectile-switch-project)
  (define-key +embark-buffer-keymap "a" #'affe-find)
  (define-key +embark-buffer-keymap "A" #'affe-find-no-ignore)
  (define-key +embark-buffer-keymap "b" #'consult-buffer)
  (define-key +embark-buffer-keymap "B" #'consult-project-buffer)
  (define-key +embark-buffer-keymap "f" #'find-file)

  (define-key embark-file-map "R" #'open-with-dragger)
  (define-key embark-file-map "L" #'copy-file-link-for-org)
  (add-to-list 'embark-become-keymaps '+embark-buffer-keymap))

(use-package embark-consult
  :defer 1
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x B" . consult-project-buffer)
         ("C-x r b" . consult-bookmark)
         ("C-c s s" . consult-ripgrep)
         ("M-g M-g" . consult-goto-line)
         ("M-y" . consult-yank-pop)
         ("C-x c i" . consult-imenu)
         ("C-M-s" . consult-line)
         ("C-M-S-s" . consult-line-multi)
         (:map minibuffer-local-map
               ("M-s" . consult-history)
               ("M-r" . consult-history)))
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-narrow-key "<")
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'multi-occur :override #'consult-multi-occur))


(use-package marginalia
  :defer 1
  :config
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :config
  (setq orderless-component-separator 'orderless-escapable-split-on-space)
  (defun orderless-flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun orderless-first-initialism (pattern index _total)
    (if (= index 0) 'orderless-initialism))

  (defun orderless-without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-style-dispatchers '(orderless-flex-if-twiddle
                                      orderless-without-if-bang)))

(use-package nerd-icons-completion
  :defer 1
  :config
  (nerd-icons-completion-mode t)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package hippie-exp
  :ensure nil
  :init
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-line
                                           try-expand-list
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol))
  :bind ("M-/" . hippie-expand))

(use-package corfu
  :defer 1
  :config
  (global-corfu-mode t)
  (setq corfu-auto nil
        corfu-auto-delay 0.04
        corfu-auto-prefix 2
        global-corfu-modes '((not erc-mode help-mode vterm-mode) t)
        corfu-preselect 'valid
        corfu-count 10
        corfu-max-width 120
        corfu-on-exact-match nil)

  (setq corfu-popupinfo-delay '(1.0 . 1.0))
  (add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))
  (add-hook 'corfu-mode-hook 'corfu-history-mode)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))



(use-package cape
  :after (corfu)
  :demand t
  :config
  (defun +cape-dabbrev-elisp-block()
    (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))
  (defun +cape-dabbrev()
    (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t))
  (add-hook 'org-mode-hook #'+cape-dabbrev-elisp-block)
  (add-hook 'markdown-mode-hook #'+cape-dabbrev-elisp-block)

  (dolist (mode '(text-mode-hook prog-mode-hook conf-mode-hook comint-mode-hook eshell-mode-hook))
    (add-hook mode #'+cape-dabbrev))

  (setq cape-dabbrev-check-other-buffers t)
  (add-hook 'completion-at-point-functions #'cape-file -10)
  (add-hook 'completion-at-point-functions #'cape-dabbrev 20)
  (setq dabbrev-ignored-buffer-regexps
        '("\\` "
          "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?"))
  (setq dabbrev-upcase-means-case-search t))



(use-package yasnippet-capf
  :after (corfu)
  :demand t
  :config
  (defun +yasnippet-capf()
    (add-hook 'completion-at-point-functions #'yasnippet-capf 30 t))
  (add-hook 'yas-minor-mode-hook #'+yasnippet-capf)
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))


(use-package nerd-icons-corfu
  :after (corfu)
  :demand t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


(use-package yasnippet
  :ensure yasnippet
  :defer 5
  :config
  (setq yasnippet-snippets-dir (expand-file-name "snippets" user-emacs-directory))
  (setq yas-snippet-dirs `(,yasnippet-snippets-dir))
  (yas-global-mode 1)
  :diminish (yas-minor-mode . "Ⓨ"))

(use-package autoinsert
  :ensure nil
  :defer 5
  :config
  (defun +autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (let ((yas-indent-line nil))
      (yas-expand-snippet (buffer-string) (point-min) (point-max))))

  (auto-insert-mode t)
  (setq auto-insert-directory (expand-file-name "auto-insert" user-emacs-directory))


  (add-to-list 'auto-insert-alist '((php-ts-mode . "PHP source code") . ["php.template" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '((go-ts-mode . "Go source code") . ["go.template" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '((sh-mode . "Shell scripts") . ["sh.template" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '((sh-mode . "Shell scripts") . ["sh.template" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '((emacs-lisp-mode . "Elisp source code") . ["el.template" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '(("blog-src/.*\\.org" . "Hugo blog") . ["hugo-org.template" +autoinsert-yas-expand]))

  (setq auto-insert-query nil))


(use-package copilot
  :ensure (:fetcher github :repo "copilot-emacs/copilot.el")
  :commands (copilot-login copilot-diagnose)
  :bind (;; ("C-c M-f" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("M-p" . 'copilot-previous-completion)
         ("M-n" . 'copilot-next-completion)
         ("<tab>" . 'copilot-accept-completion)
         ("M-f" . 'copilot-accept-completion-by-word)
         ("M-<return>" . 'copilot-accept-completion-by-line))
  :config
  ;; Sometimes the copilot agent doesn't start. Restarting fixes the issue.
  (copilot-diagnose)
  (setq copilot-indent-offset-warning-disable t
        copilot-max-char-warning-disable t
        copilot-max-char 10000))


(use-package copilot-chat
  :bind (("C-c q o" . copilot-chat-display)
         ("C-c q p" . copilot-chat-custom-prompt-selection))
  :config
  (setq copilot-chat-frontend 'shell-maker))


(use-package elysium
  :bind (("C-c q a" . elysium-query)
         ("C-c C-s A" . elysium-keep-all-suggested-changes)
         ("C-c C-s k" . elysium-discard-all-suggested-changes))
  :config
  ;; The manual merge that this creates is on wrong lines. TODO: Fix this. gptel-rewrite does it correctly but adds lang tags.
  (setq elysium-window-size 0.33)
  (setq elysium-window-style 'horizontal)
  (defhydra elysium-smerge-hydra (:color blue :hint nil :quit-key nil)
    "
        Smerge Navigation
        _n_: Next conflict    _p_: Previous conflict
        _a_: Accept lower     _k_: Keep upper
        _A_: Accept all       _R_: Reject all
        _r_: Resolve          _q_: Quit"
    ("n" smerge-next :exit nil)
    ("p" smerge-prev :exit nil)
    ("a" smerge-keep-lower :exit nil)
    ("k" smerge-keep-upper :exit nil)
    ("r" smerge-resolve :exit nil)
    ("A" elysium-keep-all-suggested-changes :exit nil)
    ("R" elysium-discard-all-suggested-changes :exit nil)
    ("q" nil :exit t))
  (defun elysium-apply-changes-setup ()
    (smerge-mode t)
    (elysium-smerge-hydra/body))
  (add-hook 'elysium-apply-changes-hook #'elysium-apply-changes-setup))


(use-package aider
  :ensure (:fetcher github :repo "tninja/aider.el")
  ;; :bind (("C-c C-a" . aider-transient-menu))
  :config
  (setq aider-args '("--model" "gpt-4o-mini"))
  (setenv "OPENAI_API_KEY" (secret-get openai-key)))


(use-package codeium
  :commands (codeium-install)
  :disabled
  :ensure nil
  :config
  (setq company-frontends '(company-preview-frontend))
  (defun +codeium-completions()
    (add-to-list 'completion-at-point-functions 'codeium-completion-at-point))
  (defun toggle-codeium()
    (interactive)
    (if (member 'codeium-completion-at-point completion-at-point-functions)
        (setq-local completion-at-point-functions (delete 'codeium-completion-at-point completion-at-point-functions))
      (add-to-list 'completion-at-point-functions 'codeium-completion-at-point)))

  ;; (add-hook 'prog-mode-hook '+codeium-completions)
  ;; (add-hook 'text-mode-hook '+codeium-completions)
  )




(provide 'core-completion)
