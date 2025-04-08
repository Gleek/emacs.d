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
               ("M-q" . vertico-multiform-flat)
               ("C-c C-c" . embark-act)
               (">" . embark-become)
               ("M-*" . embark-act-all)
               ("C-<tab>" . embark-act-with-completing-read)
               ("C-c C-o" . embark-export)))
  :hook (minibuffer-setup . vertico-repeat-save)
  :config
  (require 'embark)
  (vertico-mode t)
  (vertico-multiform-mode t) ; M-{B(uffer) F(lat) G(rid) R(everse) U(nobtrusive) V(ertical)}'
  (setq vertico-resize nil
        vertico-count 15)
  ;; (setq vertico-multiform-commands nil)
  ;; (setq vertico-multiform-categories
  ;;       '((consult-location buffer)
  ;;         (consult-grep buffer)))
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (if (bound-and-true-p savehist-loaded)
      (add-to-list 'savehist-additional-variables
                   'vertico-repeat-history)))

(use-package embark
  :bind* (("M-o" . embark-act)
          ("C-M-o" . embark-act-noquit)
          (:map minibuffer-local-map
                ("C-," . embark-become)
                ("C-<tab>" . embark-act-with-completing-read)
                ("C-<return>" . embark-dwim-noquit)))
  :config
  (set-popup-rule! "^\\*Embark Export:" :size 0.35 :ttl 0 :quit nil)
  (setq embark-help-key "?")
  (setq embark-quit-after-action
        '((consult-projectile-embark-action-remove . nil)
          (t . t)))
  ;; (defvar +embark-become-keymap (define-keymap))
  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))
  (defun embark-dwim-noquit()
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-dwim)
      (embark--restart)))
  (defun embark-act-with-completing-read (&optional arg)
    (interactive "P")
    (let* ((embark-prompter 'embark-completing-read-prompter)
           (embark-indicators '(embark-minimal-indicator)))
      (embark-act arg)))
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
  :bind (("C-x b"   . consult-buffer)
         ("C-x B"   . consult-project-buffer)
         ("C-x r b" . consult-bookmark)
         ("C-c s s" . consult-ripgrep)
         ("M-s g"   . consult-ripgrep)
         ("M-s r"   . consult-isearch-history)
         ("M-g M-g" . consult-goto-line)
         ("M-y"     . consult-yank-pop)
         ("C-x c i" . consult-imenu)
         ("C-M-s"   . consult-line)
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
  (setq-default corfu-auto t
                corfu-auto-delay 0.04
                corfu-auto-prefix 2
                global-corfu-modes '((not erc-mode help-mode vterm-mode) t)
                corfu-preselect 'valid
                corfu-count 10
                corfu-max-width 120
                corfu-on-exact-match nil)

  (defun text-corfu-configuration()
    (setq-local corfu-auto-prefix 3
                corfu-auto-delay 0.2))

  (add-hook 'text-mode-hook #'text-corfu-configuration)

  (setq corfu-popupinfo-delay '(1.0 . 1.0))
  (add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))
  (add-hook 'corfu-mode-hook 'corfu-history-mode)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))


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
  :hook ((prog-mode . copilot-mode)
         ;; (text-mode . copilot-mode)
         (conf-mode . copilot-mode))
  :config
  (setq copilot-install-dir (expand-file-name "copilot" CACHE-DIR))
  (setq copilot-indent-offset-warning-disable t
        copilot-max-char-warning-disable t
        copilot-max-char 100000))


(use-package copilot-chat
  :bind (("C-c q o" . copilot-chat-display)
         ("C-c q p" . copilot-chat-custom-prompt-selection))
  :config
  (setopt copilot-chat-default-model "claude-3.7-sonnet")
  (setopt copilot-chat-frontend 'shell-maker))




(provide 'core-completion)
