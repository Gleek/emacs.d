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


(use-package company
  :ensure company-quickhelp
  :hook ((prog-mode . +enable-company)
         (text-mode . +enable-company)
         (conf-mode . +enable-company))
  :bind ("C-." . company-complete)
  :init
  (defmacro company-backend-for-hook(hook backends)
    `(add-hook ,hook (lambda()
                       (set (make-local-variable 'company-backends)
                            ,backends))))

  :config
  (setq company-idle-delay 0.01
        company-minimum-prefix-length 2
        company-require-match 'never
        company-tooltip-limit 10
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        ;; company-backends
        ;; (quote
        ;;  (company-semantic company-clang company-capf company-files
        ;;                    (company-dabbrev-code company-keywords)
        ;;                    company-dabbrev))
        company-backends '(company-capf company-yasnippet)
        company-frontends '(company-pseudo-tooltip-frontend)
        company-auto-commit nil
        company-auto-commit-chars nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  ;; (global-company-mode)
  (defun +enable-company()
    "Only enable company mode if the buffer is not huge"
    (company-mode (if (< (buffer-size) 1000000) t -1)))
  :diminish "Ⓒ")

(use-package company-box
  :diminish
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-images
        company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-icons-alist 'company-box-icons-nerd-icons
        company-box-icons-functions
        (cons #'+company-box-icons--elisp-fn
              (delq 'company-box-icons--elisp
                    company-box-icons-functions))
        company-box-icons-nerd-icons
        ;; Courtesy: doom emacs
        `((Unknown        . ,(nerd-icons-codicon  "nf-cod-code"                :face  'font-lock-warning-face))
          (Text           . ,(nerd-icons-codicon  "nf-cod-text_size"           :face  'font-lock-doc-face))
          (Method         . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
          (Function       . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
          (Constructor    . ,(nerd-icons-codicon  "nf-cod-triangle_right"      :face  'font-lock-function-name-face))
          (Field          . ,(nerd-icons-codicon  "nf-cod-symbol_field"        :face  'font-lock-variable-name-face))
          (Variable       . ,(nerd-icons-codicon  "nf-cod-symbol_variable"     :face  'font-lock-variable-name-face))
          (Class          . ,(nerd-icons-codicon  "nf-cod-symbol_class"        :face  'font-lock-type-face))
          (Interface      . ,(nerd-icons-codicon  "nf-cod-symbol_interface"    :face  'font-lock-type-face))
          (Module         . ,(nerd-icons-codicon  "nf-cod-file_submodule"      :face  'font-lock-preprocessor-face))
          (Property       . ,(nerd-icons-codicon  "nf-cod-symbol_property"     :face  'font-lock-variable-name-face))
          (Unit           . ,(nerd-icons-codicon  "nf-cod-symbol_ruler"        :face  'font-lock-constant-face))
          (Value          . ,(nerd-icons-codicon  "nf-cod-symbol_field"        :face  'font-lock-builtin-face))
          (Enum           . ,(nerd-icons-codicon  "nf-cod-symbol_enum"         :face  'font-lock-builtin-face))
          (Keyword        . ,(nerd-icons-codicon  "nf-cod-symbol_keyword"      :face  'font-lock-keyword-face))
          (Snippet        . ,(nerd-icons-codicon  "nf-cod-symbol_snippet"      :face  'font-lock-string-face))
          (Color          . ,(nerd-icons-codicon  "nf-cod-symbol_color"        :face  'success))
          (File           . ,(nerd-icons-codicon  "nf-cod-symbol_file"         :face  'font-lock-string-face))
          (Reference      . ,(nerd-icons-codicon  "nf-cod-references"          :face  'font-lock-variable-name-face))
          (Folder         . ,(nerd-icons-codicon  "nf-cod-folder"              :face  'font-lock-variable-name-face))
          (EnumMember     . ,(nerd-icons-codicon  "nf-cod-symbol_enum_member"  :face  'font-lock-builtin-face))
          (Constant       . ,(nerd-icons-codicon  "nf-cod-symbol_constant"     :face  'font-lock-constant-face))
          (Struct         . ,(nerd-icons-codicon  "nf-cod-symbol_structure"    :face  'font-lock-variable-name-face))
          (Event          . ,(nerd-icons-codicon  "nf-cod-symbol_event"        :face  'font-lock-warning-face))
          (Operator       . ,(nerd-icons-codicon  "nf-cod-symbol_operator"     :face  'font-lock-comment-delimiter-face))
          (TypeParameter  . ,(nerd-icons-codicon  "nf-cod-list_unordered"      :face  'font-lock-type-face))
          (Template       . ,(nerd-icons-codicon  "nf-cod-symbol_snippet"      :face  'font-lock-string-face))
          (ElispFunction  . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
          (ElispVariable  . ,(nerd-icons-codicon  "nf-cod-symbol_variable"     :face  'font-lock-variable-name-face))
          (ElispFeature   . ,(nerd-icons-codicon  "nf-cod-globe"               :face  'font-lock-builtin-face))
          (ElispFace      . ,(nerd-icons-codicon  "nf-cod-symbol_color"        :face  'success))))
  (defun +company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace))))))


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
         (text-mode . copilot-mode)
         (conf-mode . copilot-mode))
  :config
  ;; Sometimes the copilot agent doesn't start. Restarting fixes the issue.
  (copilot-diagnose)
  (setq copilot-indent-offset-warning-disable t
        copilot-max-char-warning-disable t
        copilot-max-char 10000))


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
