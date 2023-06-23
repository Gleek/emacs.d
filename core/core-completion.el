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

  ;; (all-the-icons-ivy-setup)

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
          ("C-;" . embark-dwim)
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
         ("M-y" . consult-yank-replace)
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


(use-package all-the-icons-completion
  :defer 1
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

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
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-require-match 'never
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
        company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-icons-functions
        (cons #'+company-box-icons--elisp-fn
              (delq 'company-box-icons--elisp
                    company-box-icons-functions))
        company-box-icons-all-the-icons
        ;; Courtesy: doom emacs
        (let ((all-the-icons-scale-factor 0.8))
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))))
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
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (auto-insert-mode t)
  (setq auto-insert-directory (expand-file-name "auto-insert" user-emacs-directory))

  (eval-after-load 'string-inflection
    '(add-to-list 'auto-insert-alist '((php-mode . "PHP source code") . ["php.template" +autoinsert-yas-expand])))
  (add-to-list 'auto-insert-alist '((go-mode . "Go source code") . ["go.template" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '((sh-mode . "Shell scripts") . ["sh.template" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '((sh-mode . "Shell scripts") . ["sh.template" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '((emacs-lisp-mode . "Elisp source code") . ["el.template" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '(("blog-src/.*\\.org" . "Hugo blog") . ["hugo-org.template" +autoinsert-yas-expand]))

  (setq auto-insert-query nil))


(use-package copilot
  :load-path "packages/copilot/"
  :ensure s
  :ensure dash
  :ensure editorconfig
  :commands (copilot-login copilot-diagnose)
  :bind (;; ("C-c M-f" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("M-p" . 'copilot-previous-completion)
         ("M-n" . 'copilot-next-completion)
         ("<tab>" . 'copilot-accept-completion)
         ("M-f" . 'copilot-accept-completion-by-word)
         ("M-<return>" . 'copilot-accept-completion-by-line))
  ;; :hook ((prog-mode . copilot-mode)
  ;;        (text-mode . copilot-mode)
  ;;        (conf-mode . copilot-mode))
  )



(provide 'core-completion)
