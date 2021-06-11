(use-package counsel
  :ensure t
  :init
  (setq counsel-rg-base-command "rg -S --no-heading --line-number -M 500 --color never %s .")
  :bind (("M-x"     . counsel-M-x)
         ("C-c s s" . counsel-rg)
         ("C-x B"   . counsel-switch-buffer-other-window)
         ("C-c C-SPC" . counsel-mark-ring)
         ("M-y"     . counsel-yank-pop)
         ("C-x c i" . counsel-imenu)
         ("C-x C-f" . counsel-find-file)
         ("C-h v" . counsel-describe-variable)))

(use-package ivy
  :after popup
  :init
  (setq ivy-sort-max-size 7500
        ivy-height 17
        ivy-wrap nil
        ivy-use-selectable-prompt t
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ivy-use-selectable-prompt t)

  :config
  (ivy-mode t)
  (set-popup-rule! "^\\*ivy-occur" :size 0.35 :ttl 0 :quit nil)
  ;; (all-the-icons-ivy-setup)

  ;; Force change line spacing in ivy. There's a bug which makes ivy
  ;; hide few candidates if the spacing is made too large, but works
  ;; fine for smaller values.
  (defun +ivy-change-line-spacing(&rest _)
    (setq-local line-spacing (default-value 'line-spacing)))
  (advice-add 'ivy--minibuffer-setup :after #'+ivy-change-line-spacing)
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c v" . ivy-resume))
  :diminish ivy-mode)

(use-package ivy-hydra)

;; Bugging out lsp-ivy
;; (use-package all-the-icons-ivy-rich
;;   :defer 1
;;   :config
;;   (all-the-icons-ivy-rich-mode 1))

(use-package all-the-icons-ivy
  :defer 1
  :after counsel-projectile
  :config
  (setq all-the-icons-ivy-buffer-commands nil)
  (all-the-icons-ivy-setup)
  (let ((all-the-icons-ivy-file-commands
         '(counsel-projectile
           counsel-projectile-find-file
           counsel-projectile-find-dir)))
    (all-the-icons-ivy-setup)))



(use-package ivy-posframe
  :disabled
  :diminish
  :config
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  ;; (setq ivy-posframe-parameters '())
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)
          ;; (parent-frame nil)
          ))
  (setq ivy-posframe-font "Fira Code 12")
  (ivy-posframe-mode 1))

(use-package ivy-rich
  :defer 1
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  ;; Courtesy: doom emacs
  (defun +ivy-rich-describe-variable-transformer (cand)
    "Previews the value of the variable in the minibuffer"
    (let* ((sym (intern cand))
           (val (and (boundp sym) (symbol-value sym)))
           (print-level 3))
      (replace-regexp-in-string
       "[\n\t\^[\^M\^@\^G]" " "
       (cond ((booleanp val)
              (propertize (format "%s" val) 'face
                          (if (null val)
                              'font-lock-comment-face
                            'success)))
             ((symbolp val)
              (propertize (format "'%s" val)
                          'face 'highlight-quoted-symbol))
             ((keymapp val)
              (propertize "<keymap>" 'face 'font-lock-constant-face))
             ((listp val)
              (prin1-to-string val))
             ((stringp val)
              (propertize (format "%S" val) 'face 'font-lock-string-face))
             ((numberp val)
              (propertize (format "%s" val) 'face 'highlight-numbers-number))
             ((format "%s" val)))
       t)))
  (plist-put
   ivy-rich-display-transformers-list
   'counsel-describe-variable
   '(:columns
     ((counsel-describe-variable-transformer (:width 40)) ; the original transformer
      (+ivy-rich-describe-variable-transformer (:width 50)) ; display variable value
      (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))))

  (plist-put
   ivy-rich-display-transformers-list
   'counsel-projectile-switch-to-buffer
   (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
  (ivy-rich-mode t))



(use-package all-the-icons-ivy-rich
  :defer 1
  :config (all-the-icons-ivy-rich-mode t))



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
  :defer 5
  :ensure company-web
  :ensure company-quickhelp
  ;; :disabled t
  :hook ((prog-mode . +enable-company)
         (text-mode . +enable-company)
         (conf-mode . +enable-company))
  :bind ("C-." . company-complete)
  :init
  (defmacro company-backend-for-hook(hook backends)
    `(add-hook ,hook (lambda()
                       (set (make-local-variable 'company-backends)
                            ,backends))))
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
  :config
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

  (add-to-list 'auto-insert-alist '((php-mode . "PHP source code") . ["template.php" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '((go-mode . "Go source code") . ["template.go" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '((sh-mode . "Shell scripts") . ["template.sh" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '(("blog-src/.*\\.org" . "Hugo blog") . ["hugo-template.org" +autoinsert-yas-expand]))

  (setq auto-insert-query nil))


(provide 'core-completion)
