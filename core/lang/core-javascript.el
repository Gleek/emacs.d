(use-package js2-mode
  ;; :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-basic-offset 2
        js-chain-indent t
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-highlight-external-variables t
        js2-idle-timer-delay 0.1)
  ;; (load "lsp-javascript")
  ;; (add-hook 'js2-mode-hook (lambda ()
  ;;                            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  :bind (:map js2-mode-map ("M-." . nil))
  :ensure t)


;; (use-package rjsx-mode :mode ("\\.jsx\\'" . rjsx-mode) )

(use-package js
  :mode ("\\.jsx?\\'" . js-ts-mode)
  :config
  (add-hook 'js-ts-mode-hook (lambda() (setq format-all-formatters '(("Javascript" prettier)))))

  ;; Update js--treesit-font-lock-settings to remove invalid queries
  ;; queries starting with (function... are invalid
  ;; This is fixed in emacs 29.3 and will be removed once I move to that
  (setq js--treesit-font-lock-settings
        (treesit-font-lock-rules

         :language 'javascript
         :feature 'comment
         '([(comment) (hash_bang_line)] @font-lock-comment-face)

         :language 'javascript
         :feature 'constant
         '(((identifier) @font-lock-constant-face
            (:match "\\`[A-Z_][0-9A-Z_]*\\'" @font-lock-constant-face))

           [(true) (false) (null)] @font-lock-constant-face)

         :language 'javascript
         :feature 'keyword
         `([,@js--treesit-keywords] @font-lock-keyword-face
           [(this) (super)] @font-lock-keyword-face)

         :language 'javascript
         :feature 'string
         '((regex pattern: (regex_pattern)) @font-lock-regexp-face
           (string) @font-lock-string-face)

         :language 'javascript
         :feature 'string-interpolation
         :override t
         '((template_string) @js--fontify-template-string
           (template_substitution ["${" "}"] @font-lock-misc-punctuation-face))

         :language 'javascript
         :feature 'definition
         '((class_declaration
            name: (identifier) @font-lock-type-face)

           (function_declaration
            name: (identifier) @font-lock-function-name-face)

           (method_definition
            name: (property_identifier) @font-lock-function-name-face)

           (formal_parameters
            [(identifier) @font-lock-variable-name-face
             (array_pattern (identifier) @font-lock-variable-name-face)
             (object_pattern (shorthand_property_identifier_pattern) @font-lock-variable-name-face)])

           (variable_declarator
            name: (identifier) @font-lock-variable-name-face)

           (variable_declarator
            name: (identifier) @font-lock-function-name-face
            value: [(arrow_function)])

           (variable_declarator
            name: [(array_pattern (identifier) @font-lock-variable-name-face)
                   (object_pattern
                    (shorthand_property_identifier_pattern) @font-lock-variable-name-face)])

           ;; full module imports
           (import_clause (identifier) @font-lock-variable-name-face)
           ;; named imports with aliasing
           (import_clause (named_imports (import_specifier
                                          alias: (identifier) @font-lock-variable-name-face)))
           ;; named imports without aliasing
           (import_clause (named_imports (import_specifier
                                          !alias
                                          name: (identifier) @font-lock-variable-name-face)))

           ;; full namespace import (* as alias)
           (import_clause (namespace_import (identifier) @font-lock-variable-name-face)))

         :language 'javascript
         :feature 'assignment
         '((assignment_expression
            left: (_) @js--treesit-fontify-assignment-lhs))

         :language 'javascript
         :feature 'function
         '((call_expression
            function: [(identifier) @font-lock-function-call-face
                       (member_expression
                        property:
                        (property_identifier) @font-lock-function-call-face)]))

         :language 'javascript
         :feature 'jsx
         '((jsx_opening_element name: (_) @font-lock-function-call-face)
           (jsx_closing_element name: (_) @font-lock-function-call-face)
           (jsx_self_closing_element name: (_) @font-lock-function-call-face)
           (jsx_attribute (property_identifier) @font-lock-constant-face))

         :language 'javascript
         :feature 'property
         '(((property_identifier) @font-lock-property-use-face)
           (pair value: (identifier) @font-lock-variable-use-face)
           ((shorthand_property_identifier) @font-lock-property-use-face))

         :language 'javascript
         :feature 'number
         '((number) @font-lock-number-face
           ((identifier) @font-lock-number-face
            (:match "\\`\\(?:NaN\\|Infinity\\)\\'" @font-lock-number-face)))

         :language 'javascript
         :feature 'operator
         `([,@js--treesit-operators] @font-lock-operator-face
           (ternary_expression ["?" ":"] @font-lock-operator-face))

         :language 'javascript
         :feature 'bracket
         '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

         :language 'javascript
         :feature 'delimiter
         '((["," "." ";" ":"]) @font-lock-delimiter-face)

         :language 'javascript
         :feature 'escape-sequence
         :override t
         '((escape_sequence) @font-lock-escape-face))))

(provide 'core-javascript)
