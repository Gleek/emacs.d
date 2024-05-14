
(use-package apache-mode :ensure t)
(use-package csv-mode :ensure t)


(use-package abbrev
  :ensure nil
  :diminish "🆎")

(use-package lua-mode)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package terraform-mode)

(use-package json-mode
  :bind (:map json-mode-map
              (("C-c C-f" . json-pretty-print)
               ("C-c C-d" . nil))))
(use-package json-ts-mode
  :ensure nil
  :init
  (eval-after-load 'org
    '(add-to-list 'org-src-lang-modes '("json" . json-ts)))
  :bind (:map json-ts-mode-map
              ("C-z p" . json-mode-kill-path))
  :config
  (add-hook 'json-ts-mode-hook (lambda() (setq format-all-formatters '(("JSON" prettier))))))

(use-package counsel-jq
  :after json-mode
  :bind (:map json-mode-map ("C-c C-s" . counsel-jq))
  :config
  (set-popup-rule! "^\\*jq-json*\\$" :size 0.35 :ttl 0 :quit nil))

;; npm install -g quicktype
(use-package quick-type
  :commands quicktype
  :ensure nil)

(use-package less-css-mode)
(use-package rainbow-mode :diminish "🌈")


(use-package rust-mode
  :init
  (setq rust-format-on-save t)
  :config
  (flycheck-mode -1))

(use-package robe
  :ensure t
  :config
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby))
  (add-hook 'ruby-mode-hook 'robe-mode)
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package swift-mode)


(use-package makefile-executor
  :hook (makefile-mode . makefile-executor-mode)
  :bind (("C-c p m". makefile-executor-execute-project-target)
         (:map makefile-executor-mode-map
               ("C-c C-d" . nil))))

(use-package make-mode :ensure nil)

(provide 'core-misc)
