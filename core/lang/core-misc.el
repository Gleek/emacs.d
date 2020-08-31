
(use-package apache-mode :ensure t)
(use-package csv-mode :ensure t)


(use-package abbrev
  :ensure nil
  :diminish "🆎")



(use-package dap-mode)
(use-package lua-mode)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package terraform-mode
  :ensure terraform-mode
  :ensure company-terraform)

(use-package json-mode
  :bind (:map json-mode-map ("C-c C-f" . json-pretty-print)))
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


(provide 'core-misc)
