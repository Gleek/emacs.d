(use-package protobuf-mode
  :init
  ;; TODO: make a proper utility instead of this Macro
  (fset 'renumber-proto-message
        [?\C-\M-u ?\C-\M-s ?= ?\C-b ?\C-  ?\C-f ?\C-c ?m ?\C-  ?\C-k ?  ?\M-x ?m ?c ?/ ?i ?n ?s ?e ?r ?\C-n return C-S-up ?\; return])
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))

  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t)))
  (add-hook 'protobuf-mode-hook
            (function (lambda ()
                        (setq tab-width 2))))
  (defvar prototool-command)
  (setq prototool-command "/usr/local/bin/prototool")
  (defun prototool-format()
    (interactive)
    (message "Formatting proto file")
    (shell-command (concat
                    prototool-command
                    " format"
                    " -w "
                    (buffer-file-name)))
    (revert-buffer t t))
  (defun prototool-format-after-save()
    (interactive)
    (when (eq major-mode 'protobuf-mode)
      (prototool-format)))
  ;; (add-hook 'protobuf-mode
  ;;         (lambda ()
  ;;            (add-hook 'after-save-hook 'prototool-format-after-save nil t)))
  ;; (add-hook 'after-save-hook 'prototool-format-after-save)
  )
