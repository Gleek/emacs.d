(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'hbar
                      'box)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
