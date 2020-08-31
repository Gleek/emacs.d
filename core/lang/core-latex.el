;; TODO:
(defun TeX-toggle-escape nil (interactive)
       "Toggle Shell Escape."
       (setq LaTeX-command
             (if (string= LaTeX-command "latex") "latex -shell-escape"
               "latex"))
       (message (concat "shell escape "
                        (if (string= LaTeX-command "latex -shell-escape")
                            "enabled"
                          "disabled"))
                ))
(provide 'core-latex)
