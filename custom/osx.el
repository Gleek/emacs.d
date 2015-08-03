;;; osx.el -- OSX related settings
;;; Commentary:
;; OSX settings for Emacs (Keybindings fixed by Karabiner)
;;; Code:
(setq is-mac (equal system-type 'darwin))
(when is-mac
  (setq mac-command-modifier 'meta
  ;;       mac-option-modifier 'control
        ns-function-modifier 'super)
  (exec-path-from-shell-initialize))
(when (not is-mac)

  (defun fullscreen ()
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
  (global-set-key (kbd "<f11>") 'fullscreen)

  (defun maximized (&optional f)
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

  (maximized)
  )
(provide 'osx)
;;; osx.el ends here
