;;; init-exwm.el -- Core exwm features
;;
;;; Commentary:
;; EXWM config are here

;;; Code:

(use-package exwm)
(require 'exwm-config)
(load "exwm-funcs")

;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)
(setq exwm-layout-show-all-buffers t)
(setq exwm-workspace-show-all-buffers t)

;; Performance issues here so disabling
;; ;; Rename buffer to have a proper name
;; (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
;; (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

(add-to-list 'exwm-manage-finish-hook 'exwm-rename-buffer)

(setq exwm-input-move-event 's-down-mouse-1
      exwm-input-resize-event 'M-down-mouse-1)

(setq exwm-input-global-keys
          `(
            ;; 's-r': Reset (to line-mode).
            ([?\s-r] . exwm-reset)
            ;; 's-w': Switch workspace.
            ([?\s-w] . exwm-workspace-switch)
            ;; 's-&': Launch application.
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ;; 's-N': Switch to certain workspace.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))

(exwm-input-set-key (kbd "s-<return>") 'popup-terminal)
(exwm-input-set-key (kbd "C-`") 'shell-pop)

(exwm-input-set-key (kbd "s-f") 'windmove-right)
(exwm-input-set-key (kbd "s-b") 'windmove-left)
(exwm-input-set-key (kbd "s-p") 'windmove-up)
(exwm-input-set-key (kbd "s-n") 'windmove-down)

(exwm-input-set-key (kbd "<XF86MonBrightnessUp>")   'brightness-up)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'brightness-down)
(exwm-input-set-key (kbd "<XF86KbdBrightnessUp>")   'backlight-up)
(exwm-input-set-key (kbd "<XF86KbdBrightnessDown>") 'backlight-down)
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")  'volume-up)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")  'volume-down)
(exwm-input-set-key (kbd "<XF86AudioMute>")         'volume-mute)
(exwm-input-set-key (kbd "<XF86AudioPlay>")         'play-pause)
(exwm-input-set-key (kbd "<XF86AudioNext>")         'play-next)
(exwm-input-set-key (kbd "<XF86AudioPrev>")         'play-previous)
(exwm-input-set-key (kbd "s-<XF86AudioNext>")       'play-lilfor)
(exwm-input-set-key (kbd "s-<XF86AudioPrev>")       'play-lilbak)
(exwm-input-set-key (kbd "S-s-<XF86AudioNext>")     'play-bigfor)
(exwm-input-set-key (kbd "S-s-<XF86AudioPrev>")     'play-bigbak)
(exwm-input-set-key (kbd "<XF86LaunchA>")           'screenshot)
(exwm-input-set-key (kbd "s-y")                     'exwm-counsel-yank-pop)
(exwm-input-set-key (kbd "s-E")                     'exwm-counsel-unicode-char)




(exwm-input-set-key (kbd "s-x") 'lock)
(exwm-input-set-key (kbd "s-t") 'terminal)
(exwm-input-set-key (kbd "s-d") 'counsel-linux-app)
(exwm-input-set-key (kbd "s-D") 'extrarofi)
(exwm-input-set-key (kbd "s-k") 'keepass)


(exwm-input-set-key (kbd "s-<tab>") 'winner-undo)
(exwm-input-set-key (kbd "s-<iso-lefttab>") 'winner-redo)
(exwm-input-set-key (kbd "s-`") 'rotate-windows)
(exwm-input-set-key (kbd "s-~") 'transpose-frame)
(exwm-input-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(exwm-input-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(exwm-input-set-key (kbd "s-<down>") 'shrink-window)
(exwm-input-set-key (kbd "s-<up>") 'enlarge-window)


(exwm-input-set-key (kbd "s-\\") 'gotop)
(exwm-input-set-key (kbd "s-F") 'ranger)
(exwm-input-set-key (kbd "s-|") 'nethogs)

(exwm-input--update-global-prefix-keys)


(exwm-input-set-simulation-key (kbd "C-c C-c") (kbd "C-c"))
;; (exwm-input-set-simulation-key (kbd "C-b") (kbd "<left>"))
;; (exwm-input-set-simulation-key (kbd "C-f") (kbd "<right>"))
;; (exwm-input-set-simulation-key (kbd "C-p") (kbd "<up>"))
;; (exwm-input-set-simulation-key (kbd "C-n") (kbd "<down>"))
;; (exwm-input-set-simulation-key (kbd "C-a") (kbd "<home>"))
;; (exwm-input-set-simulation-key (kbd "C-x h") (kbd "C-a"))
;; (exwm-input-set-simulation-key (kbd "C-e") (kbd "<end>"))
;; (exwm-input-set-simulation-key (kbd "M-v") (kbd "<prior>"))
(exwm-input-set-simulation-key (kbd "M-w") (kbd "C-c"))
(exwm-input-set-simulation-key (kbd "C-y") (kbd "C-v"))
(exwm-input-set-simulation-key (kbd "C-w") (kbd "C-x"))
(exwm-input-set-simulation-key (kbd "C-S-w") (kbd "C-w"))
;; (exwm-input-set-simulation-key (kbd "C-s") (kbd "C-f"))
;; (exwm-input-set-simulation-key (kbd "C-v") (kbd "<next>"))
;; (exwm-input-set-simulation-key (kbd "C-d") (kbd "<delete>"))
;; (exwm-input-set-simulation-key (kbd "C-k") (kbd "S-<end> <delete>"))
;; (exwm-input-set-simulation-key (kbd "C-x h") (kbd "C-a"))

(setq exwm-manage-configurations '(((string-match-p "^floating_term" exwm-title)
                                    floating t)
                                   (string-match-p "^dragger" exwm-class-name)
                                   floating t))

(setq window-divider-default-right-width 1)
(window-divider-mode)


(exwm-enable)


(load-x-resources)
(redshift)
(playerctldaemon)
(init-dropbox)


;; (exwm-input-set-key)



(provide 'init-exwm)
;;; init-exwm.el ends here
