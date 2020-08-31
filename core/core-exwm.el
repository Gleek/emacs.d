(use-package exwm)
(require 'exwm-config)

(defun exwm-run-or-raise (buffer-prefix &optional cmd)
  (let ((existing-buffer
         (cl-dolist (buffer (buffer-list))
           (if (string-prefix-p buffer-prefix (buffer-name buffer))
               (return buffer)))))
    (if existing-buffer
        ;; it's currently displayed, go to it
        (if (get-buffer-window existing-buffer)
            (message (format "%s" (pop-to-buffer existing-buffer)))
          (exwm-workspace-switch-to-buffer existing-buffer))
      (start-process-shell-command buffer-prefix nil cmd))))

(defun pactl (cmd)
  (exwm-run-or-raise "volume control" (concat "pactl " cmd))
  (message "Volume command: %s" cmd))

(defun volume-mute () (interactive) (pactl "set-sink-mute @DEFAULT_SINK@ toggle"))
(defun volume-up () (interactive) (pactl "set-sink-volume @DEFAULT_SINK@ +5%"))
(defun volume-down () (interactive) (pactl "set-sink-volume @DEFAULT_SINK@ -5%"))


(defun backlight(action count)
  (message "Backlight %s by %d" action count)
  (start-process-shell-command "backlight" nil (concat "~/.config/i3/kbacklight.sh " action " " (number-to-string count))))

(defun backlight-up() (interactive) (backlight "up" 10))
(defun backlight-down() (interactive) (backlight "down" 10))

(defun brightness(args)
  (start-process-shell-command "brightness" nil (concat "light " args)))

(defun brightness-up ()
  (interactive)
  (brightness "-A 2")
  (message "Brightness increased"))

(defun brightness-down ()
  (interactive)
  (brightness "-U 2")
  (message "Brightness decreased"))

(defun lock()
  (interactive)
  (start-process-shell-command "lock-computer" nil "/home/umar/.config/i3/lock.sh > /tmp/lockout 2>&1"))

(defun reboot()
  (interactive)
  (start-process-shell-command "systemctl" nil "systemctl reboot"))
(defun poweroff()
  (interactive)
  (start-process-shell-command "systemctl" nil "systemctl poweroff"))

(defun screen-term() (interactive) (term-app "term" "screen -dR session"))

(defun pop-term()
  (interactive)
  (exwm-run-or-raise "pop-term" "gnome-terminal -t floating_term -e \"screen -dR session\"")
  (message "Popping term"))

(defun redshift()
  (interactive)
  (exwm-run-or-raise "redshift" "redshift"))

(defun init-dropbox()
  (interactive)
  (exwm-run-or-raise "dropbox" "dropbox start"))

(defun load-x-resources () (interactive) (shell-command "xrdb -merge ~/.Xresources"))

(defun terminal()
  (interactive) (start-process-shell-command "kitty"  nil "kitty"))

(defun popup-terminal()
  (interactive)
  (vsplit-last-buffer 1 -15)
  ;; (other-window 1 nil nil)
  (screen-term))

;; (defun rofi()
;;   (interactive)
;;   (exwm-run-or-raise "rofi" "rofi -modi combi -show combi -combi-modi window,drun,run"))
(defun extrarofi()
  (interactive)
  (exwm-run-or-raise "rofi" "~/.config/rofi/scripts/rofi.sh"))

(defun keepass()
  (interactive) (exwm-run-or-raise "keepassxc" "keepassxc"))

(defun playerctldaemon()
  (interactive)
  (exwm-run-or-raise "playerctld" "playerctld"))

(defun qrshow()
  (interactive)
  (start-process-shell-command "qr" nil (concat
                                         "~/.config/rofi/scripts/qr.sh "
                                         (shell-quote-argument
                                          (substring-no-properties
                                           (car kill-ring))))))


(defun playerctl(command)
  (start-process-shell-command "player" nil (concat "playerctl --player=playerctld,%any " command))
  (message "Player command %s" command))

(defun term-app (title command)
  (start-process-shell-command
   "kitty"
   nil
   (format
    "kitty -T \"%s\" %s" title command)))

(defun web-app(url)
  (exwm-run-or-raise "chrome-app" (concat "google-chrome-stable --app=\"" url "\"")))

(defun gotop() (interactive) (term-app "gotop" "gotop"))
(defun ranger() (interactive) (term-app "ranger" "ranger"))
(defun nethogs() (interactive) (term-app "nethogs" "nethogs wlp3s0"))


(defun dingtalk() (interactive) (exwm-run-or-raise "DingTalk" "/mnt/data/Apps/DingTalk-linux-x64/DingTalk"))

(defun musicplayer()
  (interactive)
  (exwm-run-or-raise "deadbeef" "deadbeef"))

(defun screenshot()
  (interactive)
  (exwm-run-or-raise "screenshot" "flameshot gui --path '/home/umar/Pictures/Screenshots/'"))


(defun play-pause() (interactive) (playerctl "play-pause"))
(defun play-next() (interactive) (playerctl "next"))
(defun play-previous() (interactive) (playerctl "previous"))
(defun play-lilfor() (interactive) (playerctl "position 10+"))
(defun play-lilbak() (interactive) (playerctl "position 10-"))
(defun play-bigfor() (interactive) (playerctl "position 120+"))
(defun play-bigbak() (interactive) (playerctl "position 120-"))

(defun exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ":"
           (if (<= (length exwm-title) 30) exwm-title
             (concat (substring exwm-title 0 29))))))


(defun exwm-counsel-yank-pop ()
  "Same as `counsel-yank-pop' and paste into exwm buffer."
  (interactive)
  (let ((inhibit-read-only t)
        ;; Make sure we send selected yank-pop candidate to
        ;; clipboard:
        (yank-pop-change-selection t))
    (call-interactively #'counsel-yank-pop))
  (when (derived-mode-p 'exwm-mode)
    (exwm-input--fake-key ?\C-v)))

(defun exwm-counsel-unicode-char ()
  "Same as `counsel-unicode-char' and paste into exwm buffer."
  (interactive)
  (let ((inhibit-read-only t)
        ;; Make sure we send selected yank-pop candidate to
        ;; clipboard:
        (yank-pop-change-selection t))
    (kill-new (char-to-string (get-text-property 0 'code (call-interactively #'counsel-unicode-char))))
    (when (derived-mode-p 'exwm-mode)
      (exwm-input--fake-key ?\C-v))))


(defun toggle-polybar()
  (interactive)
  (start-process-shell-command "polybar-toggle" nil "~/.config/polybar/toggle.sh"))

;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)
(setq exwm-layout-show-all-buffers t)
(setq exwm-workspace-show-all-buffers t)
;; (setq exwm-workspace-number 5)

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
(exwm-input-set-key (kbd "s-B")                     'toggle-polybar)




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

;; (setq exwm-manage-configurations '(((string-match-p "^floating_term" exwm-title)
;;                                     floating t)
;;                                    (string-match-p "^dragger" exwm-class-name)
;;                                    floating t))

(setq window-divider-default-right-width 1)
(window-divider-mode)


(exwm-enable)


(load-x-resources)
(redshift)
(playerctldaemon)
(init-dropbox)


;; (exwm-input-set-key)

(provide 'core-exwm)
