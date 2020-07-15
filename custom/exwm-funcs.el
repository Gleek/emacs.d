;;; exwm-funcs.el -- exwm utility functions
;;
;;; Commentary:
;; EXWM utility functions are here

;;; Code:

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

(defun screen-term() (interactive) (term-app "screen -dR session"))

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
  (interactive) (exwm-run-or-raise "gnome-terminal" "gnome-terminal"))

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

(defun term-app (command)
  (exwm-run-or-raise "gnome-terminal" (concat "gnome-terminal -e \"" command "\"")))

(defun web-app(url)
  (exwm-run-or-raise "chrome-app" (concat "google-chrome-stable --app=\"" url "\"")))

(defun gotop() (interactive) (term-app "gotop"))
(defun ranger() (interactive) (term-app "ranger"))
(defun nethogs() (interactive) (term-app "nethogs wlp3s0"))


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

(provide 'exwm-funcs)
;;; exwm-funcs.el ends here

