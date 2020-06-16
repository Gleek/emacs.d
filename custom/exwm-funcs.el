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
  (shell-command (concat "pactl " cmd))
  (message "Volume command: %s" cmd))

(defun volume-mute () (interactive) (pactl "set-sink-mute @DEFAULT_SINK@ toggle"))
(defun volume-up () (interactive) (pactl "set-sink-volume @DEFAULT_SINK@ +5%"))
(defun volume-down () (interactive) (pactl "set-sink-volume @DEFAULT_SINK@ -5%"))



(defun brightness-up ()
  (interactive)
  (shell-command "light -A 2")
  (message "Brightness increased"))

(defun brightness-down ()
  (interactive)
  (shell-command "light -U 2")
  (message "Brightness decreased"))

(defun lock()
  (interactive)
  (shell-command "~/.config/i3/lock.sh > /dev/null 2>&1"))

(defun reboot()
  (interactive)
  (shell-command "systemctl reboot"))
(defun poweroff()
  (interactive)
  (shell-command "systemctl poweroff"))

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

(defun playerctl(command)
  (shell-command (concat "playerctl --player=playerctld,%any " command))
  (message "Player command %s" command))

(defun term-app (command)
  (exwm-run-or-raise "gnome-terminal" (concat "gnome-terminal -e \"" command "\"")))

(defun gotop() (interactive) (term-app "gotop"))
(defun ranger() (interactive) (term-app "ranger"))
(defun nethogs() (interactive) (term-app "nethogs wlp3s0"))

(defun musicplayer()
  (interactive)
  (exwm-run-or-raise "deadbeef" "deadbeef"))

(defun screenshot()
  (interactive)
  (shell-command "flameshot gui --path '/home/umar/Pictures/Screenshots/'"))


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

(provide 'exwm-funcs)
;;; exwm-funcs.el ends here
