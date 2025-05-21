;;; emoji-mode.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Umar Ahmad
;; Created: April 06, 2025
;; Modified: April 06, 2025
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;;

;;; Code:
(defun my-get-emoji-for-char (char)
  "Return a list of candidate emojis for CHAR, or nil."
  (let ((char-downcase (downcase char)))
    (cond
     ((eq char-downcase ?a) '("🍎" "🐜" "✈️"))    ; Apple, Ant
     ((eq char-downcase ?b) '("🍌" "🫐" "🐻"  "💡" "⚾" ))    ; Banana, Blueberry, Bear
     ((eq char-downcase ?c) '("🍒" "🐈" "🍪" "🚗" "🚓" "🚙"))    ; Cherry, Cat, Cookie
     ((eq char-downcase ?d) '("🍩" "🫏" "🥁" "🐶" "👨‍⚕️" "🦆"))    ; Doughnut, Drum, Dog
     ((eq char-downcase ?e) '("🥚" "🐘" "🦅"))    ; Egg, Elephant
     ((eq char-downcase ?f) '("🍟" "🦊" "💐" "🐟"))    ; Fries, Fox, Flower
     ((eq char-downcase ?g) '("🦒" "🍇" "🍏" "🦍"))    ; Grapes, Green Apple, Green
     ((eq char-downcase ?h) '("🏠" "🚁" "🐴"))    ; House, Helicopter, Horse
     ((eq char-downcase ?i) '("🍦" "💉" "🪳" "🧊"))    ; Ice Cream, Ice
     ((eq char-downcase ?j) '("🧃" "🪼" "🧥" "🃏"))        ; Juice
     ((eq char-downcase ?k) '("🥝" "🐨" "🔑" "🪁" ))    ; Kiwi, Koala, Key
     ((eq char-downcase ?l) '("🍋" "🦁" "🍭" ))    ; Lemon, Lion, Lollipop
     ((eq char-downcase ?m) '("🥭" "🐒"  "🌙" "🦟" "🐁"))    ; Mango, Monkey, Moon
     ((eq char-downcase ?n) '("🥜" "🌃" "👃" "💅"))        ; Nut, Night
     ((eq char-downcase ?o) '("🍊" "🐙" "⭕" ))    ; Orange, Octopus, Circle
     ((eq char-downcase ?p) '("🍐" "🐷" "🍕" "🐧" "✏️" "🦜"))    ; Pear, Pig, Pizza
     ((eq char-downcase ?q) '( "👑" ))
     ((eq char-downcase ?r) '("🌈" "🐇" "🤖" ))    ; Rainbow, Rabbit, Robot
     ((eq char-downcase ?s) '("🍓" "☀️" "⭐" ))    ; Strawberry, Sun, Star
     ((eq char-downcase ?t) '("🐅" "🧸" "🌳" "🐢" ))    ; Teddy, Tree, Turtle
     ((eq char-downcase ?u) '("🦄" "☔"))    ; Unicorn, Umbrella
     ((eq char-downcase ?v) '("🚐" "🎻" "🌋"))
     ((eq char-downcase ?w) '("🍉" "🐳" "🐺" ))    ; Watermelon, Whale, Wolf
     ((eq char-downcase ?x) '( "🎄"))
     ((eq char-downcase ?y) '("🥱" "💛" "🪀" "🐂"))    ; Yellow, Yoyo
     ((eq char-downcase ?z) '("🦓" "🤐"))    ; Zebra, Zzz
     (t nil))))

(defun my-emoji-post-insert-hook-function ()
  "Hook function to replace typed characters with emojis."
  ;; Only run if the last command was self-insert and emoji-mode is active
  (when (and (eq last-command 'self-insert-command)
             emoji-mode)
    (let* ((char-inserted (char-before (point))) ;; Get the character just inserted
           (emoji-candidates (my-get-emoji-for-char char-inserted)))
      (when emoji-candidates
        (delete-backward-char 1) ;; Delete the original character
        (insert (nth (random (length emoji-candidates)) emoji-candidates))))))

(define-minor-mode emoji-mode
  "A minor mode that replaces typed letters with emojis via a hook."
  :init-value nil
  :lighter " Emoji"
  ;; No keymap needed for this approach
  (if emoji-mode
      ;; Add the hook function locally when mode is enabled
      (add-hook 'post-self-insert-hook #'my-emoji-post-insert-hook-function nil t)
    ;; Remove the hook function locally when mode is disabled
    (remove-hook 'post-self-insert-hook #'my-emoji-post-insert-hook-function t)))

;; Example usage: M-x emoji-mode
;;(global-set-key (kbd "C-c e") 'emoji-mode) ;; Example global binding.


;; Example usage: M-x emoji-mode
;;(global-set-key (kbd "C-c e") 'emoji-mode) ;; Example global binding.



(provide 'emoji-mode)
;;; emoji-mode.el ends here
