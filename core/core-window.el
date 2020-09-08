(defun vsplit-last-buffer (prefix &optional size)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically size)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))
(defun hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))

(defun rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))

;; Courtesy: Doom emacs
(use-package popup :ensure nil :demand
  :config (setq +popup-margin-width nil)
  (+popup-mode t))

(use-package winner
  :config (winner-mode t)
  :bind (("C-<tab>" . winner-undo)
         ("C-S-<tab>" . winner-redo)))

(use-package transpose-frame
  :bind ("C-c w t" . transpose-frame))
(use-package ace-window
  :bind ("C-:" . ace-window))

(provide 'core-window)
