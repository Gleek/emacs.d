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
(use-package +popup :ensure nil :demand
  :bind ("C-c w p" . +popup/buffer)
  :config (setq +popup-margin-width nil)
  (+popup-mode t))

(use-package winner
  :ensure nil
  :config (winner-mode t)
  :bind (("C-<tab>" . winner-undo)
         ("C-S-<tab>" . winner-redo)
         ("C-c w /" . winner-undo)
         ("C-c w ?" . winner-redo)))

(use-package transpose-frame
  :bind ("C-c w t" . transpose-frame))
(use-package ace-window
  :bind ("C-:" . ace-window))

(use-package tab-bar
  :ensure nil
  :bind (("C-x t n" . tab-new)
         ("s-<tab>" . tab-next)
         ("C-x t u" . tab-bar-undo-close-tab)
         ("C-x t s" . +tab-bar-save-current-window))
  :defer 3
  :config
  (defun +tab-bar-get-tabs()
    (mapcar (lambda (tab)
              (alist-get 'name tab))
            (funcall tab-bar-tabs-function)))
  (defun +tab-bar-create-main()
    "Create a Main tab-bar if does not exist and switch to it."
    (let ((main-workspace "Main"))
      (if (member main-workspace (+tab-bar-get-tabs))
          (tab-bar-switch-to-tab main-workspace)
        (tab-bar-new-tab)
        (tab-bar-rename-tab main-workspace))))
  (defun +tab-bar-save-current-window()
    "Give current window layout a name and also create a Main tab
in background. The main purpose is to quickly save window
configurations."
    (interactive)
    (call-interactively 'tab-bar-rename-tab)
    (+tab-bar-create-main)
    (tab-bar-switch-to-recent-tab))
  (setq tab-bar-show nil)
  (tab-bar-mode t)
  (+tab-bar-create-main))

(use-package zoom
  ;; :defer 1
  :diminish
  :bind ("C-c w z" . zoom)
  :init
  (setq zoom-ignored-buffer-name-regexps '("^\*ansi-term.*"))
   ;; slightly bigger than what golden ration gives me.
  (setq zoom-size '(0.7 . 0.7)))

(provide 'core-window)
