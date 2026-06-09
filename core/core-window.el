;;; core-window.el --- Window management -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar shackle-rules nil)
(defvar popper-reference-buffers nil)
(defvar +popper-escape-ignored-buffers nil)

(defmacro +popup-rule (condition &rest plist)
  "Register CONDITION as a Shackle rule and Popper reference.
The PLIST syntax is Shackle's rule plist, plus local keys:
:popper nil skips Popper registration.
:escape nil keeps `escape-quit' from dismissing matching Popper buffers."
  `(+window-popup-rule ,condition ',plist))

(defun +window-popup-rule (condition plist)
  "Register CONDITION using PLIST after Shackle and Popper load."
  (let* ((popper-disabled (and (plist-member plist :popper)
                               (not (plist-get plist :popper))))
         (escape-disabled (and (plist-member plist :escape)
                               (not (plist-get plist :escape))))
         (ignore (plist-get plist :ignore))
         (shackle-plist (cl-loop for (key value) on plist by #'cddr
                                  unless (memq key '(:popper :escape))
                                  append (list key value)))
         (shackle-rule (cons condition shackle-plist)))
    (with-eval-after-load 'shackle
      (cl-pushnew shackle-rule shackle-rules :test #'equal))
    (unless (or ignore popper-disabled)
      (with-eval-after-load 'popper
        (cl-pushnew condition popper-reference-buffers :test #'equal)
        (when (fboundp 'popper--set-reference-vars)
          (popper--set-reference-vars))
        (when (and (bound-and-true-p popper-mode)
                   (fboundp 'popper--update-popups))
          (popper--update-popups))))
    (when escape-disabled
      (with-eval-after-load 'popper
        (cl-pushnew condition +popper-escape-ignored-buffers :test #'equal)))))

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

(use-package emacs :ensure nil
  :bind (("C-x 2"   . vsplit-last-buffer)
         ("C-x 3"   . hsplit-last-buffer)
         ("C-c w r" . rotate-windows)
         ("C-c w T" . tear-off-window)
         ("C-x K"   . kill-buffer-and-window)
         ("C-x 4 f" . find-file-other-window)))

(use-package windmove :ensure nil
  :bind (("C-<right>" . windmove-right)
         ("C-<left>"  . windmove-left)
         ("C-<up>"    . windmove-up)
         ("C-<down>"  . windmove-down)))

(use-package shackle
  :defer 0.1
  :config
  (setq shackle-rules nil
        popper-reference-buffers nil)
  (+popup-rule "^\\*Completions" :regexp t :ignore t)
  (+popup-rule "^\\*Local variables\\*$" :regexp t :align below :size 0.25)
  (+popup-rule "^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)" :regexp t :align below :size 0.3)
  (+popup-rule "^\\*\\(?:Wo\\)?Man " :regexp t :align below :size 0.45 :select t)
  (+popup-rule "^\\*wclock" :regexp t :align below :size 0.4 :select t)
  (+popup-rule "^\\*Customize" :regexp t :align right :size 0.5 :select t :escape nil)
  (+popup-rule "^\\*info\\*$" :regexp t :align right :size 0.45 :select t)
  (+popup-rule "^\\*Warnings" :regexp t :align below :size 0.25)
  (+popup-rule "^\\*Backtrace" :regexp t :align below :size 0.4 :escape nil)
  (+popup-rule "^\\*Async Shell Command*" :regexp t :align below :size 0.4)
  (+popup-rule "^\\*CPU-Profiler-Report " :regexp t :align below :size 0.4 :escape nil)
  (+popup-rule "^\\*Memory-Profiler-Report " :regexp t :align below :size 0.4 :escape nil)
  (+popup-rule "^\\*Process List\\*" :regexp t :align below :size 0.25 :select t)
  (+popup-rule "^\\*\\(?:Proced\\|timer-list\\|Process List\\|Abbrevs\\|Output\\|Occur\\|unsent mail\\)\\*" :regexp t :ignore t)
  (shackle-mode 1))

(use-package popper
  :defer 0.1
  :bind (("C-`" . popper-toggle)
         ("C-M-`" . popper-cycle)
         ("C-x k" . +popper-kill-current-buffer)
         ("C-c w p" . +popper-toggle-type))
  :config
  (with-eval-after-load 'projectile
    (setq popper-group-function #'popper-group-by-projectile))
  ;; Shackle owns placement; Popper owns popup state/toggling.
  (setq popper-display-control nil)
  (popper-mode 1)
  (popper-echo-mode 1)


  (defvar +popper-escape-ignored-buffers nil
    "Popup buffers that `escape-quit' should not dismiss.")
  (defun +popper-escape-ignored-buffer-p (buffer)
    (cl-some (lambda (regexp)
               (string-match-p regexp (buffer-name buffer)))
             +popper-escape-ignored-buffers))

  (defun +popper-close-on-escape-h ()
    "Dismiss the latest Popper popup from `escape-hook'."
    (when (and (bound-and-true-p popper-mode)
               (boundp 'popper-open-popup-alist)
               popper-open-popup-alist)
      (let ((buffer (cdar popper-open-popup-alist)))
        (when (and (buffer-live-p buffer)
                   (not (+popper-escape-ignored-buffer-p buffer)))
          (popper-toggle)
          t))))

  (defun +popper-toggle-type (&optional buffer)
    "Toggle popup status of BUFFER without moving its window when raising.

`popper-raise-popup' re-displays the buffer via `pop-to-buffer',
which makes Shackle snap it back to its popup slot."
    (interactive)
    (let* ((buf (get-buffer (or buffer (current-buffer))))
           (status (buffer-local-value 'popper-popup-status buf)))
      (pcase status
        ((or 'popup 'user-popup)
         (with-current-buffer buf
           (setq popper-popup-status (if (popper-popup-p buf) 'raised nil))
           (setq mode-line-format (default-value 'mode-line-format)))
         (popper--update-popups))
        (_ (popper-lower-to-popup buf)))))

  (defun +popper-kill-current-buffer ()
    "Kill current buffer, closing its Popper popup window when applicable."
    (interactive)
    (call-interactively
     (if (and (bound-and-true-p popper-mode)
              (boundp 'popper-open-popup-alist)
              (eq (cdr (assq (selected-window) popper-open-popup-alist))
                  (current-buffer)))
         #'kill-buffer-and-window
       #'kill-current-buffer)))


  (add-hook 'escape-hook #'+popper-close-on-escape-h 'append))

(use-package winner
  :ensure nil
  :defer 1
  :config (winner-mode t)
  :bind (("C-c w /" . winner-undo)
         ("C-c w ?" . winner-redo)))

(use-package transpose-frame
  :bind ("C-c w t" . transpose-frame))
(use-package ace-window
  :bind (("C-;" . ace-window-one-command)
         ("C-:" . ace-window))
  :config
  ;; Courtesy: karthinks
  (defun ace-window-one-command ()
    (interactive)
    (let ((win (aw-select " ACE")))
      (when (windowp win)
        (with-selected-window win
          (let* ((command (key-binding
                           (read-key-sequence
                            (format "Run in %s..." (buffer-name)))))
                 (this-command command))
            (call-interactively command)))))))

(use-package tab-bar
  :ensure nil
  :bind (("C-x t n" . tab-new)
         ("S-s-<tab>" . tab-next)
         ("C-x t u" . tab-bar-undo-close-tab)
         ("C-x t s" . +tab-bar-save-current-window))
  :config
  (defun +tab-bar-get-tabs()
    (mapcar (lambda (tab)
              (alist-get 'name tab))
            (funcall tab-bar-tabs-function)))
  (defun +tab-bar-create-main()
    "Create a Main tab-bar if does not exist and switch to it."
    (let ((main-workspace "⁘"))
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
  (setq tab-bar-show nil))

(use-package zoom
  ;; :defer 1
  :diminish
  :bind (("C-c w z" . zoom)
         ("C-c w Z" . zoom-out))
  :init
  (setq zoom-ignored-buffer-name-regexps '("^\*ansi-term.*"))
  ;; slightly bigger than what golden ration gives me.
  (setq zoom-size '(0.7 . 0.7))
  :config
  (defun zoom-out()
    (interactive)
    (cl-letf (((symbol-function 'zoom--resize-one-dimension) #'zoom--resize-shrink-dimension))
      (let ((zoom-size '(0.3 . 0.3)))
        (zoom))))
  (defun zoom--resize-shrink-dimension (size-hint-cons horizontal)
    "Function return as a replacement for `zoom--resize-one-dimension'
    The difference being that it can shrink the window as well.
    Useful when other windows need to be enlarged."
    (let* ((size-hint
            (if horizontal (car size-hint-cons) (cdr size-hint-cons)))
           (frame-size
            (if horizontal (frame-width) (frame-height)))
           (window-size
            (if (floatp size-hint)
                (if horizontal (window-total-width) (window-total-height))
              (if horizontal (window-body-width) (window-body-height))))
           (min-window-size
            (if (floatp size-hint) (round (* size-hint frame-size)) size-hint))
           (desired-delta (- min-window-size window-size))
           (delta (window-resizable nil desired-delta horizontal)))
      (window-resize nil delta horizontal))))

(provide 'core-window)
