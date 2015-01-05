(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-default-font "Consolas 13")
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)


;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")

(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(electric-pair-mode t)
(electric-indent-mode)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t

      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))


(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave/")
(setq ring-bell-function 'ignore)
(global-auto-revert-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))) 

(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format " %%%dd " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)
(global-linum-mode 1)
(setq-default cursor-type '(bar . 1))
(blink-cursor-mode 0)

(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; toggel shell escape using C-c C-t C-x
(defun TeX-toggle-escape nil (interactive)
"Toggle Shell Escape"
(setq LaTeX-command
  (if (string= LaTeX-command "latex") "latex -shell-escape"
    "latex"))
(message (concat "shell escape "
         (if (string= LaTeX-command "latex -shell-escape")
         "enabled"
           "disabled"))
     ))
(add-hook 'LaTeX-mode-hook
      (lambda nil
    (local-set-key (kbd "C-c C-t x") 'TeX-toggle-escape)))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)


(setq inhibit-startup-screen t)
(add-hook 'after-init-hook (lambda () 
                             (load-theme 'zenburn t)
                             (global-auto-complete-mode t)
                             (require 'flx-ido)
                             (ido-mode 1)
                             (ido-everywhere 1)
                             (flx-ido-mode 1)
                             ;; disable ido faces to see flx highlights.
                             (setq ido-enable-flex-matching t)
                             (setq ido-use-faces nil)
                             (hlinum-activate)
                             (setq-default frame-title-format '(buffer-file-name "%b"))
                             ))

(setq is-mac (equal system-type 'darwin))

;;MAC peculiar keyboard
(when is-mac
(setq mac-command-modifier 'meta
      mac-option-modifier 'control))


(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t) 

(desktop-save-mode 1)
(defun my-setup-php ()
  ;; enable web mode
  (web-mode)

  ;; make these variables local
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)

  ;; set indentation, can set different indentation level for different code type
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(add-to-list 'auto-mode-alist '("\\.php$" . my-setup-php))


(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum-highlight-face ((t (:inherit default :background "dim gray" :foreground "dark gray"))))
 '(paren-face-match ((t (:background "#FFFFFF" :foreground "#272822" :inverse-video t :underline t :weight normal))) t))




;;custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes (quote ("9c7c1aa124c1ea23662e94d8bd69defc2c3115b0ec73983625a0ae4f3a762a6c" "6ccbd72e43b217398d9aafa7525992145430f7c0a1e586164bae0e114585adb5" "bf5d965e878026e405b1d8a76d5bf48614999cfac3a9a0b833e98b6e07d1e77a" "3a2fa6489367c5153be25348aec54756b94dedc6e275f0be267ad8f6b0317f2b" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "d04ca2c551ca3b4e05df9e28f9eada505c12803b8fff83c4e031985bcd81c790" default)))
 '(fci-rule-color "#49483E")
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors (("#49483E" . 0) ("#67930F" . 20) ("#349B8D" . 30) ("#21889B" . 50) ("#968B26" . 60) ("#A45E0A" . 70) ("#A41F99" . 85) ("#49483E" . 100)))
 '(magit-diff-use-overlays nil)
 '(safe-local-variable-values (quote ((eval when (fboundp (quote aggressive-indent-mode)) (aggressive-indent-mode -1)) (eval when (fboundp (quote rainbow-mode)) (rainbow-mode 1)) (eval when (require (quote rainbow-mode) nil t) (rainbow-mode 1)))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#F92672") (40 . "#CF4F1F") (60 . "#C26C0F") (80 . "#E6DB74") (100 . "#AB8C00") (120 . "#A18F00") (140 . "#989200") (160 . "#8E9500") (180 . "#A6E22E") (200 . "#729A1E") (220 . "#609C3C") (240 . "#4E9D5B") (260 . "#3C9F79") (280 . "#A1EFE4") (300 . "#299BA6") (320 . "#2896B5") (340 . "#2790C3") (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))


(if (not is-mac)
    
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
