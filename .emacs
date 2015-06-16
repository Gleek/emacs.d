;;;Load these settings first so that they dont pop up
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(size-indication-mode t)
(set-frame-font "Consolas 13")
;; (autoload 'zap-up-to-char "misc"
;; "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)
(add-to-list 'load-path "~/.emacs.d/custom/")


;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "M-x") 'smex)
(autoload 'magit-status "magit")


(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(electric-indent-mode)
(delete-selection-mode)
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

(setq-default line-spacing 0)
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
    (forward-line)))
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)



(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

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
(setq inhibit-startup-screen t)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(load "cleanup-buffer")
(load "remove-header-grep")

(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))

;;;;;;;;;;;;;;;;;;;;;;;<PACKAGES>;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)

(smartparens-global-mode t)
(global-undo-tree-mode t)
(global-set-key [remap undo-tree-undo] 'undo)

(setq js2-basic-offset 2)
(load-theme 'monokai t)
(yas-global-mode 1)
;; (global-auto-complete-mode t)
;; (ac-set-trigger-key "C-o")
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(hlinum-activate)
(global-anzu-mode +1)
(global-set-key (kbd "M-z") 'zop-to-char)
(require 'move-text)
(move-text-default-bindings)
(require 'vlf-setup)
;;;;;;;;;;;;;;;;;;;;;;;</PACKAGES>;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'anzu-mode)
(diminish 'yas-minor-mode)
(diminish 'undo-tree-mode)
(setq is-mac (equal system-type 'darwin))





;;fullscreen when not mac
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

(add-hook 'after-init-hook (lambda ()
                             (global-flycheck-mode)
                             (setq-default frame-title-format '(buffer-file-name "%b"))
                             (global-company-mode)
                             (setq company-idle-delay 0)
                             ))

;;MAC peculiar keyboard
(when is-mac
  (setq mac-command-modifier 'meta
        mac-option-modifier 'control
        ns-function-modifier 'super))


;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

(desktop-save-mode 1)
(setq desktop-files-not-to-save "^$")
(defun my-setup-php ()
  ;; enable web mode
  (web-mode)
  ;; make these variables local
  ;; (make-local-variable 'web-mode-code-indent-offset)
  ;; (make-local-variable 'web-mode-markup-indent-offset)
  ;; (make-local-variable 'web-mode-css-indent-offset)

  ;; set indentation, can set different indentation level for different code type
  ;; (setq web-mode-code-indent-offset 4)
  ;; (setq web-mode-css-indent-offset 2)
  ;; (setq web-mode-markup-indent-offset 2)
  )



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
 '(linum-highlight-face ((t (:inherit default :background "#39382e" :foreground "#75715E"))))
 '(paren-face-match ((t (:background "#FFFFFF" :foreground "#272822" :inverse-video t :underline t :weight normal))) t)
 '(show-paren-match ((t (:background "#FFFFFF" :foreground "#272822" :inverse-video t :underline t :weight normal)))))




;;custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(fci-rule-color "#49483E")
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)

 )
