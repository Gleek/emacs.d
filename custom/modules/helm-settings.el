;; (require 'helm-config)
;; (helm-mode 1)


(use-package helm
  :init
  (setq helm-autoresize-max-height 30
        helm-autoresize-min-height 30
        helm-split-window-in-side-p t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-display-header-line nil)
  :config
  (helm-autoresize-mode 1)
  (set-face-attribute 'helm-source-header nil :height 1)
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-c g" . helm-google-suggest)
         ("M-y" . helm-show-kill-ring)))

(use-package helm-adaptive
  :config
  (helm-adaptive-mode t))


(use-package helm-all-mark-rings
  :config
  (helm-attrset 'follow 1 helm-source-mark-ring)
  :bind ("C-h C-SPC" . helm-all-mark-rings))


(use-package helm-gtags
  :config (helm-gtags-mode)
  :bind (("M-," . helm-gtags-find-rtag)
         ("M-*" . helm-gtags-pop-stack)
         ("C-M-." . helm-gtags-find-tag)
         ("M-." . helm-gtags-find-tag-from-here))
  :diminish helm-gtags-mode)

(use-package helm-ag
  :config
  (setq helm-ag-base-command "sift --no-color --binary-skip --err-skip-line-length -n")
  :bind ("C-c a" . helm-do-ag))


(use-package helm-regexp
  :config
  (setq helm-source-moccur
         (helm-make-source "Moccur"
             'helm-source-multi-occur :follow 1)))


;; (helm-adaptive-mode t)
;; (set-face-attribute 'helm-source-header nil :height 1)
;; (setq helm-display-header-line nil)
;; (helm-autoresize-mode 1)
;; (setq helm-autoresize-max-height 30
;;       helm-autoresize-min-height 30
;;       helm-split-window-in-side-p t
;;       helm-M-x-fuzzy-match t
;;       helm-buffers-fuzzy-matching t
;;       helm-recentf-fuzzy-match t)

;; (helm-all-mark-rings)
;; (helm-attrset 'follow 1 helm-source-mark-ring)
;; (helm-attrset 'follow 0 helm-source-global-mark-ring)

;; (diminish 'helm-mode)

;; (custom-set-variables
;;  '(helm-ag-base-command "sift --no-color --binary-skip --err-skip-line-length -n"))
