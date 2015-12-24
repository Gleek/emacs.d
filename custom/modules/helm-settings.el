(require 'helm-config)
;; (helm-mode 1)
(helm-adaptive-mode t)
(set-face-attribute 'helm-source-header nil :height 1)
(setq helm-display-header-line nil)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30
      helm-autoresize-min-height 30
      helm-split-window-in-side-p t
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(diminish 'helm-mode)

(custom-set-variables
 '(helm-ag-base-command "sift --no-color --binary-skip --err-skip-line-length -n"))
