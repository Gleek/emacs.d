;;; bootstrap.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: May 17, 2022
;; Modified: May 17, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;;

;;; Code:

(setq debug-on-error t)
(defconst CACHE-DIR (expand-file-name "cache/" user-emacs-directory))
(defconst PRIV-DIR (expand-file-name "private/" user-emacs-directory))
(defconst RES-DIR   (expand-file-name "resources/" user-emacs-directory))
(defconst IS-MAC    (eq system-type 'darwin))
(defconst IS-LINUX  (eq system-type 'gnu/linux))
(defconst IS-TERM   (not (display-graphic-p)))

(setq gc-cons-threshold 8000000) ;; collect garbage after about 100 MB
;; (run-with-idle-timer 2 t (lambda () (garbage-collect)))
(setq message-log-max 10000) ;; Debugging
;; (let ((package-file (expand-file-name "package-quickstart.el" user-emacs-directory)))
;;   (if (file-exists-p package-file)
;;       (load package-file)
;;     (package-quickstart-refresh)))

(eval-after-load 'package
  '(add-to-list 'package-archives
                '("melpa" . "https://melpa.org/packages/") t))


(setq package-native-compile t)
(setq native-comp-async-report-warnings-errors 'silent)

(unless IS-TERM
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-i] [C-i]))
;; Reserve C-z as a prefix for mode level keymaps
(global-unset-key (kbd "C-z"))


;; Bootstrap `use-package'
(if (version< emacs-version "30.0")
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package)))
(setq use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
;; (use-package diminish)
;; (use-package use-package-chords
;;   :demand
;;   :config (key-chord-mode 1))

;; TODO: remove once upgrade to Emacs 30 happens
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; (use-package use-package-ensure-system-package)
(use-package benchmark-init
  :disabled
  ;; :demand
  :hook (after-init . benchmark-init/activate)
  :config
  ;; To disable collection of benchmark data after init is done.
  ;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
  )

(setq custom-file (concat CACHE-DIR "custom.el"))
;; (load custom-file)
(add-to-list 'load-path (expand-file-name "packages/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))

(use-package gcmh
  ;; We need to init this post startup as we already set the GC to infinity in early-init.
  :hook (emacs-startup . gcmh-mode)
  :config
  (gcmh-mode t))

(provide 'bootstrap)
;;; bootstrap.el ends here
