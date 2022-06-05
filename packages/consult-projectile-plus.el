;;; consult-projectile-plus.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: May 15, 2022
;; Modified: May 15, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary: Extensions for consult-projectile.el

;; 

;;; Code:

(require 'consult-projectile)
(require 'embark)
(require 'projectile)

(defvar consult-projectile-switch-action nil)
(defvar embark-projectile-project-actions (define-keymap))

(defun consult-projectile-remove-this-project()
  (projectile-remove-current-project-from-known-projects))

(defun consult-projectile-find-file-manually()
  (call-interactively 'find-file))

(defvar consult-projectile-embark-actions
  '(("o" "default" "jump to project buffer or file" 'consult-projectile)
    ("f" "find-file" "jump to a project file" 'consult-projectile-find-file)
    ("v" "vc" "open project in vc-dir / magit / monky" 'projectile-vc)
    ("d" "find-directory" "jump to a project directory" 'consult-projectile-find-dir)
    ("D" "dired" "open project in dired" 'projectile-dired)
    ("b" "buffer" "jump to a project buffer" 'consult-projectile-switch-to-buffer)
    ("m" "manual-find-file" "find file manually from project root" 'consult-projectile-find-file-manually)
    ("a" "affe" "find file with affe" 'affe-find)
    ("s" "rg" "search project with rg" 'consult-ripgrep)
    ("k" "kill" "kill all project buffers" 'projectile-kill-buffers)
    ("K" "remove" "remove project from known projects" 'consult-projectile-remove-this-project)))

(defvar consult-projectile-embark-actions-prefix
  "consult-projectile-embark-action-")


(defun consult-projectile-plus-bind-actions()
  (mapc
   (lambda (el)
     (define-key embark-projectile-project-actions
                 (car el)
                 (intern (concat consult-projectile-embark-actions-prefix (nth 1 el)))))
   consult-projectile-embark-actions)
  (setq embark-projectile-project-actions
        (make-composed-keymap embark-projectile-project-actions
                              embark-general-map))
  (add-to-list 'embark-keymap-alist '(consult-projectile-project . embark-projectile-project-actions)))

(defun consult-projectile-create-actions-defuns(el)
  `(defun ,(intern
            (concat
             consult-projectile-embark-actions-prefix
             (nth 1 el)))
       (dir) ,(nth 2 el)
       (let ((projectile-switch-project-action ,(nth 3 el)))
         (projectile-switch-project-by-name dir))))

(defmacro consult-projectile-init-defuns()
  `(progn ,@(mapcar
             'consult-projectile-create-actions-defuns
             consult-projectile-embark-actions)))

(defun consult-projectile-plus-init()
  (consult-projectile-init-defuns)
  (consult-projectile-plus-bind-actions)
  (add-to-list 'embark-post-action-hooks
               '(consult-projectile-embark-action-remove embark--restart)))

(provide 'consult-projectile-plus)
;;; consult-projectile-plus.el ends here
