(use-package pdf-tools
  :after popup
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :defer 1
  :init
  (defvar pdf-annot-highlight-colors '(("blue" . "#40e0d0") ("yellow" . "#face50") ("red" . "#ff69b4")))
  (defun pdf-annot-choose-highlight-color()
    (interactive)
    (pdf-annot-update-highilight-color
     (cdr (assoc (completing-read "Choose highlight color: " pdf-annot-highlight-colors nil t) pdf-annot-highlight-colors))))
  (defun pdf-annot-update-highilight-color (color)
    (setq pdf-annot-default-annotation-properties
          (let* ((temp-props)
                 (acolor (cons 'color color))
                 (highlight-prop '()))
            (push acolor highlight-prop)
            (push 'highlight highlight-prop)
            (dolist (element pdf-annot-default-annotation-properties)
              (when (eq (eq (car element) 'highlight) nil)
                (push element temp-props)))
            (push highlight-prop  temp-props)
            temp-props)))

  (defun pdf-cleanup-windows-h ()
    "Kill left-over annotation buffers when the document is killed."
    (when (buffer-live-p pdf-annot-list-document-buffer)
      (pdf-info-close pdf-annot-list-document-buffer))
    (when (buffer-live-p pdf-annot-list-buffer)
      (kill-buffer pdf-annot-list-buffer))
    (let ((contents-buffer (get-buffer "*Contents*")))
      (when (and contents-buffer (buffer-live-p contents-buffer))
        (kill-buffer contents-buffer))))

  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("C-c C-a H" . pdf-annot-choose-highlight-color)
              ("D" . pdf-annot-delete)
              ("m" . pdf-view-midnight-minor-mode)
              ("t" . pdf-annot-add-text-annotation))

  :config
  ;; (remove-hook 'pdf-view-mode-hook
  ;;           (add-hook 'kill-buffer-hook #'pdf-cleanup-windows-h nil t))
  ;; (pdf-view-midnight-colors '("#839496" . "#002b36"))
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (pdf-annot-update-highilight-color "#face50")
              ))
  (set-popup-rules!
    '(("^\\*Outline*" :side right :size 40 :select nil)
      ("\\(?:^\\*Contents\\|'s annots\\*$\\)" :ignore t)))
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)
  (pdf-tools-install))

(use-package saveplace-pdf-view
  :hook (pdf-view-mode . (lambda ()
                           (require 'saveplace-pdf-view)
                           (save-place-mode t)
                           )))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat CACHE-DIR "nov-places")))
(provide 'core-ebook)
