(defun +pdf-suppress-large-files-prompt-a(fn size op-type filename &optional offer-raw)
  (unless (string-match-p "\\.pdf\\'" filename)
    (funcall fn size op-type filename offer-raw)))
(advice-add 'abort-if-file-too-large :around #'+pdf-suppress-large-files-prompt-a)

(use-package speak-region
  :ensure nil
  :commands (speak-region-buf speak-region-pdf))


(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("C-c C-a H" . pdf-annot-choose-highlight-color)
              ("D" . pdf-annot-delete)
              ("R" . speak-region-pdf)
              ("m" . pdf-view-midnight-minor-mode)
              ("t" . pdf-annot-add-text-annotation))
  :config
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

  (defun +pdf-reload-midnight-minor-mode-h ()
    (when pdf-view-midnight-minor-mode
      (pdf-info-setoptions
       :render/foreground (car pdf-view-midnight-colors)
       :render/background (cdr pdf-view-midnight-colors)
       :render/usecolors t)
      (pdf-cache-clear-images)
      (pdf-view-redisplay t)))
  (defun +pdf-reload-all-buff()
    (mapc (lambda(buf)
            (with-current-buffer buf
              (if (eq (buffer-local-value 'major-mode buf) 'pdf-view-mode)
                  (+pdf-reload-midnight-minor-mode-h))))
          (buffer-list))
    t)
  (add-hook '+theme-toggle-hook #'+pdf-reload-all-buff)

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
                           (save-place-mode t))))


(use-package nov
  :ensure nov
  :ensure dash
  :bind (:map nov-mode-map
              ("R" . speak-region-buf)
              ("s-u" . reload-nov)
              ("D" . osx-dictionary-search-input)
              ("s" . nov-search-forward)
              ("b" . nov-search-backward)
              ([remap nov-history-back] . nov-history-back-pres-pos))
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat CACHE-DIR "nov-places"))

  (defun reload-nov()
    "Revert buffer to work with epub files. "
    (interactive)
    (find-alternate-file nov-file-name))

  ;; Courtesy: freesteph
  ;; TODO: rewrite this to work like isearch.
  (defun nov--search-direction (query direction)
    "Search document for QUERY in the specified DIRECTION.  DIRECTION should be either :forward or :backward."
    (cl-multiple-value-setq (search-fn next-page-fn buffer-edge-fn)
      (or (and (eq direction :forward)
               '(search-forward
                 nov-next-document
                 beginning-of-buffer))
          '(search-backward
            nov-previous-document
            end-of-buffer)))
    (let* ((original-index nov-documents-index)
                   (original-point (point))
                   (found (funcall search-fn query nil t)))
      (while (and (not found)
                  (funcall next-page-fn))
        (funcall buffer-edge-fn)
        (setf found (funcall search-fn query nil t)))
      (unless found
        (setf nov-documents-index original-index)
        (nov-render-document)
        (goto-char original-point)
        (message "No match found for %s." query))))

  (defun nov-search-backward (query)
    "Search the document backward for QUERY."
    (interactive "sSearch query backward: ")
    (nov--search-direction query :backward))

  (defun nov-search-forward (query)
    "Search the document forward for QUERY."
    (interactive "sSearch query: ")
    (nov--search-direction query :forward))




  (defun nov-history-back-pres-pos()
    "Differs from the normal nov-history-back by not recentering the
     position when we go back thereby not disorienting the
     previous view.  This preserves the visual cue when jumping
     to references and quickly jumping back."
    (interactive)
    (or nov-history
        (user-error "This is the first document you looked at"))
    (let ((history-forward (cons (list nov-documents-index (point))
                                 nov-history-forward)))
      (seq-let (index opoint) (car nov-history)
        (setq nov-history (cdr nov-history))
        (nov-goto-document index)
        (setq nov-history (cdr nov-history))
        (setq nov-history-forward history-forward)
        (goto-char opoint))))

  (defvar nov-progress-hook nil)
  (defvar-local nov-current-doc-sizes '()
    "Maintains the output of `nov-doc-sizes'. To be refreshed on file load")

  (defun nov-load-doc-sizes()
    (setq-local nov-current-doc-sizes (nov-doc-sizes)))
  (defun nov-doc-sizes()
    "Return an alist of size in bytes for all the nov-documents in same order."
    (mapcar (lambda (f)
              (file-attribute-size (file-attributes (cdr f))))
            nov-documents))
  (defun nov-calculate-percentage-finished()
    "Estimate the current percentage of finished book on the basis of
     html source file and current cursor position."
    (let (current-buffer-pos current-doc-size prev-doc-sizes all-sizes total-docs-size)
      (setq current-buffer-pos (/ (float (line-number-at-pos)) (line-number-at-pos (point-max))))
      (setq all-sizes nov-current-doc-sizes)
      (setq current-doc-size (nth nov-documents-index all-sizes))
      (setq prev-doc-sizes (apply '+ (butlast all-sizes (- (length all-sizes) nov-documents-index))))
      (setq total-docs-size (apply '+ all-sizes))
      (fround (* 100 (/ (+ (* current-buffer-pos current-doc-size) prev-doc-sizes) (float total-docs-size))))))

  (setq-default nov--last-percentage-complete 0)
  (defun nov-percentage-show-on-change()
    (let ((percentage-finished (nov-calculate-percentage-finished)))
      (when (not (equal nov--last-percentage-complete percentage-finished))
        (run-hooks 'nov-progress-hook)
        (setq-local nov--last-percentage-complete percentage-finished))))

  (defun init-nov-progress-hook()
    (add-hook 'post-command-hook 'nov-percentage-show-on-change nil t))

  (defun nov-setup-doom-modeline()
    (doom-modeline-def-segment nov-progress
      "Display progress in EPUB documents."
      (propertize (concat
                   (format " %0.0f%%%% " nov--last-percentage-complete)
                   (if doom-modeline-percent-position
                       (format "(%s ) " (format-mode-line '(" " doom-modeline-percent-position "%%")))
                     " "))
                  'face (doom-modeline-face 'doom-modeline)))
    (doom-modeline-def-modeline 'nov
      '(bar workspace-name window-number modals matches follow buffer-info remote-host nov-progress word-count parrot selection-info)
      '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))

    (add-hook 'nov-mode-hook
              (lambda() (doom-modeline-set-modeline 'nov))))

  (eval-after-load 'doom-modeline
    '(nov-setup-doom-modeline))

  (add-hook 'nov-mode-hook 'reading-mode)
  (add-hook 'nov-mode-hook 'nov-load-doc-sizes)
  (add-hook 'nov-mode-hook 'init-nov-progress-hook))



(use-package djvu)

(use-package calibredb
  :commands (+calibredb-add)
  :bind ("C-c B" . calibredb-consult-read)
  :config
  (defun +calibredb-add()
    (interactive)
    (calibredb-add nil)
    (setq calibredb-search-entries (calibredb-candidates))
    (setq calibredb-full-entries calibredb-search-entries))
  (setq calibredb-root-dir "~/Documents/Calibre")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)))

(use-package spray
  :after nov
  :bind (:map nov-mode-map
              ("S" . spray-mode))
  :config
  (setq spray-wpm 400
        spray-height 700)
  (setq spray-margin-left 7
        spray-margin-top 2))

(provide 'core-ebook)
