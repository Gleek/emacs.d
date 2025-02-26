(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :bind
  (:map markdown-mode-map
         ("C-z w" . copy-current-code-block))
  :init
  (defvar markdown-command)
  (setq markdown-enable-math t ; syntax highlighting for latex fragments
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-uppercase-checkbox t ; for compat with org-mode
        markdown-gfm-additional-languages '("sh")
        markdown-make-gfm-checkboxes-buttons t

        ;; This is set to `nil' by default, which causes a wrong-type-arg error
        ;; when you use `markdown-open'. These are more sensible defaults.
        markdown-open-command "open"

        ;; A sensible and simple default preamble for markdown exports that
        ;; takes after the github asthetic (plus highlightjs syntax coloring).
        markdown-content-type "application/xhtml+xml"
        markdown-css-paths
        '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
          "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content
        (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
                "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))
  (defun markdown-compile-marked (beg end output-buffer)
    "Compiles markdown with the marked program, if available.
Returns its exit code."
    ;; npm -g install marked
    (when (executable-find "marked")
      (apply #'call-process-region
             beg end "marked" nil output-buffer nil
             (when (eq major-mode 'gfm-mode)
               (list "--gfm" "--tables" "--breaks")))))

  ;; (setq markdown-command "/usr/bin/pandoc")
  (setq markdown-command #'markdown-compile-marked)
  :config
  ;; (sp-local-pair '(markdown-mode gfm-mode) "`" "`"
  ;;                :unless '(:add sp-point-before-word-p sp-point-before-same-p))
  ;; Define the copy-current-code-block function
  (defun copy-current-code-block ()
    "Copy the current Markdown code block to the kill ring.
A code block is defined by triple backticks ``` as the start and end."
    (interactive)
    (let (beg end)
      (save-excursion
        ;; Search backward for the start of a code block
        (if (re-search-backward "^```" nil t)
            (progn
              ;; Move to the end of the start code fence line
              (end-of-line)
              (setq beg (point))
              ;; Search forward for the end of the code block
              (if (re-search-forward "^```" nil t)
                  (progn
                    ;; Move to the beginning of the end code fence line
                    (beginning-of-line)
                    (setq end (point)))
                (message "End of code block not found.")))
          (message "Start of code block not found.")))
      (if (and beg end)
          (progn
            (kill-ring-save beg end)
            (message "Code block copied to clipboard."))
        (message "No complete code block found to copy.")))))

(use-package grip-mode)

(provide 'core-markdown)
