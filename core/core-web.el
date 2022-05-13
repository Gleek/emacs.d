;;; core-web.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: May 13, 2022
;; Modified: May 13, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary: Packages for interacting with the web

;; 

;;; Code:


(use-package w3m
  :init (setq w3m-search-default-engine "duckduckgo"))

(use-package eww :ensure nil
  :init (eval-after-load "org-protocol" `(add-to-list 'org-protocol-protocol-alist
                                                      '("eww"
                                                        :protocol "eww"
                                                        :function +start-eww-for-url)))
  (setq shr-use-xwidgets-for-media t)
  :bind
  (:map eww-mode-map
        ("%" . +eww-browse-with-xwidget))
  :config
  (setq eww-bookmarks-directory (expand-file-name "eww/" PRIV-DIR))
  (ensure-dir eww-bookmarks-directory)
  ;; Courtesy: protesilaos
  (defun +eww--rename-buffer ()
    "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
    (let ((name (if (eq "" (plist-get eww-data :title))
                    (plist-get eww-data :url)
                  (plist-get eww-data :title))))
      (rename-buffer (format "*%s # eww*" name) t)))

  (add-hook 'eww-after-render-hook #'+eww--rename-buffer)
  (advice-add 'eww-back-url :after #'+eww--rename-buffer)
  (advice-add 'eww-forward-url :after #'+eww--rename-buffer)

  (defun +eww-browse-with-xwidget ()
    "Browse the current URL with xwidget browse url."
    (interactive)
    (let ((browse-url-secondary-browser-function 'xwidget-webkit-browse-url))
      (call-interactively 'eww-browse-with-external-browser)))

  (defun +start-eww-for-url (plist)
    "Raise Emacs and call eww with the url in PLIST.
    Looks for eurl in the PLIST which is expected to be base64 encoded url, when url is not found."
    (raise-frame)
    (let ((url (plist-get plist :url))
          (eurl (plist-get plist :eurl)))
      (unless url
        (message "Decoding %s" eurl)
        (setq url (base64-decode-string eurl)))
      (message "Opening %s" url)
      (eww url))
    nil))


(use-package "web-search" :ensure nil
  :commands (+browse-url)
  :bind (("C-c s w" . duck)
         ("C-c s l" . lucky)
         ("C-c s d" . devdocs)))


(defalias 'xwwb 'xwidget-webkit-browse-url)
(use-package xwwp-follow-link-ivy
  :after xwidget
  :bind (:map xwidget-webkit-mode-map ("F" . xwwp-follow-link)))

(use-package ivy-youtube
  :bind
  ("C-c s y" . ivy-youtube)
  :config
  (setq ivy-youtube-history-file (expand-file-name "ivy-youtube-history" CACHE-DIR))
  (setq ivy-youtube-play-at "mpv")

  ;; Overriding the play function so that
  (defun ivy-youtube-play-on-process-a(video-url)
    (require 'dtache)
    (dtache-start-session (concat ivy-youtube-play-at " " (shell-quote-argument video-url)) t)
    ;; (make-process :name "Ivy Youtube"
    ;;               :command `(,ivy-youtube-play-at ,video-url))
    )
  (advice-add 'ivy-youtube-play-on-process :override 'ivy-youtube-play-on-process-a))

(use-package ace-link
  :bind (:map eww-mode-map
              ("o" . ace-link-eww)))




(provide 'core-web)
;;; core-web.el ends here
