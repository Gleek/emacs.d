(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(beacon-color "#ec4780")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(evil-emacs-state-cursor (quote ("#E57373" bar)) t)
 '(evil-insert-state-cursor (quote ("#E57373" hbar)) t)
 '(evil-normal-state-cursor (quote ("#00629D" box)) t)
 '(evil-visual-state-cursor (quote ("#C5E1A5" box)) t)
 '(fci-rule-color "#073642")
 '(global-flycheck-mode t)
 '(global-font-lock-mode t)
 '(global-git-gutter-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (quote
    ("#64B5F6" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80")))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors (quote (("#ec4780" . 0) ("#424242" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(ispell-extra-args (quote ("--run-together")))
 '(line-number-mode nil)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files (quote ("~/Dropbox/org-files/Tasks.org")))
 '(org-capture-templates
   (quote
    (("w" "Web Site" entry
      (file "~/Dropbox/org-files/Bookmarks.org")
      "* %?\\n%c\\n%:initial"))))
 '(org-export-with-sub-superscripts (quote {}))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "/usr/bin/google-chrome %s")
     ("\\.pdf\\'" . default))))
 '(org-pomodoro-finished-sound "/home/umar/.emacs.d/resources/doorbell.wav")
 '(org-pomodoro-long-break-sound "/home/umar/.emacs.d/resources/doorbell.wav")
 '(org-pomodoro-short-break-sound "/home/umar/.emacs.d/resources/doorbell.wav")
 '(org-pomodoro-start-sound "/home/umar/.emacs.d/resources/doorbell.wav")
 '(package-selected-packages
   (quote
    (lsp-treemacs smart-jump reformatter string-inflection restart-emacs let-alist doom-themes doom-one-theme nord-theme deadgrep flymake-diagnostic-at-point-mode flymake flymake-diagnostic-at-point company-box dap-mode go-playground dumb-jump protobuf-mode omnibox easy-kill treemacs treemacs-projectile flyspell-lazy docker docker-compose-mode docker-tramp dockerfile-mode w3m indium geben atom-one-dark-theme ivy-xref elmacro temporary-persistent go-mode tern js2-mode lsp-mode undo-tree yasnippet company all-the-icons form-feed spacemacs-dark phi-search company-lsp idea-darkula-theme lsp-ui org-noter pdf-tools atomic-chrome edit-server git-gutter exwm yaml-mode projectile-rails rvm robe ibuffer-vc ibuffer-projectile org-jira htmlize howdoi xref-js2 browse-at-remote bind-chord key-chord web-beautify shell-toggle shell-pop lsp-go rg wgrep-ag counsel-projectile all-the-icons-ivy rjsx-mode wgrep company-tern company-restclient flycheck-pos-tip dashboard ansi-term gxref edbi ace-window eshell-git-prompt spaceline-all-the-icons go-guru pandoc-mode org-alert persistent-scratch zeal-at-point projectile-ripgrep vimrc-mode ripgrep counsel-gtags move-text csv-mode nlinum-relative org-pomodoro php-mode zop-to-char zenburn-theme writeroom-mode which-key web-mode vlf use-package sql-indent spacemacs-theme spaceline spacegray-theme solarized-theme smooth-scroll smartparens skewer-mode rhtml-mode restclient request-deferred rainbow-mode projectile phpcbf php-boris-minor-mode php+-mode paradox ov org-bullets org nlinum neotree multiple-cursors multi-term multi-eshell markdown-mode magit less-css-mode jump jsx-mode json-rpc json-mode js-doc inf-ruby indent-guide imenu-anywhere hl-line+ helm-gtags helm-ag golden-ratio god-mode git-timemachine ggtags fringe-helper fold-this flycheck fancy-battery expand-region exec-path-from-shell evil esqlite emmet-mode ecb drag-stuff counsel company-web company-quickhelp company-math company-jedi company-go avy apache-mode anzu alert ag)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(pos-tip-background-color "#3a3a3a")
 '(pos-tip-foreground-color "#9E9E9E")
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tabbar-background-color "#353535")
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c8805d801780")
     (60 . "#bec073400bc0")
     (80 . "#b58900")
     (100 . "#a5008e550000")
     (120 . "#9d0091000000")
     (140 . "#950093aa0000")
     (160 . "#8d0096550000")
     (180 . "#859900")
     (200 . "#66aa9baa32aa")
     (220 . "#57809d004c00")
     (240 . "#48559e556555")
     (260 . "#392a9faa7eaa")
     (280 . "#2aa198")
     (300 . "#28669833af33")
     (320 . "#279993ccbacc")
     (340 . "#26cc8f66c666")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "#586e75" :foreground "light gray")))))
