(defun beginning-of-line-or-indentation ()
  "Move to beginning of line, or indentation."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))


(use-package emacs :ensure nil
  :bind (("C-a" . beginning-of-line-or-indentation)
         ("s-b" . previous-buffer)
         ("s-f" . next-buffer)
         ("s-<tab>" . mode-line-other-buffer)))


(use-package avy
  :ensure t
  :init
  (setq avy-background t)
  ;; :chords ("jk" . avy-goto-word-or-subword-1)
  :bind* (("C-\"". avy-goto-word-or-subword-1)
          ("C-'" . avy-goto-char-timer)))


(use-package isearch
  :ensure nil
  ;; :chords ("df" . isearch-forward)
  :bind (("C-r"   . isearch-backward)
         ("C-s" . isearch-forward)
         ("C-M-r" . isearch-backward-regexp)
         (:map isearch-mode-map
               ("M-r" . consult-isearch-history)
               ("C-M-s" . consult-line)
               ("C-o" . consult-line)))
  :config
  ;; Make it a bit more swiper like. Since I'm used to it now
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited))

(use-package swiper
  :bind (;; ("C-M-s" . swiper)
         :map swiper-map
         ("C-c m" . swiper-mc)))

;; (use-package ace-isearch)

(use-package affe
  :bind* ("s-o" . affe-find-no-ignore)
  :commands (affe-find affe-find-no-ignore)
  :config
  (defun affe-find-no-ignore()
    (interactive)
    (let ((affe-find-command "rg --color=never --files --no-ignore-vcs"))
      (affe-find))))

(when IS-MAC
  (use-package spotlight
    :bind ("C-c s S" . spotlight)))

(use-package phi-search :ensure t
  :init (setq phi-search-limit 10000))

(use-package anzu
  :ensure t
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package point-stack
  :ensure nil
  :defer 2
  :bind (("s-[" . point-stack-pop)
         ("s-]" . point-stack-forward-stack-pop)
         ("s-SPC" . consult-point-stack))
  :config
  (setf point-stack-advised-functions
        (cl-list*
         'consult-line
         'consult-ripgrep
         'xref-find-references
         'lsp-find-implementation
         'lsp-find-definition-mouse
         'treesit-beginning-of-defun
         'treesit-end-of-defun
         'php-beginning-of-defun
         'php-end-of-defun
         'beginning-of-defun
         'end-of-defun
         'backward-up-list
         'forward-list
         'bookmark-jump
         point-stack-advised-functions))
  (point-stack-setup-advices))


(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-file (concat CACHE-DIR "bookmarks")))

(use-package scroll-other-window
  :disabled t
  :ensure nil
  :load-path "packages/scroll-other-window.el"
  :bind (("C-M-v" . sow-scroll-other-window)
         ("C-S-M-v". sow-scroll-other-window-down)))

(use-package combobulate
  :ensure (:fetcher github :repo "mickeynp/combobulate"))

(use-package hideshow
  :ensure hydra
  :hook (prog-mode . enable-hs-minor-mode-maybe)
  :bind (:map hs-minor-mode-map
              ("C-{" . hydra-hs-folding/body)
              ("<S-mouse-1>" . +mouse-hs-toggle))
  :config
  (defun enable-hs-minor-mode-maybe()
    (if (not (and (treesit-available-p)
                  (treesit-parser-list)))
        (hs-minor-mode t)))
  (defhydra hydra-hs-folding (:color red)
    "
  _o_pen node    _c_lose node  _t_oggle fold
  close _l_evel  _s_how all    _h_ide all
  "
    ("o" hs-show-block)
    ("c" hs-hide-block)
    ("t" hs-toggle-hiding)
    ("l" hs-hide-level)
    ("s" hs-show-all)
    ("h" hs-hide-all)
    ("<tab>" hs-toggle-hiding))
  (defun +mouse-hs-toggle(e)
    (interactive "e")
    (mouse-set-point e)
    (hs-toggle-hiding e)))

(use-package treesit-fold
  :ensure (:fetcher github :repo "emacs-tree-sitter/treesit-fold")
  :hook ((prog-mode text-mode conf-mode) . enable-treesit-fold-maybe)
  :bind (:map treesit-fold-mode-map
              ("C-{" . hydra-treesit-folding/body)
              ("<S-mouse-1>" . +mouse-treesit-toggle))
  :config
  (defun enable-treesit-fold-maybe()
    (if (and (treesit-available-p)
             (treesit-parser-list))
        (treesit-fold-mode t)))

  (defhydra hydra-treesit-folding (:color red)
    "
  _o_pen node    _c_lose node  _t_oggle fold
  open _r_ecursively  _s_how all    _h_ide all
  "
    ("o" treesit-fold-open)
    ("c" treesit-fold-close)
    ("t" treesit-fold-toggle)
    ("r" treesit-fold-open-recursively)
    ("s" treesit-fold-open-all)
    ("h" treesit-fold-close-all)
    ("<tab>" hs-toggle-hiding))

  (defun +mouse-treesit-toggle(e)
    (interactive "e")
    (mouse-set-point e)
    (treesit-fold-toggle)))

(use-package origami
  :disabled t
  :after hydra
  ;; :hook (prog-mode . origami-mode)
  :bind (:map origami-mode-map
              ("C-{" . hydra-folding/body)
              ("<S-mouse-1>" . +mouse-origami-toggle))
  :config
  (defhydra hydra-folding (:color red)
    "
  _o_pen node    _n_ext fold       toggle _f_orward  _s_how current only
  _c_lose node   _p_revious fold   toggle _a_ll      undo _/_
  redo _?_      _R_eset
  "
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("a" origami-toggle-all-nodes)
    ("s" origami-show-only-node)
    ("<tab>" origami-recursively-toggle-node)
    ("/" origami-undo)
    ("?" origami-redo)
    ("R" origami-reset))

  (defun +mouse-origami-toggle(e)
    (interactive "e")
    (mouse-set-point e)
    (call-interactively 'origami-toggle-node)))

(use-package imenu-anywhere :ensure t)



(provide 'core-navigation)
