(defun beginning-of-line-or-indentation ()
  "Move to beginning of line, or indentation."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(use-package avy
  :ensure t
  :init
  (setq avy-background t)
  ;; :chords ("jk" . avy-goto-word-or-subword-1)
  :bind (("C-\"". avy-goto-word-or-subword-1)
         ("C-'" . avy-goto-char-timer)))

(use-package isearch
  :ensure nil
  ;; :chords ("df" . isearch-forward)
  :bind (("C-r"   . isearch-backward-regexp)
         ("C-s" . isearch-forward)
         ("C-M-r" . isearch-backward)
         (:map isearch-mode-map
               ("C-M-s" . swiper-from-isearch)
               ("C-o" . swiper-from-isearch)))
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
  :ensure t
  :bind (("C-M-s" . swiper)
         :map swiper-map
         ("C-c m" . swiper-mc)))

;; (use-package ace-isearch)

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

(use-package gumshoe
  :defer 2
  :bind (("s-[" . gumshoe-backtrack-back)
         ("s-]" . gumshoe-backtrack-forward))
  :config
  (global-gumshoe-mode t))

(use-package counsel-gumshoe :ensure nil
  :bind ("C-c C-SPC" . counsel-gumshoe-ring)
  :config
  (advice-add 'gumshoe--jump-to-marker :override #'gumshoe-open-mark))


(use-package hideshow
  :ensure nil
  :disabled t ;; Trying out origami
  ;; :defer 1
  ;; :bind (:map hs-minor-mode-map
  ;;             ("<S-mouse-1>" . move-mouse-and-toggle-hide))
  :config
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (defun move-mouse-and-toggle-hide(e)
    (interactive "e")
    (mouse-set-point e)
    (hs-toggle-hiding e)))


(use-package origami
  :after hydra
  :hook (prog-mode . origami-mode)
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
