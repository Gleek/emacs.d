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

(use-package hideshow
  :defer 1
  :bind (:map hs-minor-mode-map
              ("<S-mouse-1>" . move-mouse-and-toggle-hide))
  :config
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (defun move-mouse-and-toggle-hide(e)
    (interactive "e")
    (mouse-set-point e)
    (hs-toggle-hiding e)))
(use-package imenu-anywhere :ensure t)

(provide 'core-navigation)
