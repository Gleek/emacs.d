;;; emoji-search.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Umar Ahmad
;; Created: December 29, 2024
;; Modified: December 29, 2024
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; Searches for emojis using a lot more keywords than just it's name (as in `emoji-search')

;;; Code:

(require 'json)

(defcustom emoji-search-download-dir user-emacs-directory
  "Directory to download the emoji list to."
  :type 'directory
  :group 'emoji-search)

(defvar emoji-search-json-url "https://raw.githubusercontent.com/xitanggg/emoogle-emoji-search-engine/main/data/emoogle-emoji-keywords.json"
  "URL to download the emoji list from.")

(defun load-emoji-list ()
  "Load the emoji list, downloading it if not present in cache."
  (let* ((cache-file (expand-file-name "emoji-list.json" emoji-search-download-dir)))
    (unless (file-exists-p cache-file)
      (url-copy-file emoji-search-json-url cache-file))

    (unless (boundp 'emoji-list)
      (setq emoji-list
            (with-temp-buffer
              (insert-file-contents cache-file)
              (json-parse-buffer :object-type 'alist))))))

(defun emoji-search-completing-read ()
  "Prompt the user to select an emoji using completing-read with hidden but searchable keywords."
  (interactive)
  (load-emoji-list)
  (let* ((completion-ignore-case t)
         (completion-candidates
          (mapcar (lambda (item)
                    (let* ((emoji (format "%s" (car item)))
                           (keywords (append (cdr item) nil))
                           (search-string (concat emoji " " (string-join
                                                             (mapcar #'emoji-search--format-string keywords)
                                                             " "))))
                      (cons search-string emoji)))
                  emoji-list))
         (completion-table
          (lambda (string pred action)
            (pcase action
              ('metadata
               `(metadata
                 (category . emoji)
                 (display-sort-function . ,(lambda (candidates)
                                             (mapcar (lambda (c)
                                                       (cdr (assoc c completion-candidates)))
                                                     candidates)))))
              ('lambda t)
              (_
               (let ((completions (all-completions string completion-candidates pred)))
                 completions)))))
         (selection (completing-read "Select emoji: " completion-table nil t)))
    (when selection
      (message "Selected emoji: %s" selection)
      (kill-new selection))))

(defun emoji-search--format-string (obj)
  "Convert OBJ to string safely."
  (format "%s" obj))

(provide 'emoji-search)
;;; emoji-search.el ends here
