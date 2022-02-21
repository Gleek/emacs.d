;;; speak-region.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: February 19, 2022
;; Modified: February 19, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; 

;;; Code:
(require 'bongo)

(defvar bongo-speak-buffer "*bongo-speak-region*")

(defvar speak-tmp-file-name "speak-region.wav")
(defvar speak-speed "280")

(defvar speak-region-debug nil)

(defun speak-region-pdf()
  (interactive)
  (require 'pdf-tools)
  (speak-region (mapconcat 'identity (pdf-view-active-region-text) "\n")))

(defun speak-region-buf()
  (interactive)
  (speak-region (buffer-substring-no-properties (region-beginning) (region-end))))

(defun speak-region--clean(str)
  "Clean the STR to make it readable."
  (let* ((hyphenated-join (replace-regexp-in-string "\\(.\\)[-­]\n" "\\1" str))
         (joined-lines (replace-regexp-in-string "\\(.+\\)\n" "\\1 " hyphenated-join))
         (padded-lines (replace-regexp-in-string "\n+" "\n\n" joined-lines))
         (str (shell-quote-argument padded-lines)))
    str))

(defun speak-region(buf-str)
  "Cleans and plays the string using say and mpv.
`BUF-STR' is the string to read."
  (let* ((str (speak-region--clean buf-str))
         (file-name (expand-file-name speak-tmp-file-name (temporary-file-directory)))
         (command (format "say -r %s -o %s --data-format=LEF32@22050 %s && mpv %s\n"
                          speak-speed
                          file-name
                          str
                          file-name)))
    (if speak-region-debug (message "Running: %s" command))
    (start-process-shell-command "say" nil
                                 command)))

(defun speak-region-bongo(buf-str)
  (let* ((str (speak-region--clean buf-str))
         (file-name (expand-file-name speak-tmp-file-name (temporary-file-directory)))
         (command (format "say -r %s -o %s --data-format=LEF32@22050 %s"
                          speak-speed
                          file-name
                          str))
         (shell-out (shell-command-to-string command)))
    (if speak-region-debug (message "Running: %s" command))
    (if speak-region-debug (message "Output: %s" shell-out))
    (speak-region-push-to-bongo file-name)))


(dolist (buffer (buffer-list))
  (when (and (bongo-playlist-buffer-p buffer)
             (not (eq buffer (current-buffer))))
    buffer))
;; TODO: use bongo instead of directly using mpv
;; Currently works but has two problems:
;; - say runs synchronously
(defun speak-region-push-to-bongo(file)
  "FILE to play."
  (let ((buffer (get-buffer-create bongo-speak-buffer)))
    (with-current-buffer buffer
      (when (not (bongo-playlist-buffer-p))
        (bongo-playlist-mode))
      (bongo-erase-buffer)
      (bongo-insert-file file)
      (bongo-play-line (point-min)))))

(provide 'speak-region)
;;; speak-region.el ends here
