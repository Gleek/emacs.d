;;; quicktype.el --- Json to programming type converter        -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/quicktype.el
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package is wrapper over https://app.quicktype.io for quickly json convering
;; to current mode, or paste json from buffer as a real type.

(require 'simple)
(require 'cl-lib)

;;; Code:
(defvar quicktype-binary "quicktype"
  "Shell command for quicktype running.")
(defvar quicktype-languages '("cs" "go" "rs" "cr" "c++" "objc" "java" "ts" "js" "javascript-prop-types" "flow" "swift" "kotlin" "elm" "schema" "ruby" "dart" "py" "pike" "haskell")
  "List of supported languages.")
(defvar quicktype-ask-top-level-name-p t
  "Should plugin ask about name of root element.")
(defvar quicktype-just-types-p t
  "Should generate only types.
If false it will generate functions for type converting.")
(defvar quicktype-extra-arguments ""
  "Extra CLI arguments.")
(defvar quicktype-async-buffer-name "*Async shell command*"
  "Async buffer name.")
(defvar quicktype-just-types-unsupported-langs '("js" "rs")
  "List of languages that can't use just-type arg.")

(defvar quicktype-mode-configs '(("go" go-mode "")
                                 ("ts" typescript-mode "")
                                 ("js" js2-mode "")
                                 ("rs" rust-mode "")
                                 ("c++" c++-mode "")
                                 ("javascript-prop-types" js2-mode "")
                                 ("flow" flow-js2-mode "")
                                 ("swift" swift-mode "")
                                 ("kotlin" kotlin-mode "")
                                 ("elm" elm-mode "")
                                 ("ruby" ruby-mode "")
                                 ("dart" dart-mode "")
                                 ("py" python-mode "--python-version 3.7")
                                 ("haskell" haskell-mode ""))
  "List of all supported modes.
First elem is real Emacs mode, the second one is additional CLI args.")

;;;###autoload
(defun quicktype-install-dependencies ()
  "Install quicktype npm package."
  (interactive)
  (let* ((version-cmd (format "%s --version" quicktype-binary))
         (quicktype-error (shell-command version-cmd)))
    (cond ((eq quicktype-error 0) (message "Quicktype already installed"))
          ((eq quicktype-error 127)
           (async-shell-command "npm i -g quicktype"))
          (t (message "Quicktype install erorr: %s" quicktype-error)))))

(defun quicktype--just-types-support-p (lang)
  "Check have current LANG just-types flag support."
  (not (member lang quicktype-just-types-unsupported-langs)))

(defun quicktype--build-convert-cmd (lang top-level-name)
  "Build cmd for quicktype by provided LANG from 'quicktype-languages' list.
Func will use selected region or entire buffer.
if TOP-LEVEL-NAME provided it will be used for root name of type."

  (let* ((selected-text (if (use-region-p)
                            (buffer-substring (region-beginning) (region-end))
                          (buffer-substring (point-min) (point-max)))))
    (concat (format "echo '%s' | %s -l %s" selected-text quicktype-binary lang)
            (if top-level-name (format " --top-level %s" top-level-name) "")
            (if (and quicktype-just-types-p (quicktype--just-types-support-p lang)) " --just-types" "")
            quicktype-extra-arguments)))

(defun quicktype--try-request-top-level-name ()
  "Request top level of struct name."
  (if quicktype-ask-top-level-name-p (read-string "Insert top level type's name: ") nil))

(defun quicktype--request-language ()
  "Request top level of struct name."
  (completing-read "Choose language: " quicktype-languages))

(defun quicktype--apply-mode-after-ready (process signal output-buffer-name mode)
  "Proxy PROCESS and SIGNAL.
OUTPUT-BUFFER-NAME buffer for swithing and applying mode after process is done
MODE - mode that should be applied if exist"
  (when (memq (process-status process) '(exit signal))
    (switch-to-buffer-other-window output-buffer-name)
    (if (and mode (fboundp mode))
        (funcall mode)))
    (shell-command-sentinel process signal))

;;;###autoload
(defun quicktype-json-to-type ()
  "Convert json buffer to type."
  (interactive)
  (let* ((lang (quicktype--request-language))
         (top-level-name (quicktype--try-request-top-level-name))
         (cmd (quicktype--build-convert-cmd lang top-level-name))
         (lang-config (assoc lang quicktype-mode-configs))
         (output-buffer-name (format "*%s Type*" lang))
         (mode (nth 1 lang-config))
         (proc (progn
                 (async-shell-command cmd output-buffer-name)
                 (get-buffer-process output-buffer-name))))

    (if (process-live-p proc)
        (set-process-sentinel proc #'(lambda (proc signal)
                                       (quicktype--apply-mode-after-ready proc signal output-buffer-name mode))))))

(defun quicktype--get-lang-by-mode ()
  "Return lang for quicktype by current active mode."
  (let* ((config (cl-rassoc major-mode quicktype-mode-configs :test #'member)))
    (nth 0 config)))


(defun quicktype--insert-async-result (process signal)
  "Insert result of async fuction to current place by provided PROCESS and SIGNAL."
  (let ((result (when (memq (process-status process) '(exit signal))
                  (save-window-excursion
                    (switch-to-buffer-other-window quicktype-async-buffer-name)
                    (buffer-substring (point-min) (point-max))))))
    (kill-matching-buffers quicktype-async-buffer-name nil t)
    (insert result)
    (shell-command-sentinel process signal)))


;;;###autoload
(defun quicktype-paste-json-as-type ()
  "Try to paste JSON from urrent buffer under current cursor position."
  (interactive)
  (let* ((assumed-lang (quicktype--get-lang-by-mode))
         (lang (if (eq assumed-lang nil)
                   (quicktype--request-language)
                 assumed-lang))
         (top-level-name (quicktype--try-request-top-level-name))
         (tmp-buffer-name " quicktype-buffer")
         (output-buffer (generate-new-buffer "*Async shell command*"))
         (proc (save-window-excursion
                 (switch-to-buffer-other-window tmp-buffer-name)
                 (erase-buffer)
                 (clipboard-yank)
                 (async-shell-command (quicktype--build-convert-cmd lang top-level-name) output-buffer)
                 (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel proc #'quicktype--insert-async-result))))

;;;###autoload
(defun quicktype ()
  "Convert current buffer to view type if current mode is json, or insert from buffer as type."
  (interactive)
  (if (eq major-mode 'json-mode)
      (quicktype-json-to-type)
    (quicktype-paste-json-as-type)))

(provide 'quicktype)
;;; quicktype.el ends here
