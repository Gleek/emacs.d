(defun +browse-url(url &optional use-webkit)
  (if (and use-webkit
           (featurep 'xwidget-internal))
      (progn
        (xwidget-webkit-browse-url url)
        (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
          (when (buffer-live-p buf)
            (and (eq buf (current-buffer)) (quit-window))
            (pop-to-buffer buf))))
    (browse-url url)))
;; Search provider
(defvar search-providers
  '((ducky         . ("Ducky"         "http://www.duckduckgo.com/?&q=!ducky %s"))
    (google        . ("Google"        "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"))
    (duck          . ("DuckDuckGo"    "http://www.duckduckgo.com/?&q=%s"))
    (devdocs       . ("devdocs.io"    "https://devdocs.io/#q=%s"))
    (lucky         . ("Google lucky"  "http://www.google.com/search?btnI&q=%s"))
    (stackoverflow . ("StackOverflow" "https://stackoverflow.com/search?q=%s"))))

(defun search-using-provider(provider &optional use-webkit)
  (+browse-url
    (let ((provider-data (alist-get provider search-providers)))
      (format (car (cdr provider-data))
              (url-hexify-string
               (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (read-string (format "Search using %s: " (car provider-data))
                              (thing-at-point 'symbol t)))))) use-webkit))

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (search-using-provider 'google))
(defun lucky ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (search-using-provider 'lucky))

(defun duck ()
  "Duck Duck Go the selected region if any, display a query prompt otherwise."
  (interactive)
  (search-using-provider 'duck t))
(defun ducky ()
  "Duck Duck Go the selected region if any, display a query prompt otherwise."
  (interactive)
  (search-using-provider 'ducky))
(defun devdocs()
  (interactive)
  (search-using-provider 'devdocs t))
(defun stackoverflow()
  (interactive)
  (search-using-provider 'stackoverflow))
