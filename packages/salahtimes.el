;;; salahtimes.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Umar Ahmad
;; Created: May 14, 2022
;; Modified: May 14, 2022
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; Give tabulated data of the salah times

;;; Code:

(require 'json)
(require 'request)

(defgroup salahtimes nil
  "Group for salahtimes package."
  :group 'local
  :prefix "salahtimes")

(defcustom salah-city "Delhi"
  "The city name for getting the times"
  :group 'salahtimes
  :type 'string)
(defcustom salah-country "India"
  "The country name for salah times."
  :group 'salahtimes
  :type 'string)

(defcustom salah-times-buffer " *salah-times*"
  "The buffer to render the salahtimes in."
  :group 'salahtimes
  :type 'string)

(defcustom salah-school "hanafi"
  "The method hanafi or shafi"
  :group 'salahtimes
  :type '(string)
  :options '("hanafi" "shafi"))

(defcustom salah-prev-days 1
  "The number of previous days to show."
  :group 'salahtimes
  :type 'integer)
(defcustom salah-next-days 10
  "The number of next days to show."
  :group 'salahtimes
  :type 'integer)

(defun salah-times()
  (interactive)
  (require 'request)
  (request
    "http://api.aladhan.com/v1/calendarByCity"
    :params `(("city" . ,salah-city)
              ("country" . ,salah-country)
              ("method" . "1")
              ("month" . ,(format-time-string "%m"))
              ("year" . ,(format-time-string "%Y"))
              ("school" . ,(if (equal salah-school "hanafi") "1" "0")))
    :parser 'json-read
    :success (cl-function (lambda (&key data &allow-other-keys)
                            (salah-times--parse-response data)))))



(defun salah-times--parse-response(resp)
  (let (all-days today)
    (setq today (salah-times--today))
    (setq all-days (assoc-default 'data resp))
    (salah-times--render
     (mapcar
      (lambda (el)
        (let (return-obj date)
          (setq return-obj (assoc-default 'timings el))
          (setq date
                (string-join
                 (butlast (split-string ; Remove year from the date
                           (assoc-default 'readable (assoc-default 'date el)))) ; Get date from the payload
                 " "))
          (setq return-obj (mapcar (lambda(el)
                                     `(,(car el) .
                                       ,(car (split-string (cdr el))))) ; Remove the timezone string from the time.
                                   return-obj))
          (push `(Date . ,date) return-obj)
          return-obj))
      ;; Only keep yesterday's to 10 days data of the month
      (seq-subseq all-days (max 0 (- today (1+ salah-prev-days))) (min (+ salah-next-days today) (length all-days)))))))

(defun salah-times--render(data)
  (require 'vtable)
  (with-current-buffer (get-buffer-create salah-times-buffer)
    (erase-buffer)
    (make-vtable
     :columns '("Date"
                "Fajr"
                (:name "Sunrise" :width 7)
                (:name "Dhuhr" :width 6 )
                "Asr"
                (:name "Maghrib" :width 8)
                "Isha")
     :divider " "
     :row-colors (salah-times--row-colors (length data))
     :objects data
     :keymap (define-keymap
               "q" #'kill-buffer-and-window)
     :getter (lambda (object column vtable)
               (assoc-default (intern (vtable-column vtable column)) object)))
    (pop-to-buffer salah-times-buffer)))



(defun salah-times--row-colors(count)
  (let (colors today)
    (setq today (salah-times--today))
    (if (<= count 1)
        (push (doom-color 'region) colors)
      (dotimes (i count)
        (if (and (= 1 today)
                 (= i 0))
            (push (doom-color 'region) colors)
          (if (and (= i 1) (not (= today 1)))
              (push (doom-color 'region) colors)
            (if (= 0 (% i 2))
                (push (doom-color 'bg) colors)
              (push (doom-color 'bg-alt) colors))))))
    (reverse colors)))

(defun salah-times--today()
  (string-to-number (format-time-string "%d")))


(provide 'salahtimes)
;;; salahtimes.el ends here
