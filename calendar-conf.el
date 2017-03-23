;; /file calendar-conf.el  configures the calendar sets the holidays
;; 
;; $Id: calendar-conf.el,v 1.4 2009-02-27 22:05:15 twd Exp $

(require 'calendar)
(require 'holidays)
(setq calendar-latitude [48 36 north])
(setq calendar-longitude [93 23 east])
(setq calendar-location-name "Fort Frances, CA")

(setq calendar-christian-all-holidays-flag nil)

;; remove some holidays
(setq holiday-general-holidays nil)   ; get rid of too U.S.-centric holidays
(setq holiday-hebrew-holidays nil)    ; get rid of religious holidays
(setq holiday-islamic-holidays nil)   ; get rid of religious holidays
(setq holiday-oriental-holidays nil)  ; get rid of Oriental holidays
(setq holiday-bahai-holidays nil)     ; get rid of Baha'i holidays


(setq holiday-canadian-statutory-holidays
      '((holiday-fixed 01 01 "New Year's Day")
        (holiday-easter-etc -2 "Good friday")
        (holiday-fixed 07 01 "Canada Day")
        (holiday-float 10 1 2 "Thanksgiving") ; Second monday of october
        (holiday-fixed 11 11 "Remembrance day")
        (holiday-fixed 12 25 "Christmas")
        (holiday-fixed 12 26 "Boxing Day")
        (holiday-float 05 1 -1 "Victoria day" 24)  ;Monday before May 24
        (holiday-float 08 1 1 "Civic Public Holiday") ; First monday of August
        (holiday-float 09 1 1 "Labour day") ; First monday in september
        ))

(setq holiday-local-holidays
      '((holiday-fixed 02 14 "Valentine's Day")
        (holiday-float 02 1 3 "Family day") ;Third monday in february
        (holiday-fixed 04 01 "April Fools' Day")
        (holiday-float 5 0 2 "Mother's Day")
        (holiday-float 6 0 3 "Father's Day")
        (holiday-fixed 11 11 "Remembrance day")
        (holiday-advent 0 "Advent")
        (holiday-easter-etc 0 "Easter Sunday")
        (holiday-easter-etc 39 "Ascension Day")
        (holiday-easter-etc 49 "Pentecost (Whitsunday)")
        (holiday-fixed 10 31 "Halloween")
        ))

; The above settings won't be used because calendar-holidays has already been
; generated; it has to be re-filled. 
(setq calendar-holidays
      (append
       holiday-canadian-statutory-holidays
       holiday-local-holidays
       holiday-solar-holidays))

(defun insert-date (&optional days)
    (interactive "p*")
    (insert
     (calendar-date-string
      (calendar-gregorian-from-absolute
       (+ (calendar-absolute-from-gregorian (calendar-current-date))
          days)))))

;; mark dates of holidays in the calendar
(setq calendar-mark-holidays-flag t)

(defun holiday-list-to-dates (holidays y1 y2)
  (let* ((y1 2017)
         (y2 2017)
         (calendar-holidays holidays)
         (s (calendar-absolute-from-gregorian (list 2 1 y1)))
         (e (calendar-absolute-from-gregorian (list 11 1 y2)))
         (displayed-month 2)
         (displayed-year y1)
         holiday-list)
    
    (while (<= s e)
      (setq holiday-list (append holiday-list (calendar-holiday-list)))
      (calendar-increment-month displayed-month displayed-year 3)
      (setq s (calendar-absolute-from-gregorian
               (list displayed-month 1 displayed-year))))
    holiday-list))

(defun generate-garbage-dates (y garbage-start-day n tag)
  (let* ((garbage-date garbage-start-day)
         (m 1)
         (holiday-list (holiday-list-to-dates
                        holiday-canadian-statutory-holidays
                        y
                        y)))

    (while (not (calendar-date-compare (list (list m garbage-date y))
                                       (car holiday-list)))
      (setq holiday-list (cdr holiday-list)))

    (while (<= m 12)
      (while (<= garbage-date (calendar-last-day-of-month m y))
        (insert (concat (calendar-date-string (list m garbage-date y))
                        ": " tag "\n"))
        (setq garbage-date (+ n garbage-date))

        (while (and holiday-list
                    (not (calendar-date-compare (list (list m
                                                            garbage-date y))
                                                (car holiday-list))))
          (progn
            (setq holiday-list (cdr holiday-list))
            (setq garbage-date (1+ garbage-date))
            (if (= 6 (calendar-day-of-week (list m garbage-date y)))
                (setq garbage-date (+ 2 garbage-date))))))
      (setq garbage-date (max 1 (- garbage-date (calendar-last-day-of-month m y))))
      (setq m (1+ m)))
    garbage-date))

;;(generate-garbage-dates 2017 4 7 "Garbage")
