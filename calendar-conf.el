;; /file calendar-conf.el  configures the calendar sets the holidays
;; 

(require 'calendar)
(require 'holidays)

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
        (holiday-fixed 10 31 "Halloween")))

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
  "Convert a list of holidays to a list of dates"
  (let* ((calendar-holidays holidays)
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

