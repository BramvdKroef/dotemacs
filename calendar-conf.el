;; /file calendar-conf.el  configures the calendar sets the holidays
;; 
;; $Id: calendar-conf.el,v 1.4 2009-02-27 22:05:15 twd Exp $

(require 'calendar)
(require 'holidays)
(setq calendar-latitude [48 36 north])
(setq calendar-longitude [93 23 east])
(setq calendar-location-name "Fort Frances, CA")

(setq calendar-christian-all-holidays-flag t)

;; remove some holidays
(setq holiday-general-holidays nil)   ; get rid of too U.S.-centric holidays
(setq holiday-hebrew-holidays nil)    ; get rid of religious holidays
(setq holiday-islamic-holidays nil)   ; get rid of religious holidays
(setq holiday-oriental-holidays nil)  ; get rid of Oriental holidays
(setq holiday-bahai-holidays nil)     ; get rid of Baha'i holidays

(setq holiday-local-holidays
      '(
        (holiday-fixed 01 01 "New Year's Day")
        (holiday-fixed 02 14 "Valentine's Day")
        (holiday-fixed 04 01 "April Fools' Day")
        (holiday-fixed 12 25 "Christmas")

        ;; holidays with variable dates
        (holiday-float 5 0 2 "Mother's Day")
        (holiday-float 6 0 3 "Father's Day")

        ;; Dutch holidays
        (holiday-fixed 04 30 "Koninginnedag")
        (holiday-fixed 05 04 "Herdenkingsdag")
        (holiday-fixed 05 05 "Bevrijdingsdag")
        (holiday-fixed 12 05 "Sinterklaas")
        (holiday-fixed 12 26 "2e Kerstdag")

        ;; Canadian holidays
        (holiday-fixed 07 01 "Canada Day")
        (holiday-fixed 12 26 "Boxing Day")
        (holiday-fixed 11 11 "Remembrance day")
        (holiday-float 09 1 1 "Labour day") ; First monday in september
        ;; Ontario holidays
        (holiday-float 02 1 3 "Family day") ;Third monday in february
        ;Victoria day ;Monday before May 24
        (holiday-float 08 1 1 "Civic Public Holiday") ; First monday of August
        (holiday-float 10 1 2 "Thanksgiving") ; Second monday of october
        ))

; The above settings won't be used because calendar-holidays has already been
; generated; it has to be re-filled. 
(setq calendar-holidays
      (append holiday-local-holidays holiday-christian-holidays holiday-solar-holidays))

(defun insert-date (&optional days)
    (interactive "p*")
    (insert
     (calendar-date-string
      (calendar-gregorian-from-absolute
       (+ (calendar-absolute-from-gregorian (calendar-current-date))
          days)))))

;; mark dates of holidays in the calendar
(setq calendar-mark-holidays-flag t)
