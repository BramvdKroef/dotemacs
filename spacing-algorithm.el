
(defgroup spacing-algorithm nil
  "Retrieves cover images for albums in the EMMS browser"
  :prefix "spacing-algorithm-"
  :group 'applications)

;; Schedules items for testing
;; Add items through add-items then use (get-queue today) to get todays items
;; present them to the user and insert them back using (repetition item) then
;; use (get-queue today) again to get the items that have to be repeated. Loop
;; until get-queue returns an empty list.

(defvar spacing-algorithm-of '((2.5)))
(defvar spacing-algorithm-queue '())
(defcustom spacing-algorithm-fraction .5
  "Value between 0 and 1. The higher the fraction the faster the changes to the matrix."
  :group 'spacing-algorithm
  :type 'float)

(defcustom spacing-algorithm-file "~/.spacing-algorithm"
  "Where to save the optical factor matrix and the queue"
  :group 'spacing-algorithm
  :type 'file
  )

(defun spacing-algorithm-add-item (item)
  "Add an item to the queue"
  ;; set n 0 (hasn't gone through any repetitions yet)
  ;; set E-factor 2.5
  (spacing-algorithm-queue-item-today (list 0 2.5 item))
  )

(defun spacing-algorithm-load ()
  (load spacing-algorithm-file)
  )
(defun spacing-algorithm-save ()
  (with-temp-file spacing-algorithm-file
    (princ "(setq spacing-algorithm-of '" (current-buffer))
    (print spacing-algorithm-of (current-buffer))
    (princ ")\n" (current-buffer))
    (princ "(setq spacing-algorithm-queue '" (current-buffer))
    (print spacing-algorithm-queue (current-buffer))
    (princ ")\n" (current-buffer)))
  )

(defun spacing-algorithm-get-interval (n ef)
  (if (eq n 1)
      (spacing-algorithm-get-of 1 ef)
    (* (spacing-algorithm-get-interval (- n 1) ef)
       (spacing-algorithm-get-of n ef))
    )
  )

(defun spacing-algorithm-get-of (n ef)
  "Search in the matrix for the optimal factor for interval n and easiness factor ef"
  (if (eq n 1)
      4
    (let ((row (assoc n spacing-algorithm-of)))
      (if (null row)
	  ef
	(if (null (assoc ef (cadr row)))
	    ef
	  (cadr (assoc ef (cadr row))))
	)
      )
    )
  )

(defun spacing-algorithm-set-of (n ef value)
  "Set a newly calculated optimal factor in the matrix"
  (let ((row (assoc n spacing-algorithm-of)))
    (when (null row)
      (setq row (list n '()))
      (setq spacing-algorithm-of (append spacing-algorithm-of (list row)))
      )
    (if (null (assoc ef (cadr row)))
	(setcar (cdr row) (append (cadr row) (list (list ef value))))
      (setcar (cdr (assoc ef (cadr row))) value)
      )
    )
  )

(defun spacing-algorithm-queue-item (item)
  "Add the item to the schedule, calculate the interval to wait and schedule it that many days in the
future"
  (let ((queue-item (list item (+ (spacing-algorithm-today)
				  (spacing-algorithm-get-interval (car item) (cadr item))))))
    (if (null spacing-algorithm-queue)
	(setq spacing-algorithm-queue (list queue-item))
	(setcdr (last spacing-algorithm-queue) (list queue-item)))
  ))

(defun spacing-algorithm-today()
  (time-to-days (current-time))
  )

(defun spacing-algorithm-queue-item-today (item)
  "Add an item to the queue but shedule it for today instead of calculating the interval."
  (if (null spacing-algorithm-queue)
      (setq spacing-algorithm-queue (list (list item (spacing-algorithm-today))))
      (setcdr (last spacing-algorithm-queue) (list (list item (spacing-algorithm-today))))
      )
  )

(defun spacing-algorithm-get-queued-item ()
  ;; loop through queue and select the next item scheduled for date or earlier
  ;; remove items from queue
  ;; the returned list is: '( (interval1 ef1 item1) (interval2 ef2 item2))
  ;; where
  ;;   - interval is the number of times the item has been repeated and
  ;;   - ef is the easy factor of the item
  ;; Each list element has to be inserted through repetition seperately
  ;; after being presented to the user. Items that aren't inserted
  ;; dissappear.

  (let ((result)
	(current spacing-algorithm-queue)
	(previous nil)
	(today (spacing-algorithm-today))
	)
    ;; while current element in queue isn't empty
    (while (and current (not result))
      ;; if the date in the current element's value is today or earlier
      (if (<= (cadr (car current)) today)
	  ;; add the item to results (not the date)
	  (progn (setq result (caar current))
		 ;; remove from queue and advance to next item
		 (if previous
		     (setq current (setcdr previous (cdr current)))
		   (setq current (setq spacing-algorithm-queue (cdr current)))
		 ))
	;; advance to next item in queue
	(progn (setq previous current)
	       (setq current (cdr current))
	       )
	)
      )
    result
    )
  )

(defun spacing-algorithm-repetition (item quality)
  ;; Adjust the optimal factor for the item's interval number and easyness factor
  ;; depending on the quality returned
  (if (> (car item) 1)
      (let ((ofvalue (spacing-algorithm-get-of (car item) (cadr item)))
	    )
	(setq ofvalue (+ (* (- 1 spacing-algorithm-fraction) ofvalue)
			 (* spacing-algorithm-fraction (* ofvalue
							  (+ 0.72 (* quality 0.07))))))
	(spacing-algorithm-set-of (car item) (cadr item) ofvalue)
	)
    )
  ;; Adjust the easiness factor depending on the quality returned
  (if (< quality 3)
      (setcar item 1)
    (progn (setcar (cdr item)
		   (max 1.3
			(+ (cadr item)
			   (- 0.1 (* (- 5 quality)
				     (+ 0.08 (* (- 5 quality)
						0.02)))))))
	   (setcar item (+ (car item) 1)))
    )

  ;; Queue the item according to the algorithm or queue it for another repetition
  (if (< quality 4)
      (spacing-algorithm-queue-item-today item)
    (spacing-algorithm-queue-item item))
  )

(provide 'spacing-algorithm)