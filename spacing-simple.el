(require 'spacing-algorithm)

(defun spacing-simple-ask (item)
  (let ((question (car item))
	(answer (cdr item))
	quality)
    
    (if (string= (replace-regexp-in-string
                  "\n" " "
                  (read-string (concat question ": " ))) answer)
	;; Correct answer. Rate between 5 and 3 inclusive.
	(setq quality (min 5 (max 3 (- 6 (string-to-number
					  (read-string "Correct. How well did you remember the answer?
1 - Perfect response
2 - After hesitation
3 - Recalled with serious difficulty
Rate: "))))))
      ;; Wrong answer. Rate between 2 and 0 inclusive.
      (setq quality (min 2 (max 0 (- 3 (string-to-number
			  (read-string (concat "Wrong, it's " answer ". How do you rate your answer?
1 - The correct response seemed easy to recall
2 - Remembered the correct response 
3 - Complete blackout
Rate: ")))))))
      )
    )
  )

(defun spacing-simple-repitition ()
  (interactive)

  (spacing-algorithm-load)

  (let (item
	quality)
    (while (setq item (spacing-algorithm-get-queued-item))
      (setq quality (spacing-simple-ask (cadr (cdr item))))
      (spacing-algorithm-repetition item quality)
      (spacing-algorithm-save)
      )
    )
  (spacing-algorithm-save)
  )

(provide 'spacing-simple)
;;(spacing-simple-repitition)