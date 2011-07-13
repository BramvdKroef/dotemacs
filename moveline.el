(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n) 
    (setq n 1)) 
  (let ((col (current-column)))
    (beginning-of-line)
    (forward-line 1)
    (transpose-lines n)
    (forward-line -1)
    (forward-char col))) 

(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))