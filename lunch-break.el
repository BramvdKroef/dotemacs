
(require 'zone)

(defgroup lunch-break nil
  "Runs hooks right before and after lunch break"
  :prefix "lunch-break-"
  :group 'applications)

(defcustom lunch-break-start-hook '()
  "Add hooks to this list that have to be called right before lunch-break"
  :group 'lunch-break
  :type '(repeat function)
  )

(defcustom lunch-break-stop-hook '()
  "Add hooks to this list that have to be called right after lunch-break"
  :group 'lunch-break
  :type '(repeat function)
  )

(defun lunch-break-now ()
  "Alert the system that we are taking a lunch break"
  (interactive)

  (run-hooks 'lunch-break-start-hook)
  ;; Zone out
  (zone)
  ;; In case zone fails
  (read-from-minibuffer "Press enter to continue")

  (run-hooks 'lunch-break-stop-hook)
  )

(provide 'lunch-break)