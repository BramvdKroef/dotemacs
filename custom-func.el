;; Indent pasted code
(defadvice yank (after indent-region activate)
  (if (member major-mode
	      '(emacs-lisp-mode lisp-mode c-mode c++-mode latex-mode python-mode))
      (let ((mark-even-if-inactive t))
	(indent-region (region-beginning) (region-end) nil))))

;; When pressing C-k at the end of a line, eat indentation
(defadvice kill-line (before check-position activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
					   c-mode c++-mode objc-mode
					   latex-mode plain-tex-mode php-mode))

      (if (and (eolp) (not (bolp)))
	  (progn (forward-char 1)
		 (just-one-space 0)
		 (backward-char 1)))))

;; Remove all the carriage returns (^M) from files in dos format
(defun dos2unix ()
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))
    (set-buffer-file-coding-system 'unix 't)
    ))


;; Delete trailing whitespace and indent the buffer
(defun cleanup ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun clean-dirty-html ()
  (interactive)
  (perform-replace "</?font[^>]*>" "" 't 't nil nil nil (point-min) (point-max))
  (perform-replace "</?span[^>]*>" "" 't 't nil nil nil (point-min) (point-max))
  (perform-replace " align=\"[^\"]*\"" "" 't 't nil nil nil (point-min) (point-max))
  (perform-replace " style=\"[^\"]*\"" "" 't 't nil nil nil (point-min) (point-max))
  )

(defun volume-set (volume)
  "Runs a script that changes to volume. The volume argument can be 0.0 to 1.99 (i think)"
  (call-process "~/scripts/volume.sh" nil nil nil volume)
  )

(defun volume-set-normal ()
  "Set the volume to normal"
  (interactive)
  (volume-set "0.5")
  (message "Normal"))

(defun volume-set-loud ()
  "Set the volume to loud"
  (interactive)
  (volume-set "0.8")
  (message "Loud"))

(defun volume-set-quiet ()
  "Set the volume to quiet"
  (interactive)
  (volume-set "0.3")
  (message "Quiet"))

(defun volume-set-chopin ()
  "Chopin is exceptionally quiet"
  (interactive)
  (volume-set "1.3")
  (message "Very loud"))

(defun fetchmail ()
  "Start fetchmail or tell it to fetch mail"
  (interactive)
  (shell-command "/usr/bin/fetchmail"))

(defcustom my-kill-emacs-hook '()
  "Add hooks to this list that have to be called right before
emacs is killed"
  :type '(repeat function))

(defun my-kill-emacs ()
  "Run all my-kill-emacs hooks."
  (interactive)
  (run-hooks 'my-kill-emacs-hook)
  (save-some-buffers)
  ;; Give hooks some time to shut down.
  (sleep-for 2)
  (save-buffers-kill-emacs))


(defun my-count-words-in-region (start end)
  "Return the number of words in the region."
  (let ((text (buffer-substring-no-properties start end)))
    (with-temp-buffer
      (insert text)
      (shell-command-on-region (point-min) (point-max) "wc -w" nil t)
      (string-to-number (buffer-string)))))

(defun my-count-words-in-region-p ()
  "Return the number of words in the region."
  (interactive)
  (message "%d words" (my-count-words-in-region (region-beginning) (region-end))))

(defun my-count-chars-in-region ()
  "Return the number of characters in the region."
  (interactive)
  (message "%d characters" (- (region-end) (region-beginning)))
  )

(defun eval-php-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "php -r 'eval(file_get_contents(\"php://stdin\"));'")
  )

(defun generate-password (&optional n)
  (interactive "p")
  (message "%s" n)
  (or (> n 1) (setq n 12))
  (insert (shell-command-to-string (concat "echo -n `</dev/urandom "
                                           "tr -dc A-Za-z0-9 "
                                           "2> /dev/null "
                                           "| head -c"
                                           (number-to-string n)
                                           "`"))))
