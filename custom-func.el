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
                                           latex-mode plain-tex-mode
                                           php-mode))
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
    (set-buffer-file-coding-system 'unix 't)))

(defun clean-dirty-html ()
  (interactive)
  (let ((badtags '("font" "span"))
        (badattrs '("align" "style" "mce_[a-z]*")))
    (perform-replace
     (concat "</?\\("
             (mapconcat (lambda (x) x) badtags "\\|")
             "\\)[^>]*>")
     "" 't 't nil nil nil (point-min) (point-max))
    (perform-replace
     (concat " \\("
             (mapconcat (lambda (x) x) badattrs "\\|")
             "\\)=\"[^\"]*\"")
     "" 't 't nil nil nil (point-min) (point-max))
    (perform-replace "&nbsp;" " " 't 't nil nil nil
                     (point-min) (point-max))))

(defun html-remove-tables ()
  (interactive)
  (perform-replace "</?\\(table\\|tbody\\|tr\\|td\\)[^>]*>" "" 't 't nil nil nil (point-min) (point-max)))

(defun volume-set (volume)
  "Runs a script that changes to volume. The volume argument can be 0.0 to 1.99 (i think)"
  (call-process "~/scripts/volume.sh" nil nil nil volume))

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

(defcustom my-kill-emacs-hook '()
  "Add hooks to this list that have to be called right before
emacs is killed"
  :type '(repeat function)
  :group 'my-custom-functions)

(defun my-kill-emacs ()
  "Run all my-kill-emacs hooks."
  (interactive)
  (run-hooks 'my-kill-emacs-hook)
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
  (message "%d characters" (- (region-end) (region-beginning))))

(defun eval-php (str)
  (shell-command-to-string
   (concat "php -r " (shell-quote-argument str) "")))

(defun eval-php-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "php -r 'eval(file_get_contents(\"php://stdin\"));'"))

(defun eval-python-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "python2"))

(defun generate-password (&optional len)
  "Generates a string with equal amounts of lower case letters,
upper case letters and numbers.
 - n   Specify the length of the password. The default is 12."
  (interactive "p")
  (or (and (integerp len) (> len 1)) (setq len 12))

  (random t)
  (let ((result (make-string len ?x))
        (letters "abcdefghijklmnopqrstuvwxyz")
        (ltype 0))
    (dotimes (i len)
      (setq ltype (random 3))
      (aset result i
            (cond ((eq ltype 0)
                   (aref letters (random (length letters))))
                  ((eq ltype 1)
                   (upcase (aref letters (random (length letters)))))
                  ((eq ltype 2)
                   (string-to-char (number-to-string (random 9)))))))
    (insert result)))

(defun generate-phonetic-password (&optional len)
  "Generates a string that alternates consonants and vowels to make a
password that is less secure but more easy to communicate."
  (interactive "p")
  (or (and (integerp len) (> len 1)) (setq len 12))

  (random t)
  (let ((result (make-string len ?x))
        (consonants "bcdfghjklmnpqrstvwxyz")
        (vowels "aeiouy"))
    (dotimes (i len)
      (aset result i
            (if (eq (% i 2) 0)
                (aref consonants (random (length consonants)))
              (aref vowels (random (length vowels))))))
    (insert result)))

(defun inotify-message (message &optional title icon)
  (setq message (shell-quote-argument message))
  (setq title (if title (shell-quote-argument title) ""))
  (if (not icon)
      (setq icon "/usr/share/icons/hicolor/48x48/apps/emacs.png"))
  (setq icon (if icon (concat "-i '" (shell-quote-argument icon) "'") ""))
  (shell-command (concat "notify-send "
                         icon " " title " " message)))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

(defun my-flymake-show-err ()
  "Display error message at point"
  (interactive)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (message err))))

(defun moz-send-string (str)
  "The author of mozRepl.el doesn't understand delegates so I had to write this function in order to be
able to send strings"
  (comint-send-string (inferior-moz-process)
                      str))
(defun moz-stop()
  (comint-delchar-or-maybe-eof (inferior-moz-process)))

(defun conkeror-reload ()
  "Reloads the current page in conkeror"
  (interactive)
  (moz-send-string "buffers.current.document.location.reload();\n"))

(defun conkeror-open-url (url arg)
  "Loads the given url conkeror"
  (moz-send-string (concat "conkeror.browser_object_follow(buffers.current, conkeror.OPEN_NEW_BUFFER, '"
			   url
			   "');\n")))

(defun my-get-above-makefile ()
  (if (not (tramp-tramp-file-p default-directory))
      (expand-file-name "Makefile"
                        (let ((d default-directory))
                          (while (not (or (file-exists-p (expand-file-name
                                                          "Makefile" d))
                                          (string-equal "/" d)))
                            (setq d (directory-file-name
                                     (file-name-directory d))))))
    nil))
