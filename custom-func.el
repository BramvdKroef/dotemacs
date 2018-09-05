;;; custom-func ---  Custom functions
;;; Code:
;;; Commentary:


(defadvice yank (after indent-region activate)
  "Indent pasted code."
  (if (member major-mode
              '(emacs-lisp-mode lisp-mode c-mode c++-mode latex-mode python-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice kill-line (before check-position activate)
  "When pressing C-k at the end of a line, eat indentation."
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
                                           c-mode c++-mode objc-mode
                                           latex-mode plain-tex-mode
                                           php-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))


(defun dos2unix ()
  "Remove all the carriage returns (^M) from files in dos format.

Automate M-% C-q C-m RET C-q C-j RET"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))
    (set-buffer-file-coding-system 'unix 't)))

(defun clean-dirty-html ()
  "Remove font and span tags and align, style and mce_* attributes."
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
  "Remove table tags."
  (interactive)
  (perform-replace "</?\\(table\\|tbody\\|tr\\|td\\)[^>]*>" "" 't 't nil nil nil (point-min) (point-max)))


(defcustom my-kill-emacs-hook '()
  "Add hooks to this list that have to be called right before Emacs is killed."
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
  "Evaluate a string STR with php."
  (shell-command-to-string
   (concat "php -r " (shell-quote-argument str) "")))

(defun eval-php-region ()
  "Evaluate PHP code in region."
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "php -r 'eval(file_get_contents(\"php://stdin\"));'"))

(defun eval-bash-region ()
  "Evaluate bash commands."
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "bash"))


(defun my-php-unserialize-region ()
  "Unserialize region."
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end)
   "php -r 'var_export(unserialize(file_get_contents(\"php://stdin\")));'" nil t))

(defun eval-python-region ()
  "Evaluate python code."
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "python2"))

(defun generate-password (&optional len)
  "Generate a string using cli utility 'pwgen'.

Calls 'pwgen' with flags '-y' to include symbols and '-s' to
generate random (non-memorable) passwords.

 - LEN   Specify the length of the password.  The default is 12."
  (interactive "p")
  (or (and (integerp len) (> len 1)) (setq len 12))
  (call-process "pwgen" nil t nil "-ys" (number-to-string len) "1"))

(defun generate-memorable-password (&optional len)
  "Generate a string that alternates consonants and vowels.

This makes a password that is less secure but more easy to communicate.

Calls 'pwgen' with flags '-B' to exclude ambiguous characters and '-A' to
exclude capitals.

- LEN	Specify the length of the password.  The default is 12."
  (interactive "p")
  (or (and (integerp len) (> len 1)) (setq len 12))
  (call-process "pwgen" nil t nil "-BA" (number-to-string len) "1"))

(defun inotify-message (message &optional title icon)
  "Create a notification popup using 'notify-send'.

MESSAGE is displayed as the content, prefixed by TITLE and ICON."
  (setq message (shell-quote-argument message))
  (setq title (if title (shell-quote-argument title) ""))
  (if (not icon)
      (setq icon "/usr/share/icons/hicolor/48x48/apps/emacs.png"))
  (setq icon (if icon (concat "-i '" (shell-quote-argument icon) "'") ""))
  (shell-command (concat "notify-send "
                         icon " " title " " message)))

(defun untabify-buffer ()
  "Run untabify on the buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a 'before-save-hook', and that
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
  "Display error message at point."
  (interactive)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (message err))))


(defun my-get-above-makefile ()
  "Find the make file."
  (if (tramp-tramp-file-p default-directory)
      nil
    
    (expand-file-name "Makefile"
                      (let ((d default-directory))
                        (while (not (or (file-exists-p (expand-file-name
                                                          "Makefile" d))
                                          (string-equal "/" d)))
                            (setq d (file-name-directory
                                     (directory-file-name d))))
                        d))))

(defun kill-empty-lines-in-region (start end)
  "Look through the lines in a region and delete lines that have no content or
only whitespace.

START region start.
END region end."
  (interactive "r")
  (goto-char start)
  (while (< (point) end)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (eolp)
        (kill-line)
      (forward-line))))

(defun kill-empty-lines-in-buffer ()
  "Go through the buffer and remove all empty lines or lines with only
whitespace."
  (interactive)
  (kill-empty-lines-in-region (point-min) (point-max)))
