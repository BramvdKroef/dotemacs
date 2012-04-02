
;; Include Common Lisp support
(require 'cl) 
(defvar *emacs-load-start* (current-time))

;; Get rid of that abominable beep
(setq ring-bell-function (lambda () (message "*beep*")))

;; Don't make backup files all the time. (autosave files are still made
;; but thats okay since they are deleted when a buffer is saved,
;; backups aren't).
(setq make-backup-files nil)

;; Show key sequences immediately
(setq echo-keystrokes 0.01)

;; Push utf-8
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")
(setq default-file-name-coding-system 'utf-8)
(setq current-language-environment "UTF-8")
(setq default-input-method "rfc1345")

;; no-tabs
(setq-default indent-tabs-mode nil)

;; enable copying to the clipboard
(setq x-select-enable-clipboard t)

;; My keybindings. It is importand that this is loaded before the experimental stuff
;; so that, in case of an error, emacs is usable.
(load "keybindings.el")

(load "~/.emacs.d/package.el")

(add-to-list 'package-archives '("marmalade"
                                 . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA"
                                 . "http://tromey.com/elpa/"))
(package-initialize)

;; How to install a package from a url:
;;(let ((buffer (url-retrieve-synchronously
;;               "http://...")))
;;  (set-buffer buffer)
;;  (package-install-from-buffer (package-buffer-info) 'single))


;; Load theme configuration
(load "theme-conf.el")

;; EasyPG encryption. Has to be set up first because the conf files
;; need the password file.
;; Easy PG is now included in emacs.
;;(require 'epa-setup)

;; This is where all the passwords are stored
(defvar authinfo-file "~/.authinfo.gpg")
(require 'ange-ftp)
(setq ange-ftp-netrc-filename authinfo-file)
(setq ange-ftp-try-passive-mode t)


;;(require 'netrc)
(autoload 'netrc-parse "netrc")
(autoload 'netrc-machine "netrc")

;; Make looking up login information easier
(defun login-lookup (machine port)
  (let* ((authinfo (netrc-parse authinfo-file))
	 (machine (netrc-machine authinfo machine port port))
	 (process)
	 )
    (if machine
	machine
      (unless authinfo
	(message "Couldn't parse authinfo file"))
      nil
      )))

(defalias 'login-get 'netrc-get)

(defvar clipboard-paste-bin
  (if (or (eq system-type 'darwin)
	  (eq system-type 'macos))
	  "/usr/bin/pbcopy"
    (if (or (eq system-type 'windows-nt)
	    (eq system-type 'cygwin))
	"clipboard" ;;fix this
      )
    "xclip"))

(defun yank-to-clipboard ()
  "Yanks the last item in the kill ring and sends it to the system clipboard"
  (interactive)

  (call-process-shell-command
   (concat "echo -n \""
           (replace-regexp-in-string "[\\\"]" "\\\\\\\&" (car kill-ring-yank-pointer))
           "\" | "
           clipboard-paste-bin)))

(defun password-to-clipboard (machine port)
    "Look up a password for a machine and port and add it to the kill-ring. After 15
seconds it is deleted from the kill-ring."
    (interactive "sMachine: \nsPort: ")
    (let ((login (login-lookup machine port)))
      (if login
	  (let ((password (login-get login "password")))
	    ;;(call-process-shell-command
	    ;;(concat "echo -n \"" (login-get login "password") "\" | "
	    ;;clipboard-paste-bin))
	    (kill-new password)
	    (message "Copied the password for %s to kill-ring"
		     (login-get login "login"))
	    (run-at-time "10 sec" nil
			 '(lambda ()
			    (setcar kill-ring-yank-pointer "")
			    (message "Deleted password from kill-ring")
			    ))
	    )
	(message "Couldn't find login"))))

(defun server-password ()
  "Look up root password for the server"
  (interactive)
  (password-to-clipboard "fortfrances.com" "su"))

(defun password-to-clipboard-dev ()
  "Look up root password for bram@leapontheweb.com"
  (interactive)
  (password-to-clipboard "leapontheweb.com" "ssh"))
(defun password-to-clipboard-rack ()
  "Look up root password for bvanderkroef@fortfrances.com"
  (interactive)
  (password-to-clipboard "fortfrances.com" "ssh"))

;; Load the files in the home folder
;;-----------

(load "custom-func.el")

;(if window-system
;    (require 'w3m-load))

;; Lorum Ipsum generator for creating random text
(autoload 'lorem-ipsum-insert-paragraphs "lorem-ipsum" "Autoload lorem ipsum generator" t)

;; Configure the calendar and holidays
(load "calendar-conf")
;; Configure emms player
(load "emms-conf")
;; Configure planner, remember and hooks
(load "gtd-conf")
;;-----------

;; setup major modes
(load "load-modes.el")

(setq default-major-mode 'org-mode)

;; Turn on auto fill for all text modes (doesn't seem to be working)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
;; Turn on auto fill for all modes instead
(setq-default auto-fill-function 'do-auto-fill)

;; Modify all modes based on c-mode
(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Highlight FIXME, TODO and BUG words
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
	    
	    ;; Reckognize studlyCaps as seperate words when moving around
	    (subword-mode 1)))

; open .h files in c++ mode instead of c mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;(load "find-file-root.el")


(require 'lunch-break nil t)

; Schedule lunch
(if 'growl
    (run-at-time "12:00" nil '(lambda () (growl "Lunch time" "It's time for lunch"))))

(if window-system
  (server-start))

(put 'downcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(org-agenda-list)

(require 'comint)
;; close all shells on shutdown
(add-hook 'my-emacs-kill-hook
          '(lambda ()
             (let ((shell-buffer (get-buffer "*shell*")))
               (when shell-buffer
                 (set-buffer shell-buffer)
                 (comint-send-eof)
                 (delete-process "*shell*")))

             (let ((shell-buffer (get-buffer "*SQL*")))
               (when shell-buffer
                 (set-buffer shell-buffer)
                 (comint-send-eof)))))

(message "Done loading in %ds"
	 (destructuring-bind (hi lo ms) (current-time)
	   (- (+ hi lo) (+ (first *emacs-load-start*) (second
	 *emacs-load-start*)))))
