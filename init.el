
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
(load "keybindings")

(load "~/.emacs.d/package")

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
(load "theme-conf")

;; EasyPG encryption. Has to be set up first because the conf files
;; need the password file.
;; Easy PG is now included in emacs.
;;(require 'epa-setup)

;; This is where all the passwords are stored
(defvar authinfo-file "~/.authinfo.gpg")
(require 'ange-ftp)
(setq ange-ftp-netrc-filename authinfo-file)
(setq ange-ftp-try-passive-mode t)

(autoload 'netrc-parse "netrc")
(autoload 'netrc-machine "netrc")

(defadvice netrc-parse (after wipe-netrc-cache)
  "Empty the netrc-cache variable. netrc-parse stores the contents of
  gpg files in memory using what the author considers 'heavy
  encryption': rot13 and base64 encoding. The cache exists as long as
  emacs is running.

  Run emacsclient -e '(base64-decode-string (rot13-string (cdr
  netrc-cache)))' on a machine of a user that uses a gpg encrypted
  netrc file to get all his passwords.

  This only affects files that end in .gpg."
  (setq netrc-cache nil))
(ad-activate 'netrc-parse)

(defadvice auth-source-netrc-parse (after wipe-auth-netrc-cache)
  "Empty auth-source-netrc-parse cache"
  (auth-source-forget-all-cached))
(ad-activate 'auth-source-netrc-parse)

(setq auth-source-do-cache nil)

(defvar clipboard-paste-bin
  (if (or (eq system-type 'darwin)
	  (eq system-type 'macos))
	  "/usr/bin/pbcopy"
    (if (or (eq system-type 'windows-nt)
	    (eq system-type 'cygwin))
	"clipboard" ;;fix this
      "xclip")))

(defun yank-to-clipboard ()
  "Yanks the last item in the kill ring and sends it to the system clipboard"
  (interactive)

  (call-process-shell-command
   (concat "echo -n \""
           (replace-regexp-in-string "[\\\"]" "\\\\\\\&" (car kill-ring-yank-pointer))
           "\" | "
           clipboard-paste-bin)))

(defun password-to-clipboard (machine port)
    "Look up a password for a machine and port and add it to the
kill-ring. After 15 seconds it is deleted from the kill-ring."
    (interactive '(nil nil))

    (let ((accounts (netrc-parse authinfo-file))
          account)

      (if (not machine)
          (setq machine (completing-read "Host: " (netrc-list-machines
                                                   accounts))))
      (if (not port)
          (setq port (read-from-minibuffer "Port: ")))

      (setq account (netrc-machine accounts machine port))
      (if account
          (progn
            (kill-new (netrc-get account "password"))
            (message "Copied the password for %s to kill-ring"
                     (netrc-get account "login"))
            (run-at-time "10 sec" nil
                         '(lambda ()
                            (setcar kill-ring-yank-pointer "")
                            (message "Deleted password from kill-ring"))))
	(message "Couldn't find login"))))

(defun netrc-list-machines (accounts)
  (let (machines)
    (while accounts
      (push (cdr (assoc "machine" (car accounts))) machines)
      (pop accounts))
    machines))
  
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

(load "custom-func")

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
(load "load-modes")

(setq major-mode 'org-mode)

;; Turn on auto fill for all text modes (doesn't seem to be working)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
;; Turn on auto fill for all modes instead
(setq-default auto-fill-function 'do-auto-fill)


(autoload 'move-line-up "moveline" "Autoload moveline" t)
(autoload 'move-line-down "moveline" "Autoload moveline" t)

(load "automode")

;;(load "find-file-root.el")


(require 'lunch-break nil t)

; Schedule lunch
(if 'growl
    (run-at-time "12:00" nil '(lambda () (growl "Lunch time" "It's time for lunch"))))

(if window-system
  (server-start))

(put 'downcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

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
