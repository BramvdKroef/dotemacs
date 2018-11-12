
;; Start the timer
(defvar *emacs-load-start* (current-time))

;; Include Common Lisp support
(require 'cl) 

;; Get rid of that abominable beep
(setq ring-bell-function (lambda () (message "*beep*")))

;; Don't make backup files all the time. (autosave files are still made
;; but thats okay since they are deleted when a buffer is saved,
;; backups aren't).
(setq make-backup-files nil)

;; Show key sequences immediately
(setq echo-keystrokes 0.01)

;; Push UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")
(setq default-file-name-coding-system 'utf-8)
(setq current-language-environment "UTF-8")
(setq default-input-method "rfc1345")

;; no-tabs
(setq-default indent-tabs-mode nil)

;; My keybindings. It is importand that this is loaded before the
;; experimental stuff so that, in case of an error, emacs is usable.
(load "keybindings")

;; Load package manager
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; This is where all the passwords are stored
(defvar authinfo-file "~/.authinfo.gpg")

;; Load the files in the home folder
;;-----------
;; Custom functions
(load "custom-func")

;; Configure the calendar and holidays
(load "calendar-conf")

;; Setup major modes
(load "load-modes")

(load "automode")
;;-----------

;; Turn on auto fill for all text modes (doesn't seem to be working)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
;; Turn on auto fill for all modes instead
(setq-default auto-fill-function 'do-auto-fill)

(setq browse-url-browser-function 'browse-url-generic)

(setq-default fill-column 80)

;; Start the server 
(if window-system
  (server-start))

(put 'downcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq compilation-ask-about-save nil)


(message "Done loading in %ds"
         (- (second (current-time))
            (second *emacs-load-start*)))
         
