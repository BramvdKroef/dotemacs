
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

;; Load theme configuration
(load "theme-conf")

;; Load package manager
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(require 'org)

;; This is where all the passwords are stored
(defvar authinfo-file "~/.authinfo.gpg")

;; Load the files in the home folder
;;-----------
;; Custom functions
(load "custom-func")

;; Configure the calendar and holidays
(load "calendar-conf")

;; Configure planner, remember and hooks
(load "gtd-conf")

;; Setup major modes
(load "load-modes")

(load "automode")
;;-----------

(setq major-mode 'org-mode)

;; Turn on auto fill for all text modes (doesn't seem to be working)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
;; Turn on auto fill for all modes instead
(setq-default auto-fill-function 'do-auto-fill)

(setq browse-url-browser-function 'browse-url-generic)

(setq-default fill-column 80)

(if window-system
  (server-start))

(put 'downcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq compilation-ask-about-save nil)

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
         (- (second (current-time))
            (second *emacs-load-start*)))
         
