(require 'erc)

(if (require 'erc-bbdb nil t)
    (erc-bbdb-enable))

(erc-spelling-mode 1)
(setq erc-modules '(autojoin button completion fill irccontrols
                             match menu netsplit noncommands readonly
			     ring scrolltobottom stamp track smiley))
(require 'erc-nicklist nil t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
(setq erc-nick-serv '(("localhost" . "&bitlbee")))
(add-hook 'erc-after-connect
    	  '(lambda (server nick)
    	     (erc-message "PRIVMSG"
                          (format "%s identify %s" 
                                  (if (string-match server "localhost.localdomain")
                                      "&bitlbee"
                                    (erc-default-target))
                                  "bitlbee123"))))

(defun erc-notify-on-nick (matched-type nick msg)
  "Send a notification when the user's nick is mentioned."
  (when (and (string= matched-type "current-nick")
             (string-match "\\([^:]*\\).*:\\(.*\\)" msg))
    (let ((text (match-string 2 msg))
          (from (erc-extract-nick nick))
          (maxlength 128))
      (when text
        (if ( > (length msg) maxlength)
            (setq msg (concat (substring msg 0 20) "\n.. *snip* .. \n"
                              (substring msg (- 30)) ".")))
	(setq msg (concat from " : " msg))
        (inotify-message msg "bitlbee")))))

(add-hook 'erc-text-matched-hook 'erc-notify-on-nick)

(defun erc-sound-notification (matched-type nick msg)
  (when (and (string= matched-type "current-nick")
             (string-match "\\([^:]*\\).*:\\(.*\\)" msg))
    (when (match-string 2 msg)
      (play-sound-file "~/.emacs.d/site-lisp/notify.wav"))))

(add-hook 'erc-text-matched-hook 'erc-sound-notification)

(require 'bitlbee)

(defun my-kill-bitlbee ()
  "Breaks the connection with the bitlbee channel and stops bitlbee."
  (when (get-buffer "&bitlbee")
    (set-buffer "&bitlbee")
    (erc-quit-server "")
    (bitlbee-stop)))

(add-hook 'my-kill-emacs-hook 'my-kill-bitlbee)

(when (require 'bitlbee nil t)
  ;; Fire up the bitlbee server
  (bitlbee-start)
  (sleep-for 2)
  ;; Connect
  (erc :server "localhost" :port 6667 :nick "bram")

  (when 'lunch-break
    (add-hook 'lunch-break-start-hook
	      '(lambda ()
		 (with-current-buffer "&bitlbee"
		   (erc-cmd-AWAY "Food"))))
    
    (add-hook 'lunch-break-stop-hook
	      '(lambda ()
		 (with-current-buffer "&bitlbee"
		   (erc-cmd-AWAY " "))))))
