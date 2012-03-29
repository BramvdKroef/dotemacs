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
             (message server)
    	     (let* ((login (login-lookup server "irc")))
	       (if login
		   (erc-message "PRIVMSG"
                                (format "%s identify %s" 
                                        (if (string-match server "localhost.localdomain")
                                            "&bitlbee"
                                          (erc-default-target))
                                        (login-get login "password")))))))

(defun erc-notify-on-nick (matched-type nick msg)
  "Send a notification when the user's nick is mentioned."
  (when (and (string= matched-type "current-nick")
             (string-match "\\([^:]*\\).*:\\(.*\\)" msg)
             (not (string= (buffer-name) "&bitlbee")))
    (let((text (match-string 2 msg))
         (from (erc-extract-nick nick))
         (icon "/usr/share/icons/hicolor/48x48/apps/emacs.png"))
      
      (when text
        (let ((maxlength 128))
          (if ( > (length msg) maxlength )
              (setq msg (concat (substring msg 0 20) ".. *snip* .. "
                                (substring msg (- 30)) "."))))
	
        (setq msg (concat from " : " msg))
        (shell-command (concat "notify-send"
                               " -i " icon
                               " bitlbee '"
                               (format "%s:%d" from (% (nth 1
                                                            (current-time)) 3))
                               msg "'"))))))

(add-hook 'erc-text-matched-hook 'erc-notify-on-nick)

(defun erc-sound-notification (matched-type nick msg)
  (when (and (string= matched-type "current-nick")
             (string-match "\\([^:]*\\).*:\\(.*\\)" msg))
    (let((text (match-string 2 msg))
         (from (erc-extract-nick nick)))
      
      (when text
        (play-sound-file "~/.emacs.d/site-lisp/notify.wav")))))
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
