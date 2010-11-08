
(setq jabber-server "im.apinc.org"
      jabber-nickname "Bram"
      jabber-history-enabled t
      jabber-use-global-history nil
      jabber-roster-line-format "%c %n - %s (%S) %u\n"
      jabber-message-alert-same-buffer nil)

;; spell check 
(add-hook 'jabber-chat-mode-hook 'flyspell-mode)
;; Highlight urls
(add-hook 'jabber-chat-mode-hook 'goto-address)

(defun my-jabber-presence-message (who oldstatus newstatus statustext)
"This function does the same as jabber-presence-default-message but without
status text."
(cond
 ((equal oldstatus newstatus)
  nil)
 (t
  (let ((formattedname
	 (if (> (length (get who 'name)) 0)
	     (get who 'name)
	   (symbol-name who)))
	(formattedstatus
	 (or
	  (cdr (assoc newstatus
		      '(("subscribe" . " requests subscription to your presence")
			("subscribed" . " has granted presence subscription to you")
			("unsubscribe" . " no longer subscribes to your presence")
			("unsubscribed" . " cancels your presence subscription"))))
	  (concat " is now "
		  (or
		   (cdr (assoc newstatus jabber-presence-strings))
		   newstatus))))
	)
    (concat formattedname formattedstatus)))))

(setq jabber-alert-presence-message-function 'my-jabber-presence-message)

(defun my-jabber-message-default-message (from buffer text)
  (when (or jabber-message-alert-same-buffer
	    (not (memq (selected-window) (get-buffer-window-list buffer))))
    (let ((formatted-msg (if (jabber-muc-sender-p from)
		      (format "Private message from %s in %s"
			      (jabber-jid-resource from)
			      (jabber-jid-displayname (jabber-jid-user from)))
		 (format "Message from %s" (jabber-jid-displayname from))))
	  )
      formatted-msg)))
;(setq jabber-alert-message-function 'jabber-message-default-message)

(defun jabber-message-growl (from buffer text proposed-alert)
  "Show message on growl"
  (if proposed-alert
      (growl proposed-alert text))
  )
(add-to-list 'jabber-alert-message-hooks 'jabber-message-growl)

(let* ((login (login-lookup jabber-server "jabber")))
  (if login
      (progn (setq jabber-username (login-get login "login")
		   jabber-password (login-get login "password"))
	     (jabber-connect))
    (message "Jabber: No login found. Make sure there is an entry in authinfo with machine '%s' and port 'jabber'" jabber-server)
    )
  )

(add-hook 'lunch-break-start-hook
	  '(lambda ()
	     (jabber-send-away-presence)))
(add-hook 'lunch-break-stop-hook
	  '(lambda ()
	     (jabber-send-presence "" "" 10)))
			  