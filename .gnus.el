
; Set gnus inbox reading
(setq gnus-select-method '(nnml ""))

(setq nnmail-crosspost nil)

;(eval-after-load "mail-source" '(require 'pop3))
(setq pop3-debug t)

(setq mail-sources '())

(let ((varmail (concat "/var/mail/" (user-login-name)))
      (homemail (concat "/home/" (user-login-name) "/mail/INBOX")))

  (if (file-exists-p varmail)
      (add-to-list 'mail-sources `(file :path ,varmail)))

  (if (file-exists-p homemail)
      (add-to-list 'mail-sources `(file :path ,homemail))))

(setq gnus-keep-backlog 500)

(setq gnus-auto-expirable-newsgroups
      "mail.junk\\|mail.server\\|nnimap+Mail:INBOX.Trash")

(setq gnus-message-archive-method '(nnml ""))
(setq gnus-message-archive-group 
      '((lambda (x)
	  (cond
	   ;; Store personal mail messages is the same group
;;	   ((string-match "mail.*" group) group)
	   (t "mail.sent")))))

; Prefer not to show the html part if there is a plaintext part
(setq mm-discouraged-alternatives '("text/richtext" "text/html"))

(defun my-gnus-summary-view-html-alternative-in-mozilla ()
      "Display the HTML part of the current multipart/alternative MIME message
    in mozilla."
      (interactive)
      (save-current-buffer
        (gnus-summary-show-article)
        (set-buffer gnus-article-buffer)
        (let ((file (make-temp-file "html-message-" nil ".html"))
              (handle (cdr (assq 1 gnus-article-mime-handle-alist))))
          (mm-save-part-to-file handle file)
          (browse-url (concat "file://" file)))))

(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-debug-info t
      smtpmail-auth-credentials authinfo-file
      starttls-use-gnutls t
      starttls-extra-arguments nil)

(defun set-smtp (email-address)
  "Goes through the smtp-accounts list and if one of the email
addresses match the argument then the smtp settings are set to that account." 
  (let ((server nil)
        (port nil))
    (dolist (account smtp-accounts)
      (if (string= (car account) email-address)
          (setq server (nth 1 account)
                port (nth 2 account))))
    
    (if (or (eq server nil) (eq port nil))
        (error (concat "No smtp info available for " email-address)))
    
    (setq smtpmail-starttls-credentials
          (list (list server port nil nil))
          smtpmail-smtp-server server
          smtpmail-smtp-service port)))

;; set the smtp info to the default address
(set-smtp user-mail-address)

(add-hook 'message-mode-hook 'flyspell-mode)

;; extract text from Word attachments with antiword and display in email 
(add-to-list 'mm-inlined-types "application/msword")
(add-to-list 'mm-inline-media-tests
	     '("application/msword"
	       (lambda (handle)
		 (mm-inline-render-with-stdin handle nil
					      "antiword" "-"))
	       identity))

;; extract text from MS .docx documents with docx2txt.pl
(add-to-list 'mm-inlined-types "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
(add-to-list 'mm-inline-media-tests
	     '("application/vnd.openxmlformats-officedocument.wordprocessingml.document"
	       (lambda (handle)
                 (mm-inline-render-with-file handle nil
					      "docx2txt.pl" 'file "-"))
	       identity))

;; extract text from OO.org .odt documents with odt2txt
(add-to-list 'mm-inlined-types "application/vnd.oasis.opendocument.text")
(add-to-list 'mm-inline-media-tests
	     '("application/vnd.oasis.opendocument.text"
	       (lambda (handle)
                 (mm-inline-render-with-file handle nil
                                             "odt2txt" 'file))
	       identity))

;; extract text from pdf documents with pdftotext
(add-to-list 'mm-inlined-types "application/pdf")
(add-to-list 'mm-inline-media-tests
	     '("application/pdf"
	       (lambda (handle)
                 (mm-inline-render-with-file handle nil
                                             "pdftotext" 'file "-"))
	       identity))

(defun my-kill-gnus ()
  (bbdb-save-db)
  (let ((gnus-interactive-exit nil))
    (gnus-group-exit)))

(add-hook 'my-kill-emacs-hook 'my-kill-gnus)
