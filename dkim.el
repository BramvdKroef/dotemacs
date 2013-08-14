
(defgroup dkim nil
  "Retrieves cover images for albums in the EMMS browser"
  :prefix "dkim-"
  :group 'gnus
  :group 'news
  :group 'mail)

(defcustom dkim-keys '()
  "List of private keys by email address.
Each element is a list of the form (EMAIL DOMAIN SELECTOR KEYFILE)"
  :group 'dkim
  :type '(alist :value-type (group string string file)))

(defcustom dkim-headers '("From" "Sender" "Reply-To" "Subject" "Date"
                         "Message-ID" "To" "Cc" "MIME-Version"
                         "Content-Type" "Content-Transfer-Encoding"
                         "Content-ID" "Content-Description"
                         "Resent-Date" "Resent-From" "Resent-Sender"
                         "Resent-To" "Resent-Cc" "Resent-Message-ID"
                         "In-Reply-To" "References" "List-Id"
                         "List-Help" "List-Unsubscribe"
                         "List-Subscribe" "List-Post" "List-Owner"
                         "List-Archive")
  "Headers that are included in the signature. This is a list of
                         header names."
  :group 'dkim
  :type '(repeat string))

(defcustom dkim-openssl-bin "openssl"
  "Path to openssl binary."
  :group 'dkim
  :type 'string)

(defcustom dkim-hash-algo 'sha256
  "Hashing algorithm to use."
  :group 'dkim
  :type '(radio (const sha256)
                (const sha1)))

(defun dkim-sign ()
  "Sign an email with a DKIM signature."
  (interactive)
  (let (privatekey headers bodyhash dkim-header headerhash)
    
    (save-restriction
      (message-narrow-to-headers-or-head)
      
      ;; Get private key for From address
      (setq privatekey
            (or (dkim-get-private-key (dkim-message-get-from))
                (error "No private key configured for %s"
                       (dkim-message-get-from))))

      ;; Gather relevant headers
      (setq headers (dkim-message-get-headers dkim-headers))
      
      (widen)

      ;; Generate hash of content
      (setq bodyhash (dkim-body-hash (dkim-message-get-body)))
      
      ;; Generate dkim header without signature
      (setq dkim-header
            (dkim-create-header privatekey
                                ;; add ':' to the end of the header
                                ;; names and cat them into one string.
                                (concat (mapconcat 'car headers ":") ":")
                                bodyhash ""))
      
      ;; Generate signature
      (setq headerhash
            (dkim-header-hash headers dkim-header (nth 3 privatekey)))

      ;; Cat the signature to the header and add it to the email
      (message-add-header (concat dkim-header headerhash)))))

(defun dkim-header-hash (headers dkim-header privatekey)
  "Canonicalize headers and create a signed sha256 digest.

The headers argument is a list with each element being a string
  containing a full copy of a header.

The dkim-header argument is a generated DKIM-Signature header
  with an empty signature ('b=') field and without trailing
  newline characters.

The privatekey argument is a path to the file containing the private
  rsa key."
  (with-temp-buffer
    ;; Canonicalize headers (downcase names and trim spaces from values)
    (mapc
     (lambda (x) (insert (concat (downcase (car x)) ":" (cdr x))))
     headers)
    
    (insert (concat "dkim-signature:" (substring dkim-header 16)))

    ;; setup the right newlines
    (encode-coding-region (point-min) (point-max) 'dos)

    ;; use openssl to create a signed digest of the headers
    (call-process-region (point-min) (point-max) dkim-openssl-bin t t nil
                         "dgst" "-sha256" "-sign"
                         (expand-file-name privatekey))

    (encode-coding-region (point-min) (point-max) 'raw-text)
    (base64-encode-region (point-min) (point-max) t)
    (buffer-string)))

(defun dkim-body-hash (body)
  "Formats and hashes the body content.
Make sure the body ends with 1 newline and is encoded with CRLF line
  endings.
The content is hashed with sha256 and returned encoded in base64."
  (string-match "[\n \t]*\\'" body)
  (setq body (replace-match "\n" nil nil body))

  (setq body (encode-coding-string body 'dos))
  
  (base64-encode-string
   (secure-hash 'sha256 body nil nil t)))

(defun dkim-create-header (privatekey headers bodyhash signature
                                      &optional timestamp)
  "Generate a DKIM-Signature email header."
  (setq timestamp (or timestamp (format-time-string "%s")))
  (concat "DKIM-Signature: " 
          "v=1; a=rsa-sha256; c=relaxed/simple; d=" (nth 1 privatekey) "; s="
          (nth 2 privatekey) "; t=" timestamp "; bh=" bodyhash
          "; h=" headers "; b=" signature))

(defun dkim-get-private-key (email)
  (assoc email dkim-keys))

(defun dkim-message-get-from ()
  "Extract From address from the current buffer."
  (nth 1 (mail-extract-address-components
          (message-fetch-field "From"))))

(defun dkim-message-fetch-header (header)
  "Same as `message-fetch-field' only return a cons containing
the header and the value"
  (let ((field (message-fetch-field header)))
    (if field
        (cons header field)
      nil)))

(defun dkim-message-get-headers (headers)
  "Extract headers from the current buffer.
The headers arguments is a list with header names without ':' or
  trailing spaces.
The headers are returned in a list of cons elements in the form
  (header-name . value)"
  (delq nil (mapcar 'dkim-message-fetch-header headers)))

(defun dkim-message-get-body ()
  "Extract body content from the current buffer. Moves point to the
start of the body."
  (message-goto-body)
  (buffer-substring-no-properties
   (point) (point-max)))

(provide 'dkim)
