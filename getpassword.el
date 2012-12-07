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

(defvar password-history-host '())
(defvar password-history-login '())
(defvar password-buffer-name "Password account info")

(defun yank-to-clipboard ()
  "Yanks the last item in the kill ring and sends it to the system clipboard"
  (interactive)

  (call-process-shell-command
   (concat "echo -n \""
           (replace-regexp-in-string "[\\\"]" "\\\\\\\&" (car kill-ring-yank-pointer))
           "\" | "
           clipboard-paste-bin)))

(defun password-netrc-parse (file)
  "Alternative function to netrc-parse that accept any type of token name."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))

    (password-netrc-parse-comment)
    (password-netrc-parse-eol)
    
    (let (accounts account property)
      (while (not (eobp))
        (setq property (list (password-netrc-parse-token)))
        (password-netrc-parse-witespace)

        (setcdr property
                (if (string= (car property) "macdef")
                    (password-netrc-parse-macdef)
                     (password-netrc-parse-token)))
        (password-netrc-parse-witespace)
        
        (when (and (string= (car property) "machine")
                   account)
          (push (nreverse account) accounts)
          (setq account '()))
        
        (push property account))
      
      (if (length account)
          (push (nreverse account) accounts))
      
      (nreverse accounts)))) 

(defun password-netrc-parse-witespace ()
  (skip-chars-forward "\t ")
  (password-netrc-parse-eol))

(defun password-netrc-parse-eol ()
  (while (and (eolp) (not (eobp)))
    (forward-line 1)
    (password-netrc-parse-comment)))

(defun password-netrc-parse-comment ()
  (skip-chars-forward "\t ")
  
  (while (eq (char-after) ?#)
    (forward-line 1)
    (skip-chars-forward "\t ")))

(defun password-netrc-parse-token ()
  (if (= (following-char) ?\")
      (read (current-buffer))
    (buffer-substring
     (point) (progn (skip-chars-forward "^\t\n ")
                    (point)))))
(defun password-netrc-parse-macdef ()
  "Parse until the next blank line or until the
  end of the buffer."
  (buffer-substring (point)
                    (progn (re-search-forward "\n[\t ]*\n"
                                              (point-max) 2)
                           (point))))

(defun password-account-select (accounts)
  "Look up account by having the user select the machine. If there are
  multiple accounts for a machine the user is asked for the login
  name"
  (interactive)

  (setq accounts
        (netrc-filter-accounts
         accounts
         (completing-read "Machine: " (netrc-list-attr accounts "machine")
                          nil t nil 'password-history-host)))
  (if (equal (length accounts) 0)
      (error "No accounts found that match that machine."))

  (if (equal (length accounts) 1)
      (car accounts)
    (let ((login
           (completing-read "Login: " (netrc-list-attr accounts "login")
                            nil t nil 'password-history-login))
          account)
      (catch 'account
        (while accounts
          (if (string= (netrc-get (car accounts) "login") login)
              (throw 'account (car accounts)))
          (pop accounts))
        (error "No account found that match that login.")))))

(defun password-to-clipboard () 
  "Look up a password for a machine and port and add it to the
kill-ring. After 10 seconds it is deleted from the kill-ring."
  (interactive)

  (password-attribute-to-clipboard
   (password-account-select
    (password-netrc-parse authinfo-file)) "password"))

(defun password-login-to-clipboard () 
  "Look up a username for a machine and port and add it to the
kill-ring. After 10 seconds it is deleted from the kill-ring."
  (interactive)

  (password-attribute-to-clipboard
   (password-account-select
    (password-netrc-parse authinfo-file)) "login"))

(defun password-attribute-to-clipboard (account attr)
  "Copy an attribute from the account variable to the
clipboard and remove it 10 seconds later."
  (kill-new (netrc-get account attr))
  (message "Copied the %s for %s@%s to kill-ring"
           attr
           (netrc-get account "login")
           (netrc-get account "machine"))
  (run-at-time "10 sec" nil
               '(lambda ()
                  (setcar kill-ring-yank-pointer "")
                  (message "Deleted password from kill-ring"))))

(defun password-show-account-raw (account)
  "Show account information for a machine and user."
  (let ((buffer (get-buffer-create password-buffer-name)))
    (switch-to-buffer buffer)
    (erase-buffer)
    (dolist (i account)
      (if (not (string= (car i) "password"))
          (insert (car i)  ": " (cdr i) "\n")))))

(defun password-show-account ()
  "Look up a password for a machine and user and show the information
  in the account info buffer."
  (interactive)

  (let ((account (password-account-select (password-netrc-parse authinfo-file))))
    (if account
        (password-show-account-raw account)
      (message "Couldn't find login"))))

(defun netrc-machine-login (accounts machine login &optional port)
  "Look for an account in the list accounts that matches the given
machine login and optionally the port. If a port is given, accounts
  that don't specify a port are assumed to work on all ports."
  (let (result)
    (while accounts
      (let ((account (car accounts)))
        (if (and (string= (netrc-get account "machine") machine)
                 (string= (netrc-get account "login") login)
                 (or (not port)
                     (not (netrc-get account "port"))
                     (netrc-port-equal (netrc-get account "port")
                                       port)))
            (push account result)))
      (pop accounts))
    (if result
        (car result))))

(defun netrc-list-attr (accounts attrname)
  "Extract values for a given attribute from accounts."
  (let (machines)
    (while accounts
      (push (netrc-get (car accounts) attrname) machines)
      (pop accounts))
    machines))

(defun netrc-filter-accounts (accounts machine &optional port)
  "Filter the list of accounts by the given machine name and
optionally a port. Essentially it works like netrc-machine but gives a
  list of accounts."
  (let (newaccounts)
    (while accounts
      (let ((account (car accounts)))
        (if (and (string= (netrc-get account "machine") machine)
                 (or (not port)
                     (netrc-port-equal (netrc-get account "port")
                            port)))
            (push account newaccounts)))
      (pop accounts))
    newaccounts))

(defun password-to-clipboard-server ()
  "Look up root password for the server"
  (interactive)
  (password-attribute-to-clipboard
   (netrc-machine-login (netrc-parse authinfo-file)
                        "fortfrances.com" "root") "password"))

(defun password-to-clipboard-dev ()
  "Look up root password for bram@leapontheweb.com"
  (interactive)
  (password-attribute-to-clipboard
   (netrc-machine-login (netrc-parse authinfo-file)
                        "leapontheweb.com" "bram") "password"))

(defun password-to-clipboard-rack ()
  "Look up root password for bvanderkroef@fortfrances.com"
  (interactive)
  (password-attribute-to-clipboard
   (netrc-machine-login (netrc-parse authinfo-file)
                        "fortfrances.com" "bvanderkroef" "ssh") "password"))
