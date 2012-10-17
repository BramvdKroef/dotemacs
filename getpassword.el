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
