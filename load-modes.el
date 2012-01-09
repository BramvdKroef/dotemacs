;; --- nXML mode ---

;;(load  "rng-auto.el")

(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
		   'nxml-mode))

(unify-8859-on-decoding-mode)

(setq magic-mode-alist
      (cons '("<＼＼?xml " . nxml-mode)
	    magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
;; -------

;; Allow window switching using M-[number]
(require 'window-number)
(window-number-mode)
(window-number-meta-mode)

;; Give buffers with the same file name a unique name based on their path
(require 'uniquify)
(setq uniquify-buffer-name-style (quote post-forward))

;; JDE mode
(eval-after-load "jde"
  '(setq jde-ant-enable-find t
	 jde-build-function (quote (jde-ant-build))
	 jde-checkstyle-classpath (quote ("/usr/share/java/checkstyle.jar" "/usr/share/java/commons-cli.jar" "/usr/share/java/commons-beanutils.jar" "/usr/share/java/commons-logging.jar" "/usr/share/java/antlr.jar"))
	 jde-complete-function (quote jde-complete-minibuf)
	 jde-devel-debug nil
	 jde-enable-abbrev-mode t
	 jde-help-browser-function "w3m-browse-url"
	 jde-help-docsets (quote (("JDK API" "http://localhost/~bram/doc/java-docs/api" nil)))
	 jde-help-use-frames nil
	 jde-jdk-doc-url "http://localhost/~bram/doc/java-docs/api/"
	 jde-jdk-registry (quote (("1.6.0" . "/usr/lib/jvm/java-6-sun")))
	 jde-mode-abbreviations (quote (("ab" . "abstract") ("bo" . "boolean") ("br" . "break") ("by" . "byte") ("byv" . "byvalue") ("cas" . "cast") ("ca" . "catch") ("ch" . "char") ("cl" . "class") ("co" . "const") ("con" . "continue") ("de" . "default") ("dou" . "double") ("el" . "else") ("ex" . "extends") ("fa" . "false") ("fi" . "final") ("fin" . "finally") ("fl" . "float") ("fo" . "for") ("fu" . "future") ("ge" . "generic") ("go" . "goto") ("impl" . "implements") ("impo" . "import") ("ins" . "instanceof") ("inte" . "interface") ("lo" . "long") ("na" . "native") ("ne" . "new") ("nu" . "null") ("pa" . "package") ("pri" . "private") ("pro" . "protected") ("pu" . "public") ("re" . "return") ("sh" . "short") ("st" . "static") ("su" . "super") ("sw" . "switch") ("sy" . "synchronized") ("th" . "this") ("thr" . "throw") ("thro" . "throws") ("tra" . "transient") ("tr" . "true") ("vo" . "void") ("vol" . "volatile") ("wh" . "while")))))


(load-file "~/.emacs.d/site-lisp/tramp/lisp/tramp-loaddefs.el")

;; Make buffer-switch and find-file list easier
;;(require 'ido)
(ido-mode t)

;; Snippets
(autoload 'yas/hippie-try-expand "yasnippet" "Autoload yasnippet" t)
(eval-after-load "yasnippet"
  '(progn
     (yas/initialize)
     (setq yas/root-directory '("~/.emacs.d/site-lisp/yasnippet/snippets"
                                "~/.emacs.d/site-lisp/snippets"))
     (mapc 'yas/load-directory yas/root-directory)))

;; Prepend snippet expand to hippie expand list
(add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)

;; Dictionary lookup
(load "dictionary-init" t)
(global-set-key "\C-cs" 'dictionary-search)
(global-set-key "\C-cm" 'dictionary-match-words)

;; tidy
;;(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
;;(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
;;(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)

;; Typing game
(autoload 'typing-of-emacs "typing" "The Typing Of Emacs, a game." t)

;; Require growl.el on mac (sends notifications)
(if (or (eq system-type 'darwin)
	(eq system-type 'macos))
   (progn
     ;;(autoload 'growl "growl")
     (eval-after-load "growl"
       '(setq growl-program "/usr/local/bin/growlnotify"))))

;; Jabber
;;(if (require 'jabber nil t)
;;    (load "jabber-conf.el"))


(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

;; PHP mode
(autoload 'php-mode "php-mode" "" t)

;; open drupal modules in php mode
(add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; cakephp views
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . html-mode))

;; Espresso mode
;; espresso is now included in emacs 23.2 as js-mode
;;(autoload 'espresso-mode "espresso")
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.conkerorrc\\'" . javascript-mode))

(add-to-list 'auto-mode-alist '("\\.stumpwmrc\\'" . lisp-mode))

(add-hook 'css-mode-hook 'rainbow-mode)

;; Flymake mode
(require 'flymake)
;;(autoload 'flymake-mode "flymake" "" t)

(defun my-flymake-show-err ()
  "Display error message at point"
  (interactive)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (message err))))

(defun flymake-php-init()
  "Use php to check the syntax of the current file."
  (let* ((temp
          (flymake-init-create-temp-buffer-copy
           (if (or (string-match "/ftp:" buffer-file-name)
                   (string-match "/scp:" buffer-file-name)
                   (string-match "/scpc:" buffer-file-name))
               'flymake-create-temp-with-folder-structure
             'flymake-create-temp-inplace)))

         (local (file-relative-name temp (file-name-directory
                                          buffer-file-name))))
    (list "php" (list "-f" local "-l"))))

(defun flymake-php-add-hooks ()
  ;; The php syntax check uses the -l (lint) option so the file won't be executed

  (add-to-list 'flymake-err-line-patterns
               '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))

  (add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.inc$" flymake-php-init))
  ;; Drupal-type extensions
  (add-to-list 'flymake-allowed-file-name-masks '("\\.module$" flymake-php-init))
  ;; Automatically activating flymake for php-mode creates all kinds of trouble
  ;; when the buffer isn't associated with a file
  ;;(add-hook php-mode-hook (lambda () (flymake-mode 1)))
  (define-key php-mode-map '[M-S-up] 'flymake-goto-prev-error)
  (define-key php-mode-map '[M-S-down] 'flymake-goto-next-error)
  (define-key php-mode-map "\C-ce" 'my-flymake-show-err))
  
(eval-after-load "php-mode"
  '(flymake-php-add-hooks))

(eval-after-load "javascript-mode"
  '(progn
     (defun flymake-javascript-init()
       "Use jslint.js to check the syntax of the current file."
       (let* ((ftpindex (string-match "/ftp:" buffer-file-name))
	      (temp
	       (flymake-init-create-temp-buffer-copy
		(if (and ftpindex (= ftpindex 0))
		    'flymake-create-temp-with-folder-structure
		  'flymake-create-temp-inplace)))
	      (local (file-relative-name temp (file-name-directory buffer-file-name))))
	 (list "/usr/local/sbin/jslint" (list local))))

     (add-to-list 'flymake-err-line-patterns
		  '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$" nil 1 2 3))

     (add-to-list 'flymake-allowed-file-name-masks '("^\\(/Users\\|/Volumes\\|/var\\).*\\.js$" flymake-javascript-init))

     (define-key javascript-mode-map '[M-S-up] 'flymake-goto-prev-error)
     (define-key javascript-mode-map '[M-S-down] 'flymake-goto-next-error)
     (define-key javascript-mode-map "\C-ce" 'my-flymake-show-err)

     (add-hook 'javascript-mode-hook (lambda () (flymake-mode 1)))))


;; Ledger mode
(autoload 'ledger-mode "ledger" "Autoload ledger mode" t)
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

;; Gnus
(setq gnus-init-file "~/.emacs.d/site-lisp/.gnus.el")
(gnus)


;; Predictive
(autoload 'predictive-mode "predictive" "predictive" t)
(setq auto-completion-syntax-alist '(accept . word))
(set-default 'predictive-auto-add-to-dict t)
(setq predictive-main-dict 'brams-dictionary
      predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (not (tramp-tramp-file-p (buffer-file-name)))
                (predictive-mode))))

;; Pabbrev causes problems with files loaded over ftp
(autoload 'pabbrev-mode "pabbrev" "Autoloads pabbrev mode" t)

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'inferior-moz-process "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(defun moz-send-string (str)
  "The author of mozRepl.el doesn't understand delegates so I had to write this function in order to be
able to send strings"
  (comint-send-string (inferior-moz-process)
                      str))
(defun moz-stop()
  (comint-delchar-or-maybe-eof (inferior-moz-process)))

(defun conkeror-reload ()
  "Reloads the current page in conkeror"
  (interactive)
  (moz-send-string "buffers.current.document.location.reload();\n"))

(defun conkeror-open-url (url arg)
  "Loads the given url conkeror"
  (moz-send-string (concat "conkeror.browser_object_follow(buffers.current, conkeror.OPEN_NEW_BUFFER, '"
			   url
			   "');\n")))
;; open links in conkeror
(setq browse-url-browser-function 'browse-url-generic)

;;(defun javascript-custom-setup ()
;;  (moz-minor-mode 1))

;;(add-hook 'javascript-mode-hook 'javascript-custom-setup)

(autoload 'csv-mode "csv-mode" "Autoloads csv-mode" t)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(autoload 'quenya-add-lesson "quenya" "Autoloads quenya lessons" t)
(autoload 'spacing-simple-repitition "spacing-simple" "Autoloads spacing-simple" t)

;;(autoload 'twit-show-recent-tweets "twit" "Autoloads twitter client" t)

(autoload 'twit "twittering-mode" "Autoload twittering mode" t)
(setq twittering-use-master-password t)


;; (eval-after-load "sql"
;;     '(progn
;;        ;; configure a mysql connection with the test server
;;        (setq sql-server "192.168.181.80")

;;        (let ((login (login-lookup sql-server "mysql")))
;; 	 (when login 
;; 	   (setq
;; 	    sql-user (login-get login "login")
;; 	    sql-password (login-get login "password")
;; 	    )))
;;        ))


;;(when (require 'mmm-mode nil t)
;;  (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
;;  (set-face-background 'mmm-default-submode-face "gray15")
;;)

(autoload 'etags-select-find-tag-at-point "etags-select" "Autoload etag-select" t)

(autoload 'pomodoro "pomodoro" "Autoload pomodoro" t)

(require 'compile)

(defun get-above-makefile ()
  (if (not (tramp-tramp-file-p default-directory))
      (expand-file-name "Makefile"
                        (let ((d default-directory))
                          (while (not (or (file-exists-p (expand-file-name
                                                          "Makefile" d))
                                          (string-equal "/" d)))
                            (setq d (directory-file-name
                                     (file-name-directory d))))))
    nil))

;; This creates an infinite loop on remote files
(add-hook 'c-mode-hook
          (lambda () 
            (set (make-local-variable 'compile-command)
                 (format "make -f %s" (get-above-makefile)))))

(add-hook 'css-mode-hook
          (lambda ()
            (let ((file buffer-file-name))
              (when (string= (file-name-extension file) "less")
                (set (make-local-variable 'compile-command)
                     (format "lessc %s %s" file
                             (concat (file-name-sans-extension file) ".css")))))))

(require 'typopunct)
(typopunct-change-language 'english t)
(defconst typopunct-minus (decode-char 'ucs #x2212))
(defconst typopunct-pm    (decode-char 'ucs #xB1))
(defconst typopunct-mp    (decode-char 'ucs #x2213))
(defadvice typopunct-insert-typographical-dashes
  (around minus-or-pm activate)
  (cond
   ((or (eq (char-before) typopunct-em-dash)
        (looking-back "\\([[:blank:]]\\|^\\)\\^"))
    (delete-char -1)
    (insert typopunct-minus))
   ((looking-back "[^[:blank:]]\\^")
    (insert typopunct-minus))
   ((looking-back "+/")
    (progn (replace-match "")
           (insert typopunct-pm)))
   (t ad-do-it)))
(defun typopunct-insert-mp (arg)
  (interactive "p")
  (if (and (= 1 arg) (looking-back "-/"))
      (progn (replace-match "")
             (insert typopunct-mp))
    (self-insert-command arg)))
(define-key typopunct-map "+" 'typopunct-insert-mp)

(defconst typopunct-ellipsis (decode-char 'ucs #x2026))
(defconst typopunct-middot   (decode-char 'ucs #xB7)) ; or 2219
(defun typopunct-insert-ellipsis-or-middot (arg)
  "Change three consecutive dots to a typographical ellipsis mark."
  (interactive "p")
  (cond
   ((and (= 1 arg)
         (eq (char-before) ?^))
    (delete-char -1)
    (insert typopunct-middot))
   ((and (= 1 arg)
         (eq this-command last-command)
         (looking-back "\\.\\."))
    (replace-match "")
    (insert typopunct-ellipsis))
   (t
    (self-insert-command arg))))
(define-key typopunct-map "." 'typopunct-insert-ellipsis-or-middot)

(defvar my-start-bitlbee-on-boot t)

(if (and my-start-bitlbee-on-boot (require 'erc nil t))
    (load "erc-conf.el"))
