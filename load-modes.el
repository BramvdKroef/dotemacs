;; --- nXML mode ---

;;(load  "rng-auto.el")

(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
		   'nxml-mode))

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

(require 'comint)

(eval-after-load "ange-ftp"
  '(progn (setq ange-ftp-netrc-filename authinfo-file)
          (setq ange-ftp-try-passive-mode t)))

;;(load-file "~/.emacs.d/site-lisp/tramp/lisp/tramp-loaddefs.el")

;; Make buffer-switch and find-file list easier
;;(require 'ido)
(ido-mode t)

;; Modify all modes based on c-mode
(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Highlight FIXME, TODO and BUG words
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
	    
	    ;; Reckognize studlyCaps as seperate words when moving around
	    (subword-mode 1)))

;; Snippets
(autoload 'yas/hippie-try-expand "yasnippet" "Autoload yasnippet" t)
(eval-after-load "yasnippet"
  '(progn
     (yas/initialize)
     (setq yas/root-directory '("~/.emacs.d/site-lisp/snippets"))
     (mapc 'yas/load-directory yas/root-directory)))

;; Prepend snippet expand to hippie expand list
(add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)

;; Dictionary lookup
(load "dictionary-init" t)

;; tidy
;;(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
;;(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
;;(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(setq tidy-config-file "~/.tidy"
      tidy-temp-dir "/tmp")

;; Jabber
;;(if (require 'jabber nil t)
;;    (load "jabber-conf.el"))


(autoload 'no-word "no-word" "word to txt")

;; PHP mode
(autoload 'php-mode "php-mode" "" t)


(autoload 'rainbow-mode "rainbow-mode" "" t)
(add-hook 'css-mode-hook 'rainbow-mode)
;; Turn off keyword colors ("red", "white", etc.) I don't use them and
;; I'm getting a lot of false positives in keywords and strings.
(setq rainbow-html-colors-alist nil)

;; Ledger mode
(autoload 'ledger-mode "ledger" "Autoload ledger mode" t)

;; Gnus
(setq gnus-init-file "~/.emacs.d/site-lisp/.gnus")

;; Predictive
(autoload 'predictive-mode "predictive" "predictive" t)
(setq auto-completion-syntax-alist '(accept . word))
(set-default 'predictive-auto-add-to-dict t)
(setq predictive-main-dict 'brams-dictionary
      predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (if (not (tramp-tramp-file-p (buffer-file-name)))
;;                 (predictive-mode))))

;; Pabbrev causes problems with files loaded over ftp
(autoload 'pabbrev-mode "pabbrev" "Autoloads pabbrev mode" t)

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'inferior-moz-process "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(autoload 'move-line-up "moveline" "Autoload moveline" t)
(autoload 'move-line-down "moveline" "Autoload moveline" t)

(autoload 'csv-mode "csv-mode" "Autoloads csv-mode" t)

(autoload 'quenya-add-lesson "quenya" "Autoloads quenya lessons" t)
(autoload 'spacing-simple-repitition "spacing-simple" "Autoloads spacing-simple" t)

;; Lorum Ipsum generator for creating random text
(autoload 'lorem-ipsum-insert-paragraphs "lorem-ipsum" "Autoload lorem ipsum generator" t)

(setq twittering-use-master-password t)

(autoload 'etags-select-find-tag-at-point "etags-select" "Autoload etag-select" t)

(require 'compile)

(add-hook 'c-mode-hook
          (lambda () 
            (set (make-local-variable 'compile-command)
                 (format "make -f %s" (my-get-above-makefile)))))

(add-hook 'less-css-mode-hook
          (lambda ()
            (let ((file buffer-file-name))
                (set (make-local-variable 'compile-command)
                     (format "lessc %s -o %s" file
                             (concat (file-name-sans-extension file)
                                     ".css"))))
            (define-key less-css-mode-map "\C-c\C-c" 'less-css-compile)))

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

(load "erc-conf")

