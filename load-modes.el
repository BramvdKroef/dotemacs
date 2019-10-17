
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

;; Modify all modes based on c-mode
(add-hook 'c-mode-common-hook
	  (lambda ()
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
(setq tidy-config-file "~/.tidy"
      tidy-temp-dir "/tmp")

(autoload 'no-word "no-word" "word to txt")

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

;; Pabbrev causes problems with files loaded over ftp
(autoload 'pabbrev-mode "pabbrev" "Autoloads pabbrev mode" t)

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'inferior-moz-process "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(autoload 'move-line-up "moveline" "Autoload moveline" t)
(autoload 'move-line-down "moveline" "Autoload moveline" t)

(autoload 'csv-mode "csv-mode" "Autoloads csv-mode" t)

(autoload 'quenya-add-lesson "quenya" "Autoloads quenya lessons" t)
(autoload 'spacing-simple-repitition "spacing-simple" "Autoloads spacing-simple" t)

(require 'compile)

(add-hook 'c-mode-hook
          (lambda () 
            (set (make-local-variable 'compile-command)
                 (format "make -f %s" (my-get-above-makefile)))))

(add-hook 'less-css-mode-hook 
          (lambda () 
            (set (make-local-variable 'compile-command)
                 (format "make -C %s" (file-name-directory (my-get-above-makefile))))))


(require 'typopunct)
(typopunct-change-language 'english t)
(defconst typopunct-minus (decode-char 'ucs #x2212))
(defconst typopunct-pm    (decode-char 'ucs #xB1))
(defconst typopunct-mp    (decode-char 'ucs #x2213))
(defadvice typopunct-insert-typographical-dashes
  (around minus-or-pm activate)
  (cond
   ((or (eq (char-before) typopunct-em-dash)
        (looking-back "\\([[:blank:]]\\|^\\)\\^" nil))
    (delete-char -1)
    (insert typopunct-minus))
   ((looking-back "[^[:blank:]]\\^" nil)
    (insert typopunct-minus))
   ((looking-back "+/" nil)
    (progn (replace-match "")
           (insert typopunct-pm)))
   (t ad-do-it)))
(defun typopunct-insert-mp (arg)
  (interactive "p")
  (if (and (= 1 arg) (looking-back "-/" nil))
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
         (looking-back "\\.\\." nil))
    (replace-match "")
    (insert typopunct-ellipsis))
   (t
    (self-insert-command arg))))
(define-key typopunct-map "." 'typopunct-insert-ellipsis-or-middot)

(eval-after-load "erc"
  '(load "erc-conf"))

;; Configure emms player
(eval-after-load "emms"
  '(load "emms-conf"))

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(less))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(global-company-mode)

(editorconfig-mode)
