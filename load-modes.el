;;; load-modes -- Load mode customizations
;;; Commentary:
;;; Code:

(if (not (fboundp 'use-package))
    (progn
      (when (not package-archive-contents)
        (package-refresh-contents))
      (package-install 'use-package)))

(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t))

;; Give buffers with the same file name a unique name based on their path
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style (quote post-forward)))

;; when a paren or brace or bracket is typed add the closing match
(use-package skeleton
  :bind ("[" . skeleton-pair-insert-maybe))

(use-package ace-window :ensure t :bind ("C-x o" . ace-window))
(use-package comint)
(use-package javascript-mode :mode "\\.conkerorrc\\'")
(use-package lisp-mode :mode "\\.stumpwmrc\\'")
(use-package no-word :commands (no-word) :mode "\\.doc\\'")
(use-package ledger :mode "\\.ledger\\'")
(use-package csv-mode :mode "\\.csv\\'")
(use-package pabbrev :ensure t)
(use-package json-mode :ensure t :mode "\\.json$")
(use-package editorconfig
  :ensure t
  :init
  (add-hook 'after-init-hook 'editorconfig-mode))

(use-package php-mode
  :ensure t
  :mode (("\\.module\\'" . php-mode)
         ("\\.php\\'" . php-mode)
         ("\\.inc\\'" . php-mode)))

;; Snippets
(use-package yasnippet
  :ensure t
  :config
  (yas/initialize)
  (setq yas/root-directory '("~/.emacs.d/site-lisp/snippets"))
  (mapc 'yas/load-directory yas/root-directory))

(use-package yasnippet-snippets :ensure t)

(use-package hippie-exp
  :bind ([(control ? )] . hippie-expand)
  :config
  ;; Prepend snippet expand to hippie expand list
  (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand))

;; Dictionary lookup
(use-package dictionary
  :ensure t
  :bind (("\C-cs" . dictionary-search)
         ("\C-cm" . dictionary-match-words)))

(use-package rainbow-mode
  :hook css-mode
  :config
  ;; Turn off keyword colors ("red", "white", etc.) I don't use them and
  ;; I'm getting a lot of false positives in keywords and strings.
  (setq rainbow-html-colors-alist nil))
 
(use-package moveline
  :bind (([(meta up)] . move-line-up)
         ([(meta down)] . move-line-down))
  :commands (move-line-up move-line-down))

(use-package compile
  :config
  (add-hook 'c-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (format "make -f %s" (my-get-above-makefile))))))

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

(use-package typopunct
  :config
  (progn
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
    (define-key typopunct-map "+" 'typopunct-insert-mp))
  (defconst typopunct-ellipsis (decode-char 'ucs #x2026))
  (defconst typopunct-middot   (decode-char 'ucs #xB7)) ; or 2219
  (define-key typopunct-map "." 'typopunct-insert-ellipsis-or-middot))

;; Configure emms player
(use-package emms
  :bind (("\C-xpp" . emms-pause)
         ("\C-xpf" . emms-next)
         ("\C-xpb" . emms-previous)
         ("\C-xps" . emms-stop)
         ("\C-xpl" . emms-smart-browse))
  :config (load "emms-conf"))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)))

(use-package projectile :ensure t)

(defun my/flycheck-local-config ()
  "See if the file is in a project with local executables."
  (and (projectile-project-p)
       (let ((stylelintrc (expand-file-name ".stylelintrc"
                                            (projectile-project-root)))
             (phpmd-ruleset (expand-file-name "ruleset.xml"
                                              (projectile-project-root)))
             (phpstan-config (expand-file-name "phpstan.neon"
                                               (projectile-project-root)))
             (gherkin-lintrc (expand-file-name ".gherkin-lintrc"
                                               (projectile-project-root))))
         (when (file-readable-p stylelintrc)
           (setq-local flycheck-stylelintrc stylelintrc))
         (when (file-readable-p phpmd-ruleset)
           (setq-local flycheck-phpmd-rulesets phpmd-ruleset))
         (when (file-readable-p phpstan-config)
           (setq-local phpstan-config-file phpstan-config)
           (setq-local phpstan-level 'max)
           (setq-local phpstan-working-dir (projectile-project-root)))
         (when (file-readable-p gherkin-lintrc)
           (setq-local flycheck-gherkin-lintrc gherkin-lintrc)))))

(defun my/configure-web-mode-flycheck-checkers ()
  "Enable checkers for web mode

In order to have flycheck enabled in web-mode, add an entry to this
   cond that matches the web-mode engine/content-type/etc and returns the
   appropriate checker."
  (-when-let (checker (cond
                       ((string= (file-name-extension buffer-file-name) "twig")
                        'twig-twigcs)))
    (flycheck-mode)
    (flycheck-select-checker checker)))

(use-package flycheck
  :ensure t
  :hook (web-mode . #'my/configure-web-mode-flycheck-checkers)
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (progn
    (use-package flycheck-twig
      :config
      (add-hook 'flycheck-mode-hook #'flycheck-twig-setup))
    (use-package flycheck-gherkin
      :config
      (add-hook 'flycheck-mode-hook #'flycheck-gherkin-setup))
    (add-hook 'flycheck-mode-hook #'my/flycheck-local-config)
    (with-eval-after-load 'php-mode
      (use-package flycheck-phpstan :ensure t))))

(global-set-key "\C-ce" 'my-flymake-show-err)

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends
        '(company-nxml
          company-css
          company-eclim
          company-semantic
          company-cmake
          company-capf
          company-files
          company-dabbrev-code
          company-gtags
          company-etags
          company-keywords
          company-dabbrev
          company-ispell)))

(use-package phpactor
  :ensure t
  :hook (php-mode . (lambda ()
                      (make-local-variable 'eldoc-documentation-function)
                      (setq eldoc-documentation-function
                            'phpactor-hover)))
  :init
  (progn
    (use-package smart-jump :ensure t)
    (with-eval-after-load 'php-mode (phpactor-smart-jump-register))))
  
(use-package company-phpactor
  :ensure t
  :hook
  (php-mode-hook . (lambda ()
                     (add-to-list 'company-backends 'company-phpactor))))

;;(use-package php-cs-fixer
;;  :ensure t
;;  :init
;;  (remove-hook 'before-save-hook 'php-cs-fixer-before-save))
(use-package phpcbf
  :ensure t
  :init
  (add-hook 'php-mode-hook 'phpcbf-enable-on-save))
  
(with-eval-after-load 'transient
  (define-transient-command php-transient-menu ()
    "Php"
    [["Class"
      ("cc" "Copy" phpactor-copy-class)
      ("cn" "New" phpactor-create-new-class)
      ("cr" "Move" phpactor-move-class)
      ("ci" "Inflect" phpactor-inflect-class)
      ("n"  "Namespace" phpactor-fix-namespace)]
     ["Properties"
      ("a"  "Accessor" phpactor-generate-accessors)
      ("pc" "Constructor" phpactor-complete-constructor)
      ("pm" "Add missing props" phpactor-complete-properties)
      ("r" "Rename var locally" phpactor-rename-variable-local)
      ("R" "Rename var in file" phpactor-rename-variable-file)]
     ["Extract"
      ("ec" "constant" phpactor-extract-constant)
      ("ee" "expression" phpactor-extract-expression)
      ("em"  "method" phpactor-extract-method)]
     ["Methods"
      ("i" "Implement Contracts" phpactor-implement-contracts)
      ("m"  "Generate method" phpactor-generate-method)]
     ["Navigate"
      ("x" "List refs" phpactor-list-references)
      ("X" "Replace refs" phpactor-replace-references)
      ("."  "Goto def" phpactor-goto-definition)]
     ["Phpactor"
      ("s" "Status" phpactor-status)
      ("u" "Install" phpactor-install-or-update)]])
  )

(defun js--proper-indentation-custom (parse-status)
  "Return the proper indentation for the current line according to PARSE-STATUS argument."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--ctrl-statement-indentation))
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p))
                 (original-point (point))
                 (open-symbol (nth 1 parse-status)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (skip-syntax-backward " ")
             (when (eq (char-before) ?\)) (backward-list))
             (back-to-indentation)
             (js--maybe-goto-declaration-keyword-end parse-status)
             (let* ((in-switch-p (unless same-indent-p
                                   (looking-at "\\_<switch\\_>")))
                    (same-indent-p (or same-indent-p
                                       (and switch-keyword-p
                                            in-switch-p)))
                    (indent
                     (cond (same-indent-p
                            (current-column))
                           (continued-expr-p
                            (goto-char original-point)
                            ;; Go to beginning line of continued expression.
                            (while (js--continued-expression-p)
                              (forward-line -1))
                            ;; Go to the open symbol if it appears later.
                            (when (> open-symbol (point))
                              (goto-char open-symbol))
                            (back-to-indentation)
                            (+ (current-column)
                               js-indent-level
                               js-expr-indent-offset))
                           (t
                            (+ (current-column) js-indent-level
                               (pcase (char-after (nth 1 parse-status))
                                 (?\( js-paren-indent-offset)
                                 (?\[ js-square-indent-offset)
                                 (?\{ js-curly-indent-offset)))))))
               (if in-switch-p
                   (+ indent js-switch-indent-offset)
                 indent))))
          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))

(advice-add 'js--proper-indentation :override 'js--proper-indentation-custom)

(setq js-switch-indent-offset 2)

(defun custom/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)

;; open .h files in c++ mode instead of c mode
(use-package c++-mode :mode "\\.h\\'")

(use-package web-mode
  :mode (("\\.tpl.php\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.html.twig\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.ctp\\'" . web-mode))
  :config
  (progn
    (setq web-mode-engines-alist '(("php"    . "\\.phtml\\'")))
    (setq web-mode-engines-alist '(("mustache"    . "\\.mustache\\'")))))

(use-package sh-mode
  :mode (("PKGBUILD\\'" . sh-mode)
         ("\\.env\\'" . sh-mode)
         ("\\.env\\.local\\'" . sh-mode)))

(use-package magit :ensure t)
(use-package password-store :ensure t)
(use-package helm-pass :ensure t)
(use-package pass :ensure t)
(use-package yaml-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package psysh :ensure t)

(use-package eslint-fix
  :ensure t
  :init
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))

(use-package groovy-mode :ensure t)

(use-package restclient :ensure t :mode "\\.rest\\'")
(use-package feature-mode :ensure t)

(provide 'load-modes)
;;; load-modes.el ends here
