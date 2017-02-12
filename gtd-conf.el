;; /file gtd-conf.el   Setup org-mode, remember, bbdb, and the hooks into other modes
;;   


; Initialize bbdb
(require 'bbdb)
(require 'org)

(bbdb-initialize)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb/news-auto-create-p t)
(add-to-list 'hippie-expand-try-functions-list 'bbdb-complete-name t)

(require 'org-clock)
(require 'org-agenda)
(require 'org-capture)

(org-clock-persistence-insinuate)

(add-hook 'remember-mode-hook 'org-remember-apply-template)
;;(setq org-default-notes-file (concat org-directory
;;"/bookmarks.org"))

(setq org-agenda-files
      (list (concat org-directory "/work.org")
            (concat org-directory "/personal.org")
            (concat org-directory "/bookmarks.org"))
      org-clock-persist 'history
      org-agenda-custom-commands
      '(("d" todo "DELEGATED" nil))
      org-todo-keywords
      '((sequence "TODO" "|" "DONE" "DEFERRED"))
      org-log-done 'time
      org-return-follows-link t
      org-reverse-note-order t
      org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file))
      org-remember-templates
      (list (list "Todo item" ?t "* TODO %?\n  %a" (concat org-directory "/work.org")))
      remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler))

(defvar my-org-main-agenda-file "work.org")
(defun my-to-main-agenda ()
  (interactive)
  (find-file (concat org-directory "/" my-org-main-agenda-file)))

;; turn on fly-spell mode in org-mode
(add-hook 'org-mode-hook 'flyspell-mode)

(add-hook 'lunch-break-start-hook
	  '(lambda ()
             (if (org-clock-is-active)
                 (org-clock-out))))

