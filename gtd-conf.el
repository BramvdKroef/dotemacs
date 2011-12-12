;; /file gtd-conf.el   Setup planner, remember, bbdb, and the hooks into other modes
;;   
;; $Id: gtd-conf.el,v 1.9 2010-03-01 14:21:12 bram Exp $

; Muse major mode
;(require 'muse-mode)     ; load authoring mode
;(require 'muse-html)     ; load publishing styles I use
;(require 'muse-latex)
;(require 'muse-texinfo)
;(require 'muse-docbook)
;(require 'muse-project)  ; publish files in projects

; Planner mode
;(require 'planner)
;(setq planner-project "WikiPlanner")
;(setq muse-project-alist
;      '(("WikiPlanner" 
;         ("~/.emacs.d/Plans" ;; where your Planner pages are located 
;          :default "TaskPool" ;; use value of `planner-default-page' 
 ;         :major-mode planner-mode :visit-link planner-visit-link) 

          ;; This next part is for specifying where Planner pages 
          ;; should be published and what Muse publishing style to 
          ;; use. In this example, we will use the XHTML publishing 
          ;; style. 

;          (:base "planner-xhtml" 
          ;; where files are published to 
          ;; (the value of `planner-publishing-directory', if 
          ;; you have a configuration for an older version 
          ;; of Planner) 
;           :path "~/.emacs.d/Plans/public_html"))
;	("Blog"
;	 ("~/.emacs.d/Blog"
;	  :default "index")
;	 (:base "html" :path "~/Blog/public_html"))
;	))

;(setq planner-use-other-window nil)

;; Load gnus hooks (has to happen before loading remember or you can't remember emails
;(require 'planner-gnus)
;(planner-gnus-insinuate)


; Initialize bbdb
(require 'bbdb)

(bbdb-initialize)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb/news-auto-create-p t)
(add-to-list 'hippie-expand-try-functions-list 'bbdb-complete-name t)

;; Show gnus
(add-hook 'lunch-break-stop-hook
	  '(lambda () (gnus)))


(require 'org-clock)
(require 'org-agenda)
(require 'remember)

(org-clock-persistence-insinuate)
(org-remember-insinuate)
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


(defun my-to-work-agenda ()
  (interactive)
  (find-file (concat org-directory "/work.org")))

;; turn on fly-spell mode in org-mode
(add-hook 'org-mode-hook 'flyspell-mode)

(add-hook 'lunch-break-start-hook
	  '(lambda ()
             (if (org-clock-is-active)
                 (org-clock-out)
	       )))

