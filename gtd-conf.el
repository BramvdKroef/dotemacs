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

; Load remember
;;(require 'remember)
;;(require 'remember-planner)
;;(setq remember-handler-functions '(remember-planner-append))
;;(setq remember-annotation-functions planner-annotation-functions)

; Initialize bbdb
(require 'bbdb)
;(require 'planner-bbdb)
(bbdb-initialize)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb/news-auto-create-p t)
(add-to-list 'hippie-expand-try-functions-list 'bbdb-complete-name t)

; Load publish and timeclock
;(require 'planner-publish)
;(load "~/.emacs.d/site-lisp/planner/contrib/schedule.el")
;(require 'planner-timeclock)
;(require 'planner-timeclock-summary)

;(require 'planner-erc)

;; Clock out on lunch break
;; (add-hook 'lunch-break-start-hook
;; 	  '(lambda ()
;; 	     (unless (equal (downcase (car timeclock-last-event)) "o")
;; 	       (timeclock-out)
;; 	       )))

;; Show gnus
(add-hook 'lunch-break-stop-hook
	  '(lambda ()
	     (gnus)
	     ))

;(muse-derive-style "inline-xhtml" "xhtml"
;                   :header ""
;                   :footer ""
;                   )

;; Add latex style quotes
;(add-to-list 'muse-publish-markup-regexps '(3600 "``\(.+\)''" 0 quotedstring))
;(add-to-list 'muse-publish-markup-functions '(quotedstring . muse-publish-markup-quotedstring))
;(add-to-list 'muse-xhtml-markup-strings '(quotedstring . "&ldquo;%s&rdquo;"))

;(defun muse-publish-markup-quotedstring ()
;  (message (match-string 1))
;  (unless (get-text-property (match-beginning 0) 'muse-link)
;    (let ((text (match-string 1)))
;      (delete-region (match-beginning 0) (match-end 0))
;      (muse-insert-markup (muse-markup-text 'quotedstring text)))))

(setq org-agenda-files
      '("~/Dropbox/org/work.org"
        "~/Dropbox/org/personal.org"
        "~/Dropbox/org/bookmarks.org")
      org-clock-persist 'history
      org-agenda-custom-commands
      '(("d" todo "DELEGATED" nil))
      org-todo-keywords
      '((sequence "TODO" "|" "DONE" "DEFERRED"))
      org-log-done 'time
      org-return-follows-link t
      org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file))
      )
(org-clock-persistence-insinuate)
;;  (setq
;;   org-agenda-ndays 7
;;   org-deadline-warning-days 14
;;   org-agenda-show-all-dates t
;;  org-agenda-skip-deadline-if-done t
;;  org-agenda-skip-scheduled-if-done t
;;  org-agenda-start-on-weekday nil
;;  org-reverse-note-order t
;;  org-fast-tag-selection-single-key (quote expert)
;;  org-agenda-custom-commands
;;    '(("d" todo "DELEGATED" nil)
;;      ("c" todo "DONE|DEFERRED|CANCELLED" nil)
;;      ("w" todo "WAITING" nil)
;;      ("W" agenda "" ((org-agenda-ndays 21)))

(require 'remember)
(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory "/bookmarks.org"))
;; turn on fly-spell mode in org-mode
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'lunch-break-start-hook
	  '(lambda ()
             (if (org-clock-is-active)
                 (org-clock-out)
	       )))

(defvar my-org-lessc-compiler
  "lessc")
(defun my-org-lessc-compile (plist filename pub-dir)
  "Compile less files to css files"
  (call-process my-org-lessc-compiler nil nil nil filename
                (concat pub-dir (file-name-nondirectory
                                 (file-name-sans-extension filename)) ".css"))
     )
