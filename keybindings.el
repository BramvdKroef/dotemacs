;; set C-x C-m to M-x, and also set C-c C-m in case of a typo
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; remap C-w to kill word backwards
(global-set-key "\C-w" 'backward-kill-word)
;; remap kill region to C-x C-k (with C-c variant)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
;; remap C-s/r to regexp search so they are easier to reach
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-cr" 'query-replace)

;; C-f = forward-char, M-f = forward-word, C-n = forward line, M-s = ...
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)

;set C-space to hippy expand
(global-set-key [(control ? )] 'hippie-expand)
;remap set mark to C-ret
(global-set-key [(control return)] 'set-mark-command)

(add-hook 'latex-mode-hook
	  (lambda () (local-set-key [(control return)] 'set-mark-command)))
(add-hook 'html-mode-hook
	  (lambda () (local-set-key [(control return)] 'set-mark-command)))

(global-set-key "\M-g" 'goto-line)

;;(require 'cc-mode)
(eval-after-load "cc-mode"
  '(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
)
(define-key global-map (kbd "RET") 'newline-and-indent)

;when a paren or brace or bracket is typed add the closing match

(require 'skeleton)
(setq skeleton-pair t)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)

;typing % goes to the matching paren
(global-set-key "\C-cp" 'match-paren)
;match paren function
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;f5 for playing the last recorded macro
(global-set-key [f5] 'call-last-kbd-macro)

;f4 for ispell
(global-set-key (kbd "<f4>") 'ispell)

(defun my-gnus-get-news ()
  (interactive)
  (if (commandp 'gnus-group-get-new-news)
      (progn (gnus-group-get-new-news 3)
	     (switch-to-buffer "*Group*")
	     )
    (gnus))
  )

;; Global keys
(global-set-key (kbd "C-1") 'my-to-main-agenda)
(global-set-key (kbd "C-2") 'gnus)
(global-set-key (kbd "C-3") 'emms-smart-browse)
(global-set-key (kbd "C-4") 'twit)

;; disable arrow island
(global-set-key (kbd "<left>") 'nil)
(global-set-key (kbd "<right>") 'nil)
(global-set-key (kbd "<up>") 'nil)
(global-set-key (kbd "<down>") 'nil)
;;(global-set-key (kbd "TAB") 'indent-according-to-mode)

(load "moveline.el")
(global-set-key [(meta up)] 'move-line-up)
(global-set-key [(meta down)] 'move-line-down)

(global-set-key "\C-z" nil)

;; volume keys
(global-set-key "\C-xv1" 'volume-set-normal)
;; set the volume to loud
(global-set-key "\C-xv2" 'volume-set-loud)
;; set the volume to quiet
(global-set-key "\C-xv3" 'volume-set-quiet)

;; Emms keybindings
(global-set-key "\C-xpp" 'emms-pause)
(global-set-key "\C-xpf" 'emms-next)
(global-set-key "\C-xpb" 'emms-previous)
(global-set-key "\C-xps" 'emms-stop)
(global-set-key "\C-xpl" 'emms-smart-browse)

(setq yas/trigger-key (kbd "<f11>"))

(global-set-key "\C-x\C-r" 'conkeror-reload)

;;; Disable text resize key that I keep hitting by accident
(global-set-key (kbd "C-x C--") 'nil)

;; have M-. use word-at-point for the tag instead of asking for it
(global-set-key "\M-." 'etags-select-find-tag-at-point)
 
;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-c\C-r" 'org-remember)

(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key "\C-c\C-x\C-a" 'org-advertized-archive-subtree)
            (local-set-key [(control return)] 'set-mark-command)
	    )
	  )
(global-set-key "\C-c\C-q" 'quick-calc)
(global-set-key "\C-x\C-c" 'my-kill-emacs)

(global-set-key "\C-ce" 'my-flymake-show-err)