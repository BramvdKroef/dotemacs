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

;;remap set mark to C-ret
(global-set-key [(control return)] 'set-mark-command)

(add-hook 'latex-mode-hook
          (lambda () (local-set-key [(control return)] 'set-mark-command)))
(add-hook 'html-mode-hook
          (lambda () (local-set-key [(control return)] 'set-mark-command)))

(global-set-key "\M-g" 'goto-line)

(define-key global-map (kbd "RET") 'newline-and-indent)
(eval-after-load "cc-mode"
  '(define-key c-mode-base-map (kbd "RET") 'newline-and-indent))

;; typing % goes to the matching paren
(global-set-key "\C-cp" 'match-paren)
;; match paren function
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; disable arrow island
(global-set-key (kbd "<left>") 'nil)
(global-set-key (kbd "<right>") 'nil)
(global-set-key (kbd "<up>") 'nil)
(global-set-key (kbd "<down>") 'nil)

;;; Disable text resize key that I keep hitting by accident
(global-set-key (kbd "C-x C--") 'nil)
;;; Also unset control z
(global-set-key "\C-z" nil)

;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-c\C-r" 'org-remember)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-x\C-a" 'org-advertized-archive-subtree)
            (local-set-key [(control return)] 'set-mark-command)))

(global-set-key "\C-c\C-q" 'quick-calc)
(global-set-key "\C-x\C-c" 'kill-emacs)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key "\C-c\C-c" 'recompile)
(global-set-key "\C-xc" 'recompile)
(global-set-key "\C-c\C-n" 'cleanup-buffer)

(add-hook 'php-mode-hook
          (lambda () (local-set-key "\C-c\C-y" 'yas/create-php-snippet)))

