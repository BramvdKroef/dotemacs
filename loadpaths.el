(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/w3m")
(if window-system
    (require 'w3m-load))

(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme")
(add-to-list 'load-path "~/.emacs.d/site-lisp/planner")
(add-to-list 'load-path "~/.emacs.d/site-lisp/remember")
(add-to-list 'load-path "~/.emacs.d/site-lisp/bbdb/lisp")

(add-to-list 'load-path "/usr/share/emacs/site-lisp/epg")
(add-to-list 'load-path "~/.emacs.d/site-lisp/epg")

(add-to-list 'load-path "~/.emacs.d/site-lisp/emms/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet")
(add-to-list 'load-path "~/.emacs.d/site-lisp/nxhtml")
(add-to-list 'load-path "~/.emacs.d/site-lisp/nxml")

(add-to-list 'load-path "~/.emacs.d/site-lisp/tidy")
(setq tidy-config-file "~/.tidy"
      tidy-temp-dir "/tmp"
      tidy-shell-command "/Users/bram/.emacs.d/site-lisp/tidy/tidy")
(add-to-list 'load-path "~/.emacs.d/site-lisp/dictionary")

(add-to-list 'load-path "~/.emacs.d/site-lisp/jabber")

(add-to-list 'load-path "~/.emacs.d/site-lisp/predictive")

(add-to-list 'load-path "~/.emacs.d/site-lisp/erc-extras")

(add-to-list 'load-path "~/.emacs.d/site-lisp/quenya")

(add-to-list 'load-path "~/.emacs.d/site-lisp/mmm-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/muse/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/w3m")
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/php-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/twittering-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/bitlbee/lisp")
