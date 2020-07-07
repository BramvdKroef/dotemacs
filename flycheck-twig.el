;;; flycheck-twig -- Twig checker for flycheck
;;; Commentary:
;;;
;;; Support for checking twig files with twigcs
;;;
;;; See URL `https://github.com/friendsoftwig/twigcs'
;;;
;;; Code:

(require 'flycheck)

(flycheck-define-checker twig-twigcs
  "A twig syntax checker.

See URL `https://github.com/friendsoftwig/twigcs'."
  :command ("twigcs" "--no-interaction" source)
  :standard-input nil
  :error-patterns
  ((info line-start "l." line " c." column " : NOTICE "
         (message)
         line-end)
   (warning line-start "l." line " c." column " : WARNING "
            (message)
            line-end)
   (error (or (seq line-start "l." line " c." column " : ERROR "
                   (message)
                   line-end)
              (seq line-start "In " (file-name) " line " line ":"
                   line-end))))
  :modes (web-mode))

;;;###autoload
(defun flycheck-twig-setup ()
  "Setup Flycheck Twig.
Add `twig' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'twig-twigcs))

(provide 'flycheck-twig)
;;; flycheck-twig.el ends here
