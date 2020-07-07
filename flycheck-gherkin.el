;;; flycheck-gherkin -- Gherkin checker for flycheck
;;; Commentary:
;;;
;;; Support for checking gherkin files with gherkin-lint
;;;
;;; See URL `https://github.com/vsiakka/gherkin-lint/'
;;;
;;; Code:

(require 'flycheck)

(defgroup flycheck-gherkin nil
  "gherkin mode flycheck checker."
  :group 'flycheck)

(defcustom flycheck-gherkin-config nil
  "Path to the gherkin-lint config."
  :type 'string
  :group 'flycheck-gherkin)

(flycheck-def-config-file-var flycheck-gherkin-lintrc
    (gherkin) nil
  :safe #'stringp)

(defun flycheck-parse-gherkin-lint (output checker buffer)
  "Parse gherkin errors/warnings from JSON OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively."
  (apply #'append
         (mapcar
          (lambda (file-errors)
            (let-alist (car file-errors)
              (mapcar (lambda (errors)
                        (let-alist errors
                          (flycheck-error-new-at
                           .line
                           1
                           'warning
                           .message
                           :id .rule
                           :checker checker
                           :buffer buffer
                           :filename (buffer-file-name buffer))
                          )) .errors)))
          (flycheck-parse-json output))))
  
(flycheck-define-checker gherkin
  "A gherkin syntax checker."
  :command ("gherkin-lint"
            "-f" "json"
            (config-file "--config" flycheck-gherkin-lintrc)
            source)
  :standard-input nil
  :error-parser flycheck-parse-gherkin-lint
  :modes (feature-mode))

;;;###autoload
(defun flycheck-gherkin-setup ()
  "Setup Flycheck Gherkin.
Add `gherkin' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'gherkin))

(provide 'flycheck-gherkin)
;;; flycheck-gherkin.el ends here
