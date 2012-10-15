;; open .h files in c++ mode instead of c mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; open drupal modules in php mode
(add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

;; cakephp views
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . html-mode))

(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.conkerorrc\\'" . javascript-mode))

(add-to-list 'auto-mode-alist '("\\.stumpwmrc\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . sh-mode))
