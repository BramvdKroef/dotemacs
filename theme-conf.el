
;; Make transparent
(set-frame-parameter nil 'alpha '(85 85))

;; global-font-lock-mode t nil 'font-lock

;; Turn off noob gui parts
(scroll-bar-mode nil)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-message t) 
(setq inhibit-startup-screen t)

;; set parenthesis matching to highlighting instead of cursor jumping
(show-paren-mode 1)
(mouse-avoidance-mode 'animate)
(setq transient-mark-mode t)

(setq fill-column 80)

(defun color-theme-bram ()
  "Dark Color theme by Bram."
  (interactive)
  (color-theme-install
   '(color-theme-bram
     ((background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "yellow")
      (foreground-color . "gray45")
      (mouse-color . "white"))
     
     ((align-highlight-change-face . highlight)
      (align-highlight-nochange-face . secondary-selection)
      ;; apropos
      (apropos-keybinding-face . underline)
      (apropos-label-face . italic)
      (apropos-match-face . secondary-selection)
      (apropos-property-face . bold-italic)
      (apropos-symbol-face . bold)
      ;; ebnf
      (ebnf-except-border-color . "Black")
      (ebnf-line-color . "Black")
      (ebnf-non-terminal-border-color . "Black")
      (ebnf-repeat-border-color . "Black")
      (ebnf-special-border-color . "Black")
      (ebnf-terminal-border-color . "Black")
      ;; gnus fonts
      (gnus-article-button-face . bold)
      (gnus-article-mouse-face . highlight)
      (gnus-carpal-button-face . bold)
      (gnus-carpal-header-face . bold-italic)
      (gnus-cite-attribution-face . gnus-cite-attribution-face)
      (gnus-mouse-face . highlight)
      (gnus-selected-tree-face . modeline)
      (gnus-signature-face . gnus-signature-face)
      (gnus-summary-selected-face . gnus-summary-selected-face)
      (help-highlight-face . underline)
      (list-matching-lines-face . bold)
      (ps-line-number-color . "black")
      (ps-zebra-color . 0.95)
      (tags-tag-face . default)
      (vc-annotate-very-old-color . "#0046FF")
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     
     (default ((t (:stipple nil :background "black" :foreground "#9367ee"
                            :inverse-video nil :box nil :strike-through nil
                            :overline nil :underline nil :slant normal :weight
                            normal :width normal
                            :height 140
                            :family "Inconsolata-g"
                            :embolden t))))
     
     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (border ((t (:background "black"))))
     (italic ((t (:italic t :slant italic))))
     (underline ((t (:underline t))))
     (highlight ((t (:background "darkolivegreen"))))
     
     (cursor ((t (:background "steel blue"))))
     (fixed-pitch ((t (:family "Inconsolata-g"))))
     (variable-pitch ((t (:family "helv"))))
     (fringe ((t (:family "outline-courier new" :width normal :weight
                          normal :slant normal :underline nil
                          :overline nil :strike-through 
                          nil :box nil :inverse-video nil :stipple nil
                          :background "grey4" :foreground "Wheat"))))
     (header-line ((t (:family "Arial" :background "grey20"
                               :foreground "grey75"
                               :box (:line-width 3 :color "grey20"
                                                 :style released-button)
                               :height 0.9))))
     (menu ((t (nil))))

     (minibuffer-prompt ((t (:foreground "royalblue1"))))
     
     ;; mode line
     (mode-line ((t (:background "LightSteelBlue4" 
                                 :height 130
                                 :family "Helvetica"
                                 :foreground "white"))))
     (modeline-mousable-minor-mode
      ((t (:background "grey" :foreground "black" :box
                       (:line-width 2 :color "grey" :style released-button) 
                       :height 0.9 :family "Arial")))) 
     (modeline-mousable ((t (:background "grey" :foreground "black" :box (:line-width 2 :color "grey" :style released-button) :height 0.9 :family "Arial"))))
     (modeline-buffer-id ((t (:background "grey" :foreground "black" :box nil :height 0.9 :family "Arial"))))
     (mouse ((t (:background "white"))))
     (primary-selection ((t (:background "DarkSlateGray"))))
     (region ((t (:background "DarkSlateGray"))))
     (secondary-selection ((t (:background "SkyBlue4"))))
     
     (scroll-bar ((t (nil))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
     (trailing-whitespace ((t (:background "white"))))
     
     ;; info
     (Info-title-1-face ((t (:bold t :weight bold :family "helv" :height 1.728))))
     (Info-title-2-face ((t (:bold t :family "helv" :weight bold :height 1.44))))
     (Info-title-3-face ((t (:bold t :weight bold :family "helv" :height 1.2))))
     (Info-title-4-face ((t (:bold t :family "helv" :weight bold))))

     ;; bbdb
     (bbdb-company ((t (:italic t :slant italic))))
     (bbdb-field-name ((t (:bold t :weight bold))))
     (bbdb-field-value ((t (nil))))
     (bbdb-name ((t (:underline t))))

     ;; change log
     (change-log-acknowledgement-face ((t (:italic t :slant oblique :foreground "AntiqueWhite3"))))
     (change-log-conditionals-face ((t (:foreground "Aquamarine"))))
     (change-log-date-face ((t (:italic t :slant oblique :foreground "BurlyWood"))))
     (change-log-email-face ((t (:foreground "Aquamarine"))))
     (change-log-file-face ((t (:bold t :family "Verdana" :weight bold :foreground "LightSkyBlue" :height 0.9))))
     (change-log-function-face ((t (:foreground "Aquamarine"))))
     (change-log-list-face ((t (:foreground "LightSkyBlue"))))
     (change-log-name-face ((t (:bold t :weight bold :foreground "Gold"))))
     (clear-case-mode-string-face ((t (:bold t :family "Arial" :box (:line-width 2 :color "grey" :style released-button) :foreground "black" :background "grey" :weight bold :height 0.9))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "cyan"))))

     ;; customize
     (custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))
     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))
     (custom-changed-face ((t (:background "blue" :foreground "white"))))
     (custom-comment-face ((t (:background "pale green"))))
     (custom-comment-tag-face ((t (:foreground "sea green"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.1))))
     (custom-group-tag-face ((t (:bold t :family "helv" :foreground "light blue" :weight bold :height 1.1))))
     (custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "pink" :weight bold :height 1.1))))
     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))
     (custom-modified-face ((t (:background "blue" :foreground "white"))))
     (custom-rogue-face ((t (:background "black" :foreground "pink"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:background "white" :foreground "blue"))))
     (custom-state-face ((t (:foreground "lime green"))))
     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))
     (custom-variable-tag-face ((t (:bold t :family "helv" :foreground "light blue" :weight bold :height 1.2))))

     ;; diff
     ;;(diff-added-face ((t (nil))))
     ;;(diff-changed-face ((t (nil))))
     ;;(diff-removed-face ((t (nil))))
     (diff-context-face ((t (:foreground "grey70"))))
     (diff-file-header-face ((t (:bold t :background "grey60" :weight bold))))
     (diff-function-face ((t (:foreground "grey70"))))
     (diff-header-face ((t (:background "grey45"))))
     (diff-hunk-header-face ((t (:background "grey45"))))
     (diff-index-face ((t (:bold t :weight bold :background "grey60"))))
     (diff-nonexistent-face ((t (:bold t :weight bold :background "grey60"))))
     
     
     (diff-added ((t (:foreground "seagreen"))))
     (diff-changed ((t (:foreground "yellow"))))
     (diff-removed ((t (:foreground "indianred2"))))
     
     ;; font-lock
     (font-lock-builtin-face ((t (:foreground "SteelBlue"))))
     (font-lock-comment-face ((t (:italic t :foreground "#3c8249" :slant oblique))))
     (font-lock-constant-face ((t (:bold t :foreground "Gold" :weight bold))))
     (font-lock-doc-face ((t (:italic t :slant oblique :foreground "BurlyWood"))))
     (font-lock-doc-string-face ((t (:italic t :slant oblique :foreground "BurlyWood"))))
     (font-lock-function-name-face ((t (:bold t :foreground "#0e6fd3" :weight bold))))
     (font-lock-keyword-face ((t (:foreground "LightSkyBlue"))))
     (font-lock-preprocessor-face ((t (:bold t :foreground "Gold" :weight bold))))
     (font-lock-reference-face ((t (:foreground "SteelBlue"))))
     (font-lock-string-face ((t (:italic t :foreground "#669645" :slant oblique))))
     (font-lock-type-face ((t (:bold t :foreground "PaleGreen" :weight bold))))
     (font-lock-variable-name-face ((t (:foreground "#1a93c0"))))
     (font-lock-warning-face ((t (:bold t :foreground "chocolate" :weight bold))))

     ;; gnus
     (gnus-cite-attribution-face ((t (:italic t :slant italic))))
     (gnus-cite-face-1 ((t (:foreground "light blue"))))
     (gnus-cite-face-10 ((t (:foreground "medium purple"))))
     (gnus-cite-face-11 ((t (:foreground "turquoise"))))
     (gnus-cite-face-2 ((t (:foreground "light cyan"))))
     (gnus-cite-face-3 ((t (:foreground "light yellow"))))
     (gnus-cite-face-4 ((t (:foreground "light pink"))))
     (gnus-cite-face-5 ((t (:foreground "pale green"))))
     (gnus-cite-face-6 ((t (:foreground "beige"))))
     (gnus-cite-face-7 ((t (:foreground "orange"))))
     (gnus-cite-face-8 ((t (:foreground "magenta"))))
     (gnus-cite-face-9 ((t (:foreground "violet"))))
     (gnus-emphasis-bold ((t (:bold t :weight bold))))
     (gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))
     (gnus-emphasis-italic ((t (:italic t :slant italic))))
     (gnus-emphasis-underline ((t (:underline t))))
     (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold))))
     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold))))
     (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))
     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))
     (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1" :weight bold))))
     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))
     (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2" :weight bold))))
     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))
     (gnus-group-mail-3-face ((t (:bold t :foreground "aquamarine3" :weight bold))))
     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))
     (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4" :weight bold))))
     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))
     (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise" :weight bold))))
     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))
     (gnus-group-news-2-face ((t (:bold t :foreground "turquoise" :weight bold))))
     (gnus-group-news-3-empty-face ((t (nil))))
     (gnus-group-news-3-face ((t (:bold t :weight bold))))
     (gnus-group-news-4-empty-face ((t (nil))))
     (gnus-group-news-4-face ((t (:bold t :weight bold))))
     (gnus-group-news-5-empty-face ((t (nil))))
     (gnus-group-news-5-face ((t (:bold t :weight bold))))
     (gnus-group-news-6-empty-face ((t (nil))))
     (gnus-group-news-6-face ((t (:bold t :weight bold))))
     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))
     (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise" :weight bold))))
     (gnus-header-content-face ((t (:italic t :foreground "forest green" :slant italic))))
     (gnus-header-from-face ((t (:foreground "spring green"))))
     (gnus-header-name-face ((t (:foreground "SeaGreen"))))
     (gnus-header-newsgroups-face ((t (:italic t :foreground "yellow" :slant italic))))
     (gnus-header-subject-face ((t (:foreground "SeaGreen3"))))
     (gnus-signature-face ((t (:italic t :slant italic))))
     (gnus-splash-face ((t (:foreground "Brown"))))
     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
     (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue" :weight bold))))
     (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen" :weight bold))))
     (gnus-summary-high-ticked-face ((t (:bold t :foreground "pink" :weight bold))))
     (gnus-summary-high-unread-face ((t (:bold t :weight bold))))
     (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue" :slant italic))))
     (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen" :slant italic))))
     (gnus-summary-low-ticked-face ((t (:italic t :foreground "pink" :slant italic))))
     (gnus-summary-low-unread-face ((t (:italic t :slant italic))))
     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))
     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))
     (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))
     (gnus-summary-normal-unread-face ((t (nil))))
     (gnus-summary-selected-face ((t (:underline t))))

     ;; info
     (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "white"))))
     (info-header-xref ((t (:bold t :weight bold :foreground "cyan"))))
     (info-menu-5 ((t (:foreground "red1"))))
     (info-menu-header ((t (:bold t :family "helv" :weight bold))))
     (info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))
     (info-xref ((t (:bold t :foreground "cyan" :weight bold))))

     ;; isearch
     (isearch ((t (:background "darkslateblue"))))
     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     ;; makefile
     (makefile-space-face ((t (:background "hotpink"))))

     ;; message
     (message-cited-text-face ((t (:foreground "red"))))
     (message-header-cc-face ((t (:bold t :foreground "green4" :weight bold))))
     (message-header-name-face ((t (:foreground "DarkGreen"))))
     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow" :slant italic :weight bold))))
     (message-header-other-face ((t (:foreground "#b00000"))))
     (message-header-subject-face ((t (:foreground "green3"))))
     (message-header-to-face ((t (:bold t :foreground "green2" :weight bold))))
     (message-header-xheader-face ((t (:foreground "blue"))))
     (message-mml-face ((t (:foreground "ForestGreen"))))
     (message-separator-face ((t (:foreground "blue3"))))

     ;; org
     (org-todo ((t (:foreground "indianred2" :bold t))))
     (org-done ((t (:foreground "lightgreen" :bold t))))
     ;; yas
     (yas/field-highlight-face ((t (:background "DarkSlateGrey"))))
     ;; flymake
     (flymake-errline ((t (:background "indianred2"))))
     ;; widget
     (widget-button-face ((t (:bold t :weight bold))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "lime green"))))
     (widget-field-face ((t (:background "gray15"))))
     (widget-inactive-face ((t (:foreground "light gray"))))
     (widget-single-line-field-face ((t (:background "dim gray"))))
     (zmacs-region ((t (:background "DarkSlateGray")))))))


;; test a face
;;(custom-set-faces '(foreground-color ((t (:foreground "dimgray")))))

;;(set-foreground-color "gray45")
;; (custom-set-faces '(default ((t (:stipple nil :background "black" :foreground "#9367ee"
;;                             :inverse-video nil :box nil :strike-through nil
;;                             :overline nil :underline nil :slant normal :weight
;;                             normal :width normal
;;                             :family "Inconsolata-g-12"
;;                             :embolden t)))))

 ;(mode-line ((t (:background "LightSteelBlue4" 
;                                               :height 130
;                                               :family "helv"
;                                               :foreground "white")))))

;; Set color theme 
(if (require 'color-theme nil t)
    (progn (color-theme-initialize)
	   ;;(color-theme-midnight)
	   (color-theme-bram)
	   ;;(color-theme-taylor)
	   ;;(color-theme-charcoal-black)
	   ;;(color-theme-late-night)
	   ;;(color-theme-comidia)
	   )
  ;; if color-theme isn't found, manually set colors and faces
  (set-foreground-color "#aea3ff")
  (set-background-color "black")
  (set-face-background 'region "blue"))

(set-frame-font "Inconsolata-g-12")