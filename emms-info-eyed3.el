;;; emms-info-mp3info.el --- Info-method for EMMS using mp3info

;; Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

;; Authors: Ulrik Jensen <terryp@daimi.au.dk>
;;          Jorgen Schäfer <forcer@forcix.cx>
;; Keywords:

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code is a modified version of emms-info-mp3info.el which
;; has been adapted from code found in mp3player.el.
;; This supports ID3v2 tags using the eyeD3 tool. 

;; To activate this method for getting info, use something like:

;; (require 'emms-info-mp3info)
;; (add-to-list 'emms-info-functions 'emms-info-mp3info)

;;; Code:

(require 'emms-info)

(defvar emms-info-eyed3-version "0.2 $Revision: 1.1 $"
  "EMMS info eyed3 version string.")
;; $Id: emms-info-eyed3.el,v 1.1 2009-01-28 14:25:36 twd Exp $

(defgroup emms-info-eyed3 nil
  "An EMMS-info method for getting/setting ID3 tags, using the
external eyed3 program"
  :group 'emms-info)

(defcustom emms-info-eyed3-coding-system 'utf-8
  "*Coding system used in the output of eyed3."
  :type 'coding-system
  :group 'emms-info-eyed3)

(defcustom emms-info-eyed3-program-name "eyeD3"
  "*The name/path of the eyed3 tag program."
  :type 'string
  :group 'emms-info-eyed3)

(defcustom emms-info-eyed3-arguments
  `("--no-color")
  "The argument to pass to `emms-info-eyed3-program-name'.
This should be a list of info-flag=value lines."
  :type '(repeat string)
  :group 'emms-info-eyed3)

(defcustom emms-info-eyed3-names
  '((title . info-title)
    (artist . info-artist)
    (album . info-album)
    (track . info-tracknumber)
    (year . info-year)
    (Time . info-playing-time))
    "A map that translates eyeD3 terms to emms term (e.g. title = info-title)"
    :type 'alist
    :group 'emms-info-eyed3)

(defun emms-info-eyed3 (track)
  "Add track information to TRACK.
This is a useful element for `emms-info-functions'."
  (when (and (eq 'file (emms-track-type track))
             (string-match "\\.[Mm][Pp]3\\'" (emms-track-name track)))
    (with-temp-buffer
      (when (zerop
             (apply (if (fboundp 'emms-i18n-call-process-simple)
                        'emms-i18n-call-process-simple
                      'call-process)
                    emms-info-eyed3-program-name
                    nil t nil
                    (append emms-info-eyed3-arguments
                            (list (emms-track-name track)))))
        (goto-char (point-min))
	(while (re-search-forward "\\([^:\n\t]+\\): \\([^\t\n]+\\)" nil 9)
	  (let ((name (cdr (assoc (intern (match-string 1)) emms-info-eyed3-names)))
		(value (match-string 2)))
	    (unless (or (eq name nil) (string-equal value "None"))
	      (emms-track-set track 
			      name
			      (if (eq name 'info-playing-time)
				  ; convert min:sec to seconds
				  (if (string-match "\\([0-9]+\\):\\([0-9]+\\)" value)
				      (+ (* (string-to-number (match-string 1 value)) 60)
					 (string-to-number (match-string 2 value)))
				    (string-to-number value)
				      )
				(if (eq name 'info-tracknumber)
				    ; convert "1/12" to "1" (has to be a string)
				    (number-to-string (string-to-number value))
				  value))
			      )
	      )
	    )
	  )
	))))

(provide 'emms-info-eyed3)
;;; emms-info-eyed3.el ends here
