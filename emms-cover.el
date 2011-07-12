
(require 'url)
(require 'emms)
(require 'emms-url)

(defgroup emms-cover nil
  "Retrieves cover images for albums in the EMMS browser"
  :prefix "emms-cover-"
  :group 'multimedia
  :group 'applications)

(defcustom emms-cover-pathregexp '("/\\([^/-]*\\) - \\([^/]*\\)/$" 1 2)
  "The regexp to use to extract the artist and album names from the directory"
  :group 'emms-cover
  :type '(list string integer integer))

(defcustom emms-cover-savepath "~/.covers/%artist - %album.%size.%ext"
  "The path to store and find the cover files. %artist, %album, %size and %ext will be replaced."
  :group 'emms-cover
  :type 'string)

(defcustom emms-cover-imageformat '("png" "jpg" "jpeg" "gif")
  "These image extensions will replace %ext in emms-cover-savepath when searching for an image"
  :group 'emms-cover
  :type '(repeat string))

(defcustom emms-cover-download-functions '(emms-cover-download-rhapsody
					   emms-cover-download-lastfm
					   emms-cover-download-dummy)
  "A List of functions that taks an artist and album as argument and return a url to an image cover"
  :group 'emms-cover
  :type '(repeat function))

(defcustom emms-cover-dimensions '((medium . (65 65)) (small . (50 50)))
  "The dimensions for medium and small"
  :group 'emms-cover
  :type '(alist (list integer integer) (list integer integer))
  )
(defcustom emms-cover-converter "convert \"%in\" -resize %widthx%height \"%out\""
  "This command resizes image %in to dimensions %width x %height and writes the result to %out"
  :group 'emms-cover
  :type 'string
  )

(defvar emms-cover-download-queue '() "Download queue")

  ;; Looks for the path to a cover, or downloads the cover.
  ;; For some reason the track object isn't passed as a parameter so
  ;; the artist and album name have to be pulled from the directory
  ;; name with a regexp.
  ;; Then file-name-completion is used to locate the extension.
  ;; For some reason file-name-completion doesn't work with unicode
  ;; characters so an attempt is made to find the file with a jpg
  ;; extension if it returns nil.
  ;; If that doesn't work either, a command line tool is run that
  ;; will download the cover. 
(defun emms-cover-find (path size)
    "Extract artist and album from path and search for cover"
    (let ((clean-path (emms-cover-clean-path path)))
      (if (string-match (car emms-cover-pathregexp) clean-path)
	(let
	    ((artist (match-string (nth 1 emms-cover-pathregexp) clean-path))
	     (album (match-string (nth 2 emms-cover-pathregexp) clean-path)))

          (or (emms-cover-find-existing artist album size)
              (unless (emms-cover-in-queue artist album)
                (message "Downloading Album Cover for %s album %s" artist album)
                (emms-cover-download artist album size)              
                )
              ))
        )))

(defun emms-cover-find-existing (artist album size)
  "Try each extention and test for file existance.  emms-cover-savepath should
  contain '%ext', which is replaced with the allowed extensions. "
  (catch 'emms-cover-find
    (let ((cover emms-cover-savepath)
          attempt-path)
      (setq cover (replace-regexp-in-string "%artist" artist cover))
      (setq cover (replace-regexp-in-string "%album" album cover))
      (setq cover (replace-regexp-in-string "%size" (symbol-name size) cover))

      (dolist (ext emms-cover-imageformat)
        (setq attempt-path (replace-regexp-in-string "%ext" ext cover))
        (if (file-exists-p attempt-path)
            (throw 'emms-cover-find attempt-path))
        )
      nil
      )
    ))

(defun emms-cover-in-queue (artist album)
  (catch 'emms-cover-in-queue
    (dolist (item emms-cover-download-queue)
      (if (and (string= (car item) artist)
               (string= (cdr item) album))
          (throw 'emms-cover-in-queue t)
        ))
  ))

(defun emms-cover-remove-from-queue (artist album)
  (catch 'emms-cover-removed
    (let ((item emms-cover-download-queue)
          (previous))
      (while item
        (if (and (string= (caar item) artist)
                 (string= (cdar item) album))
            (progn
              (if previous
                  (setcdr previous (cdr item))
                (setq emms-cover-download-queue (cdr item)))
              (throw 'emms-cover-removed t)
              )
          (progn
            (setq previous item)
            (setq item (cdr item)))
        ))
    )))

(defun emms-cover-clean-path (path)
  "Replace characters that may not occur in a file name such as ':'"
  (replace-regexp-in-string "[:]" "" path)
  )

(defun emms-cover-download (artist album size)
  (push '(artist . album) emms-cover-download-queue)
  (catch 'emms-cover-download
    (dolist (func emms-cover-download-functions)
      (let ((url (funcall func artist album))
            (coverpath))
	  (when url
              (setq coverpath (emms-cover-download-image url artist album))
              (emms-cover-remove-from-queue artist album)
	      (throw 'emms-cover-download
		     (replace-regexp-in-string "%size" (symbol-name size) coverpath))
	    )
	  ))
      nil
      )
    )

(defun emms-cover-download-rhapsody (artist album)
  (let ((url "http://www.rhapsody.com/%artist/%album/data.xml"))
    (setq url (replace-regexp-in-string "%artist" (emms-url-quote artist) url))
    (setq url (replace-regexp-in-string "%album" (emms-url-quote album) url))
   (let ((url-show-status nil)
	  (url-request-method "GET"))
     (emms-cover-download-rhapsody-parse (url-retrieve-synchronously url))
    )
  ))

(defun emms-cover-download-rhapsody-parse (buffer)
    "Parses the server reponse and calls callback with the url of the cover image."
    (let ((cover nil))
      (with-current-buffer buffer
	(emms-http-decode-buffer buffer)
	(goto-char (point-min))
	;; skip to the first empty line and go one line further.  There the 
	;; response starts.
	(re-search-forward "^$" nil t)
	(forward-line)
	;; an xml parser and xpath would be better but not as easily available in emacs
	(if (re-search-forward
	     "<album-art[^>]*size=\"large\"[^>]*>[[:space:]]*<img[^>]+src=\"\\([^\"]*\\)\"[^>]*>[[:space:]]*</album-art>" nil t)
	    (setq cover (match-string 1))
	  )
	)
      (kill-buffer buffer)
      cover
      )
  )

(defun emms-cover-download-lastfm (artist album)
  (let ((url "http://ws.audioscrobbler.com/1.0/album/%artist/%album/info.xml"))
    (setq url (replace-regexp-in-string "%artist" (emms-url-quote artist) url))
    (setq url (replace-regexp-in-string "%album" (emms-url-quote album) url))
    (let ((url-show-status nil)
	  (url-request-method "GET"))
      (emms-cover-download-lastfm-parse (url-retrieve-synchronously url)))
    )
  )

(defun emms-cover-download-lastfm-parse (buffer)
  "Parses the server reponse and inform the user if all worked
well or if an error occured."
  (let ((cover nil))
    (with-current-buffer buffer
      (emms-http-decode-buffer buffer)
      (goto-char (point-min))
      ;; skip to the first empty line and go one line further.  There the 
      ;; response starts.
      (re-search-forward "^$" nil t)
      (forward-line)
      ;; an xml parser and xpath would be better but not as easily available in emacs
      (if (re-search-forward
	   "<coverart>\\(.\\|\n\\)*<large>\\([^<]*\\)</large>\\(.\\|\n\\)*</coverart>" nil t)
	  (setq cover (match-string 2))
	)
      )
    (kill-buffer buffer)
    cover))

(defun emms-cover-download-dummy (artist album)
  "file:///Users/bram/.covers/No Image.large.gif"
  )
  
(defun emms-cover-download-image (url artist album)
  (let ((coverpath emms-cover-savepath)
        (largecover)
        (cover))
    (setq coverpath (replace-regexp-in-string "%artist" artist coverpath))
    (setq coverpath (replace-regexp-in-string "%album" album coverpath))
    (setq coverpath (replace-regexp-in-string "%ext"
                                              (downcase (file-name-extension
                                                         url)) coverpath))

    (setq largecover (replace-regexp-in-string "%size" "large" coverpath))
    (unless (file-exists-p largecover)
      (url-copy-file url largecover))
    
    (dolist (size '(medium small))
      (setq cover (replace-regexp-in-string "%size" (symbol-name size) coverpath))
      (unless (file-exists-p cover)
	(let ((dimensions (cdr (assoc size emms-cover-dimensions))))
	  (emms-cover-resize-image largecover cover
                                   (nth 0 dimensions)
                                   (nth 1 dimensions))))
      )
    coverpath
    ))

(defun emms-cover-resize-image (file newfile width height)
  (let* ((command emms-cover-converter))
    (setq command (replace-regexp-in-string "%in" (expand-file-name file) command))
    (setq command (replace-regexp-in-string "%out" (expand-file-name newfile) command))
    (setq command (replace-regexp-in-string "%width" (number-to-string width) command))
    (setq command (replace-regexp-in-string "%height" (number-to-string height) command))
    (call-process-shell-command command)
  ))

(provide 'emms-cover)