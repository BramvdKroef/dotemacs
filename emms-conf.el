;; /file emms-conf.el  Emms (Emacs Media something something) player
;; $Id: emms-conf.el,v 1.8 2010-01-27 22:38:55 bram Exp $

(require 'emms-setup)
(require 'emms-player-simple)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-playlist-mode)
(emms-standard)

;; We'll be using  mplayer
(setq emms-player-list '(emms-player-mplayer))

;; Use my custom mp3 tag reader instead of mp3info (which only reads v1)
(require 'emms-info-eyed3)
(require 'emms-info-ogginfo)
(setq emms-info-functions '(emms-info-ogginfo emms-info-eyed3))

(require 'emms-streams)

;; mode line Now playing
(require 'emms-mode-line)
(emms-mode-line 1)

(require 'emms-playing-time)
(emms-playing-time 1)

(require 'emms-browser)

;; lastfm config
;;(require 'emms-lastfm-client)

;;(emms-lastfm-scrobbler-enable)

;; Enable album covers
;;(require 'emms-cover)
;;(setq emms-browser-covers 'emms-cover-find)

;; Stop playing on lunch break
(add-hook 'lunch-break-start-hook
	  '(lambda ()
	     (if (and emms-player-playing-p (not emms-player-paused-p)) 
		 (emms-pause))))

;; Remove the -realy-quiet parameter so we can get the stream info
(setq emms-player-mplayer-parameters '("-slave" "-quiet"))

(defun mplayer-stream-filter (proc string)
  "Checks mplayer output for ICY Info data. If any is found then the StreamTitle
option is extracted and written to the track's 'info-title property. Because
  emms-info-track-description -- the function that creates the track name -- needs a
  title *and* an artist 'info-artist is set to the stream title (the one you see in
  emms-streams)."
  (if (string-match "ICY Info: StreamTitle='\\(.*\\)';StreamUrl='.*';" string)
      (let ((track (emms-playlist-current-selected-track))
            (title (match-string 1 string)))
        (if (> (length title) 40)
            (setq title (concat (substring title 0 37) "...")))

        (emms-track-set track 'info-title title)
        (if (not (emms-track-get track 'info-artist))
            (emms-track-set track 'info-artist
                        (if (listp (emms-track-get track 'metadata))
                            (car (emms-track-get track 'metadata))
                          "")))
        (emms-track-updated track))
    ))

(defun mplayer-stream-start-listening ()
  "This emms-player-started-hook checks if the current track is a
url and the process playing it is mplayer. If it is then the
output filter mplayer-steam-filter is added to the process"
  (if (eq (emms-track-type (emms-playlist-current-selected-track)) 'url)
      (let ((process (get-process emms-player-simple-process-name)))
        (if (string= (car (process-command process)) "mplayer")
            (set-process-filter process 'mplayer-stream-filter))
        ))
  )

(add-hook 'emms-player-started-hook 'mplayer-stream-start-listening)

