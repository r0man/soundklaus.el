;;; soundklaus.el --- Play SoundCloud music in Emacs via EMMS -*- lexical-binding: t -*-

;; Copyright © 2014 r0man <roman@burningswell.com>

;; Author: r0man <roman@burningswell.com>
;; URL: https://github.com/r0man/soundklaus.el
;; Keywords: soundcloud, music, emms
;; Version: 0.1.0
;; Package-Requires: ((dash "1.5.0") (emacs "24") (emms "3.0") (request "0.1.0") (s "1.6.0") (pkg-info "0.4") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Search and play tracks and playlists from SoundCloud in Emacs via EMMS.

;;; Usage:

;; M-x soundklaus-connect - Authenticate with SoundCloud
;; M-x soundklaus-activities - List activities on SoundCloud
;; M-x soundklaus-playlists - Find playlists on SoundCloud
;; M-x soundklaus-tracks - Find tracks on SoundCloud

;; Todo:

;; - pagination

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 'emms)
(require 'json)
(require 'request)
(require 's)
(require 'widget)

;; CUSTOM

(defcustom soundklaus-api-root "https://api.soundcloud.com"
  "The SoundCloud API root URL."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-client-id "988875d70be466a2dd1bfab120c0a306"
  "The SoundKlaus OAuth2 client id."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-redirect-url "soundklaus://oauth/callback"
  "The SoundCloud OAuth2 redirect URL."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-access-token nil
  "The OAuth2 access token for the SoundCloud API."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-access-token-file "~/.soundklaus.el"
  "The filename where the SoundCloud access token is saved."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-download-dir "~/Music/soundcloud"
  "The directory of the download directory."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-activity-limit 40
  "The number of activities to fetch from SoundCloud at once."
  :type 'integer
  :group 'soundklaus-mode)

(defcustom soundklaus-playlist-limit 10
  "The number of playlists to fetch from SoundCloud at once."
  :type 'integer
  :group 'soundklaus-mode)

(defcustom soundklaus-track-limit 40
  "The number of tracks to fetch from SoundCloud at once."
  :type 'integer
  :group 'soundklaus-mode)

(defcustom soundklaus-user-limit 20
  "The number of users to fetch from SoundCloud at once."
  :type 'integer
  :group 'soundklaus-mode)

(defcustom soundklaus-port 8383
  "The port number on which playlists are served."
  :type 'integer
  :group 'soundklaus-mode)

(defcustom soundklaus-padding 2
  "The number of columns used for padding on the left side of the buffer."
  :type 'integer
  :group 'soundklaus-mode)

(defvar *soundklaus-tracks* (make-hash-table))

(defun soundklaus-width ()
  "Return the width of the renderable content."
  (- (/ (frame-width) 2) (* soundklaus-padding 2)))

(defun soundklaus-horizontal-rule ()
  "Insert a horizontal rule into the buffer."
  (widget-insert (concat (make-string soundklaus-padding ?\s)
			 (make-string (- (soundklaus-width) soundklaus-padding) ?-)
			 "\n")))

(defgeneric soundklaus-download (media)
  "Download the MEDIA from SoundCloud.")

(defgeneric soundklaus-play (media)
  "Play the SoundCloud MEDIA.")

(defgeneric soundklaus-playlist-add (media)
  "Insert the SoundCloud MEDIA into the EMMS playlist.")

(defgeneric soundklaus-render-list-item (media)
  "Render the SoundCloud MEDIA as a list item.")

(defgeneric soundklaus-path (resource)
  "Returns the path to the RESOURCE on SoundCloud.")

(defun soundklaus-url (resource)
  "Return the URL to the RESOURCE on SoundCloud."
  (concat soundklaus-api-root (soundklaus-path resource)))

(defmacro soundklaus-with-access-token (&rest body)
  "Ensure that `soundklaus-access-token` is not blank, and raise
  an error otherwise."
  `(if (s-blank? soundklaus-access-token)
       (message "Not authenticated with SoundCloud. Try M-x soundklaus-connect.")
     (progn ,@body)))

(defun soundklaus-remove-nil-values (assoc-list)
  "Remove all elements from ASSOC-LIST where the value is nil."
  (delq nil (mapcar (lambda (element)
		      (if (cdr element)
			  element))
		    assoc-list)))

(defun soundklaus-define-slot (name attribute)
  "Return the s-expression to define a resource slot.
Argument NAME is the name of the class.
Argument ATTRIBUTE is the name of the slot."
  `(,(car attribute)
    :initarg ,(intern (format ":%s" (car attribute)))
    :accessor ,(intern (format "soundklaus-%s-%s" name (car attribute)))
    :documentation ,(cadr attribute)))

(defun soundklaus-define-class (name doc slots)
  "Return the s-expression to define a resource class.
Argument NAME is the resource name.
Argument NAME is documentation string of the class.
Argument SLOTS is a list of attributes."
  (let ((class (intern (format "soundklaus-%s" name))))
    `(defclass ,class ()
       ,(mapcar (lambda (slot)
		  (soundklaus-define-slot name slot))
		slots)
       ,doc)))

(defun soundklaus-path-segments (path)
  "Return the segments of PATH as a list."
  (-remove 's-blank? (s-split "/" path)))

(defun soundklaus-path-symbols (path)
  "Return the symbols of each segment in PATH as a lists."
  (let ((segments (soundklaus-path-segments path)))
    (-map (lambda (segment)
	    (-map (lambda (id)
		    (intern (replace-regexp-in-string "^:" "" id)))
		  (s-split "-:" segment)))
	  (-filter (lambda (segment)
		     (s-match ":" segment))
		   segments))))

(defun soundklaus-replace-slots (pattern slots &rest args)
  "Replace the SLOTS in PATTERN with their values."
  (-reduce-from
   (lambda (url x)
     (s-replace (car x) (cdr x) url))
   pattern
   (cl-mapcan
    (lambda (slots instance)
      (if instance
	  (mapcar (lambda (slot)
		    (cons (format ":%s" slot)
			  (format "%s" (slot-value instance slot))))
		  slots)))
    slots args)))

(defun soundklaus-intern (name &rest args)
  "Intern NAME in the soundklaus namespace as a symbol."
  (unless (s-blank? name)
    (intern (apply 'format (format "soundklaus-%s" name) args))))

(defun soundklaus-define-path (name pattern)
  "Returns the s-expression to define a resource path method.
Argument NAME is the name of the resource."
  (let ((slots (soundklaus-path-symbols pattern))
	(resource (cl-gensym "resource-")))
    `(defmethod ,(soundklaus-intern "path") ((,resource ,(soundklaus-intern name)))
       (soundklaus-replace-slots ,pattern ',slots ,resource))))

(defmacro define-soundklaus-resource (name pattern doc attributes)
  "Define a SoundCloud resource.
Argument NAME is the name of the resource."
  `(progn
     ,(soundklaus-define-class name doc attributes)
     ,(soundklaus-define-path name pattern)))

;; USER: https://developers.soundcloud.com/docs/api/reference#users

(define-soundklaus-resource user "/users/:id"
  "A SoundCloud user"
  ((id "The id of the user")
   (username "The short name of the user")
   (city "The city of the user")
   (country "The country of the user")
   (full-name "The full name of the user")
   (track-count "The number of public tracks of the user")
   (playlist-count "The number of public playlists of the user")
   (public-favorites-count "The number of public favorites of the user")
   (followings-count "The number of followings of the user")
   (followers-count "The number of followers of the user")))

;; TRACK: https://developers.soundcloud.com/docs/api/reference#tracks

(define-soundklaus-resource track "/tracks/:id"
  "A SoundCloud track"
  ((id "The id of the track")
   (user "The minimal representation of the track owner")
   (title "The title of the track")
   (stream-url "The link the 128kbs MP3 stream of the track")
   (duration "The duration of the track in milliseconds")
   (genre "The music genre of the track")
   (artwork-url "The URL of the track artwork")
   (playback-count "The play count of the track")
   (download-count "The download count of the track")
   (comment-count "The number of comments of the track")
   (favoritings-count "The number of time the track has been favorited")))

;; PLAYLIST: https://developers.soundcloud.com/docs/api/reference#playlists

(define-soundklaus-resource playlist "/playlists/:id"
  "A SoundCloud playlist"
  ((id "The id of the playlist")
   (user "The minimal representation of the playlist owner")
   (title "The title of the playlist")
   (tracks "The tracks of the  playlist")
   (duration "The duration of the playlist in milliseconds")))

;; COLLECTION

(defclass soundklaus-collection ()
  ((content
    :initarg :content
    :accessor soundklaus-collection-content
    :documentation "The content of the collection.")
   (future
    :initarg :future
    :accessor soundklaus-collection-future
    :documentation "The URL to the page of the collection that
    will contain future resources of the collection.")
   (next
    :initarg :next
    :accessor soundklaus-collection-next
    :documentation "The URL to the next page of the collection."))
  "A collection of resources on SoundCloud.")

(defun soundklaus-make-collection (assoc-list)
  "Make a SoundCloud collection from an ASSOC-LIST."
  (let ((collection (make-instance 'soundklaus-collection)))
    (with-slots (content future next) collection
      (setf future (cdr (assoc 'future_href assoc-list)))
      (setf next (cdr (assoc 'next_href assoc-list)))
      (setf content (delq nil (mapcar
			       (lambda (resource)
				 (let ((type (cdr (assoc 'type resource)))
				       (origin (cdr (assoc 'origin resource))))
				   (cond
				    ;; TODO: Load tracks of playlist somehow
				    ;; ((equal type "playlist")
				    ;;  (soundklaus-make-playlist origin))
				    ((equal type "track")
				     (soundklaus-make-track origin)))))
			       (cdr (assoc 'collection assoc-list)))))
      collection)))

;; EMMS Sources

(define-emms-source soundklaus-track (track)
  "An EMMS source for a SoundCloud TRACK."
  (let ((emms-track (emms-track 'url (soundklaus-track-stream-url track))))
    (emms-track-set emms-track 'info-title (soundklaus-track-title track))
    (emms-track-set emms-track 'info-playing-time (soundklaus-track-duration-secs track))
    (emms-playlist-insert-track emms-track)))

(define-emms-source soundklaus-playlist (playlist)
  "An EMMS source for a SoundCloud PLAYLIST."
  (mapc 'emms-insert-soundklaus-track
        (soundklaus-playlist-tracks playlist)))

(defun soundklaus-make-user (assoc-list)
  "Make a SoundCloud user instance from an ASSOC-LIST."
  (let ((instance (soundklaus-slurp-instance 'soundklaus-user assoc-list)))
    instance))

(defun soundklaus-make-track (assoc-list)
  "Make a SoundCloud track instance from ASSOC-LIST."
  (let ((instance (soundklaus-slurp-instance 'soundklaus-track assoc-list)))
    (with-slots (user playback-count download-count comment-count favoritings-count) instance
      (setf user (soundklaus-make-user user))
      (setf playback-count (or playback-count 0))
      (setf download-count (or download-count 0))
      (setf comment-count (or comment-count 0))
      (setf favoritings-count (or favoritings-count 0))
      instance)))

(defun soundklaus-make-playlist (assoc-list)
  "Make a SoundCloud playlist instance from ASSOC-LIST."
  (let ((instance (soundklaus-slurp-instance 'soundklaus-playlist assoc-list)))
    (with-slots (user tracks) instance
      (setf user (soundklaus-make-user user))
      (setf tracks (mapcar 'soundklaus-make-track tracks))
      instance)))

(defun soundklaus-url-encode (params)
  "Return a string that is PARAMS URI-encoded. PARAMS can be a
number, string, symbol or an association list."
  (cond
   ((stringp params)
    (url-hexify-string params))
   ((symbolp params)
    (intern (soundklaus-url-encode (symbol-name params))))
   ((listp params)
    (if (listp (car params))
        (s-join "&" (delq nil (mapcar 'soundklaus-url-encode params)))
      (let ((value (if (atom (cdr params))
		       (cdr params)
		     (cadr params))))
	(if value
	    (format "%s=%s"
		    (soundklaus-url-encode (car params))
		    (soundklaus-url-encode value))))))
   (t (url-hexify-string (format "%s" params)))))

(defun soundklaus-safe-path (path)
  "Return the safe name of PATH."
  (let* ((path (replace-regexp-in-string "[^0-9A-Za-z]+" "-" path))
	 (path (replace-regexp-in-string "^-" "" path))
	 (path (replace-regexp-in-string "-$" "" path)))
    (downcase path)))

(defun soundklaus-track-duration-secs (track)
  "Return the duration of TRACK in seconds."
  (/ (soundklaus-track-duration track) 1000))

(defun soundklaus-track-download-filename (track)
  "Return the download filename for TRACK."
  (expand-file-name
   (concat (file-name-as-directory soundklaus-download-dir)
	   (file-name-as-directory (soundklaus-safe-path (soundklaus-track-username track)))
	   (concat (soundklaus-safe-path (soundklaus-track-title track)) ".mp3"))))

(defun soundklaus-playlist-download-directory (playlist)
  "Return the download directory for PLAYLIST."
  (expand-file-name
   (concat (file-name-as-directory soundklaus-download-dir)
  	   (file-name-as-directory (soundklaus-safe-path (soundklaus-playlist-username playlist)))
  	   (soundklaus-safe-path (soundklaus-playlist-title playlist)))))

(defun soundklaus-playlist-track-download-filename (playlist track n)
  "Returns the download filename of the TRACK number N in PLAYLIST."
  (expand-file-name
   (concat (file-name-as-directory (soundklaus-playlist-download-directory playlist))
	   (format "%02d-%s.mp3" n (soundklaus-safe-path (soundklaus-track-title track))))))

(defun soundklaus-track-header (track)
  "Return the TRACK header as a string."
  (concat (soundklaus-track-title track) " - "
	  (soundklaus-track-username track)))

(defun soundklaus-track-stream-url (track)
  "Returns the download url of TRACK with
`soundklaus-client-id` and `soundklaus-access-token` as query
parameters."
  (concat (slot-value track 'stream-url) "?"
	  (soundklaus-url-encode
	   (soundklaus-remove-nil-values
	    `(("client_id" . ,soundklaus-client-id)
	      ("oauth_token" . ,soundklaus-access-token))))))

(defun soundklaus-track-time (track)
  "Return the TRACK duration formatted as HH:MM:SS."
  (soundklaus-format-duration (soundklaus-track-duration track)))

(defun soundklaus-track-username (track)
  "Return the username of TRACK."
  (soundklaus-user-username (soundklaus-track-user track)))

(defun soundklaus-playlist-time (playlist)
  "Return the PLAYLIST duration formatted as HH:MM:SS."
  (soundklaus-format-duration (soundklaus-playlist-duration playlist)))

(defun soundklaus-transform (arg transform-fn)
  "Transform ARG with TRANSFORM-FN. ARG can be an assoc list,
hash table, string or a symbol. If ARG is an assoc list or hash
table only the keys will be transformed."
  (cond
   ((and (listp arg) (listp (car arg)))
    (mapcar (lambda (c) (cons (soundklaus-transform (car c) transform-fn) (cdr c))) arg))
   ((and (listp arg) (atom (car arg)))
    (cons (soundklaus-transform (car arg) transform-fn)
          (cdr arg)))
   ((hash-table-p arg)
    (let ((other (make-hash-table :test 'equal)))
      (maphash (lambda (key value) (puthash (soundklaus-transform key transform-fn) value other)) arg)
      other))
   ((stringp arg)
    (funcall transform-fn arg))
   ((symbolp arg)
    (intern (soundklaus-transform (symbol-name arg) transform-fn)))))

(defun soundklaus-dasherize (arg)
  "Replace each underscore in ARG with a dash. ARG can be an
association list, hash table, string or a symbol. If ARG is an
association list or hash table only the keys will be dasherized."
  (soundklaus-transform arg (lambda (string) (replace-regexp-in-string "_" "-" string))))

(defun soundklaus-underscore (arg)
  "Replace each underscore in ARG with a dash. ARG can be an
association list, hash table, string or a symbol. If ARG is an
association list or hash table only the keys will be underscored."
  (soundklaus-transform arg (lambda (string) (replace-regexp-in-string "-" "_" string))))

(defun soundklaus-playlist-username (playlist)
  "Return the username of PLAYLIST."
  (soundklaus-user-username (soundklaus-playlist-user playlist)))

(defun soundklaus-tag-track (track)
  "Tag the SoundCloud TRACK."
  (let ((filename (soundklaus-track-download-filename track)))
    (shell-command (format "mp3info -d %s" (shell-quote-argument filename)))
    (shell-command (format "mp3info -t %s %s"
			   (shell-quote-argument (soundklaus-track-title track))
			   (shell-quote-argument filename)))))

(defmethod soundklaus-download ((media soundklaus-track))
  (let ((url (soundklaus-track-stream-url media))
	(buffer (format "*soundklaus-download-%s*" (soundklaus-track-id media)))
	(filename (soundklaus-track-download-filename media)))
    (make-directory (file-name-directory filename) t)
    (set-process-sentinel
     (start-process "SoundCloud Download" buffer "curl" "-L" url "-o" filename)
     (lambda (process event)
       (cond
	((string= "exit" (process-status process))
	 (let ((buffer (get-buffer buffer)))
	   (if buffer (kill-buffer buffer)))
	 (soundklaus-tag-track media)
	 (message "Download of %s complete." (soundklaus-track-title media))))))
    (message "Downloading %s ..." (soundklaus-track-title media))))

(defun soundklaus-download-playlist-script (playlist)
  "Return the content of a shell script to download PLAYLIST."
  (let ((directory (soundklaus-playlist-download-directory playlist)))
    (with-temp-buffer
      (erase-buffer)
      (insert "#!/usr/bin/env bash")
      (newline)
      (insert "set -x")
      (newline)
      (insert (format "mkdir -p %s" (shell-quote-argument directory)))
      (newline)
      (cl-loop for n from 1 to (length (soundklaus-playlist-tracks playlist)) do
               (let* ((track (elt (soundklaus-playlist-tracks playlist) (- n 1)))
                      (url (soundklaus-track-stream-url track))
                      (filename (soundklaus-playlist-track-download-filename playlist track n)))
                 (insert (format "curl -L '%s' -o %s"
                                 url (shell-quote-argument filename)))
                 (newline)
                 (insert (format "mp3info -d %s" (shell-quote-argument filename)))
                 (newline)
                 (insert (format "mp3info -n %s -t %s %s"
                                 n (shell-quote-argument (soundklaus-track-title track))
                                 (shell-quote-argument filename)))
                 (newline)))
      (buffer-string))))

(defmethod soundklaus-download ((playlist soundklaus-playlist))
  (let* ((id (soundklaus-playlist-id playlist))
	 (buffer (format "*soundklaus-download-%s*" id))
	 (filename (make-temp-file (format "soundklaus-download-playlist-%s" id))))
    (message "%s" filename)
    (with-temp-buffer
      (insert (soundklaus-download-playlist-script playlist))
      (write-region (point-min) (point-max) filename))
    (set-process-sentinel
     (start-process "SoundCloud Download" buffer "bash" filename)
     (lambda (process event)
       (cond
	((string= "exit" (process-status process))
	 (let ((buffer (get-buffer buffer)))
	   (if buffer (kill-buffer buffer)))
	 ;; (delete-file filename)
	 (message "Download of %s complete." (soundklaus-playlist-title playlist))))))
    (message "Downloading %s ..." (soundklaus-playlist-title playlist))))

(defmethod soundklaus-play ((track soundklaus-track))
  (emms-play-soundklaus-track track))

(defmethod soundklaus-play ((playlist soundklaus-playlist))
  (emms-play-soundklaus-playlist playlist))

(defmethod soundklaus-playlist-add ((track soundklaus-track))
  (emms-add-soundklaus-track track))

(defmethod soundklaus-playlist-add ((playlist soundklaus-playlist))
  (emms-add-soundklaus-playlist playlist))

(defun soundklaus-append-current ()
  "Append the current SoundCloud media at point to the EMMS playlist."
  (interactive)
  (let ((media (soundklaus-current-media)))
    (if media (soundklaus-playlist-add media))))

(defun soundklaus-current-media ()
  "Return the current SoundCloud track at point."
  (get-text-property (point) :soundklaus-media))

(defun soundklaus-download-current ()
  "Download the current SoundCloud media at point."
  (interactive)
  (let ((media (soundklaus-current-media)))
    (if media (soundklaus-download media))))

(defun soundklaus-next-media ()
  "Move point to the next SoundCloud track."
  (interactive)
  (let ((pos (next-single-property-change (point) :soundklaus-media)))
    (when pos
      (goto-char pos)
      (unless (soundklaus-current-media)
	(let ((pos (next-single-property-change pos :soundklaus-media)))
	  (if pos (goto-char pos)))))))

(defun soundklaus-prev-media ()
  "Move point to the next SoundCloud track."
  (interactive)
  (let ((pos (previous-single-property-change (point) :soundklaus-media)))
    (when pos
      (goto-char pos)
      (unless (soundklaus-current-media)
	(let ((pos (previous-single-property-change pos :soundklaus-media)))
	  (if pos (goto-char pos)))))))

(defun soundklaus-play-current ()
  "Play the current SoundCloud media at point."
  (interactive)
  (let ((media (soundklaus-current-media)))
    (if media (soundklaus-play media))))

(defun soundklaus-connect-url ()
  "Return the SoundCloud connect url."
  (concat soundklaus-api-root "/connect?"
          (soundklaus-url-encode
           `(("client_id" . ,soundklaus-client-id)
             ("redirect_uri" . ,soundklaus-redirect-url)
             ("response_type" . "code_and_token")
             ("scope" . "non-expiring")))))

(defun soundklaus-connect ()
  "Connect with SoundCloud."
  (interactive)
  (browse-url (soundklaus-connect-url)))

(defun soundklaus-parse-callback (url)
  "Parse the code, token and scope from the OAuth2 callback URL."
  (condition-case nil
      (with-temp-buffer
	(let ((pattern (replace-regexp-in-string "://" ":/" soundklaus-redirect-url)))
	  (insert url)
	  (beginning-of-buffer)
	  (search-forward pattern)
	  (let ((url (url-generic-parse-url
		      (buffer-substring
		       (- (point) (length pattern))
		       (point-max)))))
	    (append (if (url-target url)
			(url-parse-query-string (url-target url)))
		    (if (url-path-and-query url)
			(url-parse-query-string (cdr (url-path-and-query url))))))))
    (error nil)))

(defadvice server-visit-files (around soundklaus-detect-protocol-server activate)
  "Handle SoundCloud OAuth2 callback URLs."
  (let* ((files (mapcar (lambda (file)
			  (let ((params (soundklaus-parse-callback (car file))))
			    (if params
				(let ((access-token (cadr (assoc "access_token" params)))
				      (error-code (cadr (assoc "error" params)))
				      (error-description (cadr (assoc "error_description" params))))
				  (when access-token
				    (setq soundklaus-access-token access-token)
				    (message "Authentication with SoundCloud complete."))
				  (when error-code
				    (message "Authentication with SoundCloud failed. %s"
					     (replace-regexp-in-string "+" " " (or error-description error-code))))
				  nil)
			      file)))
			(ad-get-arg 0)))
	 (files (delq nil files)))
    (when (not (null files))
      (ad-set-arg 0 files)
      ad-do-it)))

(defun soundklaus-append-default-params (params)
  "Set the default request PARAMS in `params`."
  (soundklaus-remove-nil-values
   (append params `(("client_id" . ,soundklaus-client-id)
		    ("oauth_token" . ,soundklaus-access-token)))))

(defun soundklaus-register-tracks (tracks)
  "Register all TRACKS by id in the *soundklaus-tracks* hash table."
  (mapc (lambda (track)
	  (puthash (soundklaus-track-id track) track *soundklaus-tracks*))
	tracks))

(defun soundklaus-request-data (method params)
  "Returns the HTTP form PARAMS for a request of type METHOD."
  (if (or (equal :post method) (equal :put method))
      (soundklaus-append-default-params params)))

(defun soundklaus-request-params (method params)
  "Returns the HTTP query PARAMS for a request of type METHOD."
  (if (equal :get method)
      (soundklaus-append-default-params params)))

(defun soundklaus-request-type (method)
  "Return the HTTP request METHOD as an uppercase string."
  (upcase (replace-regexp-in-string ":" "" (format "%s" method))))

(defun soundklaus-request (method path params on-success)
  "Send an HTTP `method` request to the SoundCloud API endpoint
at `path`, parse the response as JSON and call `callback`."
  (request
   (concat soundklaus-api-root path)
   :type (soundklaus-request-type method)
   :data (soundklaus-request-data method params)
   :params (soundklaus-request-params method params)
   :headers '(("Accept" . "application/json"))
   :parser 'json-read
   :success on-success))

(defun soundklaus-save-response (response filename)
  "Save the RESPONSE of a SoundCloud API request as JSON in FILENAME."
  (let ((data (request-response-data response)))
    (make-directory (file-name-directory filename) t)
    (with-temp-file filename (insert (format "%s" (json-encode data))))))

(defun soundklaus-slurp-instance (class assoc-list)
  "Make an instance of CLASS and initialize it's slots from the ASSOC-LIST."
  (let ((instance (make-instance class)))
    (mapc (lambda (slot)
	    (let ((key (soundklaus-underscore slot))
		  (value (cdr (assoc key assoc-list))))
	      (set-slot-value instance slot value)))
	  (aref (class-v class) class-public-a))
    instance))

(defun soundklaus-format-duration (duration-in-ms)
  "Format DURATION-IN-MS in the HH:MM:SS format."
  (let* ((duration (/ duration-in-ms 1000))
	 (hours (floor (/ duration 3600)))
	 (minutes (floor (/ (- duration (* hours 3600)) 60)))
	 (seconds (floor (- duration (* hours 3600) (* minutes 60)))))
    (format "%02d:%02d:%02d" hours minutes seconds)))

(defun soundklaus-show-playlist (playlist)
  "Show the PLAYLIST."
  (interactive)
  (soundklaus-request
   :get (format "%s/playlists/%s" soundklaus-api-root
		(soundklaus-playlist-id playlist))
   '()
   (function*
    (lambda (&key data &allow-other-keys)
      (soundklaus-with-widget
       (let ((playlist (soundklaus-make-playlist data)))
	 (widget-insert (format "\n  >> PLAYLIST >> %s\n\n" (upcase (soundklaus-playlist-title playlist))))
	 (mapcar 'soundklaus-render-track (soundklaus-playlist-tracks playlist))))))))

(defun soundklaus-render-row (left right &optional width-right)
  "Render a row with a LEFT and a RIGHT part.
Optional argument WIDTH-RIGHT is the width of the right argument."
  (let* ((width-right (or width-right (length (or right ""))))
	 (width-left (- (soundklaus-width)
			(- width-right 1)
			(* 2 soundklaus-padding)))
	 (padding (make-string soundklaus-padding ?\s)))
    (widget-insert (format
		    (format "%s%%-%s.%ss %%%s.%ss%s\n"
			    padding
			    width-left width-left
			    width-right width-right
			    padding)
		    left right))))

;; LIST ITEM WIDGETS

(defmethod soundklaus-render-list-item ((track soundklaus-track))
  (let ((start (point)))
    (soundklaus-render-row
     (propertize (soundklaus-track-header track) 'face 'bold)
     (propertize (soundklaus-track-time track) 'face 'bold))
    (soundklaus-horizontal-rule)
    (soundklaus-render-row
     (format "%s plays, %s downloads, %s comments and %s favorites."
	     (soundklaus-track-playback-count track)
	     (soundklaus-track-download-count track)
	     (soundklaus-track-comment-count track)
	     (soundklaus-track-favoritings-count track))
     (upcase (or (soundklaus-track-genre track) "Unknown")))
    (put-text-property start (point) :soundklaus-media track)
    (widget-insert "\n")))

(defmethod soundklaus-render-list-item ((playlist soundklaus-playlist))
  (let ((start (point)))
    (soundklaus-render-row
     (propertize (concat (soundklaus-playlist-title playlist) " - "
			 (soundklaus-playlist-username playlist))
		 'face 'bold)
     (propertize (soundklaus-playlist-time playlist) 'face 'bold))
    (put-text-property start (point) :soundklaus-media playlist)
    (soundklaus-horizontal-rule)
    (cl-loop for n from 1 to (length (soundklaus-playlist-tracks playlist)) do
             (let ((track (elt (soundklaus-playlist-tracks playlist) (- n 1)))
		   (start (point)))
               (soundklaus-render-row
                (format "%02d  %s " n (soundklaus-track-title track))
                (soundklaus-format-duration (soundklaus-track-duration track)))
               (put-text-property start (point) :soundklaus-media track)))
    (widget-insert "\n")))

(defmacro soundklaus-with-widget (&rest body)
  "Evaluate BODY with in the context of the SoundCloud widget buffer."
  `(progn
     (set-buffer (get-buffer-create "*soundklaus*"))
     (switch-to-buffer-other-window "*soundklaus*")
     (kill-all-local-variables)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (remove-overlays)
       ,@body)
     (use-local-map widget-keymap)
     (widget-setup)
     (soundklaus-mode)
     (widget-minor-mode)
     (goto-char 0)
     (soundklaus-next-media)))

(defun soundklaus-render-activities (collection)
  "Render SoundCloud COLLECTION as an Emacs widget."
  (soundklaus-with-widget
   (widget-insert "\n  >> ACTIVITIES\n\n")
   (let ((activities (soundklaus-collection-content collection)))
     (mapcar 'soundklaus-render-list-item activities))))

(defun soundklaus-render-tracks (tracks)
  "Render SoundCloud TRACKS as an Emacs widget."
  (soundklaus-with-widget
   (widget-insert "\n  >> TRACKS\n\n")
   (mapc 'soundklaus-render-list-item tracks)))

(defun soundklaus-render-playlists (playlists)
  "Render SoundCloud PLAYLISTS as an Emacs widget."
  (soundklaus-with-widget
   (widget-insert "\n  >> PLAYLISTS\n\n")
   (mapc 'soundklaus-render-list-item playlists)))


;;;###autoload
(defun soundklaus-activities ()
  "List activities on SoundCloud."
  (interactive)
  (soundklaus-with-access-token
   (soundklaus-request
    :get "/me/activities"
    `(("limit" . ,(number-to-string soundklaus-activity-limit)))
    (function*
     (lambda (&key data &allow-other-keys)
       (soundklaus-render-activities (soundklaus-make-collection data)))))))

;;;###autoload
(defun soundklaus-tracks (query)
  "List all tracks on SoundCloud matching QUERY."
  (interactive "MQuery: ")
  (soundklaus-request
   :get "/tracks"
   `(("limit" . ,(number-to-string soundklaus-track-limit))
     ("q" . ,query))
   (function*
    (lambda (&key data &allow-other-keys)
      (soundklaus-render-tracks (mapcar 'soundklaus-make-track data))))))

;;;###autoload
(defun soundklaus-playlists (query)
  "List all playlists on SoundCloud matching QUERY."
  (interactive "MQuery: ")
  (soundklaus-request
   :get "/playlists"
   `(("limit" . ,(number-to-string soundklaus-playlist-limit))
     ("q" . ,query))
   (function*
    (lambda (&key data &allow-other-keys)
      (soundklaus-render-playlists (mapcar 'soundklaus-make-playlist data))))))

;;;###autoload
(defun soundklaus-my-playlists ()
  "List your playlists on SoundCloud."
  (interactive)
  (soundklaus-with-access-token
   (soundklaus-request
    :get "/me/playlists"
    `(("limit" . ,(number-to-string soundklaus-playlist-limit)))
    (function*
     (lambda (&key data &allow-other-keys)
       (soundklaus-render-playlists (mapcar 'soundklaus-make-playlist data)))))))

;;;###autoload
(defun soundklaus-my-tracks ()
  "List your tracks on SoundCloud."
  (interactive)
  (soundklaus-with-access-token
   (soundklaus-request
    :get "/me/tracks"
    `(("limit" . ,(number-to-string soundklaus-track-limit)))
    (function*
     (lambda (&key data &allow-other-keys)
       (soundklaus-render-tracks (mapcar 'soundklaus-make-track data)))))))

(defvar soundklaus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<") 'emms-seek-backward)
    (define-key map (kbd "<return>") 'soundklaus-play-current)
    (define-key map (kbd ">") 'emms-seek-forward)
    (define-key map (kbd "C-n") 'soundklaus-next-media)
    (define-key map (kbd "C-p") 'soundklaus-prev-media)
    (define-key map (kbd "a") 'soundklaus-append-current)
    (define-key map (kbd "d") 'soundklaus-download-current)
    (define-key map (kbd "n") 'soundklaus-next-media)
    (define-key map (kbd "p") 'soundklaus-prev-media)
    map)
  "Keymap for SoundKlaus mode.")

(define-minor-mode soundklaus-mode
  "Play SoundCloud music in Emacs.
\\{soundklaus-mode-map}"
  :init-value nil
  :lighter " SoundKlaus"
  :keymap 'soundklaus-mode-map
  :group 'soundklaus-mode)

(provide 'soundklaus)

;;; soundklaus.el ends here
