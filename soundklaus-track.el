;;; soundklaus-track.el --- SoundKlaus tracks -*- lexical-binding: t -*-

;; Copyright © 2014-2016 r0man <roman@burningswell.com>
;; Author: r0man <roman@burningswell.com>

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

;; SoundKlaus tracks

;;; Code:

(require 'emms)
(require 'soundklaus-resource)
(require 'soundklaus-utils)
(require 'soundklaus-user)

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
   (favoritings-count "The number of time the track has been favorited")
   (permalink-url "The URL to the SoundCloud.com page")
   (user-favorite "True if the current user favorite the track, false otherwise")))

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

(defun soundklaus-track-duration-secs (track)
  "Return the duration of TRACK in seconds."
  (/ (soundklaus-track-duration track) 1000))

(defun soundklaus-track-filename (track &optional include-user)
  "Return the download filename for TRACK. Prefix the filename
with the track's username if INCLUDE-USER is true."
  (if include-user
      (format "%s-%s.mp3"
              (soundklaus-safe-path (soundklaus-track-username track))
              (soundklaus-safe-path (soundklaus-track-title track)))
    (format "%s.mp3" (soundklaus-safe-path (soundklaus-track-title track)))))

(defun soundklaus-track-header (track)
  "Return the TRACK header as a string."
  (concat
   (if (equal (soundklaus-track-user-favorite track) t)
       "♥" "♡")
   " "
   (soundklaus-track-title track) " - "
   (soundklaus-track-username track)))

(defun soundklaus-track-stream-url (track)
  "Return the stream URL of TRACK."
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

(defun soundklaus-track-like (track)
  "Like or unlike TRACK."
  (with-slots (id user-favorite) track
    (let* ((request (soundklaus-make-request
                     (format "/me/favorites/%d" id)
                     :method (if user-favorite "DELETE" "PUT")
                     :query-params '((app_version . "1456762799"))))
           (response (soundklaus-send-sync-request request)))
      (setf user-favorite (not user-favorite))
      response)))

(defun soundklaus-tag-track (track)
  "Tag the SoundCloud TRACK."
  (let ((filename (soundklaus-track-filename track)))
    (shell-command (format "mp3info -d %s" (shell-quote-argument filename)))
    (shell-command (format "mp3info -t %s %s"
			   (shell-quote-argument (soundklaus-track-title track))
			   (shell-quote-argument filename)))))

(define-emms-source soundklaus-track (track)
  "An EMMS source for a SoundCloud TRACK."
  (let ((emms-track (emms-track 'url (soundklaus-track-stream-url track))))
    (emms-track-set emms-track 'info-title (soundklaus-track-title track))
    (emms-track-set emms-track 'info-playing-time (soundklaus-track-duration-secs track))
    (emms-playlist-insert-track emms-track)))

(provide 'soundklaus-track)

;;; soundklaus-track.el ends here
