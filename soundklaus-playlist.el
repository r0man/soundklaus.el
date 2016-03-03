;;; soundklaus-playlist.el --- SoundKlaus playlists -*- lexical-binding: t -*-

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

;; SoundKlaus playlists

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'emms)
(require 'soundklaus-request)
(require 'soundklaus-resource)
(require 'soundklaus-track)
(require 'soundklaus-user)
(require 'soundklaus-utils)

(define-soundklaus-resource playlist "/playlists/:id"
  "A SoundCloud playlist"
  ((id "The id of the playlist")
   (user "The minimal representation of the playlist owner")
   (title "The title of the playlist")
   (tracks "The tracks of the  playlist")
   (duration "The duration of the playlist in milliseconds")
   (permalink-url "The URL to the SoundCloud.com page")))

(defclass soundklaus-playlist-track (soundklaus-track)
  ((number
    :initarg :number
    :accessor soundklaus-playlist-track-number
    :documentation "The number of the track in the playlist.")))

(defun soundklaus-make-playlist-track (track-number assoc-list)
  "Make a SoundCloud playlist track instance from TRACK-NUMBER and ASSOC-LIST."
  (let ((track (soundklaus-make-track assoc-list 'soundklaus-playlist-track)))
    (set-slot-value track 'number track-number)
    track))

(defun soundklaus-make-playlist (assoc-list)
  "Make a SoundCloud playlist instance from ASSOC-LIST."
  (let ((instance (soundklaus-slurp-instance 'soundklaus-playlist assoc-list)))
    (with-slots (user tracks) instance
      (setf user (soundklaus-make-user user))
      (setf tracks (-map-indexed
                    (lambda (index track)
                      (soundklaus-make-playlist-track (1+ index) track))
                    (cl-map 'list #'identity tracks)))
      instance)))

(defun soundklaus-playlist-directory (playlist)
  "Return the directory for PLAYLIST."
  (concat (soundklaus-safe-path (soundklaus-playlist-username playlist)) "-"
          (soundklaus-safe-path (soundklaus-playlist-title playlist))))

(defun soundklaus-playlist-track-filename (track track-number total-tracks)
  "Return the filename of TRACK in a playlist.
The filename will be prefixed with TRACK-NUMBER and it's padding
will be derived from TOTAL-TRACKS."
  (let* ((padding (length (number-to-string total-tracks)))
         (padding (if (< padding 2) 2 padding))
         (pattern (concat "%0" (number-to-string padding) "d-%s")))
    (format pattern track-number (soundklaus-track-filename track))))

(defun soundklaus-playlist-time (playlist)
  "Return the PLAYLIST duration formatted as HH:MM:SS."
  (soundklaus-format-duration (soundklaus-playlist-duration playlist)))

(defun soundklaus-playlist-username (playlist)
  "Return the username of PLAYLIST."
  (soundklaus-user-username (soundklaus-playlist-user playlist)))

(defun soundklaus-playlist-fetch (playlist)
  "Fetch the PLAYLIST from the SoundCloud API."
  (let* ((request (soundklaus-make-request (soundklaus-path playlist)))
         (response (soundklaus-send-sync-request request)))
    (soundklaus-make-playlist (request-response-data response))))

(defun soundklaus-playlist-fetch-async (playlist callback)
  "Fetch the PLAYLIST from the SoundCloud API asynchronously and call CALLBACK."
  (soundklaus-send-async-request
   (soundklaus-make-request (soundklaus-path playlist))
   (lambda (request response)
     (funcall callback (soundklaus-make-playlist (request-response-data response))))))

(define-emms-source soundklaus-playlist (playlist)
  "An EMMS source for a SoundCloud PLAYLIST."
  (mapc 'emms-add-soundklaus-track
        (soundklaus-playlist-tracks playlist)))

(provide 'soundklaus-playlist)

;;; soundklaus-playlist.el ends here
