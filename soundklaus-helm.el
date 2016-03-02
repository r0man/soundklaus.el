;;; soundklaus-helm.el --- SoundKlaus Helm sources -*- lexical-binding: t -*-

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

;; SoundKlaus Helm sources

;;; Code:

(require 'helm)
(require 'soundklaus-custom)
(require 'soundklaus-generic)
(require 'soundklaus-playlist)
(require 'soundklaus-request)
(require 'soundklaus-track)

(defun soundklaus-helm-track-title (track)
  "Return the Helm title for TRACK."
  (format "%s - %s"
          (soundklaus-track-title track)
          (soundklaus-track-username track)))

(defun soundklaus-helm-playlist-title (playlist)
  "Return the Helm title for PLAYLIST."
  (format "%s - %s"
          (soundklaus-playlist-title playlist)
          (soundklaus-playlist-username playlist)))

(defun soundklaus-helm-track-search ()
  "Search tracks on SoundCloud matching `helm-pattern'."
  (let* ((request (soundklaus-make-request
                   "/tracks"
                   :query-params
                   `(("limit" . ,soundklaus-helm-track-limit)
                     ("linked_partitioning" . 1)
                     ("offset" . 1)
                     ("q" . ,helm-pattern))))
         (response (soundklaus-send-sync-request request))
         (collection (soundklaus-track-collection request response)))
    (mapcar (lambda (track)
              (cons (soundklaus-helm-track-title track) track))
            (soundklaus-collection-content collection))))

(defun soundklaus-helm-playlist-search ()
  "Search playlists on SoundCloud matching `helm-pattern'."
  (let* ((request (soundklaus-make-request
                   "/playlists"
                   :query-params
                   `(("limit" . ,soundklaus-helm-playlist-limit)
                     ("linked_partitioning" . 1)
                     ("offset" . 1)
                     ("q" . ,helm-pattern)
                     ("representation" . "compact"))))
         (response (soundklaus-send-sync-request request))
         (collection (soundklaus-playlist-collection request response)))
    (mapcar (lambda (playlist)
              (cons (soundklaus-helm-playlist-title playlist) playlist))
            (soundklaus-collection-content collection))))

(defun soundklaus-helm-track-action (actions track)
  "Return the Helm ACTIONS for the TRACK."
  `((,(format "Play Track - %s" (soundklaus-helm-track-title track)) . soundklaus-play)
    (,(format "Queue Track - %s" (soundklaus-helm-track-title track)) . soundklaus-playlist-add)
    ("Show Track Metadata" . pp)))

(defun soundklaus-helm-playlist-action (actions playlist)
  "Return the Helm ACTIONS for the PLAYLIST."
  `((,(format "Play Playlist - %s" (soundklaus-helm-playlist-title playlist)) . soundklaus-play)
    (,(format "Queue Playlist - %s" (soundklaus-helm-playlist-title playlist)) . soundklaus-playlist-add)
    ("Show Playlist Metadata" . pp)))

(defvar soundklaus-helm-track-source
  (helm-build-sync-source "SoundCloud Tracks"
    :action #'soundklaus-play
    :action-transformer #'soundklaus-helm-track-action
    :candidates #'soundklaus-helm-track-search
    :delayed t
    :volatile t)
  "The Helm source for SoundCloud tracks.")

(defvar soundklaus-helm-playlist-source
  (helm-build-sync-source "SoundCloud Playlists"
    :action (lambda (playlist) (soundklaus-play (soundklaus-playlist-add playlist)))
    :action-transformer #'soundklaus-helm-playlist-action
    :candidates #'soundklaus-helm-playlist-search
    :delayed t
    :volatile t)
  "The Helm source for SoundCloud playlists.")

(defun helm-soundklaus-tracks ()
  "Search tracks on SoundCloud with Helm."
  (interactive)
  (helm :buffer "*Helm SoundKlaus Tracks*"
        :sources '(soundklaus-helm-track-source)))

(defun helm-soundklaus-playlists ()
  "Search playlists on SoundCloud with Helm."
  (interactive)
  (helm :buffer "*Helm SoundKlaus Playlists*"
        :sources '(soundklaus-helm-playlist-source)))

(defun helm-soundklaus ()
  "Search tracks and playlists on SoundCloud with Helm."
  (interactive)
  (helm :buffer "*Helm SoundKlaus*"
        :sources '(soundklaus-helm-track-source
                   soundklaus-helm-playlist-source)))

(provide 'soundklaus-helm)

;;; soundklaus-helm.el ends here
