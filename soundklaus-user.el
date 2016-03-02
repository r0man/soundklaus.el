;;; soundklaus-user.el --- SoundKlaus users -*- lexical-binding: t -*-

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

;; SoundKlaus users

;;; Code:

(require 'emms)
(require 'soundklaus-resource)
(require 'soundklaus-utils)

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

(defun soundklaus-make-user (assoc-list)
  "Make a SoundCloud user instance from an ASSOC-LIST."
  (soundklaus-slurp-instance 'soundklaus-user assoc-list))

(provide 'soundklaus-user)

;;; soundklaus-user.el ends here
