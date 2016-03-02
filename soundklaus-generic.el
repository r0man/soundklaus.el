;;; soundklaus-generic.el --- SoundKlaus generic methods -*- lexical-binding: t -*-

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

;; SoundKlaus generic methods

;;; Code:

(defgeneric soundklaus-permalink-url (media)
  "Return the permalink URL of the SoundCloud MEDIA.")

(defgeneric soundklaus-download (media)
  "Download the MEDIA from SoundCloud.")

(defgeneric soundklaus-play (media)
  "Play the SoundCloud MEDIA.")

(defgeneric soundklaus-playlist-add (media)
  "Insert the SoundCloud MEDIA into the EMMS playlist.")

(defgeneric soundklaus-render (media)
  "Render the SoundCloud MEDIA as a list item.")

(defgeneric soundklaus-path (resource)
  "Returns the path to the RESOURCE on SoundCloud.")

(provide 'soundklaus-generic)

;;; soundklaus-generic.el ends here
