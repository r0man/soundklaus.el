;;; soundklaus-custom.el --- SoundKlaus customization -*- lexical-binding: t -*-

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

;; SoundKlaus customization

;;; Code:

(defcustom soundklaus-access-token nil
  "The OAuth2 access token for the SoundCloud API."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-buffer "*soundklaus*"
  "The SoundKlaus buffer name."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-client-id "988875d70be466a2dd1bfab120c0a306"
  "The SoundKlaus OAuth2 client id."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-config-file "~/.soundklaus.el"
  "The SoundKlaus configuration file."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-desktop-entry "~/.local/share/applications/soundklaus.desktop"
  "The filename of the X Window System desktop entry."
  :type 'integer
  :group 'soundklaus-mode)

(defcustom soundklaus-download-dir "~/Music/soundcloud"
  "The directory of the download directory."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-redirect-url "soundklaus://oauth/callback"
  "The SoundCloud OAuth2 redirect URL."
  :type 'string
  :group 'soundklaus-mode)

(defcustom soundklaus-activity-limit 40
  "The number of activities to fetch from SoundCloud at once."
  :type 'integer
  :group 'soundklaus-mode)

(defcustom soundklaus-helm-track-limit 40
  "The number of tracks to fetch from SoundCloud when using Helm."
  :type 'integer
  :group 'soundklaus-mode)

(defcustom soundklaus-helm-playlist-limit 40
  "The number of playlists to fetch from SoundCloud when using Helm."
  :type 'integer
  :group 'soundklaus-mode)

(defcustom soundklaus-show-help t
  "Show help when opening the `*soundklaus*' buffer."
  :type 'boolean
  :group 'soundklaus-mode)

(defcustom soundklaus-playlist-limit 10
  "The number of playlists to fetch from SoundCloud."
  :type 'integer
  :group 'soundklaus-mode)

(defcustom soundklaus-track-limit 40
  "The number of tracks to fetch from SoundCloud."
  :type 'integer
  :group 'soundklaus-mode)

(defcustom soundklaus-padding 2
  "The number of columns used for padding on the left side of the buffer."
  :type 'integer
  :group 'soundklaus-mode)

(provide 'soundklaus-custom)

;;; soundklaus-custom.el ends here
