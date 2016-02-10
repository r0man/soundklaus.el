;;; soundklaus-collection.el --- SoundKlaus resource collection -*- lexical-binding: t -*-

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

;; SoundKlaus resource collection

;;; Code:

(require 'emms)
(require 'soundklaus-playlist)
(require 'soundklaus-track)

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
                                   (print type)
				   (cond
				    ((equal type "playlist")
				     (soundklaus-make-playlist origin))
                                    ((equal type "playlist-repost")
				     (soundklaus-make-playlist origin))
				    ((equal type "track")
				     (soundklaus-make-track origin))
                                    ((equal type "track-repost")
				     (soundklaus-make-track origin)))))
			       (cdr (assoc 'collection assoc-list)))))
      collection)))

(provide 'soundklaus-collection)

;;; soundklaus-collection.el ends here
