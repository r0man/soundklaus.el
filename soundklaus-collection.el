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
   (next
    :initarg :next
    :accessor soundklaus-collection-next
    :documentation "The URL to the next page of the collection.")
   (item-fn
    :initarg :item-fn
    :accessor soundklaus-collection-item-fn
    :documentation "The function to build an item of the collection.")
   (request
    :initarg :request
    :accessor soundklaus-collection-request
    :documentation "The request used request the collection."))
  "A collection of resources on SoundCloud.")

(defun soundklaus-make-collection (request response item-fn)
  "Make a SoundCloud track collection from REQUEST and RESPONSE."
  (make-instance
   'soundklaus-collection
   :content (delq nil (mapcar
                       (lambda (resource)
                         (funcall item-fn resource))
                       (cdr (assoc 'collection response))))
   :next (cdr (assoc 'next_href response))
   :item-fn item-fn
   :request request))

(defun soundklaus-make-activity-item (resource)
  "Make a SoundCloud activity item from RESOURCE."
  (let ((type (cdr (assoc 'type resource)))
        (origin (cdr (assoc 'origin resource))))
    (cond
     ((equal type "playlist")
      (soundklaus-make-playlist origin))
     ((equal type "playlist-repost")
      (soundklaus-make-playlist origin))
     ((equal type "track")
      (soundklaus-make-track origin))
     ((equal type "track-repost")
      (soundklaus-make-track origin)))))

(defun soundklaus-activities-collection (request response)
  "Make a SoundCloud playlist collection from REQUEST and RESPONSE."
  (soundklaus-make-collection request response #'soundklaus-make-activity-item))

(defun soundklaus-track-collection (request response)
  "Make a SoundCloud track collection from REQUEST and RESPONSE."
  (soundklaus-make-collection request response #'soundklaus-make-track))

(defun soundklaus-playlist-collection (request response)
  "Make a SoundCloud playlist collection from REQUEST and RESPONSE."
  (soundklaus-make-collection request response #'soundklaus-make-playlist))

(provide 'soundklaus-collection)

;;; soundklaus-collection.el ends here
