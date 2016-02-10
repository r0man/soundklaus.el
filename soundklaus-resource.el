;;; soundklaus-resource.el --- SoundKlaus resources -*- lexical-binding: t -*-

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

;; SoundKlaus resources

(require 'dash)
(require 'eieio)
(require 's)

;;; Code:

(defun soundklaus-define-slot (name attribute)
  "Return the s-expression to define a resource slot.
NAME is the name of the slot and ATTRIBUTE the slot options."
  `(,(car attribute)
    :initarg ,(intern (format ":%s" (car attribute)))
    :accessor ,(intern (format "soundklaus-%s-%s" name (car attribute)))
    :documentation ,(cadr attribute)))

(defun soundklaus-define-class (name doc slots)
  "Return the s-expression to define a resource class with the name NAME.
DOC is the documentation string of the class and SLOTS a list of
resource attributes."
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

(defun soundklaus-replace-slots (pattern slots &rest values)
  "Interpolate PATTERN and replace each name in SLOTS with its VALUES."
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
    slots values)))

(defun soundklaus-intern (name &rest args)
  "Intern NAME in the soundklaus namespace as a symbol.
ARGS is a list of strings appended to NAME."
  (unless (s-blank? name)
    (intern (apply 'format (format "soundklaus-%s" name) args))))

(defun soundklaus-define-path (name pattern)
  "Return the s-expression to define a path method of a resource.
NAME is the name of the resource and PATTERN is used to
interpolate the arguments passed to the generated method."
  (let ((slots (soundklaus-path-symbols pattern))
	(resource (cl-gensym "resource-")))
    `(defmethod ,(soundklaus-intern "path") ((,resource ,(soundklaus-intern name)))
       (soundklaus-replace-slots ,pattern ',slots ,resource))))

(defmacro define-soundklaus-resource (name pattern doc slots)
  "Define a SoundCloud resource with the name NAME.
PATTERN is is used to build the path to the resource, DOC is the
documentation string, and SLOTS the attributes of the resource."
  `(progn
     ,(soundklaus-define-class name doc slots)
     ,(soundklaus-define-path name pattern)))

(provide 'soundklaus-resource)

;;; soundklaus-resource.el ends here
