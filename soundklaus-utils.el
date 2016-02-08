;;; soundklaus-utils.el --- SoundKlaus utility functions -*- lexical-binding: t -*-

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

;; The SoundKlaus utility functions.

;;; Code:

(require 'url-util)
(require 's)

(defun soundklaus-transform (arg transform-fn)
  "Transform ARG with the transformation function TRANSFORM-FN.
ARG can be an assoc list, hash table, string or a symbol.  If ARG
is an assoc list or hash table only the keys will be
transformed."
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
  "Replace each underscore in ARG with a dash.
ARG can be an association list, hash table, string or a
symbol.  If ARG is an association list or hash table only the keys
will be dasherized."
  (soundklaus-transform arg (lambda (string) (replace-regexp-in-string "_" "-" string))))

(defun soundklaus-underscore (arg)
  "Replace each dash in ARG with an underscore.
ARG can be an association list, hash table, string or a
symbol.  If ARG is an association list or hash table only the keys
will be underscored."
  (soundklaus-transform arg (lambda (string) (replace-regexp-in-string "-" "_" string))))

(defun soundklaus-url-encode (params)
  "Return a URL encoded string of the PARAMS.
PARAMS can be a number, string, symbol or an association list and
the elements are joined with the ampersand character."
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

(provide 'soundklaus-utils)

;;; soundklaus-utils.el ends here
