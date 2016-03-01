;;; soundklaus-request.el --- The SoundKlaus HTTP request class -*- lexical-binding: t -*-

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

;;  The SoundKlaus HTTP request class.

;;; Code:

(require 'eieio)
(require 'json)
(require 'request)
(require 'soundklaus-custom)
(require 'soundklaus-utils)

;; The SoundCloud HTTP request

(defclass soundklaus-request ()
  ((headers
    :initarg :headers
    :accessor soundklaus-request-headers
    :documentation "The HTTP headers of the request.")
   (method
    :initarg :method
    :accessor soundklaus-request-method
    :documentation "The HTTP method of the request.")
   (scheme
    :initarg :scheme
    :accessor soundklaus-request-scheme
    :documentation "The scheme of the request.")
   (server-name
    :initarg :server-name
    :accessor soundklaus-request-server-name
    :documentation "The HTTP server hostname of the request.")
   (server-port
    :initarg :server-port
    :accessor soundklaus-request-server-port
    :documentation "The HTTP server port of the request.")
   (uri
    :initarg :uri
    :accessor soundklaus-request-uri
    :documentation "The relative URL path of the request.")
   (query-params
    :initarg :query-params
    :accessor soundklaus-request-query-params
    :documentation "The query parameters of the HTTP request."))
  "A class representing a HTTP request.")

(defun soundklaus-request-params-as-strings (request)
  "Return the query params of REQUEST with keys and values all
converted to strings."
  (soundklaus-string-alist (soundklaus-request-query-params request)))

(cl-defun soundklaus-make-request (uri &key headers method scheme server-name server-port query-params)
  "Return a HTTP request of METHOD to URL with query parameters QUERY-PARAMS."
  (let ((scheme (or scheme 'https)))
    (make-instance 'soundklaus-request
                   :headers (append headers '(("Accept" . "application/json")))
                   :method (or method "GET")
                   :scheme scheme
                   :server-name (or server-name "api.soundcloud.com")
                   :server-port (cond
                                 (server-port server-port)
                                 ((equal scheme 'http) 80)
                                 ((equal scheme 'https) 443)
                                 (t 443))
                   :uri uri
                   :query-params
                   (append query-params
                           `(("client_id" . ,soundklaus-client-id)
                             ("oauth_token" . ,soundklaus-access-token))))))

(defun soundklaus-request-url (request &optional include-params)
  "Return the formatted URL in REQUEST."
  (let ((scheme (soundklaus-request-scheme request))
        (port (soundklaus-request-server-port request)))
    (concat (symbol-name scheme) "://" (soundklaus-request-server-name request)
            (when port
              (concat ":" (number-to-string port)))
            (soundklaus-request-uri request)
            (when include-params
              (concat "?" (request--urlencode-alist
                           (soundklaus-request-params-as-strings request)))))))

(defun soundklaus-request-parser ()
  "Parse the body of a HTTP response from the current buffer."
  (encode-coding-region (point-min) (point-max) 'latin-1)
  (decode-coding-region (point-min) (point-max) 'utf-8)
  (goto-char (point-min))
  (json-read))

(defun soundklaus-send-sync-request (http-request)
  "Send the HTTP-REQUEST."
  (request-response-data
   (request
    (soundklaus-request-url http-request)
    :params (soundklaus-request-params-as-strings http-request)
    :parser #'soundklaus-request-parser
    :sync t)))

(defun soundklaus-next-request (request)
  "Return the HTTP REQUEST to return the next page of a response."
  (let* ((query-params (soundklaus-request-query-params request))
	 (limit-alist (assoc "limit" query-params))
	 (limit (or (cdr limit-alist) 10))
	 (offset-alist (assoc "offset" query-params))
	 (offset (+ (or (cdr offset-alist) 0) limit)))
    (soundklaus-make-request
     (soundklaus-request-uri request)
     :query-params
     (append (->> query-params
                  (delq limit-alist)
                  (delq offset-alist))
     	     `(("limit" . ,limit)
     	       ("offset" . ,offset))))))

(provide 'soundklaus-request)

;;; soundklaus-request.el ends here
