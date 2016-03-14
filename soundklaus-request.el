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

(require 'cl-lib)
(require 'eieio)
(require 'json)
(require 'request)
(require 'soundklaus-custom)
(require 'soundklaus-utils)
(require 'url-parse)

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
  "Return the query params of REQUEST.
The keys and values of all params are converted to strings."
  (soundklaus-string-alist (soundklaus-request-query-params request)))

(defun soundklaus-protocol-port (scheme)
  "Return the server port for SCHEME."
  (let ((scheme (downcase (format "%s" scheme))))
    (cond
     ((string= scheme "http") 80)
     ((string= scheme "https") 443)
     (t nil))))

(cl-defun soundklaus-make-request (uri &key headers method scheme server-name server-port query-params)
  "Return a HTTP request of METHOD to URL with query parameters QUERY-PARAMS."
  (let ((scheme (or scheme 'https)))
    (make-instance
     'soundklaus-request
     :headers (soundklaus-alist-merge '(("Accept" . "application/json")) headers)
     :method (or method "GET")
     :query-params (soundklaus-alist-merge (soundklaus-auth-params) query-params)
     :scheme scheme
     :server-name (or server-name "api.soundcloud.com")
     :server-port (or server-port (soundklaus-protocol-port scheme) 443)
     :uri uri)))

(defun soundklaus-next-offset-request (request)
  "Return the next offset based pagination REQUEST object."
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

(defun soundklaus-next-cursor-request (request)
  "Return the next cursor based pagination REQUEST object."
  (let* ((params (soundklaus-request-query-params (clone request)))
         (params (soundklaus-alist-merge params (soundklaus-auth-params))))
    (set-slot-value request 'query-params params)
    request))

(defun soundklaus-parse-params (params)
  "Parse the query PARAMS."
  (unless (s-blank? params)
    (cl-map 'list (lambda (param)
                    (let ((kv (s-split "=" param)))
                      (cons (car kv) (cadr kv))))
            (s-split "&" params))))

(defun soundklaus-parse-url (url)
  "Parse the URL and return a `soundklaus-request' instance."
  (let* ((spec (url-generic-parse-url url))
         (scheme (url-type spec))
         (path-query (url-path-and-query spec)))
    (make-instance
     'soundklaus-request
     :headers '(("Accept" . "application/json"))
     :method "GET"
     :scheme (intern scheme)
     :server-name (url-host spec)
     :server-port (or (url-port spec) (soundklaus-protocol-port scheme))
     :uri (car path-query)
     :query-params (soundklaus-parse-params (cdr path-query)))))

(defun soundklaus-request-url (request &optional include-params)
  "Return the formatted URL in REQUEST.
Optional argument INCLUDE-PARAMS Append the query params to the url."
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
  (let ((json-false nil))
    (json-read)))

(defun soundklaus-show-http-error (response)
  "Print the HTTP error response."
  (message "SoundCloud HTTP (%s) %s: %s"
           (soundklaus-bold (number-to-string (request-response-status-code response)))
           (request-response-symbol-status response)
           (request-response-url response)))

(defun soundklaus-send-sync-request (http-request)
  "Send the HTTP-REQUEST asynchronously."
  (with-local-quit
    (request
     (soundklaus-request-url http-request)
     :error (cl-function
             (lambda (&key response &allow-other-keys)
               (soundklaus-show-http-error response)))
     :headers '(("Accept-Charset" . "UTF-8"))
     :params (soundklaus-request-params-as-strings http-request)
     :parser #'soundklaus-request-parser
     :sync t
     :type (soundklaus-request-method http-request))))

(defun soundklaus-send-async-request (http-request callback)
  "Send the HTTP-REQUEST asynchronously and call CALLBACK with the response."
  (request
   (soundklaus-request-url http-request)
   :error (cl-function
           (lambda (&key response &allow-other-keys)
             (soundklaus-show-http-error response)))
   :headers '(("Accept-Charset" . "UTF-8"))
   :params (soundklaus-request-params-as-strings http-request)
   :parser #'soundklaus-request-parser
   :success (cl-function
             (lambda (&key response &allow-other-keys)
               (funcall callback http-request response)))
   :type (soundklaus-request-method http-request)))

(provide 'soundklaus-request)

;;; soundklaus-request.el ends here
