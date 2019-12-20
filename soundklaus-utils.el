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

(require 'cl-lib)
(require 'url-util)
(require 's)
(require 'soundklaus-custom)

(defun soundklaus-authenticate-message ()
  "Display a message that the user should authenticate."
  (message "Not authenticated with SoundCloud. Try %s."
           (soundklaus-bold "M-x soundklaus-connect")))

(cl-defun soundklaus-alist-merge (&rest lists)
  "Merge the assoc LISTS from left to right."
  (-reduce-from
   (lambda (initial item)
     (let* ((k (car item))
            (v (cdr item))
            (found (assoc k initial)))
       (if found
           (progn (setf (cdr found) v)
                  initial)
         (cons (cons k v) initial))))
   nil (apply #'append lists)))

(defun soundklaus-bold (text)
  "Add the `bold' face property to TEXT."
  (when text (propertize (format "%s" text) 'face 'bold)))

(defun soundklaus-auth-params ()
  "Return the query params used for authentication."
  (soundklaus-remove-nil-values
   `(("client_id" . ,soundklaus-client-id)
     ("oauth_token" . ,soundklaus-access-token))))

(defun soundklaus-help-bindings (command)
  "Return the formatted key bindings for `COMMAND'."
  (mapconcat (lambda (key) (propertize (key-description key) 'face 'bold))
             (where-is-internal command soundklaus-mode-map) ", "))

(defun soundklaus-help ()
  "Show help message about some key bindings."
  (message "Next (%s) / Previous (%s), Play (%s), Queue (%s), Seek forward (%s) / backward (%s)."
           (soundklaus-help-bindings #'soundklaus-next-media)
           (soundklaus-help-bindings #'soundklaus-prev-media)
           (soundklaus-help-bindings #'soundklaus-play-current)
           (soundklaus-help-bindings #'soundklaus-append-current)
           (soundklaus-help-bindings #'emms-seek-forward)
           (soundklaus-help-bindings #'emms-seek-backward)))

(defun soundklaus-string-alist (alist)
  "Convert all keys and values in ALIST to strings."
  (cl-loop for (k . v) in alist
           if (and k v)
           collect (cons (format "%s" k) (format "%s" v))))

(defun soundklaus-format-duration (duration-in-ms &optional always-show-hours)
  "Format DURATION-IN-MS in the HH:MM:SS format.
When ALWAYS-SHOW-HOURS always show the hour, otherwise only if
the length is greater than 60 minutes."
  (when duration-in-ms
    (let* ((duration (/ duration-in-ms 1000))
           (hours (floor (/ duration 3600)))
           (minutes (floor (/ (- duration (* hours 3600)) 60)))
           (seconds (floor (- duration (* hours 3600) (* minutes 60)))))
      (if (or always-show-hours (cl-plusp hours))
          (format "%02d:%02d:%02d" hours minutes seconds)
        (format "%02d:%02d" minutes seconds)))))

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

(defun soundklaus-slurp-instance (class assoc-list)
  "Make an instance of CLASS and initialize it's slots from the ASSOC-LIST."
  (let ((instance (make-instance class)))
    (mapc (lambda (slot)
	    (let* ((key (soundklaus-underscore slot))
		   (value (cdr (assoc key assoc-list))))
	      (set-slot-value instance slot value)))
	  (object-slots instance))
    instance))

(defun soundklaus-remove-nil-values (assoc-list)
  "Remove all elements from ASSOC-LIST where the value is nil."
  (delq nil (mapcar (lambda (element)
		      (if (cdr element)
			  element))
		    assoc-list)))

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

(defun soundklaus-safe-path (path)
  "Return the safe name of PATH."
  (let* ((path (replace-regexp-in-string "[^0-9A-Za-z]+" "_" path))
	 (path (replace-regexp-in-string "^_" "" path))
	 (path (replace-regexp-in-string "_$" "" path)))
    path))

(defun soundklaus-parse-callback (url)
  "Parse the code, token and scope from the OAuth2 callback URL."
  (condition-case nil
      (with-temp-buffer
	(let ((pattern (replace-regexp-in-string "://" ":/" soundklaus-redirect-url)))
	  (insert url)
	  (beginning-of-buffer)
	  (search-forward pattern)
	  (let ((url (url-generic-parse-url
		      (buffer-substring
		       (- (point) (length pattern))
		       (point-max)))))
	    (append (if (url-target url)
			(url-parse-query-string (url-target url)))
		    (if (url-path-and-query url)
			(url-parse-query-string (cdr (url-path-and-query url))))))))
    (error nil)))

(defun soundklaus-parse-duration (s)
  "Parse the duration from the string S and return the number of seconds."
  (let ((tokens `(("s" . ,1)
		  ("m" . ,60)
		  ("h" . ,(* 60 60))
		  ("d" . ,(* 60 60 24)))))
    (-reduce-from
     (lambda (memo s)
       (let* ((matches (s-match "\\([[:digit:]]+\\)\\([smhd]\\)" s))
	      (multiplicator (cdr (assoc (elt matches 2) tokens))))
	 (if multiplicator
	     (+ memo (* (string-to-number (elt matches 1))
			multiplicator))
	   memo)))
     0 (s-split-words s))))

(provide 'soundklaus-utils)

;;; soundklaus-utils.el ends here
