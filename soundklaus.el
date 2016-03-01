;;; soundklaus.el --- Play music on SoundCloud with Emacs via EMMS -*- lexical-binding: t -*-

;; Copyright © 2014-2016 r0man <roman@burningswell.com>

;; Author: r0man <roman@burningswell.com>
;; URL: https://github.com/r0man/soundklaus.el
;; Keywords: soundcloud, music, emms
;; Version: 0.1.0
;; Package-Requires: ((dash "2.12.1") (emacs "24") (emms "4.0") (s "1.10.0") (pkg-info "0.4") (cl-lib "0.5"))

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

;; Search and play tracks and playlists from SoundCloud in Emacs via EMMS.

;;; Usage:

;; M-x soundklaus-connect - Authenticate with SoundCloud
;; M-x soundklaus-activities - List activities on SoundCloud
;; M-x soundklaus-playlists - Find playlists on SoundCloud
;; M-x soundklaus-tracks - Find tracks on SoundCloud

;;; Code:

(require 'advice)
(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 'emms)
(require 's)
(require 'server)
(require 'soundklaus-collection)
(require 'soundklaus-custom)
(require 'soundklaus-playlist)
(require 'soundklaus-request)
(require 'soundklaus-track)
(require 'soundklaus-utils)
(require 'soundklaus-user)
(require 'widget)

(defvar soundklaus-current-collection nil
  "The current collection.")

(defun soundklaus-width ()
  "Return the width of the renderable content."
  (- (/ (frame-width) 2) (* soundklaus-padding 2)))

(defun soundklaus-horizontal-rule ()
  "Insert a horizontal rule into the buffer."
  (let ((width (- (soundklaus-width) soundklaus-padding)))
    (widget-insert
     (concat (make-string soundklaus-padding ?\s)
             (if (cl-plusp width)
                 (make-string width ?-))
             (make-string soundklaus-padding ?\s)
             "\n"))))

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

(defmacro soundklaus-with-access-token (&rest body)
  "Ensure that the `soundklaus-access-token` is not nil.
If `soundklaus-access-token` is not set raise an error, otherwise
evaluate BODY."
  `(if (s-blank? soundklaus-access-token)
       (message "Not authenticated with SoundCloud. Try M-x soundklaus-connect.")
     (progn ,@body)))

(defun soundklaus-download-track (track filename)
  "Download URL to FILENAME."
  (let ((url (soundklaus-track-stream-url track))
        (buffer (format "*soundklaus-download-%s*" (soundklaus-track-id track))))
    (cond
     ((file-directory-p filename)
      (error "Can't save track to existing directory, please enter a filename."))
     ((file-exists-p filename)
      (message "Track already downloaded to %s." filename))
     (t (progn
          (make-directory (file-name-directory filename) t)
          (let ((process (start-process "SoundCloud Download" buffer "curl" "-L" url "-o" filename)))
            (set-process-sentinel
             process
             (lambda (process event)
               (cond
                ((string= "exit" (process-status process))
                 (let ((buffer (get-buffer buffer)))
                   (if buffer (kill-buffer buffer)))
                 (soundklaus-tag-track track)
                 (message "Download of %s complete." (soundklaus-track-title track))))))
            (message "Downloading %s ..." (soundklaus-track-title track))
            process))))))

(defmethod soundklaus-download ((track soundklaus-track))
  (let ((filename (read-file-name
                   "Track filename: " soundklaus-download-dir
                   nil nil (soundklaus-track-filename track t))))
    (soundklaus-download-track track filename)))

(defmethod soundklaus-download ((playlist soundklaus-playlist))
  (let* ((directory (read-directory-name
                     "Playlist directory: " soundklaus-download-dir
                     nil nil (soundklaus-playlist-directory playlist)))
         (directory (expand-file-name (file-name-as-directory directory)))
         (tracks (soundklaus-playlist-tracks playlist)))
    (make-directory directory t)
    (cl-loop for track-index from 0 to (1- (length tracks))
             for track-number = (1+ track-index)
             for track = (elt tracks track-index)
             for filename = (soundklaus-playlist-track-filename track track-number (length tracks))
             for filename = (concat directory filename)
             do (soundklaus-download-track track filename))))

(defmethod soundklaus-play ((track soundklaus-track))
  (emms-play-soundklaus-track track))

(defmethod soundklaus-play ((playlist soundklaus-playlist))
  (emms-play-soundklaus-playlist playlist))

(defmethod soundklaus-permalink-url ((track soundklaus-track))
  (soundklaus-track-permalink-url track))

(defmethod soundklaus-permalink-url ((playlist soundklaus-playlist))
  (soundklaus-playlist-permalink-url playlist))

(defmethod soundklaus-playlist-add ((track soundklaus-track))
  (emms-add-soundklaus-track track))

(defmethod soundklaus-playlist-add ((playlist soundklaus-playlist))
  (emms-add-soundklaus-playlist playlist))

(defun soundklaus-append-current ()
  "Append the current SoundCloud media at point to the EMMS playlist."
  (interactive)
  (let ((media (soundklaus-current-media)))
    (if media (soundklaus-playlist-add media))))

(defun soundklaus-browse-current ()
  "Open the current media in a web browser."
  (interactive)
  (let ((media (soundklaus-current-media)))
    (when media
      (let ((url (soundklaus-permalink-url media)))
        (if url
            (browse-url url)
          (error "No permalink found."))))))

(defun soundklaus-like-current-track ()
  "Like or unlike the current track."
  (interactive)
  (let ((media (soundklaus-current-media)))
    (when (and media (same-class-p media 'soundklaus-track))
      (soundklaus-with-access-token
       (soundklaus-track-like media)
       (let ((inhibit-read-only t))
         (save-excursion
           (soundklaus-kill-current-media)
           (soundklaus-render media)))))))

(defun soundklaus-current-media-region ()
  "Return the start and end position of the current media in a list."
  (when (get-text-property (point) :soundklaus-media)
    (let* ((end (next-single-property-change (point) :soundklaus-media))
           (start (previous-single-property-change end :soundklaus-media)))
      (list start end))))

(defun soundklaus-kill-current-media ()
  "Kill the current media at point."
  (let* ((inhibit-read-only t)
         (positions (soundklaus-current-media-region)))
    (delete-region (car positions) (cadr positions))))

(defun soundklaus-current-media ()
  "Return the current SoundCloud track at point."
  (get-text-property (point) :soundklaus-media))

(defun soundklaus-download-current ()
  "Download the current SoundCloud media at point."
  (interactive)
  (let ((media (soundklaus-current-media)))
    (if media (soundklaus-download media))))

(defun soundklaus-kill-buffer ()
  "Kill the `soundklaus-buffer` and delete the current window."
  (interactive)
  (let ((buffer (get-buffer soundklaus-buffer)))
    (when (equal buffer (current-buffer))
      (delete-window))
    (when buffer
      (kill-buffer buffer))))

(defun soundklaus-next-media ()
  "Move point to the next SoundCloud track."
  (interactive)
  (let ((pos (next-single-property-change (point) :soundklaus-media)))
    (when pos
      (goto-char pos)
      (unless (soundklaus-current-media)
	(let ((pos (next-single-property-change pos :soundklaus-media)))
	  (if pos (goto-char pos)))))))

(defun soundklaus-prev-media ()
  "Move point to the next SoundCloud track."
  (interactive)
  (let ((pos (previous-single-property-change (point) :soundklaus-media)))
    (when pos
      (goto-char pos)
      (unless (soundklaus-current-media)
	(let ((pos (previous-single-property-change pos :soundklaus-media)))
	  (if pos (goto-char pos)))))))

(defun soundklaus-play-current ()
  "Play the current SoundCloud media at point."
  (interactive)
  (let ((media (soundklaus-current-media)))
    (if media (soundklaus-play media))))

(defun soundklaus-connect-url ()
  "Return the SoundCloud connect url."
  (soundklaus-request-url
   (soundklaus-make-request
    "/connect"
    :query-params
    `(("client_id" . ,soundklaus-client-id)
      ("redirect_uri" . ,soundklaus-redirect-url)
      ("response_type" . "code_and_token")
      ("scope" . "non-expiring")))))

(defadvice server-visit-files (around soundklaus-detect-protocol-server activate)
  "Handle SoundCloud OAuth2 callback URLs."
  (let* ((files (mapcar (lambda (file)
			  (let ((params (soundklaus-parse-callback (car file))))
			    (if params
				(let ((access-token (cadr (assoc "access_token" params)))
				      (error-code (cadr (assoc "error" params)))
				      (error-description (cadr (assoc "error_description" params))))
				  (when access-token
				    (setq soundklaus-access-token access-token)
				    (message "Authentication with SoundCloud complete."))
				  (when error-code
				    (message "Authentication with SoundCloud failed. %s"
					     (replace-regexp-in-string "+" " " (or error-description error-code))))
				  nil)
			      file)))
			(ad-get-arg 0)))
	 (files (delq nil files)))
    (when (not (null files))
      (ad-set-arg 0 files)
      ad-do-it)))

(defun soundklaus-render-row (left right &optional width-right)
  "Render a row with a LEFT and a RIGHT part.
Optional argument WIDTH-RIGHT is the width of the right argument."
  (let* ((width-right (or width-right (length (or right ""))))
	 (width-left (- (soundklaus-width)
			(- width-right 1)
			(* 2 soundklaus-padding)))
	 (padding (make-string soundklaus-padding ?\s)))
    (widget-insert (format
		    (format "%s%%-%s.%ss %%%s.%ss%s\n"
			    padding
			    width-left width-left
			    width-right width-right
			    padding)
		    left right))))

;; List item widgets

(defmethod soundklaus-render ((track soundklaus-track))
  (when (soundklaus-track-title track)
    (let ((start (point)))
      (soundklaus-render-row
       (propertize (soundklaus-track-header track) 'face 'bold)
       (propertize (or (soundklaus-track-time track) "") 'face 'bold))
      (soundklaus-horizontal-rule)
      (soundklaus-render-row
       (format "%s plays, %s downloads, %s comments and %s favorites."
               (soundklaus-track-playback-count track)
               (soundklaus-track-download-count track)
               (soundklaus-track-comment-count track)
               (soundklaus-track-favoritings-count track))
       (upcase (or (soundklaus-track-genre track) "Unknown")))
      (put-text-property start (point) :soundklaus-media track))))

(defmethod soundklaus-render ((playlist soundklaus-playlist))
  (when (soundklaus-playlist-title playlist)
    (let* ((playlist (soundklaus-playlist-fetch playlist))
           (start (point)))
      (soundklaus-render-row
       (propertize (concat (soundklaus-playlist-title playlist) " - "
                           (soundklaus-playlist-username playlist))
                   'face 'bold)
       (propertize (or (soundklaus-playlist-time playlist) "") 'face 'bold))
      (put-text-property start (point) :soundklaus-media playlist)
      (soundklaus-horizontal-rule)
      (cl-loop for n from 1 to (length (soundklaus-playlist-tracks playlist)) do
               (let ((track (elt (soundklaus-playlist-tracks playlist) (- n 1)))
                     (start (point)))
                 (soundklaus-render-row
                  (format "%02d  %s " n (soundklaus-track-title track))
                  (soundklaus-format-duration (soundklaus-track-duration track)))
                 (put-text-property start (point) :soundklaus-media track))))))

(defmethod soundklaus-render ((collection soundklaus-collection))
  (dolist (item (soundklaus-collection-content collection))
    (soundklaus-render item)
    (insert "\n")))

(defmacro soundklaus-with-widget (title &rest body)
  "Render a widget with TITLE and evaluate BODY."
  `(progn
     (set-buffer (get-buffer-create soundklaus-buffer))
     (switch-to-buffer-other-window soundklaus-buffer)
     (kill-all-local-variables)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (remove-overlays)
       (use-local-map widget-keymap)
       (widget-setup)
       (widget-minor-mode)
       (widget-insert (format "\n  >> %s\n\n" ,title))
       ,@body)
     (goto-char 0)
     (soundklaus-next-media)
     (soundklaus-mode)))

(defun soundklaus-seek-to (duration)
  "Seek the current player to DURATION."
  (interactive "MSeek to: ")
  (let ((seconds (soundklaus-parse-duration duration)))
    (if (numberp seconds)
	(emms-seek-to seconds))))

(defun soundklaus-render-next-page ()
  "Fetch and render the next page of the current collection."
  (let* ((collection soundklaus-current-collection)
         (request (soundklaus-collection-request collection))
         (next-request (soundklaus-next-request request))
         (response (soundklaus-send-sync-request next-request))
         (item-fn (soundklaus-collection-item-fn collection))
         (next-collection (soundklaus-make-collection next-request response item-fn)))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (soundklaus-render next-collection)
        (setq soundklaus-current-collection next-collection)))))

(defun soundklaus-pre-command-hook ()
  "Called after each command to trigger pagination when necessary."
  (when (string= (buffer-name (current-buffer)) soundklaus-buffer)
    (let ((percent (/ (* 100 (point)) (point-max))))
      (when (> percent 80)
        (soundklaus-render-next-page)))))

(defun soundklaus-setup-pagination (collection)
  "Setup pagination for COLLECTION."
  (setq soundklaus-current-collection collection)
  (add-hook 'pre-command-hook 'soundklaus-pre-command-hook t))

;;;###autoload
(defun soundklaus-activities ()
  "List activities on SoundCloud."
  (interactive)
  (soundklaus-with-access-token
   (let* ((request (soundklaus-make-request
                    "/me/activities"
                    :query-params
                    `(("limit" . ,soundklaus-activity-limit)
                      ("linked_partitioning" . 1)
                      ("offset" . 1))))
          (response (soundklaus-send-sync-request request))
          (collection (soundklaus-activities-collection request response)))
     (soundklaus-with-widget
      "ACTIVITIES"
      (soundklaus-render collection)
      ;; TODO: Paginate via linked_partitioning, not offset!
      ;; (soundklaus-setup-pagination collection)
      ))))

;;;###autoload
(defun soundklaus-connect ()
  "Connect with SoundCloud."
  (interactive)
  (browse-url (soundklaus-connect-url)))

;;;###autoload
(defun soundklaus-tracks (query)
  "List all tracks on SoundCloud matching QUERY."
  (interactive "MQuery: ")
  (let* ((request (soundklaus-make-request
                   "/tracks"
                   :query-params
                   `(("limit" . ,soundklaus-track-limit)
                     ("linked_partitioning" . 1)
                     ("offset" . 1)
                     ("q" . ,query))))
         (response (soundklaus-send-sync-request request))
         (collection (soundklaus-track-collection request response)))
    (soundklaus-with-widget
     "TRACKS"
     (soundklaus-render collection)
     (soundklaus-setup-pagination collection))))

;;;###autoload
(defun soundklaus-playlists (query)
  "List all playlists on SoundCloud matching QUERY."
  (interactive "MQuery: ")
  (let* ((request (soundklaus-make-request
                   "/playlists"
                   :query-params
                   `(("limit" . ,soundklaus-playlist-limit)
                     ("linked_partitioning" . 1)
                     ("offset" . 1)
                     ("q" . ,query)
                     ("representation" . "id"))))
         (response (soundklaus-send-sync-request request))
         (collection (soundklaus-playlist-collection request response)))
    (soundklaus-with-widget
     "PLAYLISTS"
     (soundklaus-render collection)
     (soundklaus-setup-pagination collection))))

;;;###autoload
(defun soundklaus-my-playlists ()
  "List your playlists on SoundCloud."
  (interactive)
  (soundklaus-with-access-token
   (let* ((request (soundklaus-make-request
                    "/me/playlists"
                    :query-params
                    `(("limit" . ,soundklaus-playlist-limit)
                      ("linked_partitioning" . 1)
                      ("offset" . 1)
                      ("representation" . "id"))))
          (response (soundklaus-send-sync-request request))
          (collection (soundklaus-playlist-collection request response)))
     (soundklaus-with-widget
      "MY PLAYLISTS"
      (soundklaus-render collection)
      (soundklaus-setup-pagination collection)))))

;;;###autoload
(defun soundklaus-my-favorite ()
  "List your favorite tracks on SoundCloud."
  (interactive)
  (soundklaus-with-access-token
   (let* ((request (soundklaus-make-request
                    "/me/favorites"
                    :query-params
                    `(("limit" . ,soundklaus-track-limit)
                      ("linked_partitioning" . 1)
                      ("offset" . 1))))
          (response (soundklaus-send-sync-request request))
          (collection (soundklaus-track-collection request response)))
     (soundklaus-with-widget
      "MY FAVORITE TRACKS"
      (soundklaus-render collection)
      (soundklaus-setup-pagination collection)))))

;;;###autoload
(defun soundklaus-my-tracks ()
  "List your tracks on SoundCloud."
  (interactive)
  (soundklaus-with-access-token
   (let* ((request (soundklaus-make-request
                    "/me/tracks"
                    :query-params
                    `(("limit" . ,soundklaus-track-limit)
                      ("linked_partitioning" . 1)
                      ("offset" . 1))))
          (response (soundklaus-send-sync-request request))
          (collection (soundklaus-track-collection request response)))
     (soundklaus-with-widget
      "MY TRACKS"
      (soundklaus-render collection)
      (soundklaus-setup-pagination collection)))))

;;;###autoload
(defun soundklaus-desktop-entry-save ()
  "Install the SoundKlaus desktop entry for the X Window System."
  (interactive)
  (let ((dir (file-name-directory soundklaus-desktop-entry)))
    (unless (file-exists-p dir)
      (make-directory dir))
    (with-temp-buffer
      (insert (s-join "\n" '("[Desktop Entry]"
			     "Name=SoundKlaus"
			     "Exec=emacsclient %u"
			     "Icon=emacs-icon"
			     "Type=Application"
			     "Terminal=false"
			     "MimeType=x-scheme-handler/soundklaus;")))
      (write-file soundklaus-desktop-entry))))

(defvar soundklaus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<") 'emms-seek-backward)
    (define-key map (kbd "<return>") 'soundklaus-play-current)
    (define-key map (kbd ">") 'emms-seek-forward)
    (define-key map (kbd "C-n") 'soundklaus-next-media)
    (define-key map (kbd "C-p") 'soundklaus-prev-media)
    (define-key map (kbd "a") 'soundklaus-append-current)
    (define-key map (kbd "b") 'soundklaus-browse-current)
    (define-key map (kbd "d") 'soundklaus-download-current)
    (define-key map (kbd "f") 'soundklaus-like-current-track)
    (define-key map (kbd "g") 'emms-playlist-mode-go)
    (define-key map (kbd "n") 'soundklaus-next-media)
    (define-key map (kbd "p") 'soundklaus-prev-media)
    (define-key map (kbd "q") 'soundklaus-kill-buffer)
    map)
  "Keymap for SoundKlaus mode.")

(define-derived-mode soundklaus-mode special-mode "SoundKlaus"
  "Play music on SoundCloud with Emacs via EMMS.
\\{soundklaus-mode-map}"
  :keymap 'soundklaus-mode-map
  :group 'soundklaus-mode)

(provide 'soundklaus)
(run-hooks 'soundklaus-load-hook)

;;; soundklaus.el ends here
