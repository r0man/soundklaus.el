(require 'soundklaus-playlist)
(require 'soundklaus-track)
(require 'soundklaus-user)

(defvar soundklaus-example-track-url
  "https://api.soundcloud.com/tracks/40258263")

(defvar soundklaus-example-user
  (make-instance 'soundklaus-user :id 8928131 :username "KaterMukke"))

(defvar soundklaus-example-track 
  (make-instance
   'soundklaus-track
   :id 40258263
   :user soundklaus-example-user
   :title "Acid Pauli & Nancy - iBang - Katermukke"
   :stream-url "https://api.soundcloud.com/tracks/40258263/stream"
   :duration 502080
   :genre "Tech-house"
   :playback-count 82043
   :download-count 0
   :comment-count 117
   :favoritings-count 2287
   :user-favorite nil))

(defvar soundklaus-example-playlist
  (make-instance
   'soundklaus-playlist
   :id 1914963
   :title "CS009   ACID PAULI - Mst LP"
   :user soundklaus-example-user
   :duration 2508334))

(provide 'soundklaus-examples)
