(require 'soundklaus)
(require 'ert)

(defvar soundklaus-example-track-url
  "https://api.soundcloud.com/tracks/40258263")

(defvar soundklaus-example-user
  (make-instance 'soundklaus-user :id 8928131 :username "KaterMukke"))

(defvar soundklaus-example-track
  (make-instance 'soundklaus-track
		 :id 40258263
		 :user soundklaus-example-user
		 :title "Acid Pauli & Nancy - iBang - Katermukke"
		 :stream-url "https://api.soundcloud.com/tracks/40258263/stream"
		 :duration 502080
		 :genre "Tech-house"
		 :playback-count 82043
		 :download-count 0
		 :comment-count 117
		 :favoritings-count 2287))

(ert-deftest soundklaus-remove-nil-values-test ()
  (should (equal `(("client_id" . ,soundklaus-client-id))
		 (soundklaus-remove-nil-values
		  `(("client_id" . ,soundklaus-client-id)
		    ("oauth_token" . ,soundklaus-access-token))))))

(ert-deftest soundklaus-make-user-test ()
  (let ((user soundklaus-example-user))
    (should (equal 8928131 (soundklaus-user-id user)))
    (should (equal "KaterMukke" (soundklaus-user-username user)))))

(ert-deftest soundklaus-make-track-test ()
  (let ((track soundklaus-example-track))
    (should (equal 40258263 (soundklaus-track-id track)))
    (should (equal "Acid Pauli & Nancy - iBang - Katermukke" (soundklaus-track-title track)))))

(ert-deftest soundklaus-track-duration-secs-test ()
  (let ((track soundklaus-example-track))
    (should (equal 502 (soundklaus-track-duration-secs track)))))

(ert-deftest soundklaus-track-header-test ()
  (should (equal "Acid Pauli & Nancy - iBang - Katermukke - KaterMukke"
		 (soundklaus-track-header soundklaus-example-track))))

(ert-deftest soundklaus-track-time-test ()
  (should (equal "00:08:22" (soundklaus-track-time soundklaus-example-track))))

(ert-deftest soundklaus-track-stream-url-test ()
  (should (equal (format "https://api.soundcloud.com/tracks/40258263/stream?client_id=%s" soundklaus-client-id)
		 (soundklaus-track-stream-url soundklaus-example-track)))
  (let ((soundklaus-access-token "1-82657-450979-f92c55f37ce760776"))
    (should (equal (format "https://api.soundcloud.com/tracks/40258263/stream?client_id=%s&oauth_token=%s"
			   soundklaus-client-id
			   soundklaus-access-token)
		   (soundklaus-track-stream-url soundklaus-example-track)))))

(ert-deftest soundklaus-format-duration-test ()
  (should (equal "00:00:00" (soundklaus-format-duration 0)))
  (should (equal "00:00:01" (soundklaus-format-duration 1000)))
  (should (equal "00:01:00" (soundklaus-format-duration (* 60 1000))))
  (should (equal "01:00:00" (soundklaus-format-duration (* 60 60 1000)))))

(ert-deftest soundklaus-url-encode-test ()
  (should (equal (soundklaus-url-encode "") ""))
  (should (equal (soundklaus-url-encode "x") "x"))
  (should (equal (soundklaus-url-encode "=") "%3D"))
  (should (equal (soundklaus-url-encode "a 1") "a%201"))
  (should (equal (soundklaus-url-encode '(a-1 1)) "a-1=1"))
  (should (equal (soundklaus-url-encode '((a 1) (b nil))) "a=1"))
  (should (equal (soundklaus-url-encode '((a . 1) (b . nil))) "a=1"))
  (should (equal (soundklaus-url-encode '((a-1 1) (b-2 2))) "a-1=1&b-2=2"))
  (should (equal (soundklaus-url-encode '((a-1 . 1) (b-2 . 2))) "a-1=1&b-2=2")))

(ert-deftest soundklaus-dasherize-test ()
  (should (equal "" (soundklaus-dasherize "")))
  (should (equal "-" (soundklaus-dasherize "-")))
  (should (equal "-" (soundklaus-dasherize "_")))
  (should (equal "client-id" (soundklaus-dasherize "client_id")))
  (should (equal 'client-id (soundklaus-dasherize 'client_id)))
  (should (equal :client-id (soundklaus-dasherize :client_id)))
  (should (equal '(client-id . "client_id") (soundklaus-dasherize '(client_id . "client_id"))))
  (should (equal (soundklaus-dasherize '((client_id . "client_id") ("client_secret" "client_secret")))
                 '((client-id . "client_id") ("client-secret" "client_secret"))))
  (let ((hash (make-hash-table)))
    (puthash "client_id" "client-id" hash)
    (puthash 'client_secret "client-secret" hash)
    (puthash :grant_type "grant-type" hash)
    (let ((hash (soundklaus-dasherize hash)))
      (should (equal 3 (hash-table-count hash)))
      (should (equal "client-id" (gethash "client-id" hash)))
      (should (equal "client-secret" (gethash 'client-secret hash)))
      (should (equal "grant-type" (gethash :grant-type hash))))))

(ert-deftest soundklaus-underscore-test ()
  (should (equal "" (soundklaus-underscore "")))
  (should (equal "_" (soundklaus-underscore "_")))
  (should (equal "_" (soundklaus-underscore "-")))
  (should (equal "client_id" (soundklaus-underscore "client-id")))
  (should (equal 'client_id (soundklaus-underscore 'client-id)))
  (should (equal :client_id (soundklaus-underscore :client-id)))
  (should (equal '(client_id . "client-id") (soundklaus-underscore '(client-id . "client-id"))))
  (should (equal (soundklaus-underscore '((client-id . "client_id") ("client-secret" "client_secret")))
                 '((client_id . "client_id") ("client_secret" "client_secret"))))
  (let ((hash (make-hash-table)))
    (puthash "client-id" "client-id" hash)
    (puthash 'client-secret "client-secret" hash)
    (puthash :grant-type "grant-type" hash)
    (let ((hash (soundklaus-underscore hash)))
      (should (equal 3 (hash-table-count hash)))
      (should (equal "client-id" (gethash "client_id" hash)))
      (should (equal "client-secret" (gethash 'client_secret hash)))
      (should (equal "grant-type" (gethash :grant_type hash))))))

(ert-deftest soundklaus-define-slot-test ()
  (should (equal (soundklaus-define-slot 'user '(permalink "The permalink of the user"))
		 '(permalink :initarg :permalink
			     :accessor soundklaus-user-permalink
			     :documentation "The permalink of the user"))))

(ert-deftest soundklaus-user-make-instance ()
  (let ((user (make-instance 'soundklaus-user
			     :id 1
			     :username "username"
			     :city "Berlin")))
    (should (equal 1 (soundklaus-user-id user)))
    (should (equal "username" (soundklaus-user-username user)))
    (should (equal "Berlin" (soundklaus-user-city user)))))

(ert-deftest soundklaus-path-segments-test ()
  (should (equal nil (soundklaus-path-segments "/")))
  (should (equal '("users") (soundklaus-path-segments "/users/")))
  (should (equal '("users" ":id") (soundklaus-path-segments "/users/:id"))))

(ert-deftest soundklaus-path-symbols-test ()
  (should (equal nil (soundklaus-path-symbols "/")))
  (should (equal nil (soundklaus-path-symbols "/users")))
  (should (equal '((id)) (soundklaus-path-symbols "/users/:id")))
  (should (equal '((id username) (id title)) (soundklaus-path-symbols "/users/:id-:username/tracks/:id-:title"))))

(ert-deftest soundklaus-replace-slots-test ()
  (should (equal "/users/:id"
		 (soundklaus-replace-slots
		  "/users/:id"
		  (soundklaus-path-symbols "/users/:id")
		  nil)))
  (should (equal "/users/8928131"
		 (soundklaus-replace-slots
		  "/users/:id"
		  (soundklaus-path-symbols "/users/:id")
		  soundklaus-example-user)))
  (should (equal "/users/8928131-KaterMukke"
		 (soundklaus-replace-slots
		  "/users/:id-:username"
		  (soundklaus-path-symbols "/users/:id-:username")
		  soundklaus-example-user))))

(ert-deftest soundklaus-intern-test ()
  (should (equal nil (soundklaus-intern nil)))
  (should (equal nil (soundklaus-intern "")))
  (should (equal 'soundklaus-user (soundklaus-intern "user"))))

(ert-deftest soundklaus-path-test ()
  (should (equal "/users/8928131" (soundklaus-path soundklaus-example-user)))
  (should (equal "/tracks/40258263" (soundklaus-path soundklaus-example-track))))

(ert-deftest soundklaus-url-test ()
  (should (equal "https://api.soundcloud.com/users/8928131" (soundklaus-url soundklaus-example-user)))
  (should (equal "https://api.soundcloud.com/tracks/40258263" (soundklaus-url soundklaus-example-track))))

(ert-deftest soundklaus-parse-callback-test ()
  (let ((params (soundklaus-parse-callback
		 (concat "/home/roman/soundklaus:/oauth/callback?"
			 "code=eabab65ccb58206eaeff6f11e1cb29e4#"
			 "access_token=1-82657-450979-f92c55f37ce760776&"
			 "scope=non-expiring"))))
    (should (equal "eabab65ccb58206eaeff6f11e1cb29e4" (cadr (assoc "code" params))))
    (should (equal "1-82657-450979-f92c55f37ce760776" (cadr (assoc "access_token" params))))
    (should (equal "non-expiring" (cadr (assoc "scope" params)))))
  (let ((params (soundklaus-parse-callback
		 (concat "/home/roman/soundklaus:/oauth/callback?"
			 "error=access_denied&"
			 "error_description=The+end-user+denied+the+request"))))
    (should (equal "access_denied" (cadr (assoc "error" params))))
    (should (equal "The+end-user+denied+the+request" (cadr (assoc "error_description" params))))))

(provide 'soundklaus-test)
