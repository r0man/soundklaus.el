(require 'ert)
(require 'soundklaus-resource)

(ert-deftest soundklaus-define-slot-test ()
  (should (equal (soundklaus-define-slot 'user '(permalink "The permalink of the user"))
		 '(permalink :initarg :permalink
			     :accessor soundklaus-user-permalink
			     :documentation "The permalink of the user"))))

(ert-deftest soundklaus-intern-test ()
  (should (equal nil (soundklaus-intern nil)))
  (should (equal nil (soundklaus-intern "")))
  (should (equal 'soundklaus-user (soundklaus-intern "user"))))

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

(ert-deftest soundklaus-path-test ()
  (should (equal "/users/8928131" (soundklaus-path soundklaus-example-user)))
  (should (equal "/tracks/40258263" (soundklaus-path soundklaus-example-track))))

(provide 'soundklaus-resource-test)
