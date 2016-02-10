(require 'ert)
(require 'soundklaus-examples)
(require 'soundklaus-user)

(ert-deftest soundklaus-make-user-test ()
  (let ((user soundklaus-example-user))
    (should (equal 8928131 (soundklaus-user-id user)))
    (should (equal "KaterMukke" (soundklaus-user-username user)))))

(ert-deftest soundklaus-user-make-instance ()
  (let ((user (make-instance 'soundklaus-user
			     :id 1
			     :username "username"
			     :city "Berlin")))
    (should (equal 1 (soundklaus-user-id user)))
    (should (equal "username" (soundklaus-user-username user)))
    (should (equal "Berlin" (soundklaus-user-city user)))))

(provide 'soundklaus-user-test)
