(require 'soundklaus)
(require 'soundklaus-custom)
(require 'soundklaus-utils)
(require 'ert)

(ert-deftest soundklaus-string-alist-test ()
  (should (equal (soundklaus-string-alist nil) nil))
  (should (equal (soundklaus-string-alist '((a . nil))) nil))
  (should (equal (soundklaus-string-alist '((nil . "v"))) nil))
  (should (equal (soundklaus-string-alist '((k1 . "v2") ("k2" . 1)))
                 '(("k1" . "v2") ("k2" . "1")))))

(ert-deftest soundklaus-bold-test ()
  (should (equal (get-text-property 0 'face (soundklaus-bold nil)) 'bold))
  (should (equal (get-text-property 0 'face (soundklaus-bold "x")) 'bold)))

(ert-deftest soundklaus-format-duration-test ()
  (should (equal "00:00" (soundklaus-format-duration 0)))
  (should (equal "00:00:00" (soundklaus-format-duration 0 t)))
  (should (equal "00:01" (soundklaus-format-duration 1000)))
  (should (equal "00:00:01" (soundklaus-format-duration 1000 t)))
  (should (equal "01:00" (soundklaus-format-duration (* 60 1000))))
  (should (equal "00:01:00" (soundklaus-format-duration (* 60 1000) t)))
  (should (equal "01:00:00" (soundklaus-format-duration (* 60 60 1000)))))

(ert-deftest soundklaus-remove-nil-values-test ()
  (should (equal (soundklaus-remove-nil-values
		  `(("client_id" . "SOUNDCLOUD-CLIENT-ID")
		    ("oauth_token" . nil)))
                 `(("client_id" . "SOUNDCLOUD-CLIENT-ID")))))

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

(ert-deftest soundklaus-parse-duration ()
  (should (equal 0 (soundklaus-parse-duration "0s")))
  (should (equal 1 (soundklaus-parse-duration "1s")))
  (should (equal 60 (soundklaus-parse-duration "1m")))
  (should (equal 3600 (soundklaus-parse-duration "1h")))
  (should (equal 16200 (soundklaus-parse-duration "4h 30m")))
  (should (equal 100800 (soundklaus-parse-duration "1d 4h")))
  (should (equal 60 (soundklaus-parse-duration "1m garbage"))))

(ert-deftest soundklaus-alist-merge-test ()
  (should-not (soundklaus-alist-merge))
  (should-not (soundklaus-alist-merge nil))
  (should-not (soundklaus-alist-merge nil nil))
  (should (equal (soundklaus-alist-merge '((a . 1))) '((a . 1))))
  (should (equal (soundklaus-alist-merge '((a . 1)) '((a . 2))) '((a . 2)))))

(ert-deftest soundklaus-auth-params-test ()
  (let ((soundklaus-client-id "CLIENT-ID")
        (soundklaus-access-token "ACCESS-TOKEN"))
    (should (equal (soundklaus-auth-params)
                   '(("client_id" . "CLIENT-ID")
                     ("oauth_token" . "ACCESS-TOKEN"))))))

(provide 'soundklaus-utils-test)
