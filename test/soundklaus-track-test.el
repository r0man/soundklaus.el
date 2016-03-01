(require 'ert)
(require 'soundklaus-custom)
(require 'soundklaus-examples)
(require 'soundklaus-track)

(ert-deftest soundklaus-make-track-test ()
  (let ((track soundklaus-example-track))
    (should (equal 40258263 (soundklaus-track-id track)))
    (should (equal "Acid Pauli & Nancy - iBang - Katermukke" (soundklaus-track-title track)))))

(ert-deftest soundklaus-track-duration-secs-test ()
  (let ((track soundklaus-example-track))
    (should (equal 502 (soundklaus-track-duration-secs track)))))

(ert-deftest soundklaus-track-header-test ()
  (should (string= (soundklaus-track-header soundklaus-example-track)
                   "♡ Acid Pauli & Nancy - iBang - Katermukke - KaterMukke")))

(ert-deftest soundklaus-track-time-test ()
  (should (equal "08:22" (soundklaus-track-time soundklaus-example-track))))

(ert-deftest soundklaus-track-stream-url-test ()
  (should (equal (format "https://api.soundcloud.com/tracks/40258263/stream?client_id=%s" soundklaus-client-id)
		 (soundklaus-track-stream-url soundklaus-example-track)))
  (let ((soundklaus-access-token "1-82657-450979-f92c55f37ce760776"))
    (should (equal (format "https://api.soundcloud.com/tracks/40258263/stream?client_id=%s&oauth_token=%s"
			   soundklaus-client-id
			   soundklaus-access-token)
		   (soundklaus-track-stream-url soundklaus-example-track)))))

(ert-deftest soundklaus-track-filename-test ()
  (should (string= (soundklaus-track-filename soundklaus-example-track)
                   "Acid_Pauli_Nancy_iBang_Katermukke.mp3"))
  (should (string= (soundklaus-track-filename soundklaus-example-track t)
                   "KaterMukke-Acid_Pauli_Nancy_iBang_Katermukke.mp3")))

(provide 'soundklaus-track-test)
