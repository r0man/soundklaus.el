(require 'ert)
(require 'soundklaus-examples)

(ert-deftest soundklaus-playlist-fetch-test ()
  (let* ((playlist soundklaus-example-playlist)
         (playlist (soundklaus-playlist-fetch playlist)))
    (should (string= (soundklaus-playlist-title playlist)
                     "CS009 ∆ ACID PAULI - Mst LP"))))

(ert-deftest soundklaus-playlist-path-test ()
  (let ((playlist soundklaus-example-playlist))
    (should (string= (soundklaus-path playlist) "/playlists/1914963"))))

(ert-deftest soundklaus-playlist-directory-test ()
  (should (string= (soundklaus-playlist-directory soundklaus-example-playlist)
                   "KaterMukke-CS009_ACID_PAULI_Mst_LP")))

(ert-deftest soundklaus-playlist-track-filename-test ()
  (should (string= (soundklaus-playlist-track-filename soundklaus-example-track 1 5)
                   "01-Acid_Pauli_Nancy_iBang_Katermukke.mp3"))
  (should (string= (soundklaus-playlist-track-filename soundklaus-example-track 1 10)
                   "01-Acid_Pauli_Nancy_iBang_Katermukke.mp3"))
  (should (string= (soundklaus-playlist-track-filename soundklaus-example-track 1 100)
                   "001-Acid_Pauli_Nancy_iBang_Katermukke.mp3")))

(provide 'soundklaus-playlist-test)
