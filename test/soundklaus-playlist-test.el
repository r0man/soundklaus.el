(require 'ert)
(require 'soundklaus-examples)

(ert-deftest soundklaus-playlist-fetch ()
  (let* ((playlist soundklaus-example-playlist)
         (playlist (soundklaus-playlist-fetch playlist)))
    (should (equal (soundklaus-playlist-title playlist) "CS009   ACID PAULI - Mst LP"))))

(ert-deftest soundklaus-playlist-path ()
  (let ((playlist soundklaus-example-playlist))
    (should (string= (soundklaus-playlist-path playlist) "/playlists/1914963"))))

(provide 'soundklaus-playlist-test)
