(require 'soundklaus)
(require 'soundklaus-examples)
(require 'soundklaus-helm)
(require 'ert)

(ert-deftest soundklaus-helm-track-search-test ()
  (let* ((helm-pattern "acid pauli")
         (result (soundklaus-helm-track-search)))
    (should (cl-plusp (length result)))))

(ert-deftest soundklaus-helm-track-action-test ()
  (should (equal (soundklaus-helm-track-action nil soundklaus-example-track)
                 '(("Play Track - Acid Pauli & Nancy - iBang - Katermukke - KaterMukke" . soundklaus-play)
                   ("Queue Track - Acid Pauli & Nancy - iBang - Katermukke - KaterMukke" . soundklaus-playlist-add)
                   ("Show Track Metadata" . pp)))))

(ert-deftest soundklaus-helm-playlist-search-test ()
  (let* ((helm-pattern "acid pauli")
         (result (soundklaus-helm-playlist-search)))
    (should (cl-plusp (length result)))))

(ert-deftest soundklaus-helm-playlist-action-test ()
  (should (equal (soundklaus-helm-playlist-action nil soundklaus-example-playlist)
                 '(("Play Playlist - CS009   ACID PAULI - Mst LP - KaterMukke" . soundklaus-play)
                   ("Queue Playlist - CS009   ACID PAULI - Mst LP - KaterMukke" . soundklaus-playlist-add)
                   ("Show Playlist Metadata" . pp)))))

(provide 'soundklaus-helm-test)
