(require 'ert)
(require 'soundklaus)

(ert-deftest soundklaus-width-test ()
  (should (cl-plusp (soundklaus-width))))

(ert-deftest soundklaus-horizontal-rule-test ()
  (with-output-to-temp-buffer "soundklaus"
    (soundklaus-horizontal-rule)))

(ert-deftest soundklaus-tracks-test ()
  (soundklaus-tracks "Acid Pauli"))

(ert-deftest soundklaus-playlists-test ()
  (soundklaus-playlists "Acid Pauli"))

(provide 'soundklaus-test)
