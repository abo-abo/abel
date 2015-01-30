(require 'ert)

(ert-deftest abel-no-duplicates ()
  (should (= (length abel-abbrevs)
             (length (cl-delete-duplicates
                      (mapcar #'car abel-abbrevs)
                      :test #'equal)))))

(provide 'abel-test)
