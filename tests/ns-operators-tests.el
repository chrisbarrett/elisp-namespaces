(provide 'ns-operators-tests)
(require 'ns-operators)
(require 'ns-core)
(require 'ert)

;;; @sym

(check "@sym returns the hash of the given qualified name"
  (let ((hash (car (ns/intern 'foo 'bar))))
    (should (equal hash (@sym foo/bar)))))

(check "@sym returns the hash of the given unqualified name in this namespace"
  (let ((hash (car (ns/intern 'foo 'bar))))
    (should (equal hash (@sym bar)))))
