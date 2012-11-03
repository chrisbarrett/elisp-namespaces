(provide 'ns-core-tests)
(require 'ns-core)
(require 'ert)

;;; Hashing

(check "hash function returns different hashes for different inputs"
  (should-not (equal (ns/hash 'y) (ns/hash 'x))))

(check "hash function same hash for identical inputs"
  (should (equal (ns/hash 'x) (ns/hash 'x))))

;;; Keygen

(check "keys should have structural equality"
  (let ((k1 (ns/make-key 'x 'y))
        (k2 (ns/make-key 'x 'y)))
    (should (equal k1 k2))))

;;; Interning

(check "can intern symbols for a given namespace"
  (ns/intern 'foo 'bar)
  (should (= 1 (hash-table-count ns/symbols-table))))

(check "intern returns the tuple with the hash used"
  (let ((hash (ns/hash 'foo/bar)))
    (should (equal hash (car (ns/intern 'foo 'bar))))))

(check "intern returns the tuple with the name used"
  (should (equal 'foo/bar (cdr (ns/intern 'foo 'bar)))))

(check "interned symbols are not duplicated"
  (ns/intern 'foo 'bar)
  (ns/intern 'foo 'bar)
  (should (= 1 (hash-table-count ns/symbols-table))))

(check "interned symbol data is preserved across duplicated insertions"
  (ns/intern 'foo 'bar)
  (setf (ns-meta-mutable? (ns/get-symbol-meta 'foo 'bar)) t)
  (ns/intern 'foo 'bar)
  (should (ns-meta-mutable? (ns/get-symbol-meta 'foo 'bar))))

;;; Retrieval

(check "can get name by hash for interned symbol"
  (ns/intern 'foo 'bar)
  (let* ((hash (ns/hash 'foo/bar)))
    (should (equal 'foo/bar (ns/get-symbol-name hash)))))

(check "can get hash by name for interned symbol"
  (ns/intern 'foo 'bar)
  (let* ((hash (ns/hash 'foo/bar)))
    (should (equal hash (ns/get-symbol-hash 'foo 'bar)))))

;;; Retrieving Uninterned Syms

(check "get name by hash for uninterned symbol returns nil"
  (should (equal nil (ns/get-symbol-name 'invalid))))

(check "get hash by name for uninterned symbol returns nil"
  (should (equal nil (ns/get-symbol-hash 'ns 'invalid))))

;;; Exporting

(check "can export symbols"
  (ns/export 'foo 'bar)
  (should (ns-meta-public? (gethash (ns/make-key 'foo 'bar)
                                    ns/symbols-table))))

;;; Importing

(check "can import symbols"
  (ns/export 'foo 'x)
  (ns/import 'foo 'bar 'x)
  (let* ((imports (gethash 'bar ns/imports-table))
         (names   (mapcar (lambda (k) (cdr k)) (hash-keys imports))))
    (should (member 'foo/x names))))

(check "cannot import symbols that have not been exported"
  (ns/intern 'foo 'x)
  (should-error (eval '(ns/import 'foo 'bar 'x))))

(check "imported symbols are not duplicated"
  (ns/export 'foo 'x)
  (ns/import 'foo 'bar 'x)
  (ns/import 'foo 'bar 'x)
  (should (= 1 (hash-table-count (gethash 'bar ns/imports-table)))))
