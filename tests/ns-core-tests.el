;; NS-CORE-TESTS

(require 'ert)
(require 'cl)
(add-to-list 'load-path (concat user-emacs-directory "elisp-namespaces/" "src/"))
(require 'ns-core)


(defmacro check (desc &rest body)
  "Wrap ert-deftest with a simpler interface for testing namespaces."
  (declare (indent 1))
  `(ert-deftest
       ,(intern (replace-regexp-in-string "[ .]" "-" desc)) ()
     ;; Rebind tables for tests.
     (let ((ns/symbols-table (copy-hash-table ns/symbols-table)))
       ,@body)))


;;; Hashing

(check "hash function returns different hashes for different inputs."
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

(check "after several interns the same symbol is not present multiple times"
  (ns/intern 'foo 'bar)
  (ns/intern 'foo 'bar)
  (should (= 1 (hash-table-count ns/symbols-table))))

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




;; Local Variables:
;; no-byte-compile: t
;; End:
