(provide 'ns-operators-tests)
(require 'ns-operators)
(require 'ns-core)
(require 'cl)
(require 'ert)

(defmacro __defmutable (sym &optional value)
  "Define a namespaced var for these tests."
  (let* ((tpl  (ns/split-sym sym))
         (ns   (or (car tpl) ns/current-ns))
         (sym  (cdr tpl))
         (hash (car (ns/intern ns sym))))
    `(defvar ,hash ,value)))

;;; ^sym

(check "^sym returns the hash of the given qualified name"
  (let ((hash (car (ns/intern 'foo 'bar))))
    (should (equal hash (^sym foo/bar)))))

(check "^sym uses this namespace for unqualified symbols"
  (let ((hash (car (ns/intern 'foo 'bar)))
        (ns/current-ns 'foo))
    (should (equal hash (^sym bar)))))

;;; ^using

(check "^using rebinds the current namespace for body"
  (^using foo (should (equal 'foo ns/current-ns))))

(check "^using reverts the current namespace after evalutating body"
  (let* ((g (gensym))
         (ns/current-ns g))
    (^using foo)
    (should (equal g ns/current-ns))))

;;; ^

(check "^ returns the value of the given qualified sym from this namespace"
  (__defmutable foo/bar 'expected)
  (should (equal 'expected (^ foo/bar))))

(check "^ signals error when given symbol is undefined"
  (should-error (eval '(^ foo))))

(check "^ signals error when given symbol is not accessible"
  (__defmutable foo/bar)
  (let ((ns/current-ns 'baz))
    (should-error (eval '(^ foo/bar)))))


;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
