(provide 'ns-operators-tests)
(require 'ns-operators)
(require 'ns-core)
(require 'cl)
(require 'ert)
(setq lexical-binding t)

(defmacro with-var (sym value &rest body)
  "Define a namespaced var for these tests."
  (declare (indent defun))
  (let* ((tpl  (ns/split-sym sym))
         (ns   (or (car tpl) ns/current-ns))
         (sym  (cdr tpl))
         (hash (car (ns/intern ns sym))))
    `(let ((,hash ,value))
       ,@body)))

;;; ^sym

(check "^sym returns the hash of the given qualified name"
  (let ((hash (car (ns/intern 'foo 'bar))))
    (should (equal hash (^sym foo/bar)))))

(check "^sym uses this namespace when resolving unqualified symbols"
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
  (^using foo
    (with-var x 'expected
      (should (equal 'expected (^ foo/x))))))

(check "^ returns values when given symbol is public"
  (with-var foo/x 'expected
    (setf (ns-meta-public? (ns/get-symbol-meta 'foo 'x)) t)
    (^using baz
      (should (equal 'expected (^ foo/x))))))

(check "^ signals error when given symbol is undefined"
  (should-error (eval '(^ foo))))

(check "^ signals error when given symbol is not accessible"
  (with-var foo/x nil
    (^using baz
      (eval `(^ foo/x)))))

;;; ^dynamic

(check "^dynamic delegates to ^"
  (with-var x 'expected
    (should (equal 'expected (^dynamic 'x)))))


;; Local Variables:
;; no-byte-compile: t
;; End:
