(provide 'ns-operators-tests)
(require 'ns-operators)
(require 'ns-core)
(require 'cl)
(require 'ert)
(setq lexical-binding t)

(defmacro with-var (sym value &rest body)
  "Define a namespaced var within BODY forms."
  (declare (indent defun))
  (let* ((tpl  (ns/split-sym sym))
         (ns   (or (car tpl) ns/current-ns))
         (sym  (cdr tpl))
         (hash (car (ns/intern ns sym))))
    `(let ((,hash ,value))
       ,@body)))

(defmacro with-fn (sym arglist body-form &rest body)
  "Declare a namespaced function within BODY forms."
  (declare (indent defun))
  (let* ((tpl  (ns/split-sym sym))
         (ns   (or (car tpl) ns/current-ns))
         (sym  (cdr tpl))
         (hash (car (ns/intern ns sym))))
    `(flet ((,hash ,arglist ,body-form))
       ,@body)))

;;; ^sym

(check "^sym returns the hash of the given qualified name"
  (let ((hash (car (ns/intern 'foo 'bar))))
    (^using foo
      (should (equal hash (^sym foo/bar))))))

(check "^sym uses this namespace when resolving unqualified symbols"
  (let ((hash (car (ns/intern 'foo 'bar)))
        (ns/current-ns 'foo))
    (should (equal hash (^sym bar)))))

(check "^sym signals an error when the given symbol is not public"
  (let ((hash (car (ns/intern 'foo 'bar))))
    (should-error (eval '(^sym foo/bar)))))

(check "^sym signals an error when the given symbol is undefined"
  (should-error (eval '(^sym foo/bar))))


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
      (should-error (eval `(^ foo/x))))))

;;; ^dynamic

(check "^dynamic delegates to ^"
  (with-var x 'expected
    (should (equal 'expected (^dynamic 'x)))))

;;; ^set

(check "^set modifies mutable vars using unqualified symbol"
  (^using foo
    (with-var x ()
      (setf (ns-meta-mutable? (ns/get-symbol-meta 'foo 'x)) t)
      (^set x 'expected)
      (should (equal 'expected (eval (ns/get-symbol-hash 'foo 'x)))))))

(check "^set modifies mutable vars using qualified symbol"
  (^using foo
    (with-var x ()
      (setf (ns-meta-mutable? (ns/get-symbol-meta 'foo 'x)) t)
      (^set foo/x 'expected)
      (should (equal 'expected (eval (ns/get-symbol-hash 'foo 'x)))))))

;;; ^call

(check "^call applies qualified function"
  (^using foo
    (with-fn foo/x () 'expected
      (should (equal 'expected (^call foo/x))))))

(check "^call applies arguments"
  (^using foo
    (with-fn foo/x (i) i
      (should (equal 'expected (^call foo/x 'expected))))))

(check "^call signals error when applying an inaccessbile fn"
  (with-fn foo/x () 'fail
    (^using bar
      (should-error (eval '(^call foo/x))))))

(check "^call signals error when applying an undefined fn"
  (should-error (eval `(^call fail))))

;;; ^lambda

(check "^lambda captures namespace environment"
  (let ((x))
    (^using foo (setq x (^lambda () ns/current-ns)))
    (^using bar (should (equal 'foo (funcall x))))))




;; Local Variables:
;; no-byte-compile: t
;; End:
