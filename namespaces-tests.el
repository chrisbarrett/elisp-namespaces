(provide 'ns-tests)
(require 'ert)
(require 'cl-lib)
(require 'cl)
(require 'package)
(package-initialize)
(require 'namespaces)

(defun ns/clear-hash (table)
  "Delete all entries in the given hashtable."
  (cl-loop for k being the hash-keys of table
           do (remhash k table))
  table)

(defmacro check (desc &rest body)
  "Wrap ert-deftest with a simpler interface for testing namespaces."
  (declare (indent 1))
  `(ert-deftest
       ,(intern (replace-regexp-in-string "[ .]" "-" desc)) ()
     ;; Rebind tables for tests.
     ;; Tables are copied to ensure custom equality comparers
     ;; are preserved.
     (let ((ns/symbols-table (ns/clear-hash (copy-hash-table ns/symbols-table)))
           (ns/imports-table (ns/clear-hash (copy-hash-table ns/imports-table)))
           (ns/exports-table (ns/clear-hash (copy-hash-table ns/exports-table)))
           (ns/current-ns (cl-gensym)))
       (eval '(progn ,@body)))))

;;; =============================== Core =======================================

;;; Hashing

(check "hash function returns different hashes for different inputs"
  (should-not (equal (ns/hash 'x 'y) (ns/hash 'x 'x))))

(check "hash function returns same hash for identical inputs"
  (should (equal (ns/hash 'x 'y) (ns/hash 'x 'y))))

;;; Keygen

(check "keys should have structural equality"
  (let ((k1 (ns/make-key 'x 'y))
        (k2 (ns/make-key 'x 'y)))
    (should (equal k1 k2))))

;;; Interning

(check "can intern symbols for a given namespace"
  (ns/intern 'foo 'bar)
  (should (= 1 (hash-table-count ns/symbols-table))))

(check "intern returns a struct containing the symbol hash"
  (let ((hash (ns/hash 'foo 'bar)))
    (should (equal hash (ns-symbol-hash (ns/intern 'foo 'bar))))))

(check "intern returns a struct containing the qualified name"
  (should (equal 'foo/bar (ns-symbol-name (ns/intern 'foo 'bar)))))

(check "interned symbols are not duplicated"
  (ns/intern 'foo 'bar)
  (ns/intern 'foo 'bar)
  (should (= 1 (hash-table-count ns/symbols-table))))

(check "metadata is preserved across duplicated insertions"
  (ns/intern 'foo 'bar)
  (setf (ns-meta-mutable? (ns/get-symbol-meta 'foo 'bar)) t)
  (ns/intern 'foo 'bar)
  (should (ns-meta-mutable? (ns/get-symbol-meta 'foo 'bar))))

;;; Retrieval

(check "can get name by hash for interned symbol"
  (ns/intern 'foo 'bar)
  (let ((hash (ns/hash 'foo 'bar)))
    (should (equal 'foo/bar (ns/get-symbol-name hash)))))

(check "can get hash by name for interned symbol"
  (ns/intern 'foo 'bar)
  (let ((hash (ns/hash 'foo 'bar)))
    (should (equal hash (ns/get-symbol-hash 'foo 'bar)))))

;;; Retrieving Uninterned Syms

(check "get name by hash for uninterned symbol returns nil"
  (should (equal nil (ns/get-symbol-name 'invalid))))

(check "get hash by name for uninterned symbol returns nil"
  (should (equal nil (ns/get-symbol-hash 'ns 'invalid))))

;;; Exporting

(check "exported symbols are public"
  (ns/export 'foo 'bar)
  (should (ns-meta-public? (gethash (ns/make-key 'foo 'bar)
                                    ns/symbols-table))))

;;; Importing

(check "can import symbols"
  (ns/export 'foo 'x)
  (ns/import 'foo 'bar 'x)
  (let* ((imports (gethash 'bar ns/imports-table))
         (names   (mapcar 'ns-symbol-name (ns/hash-keys imports))))
    (should (member 'foo/x names))))

(check "cannot import symbols that have not been exported"
  (ns/intern 'foo 'x)
  (should-error (ns/import 'foo 'bar 'x)))

(check "imported symbols are not duplicated"
  (ns/export 'foo 'x)
  (ns/import 'foo 'bar 'x)
  (ns/import 'foo 'bar 'x)
  (should (= 1 (hash-table-count (gethash 'bar ns/imports-table)))))

(check "can import all public symbols from one namespace into another"
  (ns/export 'foo 'x)
  (ns/export 'foo 'y)
  (ns/import-all 'foo 'bar)
  (should (equal 2 (hash-table-count (gethash 'bar ns/imports-table)))))

;;; ============================== Operators ===================================

;;; Simple substitutes for `def` and `defn` to avoid taking a dependency on those
;;; macros at this stage.

(defmacro* with-var ((sym &optional value) &rest body)
  "Define a namespaced var within BODY forms."
  (declare (indent defun))
  (let* ((tpl  (ns/split-sym sym))
         (ns   (or (car tpl) ns/current-ns))
         (sym  (cdr tpl))
         (hash (ns-symbol-hash (ns/intern ns sym))))
    `(let ((,hash ,value))
       ,@body)))

(defmacro* with-fn ((sym arglist &rest body-form) &rest body)
  "Declare a namespaced function within BODY forms."
  (declare (indent defun))
  (cl-assert (symbolp sym))
  (cl-assert (listp arglist))
  (let* ((tpl  (ns/split-sym sym))
         (ns   (or (car tpl) ns/current-ns))
         (sym  (cdr tpl))
         (hash (ns-symbol-hash (ns/intern ns sym))))
    `(flet ((,hash ,arglist ,@body-form))
       ,@body)))

;;; ----------------------------------------------------------------------------

;;; ~

(check "~ returns the hash of the given qualified name"
  (let ((hash (ns-symbol-hash (ns/intern 'foo 'bar))))
    (in-ns foo
      (should (equal hash (~ foo/bar))))))

(check "~ uses this namespace when resolving unqualified functions"
  (setq ns/current-ns 'foo)

  (with-fn (foo/x ())
    (let ((hash (ns/hash 'foo 'x)))
      (should (equal hash (~ x))))))

(check "~ uses imports when resolving unqualified functions"
  (setq ns/current-ns 'bar)

  (with-fn (foo/x ())
    (let ((hash (ns/hash 'foo 'x)))
      (ns/export 'foo 'x)
      (ns/import 'foo 'bar 'x)
      ;; Could legitimately return the sym or the alias.
      (should (or (equal hash (~ x))
                  (equal 'foo/x (~ x)))))))

(check "~ resolves qualified public functions"
  (setq ns/current-ns 'bar)

  (with-fn (foo/x ())
    (let ((hash (ns-symbol-hash (ns/intern 'foo 'x))))
      (ns/export 'foo 'x)
      ;; Could legitimately return the sym or the alias.
      (should (or (equal hash (~ foo/x))
                  (equal 'foo/x (~ foo/x)))))))

(check "~ should not signal error when quoting undefined namespace member"
  (should (~ foo/bar)))

;;; in-ns

(check "in-ns rebinds the current namespace for body"
  (in-ns foo (should (equal 'foo ns/current-ns))))

(check "in-ns reverts the current namespace after evalutating body"
  (let ((prev ns/current-ns))
    (in-ns foo)
    (should (equal prev ns/current-ns))))

;;; @

(check "@ returns the value of the given qualified sym from this namespace"
  (in-ns foo
    (with-var (x 'expected)
      (should (equal 'expected (@ foo/x))))))

(check "@ returns the value of the given unqualified sym from this namespace"
  (in-ns foo
    (with-var (x 'expected)
      (should (equal 'expected (@ x))))))

(check "@ signals an error when the given sym is not in this namespace (even when public)"
  (with-var (foo/x)
    (setf (ns-meta-public? (ns/get-symbol-meta 'foo 'x)) t)
    (in-ns bar
      (should-error (@ foo/x)))))

(check "@ signals error when given symbol is undefined"
  (should-error (@ foo)))

(check "@ signals error when given symbol is not accessible"
  (with-var (foo/x)
    (in-ns baz
      (should-error (@ foo/x)))))

;;; @set

(check "@set modifies mutable vars using unqualified symbol"
  (in-ns foo
    (with-var (x)
      (setf (ns-meta-mutable? (ns/get-symbol-meta 'foo 'x)) t)
      (@set x 'expected)
      (should (equal 'expected (eval (ns/get-symbol-hash 'foo 'x)))))))

(check "@set modifies mutable vars using qualified symbol"
  (in-ns foo
    (with-var (x)
      (setf (ns-meta-mutable? (ns/get-symbol-meta 'foo 'x)) t)
      (@set foo/x 'expected)
      (should (equal 'expected (eval (ns/get-symbol-hash 'foo 'x)))))))

(check "@set signals error when target is undefined"
  (should-error (@set foo/x 'error)))

(check "@set signals error when target is inaccessible"
  (in-ns foo
    (with-var (x)
      (setf (ns-meta-mutable? (ns/get-symbol-meta 'foo 'x)) t)
      (in-ns bar (should-error (@set foo/x 'error))))))

(check "@set signals error when target is immutable"
  (in-ns foo
    (with-var (x)
      (setf (ns-meta-mutable? (ns/get-symbol-meta 'foo 'x)) nil)
      (should-error (@set foo/x 'error)))))

;;; _

(check "_ applies qualified function"
  (in-ns foo
    (with-fn (foo/x () 'expected)
      (should (equal 'expected (_ foo/x))))))

(check "_ applies arguments"
  (in-ns foo
    (with-fn (foo/x (i) i)
      (should (equal 'expected (_ foo/x 'expected))))))

(check "_ signals error when applying an inaccessbile fn"
  (let ((fname (intern (format "foo/%s" (cl-gensym)))))
    (eval
     `(with-fn (,fname () 'fail)
        (in-ns bar
          (should-error (_ ,fname)))))))

(check "_ signals error when applying an undefined fn"
  (should-error (_ fail)))

;;; lambda-

(check "lambda- captures namespace environment"
  (let ((x))
    (in-ns foo (setq x (lambda- () ns/current-ns)))
    (in-ns bar (should (equal 'foo (funcall x))))))

;;; ============================== Definitions =================================

;;; def

(check "can define and read var created with def"
  (in-ns foo
   (def var 'expected)
   (should (equal 'expected (@ var)))))

(check "should get error when setting a var created with def"
  (def var nil)
  (should-error (@set var 'fail)))

(check "def creates accessor function for public vars"
  (in-ns foo
    (ns/export 'foo 'x)
    (def x 'expected)
    (should (equal 'expected (foo/x)))))

(check "def does not overwrite existing accessors"
  (let* ((ns   (cl-gensym))
         (ns/x (intern (concat (symbol-name ns) "/x"))))
    (eval
     `(in-ns ,ns
        (ns/export ',ns 'x)
        (defn x () 'expected)
        (def x 'fail)
        (should (equal 'expected (,ns/x)))))))

;;; defmutable

(check "can define and read var created with defmutable"
  (in-ns foo
    (defmutable var 'expected)
    (should (equal 'expected (@ var)))))

(check "can set namespaced var created with defmutable"
  (in-ns foo
     (defmutable var)
     (@set var 'expected)
     (should (equal 'expected (@ var)))))

(check "defmutable creates accessor function for public vars"
  (in-ns foo
    (ns/export 'foo 'x)
    (defmutable x 'expected)
    (should (equal 'expected (foo/x)))))

(check "defmutable does not overwrite existing accessors"
  (let* ((ns   (cl-gensym))
         (ns/x (intern (concat (symbol-name ns) "/x"))))
    (eval
     `(in-ns ,ns
        (ns/export ',ns 'x)
        (defn x () 'expected)
        (defmutable x 'fail)
        (should (equal 'expected (,ns/x)))))))

;;; Redefinitions

(check "can redefine def vars as defmutable vars and set"
  (in-ns foo
    (def var nil)
    (defmutable var)
    (@set var 'expected)
    (should (equal 'expected (@ var)))))

(check "should get error when redefining a defmutable as a def and using @set"
  (defmutable var)
  (def var nil)
  (should-error (@set var 'x)))

;;; defn

(check "can call function defined with defn"
  (defn x () 'expected)
  (should (equal 'expected (_ x))))

(check "defn should tolerate body form in docstring position"
  (defn x () "expected")
  (should (equal "expected" (_ x))))

(check "defn should be callable with arguments"
  (defn sqr (i) (* i i))
  (should (equal 9 (_ sqr 3))))

(check "defn falls back to defun for command declarations"
  (in-ns foo
    (let* ((name (cl-gensym))
           (hash (ns-symbol-hash (ns/make-key 'foo name))))
      (eval `(defn ,name () (interactive) 'expected))
      (should (commandp hash)))))

;;; ============================== Namespace Macro =============================

;;; namespace

(defun gen-namespace (&rest args)
  "Generate a unique namespace."
  (eval `(namespace ,(cl-gensym) ,@args)))

(check "namespace declaration should update current namespace"
  (namespace foo)
  (should (equal 'foo ns/current-ns)))

(check "namespace declaration should provide an emacs feature"
  (let ((ns (cl-gensym)))
    (eval
     `(progn
        (namespace ,ns)
        (should (require ',ns))))))

;;; Exported Functions

(check "can call imported public fn using unqualified symbol"
  (namespace foo :export [ public ])
  (defn public () 'expected)
  (gen-namespace :import [ foo ])
  (should (equal 'expected (_ public))))

(check "can call exported fn using qualified symbol"
  (namespace foo :export [ public ])
  (defn public () 'expected)
  (_ foo/public )
  (gen-namespace)
  (should (equal 'expected (_ foo/public))))

(check "can call exported fn without _ using qualified symbol"
  (namespace foo :export [ public ])
  (defn public () 'expected)
  (gen-namespace)
  (should (equal 'expected (foo/public))))

;;; Dependency Loading

(check "can require elisp features"
  (let ((feature (cl-gensym)))
    (provide feature)
    (eval `(gen-namespace :use [ ,feature ]))
    (should (member feature features))))

(check "can download packages"
  (let ((pkg (cl-gensym)) (loaded))

    (flet ((package-install (x) (setq loaded x)))

      (provide pkg)
      (eval `(gen-namespace :packages [ ,pkg ]))
      (should (eq loaded pkg)))))

(check "requires package after download"
  (let ((pkg (cl-gensym)))

    (flet ((package-install (x)))

      (provide pkg)
      (eval `(gen-namespace :packages [ ,pkg ]))
      (should (member pkg features)))))

;;; Autoloading

(check "can autoload elisp features"
  (let ((feature (cl-gensym))
        (result))

    (flet ((autoload (fn file &optional doc interactive type)
                (setq result `[,fn ,file ,interactive ,type])))

      (provide feature)
      (eval `(gen-namespace :use [ (,feature (x :interactive t :type boolean)) ]))
      (should (equal result `[x ,(symbol-name feature) t boolean])))))

(check "can autoload packages"
  (let ((pkg (cl-gensym))
        (result))
    (require 'package)

    (flet ((package-install (x))
              (autoload (fn file &optional doc interactive type)
                (setq result `[,fn ,file ,interactive ,type])))

      (provide pkg)
      (eval `(gen-namespace :packages [ (,pkg (x :interactive t :type boolean))]))
      (should (equal result `[x ,(symbol-name pkg) t boolean])))))

;;; Conditional Loading

(check "loads a dependency when `when` evaluates to true"
  (let ((feature (cl-gensym)))
    (provide feature)
    (eval `(gen-namespace :use [ (,feature :when (eq t t)) ]))
    (should (member feature features))))

(check "loads a dependency when `unless` evaluates to nil"
  (let ((feature (cl-gensym)))
    (provide feature)
    (eval `(gen-namespace :use [ (,feature :unless (eq t nil)) ]))
    (should (member feature features))))

(check "loads a dependency when `when` is t and `unless` is nil"
  (let ((feature (cl-gensym)))
    (provide feature)
    (eval `(gen-namespace :use [ (,feature :when t :unless nil) ]))
    (should (member feature features))))

(check "does not load a dependency when `when` evaluates to nil"
  (gen-namespace :use [ (undefined :when (eq t nil)) ]))

(check "does not load a dependency when `unless` evaluates to true"
  (gen-namespace :use [ (undefined :unless (eq t t)) ]))

(check "does not load a dependency when `when` is nil and `unless` is nil"
  (gen-namespace :use [ (undefined :when nil :unless nil) ]))

(check "does not load a dependency when `when` is t and `unless` is t"
  (gen-namespace :use [ (undefined :when t :unless t) ]))

;;; ============================== Integration =================================

;;; Encapsulation

(check "should get error when accessing another namespace's private var using unqualified symbol"
  (def private nil)
  (gen-namespace)
  (should-error (@ private)))

(check "should get error when accessing another namespace's private var using qualified symbol"
  (namespace foo)
  (def private nil)
  (gen-namespace)
  (should-error (@ foo/private)))

(check "should get error when accessing another namespace's private fn using unqualified symbol"
  (defn private ())
  (gen-namespace)
  (should-error (_ private)))

(check "should get error when accessing another namespace's private fn using qualified symbol"
  (namespace foo)
  (defn private ())
  (gen-namespace)
  (should-error (_ foo/private)))

(check "should get error when accessing unimported public var using unqualified symbol"
  (namespace foo :export [ public ])
  (def public nil)
  (gen-namespace)
  (should-error (@ public)))

(check "should get error when setting unimported public var using unqualified symbol"
  (namespace foo :export [ public ])
  (def public nil)
  (gen-namespace)
  (should-error (@set public nil)))

(check "should get error when setting undefined var using unqualified symbol"
  (should-error (@set x nil)))

(check "should get error when setting undefined member var using qualified symbol"
  (namespace foo)
  (should-error (@set foo/x nil)))

(check "should get error when calling unimported public fn using unqualified symbol"
  (namespace foo :export [ public ])
  (defn public ())
  (gen-namespace)
  (should-error (_ public)))

;;; Exported Vars

(check "cannot resolve underlying var from another namespace"
  (namespace foo :export [ public ])
  (def public 'fail)
  (gen-namespace :import [ foo ] )
  (should-error (@ foo/public)))

(check "can resolve imported accessor fn with qualified symbol"
  (namespace foo :export [ public ])
  (def public 'expected)
  (gen-namespace :import [ foo ] )
  (should (equal 'expected (_ foo/public))))

(check "can resolve imported accessor fn with unqualified symbol"
  (namespace foo :export [ public ])
  (def public 'expected)
  (gen-namespace :import [ foo ] )
  (should (equal 'expected (_ public))))

(check "cannot set public var from another namespace"
  (namespace foo :export [ public ])
  (defmutable public)
  (gen-namespace)
  (should-error (@set foo/public 'fail)))

(check "~ returns hash of accessor function for qualified symbol"
  (namespace foo :export [ x ])
  (def x nil)
  (gen-namespace)
  (should (functionp (~ foo/x))))

(check "~ returns hash of accessor function for unqualified symbol"
  (namespace foo :export [ x ])
  (def x nil)
  (gen-namespace :import [ foo ])
  (should (functionp (~ x))))

;;; External state

(ert-deftest the-default-namespace-is-USER ()
  (with-temp-buffer (gen-namespace))
  (with-temp-buffer
    (should (equal 'user ns/current-ns))))

;;; ============================================================================

;; Local Variables:
;; lexical-binding: t
;; no-byte-compile: t
;; End:
