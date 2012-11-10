(provide 'ns-tests)
(require 'ert)
(require 'namespaces)
(setq lexical-binding t)

;; Add `tests` dir to load path.
(let* ((file (or load-file-name (buffer-file-name)))
       (path (file-name-directory file))
       )
  (add-to-list 'load-path path))

;;; ----------------------------------------------------------------------------

(defun ns/clear-hash (table)
  "Delete all entries in the given hashtable."
  (assert (hash-table-p table))
  (loop for k being the hash-keys of table
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
           (ns/current-ns (gensym)))
       ,@body)))

;;; =============================== Core =======================================

;;; Hashing

(check "hash function returns different hashes for different inputs"
  (should-not (equal (ns/hash 'y) (ns/hash 'x))))

(check "hash function returns same hash for identical inputs"
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

(check "intern returns a tuple where the car is the symbol hash"
  (let ((hash (ns/hash 'foo/bar)))
    (should (equal hash (car (ns/intern 'foo 'bar))))))

(check "intern returns a tuple where the cdr is the qualified name"
  (should (equal 'foo/bar (cdr (ns/intern 'foo 'bar)))))

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

(check "exported symbols are public"
  (ns/export 'foo 'bar)
  (should (ns-meta-public? (gethash (ns/make-key 'foo 'bar)
                                    ns/symbols-table))))

;;; Importing

(check "can import symbols"
  (ns/export 'foo 'x)
  (ns/import 'foo 'bar 'x)
  (let* ((imports (gethash 'bar ns/imports-table))
         (names   (mapcar (lambda (k) (cdr k)) (ns/hash-keys imports))))
    (should (member 'foo/x names))))

(check "cannot import symbols that have not been exported"
  (ns/intern 'foo 'x)
  (should-error (eval '(ns/import 'foo 'bar 'x))))

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

;;; ----------------------------------------------------------------------------

;;; @sym

(check "@sym returns the hash of the given qualified name"
  (let ((hash (car (ns/intern 'foo 'bar))))
    (@using foo
      (should (equal hash (@sym foo/bar))))))

(check "@sym uses this namespace when resolving unqualified functions"
  (with-fn foo/x () nil
    (let ((hash (ns/hash 'foo/x))
          (ns/current-ns 'foo))
      (should (equal hash (@sym x))))))

(check "@sym uses imports when resolving unqualified functions"
  (with-fn foo/x () nil
    (let ((hash (ns/hash 'foo/x))
          (ns/current-ns 'bar))
      (ns/export 'foo 'x)
      (ns/import 'foo 'bar 'x)
      ;; Could legitimately return the sym or the alias.
      (should (or (equal hash (@sym x))
                  (equal 'foo/x (@sym x)))))))

(check "@sym resolves qualified public functions"
  (with-fn foo/x () nil
    (let ((hash (car (ns/intern 'foo 'x)))
          (ns/current-ns 'bar))
      (ns/export 'foo 'x)
      ;; Could legitimately return the sym or the alias.
      (should (or (equal hash (@sym foo/x))
                  (equal 'foo/x (@sym foo/x)))))))

(check "@sym signals an error when the given symbol is not publicly accessible"
  (let ((hash (car (ns/intern 'foo 'bar))))
    (should-error (eval '(@sym foo/bar)))))

(check "@sym signals an error when the given symbol is undefined"
  (should-error (eval '(@sym foo/bar))))

;;; @using

(check "@using rebinds the current namespace for body"
  (@using foo (should (equal 'foo ns/current-ns))))

(check "@using reverts the current namespace after evalutating body"
  (let* ((g (gensym))
         (ns/current-ns g))
    (@using foo)
    (should (equal g ns/current-ns))))

;;; @

(check "@ returns the value of the given qualified sym from this namespace"
  (@using foo
    (with-var x 'expected
      (should (equal 'expected (@ foo/x))))))

(check "@ returns the value of the given unqualified sym from this namespace"
  (@using foo
    (with-var x 'expected
      (should (equal 'expected (@ x))))))

(check "@ signals an error when the given sym is not in this namespace (even when public)"
  (with-var foo/x 'fail
    (setf (ns-meta-public? (ns/get-symbol-meta 'foo 'x)) t)
    (@using bar (should-error (eval '(@ foo/x))))))

(check "@ signals error when given symbol is undefined"
  (should-error (eval '(@ foo))))

(check "@ signals error when given symbol is not accessible"
  (with-var foo/x nil
    (@using baz
      (should-error (eval `(@ foo/x))))))

;;; @dynamic

(check "@dynamic delegates to @"
  (@using foo
     (with-var foo/x 'expected
       (should (equal 'expected (@dynamic 'x))))))

;;; @set

(check "@set modifies mutable vars using unqualified symbol"
  (@using foo
    (with-var x ()
      (setf (ns-meta-mutable? (ns/get-symbol-meta 'foo 'x)) t)
      (@set x 'expected)
      (should (equal 'expected (eval (ns/get-symbol-hash 'foo 'x)))))))

(check "@set modifies mutable vars using qualified symbol"
  (@using foo
    (with-var x ()
      (setf (ns-meta-mutable? (ns/get-symbol-meta 'foo 'x)) t)
      (@set foo/x 'expected)
      (should (equal 'expected (eval (ns/get-symbol-hash 'foo 'x)))))))

(check "@set signals error when target is undefined"
  (should-error (eval `(@set foo/x 'error))))

(check "@set signals error when target is inaccessible"
  (@using foo
    (with-var x ()
      (setf (ns-meta-mutable? (ns/get-symbol-meta 'foo 'x)) t)
      (@using bar (should-error (eval `(@set foo/x 'error)))))))

;;; @call

(check "@call applies qualified function"
  (@using foo
    (with-fn foo/x () 'expected
      (should (equal 'expected (@call foo/x))))))

(check "@call applies arguments"
  (@using foo
    (with-fn foo/x (i) i
      (should (equal 'expected (@call foo/x 'expected))))))

(check "@call signals error when applying an inaccessbile fn"
  (with-fn foo/x () 'fail
    (@using bar
      (should-error (eval '(@call foo/x))))))

(check "@call signals error when applying an undefined fn"
  (should-error (eval `(@call fail))))

;;; @lambda

(check "@lambda captures namespace environment"
  (let ((x))
    (@using foo (setq x (@lambda () ns/current-ns)))
    (@using bar (should (equal 'foo (funcall x))))))


;;; ============================== Definitions =================================

;;; def

(check "can define and read var created with def"
  (@using foo
   (def var 'expected)
   (should (equal 'expected (@ var)))))

(check "should get error when setting a var created with def"
  (def var nil)
  (should-error (eval `(@set var 'fail))))

(check "def creates accessor function for public vars"
  (@using foo
    (ns/export 'foo 'x)
    (def x 'expected)
    (should (equal 'expected (foo/x)))))

(check "def does not overwrite existing accessors"
  (let* ((ns   (gensym))
         (ns/x (intern (concat (symbol-name ns) "/x"))))
    (eval
     `(@using ,ns
        (ns/export ',ns 'x)
        (defn x () 'expected)
        (def x 'fail)
        (should (equal 'expected (,ns/x)))))))

;;; defmutable

(check "can define and read var created with defmutable"
  (@using foo
    (defmutable var 'expected)
    (should (equal 'expected (@ var)))))

(check "can set namespaced var created with defmutable"
  (@using foo
     (defmutable var)
     (@set var 'expected)
     (should (equal 'expected (@ var)))))

(check "defmutable creates accessor function for public vars"
  (@using foo
    (ns/export 'foo 'x)
    (defmutable x 'expected)
    (should (equal 'expected (foo/x)))))

(check "defmutable does not overwrite existing accessors"
  (let* ((ns   (gensym))
         (ns/x (intern (concat (symbol-name ns) "/x"))))
    (eval
     `(@using ,ns
        (ns/export ',ns 'x)
        (defn x () 'expected)
        (defmutable x 'fail)
        (should (equal 'expected (,ns/x)))))))

;;; Redefinitions

(check "can redefine def vars as defmutable vars and set"
  (@using foo
    (def var nil)
    (defmutable var)
    (@set var 'expected)
    (should (equal 'expected (@ var)))))

(check "should get error when redefining a defmutable as a def and using @set"
  (defmutable var)
  (def var nil)
  (should-error (eval `(@set var 'x))))

;;; defn

(check "can call function defined with defn"
  (defn x () 'expected)
  (should (equal 'expected (@call x))))

(check "defn should tolerate body form in docstring position"
  (defn x () "expected")
  (should (equal "expected" (@call x))))

(check "defn should be callable with arguments"
  (defn sqr (i) (* i i))
  (should (equal 9 (@call sqr 3))))

(check "defn falls back to defun for command declarations"
  (@using foo
    (let* ((name (gensym))
           (hash (car (ns/make-key 'foo name))))
      (eval `(defn ,name () (interactive) 'expected))
      (should (commandp hash)))))


;;; ============================== Namespace Macro =============================

;;; namespace

(check "namespace declaration should update current namespace"
  (namespace foo)
  (should (equal 'foo ns/current-ns)))

(check "namespace declaration should provide an emacs feature"
  (let ((ns (gensym)))
    (eval
     `(progn
        (namespace ,ns)
        (should (require ',ns))))))


;;; Exported Functions

(check "can call imported public fn using unqualified symbol"
  (namespace foo :export [ public ])
  (defn public () 'expected)
  (namespace bar :import [ foo ])
  (should (equal 'expected (@call public))))


(check "can call exported fn using qualified symbol"
  (namespace foo :export [ public ])
  (defn public () 'expected)
  (@call foo/public )
  (namespace bar)
  (should (equal 'expected (@call foo/public))))


(check "can call exported fn without @call using qualified symbol"
  (namespace foo :export [ public ])
  (defn public () 'expected)
  (namespace bar)
  (should (equal 'expected (foo/public))))


;;; Dependency Loading

(check "can require elisp features"
  (let ((feature (gensym)))
    (provide feature)
    (eval `(namespace foo :use [ ,feature ]))
    (should (member feature features))))


(check "can load elisp files using period-delimited path"
  (let    ((result))
    (flet (
           (file-exists-p (x) t)
           (load          (f) (setq result f))
           )
      (namespace foo :use [ x.y.z ])
      (should (string-match-p (concat ns/base-path "x/y/z.el$") result)))))


(check "can download packages"
  (let    ((pkg (gensym)) (loaded))
    (flet (
           (package-install (x) (setq loaded x))
           )
      (provide pkg)
      (eval `(namespace foo :packages [ ,pkg ]))
      (should (eq loaded pkg)))))


(check "requires package after download"
  (flet   ((package-install (x)))
    (let  ((pkg (gensym)))
      (provide pkg)
      (eval `(namespace foo :packages [ ,pkg ]))
      (should (member pkg features)))))


;;; Autoloading

(check "can autoload elisp features"
  (let    ((feature (gensym))
           (result))
    (flet (
           (autoload (fn file &optional doc interactive type)
             (setq result `[,fn ,file ,interactive ,type]))
           )
      (provide feature)
      (eval `(namespace foo :use [ (,feature (x :interactive t :type boolean)) ]))
      (should (equal result `[x ,(symbol-name feature) t boolean])))))


(check "can autoload packages"
  (let    ((pkg (gensym))
           (result))
    (flet (
           (package-install (x))

           (autoload (fn file &optional doc interactive type)
             (setq result `[,fn ,file ,interactive ,type]))
           )
      (provide pkg)
      (eval `(namespace foo :packages [ (,pkg (x :interactive t :type boolean))]))
      (should (equal result `[x ,(symbol-name pkg) t boolean])))))


;;; Conditional Loading

(check "loads a dependency when `when` evaluates to true"
  (let ((feature (gensym)))
    (provide feature)
    (eval `(namespace foo :use [ (,feature :when (eq t t)) ]))
    (should (member feature features))))


(check "loads a dependency when `unless` evaluates to nil"
  (let ((feature (gensym)))
    (provide feature)
    (eval `(namespace foo :use [ (,feature :unless (eq t nil)) ]))
    (should (member feature features))))


(check "loads a dependency when `when` is t and `unless` is nil"
  (let ((feature (gensym)))
    (provide feature)
    (eval `(namespace foo :use [ (,feature :when t :unless nil) ]))
    (should (member feature features))))


(check "does not load a dependency when `when` evaluates to nil"
  (namespace foo :use [ (undefined :when (eq t nil)) ]))

(check "does not load a dependency when `unless` evaluates to true"
  (namespace foo :use [ (undefined :unless (eq t t)) ]))

(check "does not load a dependency when `when` is nil and `unless` is nil"
  (namespace foo :use [ (undefined :when nil :unless nil) ]))

(check "does not load a dependency when `when` is t and `unless` is t"
  (namespace foo :use [ (undefined :when t :unless t) ]))


;;; ============================== Integration =================================

;;; Encapsulation

(check "should get error when accessing another namespace's private var using unqualified symbol"
  (def private nil)
  (namespace foo)
  (should-error (eval `(@ private))))


(check "should get error when accessing another namespace's private var using qualified symbol"
  (namespace foo)
  (def private nil)
  (namespace bar)
  (should-error (eval `(@ foo/private))))


(check "should get error when accessing another namespace's private fn using unqualified symbol"
  (defn private ())
  (namespace foo)
  (should-error (eval `(@call private))))


(check "should get error when accessing another namespace's private fn using qualified symbol"
  (namespace foo)
  (defn private ())
  (namespace bar)
  (should-error (eval `(@call foo/private))))


(check "should get error when accessing unimported public var using unqualified symbol"
  (namespace foo :export [ public ])
  (def public nil)
  (namespace bar)
  (should-error (eval `(@ public))))


(check "should get error when setting unimported public var using unqualified symbol"
  (namespace foo :export [ public ])
  (def public nil)
  (namespace bar)
  (should-error (eval `(@set public nil))))


(check "should get error when setting undefined var using unqualified symbol"
  (should-error (eval `(@set x nil))))


(check "should get error when setting undefined member var using qualified symbol"
  (namespace foo)
  (should-error (eval `(@set foo/x nil))))


(check "should get error when calling unimported public fn using unqualified symbol"
  (namespace foo :export [ public ])
  (defn public ())
  (namespace bar)
  (should-error (eval `(@call public))))


;;; Exported Vars

(check "must use accessor function to access public var"
  (namespace foo :export [ public ])
  (def public 'fail)
  (namespace bar :import [ foo ] )
  (should-error (eval `(@ foo/public))))

(check "cannot set public var from another namespace"
  (namespace foo :export [ public ])
  (defmutable public)
  (namespace bar)
  (should-error (eval `(@set foo/public 'fail))))

(check "@sym returns hash of accessor function for qualified symbol"
  (namespace foo :export [ x ])
  (def x nil)
  (namespace bar)
  (should (functionp (@sym foo/x))))

(check "@sym returns hash of accessor function for unqualified symbol"
  (namespace foo :export [ x ])
  (def x nil)
  (namespace bar :import [ foo ])
  (should (functionp (@sym x))))


(check "@sym returns hook variables directly"
  (namespace foo :export [ test-hook ])
  (def test-hook 'x)
  (namespace bar)
  (should (not (functionp (@sym foo/test-hook)))))


;;; ============================================================================

;; Local Variables:
;; no-byte-compile: t
;; End:
