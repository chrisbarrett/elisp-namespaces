(require 'ert)
(require 'cl)
(require 'namespaces)


(defmacro check (desc &rest body)
  "Wrap ert-deftest with a simpler interface for testing namespaces."
  (declare (indent 1))
  `(ert-deftest
       ,(intern (replace-regexp-in-string "[ .]" "-" desc)) ()
     ;; Rebind namespace-private tables for tests.
     (let ((__ns/hashes->symbols-table (copy-hash-table __ns/hashes->symbols-table))
           (__ns/symbols->hashes-table (copy-hash-table __ns/symbols->hashes-table))
           (__ns/exports-table         (copy-hash-table __ns/exports-table)))
       (@using test ,@body))))


;;; namespace

(check "namespace declaration should update current namespace"
  (namespace foo)
  (should (equal 'foo *ns*)))


;;; @using

(check "@using should rebind the current namespace"
  (@using foo (should (equal 'foo *ns*))))


(check "@using should reset *ns* on exit"
  (@using foo nil)
  (should (equal 'test *ns*)))


;;; @sym

(check "@sym should return the underlying symbol for a namespaced symbol"
  (def x)
  (should (string-prefix-p "__ns/namespaced_sym" (symbol-name (@sym x)))))


(check "@sym should return nil when underlying symbol does not exist"
  (should-not (@sym undefined)))


;;; def, @, @set

(check "can define and read namespaced var"
  (def var 'expected)
  (should (equal 'expected (@ var))))


(check "can set namespaced var"
  (def var)
  (@set var 'expected)
  (should (equal 'expected (@ var))))


(check "should get error when getting the value of an undefined var"
  (should-error (eval `(@ fail))))


;;; defn, @call

(check "can define and call namespaced fn"
  (defn x () 'expected)
  (should (equal 'expected (@call x))))


(check "defn should tolerate body form in docstring position"
  (defn x () "expected")
  (should (equal "expected" (@call x))))


(check "defn should be callable with arguments"
  (defn sqr (i) (* i i))
  (should (equal 9 (@call sqr 3))))


(check "should get error when calling an undefined fn"
  (should-error (eval `(@call fail))))


;;; Encapsulation

(check "should get error when accessing another namespace's private var using unqualified symbol"
  (def private)
  (namespace foo)
  (should-error (eval `(@ private))))


(check "should get error when accessing another namespace's private var using qualified symbol"
  (namespace foo)
  (def private)
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
  (def public)
  (namespace bar)
  (should-error (eval `(@ public))))


(check "should get error when setting unimported public var using unqualified symbol"
  (namespace foo :export [ public ])
  (def public)
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


;;; Import/Export

(check "can access imported public var using unqualified symbol"
  (namespace foo :export [ public ])
  (def public 'expected)
  (namespace bar :import [ foo ] )
  (should (equal 'expected (@ public))))


(check "can set imported public var using unqualified symbol"
  (namespace foo :export [ public ])
  (def public nil)
  (namespace bar :import [ foo ])
  (@set public 'expected)
  (should (equal 'expected (@ public))))


(check "can access public var using qualified symbol"
  (namespace foo :export [ public ])
  (def public 'expected)
  (namespace bar)
  (should (equal 'expected (@ foo/public))))


(check "can set public var using qualified symbol"
  (namespace foo :export [ public ])
  (def public nil)
  (namespace bar)
  (@set foo/public 'expected)
  (should (equal 'expected (@ foo/public))))


(check "can call imported public fn using unqualified symbol"
  (namespace foo :export [ public ])
  (defn public () 'expected)
  (namespace bar :import [ foo ])
  (should (equal 'expected (@call public))))


(check "can call public fn using qualified symbol"
  (namespace foo :export [ public ])
  (defn public () 'expected)
  (namespace bar)
  (should (equal 'expected (@call foo/public))))


;;; Dependency loading

(check "can require elisp features"
  (let ((feature (gensym)))
    (provide feature)
    (eval `(namespace foo :use [ ,feature ]))
    (should (member feature features))))


(check "can load elisp files using period-delimited path"
  (let    ((result))
    (flet (
           (file-exists-p (x) t)

           (load (f) (setq result f))
           )
      (namespace foo :use [ x.y.z ])
      (should (string-match-p (concat *ns-base-path* "x/y/z.el$") result)))))


(check "can download packages"
  (let    ((pkg (gensym)) (loaded))
    (flet ((package-install (x) (setq loaded x)))
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

(check "does not load a dependency when `when` is true and `unless` is nil"
  (namespace foo :use [ (undefined :when nil :unless nil) ]))

(check "does not load a dependency when `when` is t and `unless` is t"
  (namespace foo :use [ (undefined :when t :unless t) ]))


;; Local Variables:
;; no-byte-compile: t
;; End:
