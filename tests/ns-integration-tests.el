(provide 'ns-integration-tests)
(require 'ns-operators)
(require 'ns-definitions)
(require 'ert)

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

(check "can access imported public var using unqualified symbol"
  (namespace foo :export [ public ])
  (def public 'expected)
  (namespace bar :import [ foo ] )
  (should (equal 'expected (@ public))))


(check "can set imported public var using unqualified symbol"
  (namespace foo :export [ public ])
  (defmutable public)
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
  (defmutable public)
  (namespace bar)
  (@set foo/public 'expected)
  (should (equal 'expected (@ foo/public))))



;; Local Variables:
;; no-byte-compile: t
;; End: