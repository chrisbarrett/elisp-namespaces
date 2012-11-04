(provide 'ns-definitions-tests)
(require 'ns-definitions)
(require 'ns-tests)
(require 'ns-core)
(require 'ns-operators)


;;; def

(check "can define and read var created with def"
  (def var 'expected)
  (should (equal 'expected (@ var))))

(check "should get error when setting a var created with def"
  (def var nil)
  (should-error (eval `(@set var 'fail))))


;;; defmutable

(check "can define and read var created with defmutable"
  (defmutable var 'expected)
  (should (equal 'expected (@ var))))

(check "can set namespaced var created with defmutable"
  (defmutable var)
  (@set var 'expected)
  (should (equal 'expected (@ var))))


;;; Redefinitions

(check "can redefine def vars as defmutable vars and set"
  (def var nil)
  (defmutable var)
  (@set var 'expected)
  (should (equal 'expected (@ var))))

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




;; Local Variables:
;; no-byte-compile: t
;; End: