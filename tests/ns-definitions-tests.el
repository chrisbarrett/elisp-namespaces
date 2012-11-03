(provide 'ns-definitions-tests)
(require 'ns-definitions)
(require 'ns-tests)
(require 'ns-core)
(require 'ns-operators)


;;; ^def

(check "can define and read var created with ^def"
  (^def var 'expected)
  (should (equal 'expected (^ var))))

(check "should get error when setting a var created with ^def"
  (^def var nil)
  (should-error (eval `(^set var 'fail))))


;;; ^defmutable

(check "can define and read var created with ^defmutable"
  (^defmutable var 'expected)
  (should (equal 'expected (^ var))))

(check "can set namespaced var created with ^defmutable"
  (^defmutable var)
  (^set var 'expected)
  (should (equal 'expected (^ var))))


;;; Variable Definition

(check "should get error when getting the value of an undefined var"
  (should-error (eval `(^ fail))))

(check "can redefine ^def vars as ^defmutable vars and set"
  (^def var nil)
  (^defmutable var)
  (^set var 'expected)
  (should (equal 'expected (^ var))))

(check "should get error when redefining a ^defmutable as a ^def and using ^set"
  (^defmutable var)
  (^def var nil)
  (should-error (eval `(^set var 'x))))


;; Local Variables:
;; no-byte-compile: t
;; End:
