; vim: ft=lisp et
(in-package :asdf)
(defsystem "slurp.test"
  :version
  "0.1.0"
  :depends-on
  (:jingoh "slurp")
  :components
  ((:file "slurp"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :slurp args)))
