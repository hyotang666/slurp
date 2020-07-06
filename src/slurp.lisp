(in-package :cl-user)

(defpackage :slurp
  (:use :cl)
  (:export #:make-slurper))

(in-package :slurp)

(declaim
 (ftype (function (stream (or symbol (function (stream) t)))
         (values (function nil t) &optional))
        make-slurper))

(defun make-slurper (stream reader)
  (let ((slurper
         (lambda ()
           (handler-case (funcall reader stream)
             (end-of-file ()
               (close stream) nil)))))
    (trivial-garbage:finalize slurper (lambda () (close stream)))
    slurper))
