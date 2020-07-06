(in-package :cl-user)

(defpackage :slurp
  (:use :cl)
  (:export #:make-slurper))

(in-package :slurp)

(defun make-slurper (&key stream reader)
  (let ((slurper
         (lambda ()
           (handler-case (funcall reader stream)
             (end-of-file ()
               (close stream) nil)))))
    (trivial-garbage:finalize slurper (lambda () (close stream)))
    slurper))
