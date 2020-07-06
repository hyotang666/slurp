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
  (let* ((open t)
         (slurper
          (lambda ()
            (handler-case (and open (funcall reader stream))
              (end-of-file ()
                (close stream) (setf open nil))))))
    (trivial-garbage:finalize slurper (lambda () (close stream)))
    slurper))
