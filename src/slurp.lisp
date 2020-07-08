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
  ;; [CLHS says](http://www.lispworks.com/documentation/HyperSpec/Body/f_in_stm.htm)
  ;; > input-stream-p returns true if stream is an input stream; otherwise, returns false.
  ;; It does not specify the behavior when stream is closed input stream.
  (assert (open-stream-p stream))
  (assert (input-stream-p stream))
  #+clisp
  (check-type reader (or symbol function))
  (let* ((open t)
         (slurper
          (lambda ()
            (handler-case (and open (funcall reader stream))
              (end-of-file ()
                (close stream) (setf open nil))))))
    (trivial-garbage:finalize slurper (lambda () (close stream)))
    slurper))

(declaim
 (ftype (function
         (file-stream (or symbol (function (file-stream) t)) (integer 1 *) &key
          (:step (integer 1 *)) (:offset (integer 0 *)))
         (values (function nil t) &optional))
        make-random-slurper))

(defun make-random-slurper (stream reader max &key (step 1) (offset 0))
  (assert (open-stream-p stream))
  (assert (input-stream-p stream))
  #+clisp
  (check-type reader (or symbol function))
  (let* ((slurper
          (lambda ()
            (assert (file-position stream (+ offset (* (random max) step))))
            (funcall reader stream))))
    (trivial-garbage:finalize slurper (lambda () (close stream)))
    slurper))
