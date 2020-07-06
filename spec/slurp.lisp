(defpackage :slurp.spec
  (:use :cl :jingoh :slurp))
(in-package :slurp.spec)
(setup :slurp)

(requirements-about MAKE-SLURPER :doc-type function)

;;;; Description:

#+syntax
(MAKE-SLURPER stream reader) ; => result

; MAKE-SLURPER return slurper function.
#?(MAKE-SLURPER (MAKE-STRING-INPUT-STREAM "dummy") #'READ-CHAR) :be-the FUNCTION

; Slurper slurps content of STREAM with READER.
#?(funcall (make-slurper (make-string-input-stream "dummy") #'read-line))
=> "dummy"
,:test equal

; Slurper returns NIL when get END-OF-FILE.
#?(let ((slurper (make-slurper (make-string-input-stream "dummy") #'read-char)))
    (loop :repeat 7 :collect (funcall slurper)))
=> (#\d #\u #\m #\m #\y nil nil)
,:test equal

;;;; Arguments and Values:

; stream := Open input stream, otherwise signals condition depending on implementation.
#?(MAKE-SLURPER "not-stream" #'READ-CHAR) :signals CONDITION

; Not input.
#?(make-slurper (make-string-output-stream) 'read-char)
:signals error

; Not open.
#?(make-slurper (let ((stream (make-string-input-stream "dummy")))
                  (close stream)
                  stream)
                'read-char)
:signals error

; Stream will be closed when get end-of-file.
#?(let* ((stream (make-string-input-stream ""))
         (slurper (make-slurper stream #'read)))
    (values (open-stream-p stream)
            (funcall slurper)
            (open-stream-p stream)))
:values (t nil nil)

; reader := (or symbol (function (stream) *)), otherwise signals condition depending on implementation.
#?(MAKE-SLURPER (MAKE-STRING-INPUT-STREAM "dummy") "not-function-designator") :signals CONDITION

; result := (function nil *)

;;;; Affected By:
; Status of stream.

;;;; Side-Effects:
; Consume stream contents.

;;;; Notes:
; When slurper is GCed, stream will be closed.
#?(defparameter s (make-string-input-stream ""))
=> S

#?(make-slurper s #'read) :be-the function

#?(trivial-garbage:gc) => implementation-dependent

#?(open-stream-p s) => nil

;;;; Exceptional-Situations:
; When specified READER is not function as (function (stream) t), the behavior is unspecified.
#?(make-slurper (make-string-input-stream "") #'car) => unspecified
