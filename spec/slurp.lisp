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

#+ccl ; needs some times to gc finished.
#?(sleep 0.5) => NIL

#?(open-stream-p s) => nil

;;;; Exceptional-Situations:
; When specified READER is not function as (function (stream) t), the behavior is unspecified.
#?(make-slurper (make-string-input-stream "") #'car) => unspecified

(requirements-about MAKE-RANDOM-SLURPER :doc-type function)

;;;; Description:
; Make random slurper which slurps content with READER from specified file stream.

#+syntax
(MAKE-RANDOM-SLURPER stream reader max &key (step 1) (offset 0)) ; => result

#?(with-open-file (s (asdf:system-source-file (asdf:find-system :slurp)))
    #| first line is
; vim: ft=lisp et
    |#
    (let ((slurper (make-random-slurper s #'read-char 5)))
      (loop :repeat 10 :collect (funcall slurper))))
:satisfies (lambda (result)
             (loop :for char :in result :always (find char "; vim")))

;;;; Arguments and Values:

; stream := file input stream, otherwise condition.
#?(MAKE-RANDOM-SLURPER "not-stream" (CONSTANTLY T) 1) :signals condition
#?(MAKE-RANDOM-SLURPER (MAKE-STRING-INPUT-STREAM "not file-stream")
                       (CONSTANTLY T) 1)
:signals condition
#?(with-open-file (s (asdf:system-source-file (asdf:find-system :slurp))
                     :direction :output ; <--- output stream.
                     :if-exists :append)
    (make-random-slurper s (constantly t) 1))
:signals condition

; reader := (or symbol (function (file-stream) *)), otherwise condition.
#?(WITH-OPEN-FILE
      (S (ASDF/SYSTEM:SYSTEM-SOURCE-FILE (ASDF/SYSTEM:FIND-SYSTEM :SLURP)))
    (MAKE-RANDOM-SLURPER S "not function designator" 1))
:signals condition

; If specified function is not as (function (file-stream) *) undefined.
#?(with-open-file (s (asdf:system-source-file (asdf:find-system :slurp)))
    (make-random-slurper s #'car 1))
=> unspecified

; max := (integer 1), otherwise condition.
#?(WITH-OPEN-FILE
      (S (ASDF/SYSTEM:SYSTEM-SOURCE-FILE (ASDF/SYSTEM:FIND-SYSTEM :SLURP)))
    (MAKE-RANDOM-SLURPER S (CONSTANTLY T) "not integer"))
:signals condition

#?(WITH-OPEN-FILE
      (S (ASDF/SYSTEM:SYSTEM-SOURCE-FILE (ASDF/SYSTEM:FIND-SYSTEM :SLURP)))
    (MAKE-RANDOM-SLURPER S (CONSTANTLY T) 0))
:signals condition

; Never slurp contents after max.
#?(with-open-file (s (asdf:system-source-file (asdf:find-system :slurp)))
    (let ((slurper (make-random-slurper s #'read-char 1)))
      (loop :repeat 10 :collect (funcall slurper))))
=> (#\; #\; #\; #\; #\; #\; #\; #\; #\; #\;)
,:test equal

; step := (:step (integer 1)), otherwise condition.
#?(WITH-OPEN-FILE
      (S (ASDF/SYSTEM:SYSTEM-SOURCE-FILE (ASDF/SYSTEM:FIND-SYSTEM :SLURP)))
    (MAKE-RANDOM-SLURPER S (CONSTANTLY T) 1 :STEP "not integer"))
:signals condition

#?(WITH-OPEN-FILE
      (S (ASDF/SYSTEM:SYSTEM-SOURCE-FILE (ASDF/SYSTEM:FIND-SYSTEM :SLURP)))
    (MAKE-RANDOM-SLURPER S (CONSTANTLY T) 1 :STEP 0))
:signals condition

#?(with-open-file (s (asdf:system-source-file (asdf:find-system :slurp)))
    (let ((slurper (make-random-slurper s #'file-position 5 :step 2)))
      (loop :repeat 10 :collect (funcall slurper))))
:satisfies (lambda (result)
             (loop :for i :in result :always (find i '(0 2 4 6 8))))

; offset := (:offset unsigned-byte), otherwise condition.
#?(WITH-OPEN-FILE
      (S (ASDF/SYSTEM:SYSTEM-SOURCE-FILE (ASDF/SYSTEM:FIND-SYSTEM :SLURP)))
    (MAKE-RANDOM-SLURPER S (CONSTANTLY T) 1 :OFFSET "not integer"))
:signals condition

#?(WITH-OPEN-FILE
      (S (ASDF/SYSTEM:SYSTEM-SOURCE-FILE (ASDF/SYSTEM:FIND-SYSTEM :SLURP)))
    (MAKE-RANDOM-SLURPER S (CONSTANTLY T) 1 :OFFSET -1))
:signals condition

; Never slurp contents before offset.
#?(with-open-file (s (asdf:system-source-file (asdf:find-system :slurp)))
    (let ((slurper (make-random-slurper s #'file-position 5 :step 2 :offset 1)))
      (loop :repeat 10 :collect (funcall slurper))))
:satisfies (lambda (result)
             (loop :for char :in result :always (find char '(1 3 5 7 9))))

; result := (function nil *)

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:
; Stream is closed only when slurper function is GCed.
#?(DEFPARAMETER S
    (OPEN (ASDF/SYSTEM:SYSTEM-SOURCE-FILE (ASDF/SYSTEM:FIND-SYSTEM :SLURP))))
=> S

#?(MAKE-RANDOM-SLURPER S #'READ-CHAR 5) :be-the FUNCTION

#?(TRIVIAL-GARBAGE:GC) => IMPLEMENTATION-DEPENDENT

#?(OPEN-STREAM-P S) => NIL

;;;; Exceptional-Situations:

