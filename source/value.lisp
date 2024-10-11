(in-package #:world-peace)

;;;; -- Runtime Values --

(deftype integer-value ()
  "A World Peace scalar integer."
  '(signed-byte 64))

(deftype array-value ()
  "A World Peace array value."
  '(simple-vector *))

(deftype value ()
  "A World Peace value."
  '(or integer-value array-value))

(defun make-array-value (&rest values)
  "Return a World Peace array containing VALUES."
  (coerce values 'simple-vector))
