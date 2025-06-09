(in-package #:world-peace)

;;;; -- Runtime Values --

(defconstant +integer-bits+ 64
  "Number of bits in a World Peace scalar integer.")

(defconstant +integer-modulus+ (ash 1 +integer-bits+)
  "Modulo used to normalize scalar integer arithmetic.")

(defconstant +integer-sign-bit+ (ash 1 (1- +integer-bits+))
  "Lowest unsigned integer value that represents a negative scalar.")

(deftype integer-value ()
  "A World Peace scalar integer."
  '(signed-byte 64))

(defstruct (array-value
            (:constructor %make-array-value (elements)))
  "A World Peace array value."
  (elements #() :type simple-vector))

(deftype value ()
  "A World Peace value."
  '(or integer-value array-value))

(defun normalize-integer (integer)
  "Return INTEGER wrapped into the World Peace signed 64-bit range."
  (let ((unsigned (mod integer +integer-modulus+)))
    (if (>= unsigned +integer-sign-bit+)
        (- unsigned +integer-modulus+)
        unsigned)))

(defun valuep (value)
  "Return true when VALUE is a World Peace value."
  (or (typep value 'integer-value)
      (array-value-p value)))

(defun make-array-value (&rest values)
  "Return a World Peace array containing VALUES."
  (%make-array-value (coerce values 'simple-vector)))

(defun empty-array-value ()
  "Return the World Peace empty array value."
  (%make-array-value #()))

(defun value->integer (value)
  "Coerce VALUE to an integer for arithmetic."
  (etypecase value
    (integer-value value)
    (array-value
     (let ((elements (array-value-elements value)))
       (if (zerop (length elements))
           0
           (value->integer (aref elements 0)))))))

(defun value-equal-p (left right)
  "Return true when LEFT and RIGHT are strictly equal World Peace values."
  (cond
    ((and (typep left 'integer-value)
          (typep right 'integer-value))
     (= left right))
    ((and (array-value-p left)
          (array-value-p right))
     (let ((left-elements  (array-value-elements left))
           (right-elements (array-value-elements right)))
       (and (= (length left-elements)
               (length right-elements))
            (loop for index below (length left-elements)
                  always (value-equal-p (aref left-elements index)
                                        (aref right-elements index))))))
    (t nil)))

(defun value-false-ascii-p (value)
  "Return true when VALUE is the byte array spelling false."
  (and (array-value-p value)
       (let ((elements (array-value-elements value)))
         (and (= (length elements) 5)
              (= (aref elements 0) #x66)
              (= (aref elements 1) #x61)
              (= (aref elements 2) #x6c)
              (= (aref elements 3) #x73)
              (= (aref elements 4) #x65)))))

(defun value-zero-array-p (value)
  "Return true when VALUE is an array containing only numeric zero values."
  (and (array-value-p value)
       (let ((elements (array-value-elements value)))
         (and (plusp (length elements))
              (loop for index below (length elements)
                    always (let ((element (aref elements index)))
                             (or (and (typep element 'integer-value)
                                      (zerop element))
                                 (value-zero-array-p element))))))))

(defun value-truthy-p (value)
  "Return true when VALUE is truthy in World Peace."
  (cond
    ((typep value 'integer-value)
     (not (zerop value)))
    ((array-value-p value)
     (let ((elements (array-value-elements value)))
       (not (or (zerop (length elements))
                (value-false-ascii-p value)
                (value-zero-array-p value)))))
    (t nil)))

(defun boolean->value (truth)
  "Return the World Peace integer boolean for TRUTH."
  (if truth 1 0))

(defun value-index (value index)
  "Return VALUE indexed by INDEX, or the empty array when out of range."
  (let ((integer-index (value->integer index)))
    (cond
      ((minusp integer-index)
       (empty-array-value))
      ((typep value 'integer-value)
       (if (zerop integer-index)
           value
           (empty-array-value)))
      ((array-value-p value)
       (let ((elements (array-value-elements value)))
         (if (< integer-index (length elements))
             (aref elements integer-index)
             (empty-array-value))))
      (t
       (empty-array-value)))))

(defun value-length (value)
  "Return the length of VALUE as a World Peace integer."
  (etypecase value
    (integer-value 1)
    (array-value (length (array-value-elements value)))))

(defun value-push (array element)
  "Return ARRAY with ELEMENT appended."
  (let* ((old-elements (if (array-value-p array)
                           (array-value-elements array)
                           (vector array)))
         (old-length   (length old-elements))
         (new-elements (make-array (1+ old-length))))
    (replace new-elements old-elements)
    (setf (aref new-elements old-length) element)
    (%make-array-value new-elements)))

(defun value-pop (array)
  "Return ARRAY without its last element."
  (let ((old-elements (if (array-value-p array)
                          (array-value-elements array)
                          (vector array))))
    (if (zerop (length old-elements))
        (empty-array-value)
        (%make-array-value (subseq old-elements 0 (1- (length old-elements)))))))

(defun binary-integer-operation (function left right)
  "Apply FUNCTION to LEFT and RIGHT after arithmetic coercion."
  (normalize-integer (funcall function
                              (value->integer left)
                              (value->integer right))))

(defun value-add (left right)
  "Return LEFT plus RIGHT."
  (binary-integer-operation #'+ left right))

(defun value-subtract (left right)
  "Return LEFT minus RIGHT."
  (binary-integer-operation #'- left right))

(defun value-multiply (left right)
  "Return LEFT multiplied by RIGHT."
  (binary-integer-operation #'* left right))

(defun value-divide (left right)
  "Return LEFT divided by RIGHT, truncating toward zero."
  (let ((divisor (value->integer right)))
    (if (zerop divisor)
        0
        (normalize-integer (truncate (value->integer left) divisor)))))

(defun value-remainder (left right)
  "Return the integer remainder of LEFT divided by RIGHT."
  (let ((divisor (value->integer right)))
    (if (zerop divisor)
        0
        (normalize-integer (rem (value->integer left) divisor)))))

(defun value-shift-left (left right)
  "Return LEFT shifted left by RIGHT bits."
  (normalize-integer (ash (value->integer left) (value->integer right))))

(defun value-shift-right (left right)
  "Return LEFT shifted right by RIGHT bits."
  (normalize-integer (ash (value->integer left) (- (value->integer right)))))

(defun value-bit-and (left right)
  "Return the bitwise and of LEFT and RIGHT."
  (binary-integer-operation #'logand left right))

(defun value-bit-xor (left right)
  "Return the bitwise xor of LEFT and RIGHT."
  (binary-integer-operation #'logxor left right))

(defun value-bit-or (left right)
  "Return the bitwise or of LEFT and RIGHT."
  (binary-integer-operation #'logior left right))

(defun value-negate (value)
  "Return zero minus VALUE."
  (normalize-integer (- (value->integer value))))

(defun value-not (value)
  "Return logical not of VALUE."
  (boolean->value (not (value-truthy-p value))))

(defun value-less-p (left right)
  "Return true when LEFT is numerically less than RIGHT."
  (and (typep left 'integer-value)
       (typep right 'integer-value)
       (< left right)))

(defun value-less-equal-p (left right)
  "Return true when LEFT is numerically less than or equal to RIGHT."
  (and (typep left 'integer-value)
       (typep right 'integer-value)
       (<= left right)))

(defun value-greater-p (left right)
  "Return true when LEFT is numerically greater than RIGHT."
  (and (typep left 'integer-value)
       (typep right 'integer-value)
       (> left right)))

(defun value-greater-equal-p (left right)
  "Return true when LEFT is numerically greater than or equal to RIGHT."
  (and (typep left 'integer-value)
       (typep right 'integer-value)
       (>= left right)))

(defun value->exit-code (value)
  "Coerce VALUE to a process exit code."
  (let ((integer (etypecase value
                   (integer-value value)
                   (array-value
                    (let ((elements (array-value-elements value)))
                      (if (zerop (length elements))
                          0
                          (value->integer (aref elements 0))))))))
    (ldb (byte 8 0) integer)))

(defun string->byte-array (string)
  "Return STRING as a World Peace byte array."
  (apply #'make-array-value
         (loop for character across string
               collect (char-code character))))

(defun write-value-readable (value &optional (stream *standard-output*))
  "Write VALUE in a readable World Peace shape to STREAM."
  (etypecase value
    (integer-value
     (format stream "~D" value))
    (array-value
     (write-char #\[ stream)
     (loop for index below (length (array-value-elements value))
           do (when (plusp index)
                (format stream ", "))
              (write-value-readable (aref (array-value-elements value) index)
                                    stream))
     (write-char #\] stream))))
