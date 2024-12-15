(in-package #:world-peace)

;;;; -- Tests --

(defvar *test-count* 0
  "Number of test assertions executed.")

(defmacro is (form)
  "Assert that FORM is true."
  `(progn
     (incf *test-count*)
     (unless ,form
       (error "Assertion failed: ~S" ',form))))

(defmacro is-equal (expected form)
  "Assert that FORM is EQUAL to EXPECTED."
  `(let ((expected-value ,expected)
         (actual-value   ,form))
     (incf *test-count*)
     (unless (equal expected-value actual-value)
       (error "Assertion failed: expected ~S, got ~S from ~S"
              expected-value
              actual-value
              ',form))))

(defmacro is-value-equal (expected form)
  "Assert that FORM is a World Peace value equal to EXPECTED."
  `(let ((expected-value ,expected)
         (actual-value   ,form))
     (incf *test-count*)
     (unless (value-equal-p expected-value actual-value)
       (error "Assertion failed: expected value ~S, got ~S from ~S"
              expected-value
              actual-value
              ',form))))

(defun test-runtime-values ()
  "Test the World Peace runtime value model."
  (is (valuep 0))
  (is (valuep (make-array-value 1 2 3)))
  (is (not (value-equal-p 0 (make-array-value 0))))
  (is (value-equal-p (make-array-value 1 (make-array-value 2))
                     (make-array-value 1 (make-array-value 2))))
  (is-equal 5 (value->integer (make-array-value 5 6)))
  (is-equal 0 (value->integer (empty-array-value)))
  (is (not (value-truthy-p 0)))
  (is (not (value-truthy-p (empty-array-value))))
  (is (not (value-truthy-p (make-array-value 0 0 0))))
  (is (not (value-truthy-p (make-array-value #x66 #x61 #x6c #x73 #x65))))
  (is (value-truthy-p (make-array-value 0 1)))
  (is-value-equal 7 (value-add 3 (make-array-value 4 9)))
  (is-value-equal -2 (value-divide -7 3))
  (is-value-equal -1 (value-remainder -7 3))
  (is-value-equal 8 (value-shift-left 1 3))
  (is-value-equal 2 (value-shift-right 8 2))
  (is-value-equal 1 (boolean->value (value-less-p 2 3)))
  (is-value-equal 0 (boolean->value (value-greater-p 2 3)))
  (is-value-equal 2 (value-index (make-array-value 1 2 3) 1))
  (is (array-value-p (value-index (make-array-value 1) 7)))
  (is-value-equal (make-array-value 1 2 3)
                  (value-push (make-array-value 1 2) 3))
  (is-value-equal (make-array-value 1 2)
                  (value-pop (make-array-value 1 2 3)))
  (is-equal 0 (value->exit-code (empty-array-value)))
  (is-equal 255 (value->exit-code -1)))

(defun run-tests ()
  "Run the World Peace test suite."
  (setf *test-count* 0)
  (test-runtime-values)
  (format t "~D assertions passed.~%" *test-count*))
