(in-package #:world-peace)

;;;; -- Pattern Matching --

(defun environment-value (name environment)
  "Return NAME's value from ENVIRONMENT, or NIL when absent."
  (cdr (assoc name environment :test #'string=)))

(defun rest-pattern-index (patterns)
  "Return the index of the rest marker in PATTERNS, if present."
  (position-if #'rest-pattern-p patterns))

(defun match-pattern-sequence-p (patterns values environment)
  "Return true when PATTERNS match VALUES."
  (and (= (length patterns) (length values))
       (loop for pattern in patterns
             for value across values
             always (pattern-match-p pattern value environment))))

(defun match-array-pattern-p (pattern value environment)
  "Return true when array PATTERN matches VALUE."
  (and (array-value-p value)
       (let* ((patterns   (array-pattern-elements pattern))
              (values     (array-value-elements value))
              (rest-index (rest-pattern-index patterns)))
         (cond
           ((null rest-index)
            (match-pattern-sequence-p patterns values environment))
           ((not (= rest-index (position-if #'rest-pattern-p patterns :from-end t)))
            nil)
           (t
            (let* ((prefix-patterns (subseq patterns 0 rest-index))
                   (suffix-patterns (subseq patterns (1+ rest-index)))
                   (prefix-length   (length prefix-patterns))
                   (suffix-length   (length suffix-patterns))
                   (value-length    (length values)))
              (and (>= value-length (+ prefix-length suffix-length))
                   (match-pattern-sequence-p
                    prefix-patterns
                    (subseq values 0 prefix-length)
                    environment)
                   (match-pattern-sequence-p
                    suffix-patterns
                    (subseq values (- value-length suffix-length))
                    environment))))))))

(defun pattern-match-p (pattern value &optional environment)
  "Return true when PATTERN matches VALUE."
  (typecase pattern
    (wildcard-pattern t)
    (literal-pattern
     (value-equal-p (literal-pattern-value pattern) value))
    (name-pattern
     (let ((bound-value (environment-value (name-pattern-name pattern) environment)))
       (and bound-value
            (value-equal-p bound-value value))))
    (range-pattern
     (let ((integer (value->integer value)))
       (<= (range-pattern-start pattern)
           integer
           (range-pattern-end pattern))))
    (array-pattern
     (match-array-pattern-p pattern value environment))
    (t nil)))
