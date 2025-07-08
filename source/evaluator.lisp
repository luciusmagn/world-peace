(in-package #:world-peace)

;;;; -- Evaluation Conditions --

(define-condition world-peace-runtime-error (error)
  ((message :initarg :message :reader world-peace-runtime-error-message))
  (:documentation "Signals invalid World Peace runtime behavior.")
  (:report (lambda (condition stream)
             (format stream "~A" (world-peace-runtime-error-message condition)))))

(define-condition return-signal (condition)
  ((value :initarg :value :reader return-signal-value))
  (:documentation "Carries an early return value."))

;;;; -- Runtime State --

(defstruct environment
  "A lexical environment."
  (bindings (make-hash-table :test 'equal))
  parent)

(defstruct runtime
  "A running World Peace program."
  (globals       (make-hash-table :test 'equal))
  (functions     (make-hash-table :test 'equal))
  (output-stream *standard-output*)
  (input-stream  *standard-input*)
  (errno         0 :type integer-value)
  (argv          (empty-array-value)))

(defun runtime-error (message)
  "Signal a runtime error with MESSAGE."
  (error 'world-peace-runtime-error :message message))

(defun copy-value (value)
  "Return a pass-by-copy copy of VALUE."
  (etypecase value
    (integer-value value)
    (array-value
     (apply #'make-array-value
            (loop for element across (array-value-elements value)
                  collect (copy-value element))))))

(defun environment-define (environment name value)
  "Bind NAME to VALUE in ENVIRONMENT."
  (setf (gethash name (environment-bindings environment)) value))

(defun environment-bound-p (environment name)
  "Return true when NAME is bound in ENVIRONMENT or its parents."
  (cond
    ((null environment) nil)
    ((nth-value 1 (gethash name (environment-bindings environment))) t)
    (t (environment-bound-p (environment-parent environment) name))))

(defun runtime-global-bound-p (runtime name)
  "Return true when NAME is a global in RUNTIME."
  (nth-value 1 (gethash name (runtime-globals runtime))))

(defun environment-ref (runtime environment name)
  "Return NAME from ENVIRONMENT or RUNTIME."
  (cond
    ((and environment
          (nth-value 1 (gethash name (environment-bindings environment))))
     (gethash name (environment-bindings environment)))
    ((and environment
          (environment-parent environment))
     (environment-ref runtime (environment-parent environment) name))
    ((runtime-global-bound-p runtime name)
     (gethash name (runtime-globals runtime)))
    ((string= name "errno")
     (runtime-errno runtime))
    ((string= name "argv")
     (runtime-argv runtime))
    (t
     (runtime-error (format nil "Unbound name ~A" name)))))

(defun environment-set (runtime environment name value)
  "Set NAME to VALUE in ENVIRONMENT or RUNTIME."
  (cond
    ((and environment
          (nth-value 1 (gethash name (environment-bindings environment))))
     (setf (gethash name (environment-bindings environment)) value))
    ((and environment
          (environment-parent environment))
     (environment-set runtime (environment-parent environment) name value))
    ((runtime-global-bound-p runtime name)
     (setf (gethash name (runtime-globals runtime)) value))
    (environment
     (environment-define environment name value))
    (t
     (setf (gethash name (runtime-globals runtime)) value))))

(defun environment-alist (environment)
  "Return ENVIRONMENT bindings as an alist."
  (let ((items '()))
    (when (environment-parent environment)
      (setf items (environment-alist (environment-parent environment))))
    (maphash (lambda (key value)
               (push (cons key value) items))
             (environment-bindings environment))
    items))

;;;; -- Builtins --

(defun byte-value-write (value stream)
  "Write VALUE as bytes to STREAM."
  (etypecase value
    (integer-value
     (write-char (code-char (ldb (byte 8 0) value)) stream))
    (array-value
     (loop for element across (array-value-elements value)
           do (byte-value-write element stream)))))

(defun builtin-print (runtime arguments)
  "Write ARGUMENTS to the runtime output stream."
  (dolist (argument arguments 0)
    (byte-value-write argument (runtime-output-stream runtime))))

(defun builtin-read (runtime arguments)
  "Read one byte from the runtime input stream."
  (declare (ignore arguments))
  (let ((character (read-char (runtime-input-stream runtime) nil nil)))
    (if character
        (char-code character)
        (empty-array-value))))

#+sbcl
(defun current-errno ()
  "Return the current libc errno."
  (require :sb-posix)
  (let ((symbol (find-symbol "GET-ERRNO" "SB-POSIX")))
    (if symbol
        (funcall symbol)
        0)))

#+sbcl
(defun linux-syscall (arguments)
  "Call libc syscall with ARGUMENTS."
  (let ((numbers (loop for index below 7
                       collect (if (< index (length arguments))
                                   (value->integer (nth index arguments))
                                   0))))
    (apply #'sb-alien:alien-funcall
           (sb-alien:extern-alien
            "syscall"
            (function sb-alien:long
                      sb-alien:long
                      sb-alien:long
                      sb-alien:long
                      sb-alien:long
                      sb-alien:long
                      sb-alien:long
                      sb-alien:long))
           numbers)))

(defun builtin-syscall (runtime arguments)
  "Call a system syscall with ARGUMENTS."
  (if (or (null arguments)
          (> (length arguments) 7))
      (progn
        (setf (runtime-errno runtime) 22)
        -1)
      #+sbcl
      (let ((result (linux-syscall arguments)))
        (if (= result -1)
            (progn
              (setf (runtime-errno runtime) (current-errno))
              -1)
            (progn
              (setf (runtime-errno runtime) 0)
              (normalize-integer result))))
      #-sbcl
      (progn
        (setf (runtime-errno runtime) 38)
        -1)))

(defun call-builtin (runtime name arguments)
  "Call builtin NAME with ARGUMENTS when it exists."
  (cond
    ((string= name "len")
     (value-length (first arguments)))
    ((string= name "push")
     (value-push (first arguments) (second arguments)))
    ((string= name "pop")
     (value-pop (first arguments)))
    ((string= name "print")
     (builtin-print runtime arguments))
    ((string= name "read")
     (builtin-read runtime arguments))
    ((string= name "syscall")
     (builtin-syscall runtime arguments))
    (t nil)))

(defun builtin-name-p (name)
  "Return true when NAME is a builtin function."
  (member name '("len" "push" "pop" "print" "read" "syscall") :test #'string=))

(defun runtime-function-bound-p (runtime name)
  "Return true when RUNTIME has a function named NAME."
  (and (gethash name (runtime-functions runtime)) t))

(defun callable-value-p (runtime environment name)
  "Return true when NAME is bound as a callable value."
  (or (environment-bound-p environment name)
      (runtime-global-bound-p runtime name)
      (string= name "argv")))

;;;; -- Expression Evaluation --

(defun apply-binary-operator (operator left right)
  "Apply binary OPERATOR to LEFT and RIGHT."
  (ecase operator
    (:add (value-add left right))
    (:subtract (value-subtract left right))
    (:multiply (value-multiply left right))
    (:divide (value-divide left right))
    (:remainder (value-remainder left right))
    (:left-shift (value-shift-left left right))
    (:right-shift (value-shift-right left right))
    (:bit-and (value-bit-and left right))
    (:bit-xor (value-bit-xor left right))
    (:bit-or (value-bit-or left right))
    (:equal (boolean->value (value-equal-p left right)))
    (:not-equal (boolean->value (not (value-equal-p left right))))
    (:less (boolean->value (value-less-p left right)))
    (:less-equal (boolean->value (value-less-equal-p left right)))
    (:greater (boolean->value (value-greater-p left right)))
    (:greater-equal (boolean->value (value-greater-equal-p left right)))
    (:logical-and (boolean->value (and (value-truthy-p left)
                                       (value-truthy-p right))))
    (:logical-or (boolean->value (or (value-truthy-p left)
                                     (value-truthy-p right))))))

(defun evaluate-expression-list (runtime environment expressions)
  "Evaluate EXPRESSIONS in RUNTIME and ENVIRONMENT."
  (mapcar (lambda (expression)
            (evaluate-expression runtime environment expression))
          expressions))

(defun call-function-by-name (runtime name arguments)
  "Call function NAME with ARGUMENTS."
  (let ((builtin-result (and (builtin-name-p name)
                             (call-builtin runtime name arguments))))
    (if (or builtin-result
            (builtin-name-p name))
        builtin-result
        (invoke-user-function runtime name arguments))))

(defun evaluate-call (runtime environment expression)
  "Evaluate a call EXPRESSION."
  (let ((callee    (call-expression-callee expression))
        (arguments (evaluate-expression-list runtime
                                             environment
                                             (call-expression-arguments expression))))
    (typecase callee
      (name-expression
       (let ((name (name-expression-name callee)))
         (if (or (builtin-name-p name)
                 (runtime-function-bound-p runtime name)
                 (not (callable-value-p runtime environment name)))
             (call-function-by-name runtime name arguments)
             (if (= (length arguments) 1)
                 (value-index (environment-ref runtime environment name)
                              (first arguments))
                 (empty-array-value)))))
      (t
       (if (= (length arguments) 1)
           (value-index (evaluate-expression runtime environment callee)
                        (first arguments))
           (empty-array-value))))))

(defun evaluate-method-call (runtime environment expression)
  "Evaluate a method-call EXPRESSION."
  (let* ((receiver-value (evaluate-expression runtime
                                              environment
                                              (method-call-expression-receiver expression)))
         (arguments      (cons receiver-value
                               (evaluate-expression-list
                                runtime
                                environment
                                (method-call-expression-arguments expression))))
         (result         (call-function-by-name runtime
                                                (method-call-expression-name expression)
                                                arguments)))
    (when (and (typep (method-call-expression-receiver expression)
                      'name-expression)
               (member (method-call-expression-name expression)
                       '("push" "pop")
                       :test #'string=))
      (environment-set runtime
                       environment
                       (name-expression-name
                        (method-call-expression-receiver expression))
                       result))
    result))

(defun evaluate-expression (runtime environment expression)
  "Evaluate EXPRESSION."
  (typecase expression
    (integer-expression
     (integer-expression-value expression))
    (name-expression
     (environment-ref runtime environment (name-expression-name expression)))
    (array-expression
     (apply #'make-array-value
            (evaluate-expression-list runtime
                                      environment
                                      (array-expression-elements expression))))
    (unary-expression
     (let ((operand (evaluate-expression runtime
                                         environment
                                         (unary-expression-operand expression))))
       (ecase (unary-expression-operator expression)
         (:not (value-not operand))
         (:negate (value-negate operand)))))
    (binary-expression
     (if (eq (binary-expression-operator expression) :logical-and)
         (let ((left (evaluate-expression runtime
                                          environment
                                          (binary-expression-left expression))))
           (if (value-truthy-p left)
               (boolean->value
                (value-truthy-p
                 (evaluate-expression runtime
                                      environment
                                      (binary-expression-right expression))))
               0))
         (if (eq (binary-expression-operator expression) :logical-or)
             (let ((left (evaluate-expression runtime
                                              environment
                                              (binary-expression-left expression))))
               (if (value-truthy-p left)
                   1
                   (boolean->value
                    (value-truthy-p
                     (evaluate-expression runtime
                                          environment
                                          (binary-expression-right expression))))))
             (apply-binary-operator
              (binary-expression-operator expression)
              (evaluate-expression runtime environment (binary-expression-left expression))
              (evaluate-expression runtime environment (binary-expression-right expression))))))
    (call-expression
     (evaluate-call runtime environment expression))
    (method-call-expression
     (evaluate-method-call runtime environment expression))
    (method-reference-expression
     (runtime-error "Method references are not runtime values"))
    (index-expression
     (value-index (evaluate-expression runtime
                                       environment
                                       (index-expression-receiver expression))
                  (evaluate-expression runtime
                                       environment
                                       (index-expression-index expression))))
    (t
     (runtime-error "Unknown expression"))))

;;;; -- Statement Evaluation --

(defun assign-value (runtime environment statement)
  "Apply assignment STATEMENT and return the assigned value."
  (let* ((old-value (and (not (eq (assignment-statement-operator statement) :assign))
                         (environment-ref runtime
                                          environment
                                          (assignment-statement-name statement))))
         (new-value (evaluate-expression runtime
                                         environment
                                         (assignment-statement-expression statement)))
         (value     (if (eq (assignment-statement-operator statement) :assign)
                        new-value
                        (apply-binary-operator (assignment-statement-operator statement)
                                               old-value
                                               new-value))))
    (environment-set runtime environment (assignment-statement-name statement) value)
    value))

(defun evaluate-statements (runtime environment statements)
  "Evaluate STATEMENTS."
  (dolist (statement statements)
    (evaluate-statement runtime environment statement)))

(defun evaluate-if-statement (runtime environment statement)
  "Evaluate an if STATEMENT."
  (if (value-truthy-p
       (evaluate-expression runtime environment (if-statement-condition statement)))
      (evaluate-statements runtime environment (if-statement-then-body statement))
      (evaluate-statements runtime environment (if-statement-else-body statement))))

(defun evaluate-step-loop (runtime environment statement)
  "Evaluate a stepping do loop STATEMENT."
  (assign-value runtime environment (do-statement-assignment statement))
  (let* ((name      (assignment-statement-name (do-statement-assignment statement)))
         (goal      (value->integer
                     (evaluate-expression runtime environment (do-statement-goal statement))))
         (step      (if (do-statement-step statement)
                        (value->integer
                         (evaluate-expression runtime environment (do-statement-step statement)))
                        1))
         (magnitude (max 1 (abs step))))
    (loop
      (let* ((current   (value->integer (environment-ref runtime environment name)))
             (direction (if (minusp step)
                            -1
                            (if (<= current goal) 1 -1))))
        (unless (if (plusp direction)
                    (< current goal)
                    (> current goal))
          (return))
        (evaluate-statements runtime environment (do-statement-body statement))
        (environment-set runtime
                         environment
                         name
                         (normalize-integer (+ current (* direction magnitude))))))))

(defun evaluate-do-statement (runtime environment statement)
  "Evaluate a do loop STATEMENT."
  (ecase (do-statement-kind statement)
    (:forever
     (loop do (evaluate-statements runtime environment (do-statement-body statement))))
    (:while
     (loop while (value-truthy-p
                  (evaluate-expression runtime
                                       environment
                                       (do-statement-condition statement)))
           do (evaluate-statements runtime environment (do-statement-body statement))))
    (:step
     (evaluate-step-loop runtime environment statement))))

(defun evaluate-case-statement (runtime environment statement)
  "Evaluate a case STATEMENT."
  (loop for arm in (case-statement-arms statement)
        for matched-p = (or (wildcard-pattern-p (case-arm-pattern arm))
                            (pattern-match-p
                             (case-arm-pattern arm)
                             (evaluate-expression runtime
                                                  environment
                                                  (case-arm-test-expression arm))
                             (environment-alist environment)))
        when matched-p
          do (evaluate-statements runtime environment (case-arm-body arm))
             (return nil)))

(defun evaluate-statement (runtime environment statement)
  "Evaluate STATEMENT."
  (typecase statement
    (variable-statement
     (let ((declaration (variable-statement-declaration statement)))
       (environment-define environment
                           (variable-declaration-name declaration)
                           (evaluate-expression runtime
                                                environment
                                                (variable-declaration-expression declaration)))))
    (assignment-statement
     (assign-value runtime environment statement))
    (expression-statement
     (evaluate-expression runtime environment (expression-statement-expression statement)))
    (return-statement
     (signal 'return-signal
             :value (if (return-statement-expression statement)
                        (evaluate-expression runtime
                                             environment
                                             (return-statement-expression statement))
                        (empty-array-value))))
    (if-statement
     (evaluate-if-statement runtime environment statement))
    (do-statement
     (evaluate-do-statement runtime environment statement))
    (case-statement
     (evaluate-case-statement runtime environment statement))
    (t
     (runtime-error "Unknown statement"))))

;;;; -- Program Evaluation --

(defun register-function (runtime function)
  "Register FUNCTION in RUNTIME."
  (push function (gethash (function-declaration-name function)
                          (runtime-functions runtime))))

(defun register-program (runtime program)
  "Register PROGRAM globals and functions in RUNTIME."
  (dolist (item (program-items program))
    (when (function-declaration-p item)
      (register-function runtime item)))
  (maphash (lambda (name functions)
             (setf (gethash name (runtime-functions runtime))
                   (nreverse functions)))
           (runtime-functions runtime))
  (dolist (item (program-items program))
    (when (variable-declaration-p item)
      (setf (gethash (variable-declaration-name item) (runtime-globals runtime))
            (evaluate-expression runtime
                                 nil
                                 (variable-declaration-expression item))))))

(defun slice-argument (value start end)
  "Return the inclusive START to END subarray of VALUE."
  (if (array-value-p value)
      (let ((elements (array-value-elements value)))
        (if (and (<= 0 start end)
                 (< end (length elements)))
            (apply #'make-array-value
                   (loop for index from start to end
                         collect (copy-value (aref elements index))))
            (empty-array-value)))
      (empty-array-value)))

(defun bind-parameter (environment parameter argument)
  "Bind PARAMETER to ARGUMENT in ENVIRONMENT."
  (let ((filter (parameter-filter parameter)))
    (environment-define
     environment
     (parameter-name parameter)
     (typecase filter
       (parameter-slice-filter
        (slice-argument argument
                        (parameter-slice-filter-start filter)
                        (parameter-slice-filter-end filter)))
       (t
        (copy-value argument))))))

(defun parameter-matches-p (parameter argument)
  "Return true when ARGUMENT matches PARAMETER."
  (let ((filter (parameter-filter parameter)))
    (typecase filter
      (parameter-pattern-filter
       (pattern-match-p (parameter-pattern-filter-pattern filter) argument))
      (t t))))

(defun function-matches-p (function arguments)
  "Return true when FUNCTION accepts ARGUMENTS."
  (and (= (length (function-declaration-parameters function))
          (length arguments))
       (loop for parameter in (function-declaration-parameters function)
             for argument in arguments
             always (parameter-matches-p parameter argument))))

(defun evaluate-end-clause (runtime environment end-clause)
  "Evaluate END-CLAUSE."
  (cond
    ((end-clause-expression end-clause)
     (evaluate-expression runtime environment (end-clause-expression end-clause)))
    ((end-clause-arms end-clause)
     (loop for arm in (end-clause-arms end-clause)
           when (or (wildcard-pattern-p (pattern-arm-pattern arm))
                    (pattern-match-p
                     (pattern-arm-pattern arm)
                     (evaluate-expression runtime
                                          environment
                                          (pattern-arm-test-expression arm))
                     (environment-alist environment)))
             do (return (evaluate-expression runtime
                                             environment
                                             (pattern-arm-result arm)))
           finally (return (empty-array-value))))
    (t
     (empty-array-value))))

(defun invoke-matched-function (runtime function arguments)
  "Invoke FUNCTION with ARGUMENTS."
  (let ((environment (make-environment)))
    (loop for parameter in (function-declaration-parameters function)
          for argument in arguments
          do (bind-parameter environment parameter argument))
    (handler-case
        (progn
          (evaluate-statements runtime environment (function-declaration-body function))
          (evaluate-end-clause runtime
                               environment
                               (function-declaration-end-clause function)))
      (return-signal (condition)
        (return-signal-value condition)))))

(defun invoke-user-function (runtime name arguments)
  "Invoke user function NAME with ARGUMENTS."
  (let ((functions (gethash name (runtime-functions runtime))))
    (loop for function in functions
          when (function-matches-p function arguments)
            do (return (invoke-matched-function runtime function arguments))
          finally (return (empty-array-value)))))

(defun evaluate-program (program &key output-stream input-stream arguments)
  "Evaluate PROGRAM by invoking main."
  (let* ((argument-values (when arguments
                            (mapcar #'copy-value arguments)))
         (runtime         (make-runtime
                           :output-stream (or output-stream *standard-output*)
                           :input-stream  (or input-stream *standard-input*)
                           :argv          (apply #'make-array-value
                                                 argument-values))))
    (register-program runtime program)
    (call-function-by-name runtime "main" '())))

(defun evaluate-source (source &key output-stream input-stream arguments)
  "Parse and evaluate World Peace SOURCE."
  (evaluate-program (parse-source source)
                    :output-stream output-stream
                    :input-stream input-stream
                    :arguments arguments))
