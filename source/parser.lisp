(in-package #:world-peace)

;;;; -- Parser Conditions --

(define-condition world-peace-parse-error (error)
  ((message :initarg :message :reader world-peace-parse-error-message)
   (token   :initarg :token   :reader world-peace-parse-error-token))
  (:documentation "Signals invalid World Peace source syntax.")
  (:report (lambda (condition stream)
             (let ((token (world-peace-parse-error-token condition)))
               (format stream "~A at byte ~D"
                       (world-peace-parse-error-message condition)
                       (token-start token))))))

;;;; -- Syntax Tree --

(defstruct integer-expression
  "A numeric literal expression."
  (value 0 :type integer-value))

(defstruct name-expression
  "A variable or function name expression."
  (name "" :type string))

(defstruct array-expression
  "An array literal expression."
  (elements '() :type list))

(defstruct unary-expression
  "A unary operator expression."
  (operator nil :type keyword)
  operand)

(defstruct binary-expression
  "A binary operator expression."
  (operator nil :type keyword)
  left
  right)

(defstruct call-expression
  "A function call expression."
  callee
  (arguments '() :type list))

(defstruct method-call-expression
  "An alternate function call expression."
  receiver
  (name "" :type string)
  (arguments '() :type list))

(defstruct method-reference-expression
  "A dotted method reference expression."
  receiver
  (name "" :type string))

(defstruct index-expression
  "An index expression."
  receiver
  index)

(defstruct wildcard-pattern
  "A pattern that matches any value.")

(defstruct literal-pattern
  "A strict literal value pattern."
  value)

(defstruct name-pattern
  "A pattern that matches the value bound to NAME."
  (name "" :type string))

(defstruct range-pattern
  "An inclusive integer range pattern."
  (start 0 :type integer-value)
  (end   0 :type integer-value))

(defstruct rest-pattern
  "A rest marker inside an array pattern.")

(defstruct array-pattern
  "An array pattern."
  (elements '() :type list))

(defstruct program
  "A World Peace source file."
  (items '() :type list))

(defstruct load-item
  "A load statement."
  (source "" :type string)
  (url-p nil :type boolean))

(defstruct variable-declaration
  "A variable declaration."
  (name "" :type string)
  expression)

(defstruct parameter
  "A function parameter."
  (name "" :type string)
  filter)

(defstruct parameter-pattern-filter
  "A parameter pattern filter."
  pattern)

(defstruct parameter-slice-filter
  "A parameter subarray filter."
  (start 0 :type integer-value)
  (end   0 :type integer-value))

(defstruct function-declaration
  "A function declaration."
  (name       "" :type string)
  (parameters '() :type list)
  (body       '() :type list)
  end-clause)

(defstruct end-clause
  "A function end clause."
  expression
  (arms '() :type list))

(defstruct pattern-arm
  "A pattern-matching arm."
  test-expression
  pattern
  result)

(defstruct variable-statement
  "A local variable declaration statement."
  declaration)

(defstruct assignment-statement
  "An assignment statement."
  (name "" :type string)
  (operator :assign :type keyword)
  expression)

(defstruct expression-statement
  "An expression statement."
  expression)

(defstruct return-statement
  "An early return statement."
  expression)

(defstruct if-statement
  "An if statement."
  condition
  (then-body '() :type list)
  (else-body '() :type list))

(defstruct do-statement
  "A do loop statement."
  kind
  assignment
  step
  goal
  condition
  (body '() :type list))

(defstruct case-statement
  "A case statement."
  (arms '() :type list))

(defstruct case-arm
  "A case arm."
  test-expression
  pattern
  (body '() :type list))

;;;; -- Parser State --

(defstruct parser
  "Parser state over a vector of tokens."
  (tokens #() :type vector)
  (index  0   :type fixnum))

(defun current-token (parser)
  "Return PARSER's current token."
  (aref (parser-tokens parser) (parser-index parser)))

(defun previous-token (parser)
  "Return the token before PARSER's current token."
  (aref (parser-tokens parser) (1- (parser-index parser))))

(defun current-token-type (parser)
  "Return the type of PARSER's current token."
  (token-type (current-token parser)))

(defun peek-token-type (parser &optional (offset 1))
  "Return the token type OFFSET tokens after PARSER's current token."
  (let ((index (+ (parser-index parser) offset)))
    (if (< index (length (parser-tokens parser)))
        (token-type (aref (parser-tokens parser) index))
        :eof)))

(defun parser-at-p (parser type)
  "Return true when PARSER's current token has TYPE."
  (eq (current-token-type parser) type))

(defun parser-error (parser message)
  "Signal a parse error at PARSER's current token with MESSAGE."
  (error 'world-peace-parse-error
         :message message
         :token (current-token parser)))

(defun advance-parser (parser)
  "Consume and return PARSER's current token."
  (unless (parser-at-p parser :eof)
    (incf (parser-index parser)))
  (previous-token parser))

(defun match-token (parser type)
  "Consume TYPE and return true when it is current."
  (when (parser-at-p parser type)
    (advance-parser parser)
    t))

(defun expect-token (parser type message)
  "Consume TYPE or signal MESSAGE."
  (if (parser-at-p parser type)
      (advance-parser parser)
      (parser-error parser message)))

(defun skip-spacers (parser)
  "Consume function-body spacer tokens."
  (loop while (parser-at-p parser :spacer)
        do (advance-parser parser)))

;;;; -- Expressions --

(defparameter *binary-operators*
  '((:logical-or    . (:logical-or 1))
    (:logical-and   . (:logical-and 2))
    (:pipe          . (:bit-or 3))
    (:caret         . (:bit-xor 4))
    (:ampersand     . (:bit-and 5))
    (:equal         . (:equal 6))
    (:not-equal     . (:not-equal 6))
    (:less          . (:less 7))
    (:greater       . (:greater 7))
    (:less-equal    . (:less-equal 7))
    (:greater-equal . (:greater-equal 7))
    (:left-shift    . (:left-shift 8))
    (:right-shift   . (:right-shift 8))
    (:plus          . (:add 9))
    (:minus         . (:subtract 9))
    (:star          . (:multiply 10))
    (:slash         . (:divide 10))
    (:percent       . (:remainder 10)))
  "Binary expression operator mapping to AST operator and binding power.")

(defun binary-operator-info (type)
  "Return binary operator info for token TYPE."
  (cdr (assoc type *binary-operators*)))

(defun parse-comma-list (parser end-token parser-function)
  "Parse a comma-separated list ending with END-TOKEN using PARSER-FUNCTION."
  (let ((items '()))
    (unless (parser-at-p parser end-token)
      (loop
        (push (funcall parser-function parser) items)
        (unless (match-token parser :comma)
          (return))))
    (expect-token parser end-token "Expected closing delimiter")
    (nreverse items)))

(defun parse-primary-expression (parser)
  "Parse a primary expression."
  (cond
    ((match-token parser :integer)
     (make-integer-expression :value (token-value (previous-token parser))))
    ((match-token parser :name)
     (make-name-expression :name (token-value (previous-token parser))))
    ((match-token parser :left-bracket)
     (make-array-expression
      :elements (parse-comma-list parser
                                  :right-bracket
                                  #'parse-expression)))
    ((match-token parser :left-parenthesis)
     (let ((expression (parse-expression parser)))
       (expect-token parser :right-parenthesis "Expected ) after expression")
       expression))
    (t
     (parser-error parser "Expected expression"))))

(defun parse-call-arguments (parser)
  "Parse a parenthesized argument list."
  (parse-comma-list parser :right-parenthesis #'parse-expression))

(defun parse-postfix-expression (parser)
  "Parse a primary expression followed by postfix operators."
  (let ((expression (parse-primary-expression parser)))
    (loop
      (cond
        ((match-token parser :left-bracket)
         (let ((index (parse-expression parser)))
           (expect-token parser :right-bracket "Expected ] after index")
           (setf expression
                 (make-index-expression :receiver expression
                                        :index index))))
        ((match-token parser :left-parenthesis)
         (setf expression
               (make-call-expression :callee expression
                                     :arguments (parse-call-arguments parser))))
        ((match-token parser :period)
         (let ((name (token-value
                      (expect-token parser :name "Expected name after ."))))
           (if (match-token parser :left-parenthesis)
               (setf expression
                     (make-method-call-expression
                      :receiver expression
                      :name name
                      :arguments (parse-call-arguments parser)))
               (setf expression
                     (make-method-reference-expression :receiver expression
                                                       :name name)))))
        (t
         (return expression))))))

(defun parse-prefix-expression (parser)
  "Parse a prefix or postfix expression."
  (cond
    ((match-token parser :not)
     (make-unary-expression :operator :not
                            :operand (parse-expression parser 11)))
    ((match-token parser :minus)
     (make-unary-expression :operator :negate
                            :operand (parse-expression parser 11)))
    (t
     (parse-postfix-expression parser))))

(defun parse-expression (parser &optional (minimum-binding-power 0))
  "Parse an expression from PARSER."
  (let ((left (parse-prefix-expression parser)))
    (loop
      (let* ((info (binary-operator-info (current-token-type parser)))
             (operator (first info))
             (binding-power (second info)))
        (unless (and info
                     (>= binding-power minimum-binding-power))
          (return left))
        (advance-parser parser)
        (let ((right (parse-expression parser (1+ binding-power))))
          (setf left
                (make-binary-expression :operator operator
                                        :left left
                                        :right right)))))))

(defun parse-expression-source (source)
  "Parse SOURCE as one World Peace expression."
  (let ((parser (make-parser :tokens (lex-source source))))
    (prog1 (parse-expression parser)
      (expect-token parser :eof "Expected end of expression"))))

;;;; -- Patterns --

(defun parse-pattern-integer (parser)
  "Parse a possibly negative integer pattern component."
  (let ((negative-p (match-token parser :minus)))
    (let ((token (expect-token parser :integer "Expected integer pattern")))
      (if negative-p
          (normalize-integer (- (token-value token)))
          (token-value token)))))

(defun parse-pattern (parser &key allow-rest)
  "Parse a World Peace pattern."
  (cond
    ((match-token parser :underscore)
     (make-wildcard-pattern))
    ((and allow-rest
          (match-token parser :double-dot))
     (make-rest-pattern))
    ((match-token parser :left-bracket)
     (make-array-pattern :elements (parse-pattern-list parser)))
    ((or (parser-at-p parser :integer)
         (parser-at-p parser :minus))
     (let ((start (parse-pattern-integer parser)))
       (if (match-token parser :double-dot)
           (make-range-pattern :start start
                               :end (parse-pattern-integer parser))
           (make-literal-pattern :value start))))
    ((match-token parser :name)
     (make-name-pattern :name (token-value (previous-token parser))))
    (t
     (parser-error parser "Expected pattern"))))

(defun parse-pattern-list (parser)
  "Parse array pattern elements."
  (let ((patterns '()))
    (unless (parser-at-p parser :right-bracket)
      (loop
        (push (parse-pattern parser :allow-rest t) patterns)
        (unless (match-token parser :comma)
          (return))))
    (expect-token parser :right-bracket "Expected ] after pattern")
    (nreverse patterns)))

(defun parse-pattern-source (source)
  "Parse SOURCE as one World Peace pattern."
  (let ((parser (make-parser :tokens (lex-source source))))
    (prog1 (parse-pattern parser)
      (expect-token parser :eof "Expected end of pattern"))))

;;;; -- Statements and Files --

(defparameter *assignment-operators*
  '((:assign                 . :assign)
    (:plus-assign            . :add)
    (:minus-assign           . :subtract)
    (:multiply-assign        . :multiply)
    (:divide-assign          . :divide)
    (:remainder-assign       . :remainder)
    (:and-assign             . :bit-and)
    (:or-assign              . :bit-or)
    (:xor-assign             . :bit-xor)
    (:left-shift-assign      . :left-shift)
    (:right-shift-assign     . :right-shift))
  "Assignment token mapping to AST operators.")

(defun assignment-operator (token-type)
  "Return the assignment operator for TOKEN-TYPE."
  (cdr (assoc token-type *assignment-operators*)))

(defun assignment-operator-token-p (token-type)
  "Return true when TOKEN-TYPE is an assignment operator."
  (and (assignment-operator token-type) t))

(defun parse-variable-declaration (parser)
  "Parse a variable declaration."
  (expect-token parser :num "Expected num")
  (let ((name (token-value (expect-token parser :name "Expected variable name"))))
    (expect-token parser :assign "Expected = in variable declaration")
    (make-variable-declaration :name name
                               :expression (parse-expression parser))))

(defun parse-assignment-statement (parser &key semicolon)
  "Parse an assignment statement."
  (let* ((name     (token-value (expect-token parser :name "Expected assignment target")))
         (operator (assignment-operator
                    (token-type (advance-parser parser))))
         (value    (parse-expression parser)))
    (when semicolon
      (expect-token parser :semicolon "Expected ; after assignment"))
    (make-assignment-statement :name name
                               :operator operator
                               :expression value)))

(defun parse-return-statement (parser &key semicolon)
  "Parse an early return statement."
  (expect-token parser :ret "Expected ret")
  (let ((expression (unless (or (parser-at-p parser :semicolon)
                                (parser-at-p parser :comma))
                      (parse-expression parser))))
    (when semicolon
      (expect-token parser :semicolon "Expected ; after return"))
    (make-return-statement :expression expression)))

(defun parse-statement-block (parser)
  "Parse statements inside braces."
  (expect-token parser :left-brace "Expected {")
  (let ((statements '()))
    (loop
      (skip-spacers parser)
      (when (parser-at-p parser :right-brace)
        (advance-parser parser)
        (return (nreverse statements)))
      (push (parse-statement parser) statements))))

(defun parse-if-statement (parser)
  "Parse an if statement."
  (expect-token parser :if "Expected if")
  (let ((condition (parse-expression parser))
        (then-body nil)
        (else-body nil))
    (setf then-body (parse-statement-block parser))
    (when (match-token parser :else)
      (setf else-body
            (if (parser-at-p parser :if)
                (list (parse-if-statement parser))
                (parse-statement-block parser))))
    (make-if-statement :condition condition
                       :then-body then-body
                       :else-body else-body)))

(defun parse-loop-step (parser)
  "Parse a do-loop step expression."
  (cond
    ((parser-at-p parser :integer)
     (make-integer-expression :value (token-value (advance-parser parser))))
    ((and (parser-at-p parser :minus)
          (eq (peek-token-type parser) :integer))
     (advance-parser parser)
     (make-integer-expression
      :value (normalize-integer (- (token-value
                                     (expect-token parser :integer
                                                   "Expected step integer"))))))
    ((parser-at-p parser :name)
     (make-name-expression :name (token-value (advance-parser parser))))
    ((and (parser-at-p parser :minus)
          (eq (peek-token-type parser) :name))
     (advance-parser parser)
     (make-unary-expression
      :operator :negate
      :operand (make-name-expression
                :name (token-value
                       (expect-token parser :name "Expected step name")))))
    (t
     (parser-error parser "Expected loop step"))))

(defun parse-do-statement (parser)
  "Parse a do loop statement."
  (expect-token parser :do "Expected do")
  (cond
    ((parser-at-p parser :left-brace)
     (make-do-statement :kind :forever
                        :body (parse-statement-block parser)))
    ((and (parser-at-p parser :name)
          (assignment-operator-token-p (peek-token-type parser)))
     (let ((assignment (parse-assignment-statement parser :semicolon nil))
           (step       nil))
       (when (match-token parser :by)
         (setf step (parse-loop-step parser)))
       (expect-token parser :until "Expected --> in do loop")
       (make-do-statement :kind :step
                          :assignment assignment
                          :step step
                          :goal (parse-expression parser)
                          :body (parse-statement-block parser))))
    (t
     (make-do-statement :kind :while
                        :condition (parse-expression parser)
                        :body (parse-statement-block parser)))))

(defun parse-case-body (parser)
  "Parse a comma-terminated case arm body."
  (if (and (parser-at-p parser :left-brace)
           (eq (peek-token-type parser) :right-brace))
      (progn
        (advance-parser parser)
        (advance-parser parser)
        '())
      (let ((statements '()))
        (loop
          (skip-spacers parser)
          (when (parser-at-p parser :comma)
            (return (nreverse statements)))
          (let ((statement
                  (cond
                    ((parser-at-p parser :ret)
                     (parse-return-statement parser :semicolon nil))
                    ((parser-at-p parser :num)
                     (prog1 (make-variable-statement
                             :declaration (parse-variable-declaration parser))
                       (expect-token parser :semicolon
                                     "Expected ; after variable declaration")))
                    ((parser-at-p parser :if)
                     (parse-if-statement parser))
                    ((parser-at-p parser :do)
                     (parse-do-statement parser))
                    ((parser-at-p parser :case)
                     (parse-case-statement parser))
                    ((and (parser-at-p parser :name)
                          (assignment-operator-token-p (peek-token-type parser)))
                     (parse-assignment-statement parser :semicolon nil))
                    (t
                     (make-expression-statement
                      :expression (parse-expression parser))))))
            (push statement statements)
            (cond
              ((match-token parser :semicolon))
              ((parser-at-p parser :comma)
               (return (nreverse statements)))
              (t
               (parser-error parser "Expected ; or , in case arm"))))))))

(defun parse-case-arm (parser)
  "Parse a case arm."
  (let ((test-expression nil)
        (pattern         nil))
    (if (match-token parser :underscore)
        (progn
          (expect-token parser :colon "Expected : after _")
          (setf pattern (make-wildcard-pattern)))
        (progn
          (setf test-expression (parse-expression parser))
          (expect-token parser :pattern-arrow "Expected <- in case arm")
          (setf pattern (parse-pattern parser))
          (expect-token parser :colon "Expected : after case pattern")))
    (prog1 (make-case-arm :test-expression test-expression
                          :pattern pattern
                          :body (parse-case-body parser))
      (expect-token parser :comma "Expected , after case arm"))))

(defun parse-case-statement (parser)
  "Parse a case statement."
  (expect-token parser :case "Expected case")
  (expect-token parser :left-brace "Expected { after case")
  (let ((arms '()))
    (loop
      (skip-spacers parser)
      (when (parser-at-p parser :right-brace)
        (advance-parser parser)
        (return (make-case-statement :arms (nreverse arms))))
      (push (parse-case-arm parser) arms))))

(defun parse-statement (parser)
  "Parse a World Peace statement."
  (skip-spacers parser)
  (cond
    ((parser-at-p parser :num)
     (prog1 (make-variable-statement
             :declaration (parse-variable-declaration parser))
       (expect-token parser :semicolon "Expected ; after variable declaration")))
    ((parser-at-p parser :ret)
     (parse-return-statement parser :semicolon t))
    ((parser-at-p parser :if)
     (parse-if-statement parser))
    ((parser-at-p parser :do)
     (parse-do-statement parser))
    ((parser-at-p parser :case)
     (parse-case-statement parser))
    ((and (parser-at-p parser :name)
          (assignment-operator-token-p (peek-token-type parser)))
     (parse-assignment-statement parser :semicolon t))
    (t
     (prog1 (make-expression-statement :expression (parse-expression parser))
       (expect-token parser :semicolon "Expected ; after expression")))))

(defun parse-parameter-filter (parser)
  "Parse an optional parameter filter after num."
  (when (match-token parser :left-bracket)
    (let ((filter (if (match-token parser :assign)
                      (make-parameter-pattern-filter
                       :pattern (parse-pattern parser))
                      (let ((start (parse-pattern-integer parser)))
                        (expect-token parser :double-dot
                                      "Expected .. in parameter slice")
                        (make-parameter-slice-filter
                         :start start
                         :end (parse-pattern-integer parser))))))
      (expect-token parser :right-bracket "Expected ] after parameter filter")
      filter)))

(defun parse-parameter (parser)
  "Parse a function parameter."
  (expect-token parser :num "Expected num in parameter")
  (let ((filter (parse-parameter-filter parser))
        (name   (token-value (expect-token parser :name "Expected parameter name"))))
    (make-parameter :name name
                    :filter filter)))

(defun parse-parameter-list (parser)
  "Parse a function parameter list."
  (expect-token parser :left-parenthesis "Expected ( before parameters")
  (parse-comma-list parser :right-parenthesis #'parse-parameter))

(defun parse-pattern-arm (parser)
  "Parse one end-clause pattern arm."
  (let ((test-expression nil)
        (pattern         nil))
    (if (match-token parser :underscore)
        (progn
          (expect-token parser :colon "Expected : after _")
          (setf pattern (make-wildcard-pattern)))
        (progn
          (setf test-expression (parse-expression parser))
          (expect-token parser :pattern-arrow "Expected <- in pattern arm")
          (setf pattern (parse-pattern parser))
          (expect-token parser :colon "Expected : after pattern")))
    (prog1 (make-pattern-arm :test-expression test-expression
                             :pattern pattern
                             :result (parse-expression parser))
      (expect-token parser :comma "Expected , after pattern arm"))))

(defun parse-end-clause (parser)
  "Parse a function end clause."
  (expect-token parser :end "Expected end")
  (if (not (match-token parser :left-brace))
      (make-end-clause)
      (cond
        ((match-token parser :right-brace)
         (make-end-clause))
        ((parser-at-p parser :underscore)
         (let ((arms '()))
           (loop
             (push (parse-pattern-arm parser) arms)
             (if (match-token parser :right-brace)
                 (return (make-end-clause :arms (nreverse arms)))
                 (skip-spacers parser)))))
        (t
         (let ((expression (parse-expression parser)))
           (if (match-token parser :pattern-arrow)
               (let ((arms (list
                            (prog1
                                (let ((pattern (parse-pattern parser)))
                                  (expect-token parser :colon
                                                "Expected : after pattern")
                                  (make-pattern-arm
                                   :test-expression expression
                                   :pattern pattern
                                   :result (parse-expression parser)))
                              (expect-token parser :comma
                                            "Expected , after pattern arm")))))
                 (loop
                   (skip-spacers parser)
                   (if (match-token parser :right-brace)
                       (return (make-end-clause :arms (nreverse arms)))
                       (push (parse-pattern-arm parser) arms))))
               (prog1 (make-end-clause :expression expression)
                 (expect-token parser :right-brace
                               "Expected } after end expression"))))))))

(defun parse-function (parser)
  "Parse a function declaration."
  (expect-token parser :dec "Expected dec")
  (let ((name       (token-value (expect-token parser :name "Expected function name")))
        (parameters nil)
        (body       '()))
    (setf parameters (parse-parameter-list parser))
    (expect-token parser :colon "Expected : after function parameters")
    (loop
      (skip-spacers parser)
      (when (parser-at-p parser :end)
        (return))
      (push (parse-statement parser) body))
    (make-function-declaration :name name
                               :parameters parameters
                               :body (nreverse body)
                               :end-clause (parse-end-clause parser))))

(defun parse-load-item (parser)
  "Parse a load item."
  (expect-token parser :load "Expected load")
  (let ((source (current-token parser)))
    (unless (member (token-type source) '(:name :url))
      (parser-error parser "Expected load source"))
    (advance-parser parser)
    (expect-token parser :semicolon "Expected ; after load")
    (make-load-item :source (token-value source)
                    :url-p (eq (token-type source) :url))))

(defun parse-item (parser)
  "Parse a top-level item."
  (case (current-token-type parser)
    (:load
     (parse-load-item parser))
    (:num
     (prog1 (parse-variable-declaration parser)
       (expect-token parser :semicolon "Expected ; after global variable")))
    (:dec
     (parse-function parser))
    (otherwise
     (parser-error parser "Expected top-level item"))))

(defun parse-source (source)
  "Parse SOURCE into a World Peace syntax tree."
  (let ((parser (make-parser :tokens (lex-source source)))
        (items  '()))
    (loop
      (skip-spacers parser)
      (when (parser-at-p parser :eof)
        (return (make-program :items (nreverse items))))
      (push (parse-item parser) items))))
