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

(defun parse-source (source)
  "Parse SOURCE into a World Peace syntax tree."
  (declare (ignore source))
  (error 'world-peace-parse-error
         :message "World Peace file parser is not implemented yet."
         :token (make-token :type :eof)))
