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

(defmacro signals (condition-type form)
  "Assert that FORM signals CONDITION-TYPE."
  `(progn
     (incf *test-count*)
     (handler-case
         (progn
           ,form
           (error "Expected condition ~S" ',condition-type))
       (,condition-type () t))))

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
  (is-value-equal 0 (boolean->value (value-less-p (make-array-value 2) 3)))
  (is-value-equal 2 (value-index (make-array-value 1 2 3) 1))
  (is (array-value-p (value-index (make-array-value 1) 7)))
  (is-value-equal (make-array-value 1 2 3)
                  (value-push (make-array-value 1 2) 3))
  (is-value-equal (make-array-value 1 2)
                  (value-pop (make-array-value 1 2 3)))
  (is-equal 0 (value->exit-code (empty-array-value)))
  (is-equal 255 (value->exit-code -1)))

(defun token-types (tokens)
  "Return token types from TOKENS."
  (loop for token across tokens
        collect (token-type token)))

(defun token-values (tokens)
  "Return non-null token values from TOKENS."
  (loop for token across tokens
        when (token-value token)
          collect (token-value token)))

(defun test-lexer ()
  "Test World Peace lexical analysis."
  (is-equal '(:dec :name :left-parenthesis :num :name :right-parenthesis
              :colon :spacer :ret :integer :semicolon :end :left-brace
              :integer :right-brace :eof)
            (token-types (lex-source "dec f(num x): --- ret 0; end { 1 }")))
  (is-equal '("f" "x" 0 1)
            (token-values (lex-source "dec f(num x): --- ret 0; end { 1 }")))
  (is-equal (list #xac883 #b1110010010 #o123)
            (token-values (lex-source "0xAC883 0b1110010010 0o123")))
  (is-equal '(:load :url :semicolon :load :name :semicolon :eof)
            (token-types (lex-source "load lho.sh/p/cool_lib.wp; load hello;")))
  (is-equal '("lho.sh/p/cool_lib.wp" "hello")
            (token-values (lex-source "load lho.sh/p/cool_lib.wp; load hello;")))
  (is-equal '(:integer :plus :integer :eof)
            (token-types (lex-source "1 /* nested /* comment */ ok */ + 2")))
  (is-equal '(:spacer :until :minus :integer :pattern-arrow :eof)
            (token-types (lex-source "--- --> -1 <-")))
  (is-equal '(:pipe :logical-or :or-assign :eof)
            (token-types (lex-source "| || |=")))
  (signals world-peace-lex-error
    (lex-source "0o128")))

(defun expression-sexp (expression)
  "Return a compact list representation of EXPRESSION for tests."
  (typecase expression
    (integer-expression
     (integer-expression-value expression))
    (name-expression
     (intern (string-upcase (name-expression-name expression)) :keyword))
    (array-expression
     (cons :array
           (mapcar #'expression-sexp
                   (array-expression-elements expression))))
    (unary-expression
     (list (unary-expression-operator expression)
           (expression-sexp (unary-expression-operand expression))))
    (binary-expression
     (list (binary-expression-operator expression)
           (expression-sexp (binary-expression-left expression))
           (expression-sexp (binary-expression-right expression))))
    (call-expression
     (list* :call
            (expression-sexp (call-expression-callee expression))
            (mapcar #'expression-sexp
                    (call-expression-arguments expression))))
    (method-call-expression
     (list* :method-call
            (expression-sexp (method-call-expression-receiver expression))
            (intern (string-upcase (method-call-expression-name expression)) :keyword)
            (mapcar #'expression-sexp
                    (method-call-expression-arguments expression))))
    (method-reference-expression
     (list :method-reference
           (expression-sexp (method-reference-expression-receiver expression))
           (intern (string-upcase (method-reference-expression-name expression)) :keyword)))
    (index-expression
     (list :index
           (expression-sexp (index-expression-receiver expression))
           (expression-sexp (index-expression-index expression))))))

(defun test-expression-parser ()
  "Test World Peace expression parsing."
  (is-equal '(:add 1 (:multiply 2 3))
            (expression-sexp (parse-expression-source "1 + 2 * 3")))
  (is-equal '(:multiply (:add 1 2) 3)
            (expression-sexp (parse-expression-source "(1 + 2) * 3")))
  (is-equal '(:logical-or
              (:logical-and
               (:equal (:add :a (:multiply :b :c)) :d)
               (:not :x))
              (:negate 2))
            (expression-sexp (parse-expression-source "a + b * c == d && !x || -2")))
  (is-equal '(:index (:call :f :x (:array 1 2)) 0)
            (expression-sexp (parse-expression-source "f(x, [1, 2])[0]")))
  (is-equal '(:method-call :sequence :push :next)
            (expression-sexp (parse-expression-source "sequence.push(next)")))
  (is-equal '(:bit-or (:bit-and 1 2) (:bit-xor 3 4))
            (expression-sexp (parse-expression-source "1 & 2 | 3 ^ 4"))))

(defun pattern-sexp (pattern)
  "Return a compact list representation of PATTERN for tests."
  (typecase pattern
    (wildcard-pattern :_)
    (literal-pattern (literal-pattern-value pattern))
    (name-pattern (intern (string-upcase (name-pattern-name pattern)) :keyword))
    (range-pattern (list :range
                         (range-pattern-start pattern)
                         (range-pattern-end pattern)))
    (rest-pattern :..)
    (array-pattern (cons :array
                         (mapcar #'pattern-sexp
                                 (array-pattern-elements pattern))))))

(defun test-patterns ()
  "Test pattern parsing and matching."
  (is-equal '(:array :.. 1 2)
            (pattern-sexp (parse-pattern-source "[.., 1, 2]")))
  (is-equal '(:array 1 2 :..)
            (pattern-sexp (parse-pattern-source "[1, 2, ..]")))
  (is-equal '(:range -2 4)
            (pattern-sexp (parse-pattern-source "-2..4")))
  (is (pattern-match-p (parse-pattern-source "_")
                       (make-array-value 1 2)))
  (is (pattern-match-p (parse-pattern-source "1")
                       1))
  (is (not (pattern-match-p (parse-pattern-source "1")
                            (make-array-value 1))))
  (is (pattern-match-p (parse-pattern-source "2..5")
                       4))
  (is (pattern-match-p (parse-pattern-source "[1, 2]")
                       (make-array-value 1 2)))
  (is (pattern-match-p (parse-pattern-source "[.., 2]")
                       (make-array-value 0 1 2)))
  (is (pattern-match-p (parse-pattern-source "[1, ..]")
                       (make-array-value 1 2 3)))
  (is (not (pattern-match-p (parse-pattern-source "[1, 2]")
                            (make-array-value 1 2 3))))
  (is (pattern-match-p (parse-pattern-source "expected")
                       9
                       '(("expected" . 9)))))

(declaim (ftype function statement-sexp case-arm-sexp))

(defun statement-sexp (statement)
  "Return a compact list representation of STATEMENT for tests."
  (typecase statement
    (variable-statement
     (let ((declaration (variable-statement-declaration statement)))
       (list :num
             (intern (string-upcase (variable-declaration-name declaration)) :keyword)
             (expression-sexp (variable-declaration-expression declaration)))))
    (assignment-statement
     (list :assign
           (intern (string-upcase (assignment-statement-name statement)) :keyword)
           (assignment-statement-operator statement)
           (expression-sexp (assignment-statement-expression statement))))
    (expression-statement
     (list :expr
           (expression-sexp (expression-statement-expression statement))))
    (return-statement
     (list :ret
           (and (return-statement-expression statement)
                (expression-sexp (return-statement-expression statement)))))
    (if-statement
     (list :if
           (expression-sexp (if-statement-condition statement))
           (mapcar #'statement-sexp (if-statement-then-body statement))
           (mapcar #'statement-sexp (if-statement-else-body statement))))
    (do-statement
     (list :do
           (do-statement-kind statement)
           (and (do-statement-assignment statement)
                (statement-sexp (do-statement-assignment statement)))
           (and (do-statement-step statement)
                (expression-sexp (do-statement-step statement)))
           (and (do-statement-goal statement)
                (expression-sexp (do-statement-goal statement)))
           (and (do-statement-condition statement)
                (expression-sexp (do-statement-condition statement)))
           (mapcar #'statement-sexp (do-statement-body statement))))
    (case-statement
     (cons :case
           (mapcar #'case-arm-sexp (case-statement-arms statement))))))

(defun case-arm-sexp (arm)
  "Return a compact list representation of ARM for tests."
  (list :arm
        (and (case-arm-test-expression arm)
             (expression-sexp (case-arm-test-expression arm)))
        (pattern-sexp (case-arm-pattern arm))
        (mapcar #'statement-sexp (case-arm-body arm))))

(defun end-clause-sexp (end-clause)
  "Return a compact list representation of END-CLAUSE for tests."
  (cond
    ((end-clause-arms end-clause)
     (cons :end-arms
           (mapcar (lambda (arm)
                     (list (and (pattern-arm-test-expression arm)
                                (expression-sexp
                                 (pattern-arm-test-expression arm)))
                           (pattern-sexp (pattern-arm-pattern arm))
                           (expression-sexp (pattern-arm-result arm))))
                   (end-clause-arms end-clause))))
    ((end-clause-expression end-clause)
     (list :end (expression-sexp (end-clause-expression end-clause))))
    (t
     :end-empty)))

(defun parameter-sexp (parameter)
  "Return a compact list representation of PARAMETER for tests."
  (list (intern (string-upcase (parameter-name parameter)) :keyword)
        (typecase (parameter-filter parameter)
          (null nil)
          (parameter-pattern-filter
           (list :pattern
                 (pattern-sexp
                  (parameter-pattern-filter-pattern
                   (parameter-filter parameter)))))
          (parameter-slice-filter
           (list :slice
                 (parameter-slice-filter-start (parameter-filter parameter))
                 (parameter-slice-filter-end (parameter-filter parameter)))))))

(defun item-sexp (item)
  "Return a compact list representation of top-level ITEM for tests."
  (typecase item
    (load-item
     (list :load (load-item-source item) (load-item-url-p item)))
    (variable-declaration
     (list :global
           (intern (string-upcase (variable-declaration-name item)) :keyword)
           (expression-sexp (variable-declaration-expression item))))
    (function-declaration
     (list :dec
           (intern (string-upcase (function-declaration-name item)) :keyword)
           (mapcar #'parameter-sexp (function-declaration-parameters item))
           (mapcar #'statement-sexp (function-declaration-body item))
           (end-clause-sexp (function-declaration-end-clause item))))))

(defun program-sexp (source)
  "Parse SOURCE and return a compact program representation."
  (mapcar #'item-sexp
          (program-items (parse-source source))))

(defun test-file-parser ()
  "Test top-level, statement, and end-clause parsing."
  (is-equal '((:load "hello" nil)
              (:global :g 1)
              (:dec :f ((:a (:pattern 0)) (:xs (:slice 1 3)) (:n nil))
               ((:num :total 0)
                (:do :step (:assign :i :assign 0) -1 :n nil
                 ((:assign :total :add :i)))
                (:if (:equal :n 0)
                 ((:ret (:array)))
                 ((:expr (:call :print :n))))
                (:case
                 (:arm :n 1 ((:assign :total :assign 1)))
                 (:arm nil :_ nil)))
               (:end-arms
                (:n 0 (:array))
                (nil :_ :total))))
            (program-sexp
             "load hello;
              num g = 1;
              dec f(num[=0] a, num[1..3] xs, num n):
              --- num total = 0;
              --- do i = 0 by -1 --> n { total += i; }
              --- if n == 0 { ret []; } else { print(n); }
              --- case { n <- 1: total = 1, _: {}, }
              end { n <- 0: [], _: total, }"))
  (is-equal '((:dec :forever nil
               ((:do :forever nil nil nil nil
                 ((:expr (:call :print 0)))))
               :end-empty))
            (program-sexp
             "dec forever():
              --- do { print(0); }
              end"))
  (is-equal '((:dec :down nil
               ((:do :step (:assign :i :assign 3) -1 0 nil
                 ((:expr (:call :print :i)))))
               :end-empty))
            (program-sexp
             "dec down():
              --- do i = 3 --> 0 by -1 { print(i); }
              end")))

(defun test-evaluator ()
  "Test World Peace evaluation."
  (is-value-equal
   9
   (evaluate-source
    "dec main():
     --- num values = [1, 2];
     --- values.push(3);
     --- num total = 0;
     --- do i = 0 --> len(values) {
     ---   total += values[i];
     --- }
     end { total + len(values) }"))
  (is-value-equal
   55
   (evaluate-source
    "dec fibonacci(num[=0] n):
     end { 0 }

     dec fibonacci(num[=1] n):
     end { 1 }

     dec fibonacci(num n):
     end { fibonacci(n - 1) + fibonacci(n - 2) }

     dec main():
     end { fibonacci(10) }"))
  (is-value-equal
   (make-array-value 1 2)
   (evaluate-source
    "dec choose(num n):
     end {
       n <- 0: [],
       n <- 1: [1],
            _: [1, 2],
     }

     dec main():
     end { choose(9) }"))
  (let ((output (make-string-output-stream)))
    (is-value-equal
     0
     (evaluate-source
      "dec main():
       --- print([72, 105, 10]);
       end { 0 }"
      :output-stream output))
    (is-equal "Hi
" (get-output-stream-string output)))
  (is-value-equal
   20
   (evaluate-source
    "dec main():
     --- num xs = [10, 20, 30];
     end { xs(1) }"))
  (is-value-equal
   321
   (evaluate-source
    "dec main():
     --- num digits = [1, 2, 3];
     --- num result = 0;
     --- do i = len(digits) - 1 --> 0 by -1 {
     ---   result = result * 10 + digits[i];
     --- }
     end { result }"))
  #+linux
  (is-value-equal
   1
   (evaluate-source
    "dec main():
     end { syscall(39) > 0 }")))

(defun write-test-file (pathname contents)
  "Write CONTENTS to PATHNAME for tests."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

(defun test-module-loading ()
  "Test compiler module loading."
  (let* ((root (merge-pathnames
                (format nil "world-peace-test-~D/" (get-universal-time))
                (uiop:temporary-directory)))
         (main (merge-pathnames "main.wp" root))
         (util (merge-pathnames "nested/util.wp" root)))
    (unwind-protect
         (progn
           (write-test-file
            main
            "load util;
             dec main():
             end { answer() }")
           (write-test-file
            util
            "dec main():
             end { 99 }

             dec answer():
             end { 42 }")
           (is-value-equal 42
                           (evaluate-program
                            (load-entry-program root "main.wp"))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))

(defun source-spec-list (arguments)
  "Return source spec parsed from ARGUMENTS as a list."
  (multiple-value-list (source-spec-from-arguments arguments)))

(defun output-option-list (arguments)
  "Return output option parsed from ARGUMENTS as a list."
  (multiple-value-list (extract-output-option arguments)))

(defun test-command-line-parser ()
  "Test command-line argument normalization."
  (is-equal '("examples/small/" "main.wp")
            (source-spec-list '("examples/small/main.wp")))
  (is-equal '("examples/small" "main.wp")
            (source-spec-list '("examples/small" "main.wp")))
  (is-equal '("/tmp/world-peace/" "main.wp")
            (source-spec-list '("/tmp/world-peace/main.wp")))
  (is-equal '("a.out" ("examples/small/main.wp"))
            (output-option-list '("examples/small/main.wp" "-o" "a.out")))
  (is-equal '("a.out" ("examples/small/main.wp"))
            (output-option-list '("-o" "a.out" "examples/small/main.wp")))
  (is-equal '(nil nil)
            (output-option-list '("examples/small/main.wp" "-o"))))

(defun run-tests ()
  "Run the World Peace test suite."
  (setf *test-count* 0)
  (test-runtime-values)
  (test-lexer)
  (test-expression-parser)
  (test-patterns)
  (test-file-parser)
  (test-evaluator)
  (test-module-loading)
  (test-command-line-parser)
  (format t "~D assertions passed.~%" *test-count*))
