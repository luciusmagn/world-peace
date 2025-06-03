(in-package #:world-peace)

;;;; -- Lexer --

(define-condition world-peace-lex-error (error)
  ((message  :initarg :message  :reader world-peace-lex-error-message)
   (position :initarg :position :reader world-peace-lex-error-position))
  (:documentation "Signals invalid World Peace lexical syntax.")
  (:report (lambda (condition stream)
             (format stream "~A at byte ~D"
                     (world-peace-lex-error-message condition)
                     (world-peace-lex-error-position condition)))))

(defstruct token
  "A World Peace token."
  (type  :eof :type keyword)
  (value nil)
  (start 0 :type fixnum)
  (end   0 :type fixnum))

(defparameter *keywords*
  '(("dec"  . :dec)
    ("ret"  . :ret)
    ("end"  . :end)
    ("do"   . :do)
    ("by"   . :by)
    ("case" . :case)
    ("num"  . :num)
    ("load" . :load)
    ("if"   . :if)
    ("else" . :else))
  "World Peace keyword token mapping.")

(defparameter *operators*
  '((">>=" . :right-shift-assign)
    ("<<=" . :left-shift-assign)
    ("-->" . :until)
    ("---" . :spacer)
    ("<-"  . :pattern-arrow)
    ("=="  . :equal)
    ("!="  . :not-equal)
    ("<="  . :less-equal)
    (">="  . :greater-equal)
    ("<<"  . :left-shift)
    (">>"  . :right-shift)
    ("&&"  . :logical-and)
    ("||"  . :logical-or)
    ("+="  . :plus-assign)
    ("-="  . :minus-assign)
    ("*="  . :multiply-assign)
    ("/="  . :divide-assign)
    ("%="  . :remainder-assign)
    ("&="  . :and-assign)
    ("|="  . :or-assign)
    ("^="  . :xor-assign)
    (".."  . :double-dot)
    ("("   . :left-parenthesis)
    (")"   . :right-parenthesis)
    ("{"   . :left-brace)
    ("}"   . :right-brace)
    ("["   . :left-bracket)
    ("]"   . :right-bracket)
    (","   . :comma)
    (":"   . :colon)
    (";"   . :semicolon)
    ("."   . :period)
    ("="   . :assign)
    ("+"   . :plus)
    ("-"   . :minus)
    ("*"   . :star)
    ("/"   . :slash)
    ("%"   . :percent)
    ("!"   . :not)
    ("^"   . :caret)
    ("<"   . :less)
    (">"   . :greater)
    ("&"   . :ampersand)
    ("|"   . :pipe))
  "World Peace operator and punctuation token mapping.")

(defun lex-error (message position)
  "Signal a lexical error with MESSAGE at POSITION."
  (error 'world-peace-lex-error :message message :position position))

(defun identifier-start-p (character)
  "Return true when CHARACTER may start an identifier."
  (or (alpha-char-p character)
      (char= character #\_)))

(defun identifier-part-p (character)
  "Return true when CHARACTER may continue an identifier."
  (or (alphanumericp character)
      (char= character #\_)))

(defun digit-for-base-p (character base)
  "Return true when CHARACTER is a valid digit for BASE."
  (let ((value (digit-char-p character base)))
    (and value t)))

(defun url-character-p (character)
  "Return true when CHARACTER may appear in a load URL token."
  (or (alphanumericp character)
      (find character "_-./" :test #'char=)))

(defun starts-with-p (source index prefix)
  "Return true when SOURCE has PREFIX at INDEX."
  (let ((end (+ index (length prefix))))
    (and (<= end (length source))
         (string= source prefix :start1 index :end1 end))))

(defun whitespace-character-p (character)
  "Return true when CHARACTER is lexical whitespace."
  (find character '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun strip-comments (source)
  "Return SOURCE with comments replaced by whitespace."
  (let* ((result (copy-seq source))
         (length (length source))
         (index  0))
    (labels ((blank (position)
               (unless (char= (aref result position) #\Newline)
                 (setf (aref result position) #\Space))))
      (loop while (< index length) do
        (cond
          ((and (< (1+ index) length)
                (char= (aref source index) #\/)
                (char= (aref source (1+ index)) #\/))
           (blank index)
           (blank (1+ index))
           (incf index 2)
           (loop while (and (< index length)
                            (not (char= (aref source index) #\Newline)))
                 do (blank index)
                    (incf index)))
          ((and (< (1+ index) length)
                (char= (aref source index) #\/)
                (char= (aref source (1+ index)) #\*))
           (let ((depth 1))
             (blank index)
             (blank (1+ index))
             (incf index 2)
             (loop while (and (< index length)
                              (plusp depth))
                   do (cond
                        ((and (< (1+ index) length)
                              (char= (aref source index) #\/)
                              (char= (aref source (1+ index)) #\*))
                         (blank index)
                         (blank (1+ index))
                         (incf depth)
                         (incf index 2))
                        ((and (< (1+ index) length)
                              (char= (aref source index) #\*)
                              (char= (aref source (1+ index)) #\/))
                         (blank index)
                         (blank (1+ index))
                         (decf depth)
                         (incf index 2))
                        (t
                         (blank index)
                         (incf index))))
             (when (plusp depth)
               (lex-error "Unterminated block comment" index))))
          (t
           (incf index)))))
    result))

(defun parse-integer-token (source start end base)
  "Parse SOURCE from START to END as an integer in BASE."
  (let ((digits (remove #\_ (subseq source start end))))
    (parse-integer digits :radix base)))

(defun lex-number (source index)
  "Return the integer token and next index for SOURCE at INDEX."
  (let* ((length      (length source))
         (base        10)
         (digits-start index)
         (scan-start  index))
    (when (and (< (+ index 2) length)
               (char= (aref source index) #\0)
               (find (aref source (1+ index)) "xXbBoO" :test #'char=))
      (setf base (ecase (char-downcase (aref source (1+ index)))
                   (#\x 16)
                   (#\b 2)
                   (#\o 8))
            digits-start (+ index 2)
            scan-start digits-start))
    (when (or (>= scan-start length)
              (char= (aref source scan-start) #\_)
              (not (digit-for-base-p (aref source scan-start) base)))
      (lex-error "Invalid number literal" index))
    (loop while (and (< scan-start length)
                     (or (char= (aref source scan-start) #\_)
                         (digit-for-base-p (aref source scan-start) base)))
          do (incf scan-start))
    (when (and (< scan-start length)
               (identifier-part-p (aref source scan-start)))
      (lex-error "Invalid number literal" index))
    (values (make-token :type :integer
                        :value (normalize-integer
                                (parse-integer-token source digits-start scan-start base))
                        :start index
                        :end scan-start)
            scan-start)))

(defun lex-identifier (source index)
  "Return the identifier or keyword token and next index for SOURCE at INDEX."
  (let ((end index))
    (loop while (and (< end (length source))
                     (identifier-part-p (aref source end)))
          do (incf end))
    (let* ((text (subseq source index end))
           (type (or (cdr (assoc text *keywords* :test #'string=))
                     (if (string= text "_") :underscore :name))))
      (values (make-token :type type
                          :value (and (eq type :name) text)
                          :start index
                          :end end)
              end))))

(defun lex-load-source (source index)
  "Return a module source token and next index for SOURCE at INDEX."
  (let ((end index))
    (loop while (and (< end (length source))
                     (url-character-p (aref source end)))
          do (incf end))
    (when (= end index)
      (lex-error "Expected load source" index))
    (let* ((text     (subseq source index end))
           (url-p    (or (find #\/ text :test #'char=)
                         (search ".wp" text)))
           (name-p   (and (identifier-start-p (aref text 0))
                          (loop for character across text
                                always (identifier-part-p character))))
           (type     (cond
                       (url-p :url)
                       (name-p :name)
                       (t (lex-error "Invalid load source" index)))))
      (values (make-token :type type
                          :value text
                          :start index
                          :end end)
              end))))

(defun lex-operator (source index)
  "Return an operator token and next index for SOURCE at INDEX."
  (loop for (spelling . type) in *operators*
        when (starts-with-p source index spelling)
          do (return-from lex-operator
               (values (make-token :type type
                                   :start index
                                   :end (+ index (length spelling)))
                       (+ index (length spelling)))))
  (lex-error "Unexpected character" index))

(defun lex-source (source)
  "Tokenize World Peace SOURCE."
  (let* ((clean-source        (strip-comments source))
         (tokens              '())
         (index               0)
         (expect-load-source  nil)
         (length              (length clean-source)))
    (labels ((push-token (token)
               (push token tokens)
               (setf expect-load-source (eq (token-type token) :load))))
      (loop while (< index length) do
        (let ((character (aref clean-source index)))
          (cond
            ((whitespace-character-p character)
             (incf index))
            (expect-load-source
             (multiple-value-bind (token next-index) (lex-load-source clean-source index)
               (push-token token)
               (setf index next-index)))
            ((digit-char-p character)
             (multiple-value-bind (token next-index) (lex-number clean-source index)
               (push-token token)
               (setf index next-index)))
            ((identifier-start-p character)
             (multiple-value-bind (token next-index) (lex-identifier clean-source index)
               (push-token token)
               (setf index next-index)))
            (t
             (multiple-value-bind (token next-index) (lex-operator clean-source index)
               (push-token token)
               (setf index next-index)))))))
    (coerce (nreverse (cons (make-token :type :eof
                                        :start length
                                        :end length)
                            tokens))
            'vector)))
