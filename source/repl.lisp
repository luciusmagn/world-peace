(in-package #:world-peace)

;;;; -- REPL --

(defun repl-print-value (value)
  "Print VALUE for the REPL."
  (write-value-readable value)
  (terpri))

(defun repl-evaluate-line (runtime environment line)
  "Evaluate one REPL LINE."
  (handler-case
      (let ((value (evaluate-expression runtime
                                        environment
                                        (parse-expression-source line))))
        (repl-print-value value))
    (world-peace-parse-error ()
      (let ((program (parse-source line)))
        (register-program runtime program)
        (format t "ok~%")))))

(defun run-repl ()
  "Start the World Peace REPL."
  (let ((runtime     (make-runtime))
        (environment (make-environment)))
    (format t "World Peace~%")
    (loop
      (format t "peace> ")
      (finish-output)
      (let ((line (read-line *standard-input* nil nil)))
        (cond
          ((null line)
           (return))
          ((member line '(":q" ":quit" "quit") :test #'string=)
           (return))
          ((string= line "")
           nil)
          (t
           (handler-case
               (repl-evaluate-line runtime environment line)
             (condition (condition)
               (format *error-output* "~A~%" condition)))))))))
