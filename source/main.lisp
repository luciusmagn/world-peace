(in-package #:world-peace)

;;;; -- Command Line --

(defun usage (&optional (stream *standard-output*))
  "Print command-line usage to STREAM."
  (format stream "usage: peace [repl | run SOURCE-ROOT ENTRYPOINT | compile SOURCE-ROOT ENTRYPOINT -o OUTPUT]~%"))

(defun main ()
  "Run the World Peace command-line entry point."
  (let ((arguments (uiop:command-line-arguments)))
    (cond
      ((or (null arguments)
           (string= (first arguments) "repl"))
       (run-repl))
      ((string= (first arguments) "compile")
       (destructuring-bind (&optional source-root entrypoint option output &rest extra)
           (rest arguments)
         (if (and source-root
                  entrypoint
                  option
                  output
                  (string= option "-o")
                  (null extra))
             (run-compiler :source-root source-root
                           :entrypoint entrypoint
                           :output output)
             (progn
               (usage *error-output*)
               (uiop:quit 64)))))
      ((string= (first arguments) "run")
       (destructuring-bind (&optional source-root entrypoint &rest extra)
           (rest arguments)
         (if (and source-root
                  entrypoint
                  (null extra))
             (run-source-program :source-root source-root
                                 :entrypoint entrypoint)
             (progn
               (usage *error-output*)
               (uiop:quit 64)))))
      (t
       (usage *error-output*)
       (uiop:quit 64)))))
