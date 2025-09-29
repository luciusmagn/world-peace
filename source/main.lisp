(in-package #:world-peace)

;;;; -- Command Line --

(defun usage (&optional (stream *standard-output*))
  "Print command-line usage to STREAM."
  (format stream "usage: peace repl~%")
  (format stream "       peace run FILE.wp~%")
  (format stream "       peace run SOURCE-ROOT ENTRYPOINT~%")
  (format stream "       peace compile FILE.wp -o OUTPUT~%")
  (format stream "       peace compile SOURCE-ROOT ENTRYPOINT -o OUTPUT~%"))

(defun wp-path-p (argument)
  "Return true when ARGUMENT looks like a World Peace source path."
  (and argument
       (uiop:string-suffix-p argument ".wp")))

(defun source-spec-from-arguments (arguments)
  "Return source root and entrypoint parsed from ARGUMENTS."
  (cond
    ((and (= (length arguments) 1)
          (wp-path-p (first arguments)))
     (let ((pathname (uiop:parse-native-namestring (first arguments))))
       (values (namestring (uiop:pathname-directory-pathname pathname))
               (file-namestring pathname))))
    ((= (length arguments) 2)
     (values (first arguments)
             (second arguments)))
    (t
     (values nil nil))))

(defun extract-output-option (arguments)
  "Return output and remaining ARGUMENTS after removing -o OUTPUT."
  (let ((output nil)
        (remaining '()))
    (loop while arguments
          for argument = (pop arguments)
          do (if (string= argument "-o")
                 (progn
                   (when output
                     (return-from extract-output-option (values nil nil)))
                   (unless arguments
                     (return-from extract-output-option (values nil nil)))
                   (setf output (pop arguments)))
                 (push argument remaining)))
    (values output (nreverse remaining))))

(defun main ()
  "Run the World Peace command-line entry point."
  (let ((arguments (uiop:command-line-arguments)))
    (cond
      ((or (null arguments)
           (string= (first arguments) "repl"))
       (run-repl))
      ((string= (first arguments) "compile")
       (multiple-value-bind (output source-arguments)
           (extract-output-option (rest arguments))
         (multiple-value-bind (source-root entrypoint)
             (source-spec-from-arguments source-arguments)
           (if (and output source-root entrypoint)
               (run-compiler :source-root source-root
                             :entrypoint entrypoint
                             :output output)
               (progn
                 (usage *error-output*)
                 (uiop:quit 64))))))
      ((string= (first arguments) "run")
       (multiple-value-bind (source-root entrypoint)
           (source-spec-from-arguments (rest arguments))
         (if (and source-root entrypoint)
             (run-source-program :source-root source-root
                                 :entrypoint entrypoint)
             (progn
               (usage *error-output*)
               (uiop:quit 64)))))
      (t
       (usage *error-output*)
       (uiop:quit 64)))))
