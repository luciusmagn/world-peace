(in-package #:world-peace)

;;;; -- Compiler --

(define-condition compiler-error (error)
  ((message :initarg :message :reader compiler-error-message))
  (:documentation "Signals a World Peace compiler failure.")
  (:report (lambda (condition stream)
             (format stream "~A" (compiler-error-message condition)))))

(defun run-compiler (&key source-root entrypoint output)
  "Compile ENTRYPOINT from SOURCE-ROOT and write OUTPUT."
  (declare (ignore source-root entrypoint output))
  (error 'compiler-error :message "World Peace compiler is not implemented yet."))
