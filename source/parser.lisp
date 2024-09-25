(in-package #:world-peace)

;;;; -- Parser --

(define-condition world-peace-parse-error (error)
  ((message :initarg :message :reader world-peace-parse-error-message))
  (:documentation "Signals invalid World Peace source syntax.")
  (:report (lambda (condition stream)
             (format stream "~A" (world-peace-parse-error-message condition)))))

(defun parse-source (source)
  "Parse SOURCE into a World Peace syntax tree."
  (declare (ignore source))
  (error 'world-peace-parse-error :message "World Peace parser is not implemented yet."))
