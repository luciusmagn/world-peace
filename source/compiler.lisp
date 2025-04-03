(in-package #:world-peace)

;;;; -- Compiler --

(define-condition compiler-error (error)
  ((message :initarg :message :reader compiler-error-message))
  (:documentation "Signals a World Peace compiler failure.")
  (:report (lambda (condition stream)
             (format stream "~A" (compiler-error-message condition)))))

(defvar *compiled-program* nil
  "Program embedded into a saved World Peace image.")

(defun compiler-error (message)
  "Signal a compiler error with MESSAGE."
  (error 'compiler-error :message message))

(defun pathname-basename (pathname)
  "Return PATHNAME's basename without type."
  (pathname-name pathname))

(defun wp-file-p (pathname)
  "Return true when PATHNAME names a World Peace source file."
  (and (pathname-type pathname)
       (string-equal (pathname-type pathname) "wp")))

(defun collect-wp-files (directory)
  "Return all .wp files under DIRECTORY recursively."
  (let ((files '()))
    (labels ((walk (path)
               (dolist (entry (uiop:directory-files path))
                 (when (wp-file-p entry)
                   (push entry files)))
               (dolist (child (uiop:subdirectories path))
                 (walk child))))
      (walk (uiop:ensure-directory-pathname directory)))
    (nreverse files)))

(defun module-index (source-root)
  "Return a module-name to path index for SOURCE-ROOT."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (file (collect-wp-files source-root))
      (push file (gethash (pathname-basename file) index)))
    index))

(defun find-module-path (index name)
  "Find module NAME in INDEX."
  (let ((matches (gethash name index)))
    (case (length matches)
      (0 (compiler-error (format nil "Cannot find module ~A" name)))
      (1 (first matches))
      (otherwise
       (compiler-error (format nil "Module ~A is ambiguous" name))))))

(defun read-file-string (pathname)
  "Read PATHNAME into a string."
  (with-open-file (stream pathname :direction :input)
    (let ((text (make-string (file-length stream))))
      (read-sequence text stream)
      text)))

(defun fetch-url-source (url)
  "Fetch a World Peace module URL."
  (unless (uiop:string-suffix-p url ".wp")
    (compiler-error "URL loads must end with .wp"))
  (uiop:run-program (list "curl" "-fsSL" (concatenate 'string "https://" url))
                    :output :string))

(defun load-source-program (source)
  "Parse SOURCE as a World Peace program."
  (parse-source source))

(defun load-path-program (pathname)
  "Parse PATHNAME as a World Peace program."
  (load-source-program (read-file-string pathname)))

(defun merge-program-items (entry-program loaded-programs)
  "Merge ENTRY-PROGRAM and LOADED-PROGRAMS into one program."
  (let ((items '()))
    (dolist (program loaded-programs)
      (dolist (item (program-items program))
        (unless (or (load-item-p item)
                    (and (function-declaration-p item)
                         (string= (function-declaration-name item) "main")))
          (push item items))))
    (dolist (item (program-items entry-program))
      (unless (load-item-p item)
        (push item items)))
    (make-program :items (nreverse items))))

(defun resolve-program-loads (program source-root index loaded)
  "Resolve PROGRAM load items from SOURCE-ROOT using INDEX and LOADED."
  (let ((programs '()))
    (dolist (item (program-items program))
      (when (load-item-p item)
        (let* ((key            (if (load-item-url-p item)
                                   (load-item-source item)
                                   (namestring
                                    (find-module-path index (load-item-source item)))))
               (already-loaded (gethash key loaded)))
          (unless already-loaded
            (setf (gethash key loaded) t)
            (let ((loaded-program
                    (if (load-item-url-p item)
                        (load-source-program
                         (fetch-url-source (load-item-source item)))
                        (load-path-program key))))
              (setf programs
                    (nconc programs
                           (resolve-program-loads loaded-program
                                                  source-root
                                                  index
                                                  loaded)
                           (list loaded-program))))))))
    programs))

(defun load-entry-program (source-root entrypoint)
  "Load ENTRYPOINT and its modules from SOURCE-ROOT."
  (let* ((root          (uiop:ensure-directory-pathname source-root))
         (entry-path    (merge-pathnames entrypoint root))
         (index         (module-index root))
         (entry-program (load-path-program entry-path))
         (loaded        (make-hash-table :test 'equal)))
    (setf (gethash (namestring entry-path) loaded) t)
    (merge-program-items
     entry-program
     (resolve-program-loads entry-program root index loaded))))

(defun command-line-arguments-as-values ()
  "Return process arguments as World Peace byte arrays."
  (mapcar #'string->byte-array
          #+sbcl sb-ext:*posix-argv*
          #-sbcl (cons "peace" (uiop:command-line-arguments))))

(defun run-program-main (program)
  "Run PROGRAM's main function and quit with its exit code."
  (let ((value (evaluate-program program
                                 :arguments (command-line-arguments-as-values))))
    (uiop:quit (value->exit-code value))))

(defun compiled-main ()
  "Run the program embedded in the current image."
  (unless *compiled-program*
    (compiler-error "No World Peace program is embedded in this image"))
  (run-program-main *compiled-program*))

(defun run-compiler (&key source-root entrypoint output)
  "Compile ENTRYPOINT from SOURCE-ROOT and write OUTPUT."
  (let ((program (load-entry-program source-root entrypoint)))
    (setf *compiled-program* program)
    #+sbcl
    (sb-ext:save-lisp-and-die output
                              :toplevel #'compiled-main
                              :executable t
                              :compression t)
    #-sbcl
    (compiler-error "Image compilation currently requires SBCL")))

(defun run-source-program (&key source-root entrypoint)
  "Load and run ENTRYPOINT from SOURCE-ROOT in the current image."
  (run-program-main (load-entry-program source-root entrypoint)))
