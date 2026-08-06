(in-package #:world-peace)

;;;; -- Linux native backend --

#+linux
(cffi:defcfun ("__errno_location" native--errno-location) :pointer)

#+linux
(cffi:defcfun ("syscall" native--linux-syscall) :long
  (number :long)
  (argument-1 :long)
  (argument-2 :long)
  (argument-3 :long)
  (argument-4 :long)
  (argument-5 :long)
  (argument-6 :long))

(defun native-linux-syscall-supported-p ()
  "Return whether this image uses the Linux x86-64 syscall ABI."
  (and (member :linux *features*)
       (member (string-downcase (machine-type))
               '("x86-64" "x86_64")
               :test #'string=)
       (= 8 (cffi:foreign-type-size :long))))

(defun native-current-errno ()
  "Return libc errno after a Linux syscall, or ENOSYS when unavailable."
  #+linux
  (if (native-linux-syscall-supported-p)
      (cffi:mem-ref (native--errno-location) :int)
      38)
  #-linux
  38)

(defun native--array-address (value)
  "Return VALUE's temporary native byte address and its storage pointer."
  (let* ((elements (array-value-elements value))
         (length (length elements))
         (storage (and (plusp length)
                       (cffi:foreign-alloc :uint8 :count length))))
    (when storage
      (loop for index below length
            do (setf (cffi:mem-aref storage :uint8 index)
                     (value-octet (aref elements index)))))
    (values (if storage
                (cffi:pointer-address storage)
                0)
            storage)))

(defun native--syscall-argument (argument)
  "Return ARGUMENT's native syscall integer and optional temporary storage."
  (etypecase argument
    (integer-value
     (values argument nil))
    (byte-buffer-value
     (values (byte-buffer-address argument) nil))
    (array-value
     (native--array-address argument))))

(defun native-syscall (arguments)
  "Call Linux syscall with ARGUMENTS and return result and errno.

The interface requires Linux x86-64 with an eight-byte C long. Other hosts report the
language-level ENOSYS result without attempting a native call."
  #+linux
  (if (native-linux-syscall-supported-p)
      (let ((temporaries '()))
        (unwind-protect
             (let ((numbers
                     (loop for index below 7
                           collect (if (< index (length arguments))
                                       (multiple-value-bind (number storage)
                                           (native--syscall-argument
                                            (nth index arguments))
                                         (when storage
                                           (push storage temporaries))
                                         number)
                                       0))))
               (let ((result (apply #'native--linux-syscall numbers)))
                 (values (normalize-integer result)
                         (if (= result -1)
                             (native-current-errno)
                             0))))
          (dolist (storage temporaries)
            (cffi:foreign-free storage))))
      (values -1 38))
  #-linux
  (values -1 38))
