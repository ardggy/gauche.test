(defpackage gauche.test
  (:use :cl)
  (:export :test :test* :test-error :test-error-p :prim-test :test-check
	   :test-start :test-end :test-section
	   :test-section :test-check :*test-count*))

(in-package gauche.test)

(define-condition test-error (error)
  ((message :initarg :message :initform nil)
   (class :initarg :class :initform nil)))

(defun test-error-p (obj)
  (typep obj 'test-error))

(defun test-error (&rest maybe)
  (make-condition 'test-error
		  :class (and (consp maybe)
			      (car maybe))))

(defun test-check (expected result)
  (cond ((test-error-p expected)
	 (and (test-error-p result)
	      (let ((x (slot-value expected 'class))
		    (y (slot-value result 'class)))
		(or (not x)
		    ; TODO: find from precedence-list of result OR
		    ; represent Error by other way
		    (eq x y)))))
	(t (equal expected result))))
		  
(defvar *discrepancy-list* nil)

;; (defvar *test-error* (make-condition 'test-error)) ; depricated

(defmacro make-test-counter (&rest counters)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn
       (defvar *test-count* (make-array ,(length counters) :initial-element 0))
       ,@(loop for (name . index) in counters
	    collect `(defun ,(intern
			      (concatenate 'string
					   "TEST-" (symbol-name name) "++"))
			 ()
		       (incf (aref *test-count* ,index)))))))

(make-test-counter
 (count . 0) (pass . 1) (fail . 2) (abort . 3))

(defmethod print-object ((obj test-error) stream)
  (let ((cname (or (slot-value obj 'class) 'error)))
    (let ((it (slot-value obj 'message)))
      (if (slot-value obj 'message)
	  (format stream "~s" it)
	  (format stream "#<~a>" cname)))))

(defun prim-test (msg expect thunk &optional compare)
  (let ((cmp (if (consp compare) (car compare) #'test-check)))
    (format t "test ~a, expects ~s ==> " msg expect)
    (force-output)
    (test-count++)
    (let ((result (funcall thunk)))
      (cond ((funcall cmp expect result)
	     (format t "ok~%") (test-pass++))
	    (t (format t "ERROR: got ~s~%" result)
	       (push (list msg expect result) *discrepancy-list*)
	       (test-fail++)))
      (force-output))))

(defun test (msg expect thunk &rest compare)
  (apply #'prim-test msg expect
	 (lambda ()
	   (handler-case
	       (funcall thunk)
	     (t (e) (make-condition 'test-error :message e :class (type-of e)))))
	 compare))

(defmacro test* (msg expect form &optional compare)
  `(test ,msg ,expect (lambda () ,form) ,compare))

(defun test-section (msg)
  (let ((msglen (length msg)))
    (format t "<~a>~v@{-~}~%" msg (max 5 (- 77 msglen)) t)))

(defun format-summary ()
  (format t "Total: ~5d tests, ~5d passed, ~5d failed, ~5d aborted~%"
	  (aref *test-count* 0)
	  (aref *test-count* 1)
	  (aref *test-count* 2)
	  (aref *test-count* 3)
	  ))

(defun test-start (msg)
  (let ((s (format nil "Testing ~a ...~%" msg)))
    (format t s)
    (format t "~v@{ ~}~%" (max 3 (- 65 (length s))) t)
    (force-output))
  (setf *discrepancy-list* nil)
  (let ((msglen (length msg)))
    (format t "Testing ~a ~v@{=~}~%" msg (max 5 (- 70 msglen)) t)
    (force-output)))

(defun test-end ()
  (if (null *discrepancy-list*)
      (format t "passed.~%")
      (format t "failed.~%discrepancies found. Errors are~%"))
  (mapc (lambda (x)
	  (apply #'format t "test ~a: expects ~s => got ~s~%" x))
	(reverse *discrepancy-list*))
  (format-summary)
  (length *discrepancy-list*))

;;; i haven't decided to use following functions,
;;; Gauche's procedure (current-*-port).
;;; so, FORMAT's output is only supported to STDOUT, sorry.
(defun current-output-stream ()
  (let ((stream-symbol (slot-value *standard-output* 'symbol)))
    #+sbcl
    (sb-sys:fd-stream-fd (symbol-value stream-symbol))))

(defun current-input-stream ()
  (let ((stream-symbol (slot-value *standard-input* 'symbol)))
    #+sbcl
    (sb-sys:fd-stream-fd (symbol-value stream-symbol))))

(defun current-error-stream ()
  (let ((stream-symbol (slot-value *error-output* 'symbol)))
    #+sbcl
    (sb-sys:fd-stream-fd (symbol-value stream-symbol))))

(defun sys-isatty (stream)
  (let ((r #+sbcl(sb-unix:unix-isatty stream)))
    (cond ((= 0 r) nil)
	  ((= 1 r) t))))
