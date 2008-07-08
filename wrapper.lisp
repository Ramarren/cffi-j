(in-package :cffi-j)

;;; Lisp wrapper for J engine.

(defun init ()
  "Initialize J engine."
  (setf *j* (init-j)))

(defun free ()
  "Free J engine. Might not actually work."
  (free-j *j*)
  (setf *j* nil))

(defun do (cmd)
  "Execute J statement."
  (do-j *j* cmd))

(defun cmd (cmd)
  "Execute J statement and return the result. Will clobber `jdat` variable."
  (let ((do-code (do (format nil "jdat =: ~a" cmd))))
    (if (zerop do-code)
	(get "jdat")
	:do-error)))

(defun get-boxed (data array)
  (declare (ignore data array))
  :cant-read-boxed)

(defun get-j (j name)
  (with-foreign-objects ((type :uint32)
			 (rank :uint32)
			 (shape :pointer)
			 (data :pointer))
    (%get j name type rank shape data)
    (values (j-fix (mem-ref type :uint32)
		   (mem-ref rank :uint32)
		   (mem-ref shape :pointer)
		   (mem-ref data :pointer))
	    (mem-ref type :uint32) (mem-ref rank :uint32))))

(defun get (name)
  "Get J variable `name`."
  (get-j *j* (string name)))

(defun set-datum (datum data-ptr offset)
  (etypecase datum
    ((boolean (setf (mem-aref data-ptr :uint8 offset) (if datum 1 0)))
     (character (let ((code (char-code datum)))
		  (assert (<= code 255))
		  (setf (mem-aref data-ptr :uint8 offset) code)))
     (integer (assert (<= (integer-length datum) 32))
	      (setf (mem-aref data-ptr :uint32 offset) datum))
     (float (setf (mem-aref data-ptr :double offset) (coerce datum 'double-float)))
     (complex (setf (mem-aref data-ptr :double (* 2 offset)) (coerce (realpart datum) 'double-float)
		    (mem-aref data-ptr :double (1+ (* 2 offset)) (coerce (imagpart datum) 'double-float)))))))

(defun set-scalar (j name datum)
  (with-foreign-objects ((type :uint32)
			 (data-ptr :pointer))
    (setf (mem-ref type :uint32) (etypecase datum
				   (boolean 1)
				   (character 2)
				   (integer 4)
				   (float 8)
				   (complex 16)))
    (with-foreign-objects ((f-data foreign-type (if (typep datum 'complex) 2 1)))
      (set-datum datum f-data 0)
      (setf (mem-ref data-ptr :pointer) f-data)
      (%set j name type 0 (null-pointer) f-data))))

(defun set-array (j name data)
  (with-foreign-objects ((type :uint32)
			 (rank :uint32)
			 (shape-ptr :pointer)
			 (data-ptr :pointer))
    (setf (mem-ref type :uint32) (etypecase datum
				   (boolean 1)
				   (character 2)
				   (integer 4)
				   (float 8)
				   (complex 16)))
    (setf (mem-ref rank :uint32) (array-rank data))
    (with-foreign-objects ((f-data foreign-type (reduce #'* (array-dimensions data)))
			   (shape :uint32 (array-rank data)))
      (iter (for i from 0 below (reduce #'* (array-dimensions data)))
	    (set-datum (row-major-aref data i) f-data i))
      (setf (mem-ref data-ptr :pointer) f-data)
      (iter (for s in (array-dimensions data))
	    (for i from 0)
	    (setf (mem-aref shape :uint32 i) s))
      (setf (mem-ref shape-ptr :pointer) shape)
      (%set j name type 0 shape-ptr data-ptr))))

(defun set-j (j name data)
  (if (not (arrayp data))
      (set-scalar j name data)
      (set-array j name data)))

(defun set (name data)
  "Set J variable `name` to `data`. Can be either and array or a scalar, of type integer, float, simple-char."
  (set-j *j* (string name) data))

(defmacro with-j-engine (&body body)
  "Execute body with new j-engine. Do not use this if free does not work."
  `(let ((*j* (init-j)))
     (unwind-protect
	  (progn
	    ,@body)
       (free-j *j*))))
