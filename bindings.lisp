(in-package :cffi-j)

(defparameter *j* nil)

(define-foreign-library j-engine (:unix "/home/ramarren/j/j602/bin/libj.so"));specific for my system for now

(use-foreign-library j-engine)

(defcfun ("JInit" init-j) :pointer)
(defcfun ("JFree" free-j) :int (j :pointer))

(defcfun ("JDo" do-j) :int (j :pointer) (cmd :string))

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

(defcfun ("JGetM" %get) :int
  (j :pointer) (name :string) (type :pointer) (rank :pointer) (shape :pointer) (data :pointer))

(defun get-j (j name)
  (with-foreign-objects ((type :int)
			 (rank :int)
			 (shape :pointer)
			 (data :pointer))
    (%get j name type rank shape data)
    (let ((j-type (ecase (mem-ref type :long)
		    (1 :bool)
		    (2 :literal)
		    (4 :integer)
		    (8 :double)
		    (16 :complex)
		    (32 :boxed)
		    (64 :extended-integer)
		    (128 :rational)
		    (1024 :sparse-boolean)
		    (2048 :sparse-literal)
		    (4096 :sparse-integer)
		    (8192 :sparse-floating-point)
		    (16384 :sparse-complex)
		    (32768 :sparse-boxed)
		    (65536 :symbol)
		    (131072 :unicode)))
	  (rank (mem-ref rank :int))
	  (j-shape nil))
      (let ((result
	     (if (zerop rank) (mem-ref (mem-ref data :pointer) :int)
		 (let ((shape (iter (for i from 0 below rank)
				    (collect (mem-aref (mem-ref shape :pointer) :int i)))))
		   (setf j-shape shape)
		   (assert (< (reduce #'* shape) 500))
		   (let ((out-array (make-array shape)))
		    (iter (for i in-affi (affi:make-affi shape))
			  (setf (row-major-aref out-array i)
				(mem-aref (mem-ref data :pointer) (ecase j-type
								    (:integer :long)
								    (:double :double)
								    (:literal :uint8)) i))
			  (finally (return out-array))))))))
	(values (ecase j-type
		  ((:integer :double) result)
		  ((:literal :unicode) (babel:octets-to-string (coerce result '(vector (unsigned-byte 8))))))
		j-type
		rank
		j-shape)))))

(defun get (name)
  "Get J variable `name`."
  (get-j *j* (string name)))

(defcfun ("JSetM" %set) :int
  (j :pointer) (name :string) (type :pointer) (rank :pointer) (shape :pointer) (data :pointer))

(defun set-scalar (j name datum)
  (with-foreign-objects ((type :int)
			 (data-ptr :pointer))
    (let ((foreign-type (etypecase datum
			  (integer :long)
			  (float :double)
			  (character :uint8))))
      (setf (mem-ref type :int) (ecase foreign-type
				  (:long 4)
				  (:double 8)
				  (:uint8 2)))
      (with-foreign-objects ((f-data foreign-type))
	(setf (mem-ref f-data foreign-type) datum)
	(setf (mem-ref data-ptr :pointer) f-data)
	(%set j name type 0 (null-pointer) f-data)))))

(defun set-array (j name data)
  (with-foreign-objects ((type :int)
			 (rank :int)
			 (shape-ptr :pointer)
			 (data-ptr :pointer))
    (let ((foreign-type (etypecase (aref data 0)
			    (integer :long)
			    (float :double)
			    (character :uint8))))
	(setf (mem-ref type :int) (ecase foreign-type
				    (:long 4)
				    (:double 8)
				    (:uint8 2)))
	(setf (mem-ref rank :int) (array-rank data))
	(with-foreign-objects ((f-data foreign-type (reduce #'* (array-dimensions data)))
			       (shape :int (array-rank data)))
	  (iter (for i from 0 below (reduce #'* (array-dimensions data)))
		(setf (mem-aref f-data foreign-type i) (row-major-aref data i)))
	  (setf (mem-ref data-ptr :pointer) f-data)
	  (iter (for s in (array-dimensions data))
		(for i from 0)
		(setf (mem-aref shape :int i) s))
	  (setf (mem-ref shape-ptr :pointer) shape)
	  (%set j name type 0 shape-ptr data-ptr)))))

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
