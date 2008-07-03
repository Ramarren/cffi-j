(in-package :cffi-j)

(define-foreign-library j-engine (:unix "/home/ramarren/j/j602/bin/libj.so"));specific for my system for now

(use-foreign-library j-engine)

(defcfun ("JInit" init) :pointer)
(defcfun ("JFree" free) :int (j :pointer))

(defcfun ("JDo" do) :int (j :pointer) (cmd :string))

(defcfun ("JGetM" %get) :int
  (j :pointer) (name :string) (type :pointer) (rank :pointer) (shape :pointer) (data :pointer))

(defun get (j name)
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

(defcfun ("JSetM" %set) :int
  (j :pointer) (name :string) (type :pointer) (rank :pointer) (shape :pointer) (data :pointer))

(defun set (j name data)
  (with-foreign-objects ((type :int)
			 (rank :int)
			 (shape-ptr :pointer)
			 (data-ptr :pointer))
    (if (not (arrayp data))
	(let ((foreign-type (etypecase data
			      (integer :long)
			      (float :double)
			      (character :uint8))))
	 (with-foreign-objects ((f-data foreign-type))
	   (setf (mem-ref f-data foreign-type) data)
	   (setf (mem-ref data-ptr :pointer) f-data)
	   (%set j name type 0 (null-pointer) f-data)))
	(let ((foreign-type (etypecase (aref data 0)
			      (integer :long)
			      (float :double)
			      (character :uint8))))
	 (with-foreign-objects ((f-data foreign-type (reduce #'* (array-dimensions data)))
				(shape :int (array-rank data)))
	   (iter (for i from 0)
		 (setf (mem-aref f-data foreign-type i) (row-major-aref data i)))
	   (setf (mem-ref data-ptr :pointer) f-data)
	   (iter (for s in (array-dimensions data))
		 (for i from 0)
		 (setf (mem-aref shape :int i) s))
	   (setf (mem-ref shape-ptr :pointer) shape)
	   (%set j name type 0 shape-ptr data-ptr)))
	)

    ))

