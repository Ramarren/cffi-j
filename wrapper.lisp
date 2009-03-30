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

(defun type-from-number (type-number)
  (ecase type-number
    (1 :boolean)
    (2 :literal)
    (4 :integer)
    (8 :double)
    (16 :complex)
    (32 :boxed)
    (64 :extended-integer)
    (128 :rational)))

(defun name-character (c)
  (or (char<= #\a c #\z)
      (char<= #\A c #\Z)
      (char<= #\0 c #\9)
      (char= c #\_)))

(defun name-class (name)
  (if (or (not (stringp name))
          (notevery #'name-character name))
      :invalid
      (let ((code (cmd (format nil "4!:0 <'~a'" name))))
        (ecase code
          ((:do-error -2) :invalid)
          (-1 :unused)
          (0 :noun)
          (1 :adverb)
          (2 :conjunction)
          (3 :verb)))))

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
            (type-from-number (mem-ref type :uint32))
            (mem-ref rank :uint32))))

(defun get (name)
  "Get J variable `name`."
  (if (eql (name-class name) :noun)
      (get-j *j* (string name))))

(defun set-datum (datum data-ptr offset)
  (etypecase datum
    (boolean (setf (mem-aref data-ptr :uint8 offset) (if datum 1 0)))
    (character (let ((code (char-code datum)))
                 (assert (<= code 255))
                 (setf (mem-aref data-ptr :uint8 offset) code)))
    (integer (assert (<= (integer-length datum) 32))
             (setf (mem-aref data-ptr :uint32 offset) datum))
    (float (setf (mem-aref data-ptr :double offset) (coerce datum 'double-float)))
    (complex (setf (mem-aref data-ptr :double (* 2 offset)) (coerce (realpart datum) 'double-float)
                   (mem-aref data-ptr :double (1+ (* 2 offset))) (coerce (imagpart datum) 'double-float)))))

(defun get-type-info (datum)
  (etypecase datum
    (boolean (values 1 :uint8))
    (character (values 2 :uint8))
    (integer (values 4 :uint32))
    (float (values 8 :double))
    (complex (values 16 :double))))

(defun set-scalar (j name datum)
  (with-foreign-objects ((type :uint32)
                         (data-ptr :pointer))
    (multiple-value-bind (type-number foreign-type) (get-type-info datum)
     (setf (mem-ref type :uint32) type-number)
     (with-foreign-objects ((f-data foreign-type (if (typep datum 'complex) 2 1)))
       (set-datum datum f-data 0)
       (setf (mem-ref data-ptr :pointer) f-data)
       (%set j name type 0 (null-pointer) f-data)))))

(defun set-array (j name data)
  (with-foreign-objects ((type :uint32)
                         (rank :uint32)
                         (shape-ptr :pointer)
                         (data-ptr :pointer))
    (assert (iter (for i from 0 below (reduce #'* (array-dimensions data)))
                  (for a next (get-type-info (row-major-aref data i)))
                  (for aa previous a)
                  (unless (first-iteration-p)
                    (always (= a aa)))))
    (multiple-value-bind (type-number foreign-type) (get-type-info (row-major-aref data 0))
     (setf (mem-ref type :uint32) type-number)
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
       (%set j name type rank shape-ptr data-ptr)))))

(defun set-j (j name data)
  (etypecase data
      ((or complex float integer character boolean) (set-scalar j name data))
      (array (set-array j name data))
      (list (set-j j name (make-array (length data) :initial-contents data)))))

(defun set (name data)
  "Set J variable `name` to `data`. Can be either and array or a scalar, of type integer, float, simple-char, complex. List will be coerced to arrays."
  (unless (eql (name-class name :invalid))
    (set-j *j* (string name) data)))

(defun clear ()
  "Clear J engine. This erases all names in current locale."
  (clear-j *j*))

(defmacro with-j-engine (&body body)
  "Execute body with new j-engine. Do not use this if free does not work."
  `(let ((*j* (init-j)))
     (unwind-protect
          (progn
            ,@body)
       (free-j *j*))))
