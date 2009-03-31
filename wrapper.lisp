(in-package :cffi-j)

;;; Lisp wrapper for J engine.

(defun init ()
  "Initialize J engine."
  (setf *j* (init-j)))

(defun free ()
  "Free J engine. Might not actually work."
  (free-j *j*)
  (setf *j* nil))

(defun just-do (cmd)
  (do-j *j* cmd))

(defun do (cmd)
  "Execute J statement."
  (let ((code (just-do cmd)))
    (if (zerop code)
        :done
        (error 'do-error :cmd cmd :code code))))

(defun cmd (cmd)
  "Execute J statement and return the result. Will clobber `jdat` variable."
  (do (format nil "jdat =: ~a" cmd))
  (get-unsafe "jdat"))

(defun get-boxed (data array)
  (declare (ignore data array))
  :cant-read-boxed)

(defun type-from-number (type-number)
  (ecase type-number
    (1      :boolean)
    (2      :literal)
    (4      :integer)
    (8      :double)
    (16     :complex)
    (32     :boxed)
    (64     :extended-integer)
    (128    :rational)
    (1024   :sparse-boolean)
    (2048   :sparse-literal)
    (4096   :sparse-integer)
    (8192   :sparse-floating-point)
    (16384  :sparse-complex)
    (32768  :sparse-boxed)
    (65536  :symbol)
    (131072 :unicode)))

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
    (let ((noun-type (type-from-number (mem-ref type :uint32))))
      (case noun-type
        ((:boolean :literal :integer :double :complex :boxed :extended-integer)
           (values (j-fix (mem-ref type :uint32)
                          (mem-ref rank :uint32)
                          (mem-ref shape :pointer)
                          (mem-ref data :pointer))
                   (type-from-number (mem-ref type :uint32))
                   (mem-ref rank :uint32)))
        (otherwise (error 'get-type-error :name name :type noun-type))))))

(declaim (inline get-unsafe))
(defun get-unsafe (name)
  (get-j *j* (string name)))

(defun get (name)
  "Get J variable `name`."
  (if (eql (name-class name) :noun)
      (get-unsafe name)
      (error 'get-name-error :name name)))

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
  (typecase datum
    (boolean (values 1 :uint8))
    (character (values 2 :uint8))
    (integer (values 4 :uint32))
    (float (values 8 :double))
    (complex (values 16 :double))
    (otherwise nil)))

(defun set-scalar (j name datum)
  (with-foreign-objects ((type :uint32)
                         (rank :uint32)
                         (data-ptr :pointer)
                         (shape :uint32))
    (multiple-value-bind (type-number foreign-type) (get-type-info datum)
      (setf (mem-ref type :uint32) type-number
            (mem-ref rank :uint32) 0
            (mem-ref shape :uint32) 0)
      (with-foreign-object (f-data foreign-type (if (typep datum 'complex) 2 1))
        (set-datum datum f-data 0)
        (setf (mem-ref data-ptr :pointer) f-data)
        (%set j name type rank shape data-ptr)))))

(defun array-typecheck (name data)
  (unless (get-type-info (row-major-aref data 0))
    (error 'set-invalid-type :name name :type (type-of (row-major-aref data 0))))
  ;;check type consistency
  (iter (for i from 0 below (reduce #'* (array-dimensions data)))
        (for a next (get-type-info (row-major-aref data i)))
        (for aa previous a)
        (when (and (= a 2)
                   (not (<= 0 (char-code (row-major-aref data i)) 255)))
          (error 'set-non8bit-character :name name :char (row-major-aref data i)))
        (unless (or (first-iteration-p)
                    (= a aa))
          (error 'set-heterogeneous-array :name name :array data))))

(defun set-array (j name data)
  (array-typecheck name data)
  (with-foreign-objects ((type :uint32)
                         (rank :uint32)
                         (shape-ptr :pointer)
                         (data-ptr :pointer))
    (multiple-value-bind (type-number foreign-type) (get-type-info (row-major-aref data 0))
      (setf (mem-ref type :uint32) type-number)
      (setf (mem-ref rank :uint32) (array-rank data))
      (let ((arr-len (reduce #'* (array-dimensions data))))
       (with-foreign-objects ((f-data foreign-type (if (= type-number 16)
                                                       (* 2 arr-len)
                                                       arr-len))
                              (shape :uint32 (array-rank data)))
         (iter (for i from 0 below (reduce #'* (array-dimensions data)))
               (set-datum (row-major-aref data i) f-data i))
         (setf (mem-ref data-ptr :pointer) f-data)
         (iter (for s in (array-dimensions data))
               (for i from 0)
               (setf (mem-aref shape :uint32 i) s))
         (setf (mem-ref shape-ptr :pointer) shape)
         (%set j name type rank shape-ptr data-ptr))))))

(defun set-j (j name data)
  (typecase data
    (character
       (if (<= 0 (char-code data) 255)
           (set-scalar j name data)
           (error 'set-non8bit-character :name name :char data)))
    ((or complex float integer boolean) (set-scalar j name data))
    (array (if (zerop (array-rank data))
               (set-j j name (aref data))
               (set-array j name data)))
    (list (set-j j name (make-array (length data) :initial-contents data)))
    (otherwise (error 'set-invalid-type :name name :type (type-of data)))))

(defun set (name data)
  "Set J variable `name` to `data`. Can be either and array or a scalar, of type integer, float, simple-char, complex. List will be coerced to arrays."
  (if (eql (name-class name) :invalid)
      (error 'set-invalid-name :name name)
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
