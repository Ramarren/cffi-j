(in-package :cffi-j)

;;; Here go things from jdll.ijs ported more or less directly

(defun get-object (obj-ptr)
  (let ((obj-head
	 (iter (for i from 0 below 7)
	       (collect (mem-aref obj-ptr :uint32 i)))))
    (destructuring-bind (p j1 j2 type c l r) obj-head
      (declare (ignore j1 j2 c))
      (j-fix type r
	     (make-pointer (+ (pointer-adress obj-ptr) 28))
	     (make-pointer (+ (pointer-adress obj-ptr) p))))))

(defun get-extended (extptr)
  )

(defun j-fix (type rank shape-ptr data-ptr)
  ;; note: in jdll.ijs d=.y,0 is for arguments to memr, len there is count
  ;; len=. */p=. i2j memr s,0,r*4
  ;; memr defaults to type 2(char) -> uint8
  (let ((p (iter (for i from 0 below rank)
		 (collect (mem-aref shape-ptr :uint32 i)))))
    ;; */ on empty list is 1 (group identity)
    (let ((len (if p
		   (reduce #'* p)
		   1)))
     (flet ((memr (count type)
	      (iter (for i from 0 below count)
		    (collect (mem-aref data-ptr type i)))))
      (let ((datastream
	     (ecase type
	       ;; boolean
	       (1 (mapcar (curry #'= 1) (memr len :uint8)))
	       (2 (memr len :uint8))
	       (4 (memr len :uint32))
	       (8 (memr len :double))
	       (16 (iter (for (rp ip . nil) on (memr (* 2 len) :double) by #'cddr)
			 (collect (complex rp ip))))
	       (32 (mapcar #'get-object (memr len :pointer)))
	       (64 (mapcar #'get-extended (memr len :pointer))))))
	(if p
	    (let ((out-array (make-array p)))
	      (iter (for i from 0)
		    (for o in datastream)
		    (setf (row-major-aref out-array i)
			  o)
		    (finally (return out-array))))
	    (car datastream)))))))

