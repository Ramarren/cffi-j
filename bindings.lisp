(in-package :cffi-j)

(defparameter *j* nil)

(define-foreign-library j-engine (:unix "libj.so"))

(use-foreign-library j-engine)

(defcfun ("JInit" init-j) :pointer)
(defcfun ("JFree" free-j) :int (j :pointer))
(defcfun ("JClear" clear-j) :pointer (j :pointer))

(defcfun ("JDo" do-j) :int (j :pointer) (cmd :string))

(defcfun ("JGetM" %get) :int
  (j :pointer) (name :string) (type :pointer) (rank :pointer) (shape :pointer) (data :pointer))

(defcfun ("JSetM" %set) :int
  (j :pointer) (name :string) (type :pointer) (rank :pointer) (shape :pointer) (data :pointer))
