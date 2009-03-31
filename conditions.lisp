(in-package :cffi-j)

(define-condition j-condition ()
  ())

(define-condition do-error (j-condition)
  ((cmd :reader cmd-of :initarg :cmd)
   (code :reader code-of :initarg :code))
  (:report (lambda (c stream)
             (format stream
                     "Error while executing J command:~s. Error code: ~a"
                     (cmd-of c)
                     (code-of c)))))

(define-condition get-error (j-condition)
  ((name :reader name-of :initarg :name)))

(define-condition get-name-error (get-error)
  ()
  (:report (lambda (c stream)
             (format stream
                     "Variable ~s is not a J-side noun."
                     (name-of c)))))

(define-condition get-type-error (get-error)
  ((type :reader noun-type-of :initarg :type))
  (:report (lambda (c stream)
             (format stream
                     "Can't retrieve noun ~a of type ~a"
                     (name-of c)
                     (noun-type-of c)))))

(define-condition set-error (j-condition)
  ((name :reader name-of :initarg :name)))

(define-condition set-invalid-name (set-error)
  ()
  (:report (lambda (c stream)
             (format stream "Name ~s is invalid." (name-of c)))))

(define-condition set-non8bit-character (set-error)
  ((char :reader char-of :initarg :char))
  (:report (lambda (c stream)
             (format stream "When setting variable ~s : Only 8-bit characters allowed. ~s is not."
                     (name-of c) (char-of c)))))

(define-condition set-heterogeneous-array (set-error)
  ((array :reader array-of :initarg :array))
  (:report (lambda (c stream)
             (format stream "When setting variable ~s : Arrays must have all elements of the same type."
                     (name-of c)))))

(define-condition set-invalid-type (set-error)
  ((type :reader noun-type-of :initarg :type))
  (:report (lambda (c stream)
             (format stream "When setting variable ~s : Type ~a cannot be encoded."
                     (name-of c) (noun-type-of c)))))
