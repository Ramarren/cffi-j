(defpackage #:cffi-j
  (:nicknames #:j)
  (:export #:init #:free #:clear #:do #:get #:set #:cmd #:with-j-engine
           #:j-condition
           #:do-error
           #:get-error
           #:get-name-error
           #:get-type-error
           #:set-error
           #:set-invalid-name
           #:set-non8bit-character
           #:set-heterogeneous-array
           #:set-invalid-type)
  (:shadow "DO" "GET" "SET")
  (:use #:cl #:iterate #:alexandria #:cffi))
