(defpackage #:cffi-j
  (:nicknames #:j)
  (:export #:init #:free #:do #:get #:set #:cmd #:with-j-engine)
  (:shadow "DO" "GET" "SET")
  (:use #:cl #:iterate #:alexandria #:cffi))
