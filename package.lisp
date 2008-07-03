(defpackage #:cffi-j
  (:nicknames #:j)
  (:export #:init #:free #:do #:get #:set)
  (:shadow "DO" "GET" "SET")
  (:use #:cl #:iterate #:alexandria #:cffi))
