(defpackage #:cffi-j
  (:nicknames #:j)
  (:export #:init #:free #:do #:get #:set #:do-j #:set-j #:get-j #:*j* #:cmd)
  (:shadow "DO" "GET" "SET")
  (:use #:cl #:iterate #:alexandria #:cffi))
