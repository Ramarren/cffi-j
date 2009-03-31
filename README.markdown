# CFFI-J

Common Lisp CFFI bindings for [J](http://jsoftware.com) programming language engine.

# Notes

J engine (libj.so) API does not seem to be documented anywhere. I have ported the `jdll.ijs` example to Common Lisp, which allows executing J statements and getting and setting arrays of basic types. Callbacks are not supported.

According to documentation `free` under *nix systems does not work as of J602, so it is recommended to use a single J engine per process.

Setting and getting variables involves copying arrays. It might be possible to use pinned Lisp arrays in some cases, but I don't think it is worth it.

There are assumptions about the engine being the 32 bit version in few places. Integers are limited to `(signed-byte 32)`.

# Usage

The package `cffi-j`, nicknamed `j`, is not supposed to be `USE`d directly, as some names will collide with CL namespace.

`init` and `free` will initialize and free the default J engine. `with-j-engine` macro will do this automatically for the dynamic extent of it's body, but this may lead to memory leaks if `JFree` call is not supported.

`clear` clears J engine. It is not documented what this does exactly, but it does erase all names in current locale at least.

`do` will execute a J statement. This does not allow multiline statements. Will signal on error.

`cmd` will return the result of command (by clobbering *jdat* variable). Second value is a type of value in the array and third is its rank, 0 for scalars.

`get` will return value of J variable. If the name is invalid or not bound to a noun an error will be signalled. Arrays and scalars of booleans, integers, floats, complex and extended numbers can be returned. Boxed elements are unboxed. Rationals and other complex types can't be returned. Strings are returned as strings. `get` return type and rank of variable as nonprimary values, as `cmd` above.

`set` sets a J variable. Acceptable Lisp values are as for `get`, except that extended integers or boxed objects cannot be set. Lists will be coerced to arrays, and complex numbers will be coerced to floating point representation.

# Conditions

Errors will be signalled on most errors. All inherit from `j-condition` condition. All except `do-error` involve setting and getting variables.

`do-error` will be signalled when J engine returns an error from command executions. Error codes do not seem to be documented.

`get-name-error` will be signalled when trying to retrieve a variable with either an invalid name, or not naming a noun.

`get-type-error` will be signalled when trying to retrieve a noun of type which cannot be read.

`set-invalid-name` will be signalled when trying to set a variable with invalid name.

`set-non8bit-character` will be signalled when trying to set a string with extended characters. Unicode is not handled.

`set-heterogeneous-array` will be signalled when trying to set an array of more than one type.

`set-invalid-type` will be signalled when trying to set a variable to type which cannot be encoded.
