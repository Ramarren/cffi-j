# CFFI-J

Common Lisp CFFI bindings for [J](http://jsoftware.com) programming language engine.

# Notes

J engine (libj.so) API does not seem to be documented anywhere. I have ported the `jdll.ijs` example to Common Lisp, which allows executing J statements and getting and setting arrays of basic types. Callbacks are not supported.

Since `free` under *nix systems does not seem to work as of J602, it is recommended to use a single J engine per process.

# Usage

The package `cffi-j`, nicknamed `j`, is not supposed to be `USE`d directly, as some names will collide with CL namespace.

`init` and `free` will initialize and free the default J engine. `with-j-engine` macro will do this automatically for the dynamic extent of it's body, but this may lead to memory leaks if `JFree` call is not supported.

`clear` clears J engine. It is not documented what this does exatcly, but it does erase all names in current locale at least.

`do` will execute a J statement. This does not allow multiline statements. Zero is returned on success and a positive integer otherwise.

`cmd` will return the result of command (by clobbering *jdat* variable).

`get` will return value of J variable. Giving a nonexisting name will result in memory fault. Arrays and scalars of booleans, integers, floats, complex and extended numbers can be returned. Boxed elements are unboxed. Rationals and other complex types can't be returned. String are returned as arrays of integers. `get` return type and rank of variable as nonprimary values.

`set` sets a J variable. Acceptable Lisp values are as for `get`. Lists will be coerced to arrays.