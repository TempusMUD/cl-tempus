cl-tempus
=========

This is a half-completed rewrite of Tempuscode, written in Common Lisp
and distributed under the BSD license.  It is intended to be a drop-in
replacement for the old code, so it uses the same library and database
structures.  It is a functioning MUD at this point, though one without
skills, spells, scripting, and most special functions.

It is mostly portable, but was written for sbcl.

Usage
-----

Follow the installation instructions at
http://github.com/TempusMUD/Tempuscode.

Install [sbcl](http://sbcl.org/), then from sbcl, install
[quicklisp](http://quicklisp.org).

Place a symlink to `tempus.asd` into `~/quicklisp/local-projects`.

At the sbcl REPL, type `(ql:quickload "tempus")`.  It should download
any needed packages.

After compiliation, type `(tempus:main :port <network port> :dir <path
to lib dir>)`.  You should be able to connect to the network port and
login.
