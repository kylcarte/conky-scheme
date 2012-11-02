conky-scheme
============

A simple DSL in scheme for writing and maintaining conky config files!

Two examples of conky configs in write-conky.ss.

Invoke 'make' to write config files.

    (if_ (t args ...) c a)
    ==>
    ${if_<t> args ...}c${else}a${endif}

    (var v args ...)
    ==>
    ${v args ...}

Special syntax:

There are three situations in which a macro implicitly makes an identifier into a symbol, for convenience.

    (var x v0 v* ...)
    (if_ (x v0 v* ...) e1 e2) ;; and (case_ ...)
    (color x e)

In each case, x is implicitly quoted.

