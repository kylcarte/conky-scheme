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

Only x is implicitly quoted. All other expressions must be valid scheme.

This allows you to conveniently write conky vars, but stays out of the way enough to wrap nice scheme around things, with all the functions and macros your little heart desires.

Of course, the consequence of this is that you cannot abstract over conky vars:

    (define foo
      (lambda (v)
        (var v 1 2 3)))

If you really need to do that, you're probably clever enough to figure out how.

Left as exercise to the reader.

Enjoy.

