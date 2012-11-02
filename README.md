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
