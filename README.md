How to run
==========

Just call `./arc` and you'll get a REPL.

You can also use `./arc foo` to load the Arc file "foo.arc".

This also means that the `arc` executable is suitable for writing shell
scripts:

    #! /path/to/arc
    (prn "foo")

Use `./arc -h` to see all the available options.


What is it?
===========

Arc/Nu is Arc 3.1 implemented with the Nu compiler. It also includes some
additional libraries and applications that I've found useful.

  * _"01 nu.rkt"_ is the Nu compiler for Arc
  * _"02 arc.arc"_ is copied unmodified from Arc 3.1
  * _"03 utils.arc"_ contains generic utilities
  * _"04 parameters.arc"_ implements implicit parameters
  * _"05 paths.arc"_ contains functions for inspecting and manipulating paths
  * _"06 import.arc"_ implements an `import` macro for loading files
  * _"07 repl.arc"_ implements a REPL
  * _"arc"_ is an executable that will load the above files in order

  * _"lib/"_ contains other useful libraries
  * _"app/"_ contains applications I've written using Nu

Okay, so it's basically Arc 3.1 (it even copies arc.arc from Arc 3.1!).
Why would you want to use it over Arc 3.1 or Anarki, then?

  * It's faster! Nu strives to be *at least* as fast as Arc 3.1, and in some
    cases is significantly faster. For instance, `(+ 1 2)` was 132.48% faster
    in Nu than in Arc 3.1, last time I checked. You can view the latest timing
    tests [here](timing)

  * Nu makes it possible to add in awesome things like namespaces, aliases,
    and implicit parameters as a library without hacking the compiler.

    As an example, Arc/Nu implements `defcall` as a library by extending the
    `ref` function in _"lib/01 utils.arc"_ and implements implicit parameters
    in _"lib/02 parameters.arc"_

  * The REPL is implemented **substantially** better:

      * Ctrl+D exits the REPL

      * Ctrl+C aborts the current computation but doesn't exit the REPL:

            > ((afn () (self)))
            ^Cuser break
            >

      * Readline support is built-in, which means:

          * Pressing Tab will autocomplete the names of global variables:

                > f
                filechars    file-exists  fill-table   find         firstn       flat         flushout     fn           for          force-close  forlen       fromdisk     fromstring

          * Pressing Up will recall the entire expression rather than only the
            last line:

                > (+ 1
                     2
                     3)
                6
                > (+ 1
                     2
                     3)

  * You can use the "arc" executable to write shell scripts:

        #! /path/to/arc
        (prn "foo")

    This is like "arc.sh" in Anarki but implemented in Racket rather than as a
    bash script, so it should be cleaner and more portable.

    In addition, it supports common Unix idioms such as:

        $ /path/to/arc < foo.arc
        $ echo "(+ 1 2)" | /path/to/arc

    This idea is courtesy of [this thread](http://arclanguage.org/item?id=10344)

  * Like Anarki, Nu provides a form that lets you bypass the compiler and drop
    directly into Racket. In Anarki this form is `$` and in Nu it's `%`:

        > (% (let loop ((a 3))
               (if (= a 0)
                   #f
                   (begin (displayln a)
                          (loop (- a 1))))))
        3
        2
        1
        #f

    This also lets you call Nu compiler/Racket functions that aren't exposed
    to Arc:

        > (%.make-global-var 5)
        #<fn>

        > (%.string? "foo")
        #t

  * `[a b c]` is expanded into `(square-brackets a b c)` which is then
    implemented as a macro:

        (mac square-brackets body
          `(fn (_) ,body))

    Likewise, `{a b c}` is expanded into `(curly-brackets a b c)`

    This makes it easy to change the meaning of `[...]` and `{...}` from
    within Arc

  * The Nu compiler is written in Racket, rather than mzscheme

  * Nu cleans up a lot of stuff in Arc 3.1 and fixes bugs (Anarki also fixes
    some bugs in Arc 3.1, but it generally doesn't clean things up)

  * Nu has reorganized Arc 3.1 significantly, hopefully this makes it easier
    to understand and hack

  * All special forms (`assign`, `fn`, `if`, `quasiquote`, and `quote`) are
    implemented as ordinary Arc macros

  * For more details on the differences between Arc/Nu and Arc/pg, see [this
    page](../../blob/arc%2Fnu/notes/differences.md)
