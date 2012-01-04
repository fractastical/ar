Lite Nu is Nu stripped down to the bare minimum:

  * _"01 nu.rkt"_ is the Nu compiler for Arc
  * _"02 arc.arc"_ is copied unmodified from Arc 3.1
  * _"03 repl.arc"_ implements a REPL
  * _"arc"_ is an executable that will load the above three files in order

Okay, so it's basically just Arc 3.1 (it even copies arc.arc from Arc 3.1!).
Why would you want to use it over Arc 3.1 or Anarki, then?

  * It's faster! Nu strives to be *at least* as fast as Arc 3.1, and in some
    cases is significantly faster. For instance, `(+ 1 2)` was 52.94% faster
    in Nu than in Arc 3.1, last time I checked

  * Nu lets you define custom calling behavior for any non-function type by
    extending the `ref` function. Using this, it's possible to implement
    `defcall` from Anarki:

        (= ref* (obj))

        (extend ref (x . args) (orig ref* (type x))
          (apply it args))

        (mac defcall (type parms . body)
          `(= (ref* ',type) (fn ,parms ,@body)))



    As a convenience, rather than extending the `ref` function, you can simply
    assign a function to the `ref*` table:

        (= (ref* 'foo) (fn ...))

    Now Nu will call the function when calling something with a type of
    `'foo`. This is like `defcall` in Anarki



  * Nu reflects some of the compiler functions into Arc, so they can be called
    and hacked from within Arc

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

        > (%.global-ref 'foo)
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

  * Nu makes it possible to add in awesome things like namespaces, aliases,
    and implicit parameters as a library without hacking the compiler

  * Nu cleans up a lot of stuff in Arc 3.1 and fixes bugs (Anarki also fixes
    some bugs in Arc 3.1, but it generally doesn't clean things up)

  * You can use the "arc" executable to write shell scripts:

        #! /path/to/arc
        (prn "foo")

    This is like "arc.sh" in Anarki but implemented in Racket rather than as a
    bash script, so it should be cleaner and more portable

  * Nu has reorganized Arc 3.1 significantly, hopefully this makes it easier
    to understand and hack

  * All special forms (`assign`, `fn`, `if`, `quasiquote`, and `quote`) are
    implemented as ordinary Arc macros

  * For more details on the differences between Arc/Nu and Arc/pg, see [this
    file](../blob/lite-nu/notes/differences.md)
