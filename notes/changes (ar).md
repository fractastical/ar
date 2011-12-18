Links
=====

  * [Implicit variables](dynamic.md) are implemented better

  * [Quasiquote](quasiquote.md) is implemented better


Better REPL
===========

The REPL has been improved significantly. GNU readline support is built-in,
so you no longer need to call `rlwrap`. As a consequence, you can press `Tab`
to activate name autocompletion:

    > f
    filechars    fill-table   firstn       flushout     fn?          force-close  fromdisk
    file-exists  find         flat         fn           for          forlen       fromstring

In addition, when inputting an expression that spans multiple lines, pressing
the `Up` arrow key will bring up the entire expression, rather than the last
line:

    > (def foo ()
        (+ 1 2))
    #<fn:foo>

(press the `Up` key now)

    > (def foo ()
        (+ 1 2))

It also accepts multiple expressions, which are evaluated sequentially:

    > "foo" "bar" "qux"
    "foo"
    "bar"
    "qux"

Lastly, `Ctrl+C` aborts the currently-evaluating expression, but does not exit
the REPL:

    > ((afn () (self)))

(press `Ctrl+C` now)

    ^Cuser break
    >


Other
=====

  * Starts up significantly faster than _ar_, but significantly slower than
    _Arc 3.1_

  * No separation between ar and ac: the entire compiler is in _compiler.arc_

  * Compiler names uniformly start with `ac-` rather than `ac-` and `ar-`

  * Does not provide `(ail-code ...)`. Instead, `(%nocompile ...)` causes the
    expression to not be compiled at all: it passes directly to Racket. This
    is mostly needed when dealing with Racket macros. If you want an
    expression within `%nocompile` to be compiled, you can splice it in with
    `ac-compile` or `ac-args` like so:

        #`(%nocompile ('some-racket-macro ,(ac-compile ...) ,@(ac-args ...)))

  * `nil` is a global variable that contains the Racket null value `'()` This
     is not noticable in Arc code, but makes the compiler implementation much
     simpler

  * Some built-in functions are defined in Arc, rather than in _ar.arc_ and
    _ac.arc_. This results in much shorter and clearer code

  * Can include literal Racket values like `#t` and `#f` in code, without
    needing to wrap them in `ail-code` or `%nocompile`

  * Global functions have names. In _ar_, all global functions are given a
    gensym as a name:

        > (def foo ())
        #<fn:g1>

    But in _Nu_, the above function would have the name `foo`, as one would
    expect

  * `annotate` has been implemented with a Racket `struct` rather than
    `vector`

  * `implicit` has been renamed to `parameter`
