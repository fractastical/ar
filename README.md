Nu is an Arc compiler which is derived from _ar_, but differs significantly in certain ways. Large chunks of the compiler were copied from _ar_, but a lot was written by me.

The current differences are as follows:

*   No separation between ar and ac: the entire compiler is in _compiler.arc_

*   Compiler names uniformly start with `ac-` rather than `ac-` and `ar-`

*   Does not provide `(ail-code ...)` Instead, `(%nocompile ...)` causes the expression to not be compiled at all: it passes directly to Racket. This is needed for Racket macros. Unlike `ail-code`, however, you can use `(%compile ...)` within `%nocompile` to cause that expression to be compiled, like so:

        (%nocompile (some-racket-macro ... (%compile ...)))

    This is similar to `quasiquote` and `unquote`

*   _arc.arc_ is now split into multiple files:

    *   _compiler.arc_ contains the absolute bare minimum
    *   _core.arc_ contains very basic things (like `def`, `mac`, and `let`) and implements the rest of _compiler.arc_
    *   _arc.arc_ contains everything else
    *   _repl.arc_ contains a simple REPL written in a more limited form of Arc

*   Code is organized into different sections, making it easier to navigate the code base

*   Nu does not hardcode any symbols whatsoever. All special forms (`fn`, `assign`, `quote`, etc.) are implemented as macros. This makes Arc much simpler and easier to reason about, without any cost in code maintenance

*   All binary operators (`is`, `+`, `<`, etc.) are implemented in terms of `case-lambda` for increased speed, as suggested by waterhouse (https://sites.google.com/site/arclanguagewiki/arc-3_1/optimizations)

*   `nil` is a global variable that contains the Racket null value `'()` This is not noticable in Arc code, but makes the compiler implementation much simpler

*   A lot of built-in functions are defined in Arc, rather than in _ar.arc_ and _ac.arc_. This results in much shorter and clearer code

*   Can include literal Racket values like `#t` and `#f` in code, without needing to wrap them in `ail-code` or `%nocompile`

    *   Because of this, keyword arguments are also supported automatically:

            ((fn (#:foo foo) foo) #:foo 50) -> 50

        I'm working on better syntax for it, though

*   `(coerce 2 'num)` returns `2.0` rather than `2`

*   Optional arguments use their defaults when explicitly passed `nil`:

        (def foo (a (o b 5) (o c 10))
          (list a b c))

        (foo 1 2 3)     -> (1 2 3)
        (foo 1 nil 3)   -> (1 5 3)
        (foo 1)         -> (1 5 10)
        (foo 1 nil nil) -> (1 5 10)

*   A complex fn is only created when destructuring: default, optional, and rest args are all handled with a plain `racket-lambda`
