Nu is an Arc compiler which is derived from ar, but differs significantly in certain ways. Large chunks of the compiler were copied from ar, but a lot was written by me.

The current differences are as follows:

*   No separation between ar and ac: the entire compiler is in compiler.arc

*   Compiler names uniformly start with ac- rather than ac- and ar-

*   Does not provide (ail-code ...) Instead, (%nocompile ...) causes the expression to not be compiled at all: it passes directly to Racket. This is needed for Racket macros. Unlike ail-code, however, you can use (%compile ...) within a %nocompile to cause that expression to be compiled, like so:

        (%nocompile (some-racket-macro ... (%compile ...)))

    This is similar to quasiquote and unquote

*   arc.arc is now split into multiple files:
 *   compiler.arc contains the absolute bare minimum
 *   core.arc contains very basic things (like def, mac, and let) and implements the rest of compiler.arc
 *   arc.arc contains everything else
 *   repl.arc contains a simple REPL written in a more limited form of Arc

*   Code is organized into different sections, making it easier to navigate the code base

*   Nu does not hardcode any symbols whatsoever. All special forms (fn, assign, quote, etc.) are implemented as macros. This makes Arc much simpler and easier to reason about, without any cost in code maintenance

*   All binary operators (is, +, <, etc.) are implemented in terms of case-lambda for increased speed, as suggested by waterhouse (https://sites.google.com/site/arclanguagewiki/arc-3_1/optimizations)

*   nil is a global variable that contains the Racket null value '() This is not noticable in Arc code, but makes the compiler implementation much simpler

*   A lot of built-in functions are defined in Arc, rather than in ar.arc and ac.arc. This results in much shorter and clearer code

*   Can include literal Racket values like #t and #f in code, without needing to wrap them in ail-code or %nocompile
 *   Because of this, keyword arguments are also supported automatically:

        ((fn (#:foo foo) foo) #:foo 50) -> 50

     I'm working on better syntax for it, though

*   (coerce 2 'num) returns 2.0 rather than 2

*   Optional arguments use their defaults when explicitly passed nil:

        (def foo (a (o b 5) (o c 10))
          (list a b c))

        (foo 1 2 3)     -> (1 2 3)
        (foo 1 nil 3)   -> (1 5 3)
        (foo 1)         -> (1 5 10)
        (foo 1 nil nil) -> (1 5 10)

*   A complex fn is only created when destructuring: default, optional, and rest args are all handled with a plain racket-lambda
