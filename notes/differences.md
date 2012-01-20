Differences between Arc/Nu and Arc/pg
=====================================

  * The following special Racket values are defined:

        #%app #%begin #%call #%datum #%if #%lambda #%let* #%set #%set-global #%top

  * The following Arc macros are defined:

        % assign fn if square-brackets quasiquote quote

  * The following Arc functions are defined:

        %symbol-global close1 pipe ref

  * The following macro works differently in Nu (use `%` instead):

        > (mac $ (x) `(cdr `(0 . ,,x)))
        > ($ (let a 5 a))
        5

  * Lexical variables take precedence over macros:

        > (mac foo (x) `(+ ,x 2))
        > (foo 0)
        2
        > (let foo [+ _ 5] (foo 0))
        5

  * Functions print with `#<fn:...>` and macros print with `#<mac:...>`. In
    addition, macros have names:

        > do
        #<mac:do>

  * `[a b c]` is expanded into `(square-brackets a b c)` which is then
    implemented as a macro:

        (mac square-brackets body
          `(fn (_) ,body))

    Likewise, `{a b c}` is expanded into `(curly-brackets a b c)`

    This makes it easy to change the meaning of `[...]` and `{...}` from
    within Arc

  * `quote` passes its value unchanged through the compiler, instead of
    copying it:

        > (mac inline (x) `',(eval x))
        > (= x '(a b c))
        > (is x (inline x))
        t

  * Global variables are represented with their Arc names:

        > x
        error: reference to undefined identifier: x

  * Function rest args are `nil`-terminated:

        > (cdr ((fn args args) 1))
        nil

  * `uniq` is implemented using actual Racket gensyms

  * The queue bug [has been fixed](http://arclanguage.org/item?id=13616)

  * Anything not understood by the compiler is considered to be a literal.
    Thus, Racket values can be used freely:

        > (if #f 5 10)
        10

        > #(foo bar qux)
        #(foo bar qux)

    In addition, function and macro values can be included by macros:

        > (mac foo (x)
            `(,let a 5
               (,+ ,x a)))

        > (macex1 '(foo 10))
        (#<mac:let> a 5 (#<fn:+> 10 a))

        > (foo 10)
        15

    This enables you to write hygienic macros in Arc

  * A new `'inline-calls` declare mode, which is even faster than
    `'direct-calls`:

        > (declare 'inline-calls t)

        > (%.ac '(+ 1 2))
        (#<fn:+> 1 2)

    Basically, it takes the value of the symbol at compile-time and splices it
    into the expression. This is much faster than direct-calls because it
    doesn't need to do a global lookup at runtime.

    The downside is that if you redefine any global variable, even functions,
    those changes aren't retroactive: they'll affect new code but not old
    code
