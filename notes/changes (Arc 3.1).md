Complex fns
===========

Unlike _Arc 3.1_ and _ar_, _Nu_ does not use so-called "complex fn"s:
everything is done with a plain `racket-lambda`. This is faster while also
providing better error messages. What is a complex fn? Basically, it means
that this:

    (fn (a (o b 3)) ...)

Is compiled into (the equivalent of) this:

    (w/uniq u
      `(fn ,u
         (withs (a (car ,u)
                ,u (cdr ,u)
                 b (or (car ,u) 3))
           ...)))

As you can see, it creates a function that takes any number of arguments, and
then *manually parses* the argument list to get the desired result. This is
bad for many reasons:

*   It's slower because it's manually parsing the argument list, rather than
    letting Racket do it

*   You get less useful error messages, unless you put in a lot of extra work
    and error checking. As an example, consider this function:

        (def foo (a (o b 3))
          (list a b))

    If you call `(foo)` in _Arc 3.1_, you get the following error message:
    `Error: "car: expects argument of type <pair>; given ()"` This is because
    it's using `car` to manually parse the argument list, as demonstrated
    above.

    In _ar_ it's even worse: it returns the list `(nil 3)`. Even though `a` is
    specified as a required argument, _ar_ acts as if it's optional. But it
    only does this with complex fns, so whether it throws an error or not is
    dependent on whether your function uses optional/destructuring args or
    not!

    In _Nu_, you get this error: `error: procedure foo: no clause matching 0
    arguments`

    Not the best error message in the world, but certainly better than
    _Arc 3.1_. This is because _Nu_ uses plain `lambda`s, which means it
    automatically uses Racket's parsing and error checking code, rather than
    manually doing it in the compiler.

    How is this possible, you might wonder? Well, it's actually really simple:
    [Racket already supports optional, rest, and even keyword arguments](http://docs.racket-lang.org/guide/lambda.html).
    So in _Nu_, this function:

        (fn (a (o b 3) . c) ...)

    Is compiled into this:

        (racket-lambda (a (b 3) . c) ...)

    The only issue then is destructuring args, which Racket doesn't support.
    In that case, I still use a normal lambda, but do the destructuring in the
    function's body. Thus, this:

        (fn (a (b) c) ...)

    Is compiled into this (where `g1` is a gensym):

        (racket-lambda (a g1 c)
          (racket-let* ((b (car g1)))
            ...))

Like *Arc 3.1* and *ar*, this means the overhead from destructuring is
*very low*: it's just as fast as if you had done the destructuring yourself.


Other
=====

  * _"arc.arc"_ is now split into multiple files:

      * _"01 compiler.arc"_ contains the bare minimum needed to run _"02 core.arc"_
      * _"02 core.arc"_ contains very basic things (like `def`, `mac`, and `let`)
        and implements the rest of _"01 compiler.arc"_
      * _"03 ssyntax.arc"_ implements `ssyntax` and `ssexpand`
      * _"04 arc.arc"_ contains everything else
      * _"05 import.arc"_ implements namespace inheritance and file loading
      * _"06 extra.arc"_ contains new functions that I've found to be useful

      * _"lib/repl.arc"_ contains the REPL
      * _"lib/compat.arc"_ serves as a compatibility layer between _Nu_ and _Arc 3.1_

  * Code is organized into different sections, making it easier to navigate
    the code base

  * The _Nu_ compiler does not hardcode any of the special forms: `assign`,
    `fn`, `if`, and `quote` are implemented as macros. This makes Arc much
    simpler and easier to reason about, without any cost in code maintenance.

  * All binary operators (`is`, `+`, `<`, etc.) are implemented in terms of
    `case-lambda` for increased speed, [as suggested by waterhouse](https://sites.google.com/site/arclanguagewiki/arc-3_1/optimizations)

  * `(coerce 2 'num)` returns `2.0` rather than `2`

  * `sym` accepts multiple arguments: `(sym "foo" "bar")` -> `foobar`

  * Macros have names too:

        > do
        #<mac:do>

  * Ssyntax is expanded in function arguments. So you can do things like this:
    `(fn (a.b) ...)` which is the same as `(fn ((a b)) ...)`

  * The `do` and `with` macros are smarter: `(do 5)` and `(with () 5)` compile
    into `5` rather than `((racket-lambda nil 5))`

  * `num` (from _"strings.arc"_) has been renamed to `commafy`

  * `assoc` now has the list first, and the key second:

        (assoc foo 'bar)

  * `dir` accepts two optional arguments: a string path and a function to
    apply on every element in the list. In addition, directories have a
    trailing `/` at the end of their name

  * `last` accepts a string: `(last "foo")` returns `#\o`

  * `empty` now works on the empty symbol `'||` as well as any sequence that
    has a `len` of `0`

  * `in` is faster when given only two arguments: `(in x 5)` expands into
    `(is x 5)`

  * `setforms` has been implemented better. For instance, `(zap + x 1)` just
    expands into `(assign x (+ x 1))` and `(push x foo)` expands into
    `(assign foo (cons x foo))`

  * `=` no longer calls `atomic-invoke`. If you want to *guarantee* that
    assignment is thread-safe, wrap it yourself

  * Nu no longer has `defset`: everything is done in `sref`. This idea is
    courtesy of rocketnia

  * The `whilet` macro supports destructuring:

        > (= my-alist '((a 1) (b 2) (c 3) (d 4)))

        > (whilet (k v) pop.my-alist
            (prn k " " v))
        a 1
        b 2
        c 3
        d 4
        nil

  * `or` is implemented better: `(or a b)` is compiled into
    `((fn (g1) (if g1 g1 b)) a)`

  * `coerce` allows you to coerce to/from a table

  * `coerce` is implemented better: you can use `defcoerce` and `defcoerces`
    to easily define new coercion rules

  * `assoc` and `alref` accept an optional "value not found" parameter:

        > (alref '((a 5) (b 10)) 'c 15)
        15

  * `commafy` now inserts an initial `0` by default:

        > (commafy 0.5)
        "0.5"