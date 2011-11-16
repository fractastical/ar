_Nu_ is an Arc compiler which is derived from _ar_, but differs significantly in certain ways. Large chunks of the compiler were copied from _ar_, but a lot was written by me.

The current differences are as follows:

*   No separation between ar and ac: the entire compiler is in _compiler.arc_

*   Compiler names uniformly start with `ac-` rather than `ac-` and `ar-`

*   Does not provide `(ail-code ...)` Instead, `(%nocompile ...)` causes the expression to not be compiled at all: it passes directly to Racket. This is needed for Racket macros. Unlike `ail-code`, however, you can use `(%compile ...)` within `%nocompile` to cause that expression to be compiled, like so:

        (%nocompile (some-racket-macro ... (%compile ...)))

    This is similar to `quasiquote` and `unquote`

*   _arc.arc_ is now split into multiple files:

    *   _compiler.arc_ contains the bare minimum needed to run _core.arc_
    *   _core.arc_ contains very basic things (like `def`, `mac`, and `let`) and implements the rest of _compiler.arc_
    *   _arc.arc_ contains everything else
    *   _repl.arc_ contains a simple REPL written in a more limited form of Arc

*   Code is organized into different sections, making it easier to navigate the code base

*   _Nu_ does not hardcode **any** symbols _whatsoever_. All special forms (`fn`, `assign`, `quote`, etc.) are implemented as macros. This makes Arc much simpler and easier to reason about, without any cost in code maintenance

*   All binary operators (`is`, `+`, `<`, etc.) are implemented in terms of `case-lambda` for increased speed, [as suggested by waterhouse](https://sites.google.com/site/arclanguagewiki/arc-3_1/optimizations)

*   `nil` is a global variable that contains the Racket null value `'()` This is not noticable in Arc code, but makes the compiler implementation much simpler

*   A lot of built-in functions are defined in Arc, rather than in _ar.arc_ and _ac.arc_. This results in much shorter and clearer code

*   Can include literal Racket values like `#t` and `#f` in code, without needing to wrap them in `ail-code` or `%nocompile`

*   `(coerce 2 'num)` returns `2.0` rather than `2`

*   Optional arguments use their defaults when explicitly passed `nil`:

        (def foo (a (o b 5) (o c 10))
          (list a b c))

        (foo 1)         -> (1 5 10)
        (foo 1 nil nil) -> (1 5 10)
        (foo 1 nil 3)   -> (1 5 3)
        (foo 1 2 3)     -> (1 2 3)

*   There are no ["complex fn"](#complexfn)s in _Nu_: everything is done with a plain `racket-lambda`. This should hopefully be faster while also providing better error messages

*   Keyword arguments are supported:

        (def foo (a b #:c)
          (list a b c))

        (foo 1 2)       -> (1 2 nil)
        (foo 1 2 #:c 3) -> (1 2 3)
        (foo #:c 3 1 2) -> (1 2 3)


        (def foo (a b (o #:c 5))
          (list a b c))

        (foo 1 2)       -> (1 2 5)
        (foo 1 2 #:c 7) -> (1 2 7)

*   `{a b c}` expands into `(curly-bracket a b c)` which lets you write a macro/fn to change the behavior of the `{}` syntax

*   It is easy to specify that all arguments to a function should be optional: just parameterize `ac-fn-required-args?` to `nil`. Likewise, it's easy to specify that destructuring should be rigid by setting `ac-fn-rigid-destructuring?` to `t`.

    Note: rigid destructuring means that `(let (a b) (list 1) a)` will fail because an insufficient number of arguments were passed.

*   It is easy to change the behavior of function arguments by changing parameters:

    *   `ac-fn-required-args?` specifies whether required arguments are allowed, or whether all arguments are optional
    *   `ac-fn-excess-args?` specifies whether it's allowed to give a function more arguments than it requires
    *   `ac-fn-rigid-destructuring?` changes whether destructuring allows the supplied list to be bigger or smaller in size than specified

<h1 id="complexfn">Complex fns</h1>

Unlike _Arc 3.1_ and _ar_, _Nu_ does not use so-called "complex fn"s. What is a complex fn? Basically, it means that this:

    (fn (a (o b 3)) ...)

Is compiled into (the equivalent of) this:

    (w/uniq u
      `(fn ,u
         (withs (a (car ,u)
                ,u (cdr ,u)
                 b (or (car ,u) 3))
           ...)))

As you can see, it creates a function that takes any number of arguments, and then *manually parses* the argument list to get the desired result. This is bad for many reasons:

*   It's slower because it's manually parsing the argument list, rather than letting Racket do it

*   You get less useful error messages, unless you put in a lot of extra work and error checking. As an example, consider this function:

        (def foo (a (o b 3))
          (list a b))

    If you call `(foo)` in _Arc 3.1_, you get the following error message: `Error: "car: expects argument of type <pair>; given ()"` This is because it's using `car` to manually parse the argument list, as demonstrated above.

    In _ar_ it's even worse: it returns the list `(nil 3)`. Even though `a` is specified as a required argument, _ar_ acts as if it's optional. But it only does this with complex fns, so whether it throws an error or not is dependent on whether your function uses optional/destructuring args or not!

    In _Nu_, you get this error: `error: procedure foo: no clause matching 0 arguments`

    Not the best error message in the world, but certainly better than _Arc 3.1_. This is because _Nu_ uses plain `lambda`s, which means it automatically uses Racket's parsing and error checking code, rather than manually doing it in the compiler.

    How is this possible, you might wonder? Well, it's actually really simple: [Racket already supports optional, rest, and even keyword arguments](http://docs.racket-lang.org/guide/lambda.html). So in _Nu_, this function:

        (fn (a (o b 3) . c) ...)

    Is compiled into this:

        (racket-lambda (a (b 3) . c)
          (racket-set! c (racket-list->mlist c))
          ...)

    The only issue then is destructuring args, which Racket doesn't support. But that too can use plain old Racket `lambda`s, simply by changing the function's body. Thus, this:

        (fn (a (b (c d)) e) ...)

    Is compiled into this (where `g1` through `g4` are gensyms):

        (racket-lambda (a g1 e)
          (racket-let* ((b  (car g1))
                        (g1 (cdr g1))
                        (g2 (car g1))
                        (c  (car g2))
                        (g2 (cdr g2))
                        (d  (car g2)))
            ...))

        (racket-lambda (a g1 e)
          (apply (racket-lambda ((b nil) (g2 nil) . g4)
                   (apply (racket-lambda ((c nil) (d nil) . g3)
                            ...)
                          g2))
                 g1))

    I'm using a rather interesting technique here. I realized that `(fn (a (b)) ...)` could be statically translated into `(fn (a _) (apply (fn (b) ...) _))` In other words, by applying nested functions, you end up destructuring the arguments. But in Arc, destructuring is very lax: if you supply a smaller or bigger list than expected, it'll just return nil rather than error. That's why the `racket-lambda`s above are more complicated. This can be changed by parameterizing `ac-fn-rigid-destructuring?`.

    This code is just as efficient as if you had done the destructuring by hand!
