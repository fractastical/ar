How to run it
=============

Just call `./arc` and you'll get a REPL. You can also load the file "foo.arc" with `./arc foo`.

If you would like to load a file *and* run the REPL, use the `-i` or `--repl` flags: `./arc -i foo`

If you would like to load *all* the files rather than only the first, use the `-a` or `--all` flags: `./arc -a foo bar qux`

If you would like to run all the unit tests in the `tests/` subdirectory, just call `./arc run-tests | less`


Details
=======

_Nu_ is an Arc compiler which is derived from _ar_, but differs significantly in certain ways. Large chunks of the compiler were copied from _ar_, but a lot was written by me.

The current differences are as follows:

*   No separation between ar and ac: the entire compiler is in _compiler.arc_

*   Compiler names uniformly start with `ac-` rather than `ac-` and `ar-`

*   Does not provide `(ail-code ...)`. Instead, `(%nocompile ...)` causes the expression to not be compiled at all: it passes directly to Racket. This is needed for Racket macros. Unlike `ail-code`, however, you can use `(%compile ...)` within `%nocompile` to cause that expression to be compiled, like so:

        (%nocompile (some-racket-macro ... (%compile ...)))

    This is similar to `quasiquote` and `unquote`

*   Also supports a `(%splice ...)` form which splices the items into the list at compile-time. Thus, `(+ (%splice 1 2 3))` is exactly the same as `(+ 1 2 3)`. This is useful within macro expansions.

*   _arc.arc_ is now split into multiple files:

    *   _compiler.arc_ contains the bare minimum needed to run _core.arc_
    *   _core.arc_ contains very basic things (like `def`, `mac`, and `let`) and implements the rest of _compiler.arc_
    *   _ssyntax.arc_ implements `ssyntax` and `ssexpand`
    *   _compat.arc_ serves as a compatibility layer between _Nu_ and _Arc_
    *   _arc.arc_ contains everything else
    *   _repl.arc_ contains a simple REPL

*   Code is organized into different sections, making it easier to navigate the code base

*   The _Nu_ compiler does not hardcode any of the special forms: `assign`, `fn`, `if`, and `quote` are implemented as macros. This makes Arc much simpler and easier to reason about, without any cost in code maintenance.

*   All binary operators (`is`, `+`, `<`, etc.) are implemented in terms of `case-lambda` for increased speed, [as suggested by waterhouse](https://sites.google.com/site/arclanguagewiki/arc-3_1/optimizations)

*   `nil` is a global variable that contains the Racket null value `'()` This is not noticable in Arc code, but makes the compiler implementation much simpler

*   A lot of built-in functions are defined in Arc, rather than in _ar.arc_ and _ac.arc_. This results in much shorter and clearer code

*   Can include literal Racket values like `#t` and `#f` in code, without needing to wrap them in `ail-code` or `%nocompile`

*   `(coerce 2 'num)` returns `2.0` rather than `2`

*   `sym` accepts multiple arguments: `(sym "foo" "bar")` -> `foobar`

*   There are no ["complex fn"](#complexfn)s in _Nu_: everything is done with a plain `racket-lambda`. This is faster while also providing better error messages

*   `{a b c}` expands into `(curly-bracket a b c)` which lets you write a macro/fn to change the behavior of the `{}` syntax

*   It is easy to change the behavior of function arguments by changing parameters:

    *   `ac-fn-required-args?` specifies whether required arguments are allowed, or whether all arguments are optional
    *   `ac-fn-excess-args?` specifies whether it's allowed to give a function more arguments than it requires
    *   `ac-fn-rigid-destructuring?` changes whether destructuring allows the supplied list to be bigger or smaller in size than specified
    *   `ac-fn-optional-on-nil?` changes whether `nil` causes optional arguments to trigger their default. In other words, whether `((fn ((o a 5)) a) nil)` should return `5` or `nil`

*   Starts up significantly faster than _ar_, but significantly slower than _Arc 3.1_

*   Global functions have names. In _ar_, all global functions are given a gensym as a name:

        > (def foo ())
        #<fn:g1>

    But in _Nu_, the above function would have the name `foo`, as one would expect

*   Macros have names too:

        > do
        #<mac:do>

*   Ssyntax is expanded in function arguments. So you can do things like this: `(fn (a.b) ...)` which is the same as `(fn ((a b)) ...)`

*   The `do` macro is smarter: `(do 1)` compiles into `1` rather than `((racket-lambda nil 1))`

*   `num` has been renamed to `comma`

*   `annotate` has been implemented with a Racket `struct` rather than `vector`

*   Keyword arguments are supported:

        > (def foo (:a :b)
            (list a b))

        > (foo :a 1)
        (1 nil)

        > (foo :a 1 :b 2)
        (1 2)

*   Keyword destructuring is supported:

        > (let (:a :b :c) (obj a 1 b 2 c 3)
            (list a b c))
        (1 2 3)

*   Alists support indexing by key rather than number:

        > (= foo '((b 2) (a 1) (c 3)))
        ((b 2) (a 1) (c 3))

        > foo!a
        1

        > (= foo!a 10)
        10

        > foo
        ((b 2) (a 10) (c 3))

*   `assoc` now has the list first, and the key second:

        (assoc foo 'bar)

*   `dir` accepts two optional arguments: a string path and a function to apply on every element in the list. In addition, directories have a trailing `/` at the end of their name

*   `last` accepts a string: `(last "foo")` returns `#\o`

*   `implicit` has been renamed to `parameter`

*   `empty` now works on the empty symbol `'||` as well as any sequence that has a `len` of `0`

*   `in` is faster when given only two arguments: `(in x 5)`

*   `setforms` has been removed. For instance, `(zap + x 1)` just expands into `(= x (+ x 1))` and `(push x foo)` expands into `(= foo (cons x foo))`

*   `=` no longer calls `atomic-invoke`. If you want to *guarantee* that assignment is thread-safe, wrap it yourself

*   [The REPL](#repl) is implemented better

*   [Implicit variables](#implicit) are implemented better



<h2 id="hygiene">Hygienic macros</h2>

Ar made a rather interesting change: it is possible to place a function/macro *value* into a quasiquote. In other words, rather than writing this:

    (mac unless (test . body)
      `(if (no ,test) (do ,@body)))

You can instead write this, which has the same effect:

    (mac unless (test . body)
      `(,if (,no ,test) (,do ,@body)))

Why would you want to do this? Two reasons:

 1. It's faster. Because you're splicing the value in, the compiler no longer has to do a global lookup. In addition, the Nu compiler will optimize code that inserts function values. Basically, rather than doing a global lookup every time, you instead do it once (when the macro is expanded), and then reuse that value.

 2. More importantly, it makes macros hygienic. Keep in mind the reason for bad hygiene with macros: name collision. Let's look at the macro above. Most of the time, it'll work fine. But if I do this...

        (let no (fn (x) nil)
          (unless foo
            ...))

    ...then the macro will be using the local definition of `no` rather than the global one. It's possible to work around this by using gensyms, but that's impractical in this case. Instead, all you have to do is unquote the functions and macros inside quasiquote: now they will always use the global definition, and the above example will work fine. That's all that's needed to make macros hygienic.

    What's interesting about this technique is that it's *extremely super* easy to implement: no big complicated hygiene systems. It also makes it really easy for the person writing the macro to specify whether they want hygiene or not: if you unquote the value, it's hygienic. If you don't unquote it, it's not. No need for a separate macro system!

    And this works perfectly with Nu namespaces as well: if you unquote a value, it'll refer to the name in the namespace where the macro is defined. If you don't unquote it, it'll use the value in the current namespace.

    Unfortunately, it's clunky and ugly to have to unquote almost everything, so Nu provides a special syntax which is similar to quasiquote, but rather than quoting things by default, it unquotes them. Thus the above macro could be written as:

        (mac unless (test . body)
          #`(if (no test) (do ,@body)))

    But you might ask: what about anaphoric macros? If you want to include a literal symbol rather than a value, you use quote:

        (mac afn (parms . body)
          #`(rfn 'self parms ,@body))

    You can still use the old quasiquote syntax if you like, this new syntax is just a convenience to make writing hygienic macros easier.



<h2 id="repl">Better REPL</h2>

The REPL has been improved significantly. GNU readline support is built-in, so you no longer need to call `rlwrap`. As a consequence, you can press `Tab` to activate name autocompletion:

    > f
    filechars    fill-table   firstn       flushout     fn?          force-close  fromdisk
    file-exists  find         flat         fn           for          forlen       fromstring

In addition, when inputting an expression that spans multiple lines, pressing the `Up` arrow key will bring up the entire expression, rather than the last line:

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

Lastly, `Ctrl+C` aborts the currently-evaluating expression, but does not exit the REPL:

    > ((afn () (self)))

(press `Ctrl+C` now)

    ^Cuser break
    >



<h2 id="implicit">Implicit variables</h2>

_ar_ contains a very useful feature: Racket parameters do not need to be called to extract their values. In other words, rather than saying `(stdout)` you can instead just say `stdout`. Very convenient! Unfortunately, Arc code doesn't expect this, thus _ar_ is unable to make use of any Arc libraries that use `stdin`, `stdout`, or `stderr`.

There are a couple ways to work around this:

  1. _ar_ could choose to make *most* parameters implicit, but not the `std*` ports

  2. _ar_ could use a compatibility library that would temporarily disable implicit variables (at least for the `std*` ports) when loading _Arc 3.1_ code

I chose a different option for _Nu_, however... here's how it works. When the _Nu_ compiler sees a symbol, it will generate different code depending on whether the variable is global or local. In addition, _Nu_ knows whether the symbol is in functional position or not. Thus, with the Arc code `(foo bar qux)`, _Nu_ can output different code for `foo` and `bar qux`. So the rule is: a global variable that is *not in* functional position is looked up at runtime.

Thus, the above example would compile into `(foo (ac-lookup-global-arg bar) (ac-lookup-global-arg qux))`. What does `ac-lookup-global-arg` do? Why, it checks if its argument is a parameter, and if so, it calls it. Because it only does this for function *arguments* and not for the first element in the list, that means you can use either `stdout` or `(stdout)` and they'll both evaluate to the exact same value.

Thus, _Nu_ has implicit variables, while maintaining backwards compatibility with _Arc 3.1_.



<h2 id="complexfn">Complex fns</h2>

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
          (racket-let* ((c (racket-list->mlist c)))
            ...))

    The only issue then is destructuring args, which Racket doesn't support. In that case, I still use a normal lambda, but do the destructuring in the function's body. Thus, this:

        (fn (a (b) c) ...)

    Is compiled into this (where `g1` is a gensym):

        (racket-lambda (a g1 c)
          (racket-let* ((b (car g1)))
            ...))

Just like *Arc 3.1* and *ar*, this means the overhead from destructuring is *very low*: it's just as fast as if you had done the destructuring yourself.
