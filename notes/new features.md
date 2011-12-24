Links
=====

  * Nu provides a convenient syntax for [hygienic macros](hygiene.md)

  * Nu has good [namespace support](namespaces.md)

  * Nu has [fexprs](fexprs.md)


Other
=====

  * Supports a `(%splice ...)` form which splices the items into the list at
    compile-time. Thus, `(+ (%splice 1 2 3))` is exactly the same as
    `(+ 1 2 3)`. This is useful within macro expansions.

  * Provides `%if` and `%fncase` macros to make it easier to interface with
    Racket code:

        (%if (racket-pair? x)
               ...
             (racket-symbol? x)
               ...)

        (%fncase x
          racket-pair?   ...
          racket-symbol? ...)

  * Provides `%eval` and `%inline` macros which return the value of the
    expression at compile-time:

        > (ac-compile '(%eval +))
        #<fn:+>

        > (ac-compile '(%eval (+ 1 2 3 (- 4 5))))
        5

        > (ac-compile '(%inline (+ 1 2 3 (- 4 5))))
        (#<fn:+> 1 2 3 (#<fn:-> 4 5))

    `%eval` evaluates the expression at compile-time, so naturally it works on
    any expression, but `%inline` works differently: for *global* symbols it
    returns the value, for lists it maps over them, and for everything else it
    just returns it as-is.

    This is primarily for performance: code will run significantly faster
    because it only needs to look up the global variable once rather than
    every time the function is run:

        (def no (x)
          (%inline (is x nil)))

    The obvious downside is that because it only looks up the variable once,
    if you change the variable, the changes won't work. So Nu uses this only
    for situations where that's acceptable, such as for `racket-` functions
    and compiler functions like `ac-tnil`, or functions where speed is *really
    critical* like `+`, `<`, `no`, etc.

  * `{a b c}` expands into `(curly-bracket a b c)` which lets you write a
    macro/fn to change the behavior of the `{}` syntax

  * It is easy to change the behavior of function arguments by changing
    parameters:

      * `ac-fn-required-args?` specifies whether required arguments are
        allowed, or whether all arguments are optional
      * `ac-fn-excess-args?` specifies whether it's allowed to give a function
        more arguments than it requires
      * `ac-fn-rigid-destructuring?` changes whether destructuring allows the
        supplied list to be bigger or smaller in size than specified
      * `ac-fn-optional-on-nil?` changes whether `nil` causes optional
        arguments to trigger their default. In other words, whether
        `((fn ((o a 5)) a) nil)` should return `5` or `nil`

  * Keyword arguments are supported:

        > (def foo (:a :b)
            (list a b))

        > (foo :a 1)
        (1 nil)

        > (foo :a 1 :b 2)
        (1 2)

  * Keyword destructuring is supported:

        > (let (:a :b :c) (obj a 1 b 2 c 3)
            (list a b c))
        (1 2 3)

  * Alists support indexing by key rather than number:

        > (= foo '((b 2) (a 1) (c 3)))
        ((b 2) (a 1) (c 3))

        > foo!a
        1

        > (= foo!a 10)
        10

        > foo
        ((b 2) (a 10) (c 3))

  * [Courtesy of rocketnia](http://arclanguage.org/item?id=13450), `:` syntax
    can be used to do more complex composes. The following two are equivalent:

        (+ 1 2 : + 3 4 : + 5 6)
        (+ 1 2 (+ 3 4 (+ 5 6)))

  * `plist` and `plref` for plists:

        > (plist '(b 10 a 5 c 15) 'a)
        (a 5 c 15)

        > (plref '(b 10 a 5 c 15) 'a)
        5
