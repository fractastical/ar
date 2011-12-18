Links
=====

  * Nu has [hygienic macros](hygiene.md)

  * Nu has good [namespace support](namespaces.md)


Other
=====

  * Supports a `(%splice ...)` form which splices the items into the list at
    compile-time. Thus, `(+ (%splice 1 2 3))` is exactly the same as
    `(+ 1 2 3)`. This is useful within macro expansions.

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
