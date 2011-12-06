Dynamic variables
=================

In ar, the usual way to create a dynamic variable is to use the `implicit`
form:

    > (implicit foo 5)

    > foo
    5

    > (w/foo 10 foo)
    10

    > foo
    5

This is very convenient, and so Nu provides exactly the same thing, except
`implicit` has been renamed to `parameter`:

    > (parameter foo 5)

    > foo
    5

    > (w/foo 10 foo)
    10

Ar also provides another form called `dynamic` which is exactly like
`implicit` except that it doesn't create the `w/` macro. In my own code, I
have never once used `dynamic` since `implicit` is pretty much always better.

As I was implementing namespaces, however, I discovered that parameters do not
play well with multiple namespaces. So I created a `dynamic` form, except it
behaves very differently from the `parameter` form. Here are the three
differences:

 1. `dynamic` does not create a `w/` form. For instance, `(dynamic foo 5)`
    does not create `w/foo`. In this regard, Nu's `dynamic` is just like ar's
    `dynamic`

 2. Dynamic variables are dynamic with respect to multiple namespaces,
    whereas parameter variables are not. To explain this, I'll use a simple
    example:

        ;; foo.arc
        (def foo (x)
          (bar x))

        (def bar (x)
          x)

        ;; bar.arc
        (import foo)

        (def bar (x)
          10)

        (foo 50)

    In the above, do you expect the call to `foo` to return `50` or `10`?
    Because of lexical scoping, it returns `50`. But let's suppose you don't
    want that: let's suppose you want the `bar` function to be *dynamic*.

    You can then do the following:

      (eval-w/ foo
        (dynamic bar bar))

    And now when you call `(foo 50)` it will return `10` because it's using
    "bar.arc"s version of `bar` rather than "foo.arc"s. To give a more
    concrete example, consider Arubic.

    Arubic is currently implemented as a library on top of Nu. It can do this
    because it resides in a different namespace, so as not to interfere with
    Arc.

    Now, Arubic wants to change the `print` function so that it can change how
    various objects print. But merely changing the `print` function is not
    enough: `write`, `disp`, `prn`, among others all call `print`, but they
    use Arc's version of `print`, not Arubic's version.

    The solution to this is to use `(dynamic print print)` and now every time
    a function calls `print`, it will use whichever `print` is defined in the
    current namespace, rather than the original namespace.

 3. Changing a parameter affects all namespaces, but changing a dynamic
    variable affects only the current namespace. I'll illustrate with an
    example:

        ;; foo.arc
        (implicit qux   5)
        (dynamic  corge 5)

        ;; bar.arc
        (import foo)

        (prn qux)   -> this prints 5
        (prn corge) -> this prints 5

        (= qux   10) -> qux is now 10 in every namespace
        (= corge 10) -> corge is now 10 only in bar.arc's namespace

    Thus, parameters are useful if you wish to supply options that affect the
    behavior of your program, or if you wish to thread a value through many
    functions but don't want to have to pass an argument explicitly.

    On the other hand, dynamic variables are useful if you wish for a variable
    to obtain it's value from the current namespace, rather than the namespace
    that it was defined in.
