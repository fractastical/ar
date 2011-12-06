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

    > foo
    5

Ar also provides another form called `dynamic` which is exactly like
`implicit` except that it doesn't create the `w/` macro. In my own code, I
have never once used `dynamic` since `implicit` is pretty much always better.

As I was implementing namespaces, however, I discovered that parameters do not
play well with multiple namespaces. So I created a `dynamic` form, except it
behaves very differently from the `parameter` form. Here are the two
differences:

 1. `dynamic` does not create a `w/` form. For instance, `(dynamic foo 5)`
    does not create `w/foo`. In this regard, Nu's `dynamic` is just like ar's
    `dynamic`

 2. Dynamic variables are dynamic with respect to multiple namespaces,
    whereas parameter variables are not. To explain this, I'll use a simple
    example:

        ;; foo.arc
        (implicit qux 5)
        (dynamic  nou 5)

        ;; bar.arc
        (import foo)

        (prn qux) -> this prints 5
        (prn nou) -> this prints 5

        (= qux 10) -> qux is now 10 in every namespace
        (= nou 10) -> nou is now 10 only in bar.arc's namespace

    As you can see, dynamic variables obtain their value from the current
    namespace, whereas parameter variables always refer to the value in the
    namespace they were defined in.

    In addition, setting a dynamic variable always sets it in the current
    namespace, but setting a parameter variable always sets it in the defining
    namespace.

    Thus, parameters are useful if you wish to supply options that affect the
    behavior of your program, or if you wish to thread a value through many
    functions but don't want to pass an argument explicitly.

    On the other hand, dynamic variables are useful if you wish for a variable
    to obtain its value from the current namespace, rather than the namespace
    that it was defined in.

    To illustrate why this is useful, let's talk about Arubic. Arubic is
    currently implemented as a library on top of Nu. It can do this
    because it resides in a different namespace, so as not to interfere with
    Arc.

    Now, Arubic wants to change the `print` function so that it can change how
    various objects print. But merely changing the `print` function is not
    enough: `write`, `disp`, `prn`, among others all call `print`, but they
    use Arc's version of `print`, not Arubic's version.

    The solution to this is to use `(dynamic print print)` and now every time
    a function calls `print`, it will use whichever `print` is defined in the
    current namespace, rather than the original namespace.
