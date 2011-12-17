Implicit variables
==================

_ar_ contains a very useful feature: Racket parameters do not need to be
called to extract their values. In other words, rather than saying `(stdout)`
you can instead just say `stdout`. Very convenient! Unfortunately, Arc code
doesn't expect this, thus _ar_ is unable to make use of any Arc libraries that
use `stdin`, `stdout`, or `stderr`.

There are a couple ways to work around this:

  1. _ar_ could choose to make *most* parameters implicit, but not the `std*`
     ports

  2. _ar_ could use a compatibility library that would temporarily disable
     implicit variables (at least for the `std*` ports) when loading _Arc 3.1_
     code

I chose a different option for _Nu_, however... here's how it works. When the
_Nu_ compiler sees a symbol, it will generate different code depending on
whether the variable is global or local. In addition, _Nu_ knows whether the
symbol is in functional position or not. Thus, with the Arc code
`(foo bar qux)`, _Nu_ can output different code for `foo` and `bar qux`. So
the rule is: a global variable that is *not in* functional position is looked
up at runtime.

Thus, the above example would compile into:

    `(foo (ac-lookup-global-arg bar) (ac-lookup-global-arg qux))`

What does `ac-lookup-global-arg` do? Why, it checks if its argument is a
parameter, and if so, it calls it. Because it only does this for function
*arguments* and not for the first element in the list, that means you can use
either `stdout` or `(stdout)` and they'll both evaluate to the exact same
value.

Thus, _Nu_ has implicit variables, while maintaining backwards compatibility
with _Arc 3.1_.


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
    `dynamic`.

 2. Dynamic variables are dynamic with respect to multiple namespaces,
    whereas parameter variables are not. To explain this, I'll use a simple
    example:

        ;; foo.arc
        (parameter one 5)
        (dynamic   two 5)

        ;; bar.arc
        (import foo)

        (prn one) -> this prints 5
        (prn two) -> this prints 5

        (= one 10) -> one is now 10 in every namespace
        (= two 10) -> two is now 10 only in bar.arc's namespace

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
