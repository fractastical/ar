The case for namespaces
=======================

To some, namespaces are merely a luxury, something that is nice but not
necessary. But if your goal is to make the language as hackable and malleable
as possible, then I think that namespaces are a *necessity*.

At first glance, it appears that namespaces serve a simple purpose: they exist
so as to prevent variable collisions, thus allowing different modules to use
the same variable names without clobbering each other:

    ;; foo.arc
    (def one ...)
    (def two ...)

    ;; bar.arc
    (def one ...)

    ;; qux.arc
    (import foo bar)

    (def one ...)

In the above example, assuming a proper namespace system has been implemented,
the three files should be able to use the name `one` without clashing at all.
Let's examine how a few languages implement namespaces.

In Python, the above example would create a global variable `foo`, load the
file "foo.arc" into the `foo` namespace, and then do the same for `bar`. Now,
if you want to call the "foo.arc" functions, you need to do this:

    (foo!one ...)
    (foo!two ...)

And likewise for "bar.arc". Although I mentioned Python, ar uses the same
system, so the same criticisms apply to both of them. What are those
criticisms?

 1. As demonstrated, having to prefix every imported function with the
    namespace's name is a huge pain. Python allows for `from foo import *`
    but this is highly discouraged because it basically bypasses the namespace
    mechanism entirely, thus allowing for variable collisions.

 2. It's inefficient. In ar, if you wish to load up a library into a separate
    namespace so that it doesn't interfere with your code, you have to first
    create a new namespace (this is fast), then you need to load the compiler
    into it (this isn't too bad), and then you need to load "arc.arc" into it.

    Wait a moment. You already loaded the compiler and "arc.arc", and now you
    need to *load them again*, from scratch? Yup. And if you have 5 different
    libraries, and you want namespace isolation for them, you now need to load
    the compiler and "arc.arc" a whopping 6 times total.

    Needless to say, that's impractically slow. I assume Python works around
    this by using compiled versions of the built-in functions, so creating new
    namespaces is fast enough.

 3. It does not easily allow for coupling. What am I talking about? I was
    working on Arubic, which is basically my fork of Arc. It's *very* similar
    to Arc, but with a few changes here and there. Unfortunately, these
    changes (although small) break backwards compatibility.

    That means that Arc code cannot make use of Arubic libraries, and vice
    versa. But isn't that what namespaces are supposed to solve? But alas,
    ar's namespaces cannot work. Why not? Because Arubic's changes break
    "arc.arc" itself.

    As an example, in Arubic you would say `(map x foo ...)` rather than
    `(map [...] foo)`. But there is lots of code in "arc.arc" which relies
    upon the current definition of `map`. Because Arubic is so similar to Arc,
    I'd really like to be able to reuse "arc.arc" rather than reimplementing
    large chunks of it in Arubic.

    But in ar, even if I created a separate namespace and then loaded
    "arc.arc" into it, as soon as I load Arubic, it breaks. Thus namespaces
    give isolation from other namespaces, but this isolation is *too great*:
    you want to be able to loosely couple namespaces so that one namespace can
    easily refer to bindings in another namespace.

    It is possible to have one namespace refer to another, like so:

        (let x (runtime)
          (= x!foo foo)
          (= x!bar bar)
          ...)

    But that is tedious. And that doesn't handle updating: if code creates a
    new variable, you need to manually propagate it into the necessary
    namespaces.

    The simple reason for this is that you are taking the *value* of the
    variable and placing it into the namespace, but there's no coupling at
    all, so if the value changes, you need to update it yourself.

How do Nu namespaces work, then? Conceptually, they're exactly like closures,
though they are implemented differently. Consider this example:

    ;; foo.arc
    (def one ...)
    (def two ...)

    ;; bar.arc
    (def one ...)

    ;; qux.arc
    (import foo bar)

    (def one ...)

Right now, if you loaded everything into a single namespace, such as with
`load`, it would look like this:

    ;; foo.arc
    (def one ...)
    (def two ...)

    ;; bar.arc
    (def one ...)

    ;; qux.arc
    (def one ...)

Thus later definitions overwrite the earlier ones, and Bad Things happen.
But this problem has already been solved, and it's called closures. Using
closures, you can create local variables that have the same name as outer
variables:

    ;; foo.arc
    (let (one two) nil
      (def one ...)
      (def two ...)

      ;; bar.arc
      (let (one) nil
        (def one ...)

        ;; qux.arc
        (let (one) nil
          (def one ...))))

As you can see, by nesting local scopes, every file can use the same variable
names without overwriting each other. Instead, inner definitions shadow outer
definitions.

This sounds exactly like what you want! Now "qux.arc" can refer to variables
defined in "foo.arc" and "bar.arc" without needing to prefix the name with
`foo!` or `bar!`. Yet "qux.arc" doesn't need to worry about other libraries
messing with its stuff, nor does it need to worry about accidentally breaking
something.

And because of the way closures work (inheriting variables from the outer
scope), you can create namespaces that are coupled with other ones. Thus,
rather than creating a new empty namespace and loading "arc.arc" into it, you
can instead create a new namespace that *inherits* from "arc.arc". Now you
only need to load "arc.arc" once, and can reuse it multiple times!

If you only focus on the performance benefits, you may erroneously conclude
that this is just a performance optimization. That's as absurd as saying that
closures are an implementation detail. The important thing about this
technique is not that it's more efficient, what's important is what it allows
you to *do*.

With Nu namespaces, it's trivial to write Arubic as a library. Here's how you
would do it. First, at the top of "arubic.arc", you place this line of code:

    (zap new-namespace namespace)

What that does is create a new namespace that inherits from the current
namespace, and then sets the new namespace as the current namespace. Now,
any global variables that occur after that line will be in the new namespace,
and won't clobber existing stuff.

That means I can now write a `map` macro that has different behavior than Arc,
yet Arc will continue to work fine! And even better still, I can load
libraries into Arc's namespace, and Arc can load Arubic libraries.

But that's just the tip of the iceberg. This really simple idea has some very
interesting implications. Think for a moment about what I just described. I
talked about creating a new language that is very similar to an existing one,
yet it can easily run alongside the old language. The new language can make
use of old libraries, and old libraries can make use of libraries written in
the new language.

Does that sound familiar? It sounds a lot like language versions. The
differences between Arc 3.1 and Arubic are less than the differences between
Arc 2 and Arc 3. Right now, language designers are in a tough spot: they want
to improve the language, adding features and fixing bugs, but often cannot
because it would break backwards compatibility.

Nobody wants to be the guy to break existing libraries or programs. Thus
languages stagnate until eventually the language designers decide enough is
enough and they break things (such as the transition from Python 2 to 3).

Because he wanted to make the best language he could, Paul Graham decided he
wasn't going to worry about backwards compatibility: if breaking something
would make Arc a better language in the long run, he would do it.

Unfortunately this means every new version of Arc requires existing code to be
ported, and if the code isn't ported, it simply won't work in the new version.
This is a very sad state of affairs, but it seems the only alternative is to
let the language stagnate.

But imagine if Arc was written differently. Imagine if Paul Graham had written
Arc 2, and then decided that it wasn't good enough, so he started work on Arc
3. But this time, rather than creating a new version that's isolated and
separate from Arc 2, he instead creates a new namespace that inherits from Arc
2.

And then when he realizes Arc 3 is inadequate, he just creates a new namespace
that inherits from Arc 3, and that namespace then becomes Arc 4. And because
of the namespace system, it can seamlessly use Arc 2, 3, and 4 libraries.

This is like as if you bundled the Arc 2 binary with the Arc 3 binary, except
it's only a single binary, which makes it a lot easier for the different
versions to communicate with each other.

And because each namespace holds the *differences* between versions, this has
a lovely side effect: it automatically documents the changes between Arc
versions. No more need for changelogs!

This allows you to make as many changes as you want, while still maintaining
backwards compatibility. Because namespaces allow you to so easily change
things that you otherwise could not change (without breaking things), I
consider namespaces to be essential to Arc's goal of hackability.


Okay, but how do you implement it?
==================================

One of the great things about _Nu_ is that it has good namespace support.
Unfortunately, _Arc 3.1_ doesn't have namespaces, and _ar_'s namespaces are
essentially unusable. What will it take to add in namespace support to
_Arc 3.1_, _ar_, and the other Arc implementations? Not much! Here's what I
have found to be the minimum necessary to implement namespaces in the
compiler:

 1. A Racket parameter called `ac-namespace` that is initialized to Arc's
    namespace. In other words:

        (define ac-namespace (make-parameter (current-namespace)))

 2. Wrap global variables in `(ac-lookup-global ...)`. In other words, this
    expression...

        (+ foo bar)

    ...would be compiled into this:

        (ac-funcall2 (ac-lookup-global +)
                     (ac-lookup-global foo)
                     (ac-lookup-global bar))

    And then `ac-lookup-global` is defined like so:

        (define (ac-lookup-global x) x)

    In other words, it returns its argument unchanged.

 3. When assigning to global variables, it should call `ac-assign-global-raw`
    rather than `set!`. In other words, this expression...

        (= foo "bar")

    ...would be compiled into this:

        (ac-assign-global-raw #<namespace:0> 'foo "bar")

    Notice that the variable name is quoted. The implementation may look
    something like this:

        (define (ac-set1 a b env)
          (if (lex? a env)
                `(set! ,a ,b)
              `(ac-assign-global-raw ,(ac-namespace) ',a ,b)))

    And then `ac-assign-global-raw` can be defined as follows:

        (define (ac-assign-global-raw space a b)
          (namespace-set-variable-value! a b #f space))

 4. If the value of `ac-namespace` is different from the value of
    `current-namespace`, global variables should be wrapped in
    `ac-lookup-global-raw` **in addition** to `ac-lookup-global`. In other
    words, this...

        (+ foo bar)

    ...would be compiled into this:

        (ac-funcall2 (ac-lookup-global (ac-lookup-global-raw #<namespace:0> '+))
                     (ac-lookup-global (ac-lookup-global-raw #<namespace:0> 'foo))
                     (ac-lookup-global (ac-lookup-global-raw #<namespace:0> 'bar)))

    Notice that the variable name is now quoted. The implementation may look
    something like this:

        (define (ac-global-name x)
          (if (eq? (ac-namespace)
                   (current-namespace))
            `(ac-lookup-global ,x)
            `(ac-lookup-global (ac-lookup-global-raw ,(ac-namespace) ',x))))

    `ac-lookup-global-raw` is expected to take two arguments: the namespace
    that the variable was compiled in, and a symbol. It's expected to return a
    value. That value will then be passed to `ac-lookup-global`.

    And because it only does this when `ac-namespace` differs from the
    `current-namespace`, you don't even need to define `ac-lookup-global-raw`:
    it can be defined later on in Arc, or in a library written in Arc.

Also, make sure that Arc code has a way of accessing `ac-namespace`,
`ac-lookup-global`, `ac-assign-global-raw`, and `ac-lookup-global-raw`. In
_Nu_ and _ar_ this happens automatically. In _Arc 3.1_ you would use `xdef`.

That's it! You can now implement any namespace system you want in Arc,
including namespace inheritance as described above. Really. And as you can
see, implementing these changes requires very little code to be added to the
compiler.


Explanation
===========

Okay, so how does this all work? Well, let's look at the above example of
`(+ foo bar)`. If I change the value of `ac-namespace`, the global variables
will be wrapped in `(ac-lookup-global-raw ...)`. I can then define
`ac-lookup-global-raw` to use my own custom namespace system to look up the
value of the symbol at runtime. And likewise for `ac-assign-global-raw`.
Voila, namespaces!

And what's `ac-lookup-global` for? I found when implementing things like
implicit parameters and aliases that it works out a *lot better* to dispatch
based on the *value* rather than the *symbol*. In other words, an alias might
be defined like so:

    (annotate 'alias (list get set))

And then you can change `ac-lookup-global` so when it sees something that's
annotated with type `'alias` it can do something special, like calling the
`get` function. This allows things like parameters, aliases, etc. to work
properly across namespaces.
