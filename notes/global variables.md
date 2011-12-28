In my attempts to properly implement parameters and aliases in a way that's
both correct and fast, I've discovered the following:

    (def foo () bar)

What should the symbol `bar` in the above expression be compiled into? I've
compiled a list of the different approaches I've found, ranked in order of
what I consider to be the best:

     >> (ac-lookup-global bar)                                              Nu
    + supports parameters and aliases
    + works if bar changes to a parameter or alias
    + works with multiple namespaces
    - slow

This is the old approach Nu used for a long time. It is trivial to implement
and also fully supports parameters and aliases, but unfortunately it's *very*
slow: about 100% slower than raw globals.

     >> (%eval (if (parameter? (ac-var 'bar)) `(bar) `bar))                N/A
    + supports parameters and aliases
    - if bar changes to a parameter or alias, code breaks
    + works with multiple namespaces
    + fast

This is a theoretical approach that (to the best of my knowledge) no Arc
implementation uses. I include it for completeness, and also note that it's
*very slightly* superior to ar's approach. I still consider it unusable due
to the lack of retroactivity.

     >> (%eval (if (ac-zeroarg* 'bar) `(bar) `bar))                         ar
    + supports parameters and aliases
    - if bar changes to a parameter or alias, code breaks
    - works with multiple namespaces
    + fast

This is the approach that ar uses. The only real upshot of it is that it
supports parameters and aliases, and does so with extremely good speed. But
those're the only benefits, as variables aren't retroactive, and they don't
work properly with multiple namespaces.

     >> bar                                                            Arc 3.1
    - no parameters or aliases
    - if bar changes to a parameter or alias, code breaks
    + works with multiple namespaces
    + fast

This is the approach that Arc 3.1 uses, which is to basically not support
parameters or aliases at all. It is naturally the easiest to implement, and
also extremely fast, but I consider it completely unusable due to the lack of
parameters and aliases.


Retroactivity
=============

What did I mean when I said ar's approach isn't retroactive? Consider this
piece of code:

    (= bar 5)
    (def foo () bar)

If you call `(foo)` it will correctly return `5`. But now let's change `bar`
to be a parameter:

    (implicit bar 5)

Now if you call `(foo)` it'll return the parameter `bar`, not the value `5`
like we wanted! The problem is that ar determines *at compile time* whether a
variable should be treated as a parameter or not. By the time we define `bar`
to be a parameter, it's already too late.

Retroactivity is *critical* for dynamic variables. You need to be able to
specify that a variable is dynamic *after* it has been created. That can only
work with retroactivity.

Effectively, what this means is that the check needs to be done at runtime,
which is exactly what Nu's `ac-lookup-global` did: it would check if its
argument was a parameter, and if so, call it. But this was very slow, so I
looked for an alternative that was faster.


Namespaces
==========

ar also made the unfortunate mistake of hardcoding *symbols* rather than
*values*, which means that ar parameters don't work properly across multiple
namespaces. Consider this example:

    ;; namespace 1
    (implicit foo 5)

    ;; namespace 2
    (= foo 10)

As you can see, in one namespace we're defining `foo` to be a parameter, but
in a different namespace we're defining `foo` to be an ordinary variable. But
the ar compiler can't tell the difference because it hardcoded the *symbol*
`'foo` which means that it'll treat both variables as parameters.

This can be fixed, but it essentially requires doing either *really* hacky
things, or overwriting/undoing the changes that ar made and implementing
parameters better.


What does Nu do, then?
======================

I mentioned that the *old Nu* used `ac-lookup-global`, but what does the
*new Nu* use? Basically, at random, I had an epiphany: what if I made all
global variables into Racket parameters?

Because they're *all* parameters, retroactivity is a non-issue because
it happens automatically, and it'd also have the neat side effect that you can
parameterize *any* global. And because I knew that `ac-lookup-global` was
slow, I figured that using parameters *might* be faster!

Well, turns out they weren't. They're a *very tiny smidgen bit* slower than
using `ac-lookup-global`, so that idea was out. But then I wondered... what if
I used ordinary functions as thunks? In other words...

     >> (bar)                                                               Nu
    + supports parameters and aliases
    + works if bar changes to a parameter or alias
    + works with multiple namespaces
    + fast

This is the current approach used by Nu. Basically, global variables are all
thunks that when called will extract their value. The compiler automatically
adds in the parens so this isn't noticable in Arc code. As you can see, it
is the best approach.

It fully supports parameters and aliases, and does so with *extreme speed*:
it's basically just as fast as Arc 3.1's approach, which is astonishingly
fast. But surely there must be *some downside*, right? Wellllll... there is
one...

It's difficult to implement in ar. And when I say difficult, I mean **really**
difficult, as in "don't even bother trying" difficult. The problem is that ar
is written in Arc's namespace. So if you try to change the representation of
global variables, it breaks things, because all of the changes that you're
making to Arc also affect the compiler.

Arc 3.1 and Nu don't suffer from this because the compiler is written in a
separate Racket module, so there's a clean separation: *this over here* is Arc
code, and *that over there* is Racket code. That clean separation not only
allows the compiler to run a *hell of a lot* (about 17 times) faster, but it
also makes it trivial to make these kinds of changes.

Okay, but wait, I thought the point of ar was that it's a good idea to put the
compiler into Arc's namespace, right? That way we can change the compiler from
within Arc. Why would I want to give that up?

Well, I don't. You see, I had tried several different approaches trying to
work around Racket's immutable modules, but no luck: I had to either choose
between (speed + cleanliness) xor (hackability + flexibility).

But I had a second epiphany and discovered a way that lets me get the same
mutability as ar, but with the speed and clean separation of Arc 3.1. Thus, I
finally discovered a way to bypass Racket's immutability, at least for the Arc
compiler.

Basically, how it works is that *every* global variable is represented as a
function. This function when called with *no arguments* is expected to return
a value. *But* when called with a *single argument*, it's expected to assign
to the value. This means that essentially every global variable behaves like a
Racket parameter (but far faster), and it also means we get aliases *for
free*.

Okay, and in the Nu compiler you use `set` to define a global variable in
Arc's namespace:

    (set 'foo (lambda () ...))

But what I can do is provide a different form, like `mdef`, which will
essentially make the Arc compiler mutable. Here's how it works:

    (mdef ac ac)

That will compile into this:

    (set 'ac (case-lambda
               (()  ac)
               ((x) (set! ac x))))

Okay, so now in the Arc namespace there's a global variable called `ac` which
when evaluated will return the value of `ac` *in the compiler*. And when you
assign to the variable `ac` in Arc, it will then set the value of `ac` *in the
compiler*. This works because [as explained in the Racket docs](http://pre.racket-lang.org/docs/html/guide/module-set.html),
it's possible to use `set!` inside of a module, but not outside of it. And the
`mdef` macro expands inside the module, so it's fine.

Thus, not only does this technique give us aliases for free, has perfect
retroactivity with parameters and aliases, works perfectly with multiple
namespaces, and is very fast... but it also allows us to make the compiler
mutable from within Arc, thereby gaining *all* of the benefits of ar, while
still maintaining the clean isolation and speed of Racket modules!

And in addition to all of that, it was trivial to add the technique once I got
the Nu compiler up and running (just 9 lines of code needed to be changed).
And it's a very simple and elegant idea as well: all global variables are
functions that can be called to get and set their values. That's it! Yet that
simple idea has such huge consequences.


How fast is it?
===============

I decided to run various tests to see how fast the different approaches are.
Here are my results. First, the control test:

    > (let a 5 a)
       arc3 eval: 5
    iter: 346,687  gc: 0  mem: 984  diff: 0%

         ar eval: 5
    iter: 347,988  gc: 0  mem: 984  diff: 0.38%

     old nu eval: 5
    iter: 347,461  gc: 0  mem: 1144  diff: 0.22%

     new nu eval: 5
    iter: 349,330  gc: 0  mem: 1144  diff: 0.76%

Good, good. My timing program gets very consistent results, as you can see.
Okay, now let's look at a simple function, `idfn`:

    > (idfn 'x)
       arc3 eval: x
    iter: 318,038  gc: 0  mem: 1792  diff: 0%

         ar eval: x
    iter: 314,168  gc: 0  mem: 1472  diff: -1.23%

     old nu eval: x
    iter: 189,093  gc: 0  mem: 832  diff: -68.19%

     new nu eval: x
    iter: 293,689  gc: 0  mem: 832  diff: -8.29%

This shows quite clearly how slow `ac-lookup-global` is: the old version of Nu
is 68.19% slower than Arc 3.1, *solely* because of `ac-lookup-global`! On the
other hand, the new Nu is only 8.29% slower, even though *all* Arc globals are
thunks. This is *extremely* good speed, given all the benefits of the
technique.

    > nil
       arc3 eval: nil
    iter: 350,597  gc: 0  mem: 984  diff: 0%

         ar eval: nil
    iter: 345,870  gc: 0  mem: 992  diff: -1.37%

     old nu eval: ()
    iter: 171,596  gc: 0  mem: 992  diff: -104.32%

     new nu eval: ()
    iter: 325,785  gc: 0  mem: 1632  diff: -7.62%

And again, the old Nu is terribly slow because of `ac-lookup-global`, and the
new Nu is only 7.62% slower: this is consistent with `idfn`.

    > (rev '(1 2 3 4 5))
       arc3 eval: (5 4 3 2 1 . nil)
    iter: 45,603  gc: 4  mem: 4302616  diff: 0%

         ar eval: {5 4 3 2 1 . nil}
    iter: 39,060  gc: 4  mem: 375928  diff: -16.75%

     old nu eval: (5 4 3 2 1)
    iter: 9,560  gc: 0  mem: 2066040  diff: -377.02%

     new nu eval: (5 4 3 2 1)
    iter: 48,211  gc: 0  mem: 10416768  diff: 5.72%

I use `rev` as a good timing benchmark because it's simple and calls 4 global
variables in a tight loop: and as you can see the old Nu takes a **huge** hit
because of that.

Meanwhile, even though it has to unwrap 4 global variables on every iteration
of the loop (21 unwraps within the loop, 34 unwraps total), the new version of
Nu is actually the *fastest*!

All four implementations used the *exact same* algorithm from arc.arc, so
there's no special fancy-pants sneaky stuff that Nu does that the rest don't.

One explanation for this discrepancy is that Nu uses [waterhouse's idea](https://sites.google.com/site/arclanguagewiki/arc-3_1/optimizations)
for implementing `+`, `<`, `>`, and `is` with `case-lambda` for the common
two-argument case.

This has similar benefits as using the `funcall*` functions: it doesn't need
to call `apply`, and it doesn't need to cons the argument list. And within
`rev`'s loop, it calls `no` which calls `is`. That does indeed explain why Nu
is faster than Arc 3.1.

In any case, this demonstrates that the thunk technique is *very comparable*
in speed to raw global variables; which is great news since it means Nu gets
all the nice shiny goodies *and* great speed along with it.

If you'd like to see all the timing tests, [go here](../timing). I frequently
run suites of tests to see what I can improve with Nu. If you go there, you'll
find some interesting bits of information, such as that Nu is actually faster
at destructuring than calling `car` yourself:

    > (let (a b) '(1 2) a)
       arc3 eval: 1
    iter: 287,511  gc: 0  mem: 8954072  diff: 0%

         ar eval: 1
    iter: 202,205  gc: 0  mem: -13059120  diff: -42.19%

     old nu eval: 1
    iter: 210,825  gc: 0  mem: 664  diff: -36.37%

     new nu eval: 1
    iter: 330,730  gc: 0  mem: 1624  diff: 15.03%



    > (let a '(1 2) (car a))
       arc3 eval: 1
    iter: 318,026  gc: 0  mem: 1792  diff: 0%

         ar eval: 1
    iter: 303,670  gc: 0  mem: 1632  diff: -4.73%

     old nu eval: 1
    iter: 174,756  gc: 0  mem: 512  diff: -81.98%

     new nu eval: 1
    iter: 287,469  gc: 0  mem: 1792  diff: -10.63%

Or that Nu's implementation of `+` is 44.12% faster than Arc 3.1's
implementation:

    > (+ 1 2)
       arc3 eval: 3
    iter: 182,952  gc: 0  mem: 11275936  diff: 0%

         ar eval: 3
    iter: 175,642  gc: 0  mem: 10604320  diff: -4.16%

     old nu eval: 3
    iter: 179,321  gc: 0  mem: 1472  diff: -2.02%

     new nu eval: 3
    iter: 263,679  gc: 0  mem: 1792  diff: 44.12%

And for fun, let's see Nu's implementation of `join`:

    > (join '(1 2) '(3 4))
       arc3 eval: (1 2 3 4 . nil)
    iter: 17,913  gc: 4  mem: 8009360  diff: 0%

         ar eval: {1 2 3 4 . nil}
    iter: 102,897  gc: 4  mem: -2041584  diff: 474.43%

     old nu eval: (1 2 3 4)
    iter: 122,256  gc: 4  mem: -7493776  diff: 582.5%

     new nu eval: (1 2 3 4)
    iter: 192,203  gc: 0  mem: -10585776  diff: 972.98%

That's just picking on Arc 3.1, though, because in Nu and ar, `join` is
implemented with Racket's `append`, but in Arc 3.1 it's defined in arc.arc.
