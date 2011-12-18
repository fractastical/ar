Hygienic macros
===============

_ar_ made a rather interesting change: it is possible to place a
function/macro *value* into a quasiquote. In other words, rather than writing
this:

    (mac unless (test . body)
      `(if (no ,test) (do ,@body)))

You can instead write this, which has the same effect:

    (mac unless (test . body)
      `(,if (,no ,test) (,do ,@body)))

Why would you want to do this? Two reasons:

 1. It's faster. Because you're splicing the value in, the compiler no longer
    has to do a global lookup. In addition, the Nu compiler will optimize code
    that inserts function values. Basically, rather than doing a global lookup
    every time, you instead do it once (when the macro is expanded), and then
    reuse that value.

 2. More importantly, it makes macros hygienic. Keep in mind the reason for
    bad hygiene with macros: name collision. Let's look at the macro above.
    Most of the time, it'll work fine. But if I do this...

        (let no (fn (x) nil)
          (unless foo
            ...))

    ...then the macro will be using the local definition of `no` rather than
    the global one. It's possible to work around this by using gensyms, but
    that's impractical in this case. Instead, all you have to do is unquote
    the functions and macros inside quasiquote: now they will always use the
    global definition, and the above example will work fine. That's all that's
    needed to make macros hygienic.

    What's interesting about this technique is that it's *extremely super
    easy* to implement: no big complicated hygiene systems. It also makes it
    really easy for the person writing the macro to specify whether they want
    hygiene or not: if you unquote the value, it's hygienic. If you don't
    unquote it, it's not. No need for a separate macro system!

    And this works perfectly with Nu namespaces as well: if you unquote a
    value, it'll refer to the name in the namespace where the macro is
    defined. If you don't unquote it, it'll use the value in the current
    namespace.

    Unfortunately, it's clunky and ugly to have to unquote almost everything,
    so Nu provides a special syntax which is similar to quasiquote, but rather
    than quoting things by default, it unquotes them. Thus the above macro
    could be written as:

        (mac unless (test . body)
          #`(if (no test) (do . body)))

    You can still use the old quasiquote syntax if you like, this new syntax
    is just a convenience to make writing hygienic macros easier.


Quasisyntax
===========

Nu provides a convenient syntax for writing hygienic macros: `quasisyntax`.
Compare the following two macro definitions:

    ;; quasisyntax
    (mac complement (f)
      (w/uniq g
        #`(fn g (no (apply f g)))))

    ;; quasiquote
    (mac complement (f)
      (w/uniq g
        `(,fn ,g (,no (,apply ,f ,g)))))

As you can see, with `quasisyntax`, you no longer need to `unquote`
everything: the default is to unquote. But you can still create anaphoric
macros by quoting the symbol:

    ;; quasisyntax
    (mac afn (parms . body)
      #`(rfn 'self parms . body))

    ;; quasiquote
    (mac afn (parms . body)
      `(,rfn self ,parms ,@body))

And you can include `(quote ...)` by doubling the quotes:

    ;; quasisyntax
    (mac mac (name parms . body)
      #`(do (sref sig ',parms ',name)
            (safeset name (annotate ''mac (fn parms . body)))))

    ;; quasiquote
    (mac mac (name parms . body)
      `(,do (,sref ,sig ',parms ',name)
            (,safeset ,name (,annotate 'mac (,fn ,parms ,@body)))))


Converting quasiquote into quasisyntax
======================================

It is usually very easy and even mechanical to convert existing Arc macros to
be hygienic. Let's transform this macro:

    (mac aif (expr . body)
      `(let it ,expr
         (if it
             ,@(if (cddr body)
                     `(,(car body) (aif ,@(cdr body)))
                   body))))

First, replace all instances of \` with #\`. Now, look at every symbol. If the
symbol is unquoted, remove the unquote. If the symbol is anaphoric, add a
quote. If the symbol is already quoted, add another quote. Otherwise, leave
the symbol exactly as-is. That's it! Let's look at what the above macro looks
like when following these rules:

    (mac aif (expr . body)
      #`(let 'it expr
          (if 'it
              ,@(if (cddr body)
                      #`(,(car body) (aif ,@(cdr body)))
                    body))))

Even nested quasiquotes can be transformed into nested quasisyntaxes. As an
example, I translated these macros:

    (mac make-w/ (name)
      `(mac ,(sym "w/" name) (val . body)
         `(parameterize (,',name ,val) ,@body)))

    (mac buildeach (name f)
      (w/uniq (args expr body)
        `(remac ,name (,args ,expr . ,body)
           `(,',f (fn (,,args) ,@,body) ,,expr))))

Into these:

    (mac make-w/ (param (o name param))
      (w/uniq (val body)
        #`(mac ,(sym "w/" name) (val . body)
            #`(parameterize (,',param val) . body))))

    (mac buildeach (name f)
      (w/uniq (args expr body)
        #`(remac name (args expr . body)
            #`(f (fn (args) . body) expr))))
