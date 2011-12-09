Quasiquote
==========

Nu uses a quasiquote algorithm which results in much shorter (and faster!) code:

    > (qq-expand '`(foo (bar nou) ,@qux ,corge))

    ;; Nu
    (cons (quote cons) (cons (cons (quote quote) (cons (quote foo) nil)) (cons (cons (quote cons) (cons (cons (quote cons) (cons (cons (quote quote) (cons (quote bar) nil)) (cons (cons (quote cons) (cons (cons (quote quote) (cons (quote nou) nil)) (cons (quote nil) nil))) nil))) (cons (cons (quote join) (cons (quote qux) (cons (cons (quote cons) (cons (quote corge) (cons (quote nil) nil))) nil))) nil))) nil)))

    ;; ar
    (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (foo)) (quote nil))) (quote nil)))) (join (list (join (quote (join)) (join (list (join (quote (list)) (join (list (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (bar)) (quote nil))) (quote nil)))) (join (list (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (nou)) (quote nil))) (quote nil)))) (join (list (join (quote (quote)) (join (quote (nil)) (quote nil)))) (quote nil))))) (quote nil))))) (quote nil)))) (join (list (join (quote (join)) (join (quote (qux)) (join (list (join (quote (join)) (join (list (join (quote (list)) (join (quote (corge)) (quote nil)))) (join (list (join (quote (quote)) (join (quote (nil)) (quote nil)))) (quote nil))))) (quote nil))))) (quote nil))))) (quote nil))))


This algorithm works even with nested quasiquotes:

    > (qq-expand '`(foo `(bar nou) ,@qux ,corge))

    ;; Nu
    (cons (quote cons) (cons (cons (quote quote) (cons (quote foo) nil)) (cons (cons (quote cons) (cons (cons (quote cons) (cons (cons (quote quote) (cons (quote cons) nil)) (cons (cons (quote cons) (cons (cons (quote cons) (cons (cons (quote quote) (cons (quote quote) nil)) (cons (cons (quote cons) (cons (cons (quote quote) (cons (quote bar) nil)) (cons (quote nil) nil))) nil))) (cons (cons (quote cons) (cons (cons (quote cons) (cons (cons (quote quote) (cons (quote cons) nil)) (cons (cons (quote cons) (cons (cons (quote cons) (cons (cons (quote quote) (cons (quote quote) nil)) (cons (cons (quote cons) (cons (cons (quote quote) (cons (quote nou) nil)) (cons (quote nil) nil))) nil))) (cons (cons (quote cons) (cons (cons (quote quote) (cons (quote nil) nil)) (cons (quote nil) nil))) nil))) nil))) (cons (quote nil) nil))) nil))) nil))) (cons (cons (quote join) (cons (quote qux) (cons (cons (quote cons) (cons (quote corge) (cons (quote nil) nil))) nil))) nil))) nil)))

    ;; ar
    (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (foo)) (quote nil))) (quote nil)))) (join (list (join (quote (join)) (join (list (join (quote (list)) (join (list (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (join)) (quote nil))) (quote nil)))) (join (list (join (quote (join)) (join (list (join (quote (list)) (join (list (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (quote)) (quote nil))) (quote nil)))) (join (list (join (quote (join)) (join (list (join (quote (list)) (join (list (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (bar)) (quote nil))) (quote nil)))) (join (list (join (quote (quote)) (join (quote (nil)) (quote nil)))) (quote nil))))) (quote nil)))) (join (list (join (quote (quote)) (join (quote (nil)) (quote nil)))) (quote nil))))) (quote nil))))) (quote nil)))) (join (list (join (quote (join)) (join (list (join (quote (list)) (join (list (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (join)) (quote nil))) (quote nil)))) (join (list (join (quote (join)) (join (list (join (quote (list)) (join (list (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (quote)) (quote nil))) (quote nil)))) (join (list (join (quote (join)) (join (list (join (quote (list)) (join (list (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (nou)) (quote nil))) (quote nil)))) (join (list (join (quote (quote)) (join (quote (nil)) (quote nil)))) (quote nil))))) (quote nil)))) (join (list (join (quote (quote)) (join (quote (nil)) (quote nil)))) (quote nil))))) (quote nil))))) (quote nil)))) (join (list (join (quote (join)) (join (list (join (quote (list)) (join (list (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (quote)) (quote nil))) (quote nil)))) (join (list (join (quote (join)) (join (list (join (quote (quote)) (join (list (join (quote (nil)) (quote nil))) (quote nil)))) (join (list (join (quote (quote)) (join (quote (nil)) (quote nil)))) (quote nil))))) (quote nil))))) (quote nil)))) (join (list (join (quote (quote)) (join (quote (nil)) (quote nil)))) (quote nil))))) (quote nil))))) (quote nil))))) (quote nil)))) (join (list (join (quote (quote)) (join (quote (nil)) (quote nil)))) (quote nil))))) (quote nil))))) (quote nil))))) (quote nil)))) (join (list (join (quote (join)) (join (quote (qux)) (join (list (join (quote (join)) (join (list (join (quote (list)) (join (quote (corge)) (quote nil)))) (join (list (join (quote (quote)) (join (quote (nil)) (quote nil)))) (quote nil))))) (quote nil))))) (quote nil))))) (quote nil))))


Quasisyntax
===========

Nu also provides a convenient syntax for writing hygienic macros: `quasisyntax`. Compare the following two macro definitions:

    ;; quasisyntax
    (mac complement (f)
      (w/uniq g
        #`(fn g (no (apply f g)))))

    ;; quasiquote
    (mac complement (f)
      (w/uniq g
        `(,fn ,g (,no (,apply ,f ,g)))))

As you can see, with `quasisyntax`, you no longer need to `unquote` everything: the default is to unquote. But you can still create anaphoric macros by quoting the symbol:

    ;; quasisyntax
    (mac afn (parms . body)
      #`(rfn 'self parms ,@body))

    ;; quasiquote
    (mac afn (parms . body)
      `(,rfn self ,parms ,@body))

And you can include `(quote ...)` by doubling the quotes:

    ;; quasisyntax
    (mac mac (name parms . body)
      #`(do (sref sig ',parms ',name)
            (safeset name (annotate ''mac (fn parms ,@body)))))

    ;; quasiquote
    (mac mac (name parms . body)
      `(,do (,sref ,sig ',parms ',name)
            (,safeset ,name (,annotate 'mac (,fn ,parms ,@body)))))
