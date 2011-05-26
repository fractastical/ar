(each (name signature)
      (pair
       '(-                 args
         /                 args
         *                 args
         <2                (x y)
         <                 args
         >2                (x y)
         >                 args
         +                 args
         annotate          (totype rep)
         ar-apply          (fn . racket-arg-list)
         ar-apply-cons     (fn . racket-arg-list)
         ar-apply-table    (fn . racket-arg-list)
         ar-apply-string   (fn . racket-arg-list)
         ar-deep-fromarc   (x)
         ar-exint          (x)
         ar-funcall0       (fn)
         ar-funcall1       (fn arg1)
         ar-funcall2       (fn arg1 arg2)
         ar-funcall3       (fn arg1 arg2 arg3)
         ar-funcall4       (fn arg1 arg2 arg3 arg4)
         ar-iround         (x)
         ar-list-fromarc   (x)
         ar-no             (x)
         ar-pairwise       (pred lst)
         ar-r/list-toarc   (x)
         ar-tagged         (x)
         ar-tnil           (x)
         ar-toarc          (x)
         ar-true           (x)
         car               (x)
         cadr              (x)
         caris             (x val)
         cdr               (x)
         cddr              (x)
         coerce            (x totype . args)
         cons              (a b)
         err               args
         inside            (s)
         instring          (str)
         is                args
         is2               (a b)
         join              args
         len               x
         list              args
         list-len          x
         map1              (f xs)
         outstring         ()
         peekc             ((o port stdin))
         racket-module-ref (a/module)
         readc             ((o port stdin) (o eof nil))
         rep               (x)
         type              (x)
         writec            (c (o port stdout))
         uniq              ()))
  (= sig.name signature))
