(assign do (annotate 'mac
             (fn args `((fn () ,@args)))))

(assign safeset
  (annotate 'mac
    (fn (var val)
      `(do (if (bound ',var)
               (do (racket-disp "*** redefining " (racket-stderr))
                   (racket-disp ',var (racket-stderr))
                   (racket-disp #\newline (racket-stderr))))
           (assign ,var ,val)))))

(assign assign-fn
  (annotate 'mac
    (fn (name signature func)
      `(do (sref sig ',signature ',name)
           (safeset ,name ,func)))))

(assign def
  (annotate 'mac
    (fn (name parms . body)
      `(assign-fn ,name ,parms (fn ,parms ,@body)))))

(assign mac (annotate 'mac
              (fn (name parms . body)
                `(do (sref sig ',parms ',name)
                     (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))

(def no (x) (is x nil))

(def caar (xs) (car (car xs)))
(def cadr (xs) (car (cdr xs)))
(def cddr (xs) (cdr (cdr xs)))

(assign-fn pair (xs (o f list))
  (fn args
    ((fn (xs f)
       (if (no xs)
            nil
           (no (cdr xs))
            (list (list (car xs)))
            (cons (f (car xs) (cadr xs))
                  (pair (cddr xs) f))))
     (car args)
     (if (cdr args) (cadr args) list))))

(mac with (parms . body)
 `((fn ,(map1 car (pair parms))
    ,@body)
   ,@(map1 cadr (pair parms))))

(mac let (var val . body)
  `(with (,var ,val) ,@body))
