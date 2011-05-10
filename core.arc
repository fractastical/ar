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
