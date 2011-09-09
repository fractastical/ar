(use arc)

(= del-rules* (table))

(mac defdel (name parms . body)
  `(= (del-rules* ',name) (fn ,parms ,@body)))

(let fail (uniq)
  (def dref (x l (o r fail) (o s fail))
    ;(prn x)
    (case (type x)
      table (do (ail-code (racket-hash-remove! x l))
                x)
      cons  (do (if (and (is r fail)
                         (is s fail))
                    (= (x l (1+ l)) nil)
                    (= (x l r s) nil))
                #|(join (cut x 0 l)
                      (cut x (or r (1+ l))))|#
                x)
            (err "cannot delete" x))))

(mac del (x)
  (when ac-ssyntax.x
    (zap ac-expand-ssyntax x))

  (if #|(ac-ssyntax x)
        `(del ,(ac-expand-ssyntax x))|#
      (cons? x)
        `(do1 ,x
              ,(iflet d (del-rules* (car x))
                 (apply d (cdr x))
                 `(zap dref ,@x)))
      (err "unimplemented")))
#|  (case (car x)
    get-attribute `(del-attribute ,@(cdr x))
                   (err "unimplemented")))|#

(defdel car (x)
  `(zap cdr ,x))

(defdel cdr (x)
  `(scdr ,x nil))
