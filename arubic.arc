(use import)

;(= namespace (new-namespace namespace))

(zap new-namespace namespace)

;(debug "hiya!" racket-hash-ref loaded*)

(use utils re strings) ;ssyntax

;(debug defcall rep.defcall)

;(require scheme/package)

#|(ail-code
  (racket-module foo scheme/base
    (racket-provide bar)
    (racket-define (bar) 5)))|#

#|(ail-code
  (racket-define-package foo (bar)
    (def bar ()
      "secret bar!")
    (def qux ()
      "secret qux!")))|#

(let string string
  ;(use utils)

  (extend len (x) (sym? x)
    (orig:string x))

  (defcall sym (x y)
    (sym (string.x y)))

  (extend print (primitive x port) sym?.x
    (let s string.x
      ;; TODO: write unit tests for this
      ;; TODO: better regexp literal support...
      ;; http://docs.racket-lang.org/reference/printing.html?q=symbol&q=identifier#%28part._print-symbol%29
      (if (re-match "^[0-9]+\\.?[0-9]+$|[ \\(\\)\\[\\]\\{\\}\",'`;\\\\]" s)
            (primitive s port)
          (primitive x port))))

  #|(extend print (primitive x port) ar-tagged.x ;(ar-tnil:racket-vector? x)
    ;(racket-vector->list x)
    (disp (annotate type.x (ar-toarc rep.x)) port)
    )|#

  #|(extend coerce (x type . r) (sym? x)
    (if (is type 'sym) ; 'string 'char)
          x
        (apply orig string.x type r)))|#
)

#|(extend ac (x env) (isa x 'string 'char)
  `(racket-quote ,(sym x)))|#

#|(extend ac (x env) (isa x 'char)
  `(racket-quote ,(sym x)))|#

;(extend car (x) (sym? x) nil)
;(extend cdr (x) (sym? x) nil)
;(extend map1 (f x) (sym? x) (disp "map1 ") (disp x) x)

(redef map1 (f xs)
  (if cons?.xs
        (cons (f car.xs) (map1 f cdr.xs))
      xs))



#|
;; TODO: a bit odd, should automate
(=         mapfn      map
           somefn     some
           mappendfn  mappend
           keepfn     keep)

(buildeach map        mapfn)
(buildeach mappend    mappendfn)
(buildeach some       somefn)
(buildeach keep       keepfn)|#

(mac buildeach (name f)
  (w/uniq args
    `(remac ,name ,args
       ;; TODO: clunky, shouldn't use car, cadr, and cddr
       `(,,f (fn (,(car ,args)) ,@(cddr ,args)) ,(cadr ,args)))))

(mac fnify args
  `(do ,@(mappend (fn (x)
                    (let f (sym x 'fn)
                      `((= ,f ,x)
                        (buildeach ,x ,f))))
                  args)))

(fnify map mappend some all keep)

;(prn extend)

(extend prn args cdr.args
  (apply orig (intersperse " " args)))


;(extend car (x) str?.x (prn "foo!" x) (x 0))
;(extend cdr (x) str?.x (cut x 1))
;(extend is (x y) (and (orig x "") (orig x nil)) t)

#|(mac square-bracket (x . args)
  (if (cons? x)
        `(fn ((o _)) ,x ,@args)
      `(fn ((o _)) (,x ,@args))))|#

#|(remac square-bracket (x . args)
  `(fn ((o _)) (,x ,@args)))|#

(remac square-bracket args
  `(list ,@args))

(implicit indent-level 0)

(def printwith-table (primitive x keys port)
  (whenlet n (car keys)
    (disp "(" port)
    (print primitive n port)
    (disp " . " port)
    (print primitive (x n) port)
    (disp ")" port)
    (when (cdr keys)
      (disp (sym "\n" (* '|      | indent-level)) port)
      (printwith-table primitive x (cdr keys) port))))

;; should move `keys` and `sort` into base.arc

;(debug "hiya!")

(defrule print (isa x 'table)
  (w/indent-level (+ indent-level 1)
    (disp "#hash(" port)
    (printwith-table primitive x (keys x) port) ; (sort < (keys x))
    (disp ")" port)))

;(debug "hiya2!")
