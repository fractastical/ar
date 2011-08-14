(use arc defcall re strings)

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
  (redef sym args
    (coerce (apply string args) 'sym))

  (use utils)

  (extend len (x) (sym? x)
    (orig:string x))

  (defcall sym (x y)
    (sym (string.x y)))

  (extend print (primitive x port) (sym? x)
    (let s string.x
      ;; TODO: write unit tests for this
      ;; TODO: better regexp literal support...
      ;; http://docs.racket-lang.org/reference/printing.html?q=symbol&q=identifier#%28part._print-symbol%29
      (if (re-match "^[0-9]+\\.?[0-9]+$|[ \\(\\)\\[\\]\\{\\}\",'`;\\\\]" s)
            (primitive s port)
          (primitive x port))))

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
  (if (cons? xs)
        (cons (f car.xs) (map1 f cdr.xs))
      xs))

(extend prn args (cdr args)
  (apply orig (intersperse " " args)))

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

(defrule print (isa x 'table)
  (w/indent-level (+ indent-level 1)
    (disp "#hash(" port)
    (printwith-table primitive x (keys x) port) ; (sort < (keys x))
    (disp ")" port)))
