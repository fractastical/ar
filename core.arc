;=============================================================================
;  Core
;=============================================================================

(assign do (annotate 'mac
             (fn args `((fn () ,@args)))))

(assign safeset (annotate 'mac
                  (fn (var val)
                    `(do (if (bound ',var)
                             (do (disp "*** redefining " (stderr))
                                 (disp ',var (stderr))
                                 (disp #\newline (stderr))))
                         (assign ,var ,val)))))

(assign def (annotate 'mac
               (fn (name parms . body)
                 `(do (sref sig ',parms ',name)
                      (safeset ,name (fn ,parms ,@body))))))

(def no (x) (is x nil))

(def pair (xs (o f list))
  (if (no xs)
       nil
      (no (cdr xs))
       (list (list (car xs)))
      (cons (f (car xs) (cadr xs))
            (pair (cddr xs) f))))

(assign mac (annotate 'mac
              (fn (name parms . body)
                `(do (sref sig ',parms ',name)
                     (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))

(mac with (parms . body)
  `((fn ,(map1 car (pair parms))
     ,@body)
    ,@(map1 cadr (pair parms))))

(mac let (var val . body)
  `(with (,var ,val) ,@body))


;=============================================================================
;  Types
;=============================================================================

(def isa     (x y) (is (type x) y))
(def cons?   (x)   (isa x 'cons))
(def int?    (x)   (isa x 'int))
(def string? (x)   (isa x 'string))
(def sym?    (x)   (isa x 'sym))
(def char?   (x)   (isa x 'char))


(def string1 (x)
  (if (string? x)
        x
      (char? x)
        (racket-string x)
      (cons? x)
        (apply racket-string-append (map1 string x))
      (no x)
        ""
      (num? x)
        (racket-number->string x)
      (sym? x)
        (racket-symbol->string x)))

(def string args
  (apply racket-string-append (map1 string1 args)))


(def sym1 (x)
  (if (sym? x)
        x
      (char? x)
        (racket-string->symbol (racket-string x))
      (string? x)
        (racket-string->symbol x)))

(def sym args
  (sym1 (apply string args)))


;=============================================================================
;  Logical Booleans
;=============================================================================

(mac w/uniq (names . body)
  (if (cons? names)
        `(with ,(ac-mappend (fn (n) (list n '(uniq))) names)
           ,@body)
      `(let ,names (uniq) ,@body)))


(mac and args
  (if args
      (if (cdr args)
            `(if ,(car args) (and ,@(cdr args)))
          (car args))
      t))

(mac or args
  (and args
       (w/uniq g
         `(let ,g ,(car args)
            (if ,g ,g (or ,@(cdr args)))))))



(def num?    (x)   (or (int? x) (isa x 'num)))
(def list?   (x)   (or (no x)   (cons? x)))


;=============================================================================
;  Backwards Compatibility
;=============================================================================

;(alias acons cons?)
;(alias alist list?)


;=============================================================================
;  List functions
;=============================================================================

(def reduce (f xs)
  (if (cddr xs)
        (reduce f (cons (f (car xs) (cadr xs)) (cddr xs)))
      (apply f xs)))

(def rreduce (f xs)
  (if (cddr xs)
        (f (car xs) (rreduce f (cdr xs)))
      (apply f xs)))

; Rtm prefers to overload + to do this
(def join args
  (if (no args)
        nil
      (let a (car args)
        (if (no a)
              (apply join (cdr args))
            (cons (car a) (apply join (cdr a) (cdr args)))))))


;=============================================================================
;  Binary functions
;=============================================================================

#|(mac case-fn args
  `(%nocompile
     (racket-case-lambda
       ,@(map1 (fn (x)
                 (let n (car x)
                   `(,n (%compile ,@(if (sym? n)
                                          `((assign ,n (racket-list->mlist ,n))
                                            ,@(cdr x))
                                        (cdr x))))))
               args))))|#


(def +-2 (x y)
  (if (num? x)
        (racket-+ x y)
      (or (string? x)
          (char? x))
        (string x y)
      (list? x)
        (join x y)))

(def <2 (x y)
  (ac-tnil
    (if (and (num? x) (num? y))
          (racket-< x y)
        (and (string? x) (string? y))
          (racket-string<? x y)
        (and (sym? x) (sym? y))
          (racket-string<? (sym x) (sym y))
        (and (char? x) (char? y))
          (racket-char<? x y)
        (err "Can't <" x y))))

(def >2 (x y)
  (ar-tnil
    (if (and (num? x) (num? y))
          (racket-> x y))
        (and (string? x) (string? y))
          (racket-string>? x y)
        (and (sym? x) (sym? y))
          (racket-string>? (string x) (string y))
        (and (char? x) (char? y))
          (racket-char>? x y)
        (err "Can't >" x y)))

(assign +  (ac-binary +-2 reduce))
(assign <  (ac-binary  <2 ac-pairwise))
(assign >  (ac-binary  >2 ac-pairwise))
(assign is (ac-binary is2 ac-pairwise))


;=============================================================================
;  Hash tables
;=============================================================================

(def table ((o init))
  (let x (racket-make-hash)
    (if init (init x))
    x))
