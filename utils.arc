(use arc)

;=============================================================================
;  Should be in strings.arc
;=============================================================================

(def splitmatch1 (pat seq)
  ;; TODO: clunky
  (let x string.seq
    (iflet pivot (posmatch pat x)
            ;; TODO: clunky
      (list (coerce (cut x 0 pivot)           type.seq)
            (coerce (cut x (+ len.pat pivot)) type.seq)))))

;; TODO: should be faster
;; TODO: should handle symbols as well as strings
;; TODO: clunky
(def splitmatch (pat seq)
  (let l len.pat
    ((afn (x)
       (let pivot (posmatch pat x)
         (cons (cut x 0 pivot)
               (when pivot
                 (self:cut x (+ l pivot))))))
     seq)))

#|(def splitmatch (pat seq)
  (let l len.pat
    ((afn (x)
       (let pivot (posmatch pat x)
         (prn x pivot)
         (= pivot (split x pivot))
         (zap self cadr.pivot)
         pivot))
     seq)))|#

;; TODO: tests
;;
;; > (splitmatch1 "foo" "barfoobarfoobar")
;; ("bar" "barfoobar")
;;
;; > (splitmatch1 "foo" "barquxcorge")
;; nil
;;
;; > (splitmatch "foo" "barfoobarfoobar")
;; ("bar" "bar" "bar")
;;
;; > (splitmatch "foo" "barufooquxufoocorgeu")
;; ("baru" "quxu" "corgeu")
;;
;; > (splitmatch "foo" "barufoofoofooquxufoocorgeu")
;; ("baru" "" "" "quxu" "corgeu")
;; ?????


;=============================================================================
;  Should be in arc.arc
;=============================================================================

;; TODO: funny, but somewhat inaccurate name
;;       donil sounds awkward
(mac dont body
  `(do ,@body nil))

(mac retif (name x)
  `(iflet ,name ,x
     ,name
     ,name))

#|
;; TODO: haha, not needed
(def nthcar (n xs)
  (car (nthcdr n xs)))|#

(mac letr (var expr . body)
  `(let ,var nil
     (= ,var ,expr)
     ,@body))

#|(buildeach mapeach  map)
(buildeach mappeach mappend)
(buildeach someach  some)
(buildeach keepeach keep)|#

#|(mac mapeach (var expr . body)
  `(map (fn (,var) ,@body) ,expr))

(mac mappeach (var expr . body)
  `(mappend (fn (,var) ,@body) ,expr))

(mac someach (var expr . body)
  `(some (fn (,var) ,@body) ,expr))|#


(def carref (x y)
  (and cons?.y (x car.y)))


(mac collect (x)
  `(accum yield ,x))


(mac ^ args
  `(fn ((o _)) ,@args))


;; TODO: this macro definition doesn't work
#|(mac isa (x . args)
  `(in (type ,x) ,@args))|#

#|(def isa (x . args)
  (some x args))|#


#| TODO: figure out why this is here
(redef empty (x)
  (errsafe:is len.x 0))|#


;; TODO: rewrite code to use dopair
(mac dopair (var args . body)
  (w/uniq u
    `(let ,u (mapeach ,var (pair ,args) ,@body)
       (if (cdr ,u)
             `(do ,@,u)
           (car ,u)))))


#|
;; TODO: throws an error
(mac var args
  (dopair (x y) args
    (if (sym? x)
          `(ail-code (racket-define ,x ,y))
        (err "cannot create variable" x))))|#

(mac var args
  `(ail-code
     (racket-begin
       ,@(mapeach (x y) pair.args
           (if (sym? x)
                 `(racket-define ,x ,y)
               (err "cannot create variable" x))))))


;; TODO: inefficient
(def zip args
  ;; TODO: this causes it to stop when all the lists are empty.
  ;;       however, in Python, it stops when the first list is
  ;;       empty. I'm not sure which is the better semantic.
  ;;       to get the Python behavior, just change `all` to `some`
  (if (all no args)
        nil
      (cons (apply list (map car args))
            (apply zip  (map cdr args)))))


(def flat1 args
  (apply + nil args))

(def flatzip args
  (apply flat1 (apply zip args)))


(def cdddr (x)
  (cdr:cddr x))

(mac require (x)
  `(ail-code (racket-require (racket-prefix-in racket- ,x))))


(def 1+ (x)
  (+ 1 x))

(def 1- (x)
  (- x 1))


;; TODO: is there a better name than imac?
(mac imac (parms . body)
  `(annotate 'mac (fn ,parms ,@body)))


(def splitlast (x)
  (split x (- (len x) 1)))


(def list* args
  (if (no args)
        nil
      (cddr args)
        (cons car.args (apply list* cdr.args))
      (cons car.args cadr.args)))


;; TODO: I think macex1 and macex can be defined better
(def macex1 (x)
  (macex x t))


(def lastcdr (x)
  (nthcdr (- (len x) 1) x))

;; TODO: should use (defset last ...)
(mac setlast (x y)
  `(whenlet it (lastcdr ,x)
     (scar it ,y)))
     ;(= (car it) ,y))


(mac zap2 (x y var . args)
  `(= ,var (,x ,y ,var ,@args)))
;  `(zap (^:apply ,x ,y _ ,args) ,var))

(def none (x y)
  (no:some x y))

(extend * (x . args) (isa x 'sym 'string) ;(or (sym? x) (isa x 'string)) ;(str? x)
  (apply sym (n-of (apply + args) x)))

(mac and= args
  `(do ,@(mapeach (x y) (pair args)
           `(and ,x (= ,x ,y)))))

(mac afneach (parms x . body)
  (w/uniq u
    `((afn (,u)
        (whenlet ,parms ,u
          ,@body))
      ,x)))

(mac w/ccc (x . body)
  `(ccc (fn (,x) ,@body)))

;; TODO: should this return a function or quasiquoted code?
(def thunk (x)
  `(fn () ,x))

(def square (x) (expt x 2))

(def readlines (x)
  (drain:readline x))


(def floor (x)
  (racket-floor x))

(def ceil (x)
  (racket-ceiling x))


;=============================================================================
;  Should be in binary.arc
;=============================================================================

(def bitand args
  (apply racket-bitwise-and args))


;=============================================================================
;  Should be in fninfo.arc
;=============================================================================

(def fninfo (x)
  (cons (name x)
        (cons (fn-args x)
              (fn-body x))))


;=============================================================================
;  Should be somewhere else (not sure where)
;=============================================================================

(def fn-iferr (trial then else)
  (let (had-err val) (on-err (^:list t _)
                             (^:list nil (trial)))
    (.val:if had-err then else)))

(mac aiferr (x y . body)
  `(fn-iferr (fn ()   ,x)
             (fn (it) ,y)
             (fn (it) ,@body)))
