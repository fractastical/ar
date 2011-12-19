(def zip args
  ;; TODO: how fast is map applied to multiple lists?
  (apply map list args))


(mac rloop (name parms . body)
  ;; TODO: can this just use mappend rather than zip? should it?
  ;;       what about (map car parms)...?
  ;;       look at xloop in ar's arc.arc
  (let (l r) (apply zip pair.parms)
    #`((rfn name l ,@body) ,@r)))

(mac aloop (parms . body)
  #`(rloop 'self parms ,@body))


(mac afnlet (parms x . body)
  (w/uniq u
    #`((afn (u)
         (whenlet parms u
           ,@body))
       x)))


(def maplast (f xs)
  (if (no cdr.xs)
        (f car.xs)
      (do (f car.xs)
          (maplast f cdr.xs))))


(mac catcherr (expr)
  (w/uniq c
    #`(on-err (fn (c) (details c))
              (fn ()  expr nil))))

(def xml-encode (s)
  (multisubst '(("&" "&amp;")
                ("<" "&lt;")) s))


(def readlines args
  (drain:apply readline args))

(def pipe-lines (y)
  (w/pipe-from x y readlines.x))

(def dispfile (val file)
  ;; TODO: should probably use a temporary file
  (w/outfile o file (disp val o))
  val)


(mac extend (name parms test . body)
  (w/uniq u
    #`(let 'orig name
        (= name (fn u
                  ;; TODO: (apply (fn ,parms ...) ,u) ?
                  (let parms u
                    (aif test (do ,@body)
                              (apply 'orig u)))
                 )))))


(= defcall-types* (obj))

(mac defcall (name parms . body)
  #`(= (defcall-types* ',name) (fn parms ,@body)))

(extend ac-apply-non-fn (x . args)
  (orig defcall-types* (type x))
  (apply it x args))


(def listify (x)
  (if (cons? x) x (list x)))


;=============================================================================
;  %splice / %eval
;=============================================================================

#|(def ac-nocompile (x)
  (let x (let self ((x x))
           (mappend (fn (x)
                          ;; TODO: don't hardcode the symbol %compile
                      (if (caris x '%compile)
                            (ac-args (cdr x))
                          (cons? x)
                            (list (self x))
                          (list x)))
                     x))
    (if (no (cdr x))
          (car x)
        (cons 'racket-begin x))))|#

#|(nomac %nocompile args
  (if (cdr args)
        `(racket-begin ,@args)
      (car args)))|#

(mac %splice args
  #`(%nocompile (ac-splice . args)))

(mac %eval body
  (maplast eval body))


;=============================================================================
;  compat.arc
;=============================================================================

(alias acons cons?)
(alias alist list?)

(def call-w/stdout (port thunk)
  (parameterize (racket-current-output-port port) (thunk)))

(def call-w/stdin (port thunk)
  (parameterize (racket-current-input-port port) (thunk)))

(def tablist (h)
  (coerce x 'cons))

(def listtab (x)
  (coerce x 'table))


;=============================================================================
;  arubic.arc New additions
;=============================================================================

(alias pow   expt)
(alias str   string)
(alias str?  string?)

(def mv (old new)
  ;; only difference with mvfile is this one uses #f
  (racket-rename-file-or-directory old new #f)
  nil)

(mac curly-bracket args
  #`(obj ,@args))


(mac buildeach (name f)
  (w/uniq (args expr body)
    #`(mac name (args expr . body)
        #`(f (fn (args) . body) expr))))

;; TODO: w/let
;; TODO: probably don't need this let
(let mappend mappend
  (mac fnify args
    #`(do ,@(mappend (fn (x)
                       (let f (sym x 'fn)
                         #`((= f x)
                            (buildeach x f))))
                     args))))


#|
;; TODO: use buildeach
(mac maplet (var x . body)
  #`(map (fn (var) ,@body) x))|#

(buildeach maplet map)


;=============================================================================
;  arc.arc
;=============================================================================

(mac atomic body
  #`(atomic-invoke (fn () ,@body)))

(mac atlet args
  #`(atomic (let ,@args)))

(mac atwith args
  #`(atomic (with ,@args)))

(mac atwiths args
  #`(atomic (withs ,@args)))


(def queue () (list nil nil 0))

; Despite call to atomic, once had some sign this wasn't thread-safe.
; Keep an eye on it.

(def enq (obj q)
  (atomic
    (++ (q 2))
    (if (no (car q))
        (= (cadr q) (= (car q) (list obj)))
        (= (cdr (cadr q)) (list obj)
           (cadr q)       (cdr (cadr q))))
    (car q)))

(def deq (q)
  (atomic (unless (is (q 2) 0) (-- (q 2)))
          (pop (car q))))

; Should redef len to do this, and make queues lists annotated queue.

(def qlen (q) (q 2))

(def qlist (q) (car q))

(def enq-limit (val q (o limit 1000))
  (atomic
     (unless (< (qlen q) limit)
       (deq q))
     (enq val q)))


(def downcase (x)
  (let downc (fn (c)
               (let n (coerce c 'int)
                 (if (or (< 64 n 91) (< 191 n 215) (< 215 n 223))
                     (coerce (+ n 32) 'char)
                     c)))
    (case (type x)
      string (map downc x)
      char   (downc x)
      sym    (sym (map downc (coerce x 'string)))
             (err "can't downcase" x))))

(def upcase (x)
  (let upc (fn (c)
             (let n (coerce c 'int)
               (if (or (< 96 n 123) (< 223 n 247) (< 247 n 255))
                   (coerce (- n 32) 'char)
                   c)))
    (case (type x)
      string (map upc x)
      char   (upc x)
      sym    (sym (map upc (coerce x 'string)))
             (err "can't upcase" x))))


;; Used in news.arc and app.arc but not anywhere else
(= hooks* (obj))

(def hook (name . args)
  (aif (hooks* name) (apply it args)))

(mac defhook (name . rest)
  #`(= (hooks* ',name) (fn ,@rest)))
