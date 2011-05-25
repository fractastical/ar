(implicit fail)

(mac isnt/fail (var val then . else)
  (w/uniq x
    `(w/fail (uniq)
       (let ,x ,val
         (if (is ,x fail)
           (do ,@else)
           (let ,var ,x
             ,then))))))


(def get-attribute (tab key (o default fail))
  (racket-hash-ref (rep tab) key default))

(def set-attribute (tab key (o value))
  (racket (racket-hash-set! (rep tab) key value))
  value)

(def del-attribute (tab key)
  (racket (racket-hash-remove! (rep tab) key))
  nil)


(defset get-attribute (x k)
  (w/uniq g
    (list (list g x)
          `(get-attribute ,x ',g)
          `(fn (val) (set-attribute ,g ,k val)))))


(def obj-attr (x n)
  (when (object? x)
    (w/fail (uniq)
      (let x (get-attribute x n)
        (and (isnt x fail)
             (or x t))))))

;(extend not (x) (is x fail) t)


(let orig type
  (def object? (x)
    (is (orig x) 'object)))

(extend type (x) (object? x)
  (isnt/fail x (get-attribute x 'type)
    x
    'table))

#|
(extend type (x) (object? x)
  (isnt/fail x (get-attribute x 'type)
    x
    (orig x)))
|#

(def maxstrlen (xs)
  (apply max (map [len:string _] xs)))

(def padding (x p)
  (newstring (- p (len:string x)) #\space))

(def str< (x y)
  (< (string x) (string y)))

(def pretty-print-table (name tab)
  (pr "(" name)
  (withs (k (sort str< (keys tab))
          s (1+ (len name))
          m (maxstrlen k))

    ((afn ((x . y))
       (pr " " x (padding x m) " " (tab x))
       (when y
         (prn)
         (pr:newstring s #\space)
         (self y)))
     k))
  (pr ")"))

#|(each x k
      (pr " ")
      (pr:string x (padding x m))
      (pr " ")
      (prn (tab x))
      (pr:newstring s #\space))|#

(def print-object (tab port)
  (w/stdout port
    (pretty-print-table "object" tab)))

#|  (each (k v) x
    (disp " " port)
    (disp k port)
    (disp " " port)
    (disp v port)
    (disp "\n       " port))|#

(extend print (p x port) (object? x)
  (isnt/fail x (get-attribute x 'print)
    (disp (x) port)
    (print-object x port)))

(extend len (x) (object? x)
  (isnt/fail len (get-attribute x 'len)
    (len)
    (isnt/fail keys (get-attribute x 'keys)
      (len (keys))
      (len (rep x)))))

(extend maptable (f tab) (object? tab)
  (let old fail
    (isnt/fail keys (get-attribute tab 'keys)
      (w/fail old
        (each k (keys)
          (f k (tab k)))
        tab)
      (orig f (rep tab)))))


#|(def ar-apply (x . args)
  (racket-apply x (ar-toracket args)))|#

#|(let orig ar-apply
  (def ar-apply (x . args)
    (orig orig x args)))

(extend ar-apply (x . args) (object? x)
  (isnt/fail call (get-attribute x 'call)
    (apply call args)
    (apply (rep x) args)))|#


(extend coerce (x type . r) (and (is type 'fn)
                                 (object? x))
  (isnt/fail x (get-attribute x 'call)
    x
    (fn (k (o d fail))
      (get-attribute x k d))))
    ;(orig (rep x) type)))

  #|
  (w/fail (uniq)
    (let call (get-attribute x 'call)
      (if (is call fail)
            (fn (k (o d fail))
              (get-attribute x k d))
            call))))
  |#

(extend sref (x v k) (object? x)
  (isnt/fail x (get-attribute x 'set)
    (x k v)
    (set-attribute x k v)))


(def del (x)
  ; unimplemented
  )


(mac object args
  (w/uniq x
    `(let ,x (table)
       ,@(map (fn ((k v))
                `(set-attribute ,x ',k ,v))
              (pair args))
       (annotate 'object ,x))))
