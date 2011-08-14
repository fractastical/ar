(use arc defcall)


(mac w/normalize-cut (x start end . body)
  (w/uniq u
    `(let ,u (len ,x)
       (with (,end    (if (no ,end)    ,u
                          (< ,end 0)   (+ ,end ,u)
                                       ,end)
              ,start  (if (no ,start)  0
                          (< ,start 0) (+ ,start ,u)
                                       ,start))
         (when (< ,end ,start)
           (err "start index must be smaller than the end index"))
         ,@body))))


(def set-cut (x v start end)
  (w/normalize-cut x start end
    (let v (join v (nthcdr end x))
      (= x (if (is start 0)
                 (cons nil x)
               (nthcdr (1- start) x)))

      (if v (do (scar cdr.x car.v)
                (scdr cdr.x cdr.v))
            (is start 0)
              (err "cannot set the entire list to nil")
            (scdr x nil))))
  v)


(redef cut (x (o start) (o end) (o s))
  (w/normalize-cut x start end
    (if (isa x 'string) ;; TODO: use str?
          (let s2 (newstring (- end start))
            (for i 0 (- end start 1)
              (= s2.i (x (+ start i))))
            s2)

        (let x (firstn (- end start) (nthcdr start x))
          ;; TODO: write tests!
          (if s (if (is s 0) (err "step cannot be 0") ;; TODO: should return nil?
                    (< s 0)  (rev:map car (tuples x abs.s))
                             (map car (tuples x s)))
                x)))))


(let fail (uniq)
  (extend sref (x v l (o r fail)) (and (acons x) ;; TODO: use cons?
                                       (isnt r fail))
    (set-cut x v l r))

  ; replace with defcall later
  #|(extend coerce (x type . r) (and (is type 'fn)
                                   (cons? x))
    (fn (start (o end fail))
      ))|#
  (defcall cons (x start (o end fail) (o step))
    (if (is end fail)
          (do (when (< start 0)
                (++ start len.x))
              (racket-mlist-ref x start))
        (cut x start end step))))
