(def set-cut (x v start end)
  (let len (len x)
    (if (no end)    (=  end len)
        (< end 0)   (++ end len))

    (if (no start)  (=  start 0)
        (< start 0) (++ start len)))

  (when (< end start)
    (err "start index must be smaller than the end index"))

  (let v (join v (nthcdr end x))
    (= x (if (is start 0)
               (cons nil x)
             (nthcdr (1- start) x)))

    (if v (do (scar (cdr x) (car v))
              (scdr (cdr x) (cdr v)))
          (is start 0)
            (err "cannot set the entire list to nil")
            (scdr x nil)))
  v)


(def cut (x (o start) (o end))
  (let len (len x)
    (if (no end)    (=  end len)
        (< end 0)   (++ end len))

    (if (no start)  (=  start 0)
        (< start 0) (++ start len)))

  (when (< end start)
    (err "start index must be smaller than the end index"))

  (if (isa x 'string)
      (let s2 (newstring (- end start))
        (for i 0 (- end start 1)
          (= (s2 i) (x (+ start i))))
        s2)
      (firstn (- end start) (nthcdr start x))))


(let fail (uniq)
  (extend sref (x v l (o r fail)) (and (cons? x)
                                       (isnt r fail))
    (set-cut x v l r))

  ; replace with defcall later
  (extend coerce (x type . r) (and (is type 'fn)
                                   (acons x))
    (fn (start (o end fail))
      (if (is end fail) (do (when (< start 0)
                              (++ start (len x)))
                            (racket-mlist-ref x start))
                        (cut x start end)))))
