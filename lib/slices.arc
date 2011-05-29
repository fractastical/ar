(def set-cut (x v start end)
  #|(prn end)
  (prn x)
  (prn (nthcdr (+ end (len x)) x))|#
  ; (and end (< end 0))
  ;(prn start " " end)

  (let len (len x)
    (if (no end)    (=  end len)
        (< end 0)   (++ end len))

    (if (no start)  (=  start 0)
        (< start 0) (++ start len))

    ;(when (< start 0)
    ;  ;(= start (mod start len))
    ;  (++ start len)
    ;  (prn start)
    ;  (prn end)
      ;(-- end)
      ;(++ end)
      #|(when (< end start)
        (swap start end))|#
    ;  )
      )

  (when (< end start)
    (err "start index must be smaller than the end index"))
                   ;(nthcdr end x))

  ;(prn start " " end)
  #|(-- start)
  (zap nthcdr start (if (is start -1)
                          (cons nil x)
                          x))|#

  ;(when (is start 0)
    ;(= start (cons nil start)))

  ;(= start (nthcdr (max (1- start) 0) x))

  ;(if (is start 0)
;                   )

  #|(when (< end 0)
    (++ end (len x)))|#

  ;(prn x)
  ;(prn (cut x start end))
    ;(= end (+ (len x) end))

  ;(let start (nthcdr start x)
    ;(prs x v end)
    ;(prn)
  (let v (join v (nthcdr end x))
    ;(prn v)
    ;(scdr (lastcdr v) (nthcdr end start))
    ;(scdr (cdr start) (cdr v))
    (= x (if (is start 0)
               (cons nil x)
             (nthcdr (1- start) x)))

    ;(prn x)
    ;(prn start)
    ;(prn v)

    ;(scdr x v)

    (if v (do (scar (cdr x) (car v))
              (scdr (cdr x) (cdr v)))
          (is start 0) ;(no:car start)
            (err "cannot set the entire list to nil")
            (scdr x nil))
    ;(scar (car start) (car v))
    ;(scar start v)
    ;(scdr start v)
    )

  v);)


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

#|(if (is r fail) (orig x v l))|#
