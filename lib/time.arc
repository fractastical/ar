(import strings)

#|
;; This is to make the code work in Arc 3.1, ar, and Nu.
;; Yes, it's using variable sniffing, but it's just a quick
;; hacky script, so that's okay
(if (bound 'ac-assign)
      (= gc-msec      racket-current-gc-milliseconds
         process-msec racket-current-process-milliseconds
         memory       racket-current-memory-use
         inexact      racket-exact->inexact)
      (= gc-msec      current-gc-milliseconds
         process-msec current-process-milliseconds
         inexact      num))|#

;; Originally by waterhouse (http://arclanguage.org/item?id=14186)
;; Edited to return a list rather than printing the numbers directly
(mac utime body
  (w/uniq (gtime ggc gmem)
    #`(with (gtime  (msec)
             ggc    (current-gc-milliseconds)
             gmem   (memory))
        ,@body
        (list (- (msec)                    gtime)
              (- (current-gc-milliseconds) ggc)
              (- (memory)                  gmem)))))


(mac timeit1 (x limit)
  (let x (ac-compile x)
    (w/uniq (u n time gc mem)
      #`(with (n     0
               time  (msec)
               gc    (current-gc-milliseconds)
               mem   (memory))
          (rwith u ()
            (% x)
            (++ n)
            (when (< (- (msec) time) limit)
              (u)))
          (list n
                (- (current-gc-milliseconds) gc)
                (- (memory)                  mem))))))


(mac timeit (x (o limit 10000))
  (w/uniq (a b c)
    #`(let (a b c) (timeit1 x limit)
        (prn "iter: "   (commafy a)
             "  gc: "   b
             "  mem: "  c)
        nil)))


;; TODO: there should be maor functions, less macros!
(mac timeit-range (x time (o variance 5))
  (w/uniq (a b c)
    #`(let (a b c) (timeit1 x 1000)
        (if (< a (* (- 100 variance) 0.01 time))
              (err:string a " is lower than expected: " time)
            (> a (* (+ 1 (* variance 0.01)) time))
              (err:string a " is higher than expected: " time)
            t))))

#|(mac make-timeit-range (x (o variance 1.25)) ;1.25 ;1
  (w/uniq (a b c self time iter total)
    #`(rwith self (time   0
                   iter   0
                   total  0)
        (let (a b c) (timeit1 x 10)
          (if (< a (* (- 100 variance) 0.01 time))
                (do (prn a " is lower than expected: " (racket-exact->inexact time))
                    (self (- time (/ (- time a) 2)) 0 (1+ total)))
              (> a (* (+ 1 (* variance 0.01)) time))
                (do (prn a " is higher than expected: " (racket-exact->inexact time))
                    (self (+ time (/ (- a time) 2)) 0 (1+ total)))
              (and (> total 100)
                   ;(> iter 5)
                   )
                (round time)
              (do (prn "iteration " iter " with time " a)
                  (self time (1+ iter) (1+ total))))))))|#

#|(mac make-timeit-range (x)
  (w/uniq (a b c self time total)
    #`(rwith self (time   0
                   total  0)
        (let (a b c) (timeit1 x 100)
          (if (> total 50)
                (round time)
              (< a time)
                (do (prn a " is lower than expected: " (racket-exact->inexact time))
                    (self (- time (/ (- time a) 2)) (1+ total)))
              (> a time)
                (do (prn a " is higher than expected: " (racket-exact->inexact time))
                    (self (+ time (/ (- a time) 2)) (1+ total)))
              (do (prn "WTF it's the same value?! " a)
                  (self time (1+ total))))))))|#

(mac make-timeit-range (x)
  #`(car (map median (apply zip (n-of 50 (timeit1 x 100))))))

;> (make-timeit-range (+ 1 2))
;165429
;176597
;176906
;168631
;168050
;175684
;174515
;167000
;175153
;190004
;179288
;176339
;181344
;172001
;173521
;172756
;186160

;166813
;182045
;160138
;171480
;184352
;184643
;183815
;173618
;182776
;182603
;185556
;184789

#|
> (make-timeit-range (let a 5 a))
354250
375965
369905
373764
378538
346919
364486
351271
366158
347915
350465
370287
357909
355496
356007
366106
367947
368371
373325
347352
363734
364107
344660
374035
372649
368426
381267
377858
352397
359613
359915
362979
355607

|#
