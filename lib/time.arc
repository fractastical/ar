(import strings)

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
         inexact      num))

;; Originally by waterhouse (http://arclanguage.org/item?id=14186)
;; Edited to return a list rather than printing the numbers directly
(mac utime body
  (w/uniq (gtime ggc gmem)
    `(,with (,gtime (,msec) ,ggc (,gc-msec) ,gmem (,memory))
       ,@body
       (,list (,- (,msec) ,gtime)
              (,- (,gc-msec) ,ggc)
              (,- (,memory) ,gmem)))))


(mac timeit1 (x limit)
  (w/uniq (u n time gc mem)
    `(,with (,n     0
             ,time  (,msec)
             ,gc    (,gc-msec)
             ,mem   (,memory))
       ((,rfn ,u ()
          ,x
          (,++ ,n)
          (,when (,< (,- (,msec) ,time) ,limit)
            (,u))))
       (,list ,n
              (,- (,gc-msec) ,gc)
              (,- (,memory)  ,mem)))))


(mac timeit (x (o limit 10000))
  `(,let (a b c) (,timeit1 ,x ,limit)
     (,prn "iter: "   (,comma a)
           "  gc: "   b
           "  mem: "  c)
     ,nil))
