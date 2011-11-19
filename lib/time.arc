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

(mac utime body
  (w/uniq (gtime ggc gmem)
    `(with (,gtime (msec) ,ggc (gc-msec) ,gmem (memory))
       ,@body
       (list (- (msec) ,gtime)
             (- (gc-msec) ,ggc)
             (- (memory) ,gmem)))))

;; TODO: move this someplace else
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

(mac timeit (x (o a 1000) (o n 10000))
  `(let (a b c) (map inexact:avg
                     (apply zip (n-of ,a (utime (repeat ,n ,x)))))
     (prn "time: " a
          " gc: "  b
          " mem: " c)
     nil))
