#|
"foo" "bar" "corge"

e = 3
g = 1
r = 0
o = 0
c = 3
b = 3
f = 3
|#

#|(while (>= hlen nlen)
        (for i nlen 0
          (if (is x.i input.i)
                (throw t)
              (do (-- hlen (tab input.nlen))
                  (= i nlen)))))|#
      #|((afn (i j)
         (prn i " " x.i " " j " " input.j)
         (if (< i 0)
               (throw t)
             (is x.i input.i)
               (self (- i 1) (- j 1))
             (self l (min li (+ j (tab input.i low))))))
       l l)|#

;; TODO: should be elsewhere
(mac rloop (name parms . body)
  (let (l r) (apply zip pair.parms)
    `((rfn ,name ,l ,@body) ,@r)))

(mac aloop (parms . body)
  `(rloop self ,parms ,@body))


(def boyer-moore-process-char (pattern char)
  (let l len.pattern
    ;; TODO: ew, catch
    (catch:down i (- l 2) 0
      (when (is pattern.i char)
        (throw:- l i 1)))))

(def boyer-moore-process patterns
  (with (tab  (table)
         low  nil)
    (each x patterns
      ;; haha, what a crazy hack
      (zap if low (min low len.x) len.x)
      (each c x
        (let x (boyer-moore-process-char x c)
          (zap if tab.c (min tab.c x) x))))
    (list patterns low tab)))

#|(def boyer-moore-search (x low tab input)
  (prn x " " low " " tab " " input)
  (let hlen (- len.input 1)
    (catch:let nlen (- len.x 1)
      (aloop (x  pats
              i  nlen
              j  nlen)
        (prn i " " j)
        (if (< i 0)
              (throw:+ j 1)
            (> j hlen)
              (throw nil)
            (is x.i input.j)
              (self (- i 1) (- j 1))
            (self nlen (+ j (tab input.j low))))))))|#

#|(def boyer-moore-search1 (x hlen low tab input)
  (let nlen (- (len car.x) 1)
    )|#

;; This is actually the Boyer–Moore–Horspool algorithm
(def boyer-moore-search ((pats low tab) input)
  ;(prn pats " " low " " tab " " input)
  (with (hlen  (- len.input 1)
         nlen  (- (len car.pats) 1))
    (aloop (x  pats
            i  nlen
            j  nlen)
      ;(prn i " " j " " x)
      (if (< i 0)
            (+ j 1)
          (> j hlen) ;(or (< j 0) )
            nil
          (is car.x.i input.j)
            (self x (- i 1) (- j 1))
          (no cdr.x)
            (self pats nlen (+ j (tab input.j low)))
          (let nlen (- (len cadr.x) 1)
            (self cdr.x nlen nlen))))))

#|(def boyer-moore-search-all ((pats low tab) input)
  (aloop (x pats)
    (if (no x)
          nil
        (or (boyer-moore-search car.x low tab input)
            (self cdr.x)))))|#

#|(def boyer-posmatch (pattern input)
  (boyer-moore-search boyer-moore-process.pattern input))|#
