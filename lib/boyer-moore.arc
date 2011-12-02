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


#|(def boyer-moore-process-char (pattern char)
  (let l len.pattern
    ;; TODO: ew, catch
    (catch
      ;; TODO: throws an error when the character is
      (down i (- l 2) 0
        (when (is pattern.i char)
          (throw:- l i 1)))
      ;; TODO: is this right?
      l)))|#

(def boyer-moore-process-char (pattern tab)
  (let l len.pattern
    (down i (- l 2) 0
      (with (c  pattern.i
             x  (- l i 1))
        ;; haha, what a crazy hack
        (zap if tab.c (min tab.c x) x)))))

#|(def boyer-moore-process patterns
  (with (tab  (table)
         low  nil)
    (each x patterns
      ;; haha, what a crazy hack
      (zap if low (min low len.x) len.x)
      (boyer-moore-process-char x tab)
      #|(each c x
        (let x (boyer-moore-process-char x c)
          (zap if tab.c (min tab.c x) x)))|#
      )
    (list patterns low tab)))|#

(def boyer-moore-process (x)
  (let tab (table)
    (boyer-moore-process-char x tab)
    (list x tab)))

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

#|
(def boyer-moore-search ((pats low tab) input)
  ;(prn pats " " low " " tab " " input)
  (with (hlen  (- len.input 1)
         nlen  (- (len car.pats) 1))
    (aloop (x  pats
            i  nlen
            j  nlen)
      (prn car.x.i " " j " " (tab input.j low))
      (prn x)
      (if (< i 0)
            (+ j 1)
          (> j hlen) ;(or (< j 0) )
            nil
          (is car.x.i input.j)
            (self x (- i 1) (- j 1))
          (no cdr.x)
            (self pats nlen (+ j (tab input.j low)))
          (let nlen (- (len cadr.x) 1)
            ;(prn nlen " " (tab input.j low))
            (self cdr.x nlen nlen))))))|#

#|
(def boyer-moore-search ((pats low tab) input)
  ;(prn pats " " low " " tab " " input)
  (with (hlen  (- len.input 1)
         nlen  (- (len car.pats) 1))
    (aloop (x  pats
            l  nlen
            i  nlen
            j  nlen)
      ;(prn i " " j " " (tab input.j low))
      (prn x " " l " " i " " j)
      (if (< i 0)
            (+ j 1)
          (> j hlen) ;(or (< j 0) )
            (if (no cdr.x)
                  nil
                (let nlen (- (len cadr.x) 1)
                  ;(prn nlen " " (tab input.j low))
                  (self cdr.x nlen nlen nlen)))
          (is car.x.i input.j)
            (self x l (- i 1) (- j 1))
          (do (prn input.j " " (- (+ j l) 1) " " )
          (self x l l (+ j (tab input.j low))))))))|#

;; This is actually the Boyer–Moore–Horspool algorithm
(def boyer-moore-search ((x tab) input)
  ;(prn x " " tab " " input)
  ;(prn input)
  (withs (low   len.x
          nlen  (- low 1)
          hlen  (- len.input 1))
    (aloop (i  nlen
            j  nlen
            o  nlen)
      ;(prn x.i " " input.j " i" i " j" j " o" o " tab" (tab input.o low)); " is " (is x.i input.j)
      (if (< i 0)
            (+ j 1)
          (> j hlen)
            nil
          (is x.i input.j)
            (self (- i 1) (- j 1) o)
          (let shift (+ o (tab input.o low))
            (self nlen shift shift))))))


#|
#hash((#\r . 23)
      (#\s . 6)
      (#\T . 4)
      (#\u . 25)
      (#\t . 20)
      (#\R . 28)
      (#\g . 32)
      (#\- . 15)
      (#\  . 5)
      (#\a . 8)
      (#\c . 12)
      (#\S . 13)
      (#\e . 2)
      (#\' . 7)
      (#\h . 3)
      (#\i . 19)
      (#\C . 43)
      (#\l . 9)
      (#\m . 1)
      (#\n . 17)
      (#\o . 18))
"Chrono Trigger Resurrection - Schala's Theme"
"Chrono Trigger Resurrection OST Schala's Theme <ciNjsDPjrSw>.mp4"


#hash((#\f . 2)
      (#\o . 1))
   "foo"
"barufoo"

(boyer-posmatch "foo" "barufoo")

|#

#|(def boyer-moore-search-all ((pats low tab) input)
  (aloop (x pats)
    (if (no x)
          nil
        (or (boyer-moore-search car.x low tab input)
            (self cdr.x)))))|#

(def boyer-posmatch (pattern input)
  (boyer-moore-search boyer-moore-process.pattern input))

(def boyer-multi-match1 (patterns inputs)
  (keep (fn (in)
          (some (fn (pat)
                  (boyer-moore-search pat in))
                patterns))
        inputs))

(def boyer-multi-match (patterns inputs)
  (boyer-multi-match1 (map boyer-moore-process patterns) inputs))

(def multi-match (patterns inputs)
  (keep (fn (in)
          (some (fn (pat)
                  (posmatch pat in))
                patterns))
        inputs))
