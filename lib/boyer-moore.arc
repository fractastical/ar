(def boyer-moore-process-char (pattern tab)
  (let l len.pattern
    (down i (- l 2) 0
      (with (c  pattern.i
             x  (- l i 1))
        ;; haha, what a crazy hack
        (zap if tab.c (min tab.c x) x)))))

(def boyer-moore-process (x)
  (let tab (obj)
    (boyer-moore-process-char x tab)
    (list x tab)))

;; This is actually the Boyer–Moore–Horspool algorithm
(def boyer-moore-search ((x tab) input)
  (withs (low   len.x
          nlen  (- low 1)
          hlen  (- len.input 1))
    (awith (i  nlen
            j  nlen
            o  nlen)
      (if (< i 0)
            (+ j 1)
          (> j hlen)
            nil
          (is x.i input.j)
            (self (- i 1) (- j 1) o)
          (let shift (+ o (tab input.o low))
            (self nlen shift shift))))))

(def boyer-posmatch (pattern input)
  (boyer-moore-search boyer-moore-process.pattern input))

                                         ;; TODO: :every isn't used here
(def boyer-multi-match1 (patterns inputs :every)
  (keep (fn (in)
          (some (fn (pat)
                  (boyer-moore-search pat in))
                patterns))
        inputs))

                                        ;; TODO: :every isn't used here
(def boyer-multi-match (patterns inputs :every)
  (boyer-multi-match1 (map boyer-moore-process patterns) inputs :every every))


;; for testing posmatch: doesn't have anything to do with boyer-moore
(def multi-match (patterns inputs)
  (keep (fn (in)
          (some (fn (pat)
                  (posmatch pat in))
                patterns))
        inputs))
