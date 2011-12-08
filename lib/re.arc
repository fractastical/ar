(def regexp (pattern)
  (if (string? pattern)
        (racket-pregexp pattern)
      pattern))

(def re-match (pattern (o in stdin))
  (let result ((if (string? in)
                    racket-regexp-match
                    racket-regexp-try-match)
               (regexp pattern) in)
    (if (is result #f)
          nil
                 ;; TODO: figure out how to get rid of this `if`
        (map [if (ac-tnil (racket-bytes? _))
                   (string _)
                 _]
             (racket-list->mlist result)))))

; This isn't anchored at the beginning of the input unless you use
; "^" yourself.
(def re-looking-at (pattern (o in stdin))
  (ac-tnil:racket-regexp-match-peek (regexp pattern) in))


(def re-replace (pattern in replace)
  (racket-regexp-replace regexp.pattern in replace))

;; TODO: what should this be called...?
(def re-replace* (pattern in replace)
  (racket-regexp-replace* regexp.pattern in replace))

(mac re-multi-replace (x . args)
  ;; TODO: can this use afneach?
  ((afn (((from to (o g)) . rest))
     ;(debug "re-multi-replace" from to g)
     (list (if g 'racket-regexp-replace*
                 'racket-regexp-replace)
           (regexp:string from)
           (if rest self.rest x)
           (if sym?.to string.to to)))
   (rev args)))


(def re-split (x y)
  (racket-list->mlist:racket-regexp-split (regexp x) y))
