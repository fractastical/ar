(def regexp (pattern)
  (if (string? pattern)
        (%inline:racket-pregexp pattern)
      pattern))

(def re-match (pattern (o in stdin))
  (let result ((if (string? in)
                    %get.racket-regexp-match
                    %get.racket-regexp-try-match)
               (regexp pattern) in)
    (if (is result #f)
          nil
             ;; TODO: figure out how to get rid of this `if`
        (map [%if (%inline:racket-bytes? _)
                    (string _)
                  _]
             result)))) ;(racket-list->mlist )

; This isn't anchored at the beginning of the input unless you use
; "^" yourself.
(def re-looking-at (pattern (o in stdin))
  ;; TODO: different ssyntax priorities
  (ac-tnil (%get.racket-regexp-match-peek (regexp pattern) in)))


(def re-replace (pattern in replace)
  (%get.racket-regexp-replace regexp.pattern in replace))

;; TODO: what should this be called...?
(def re-replace* (pattern in replace)
  (%get.racket-regexp-replace* regexp.pattern in replace))

(mac re-multi-replace (x . args)
  (alet ((from to (o g)) . rest) nrev.args
    (list (if g %get.racket-regexp-replace*
                %get.racket-regexp-replace)
          (regexp:string from)
          (if rest self.rest x)
          (if sym?.to string.to to))))


(def re-split (x y)
  (%get.racket-regexp-split (regexp x) y)) ;(racket-list->mlist )
