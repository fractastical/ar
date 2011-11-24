;; TODO: use alias
(= acons  cons?
   alist  list?)

(def call-w/stdout (port thunk)
  (parameterize (stdout port) (thunk)))

(def call-w/stdin (port thunk)
  (parameterize (stdin port) (thunk)))

;; lib/strings.arc
;(= num comma)


;; TODO: compat.arc should be in a separate namespace
#|(def readline ((o str (stdin)))
  (awhen (readc str)
    (tostring
      (writec it)
      (whiler c (readc str) [in _ nil #\newline]
        (writec c)))))|#

;(= assoc (reverse-args assoc))
