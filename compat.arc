;; TODO: use alias
(= acons  cons?
   alist  list?)

(def call-w/stdout (port thunk)
  (parameterize (stdout port) (thunk)))

(def call-w/stdin (port thunk)
  (parameterize (stdin port) (thunk)))
