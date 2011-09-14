(def defvar (n x y)
  ;; TODO: I don't like varset, because it uses eval
  (varset n x)
  (= (ac-var-assigner* n) y)
  (ac-zeroarg n))

(mac alias (x y)
  (w/uniq u
    `(defvar ',x
       (fn ()   ,y)
       (fn (,u) (= ,y ,u)))))
