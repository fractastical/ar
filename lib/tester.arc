(load "lib/re.arc")

(mac collect (x)
  `(accum yield ,x))


(def input->list (x)
  (collect:whiler c (readc x nil) no
    (yield c)))


(def writeuntil (str x)
  (whenlet c (readc str)
    (writec c)
    (if (isnt c x)
          (writeuntil str x))))

(def readuntil (str x)
  (tostring ((afn ()
               (writeuntil str #\newline)
               (iflet c (peekc str)
                 (if (isnt c x)
                   (self)))))))


(def parse-string (str)
  (draintarget str
    (let spaces (- (len:re "^ *>") 1)
      (re ">> ")
      (let rest (instring:string re-text*)
        (do1 (list (read rest)
                   (errsafe:cut (readuntil rest #\newline) 1 -1))
             (= re-text* (input->list rest)))))))


(mac errdet (body)
  `(on-err (fn (e) (pr:+ "error: " (details e)))
           (fn ()  (write ,body))))


(def backescape (str)
  (map (fn (x)
         (list (string x)
               (+ "\\" x))) (coerce str 'cons)))


(def escape (x)
  (multisubst (backescape "\\.^$*+?{[]|()") x))


(def match (x y)
  (w/target x
    (re:subst ".*" "\\.\\.\\." (escape y))
    (no re-text*)))


(def eval-test ((run expect))
  (let result (tostring
                (errdet:eval run))
    (if (match result expect) t
        (do (prn "\n- Failed example:")
            (write run)
            (prn)
            (prn "\n- Expected:")
            (prn expect)
            (prn "\n- Got:")
            (prn result)
            (prn "\n" (newstring 80 #\=))
            nil))))

(def test-file (x)
  (w/infile file x
    (each x (parse-string:allchars file)
      (eval-test x))))