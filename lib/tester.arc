(load "re.arc")

;; options
(= verbose nil)

#|
(def input->list (x)
  (drain (readc x)))
;  (collect:whiler c (readc x nil) no
;    (yield c)))
|#

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
  (w/target (drain str)
;    (prn (re "^ *>"))
    (let spaces (re "(^|\n) *>>>")
;      (re ">>> ")
#|    (let spaces (1- (len:re "^ *>"))
      (re ">> ")|#
      (let rest re-input*
;      (let rest (instring:string re-text*)
        (do1 (list (read rest)
                   (errsafe:cut (readuntil rest #\newline) 1 -1))
;             (= re-text* (input->list rest))
             )))))


(mac errdet (body)
  `(on-err (fn (e) (pr:+ "error: " (details e)))
           (fn ()  (write ,body))))


(def backescape (str)
  (map (fn (x)
         (list (string x)
               (+ "\\" x)))
       (coerce str 'cons)))


(def escape (x)
  (multisubst (backescape "\\.^$*+?{[]|()") x))


(def match (x y)
  (w/target x
    (re:subst ".*" "\\.\\.\\." (escape y))
    (no:peekc re-input*)
;    (no re-text*)
    ))


(def eval-test ((run expect))
  (let result (tostring:w/stderr stdout
                (errdet:eval run))
    (if (match result expect)
          (do (when verbose
                (prn "\n- Testing example:")
                (write run)
                (prn)
                (prn "\n- Success:")
                (prn result)
                (prn "\n" (newstring 80 #\=)))
              t)
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
  (w/curdir (dirname x)
    (w/infile file (basename x)
      (each x (parse-string:allchars file)
        (eval-test x)))))

(mac test-files args
  `(do ,@(map (fn (x)
                `(test-file ,x))
              args)))
