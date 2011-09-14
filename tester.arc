(use cwd path) ; re strings

;; options
(implicit verbose)
(implicit expects-strings)

(implicit debug-statements)
(implicit line-number)

(implicit separator-width 78)


#|
;; should be somewhere else, like re.arc
(mac re-multi-replace (x . args)
  ((afn (((from to (o g)) . rest))
     (list (if g 'racket-regexp-replace*
                 'racket-regexp-replace)
           (string from)
           (if rest (self rest) x)
           (string to)))
   (rev args)))|#


(def debug args
  (push (tostring:mapfn disp (intersperse " " args))
        debug-statements))

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


#|(def parse-string (str)
  (w/target (drain str)
    (++ line-number)
    (re "(^|\n)")
;    (prn (re "^ *>"))
    (let spaces (re " *>")
;      (re ">>> ")
#|    (let spaces (1- (len:re "^ *>")))
      (re ">> ")|#
      (let rest re-input*
;      (let rest (instring:string re-text*))
        (do1 (obj eval    (read rest)
                  expects (errsafe:cut (readuntil rest #\newline) 1 -1)
                  line-number line-number)
;             (= re-text* (input->list rest))
             )))))|#

#|(def parse-string (in)
  (readuntil rest #\newline)
  (trues (fn (x)
           (++ line-number)

           (when (re-looking-at "^ *>" in)
             (prn (read in))
             (obj))
           nil)
         (tokens in "\n")))|#


(def parse-string (in)
  ;(readuntil in #\newline)
  (when peekc.in
    (if (re-match "^ *>" in)
              ;(prn (peekc in))
              ;(prn (read in))
              ;(readc in)
              ;(readline in)
              ;(prn )
              #|(let expect (tostring:while (aand (peekc in)
                                                (isnt it "\n"))
                            ;(prn:readline in)
                            ;nil
                            (prn:readline in)
                            (prn (is (peekc in) "\n"))
                            (prn "-------------")
                            )
                (prn expect)
                )|#
          (let x (readuntil in #\newline)
            (zap re-multi-replace x ("\n\\\\\n"  "\n\n" g))
            (let c (count #\newline x)
              (zap instring x)
              ;(cons ... (parse-string in))
              (do1 (obj eval         (read x)
                        expects      (errsafe:cut allchars.x 1 -1)
                        line-number  line-number)
                   (++ line-number c))))

        (do (readline in)
            (++ line-number)
            (parse-string in)))))


(mac errdet (body)
  `(on-err (fn (e) (pr:+ "error: " details.e))
           (fn ()  (write ,body))))


(def backescape (str)
  (map x (coerce str 'cons)
    (list (string x)
          (+ "\\" x))))


(def escape (x)
  (multisubst (backescape "\\.^$*+?{}[]|()") x))


(def match (x y)
  ;(prn (string "^" (subst ".*" "\\.\\.\\." (escape y)) "$"))
  (re-match (string "^" (subst ".*" "\\.\\.\\." (escape y)) "$")
            x))
  ;(if expects-strings )
#|  (w/target x
    (re:string "^" (subst ".*" "\\.\\.\\." (escape y)) "$")
    (no:peekc re-input*)
;    (no re-text*)
    ))|#

(def eval-test (x)
  (with (run     x!eval
         expect  x!expects
         line    x!line-number)
    (w/debug-statements nil
      (let result (tostring:w/stderr stdout
                    (errdet:eval run))
        (when expect
          (when expects-strings
            (zap re-multi-replace result ("\\\\\""  "\"" g)
                                         ("\\\\n"   "\n" g)
                                         ("^\""     ""    )
                                         ("\"$"     ""    ))
            #|(zap re-multi-replace result '("\\\\\"|\\\\\n"  "" g)
                                         '("^\""            ""  )
                                         '("\"$"            ""  ))|#
            #|(zap [racket-regexp-replace "^\""
                                        (racket-regexp-replace* "\\\\\"|\\\\\n" _ "")
                                        ""] result)|#
            #|(= result (multisubst '(("\\n"  "\n")
                                    ("\\\"" "\""))
                                  (cut result 1 -1)))|#
                                )

          ;(= expect (subst "\n\n" "\n\\\n" expect))

          (zap re-multi-replace expect ("\n\\\\\n"   "\n\n" g)
                                       ("^\\\\$"     ""      ))

          ;(prn result expect)

          (if (match result expect)
                (do ;(++ failed)
                    (when verbose
                      (prn "\n- Testing example on line " line ":")
                      (write run)
                      (prn)
                      (prn "\n- Success:")
                      (prn result)
                      (prn "\n" (newstring separator-width #\=)))
                    t)
                (do (prn "\n- Failed example on line " line ":")
                    (write run)
                    (prn)
                    (prn "\n- Expected:")
                    (prn expect)
                    (prn "\n- Got:")
                    (prn result)
                    (when debug-statements
                      (prn "\n- Debug:")
                      (eachfn prn rev.debug-statements))
                    (prn "\n" (newstring separator-width #\=))
                    nil)))))))

#|> (let orig pr
    (redef pr args
      (apply orig args)
      nil))

> (let orig eval-test
    (def eval-test ((run expect))
      (orig:list run
                 (tostring (pr #\")
                           (pr (multisubst '(("\n" "\\n")
                                             ("\"" "\\\"")) expect))
                           (pr #\")))))|#

(def test-file (x)
  (= expects-strings nil)
  (w/line-number 1
    (w/cwd (or dirpart.x ".") ; dirname
      (w/infile file filepart.x ; basename
        (whilet x parse-string.file
          (eval-test x))
        #|(each x parse-string.file
          (prn x)
          (eval-test x))|#
          ))))

(mac test-files args
  `(do ,@(mapeach x args
           `(test-file ,x))))
