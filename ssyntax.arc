;; TODO: better ssyntax system (see ssyntax.arc.bak)

(def ac-symbol->chars (x)
  (coerce (coerce x 'string) 'cons))

(def ac-chars->value (x)
  (read (string x)))

(def ac-insym? (char sym)
  (mem char (ac-symbol->chars sym)))

(def ac-tokens (test source token acc keepsep?)
  (if (no source)
        (rev (if (cons? token)
                   (cons (rev token) acc)
                 acc))
      (test (car source))
        (ac-tokens test
                   (cdr source)
                   '()
                   (let rec (if (no token)
                                  acc
                                (cons (rev token) acc))
                     (if keepsep?
                           (cons (car source) rec)
                         rec))
                   keepsep?)
       (ac-tokens test
                  (cdr source)
                  (cons (car source) token)
                  acc
                  keepsep?)))

(def ac-build-sexpr (toks orig)
  (if (no toks)
        'get
      (no (cdr toks))
        (ac-chars->value (car toks))
      (list (ac-build-sexpr (cddr toks) orig)
            (if (is (cadr toks) #\!)
                  (list 'quote (ac-chars->value (car toks)))
                (if (in (car toks) #\. #\!)
                      (err "Bad ssyntax" orig)
                    (ac-chars->value (car toks)))))))

(def ac-expand-sexpr (sym)
  (ac-build-sexpr (rev (ac-tokens [in _ #\. #\!] (ac-symbol->chars sym) nil nil t))
                  sym))

(def ac-expand-compose (sym)
  (let elts (map1 (fn (tok)
                    (if (is (car tok) #\~)
                          (if (no (cdr tok))
                                'no
                              `(,complement ,(ac-chars->value (cdr tok))))
                        (ac-chars->value tok)))
                  (ac-tokens [is _ #\:] (ac-symbol->chars sym) nil nil nil))
    (if (no (cdr elts))
          (if (sym? (car elts))
                (keyword (car elts))
              (car elts))
        (cons compose elts))))

(def ac-expand-and (sym)
  (let elts (map1 ac-chars->value
                  (ac-tokens [is _ #\&] (ac-symbol->chars sym) nil nil nil))
    (if (no (cdr elts))
          (car elts)
        (cons andf elts))))



(redef ssyntax (x)
  (and (sym? x)
       ;; TODO: ew
       (let x (string x)
         (recstring [in (x _) #\: #\~ #\& #\. #\!] x))))

(redef ssexpand (x)
  (if (sym? x)
        (if (or (ac-insym? #\. x)
                (ac-insym? #\! x))
              (ac-expand-sexpr x)
            (or (ac-insym? #\: x)
                (ac-insym? #\~ x))
              (ac-expand-compose x)
            (ac-insym? #\& x)
              (ac-expand-and x)
            x)
      x))
