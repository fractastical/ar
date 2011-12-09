;;  sxml.arc: converts S-expressions into XML

(= xml-declaration* "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
   html-doctype*    "<!DOCTYPE html>")

; http://dev.w3.org/html5/markup/syntax.html#void-element
(= html-void-elements* '(area base br col command embed hr img input
                         keygen link meta param source track wbr))


(def noop (s type depth))

(def pretty (s type depth)
  (when (or (is type 'start) (acons:car s))
    (pr "\n" (newstring (* depth 2) #\space))))


(parameter mode   noop)
(parameter parser nil)


(def attrs (xs)
  (if (no xs)         nil
      (acons:car xs)  xs
      (acons:cdr xs)  (do (pr " " (car xs) "=\"" (cadr xs) "\"")
                          (attrs (cddr xs)))
                      xs))


(def parse (s depth)
  (if (no s)    nil
      (atom s)  (pr s)
                (let (l . r) s
                  (if (caris (car r) '@)
                        (let res (join (flat:cdar r) (cdr r))
                          (parse (cons l res) depth))

                      (acons l)
                        (do (parse l depth)
                            (parse r depth))

                      r
                        (let res nil
                          ;; TODO: get this to work without the parens around mode
                          ((mode) l 'start depth)
                          (pr "<" l)
                          (= res (attrs r))
                          (if (or (and (isnt parser 'html)
                                       (no res))
                                  (and (is parser 'html)
                                       (some l html-void-elements*)))
                                (do (pr " />")
                                    (when res
                                      (apply warn
                                        (string "<" l " /> was closed, but it contained content:")
                                        res)))
                                (do (pr ">")
                                    (parse (car res) (+ depth 1))
                                    (parse (cdr res) (+ depth 1))
                                    ((mode) res 'end depth)
                                    (pr "</" l ">"))))

                      (and (is parser 'html)
                           (none l html-void-elements*))
                        (pr "<" l "></" l ">")

                      (pr "<" l " />")))))


(def ->string (s (o depth 0))
  (tostring:parse s depth))

(def ->html (root)
  (w/parser 'html
    (string html-doctype* (->string root))))

(def ->xml (root)
  (w/parser 'xml
    (string xml-declaration* (->string root))))

(mac w/pretty args
  #`(w/mode pretty ,@args))


;; why doesn't it work?!
;(mac w/pretty body
;  (w/uniq x
;    `(let ,x ,noop
;       (assign noop ,pretty)
;       ,@body
;       (assign noop ,x))))
