>>> (load "../lib/object.arc")
nil

>>> fail
nil

>>> self
nil


==============================================================================


>>> (type (object))
table

>>> (type (object type 'table))
table

>>> (type (object type 'cons))
cons

>>> (type (object type '(cons table)))
#<is [cons table]>


>>> (isa (object) 'table)
t

>>> (isa (object) 'object)
t

>>> (isa (object type 'cons) 'cons)
t

>>> (isa (object type '(cons table)) 'cons)
t

>>> (isa (object type '(cons table)) 'table)
t


==============================================================================


>>> (object)
(object)


>>> (object a           1
            b           2
            set         (fn ())
            intersperse (fn ()))
(object a           1
        b           2
        intersperse #<procedure>
        set         #<procedure>)


>>> (object type 'table)
#hash((type . table))


>>> (object type 'table print (fn () "foobar"))
foobar


>>> (= foo (object print (fn () (+ "#<table of length " len.self ">"))))
#<table of length 1>

>>> foo<-print
#<procedure>

>>> (foo<-print)
"#<table of length 1>"

>>> (= foo<-bar "qux")
"qux"

>>> foo<-bar
"qux"

>>> (len foo)
2

>>> foo
#<table of length 2>


==============================================================================


>>> (= foo (object len (fn () self<-value) value 5))
(object len   #<procedure>
        value 5)

>>> foo<-len
#<procedure>

>>> (foo<-len)


>>> (len foo)
5


>>> (len (object))
0

>>> (len (object a 1 b 2))
2

>>> (len (object keys (fn () '(a b c d))))
4

>>> (len (object keys (fn () self<-value) value '(a b c d)))
4


==============================================================================


>>> (maptable (fn (x y)
                (prn x " - " y))
              (object a 1 b 2))
a - 1
b - 2
#hash((a . 1)
      (b . 2))


>>> (maptable (fn (x y)
                (prn x " - "y))
              (object a 1 b 2 keys (fn () '(a b c))))
a - 1
b - 2
c - nil
(object a 1
        b 2
        c nil)


>>> (maptable (fn (x y)
                (prn x " - " y))
              (object keys (fn () '(a b c))))
a - nil
b - nil
c - nil
(object a nil
        b nil
        c nil)


>>> (each (x y) (object call (fn (k) "bar")
                        keys (fn () '(a b c)))
      (prn x " - " y))
a - bar
b - bar
c - bar
(object a bar
        b bar
        c bar)


>>> (each (x y) (object keys   (fn () self<-value)
                        value '(a b c))
      (prn x " - " y))
a - nil
b - nil
c - nil
(object a nil
        b nil
        c nil)


==============================================================================


>>> (= foo (object call (fn () self<-y) y 50))
(object call #<procedure>
        y    50)

>>> (= bar (object))
(object)


>>> (foo)
50

>>> (foo<-call)
50


>>> (w/self bar (foo))
nil

>>> (= bar<-y 22)
22

>>> (w/self bar (foo))
22

>>> (w/self bar (foo<-call))
22


==============================================================================


>>> (= foo (object call (fn (k) nil)
                   set  (fn (k v)
                          (prn "assigning " k " to " v)
                          (= self<-y v))))
(object call #<procedure>
        set  #<procedure>)


>>> (foo 'y)
nil

>>> foo<-y
nil


>>> (= (foo 'x) 50)
assigning x to 50
50

>>> (foo 'x)
nil

>>> (foo 'y)
nil

>>> foo<-x
nil

>>> foo<-y
50


>>> (= foo<-x 65)
65

>>> (foo 'x)
nil

>>> (foo 'y)
nil

>>> foo<-x
65

>>> foo<-y
50


>>> (= (foo 'set) 20)
assigning set to 20
20

>>> (foo 'set)
nil

>>> (foo 'y)
nil

>>> foo<-set
#<procedure>

>>> foo<-y
20


>>> (del foo<-set)
nil

>>> (= (foo 'x) 26)
26

>>> (foo 'x)
nil

>>> (foo 'y)
nil

>>> foo<-x
26

>>> foo<-y
20


>>> foo
(object call #<procedure>
        x    26
        y    20)


==============================================================================


>>> (macex1 '(del foo<-x))
(del-attribute foo (quote x))

>>> (macex1 '(del (get-attribute foo 'x)))
(del-attribute foo (quote x))


>>> (= foo (object a   1
                   b   2
                   del (fn (k)
                         (prn "deleting " k " which is " (get-attribute self k))
                         nil)))
(object a   1
        b   2
        del #<procedure>)


>>> foo<-a
1

>>> foo<-b
2


>>> (del (foo 'a))
deleting a which is 1
nil

>>> (del (foo 'b))
deleting b which is 2
nil

>>> (del foo!a)
deleting a which is 1
nil

>>> (del foo!b)
deleting b which is 2
nil


>>> foo<-a
1

>>> foo<-b
2


>>> (del foo<-a)
nil

>>> foo<-a
nil

>>> foo<-b
2


>>> (del foo<-b)
nil

>>> foo<-a
nil

>>> foo<-b
nil


>>> foo
(object del #<procedure>)
