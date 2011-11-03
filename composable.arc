#|
    Racket
    Copyright (c) 2010-2011 PLT Scheme Inc.

    Copyright © 2011 Paul Chapman <pcxunlimited@gmail.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    The GPL  is located under LICENSE/gpl.txt
    The LGPL is located under LICENSE/lgpl.txt

    This file was created by copying and then significantly modifying
    /usr/lib/plt/collects/mzlib/control.ss
|#

(use racket)

(def abort-current-continuation/keep-prompt (tag thunk)
  ((racket-call-with-continuation-prompt
     (fn ()
       ((racket-call-with-current-continuation
          (fn (k) (fn () k))
          tag)))
     tag)
   thunk))

(def call-with-shift (f (o tag))
  (if tag (racket-call-with-composable-continuation
            (fn (k)
              (abort-current-continuation/keep-prompt
                tag
                (fn ()
                  (f (fn vals
                       (racket-call-with-continuation-prompt
                         (fn () (apply k vals))
                         tag
                         racket-#f))))))
            tag)
          (call-with-shift f (racket-default-continuation-prompt-tag))))

(mac shift (x y)
  `(call-with-shift (fn (,x) ,y)))

(mac reset (x)
  `(racket-call-with-continuation-prompt (fn () ,x)))
