                              FORMULADOR
                              ==========

                           By Robert Smith

Formulador is intended to be a text-based formula renderer, and maybe
editor one day.

It is still a work-in-progress; we can't reinvent an ad hoc,
informally-specified, bug-ridden, slow implementation of half of TeX
overnight.

The way it works right now is simple: build your typesetting object,
and draw it out to a canvas. Formulador will take the liberty of
making the canvas for you.

Fundamentally, as it stands, input is composed of boxes. Each box
contains an element you'd like to typeset. Right now we have the
following boxes

    * empty-box (a sort of no-op; typesets as nothing)
    * strings (typeset as themselves)
    * characters (typeset as themselves)
    * string-box (a wrapper for a string)
    * frac-box (fractions)
    * row-box (a horizontal concatenation of boxes)
    * frame-box (a box with decorative appeal)
    * script-box (superscripts and subscripts)
    * limits-box (limits for operators)
    * sqrt-box (Nth roots)

These will be extended and given more options over time.

Here is a simple example:

> (draw (frac-box "x" "y"))
+---+
| x |
|---|
| y |
+---+

And a more complex example:

> (draw
   (frac-box (row-box (list
                       (frac-box "x" "y")
                       "+"
                       (frac-box "w"
                                 (row-box (list "u" "+" "v") :padding 1)))
                      :padding 1)
             (frac-box (row-box (list "p" "+" "q") :padding 1)
                       (row-box (list "r" "+" (frac-box "1" "s")) :padding 1))))
+---------------+
|  x       w    |
| --- + ------- |
|  y     u + v  |
|---------------|
|     p + q     |
|   ---------   |
|         1     |
|    r + ---    |
|         s     |
+---------------+

Demonstration of the use of combinators:

> (defun interleave (x list)
    (rest (mapcan (lambda (y) (list x y)) list)))
INTERLEAVE
> (interleave '* '(x y z))
(X * Y * Z)
> (draw
   (frac-box
    "1"
    (row-box (interleave "+" (map 'list
                                  (lambda (c)
                                    (frac-box "1" c))
                                  "rtoyg"))
             :padding 1)))
+-----------------------------+
|              1              |
|-----------------------------|
|  1     1     1     1     1  |
| --- + --- + --- + --- + --- |
|  r     t     o     y     g  |
+-----------------------------+


Baseline test:

> (draw (glue +sigma+
              (row-box (list (frac-box (frac-box "x + y" "z")
                                       "b")
                             "+"
                             (parens-box
                              (row-box
                               (list (frame-box (frac-box
                                                 "c"
                                                 (row-box (list
                                                           (frac-box "p"
                                                                     "q")
                                                           "+"
                                                           "r")
                                                          :padding 1)))
                                     "*" 
                                     "r")
                               :padding 1)))
                       :padding 1)))
+----------------------------------+
|     x + y                        |
|=== -------    / +---------+     \|
|\      z       | |    c    |     ||
| > --------- + | |---------| * r ||
|/      b       | |  p      |     ||
|===            | | --- + r |     ||
|               | |  q      |     ||
|               \ +---------+     /|
+----------------------------------+

Now with Unicode...

> (let ((*frame-charmap* *unicode-plain-frame-charmap*)
        (*vinculum-charmap* *unicode-vinculum-charmap*)
        (*paren-charmap* *unicode-paren-charmap*))
    (draw (glue +sigma+
                (row-box (list (frac-box (frac-box "x + y" "z")
                                         "b")
                               "+"
                               (parens-box
                                (row-box
                                 (list (frame-box (frac-box
                                                   "c"
                                                   (row-box (list
                                                             (frame-box
                                                              (frac-box "p"
                                                                        "q"))
                                                             "+"
                                                             "r")
                                                            :padding 1)))
                                       "*" 
                                       "r")
                                 :padding 1)))
                         :padding 1))))
+------------------------------------+
|     x + y                          |
|=== ───────    ⎛ ┌───────────┐     ⎞|
|\      z       ⎜ │     c     │     ⎟|
| > ───────── + ⎜ │───────────│ * r ⎟|
|/      b       ⎜ │ ┌───┐     │     ⎟|
|===            ⎜ │ │ p │     │     ⎟|
|               ⎜ │ │───│ + r │     ⎟|
|               ⎜ │ │ q │     │     ⎟|
|               ⎜ │ └───┘     │     ⎟|
|               ⎝ └───────────┘     ⎠|
+------------------------------------+

Superscripts and subscripts:

> (flet ((x_ (n)
           (script-box "x"
                       :superscript "2"
                       :subscript (prin1-to-string n))))
    (draw
     (glue "r"
           " = "
           (script-box (parens-box (glue (x_ 1)
                                         " + "
                                         (x_ 2)
                                         " + "
                                         (x_ 3)))
                       :superscript "1/2"))))
+-----------------------+
|                    1/2|
|    /  2    2    2 \   |
|r = | x  + x  + x  |   |
|    \  1    2    3 /   |
+-----------------------+

From examples.lisp, we have the Chudnovsky formula:

> (draw *chudnovsky*)
#<CANVAS 
+------------------------------------------------------------------------+
|         _______    ∞                                                   |
|        |      3   ===                                                 k|
| 1     \|640320    \     (6k)! (545140134k + 13591409)  /       1     \ |
|--- = -----------   >   ------------------------------- | - --------- | |
| π        12       /                        3           |          3  | |
|                   ===            (3k)! (k!)            \    640320   / |
|                  k = 0                                                 |
+------------------------------------------------------------------------+
with 54 defined regions>

We can extract parts of a drawn expression and see the associated tree.

> (mapcar #'draw (objects-at-point (draw *chudnovsky*) 67 4))
(#<CANVAS 
+-+
|3|
+-+
with 1 defined region>
 #<CANVAS 
+-------+
|      3|
|640320 |
+-------+
with 4 defined regions>
 #<CANVAS 
+---------+
|    1    |
|---------|
|       3 |
| 640320  |
+---------+
with 6 defined regions>
 #<CANVAS 
+-----------+
|      1    |
|- ---------|
|         3 |
|   640320  |
+-----------+
with 8 defined regions>
 #<CANVAS 
+---------------+
|/       1     \|
|| - --------- ||
||          3  ||
|\    640320   /|
+---------------+
with 19 defined regions>
 #<CANVAS 
+----------------+
|               k|
|/       1     \ |
|| - --------- | |
||          3  | |
|\    640320   / |
+----------------+
with 22 defined regions>
 #<CANVAS 
+------------------------------------------------------------------------+
|         _______    ∞                                                   |
|        |      3   ===                                                 k|
| 1     \|640320    \     (6k)! (545140134k + 13591409)  /       1     \ |
|--- = -----------   >   ------------------------------- | - --------- | |
| π        12       /                        3           |          3  | |
|                   ===            (3k)! (k!)            \    640320   / |
|                  k = 0                                                 |
+------------------------------------------------------------------------+
with 54 defined regions>)
