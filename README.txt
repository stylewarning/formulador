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

FORMULADOR> (draw (frac-box (box "x") (box "y")))
#<CANVAS 
+---+
| x |
|---|
| y |
+---+
with 3 defined regions>

And a more complex example:

FORMULADOR> (draw
             (frac-box (row-box (list
                                 (frac-box (box "x") (box "y"))
                                 (box "+")
                                 (frac-box (box "w")
                                           (row-box (list (box "u") (box "+") (box "v")) :padding 1)))
                                :padding 1)
                       (frac-box (row-box (list (box "p") (box "+") (box "q")) :padding 1)
                                 (row-box (list (box "r") (box "+") (frac-box (box "1") (box "s"))) :padding 1))))
#<CANVAS 
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
with 23 defined regions>

Demonstration of the use of combinators:

FORMULADOR> (defun interleave (x list)
              (rest (mapcan (lambda (y) (list x y)) list)))
INTERLEAVE
FORMULADOR> (interleave '* '(x y z))
(X * Y * Z)
FORMULADOR> (draw
             (frac-box
              (box "1")
              (row-box (interleave (box "+") (map 'list
                                                  (lambda (c)
                                                    (frac-box (box "1") (box c)))
                                                  "harmonic"))
                       :padding 1)))
#<CANVAS 
+-----------------------------------------------+
|                       1                       |
|-----------------------------------------------|
|  1     1     1     1     1     1     1     1  |
| --- + --- + --- + --- + --- + --- + --- + --- |
|  h     a     r     m     o     n     i     c  |
+-----------------------------------------------+
with 34 defined regions>


Formulador is aware of baselines. Note the correct baselines in the following example.

FORMULADOR> (draw (glue +sigma+
                        (row-box (list (frac-box (frac-box (box "x + y") (box "z"))
                                                 (box "b"))
                                       (box "+")
                                       (parens-box
                                        (row-box
                                         (list (frame-box (frac-box
                                                           (box "c")
                                                           (row-box (list
                                                                     (frac-box (box "p")
                                                                               (box "q"))
                                                                     (box "+")
                                                                     (box "r"))
                                                                    :padding 1)))
                                               (box "*")
                                               (box "r"))
                                         :padding 1)))
                                 :padding 1)))
#<CANVAS 
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
with 22 defined regions>

If your terminal supports unicode, it can print using unicode by
setting the appropriate so-called "charmaps". Charmaps are alternative
character sets for different decorations or constructions.

FORMULADOR>  (let ((*frame-charmap* *unicode-plain-frame-charmap*)
                   (*vinculum-charmap* *unicode-vinculum-charmap*)
                   (*paren-charmap* *unicode-paren-charmap*))
               (draw (glue +sigma+
                           (row-box (list (frac-box (frac-box (box "x + y") (box "z"))
                                                    (box "b"))
                                          (box "+")
                                          (parens-box
                                           (row-box
                                            (list (frame-box (frac-box
                                                              (box "c")
                                                              (row-box (list
                                                                        (frame-box
                                                                         (frac-box (box "p")
                                                                                   (box "q")))
                                                                        (box "+")
                                                                        (box "r"))
                                                                       :padding 1)))
                                                  (box "*")
                                                  (box "r"))
                                            :padding 1)))
                                    :padding 1))))
#<CANVAS 
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
with 23 defined regions>

Superscripts and subscripts are supported. Here's the standard Euclidean metric in 3-space:

FORMULADOR> (flet ((x_ (n)
                     (script-box (box "x")
                                 :superscript (box "2")
                                 :subscript (box (prin1-to-string n)))))
              (draw
               (glue (box "r")
                     (box " = ")
                     (script-box (parens-box (glue (x_ 1)
                                                   (box " + ")
                                                   (x_ 2)
                                                   (box " + ")
                                                   (x_ 3)))
                                 :superscript (box "1/2")))))
#<CANVAS 
+-----------------------+
|                    1/2|
|    /  2    2    2 \   |
|r = | x  + x  + x  |   |
|    \  1    2    3 /   |
+-----------------------+
with 22 defined regions>

There are more examples in examples.lisp. One such is Chudnovsky's formula.

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

We can extract parts of a drawn expression and see the associated
tree. This is useful for editing tasks.

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
