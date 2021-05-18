# circle5.plot  06 April 2014
#
file circle5.ps

  space -2.0 -2.0 +2.0 +2.0

  page

    line_rgb 0.8 0.8 0.8
    grid -2.0 -2.0 +2.0 +2.0 21 21
#
#  Black things.
#
    line_width 3

    fill_rgb 0.0 0.0 0.0
    moveto -2.0  0.0
    drawto +2.0  0.0
    moveto  0.0 -2.0
    drawto  0.0 +2.0
#
#  Blue things
#
    fill_rgb 0.0 0.0 1.0
    circle  0.0  0.0  1.0

    line 0.933 -2.0 0.933 +2.0
#
#  Green things
#
    fill_rgb 0.0 1.0 0.0
    circle_fill  0.5  -1.75  0.05

    circle_fill  0.5 -0.866  0.05
    circle_fill 0.933 -0.616 0.05
#
#  Red things
#
    fill_rgb 1.0 0.0 0.0
    circle_fill 0.933 +0.359 0.05
    circle_fill 0.933 -0.359 0.05
#
#  Black things.
#
    font_size 0.40
    fill_rgb 0.0 0.0 0.0
    moveto 1.0 -0.866
    label Next start

    moveto 0.7 1.5
    label Augment eqn
    moveto 1.0 -0.359 
    label Solution 1
    moveto 1.0 +0.359
    label Solution 2

  endpage

endfile
