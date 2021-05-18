# circle1.plot  06 April 2014
#
file circle1.ps

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
#
#  Red things
#
    fill_rgb 1.0 0.0 0.0
    circle_fill  0.5  -1.75  0.05
#
#  Black things.
#
    font_size 0.40
    fill_rgb 0.0 0.0 0.0

    moveto 0.75 -1.75
    label Starting point

  endpage

endfile
