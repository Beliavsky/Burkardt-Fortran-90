# circle3.plot  06 April 2014
#
file circle3.ps

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
#  Green things
#
    fill_rgb 0.0 1.0 0.0
    circle_fill  0.5  -1.75  0.05

    circle_fill  0.5 -0.866  0.05
#
#  Red things
#
    fill_rgb 1.0 0.0 0.0
    arrow 0.5 -0.866 1.0    -1.732
    arrow 0.5 -0.866 1.366  -0.366
#
#  Black things.
#
    font_size 0.40
    fill_rgb 0.0 0.0 0.0

    moveto 1.20 -1.75
    label Normal
    moveto 1.20 -0.866
    label Tangent

  endpage

endfile
