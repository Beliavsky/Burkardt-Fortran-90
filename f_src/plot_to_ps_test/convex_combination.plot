# convex_combination.plot  02 October 2013
#
file convex_combination.ps

  space -4.0 -3.0 8.0 8.0

  page

    line_rgb 0.8 0.8 0.8
    grid -4.0 -3.0 8.0 8.0 13 12
#
#  Red things.
#
    line_width 3

    fill_rgb 1.0 0.0 0.0
    moveto -4.0  8.0
    drawto  7.0 -3.0
#
#  Blue things
#
    fill_rgb 0.0 0.0 1.0

    moveto  0.0  4.0
    drawto  4.0  0.0

    circle_fill  0.0  4.0  0.35
    circle_fill  4.0  0.0  0.35
#
#  Green things
#
    fill_rgb 0.0 1.0 0.0

    circle_fill -3.0  7.0  0.25
    circle_fill  1.0  3.0  0.25
    circle_fill  2.0  2.0  0.25
    circle_fill  3.0  1.0  0.25
    circle_fill  5.0 -1.0  0.25
#
#  Black things
#
    fill_rgb 0.0 0.0 0.0

    line_width 3

    moveto  0.0 -3.0
    drawto  0.0  8.0
    moveto -4.0  0.0
    drawto  8.0  0.0

    font_size 0.40

    moveto -2.5 7.0
    label (-3/4, 7/4 )
    moveto 0.5 4.0
    label ( 0/4, 4/4 )
    moveto 1.5 3.0
    label ( 1/4, 3/4 )
    moveto 2.5 2.0
    label ( 1/2, 1/2 )
    moveto 3.5 1.0
    label ( 3/4, 1/4 )
    moveto 4.5 0.0
    label ( 4/4, 0/4 )
    moveto 5.5 -1.0
    label ( 5/4,-1/4 )

    moveto 3.0 -1.0
    label X1
    moveto -1.25 3.25
    label X2

  endpage

endfile
