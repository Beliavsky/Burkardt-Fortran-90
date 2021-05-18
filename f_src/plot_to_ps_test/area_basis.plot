# area_basis.plot  03 October 2013
#
file area_basis.ps

  space -0.10 -0.10 1.05 1.05

  page
#
#  Do the filled regions first.
#
    fill_rgb 0.950  0.800  0.800
    polygon_fill
       0.0  0.0
       0.5  0.5
       0.3  0.9
       0.0  0.0
    endpolygon

    fill_rgb 0.800  0.950  0.800
    polygon_fill
       0.0  0.0
       1.0  0.2
       0.5  0.5
       0.0  0.0
    endpolygon

    fill_rgb 0.800  0.800  0.950
    polygon_fill
       1.0  0.2
       0.3  0.9
       0.5  0.5
       1.0  0.2
    endpolygon
#
#  Draw the triangle edges in black.
#
    line_rgb 0.0 0.0 0.0
    line_thick 8
    moveto  0.0  0.0
    drawto  1.0  0.2
    drawto  0.3  0.9
    drawto  0.0  0.0
#
#  Draw red lines from the vertices to P.
#
    line_rgb 1.0 0.0 0.0
    line_thick 8
    moveto  0.0  0.0
    drawto  0.5  0.5
    moveto  1.0  0.2
    drawto  0.5  0.5
    moveto  0.3  0.9
    drawto  0.5  0.5
#
#  Mark the vertices and P.
#
    line_thick 8
    fill_rgb 0.4 0.8 0.4
    circle_fill 0.0 0.0 0.025
    circle_fill 1.0 0.2 0.025
    circle_fill 0.3 0.9 0.025
    circle_fill 0.5 0.5 0.025
#
#  Label the vertices and P.
#
    font_size 0.35
    fill_rgb 0.0 0.0 0.0

    moveto -0.05  0.05
    label A
    moveto  1.0  0.25
    label B
    moveto  0.3  0.95
    label C
    moveto  0.5  0.55
    label P

    moveto  0.55  0.60
    label Area(P,B,C)
    moveto  0.20  0.50
    label Area(A,P,C)
    moveto  0.4  0.28
    label Area(A,B,P)

  endpage

endfile
