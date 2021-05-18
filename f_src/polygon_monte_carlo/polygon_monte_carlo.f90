function angle_degree ( x1, y1, x2, y2, x3, y3 )

!*****************************************************************************80
!
!! ANGLE_DEGREE returns the degree angle defined by three points.
!
!  Discussion:
!
!        P1
!        /
!       /
!      /
!     /
!    P2--------->P3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real X1, Y1, X2, Y2, X3, Y3, the coordinates of the points
!    P1, P2, P3.
!
!    Output, real VALUE, the angle swept out by the rays, measured
!    in degrees.  0 <= VALUE < 360.  If either ray has zero length,
!    then VALUE is set to 0.
!
  implicit none

  real ( kind = 8 ) angle_degree
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) value
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) y
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3

  x = ( x3 - x2 ) * ( x1 - x2 ) + ( y3 - y2 ) * ( y1 - y2 )

  y = ( x3 - x2 ) * ( y1 - y2 ) - ( y3 - y2 ) * ( x1 - x2 )

  if ( x == 0.0D+00 .and. y == 0.0D+00 ) then

    value = 0.0D+00

  else

    value = atan2 ( y, x )

    if ( value < 0.0D+00 ) then
      value = value + 2.0D+00 * r8_pi
    end if

    value = 180.0D+00 * value / r8_pi

  end if

  angle_degree = value

  return
end
function between ( xa, ya, xb, yb, xc, yc )

!*****************************************************************************80
!
!! BETWEEN is TRUE if vertex C is between vertices A and B.
!
!  Discussion:
!
!    The points must be (numerically) collinear.
!
!    Given that condition, we take the greater of XA - XB and YA - YB
!    as a "scale" and check where C's value lies.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XA, YA, XB, YB, XC, YC, the coordinates of 
!    the vertices.
!
!    Output, logical ( kind = 4 ) BETWEEN, is TRUE if C is between A and B.
!
  implicit none

  logical ( kind = 4 ) between
  logical ( kind = 4 ) collinear
  logical ( kind = 4 ) value
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb
  real ( kind = 8 ) xc
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ) ya
  real ( kind = 8 ) yb
  real ( kind = 8 ) yc
  real ( kind = 8 ) ymax
  real ( kind = 8 ) ymin

  if ( .not. collinear ( xa, ya, xb, yb, xc, yc ) ) then
    value = .false.
  else if ( abs ( ya - yb ) < abs ( xa - xb ) ) then
    xmax = max ( xa, xb )
    xmin = min ( xa, xb )
    value = ( xmin <= xc .and. xc <= xmax )
  else
    ymax = max ( ya, yb )
    ymin = min ( ya, yb )
    value = ( ymin <= yc .and. yc <= ymax )
  end if

  between = value

  return
end
function collinear ( xa, ya, xb, yb, xc, yc )

!*****************************************************************************80
!
!! COLLINEAR returns a measure of collinearity for three points.
!
!  Discussion:
!
!    In order to deal with collinear points whose coordinates are not
!    numerically exact, we compare the area of the largest square
!    that can be created by the line segment between two of the points
!    to (twice) the area of the triangle formed by the points.
!
!    If the points are collinear, their triangle has zero area.
!    If the points are close to collinear, then the area of this triangle
!    will be small relative to the square of the longest segment.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2016
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XA, YA, XB, YB, XC, YC, the coordinates of 
!    the vertices.
!
!    Output, logical ( kind = 4 ) COLLINEAR, is TRUE if the points are judged 
!    to be collinear.
!
  implicit none

  real ( kind = 8 ) area
  logical ( kind = 4 ) collinear
  real ( kind = 8 ), parameter :: r8_eps = 2.220446049250313D-016
  real ( kind = 8 ) side_ab_sq
  real ( kind = 8 ) side_bc_sq
  real ( kind = 8 ) side_ca_sq
  real ( kind = 8 ) side_max_sq
  real ( kind = 8 ) triangle_area
  logical ( kind = 4 ) value
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb
  real ( kind = 8 ) xc
  real ( kind = 8 ) ya
  real ( kind = 8 ) yb
  real ( kind = 8 ) yc

  area = triangle_area ( xa, ya, xb, yb, xc, yc )

  side_ab_sq = ( xa - xb ) ** 2 + ( ya - yb ) ** 2
  side_bc_sq = ( xb - xc ) ** 2 + ( yb - yc ) ** 2
  side_ca_sq = ( xc - xa ) ** 2 + ( yc - ya ) ** 2

  side_max_sq = max ( side_ab_sq, max ( side_bc_sq, side_ca_sq ) )

  if ( side_max_sq <= r8_eps ) then
    value = .true.
  else if ( 2.0D+00 * abs ( area ) <= r8_eps * side_max_sq ) then
    value = .true.
  else
    value = .false.
  end if

  collinear = value

  return
end
function diagonal ( im1, ip1, n, prev_node, next_node, x, y )

!*****************************************************************************80
!
!! DIAGONAL: VERTEX(IM1) to VERTEX(IP1) is a proper internal diagonal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IM1, IP1, the indices of two vertices.
!
!    Input, integer ( kind = 4 ) N, the number of vertices.
!
!    Input, integer ( kind = 4 ) PREV_NODE(N), the previous neighbor of 
!    each vertex.
!
!    Input, integer ( kind = 4 ) NEXT_NODE(N), the next neighbor of each vertex.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of each vertex.
!
!    Output, logical ( kind = 4 ) DIAGONAL, the value of the test.
!
  implicit none

  integer ( kind = 4 ) n

  logical ( kind = 4 ) diagonal
  logical ( kind = 4 ) diagonalie
  integer ( kind = 4 ) im1
  logical ( kind = 4 ) in_cone
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) next_node(n)
  integer ( kind = 4 ) prev_node(n)
  logical ( kind = 4 ) value1
  logical ( kind = 4 ) value2
  logical ( kind = 4 ) value3
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  value1 = in_cone ( im1, ip1, n, prev_node, next_node, x, y )
  value2 = in_cone ( ip1, im1, n, prev_node, next_node, x, y )
  value3 = diagonalie ( im1, ip1, n, next_node, x, y )

  diagonal = ( value1 .and. value2 .and. value3 )

  return
end
function diagonalie ( im1, ip1, n, next_node, x, y )

!*****************************************************************************80
!
!! DIAGONALIE is true if VERTEX(IM1):VERTEX(IP1) is a proper diagonal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 May 2014
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IM1, IP1, the indices of two vertices.
!
!    Input, integer ( kind = 4 ) N, the number of vertices.
!
!    Input, integer ( kind = 4 ) NEXT_NODE(N), the next neighbor of each vertex.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of each vertex.
!
!    Output, logical ( kind = 4 ) DIAGONALIE, the value of the test.
!
  implicit none

  integer ( kind = 4 ) n

  logical ( kind = 4 ) diagonalie
  integer ( kind = 4 ) first
  integer ( kind = 4 ) im1
  logical ( kind = 4 ) intersect
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jp1
  integer ( kind = 4 ) next_node(n)
  logical ( kind = 4 ) value
  logical ( kind = 4 ) value2
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  first = im1
  j = first
  jp1 = next_node(first)

  value = .true.
!
!  For each edge VERTEX(J):VERTEX(JP1) of the polygon:
!
  do
!
!  Skip any edge that includes vertex IM1 or IP1.
!
    if ( j == im1 .or. j == ip1 .or. jp1 == im1 .or. jp1 == ip1 ) then

    else

      value2 = intersect ( x(im1), y(im1), x(ip1), y(ip1), x(j), y(j), &
        x(jp1), y(jp1) )

      if ( value2 ) then
        value = .false.
        exit
      end if

    end if

    j = jp1
    jp1 = next_node(j)

    if ( j == first ) then
      exit
    end if

  end do

  diagonalie = value

  return
end
function in_cone ( im1, ip1, n, prev_node, next_node, x, y )

!*****************************************************************************80
!
!! IN_CONE is TRUE if the diagonal VERTEX(IM1):VERTEX(IP1) is strictly internal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IM1, IP1, the indices of two vertices.
!
!    Input, integer ( kind = 4 ) N, the number of vertices.
!
!    Input, integer ( kind = 4 ) PREV_NODE(N), the previous neighbor of 
!    each vertex.
!
!    Input, integer ( kind = 4 ) NEXT_NODE(N), the next neighbor of each vertex.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of each vertex.
!
!    Output, logical ( kind = 4 ) IN_CONE, the value of the test.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) im1
  integer ( kind = 4 ) im2
  logical ( kind = 4 ) in_cone
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) next_node(n)
  integer ( kind = 4 ) prev_node(n)
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) t3
  real ( kind = 8 ) t4
  real ( kind = 8 ) t5
  real ( kind = 8 ) triangle_area
  logical ( kind = 4 ) value
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  im2 = prev_node(im1)
  i = next_node(im1)

  t1 = triangle_area ( x(im1), y(im1), x(i), y(i), x(im2), y(im2) )

  if ( 0.0D+00 <= t1 ) then

    t2 = triangle_area ( x(im1), y(im1), x(ip1), y(ip1), x(im2), y(im2) )
    t3 = triangle_area ( x(ip1), y(ip1), x(im1), y(im1), x(i), y(i) )
    value = ( ( 0.0D+00 < t2 ) .and. ( 0.0D+00 < t3 ) )

  else

    t4 = triangle_area ( x(im1), y(im1), x(ip1), y(ip1), x(i), y(i) )
    t5 = triangle_area ( x(ip1), y(ip1), x(im1), y(im1), x(im2), y(im2) )
    value = .not. ( ( 0.0D+00 <= t4 ) .and. ( 0.0D+00 <= t5 ) )

  end if

  in_cone = value

  return
end
function intersect ( xa, ya, xb, yb, xc, yc, xd, yd )

!*****************************************************************************80
!
!! INTERSECT is true if lines VA:VB and VC:VD intersect.
!
!  Discussion:
!
!    Thanks to Gene Dial for correcting the call to intersect_prop(),
!    08 September 2016.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2016
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XA, YA, XB, YB, XC, YC, XD, YD, the X and Y 
!    coordinates of the four vertices.
!
!    Output, logical ( kind = 4 ) VALUE, the value of the test.
!
  implicit none

  logical ( kind = 4 ) between
  logical ( kind = 4 ) intersect
  logical ( kind = 4 ) intersect_prop
  logical ( kind = 4 ) value
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb
  real ( kind = 8 ) xc
  real ( kind = 8 ) xd
  real ( kind = 8 ) ya
  real ( kind = 8 ) yb
  real ( kind = 8 ) yc
  real ( kind = 8 ) yd

  if ( intersect_prop ( xa, ya, xb, yb, xc, yc, xd, yd ) ) then
    value = .true.
  else if ( between ( xa, ya, xb, yb, xc, yc ) ) then
    value = .true.
  else if ( between ( xa, ya, xb, yb, xd, yd ) ) then
    value = .true.
  else if ( between ( xc, yc, xd, yd, xa, ya ) ) then
    value = .true.
  else if ( between ( xc, yc, xd, yd, xb, yb ) ) then
    value = .true.
  else
    value = .false.
  end if

  intersect = value

  return
end
function intersect_prop ( xa, ya, xb, yb, xc, yc, xd, yd )

!*****************************************************************************80
!
!! INTERSECT_PROP is TRUE if lines VA:VB and VC:VD have a proper intersection.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XA, YA, XB, YB, XC, YC, XD, YD, the X and Y 
!    coordinates of the four vertices.
!
!    Output, logical ( kind = 4 ) INTERSECT_PROP, the result of the test.
!
  implicit none

  logical ( kind = 4 ) collinear
  logical ( kind = 4 ) intersect_prop
  logical ( kind = 4 ) l4_xor
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) t3
  real ( kind = 8 ) t4
  real ( kind = 8 ) triangle_area
  logical ( kind = 4 ) value
  logical ( kind = 4 ) value1
  logical ( kind = 4 ) value2
  logical ( kind = 4 ) value3
  logical ( kind = 4 ) value4
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb
  real ( kind = 8 ) xc
  real ( kind = 8 ) xd
  real ( kind = 8 ) ya
  real ( kind = 8 ) yb
  real ( kind = 8 ) yc
  real ( kind = 8 ) yd

  if ( collinear ( xa, ya, xb, yb, xc, yc ) ) then
    value = .false.
  else if ( collinear ( xa, ya, xb, yb, xd, yd ) ) then
    value = .false.
  else if ( collinear ( xc, yc, xd, yd, xa, ya ) ) then
    value = .false.
  else if ( collinear ( xc, yc, xd, yd, xb, yb ) ) then
    value = .false.
  else

    t1 = triangle_area ( xa, ya, xb, yb, xc, yc )
    t2 = triangle_area ( xa, ya, xb, yb, xd, yd )
    t3 = triangle_area ( xc, yc, xd, yd, xa, ya )
    t4 = triangle_area ( xc, yc, xd, yd, xb, yb )

    value1 = ( 0.0D+00 < t1 )
    value2 = ( 0.0D+00 < t2 )
    value3 = ( 0.0D+00 < t3 )
    value4 = ( 0.0D+00 < t4 )

    value = ( l4_xor ( value1, value2 ) ) .and. ( l4_xor ( value3, value4 ) )
 
  end if

  intersect_prop = value

  return
end
function l4_xor ( l1, l2 )

!*****************************************************************************80
!
!! L4_XOR returns the exclusive OR of two L4's.
!
!  Discussion:
!
!    An L4 is a logical ( kind = 4 ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!   John Burkardt
!
!  Parameters:
!
!    Input, logical ( kind = 4 ) L1, L2, two values whose exclusive OR 
!    is needed.
!
!    Output, logical ( kind = 4 ) L4_XOR, the exclusive OR of L1 and L2.
!
  implicit none

  logical ( kind = 4 ) l1
  logical ( kind = 4 ) l2
  logical ( kind = 4 ) l4_xor
  logical ( kind = 4 ) value1
  logical ( kind = 4 ) value2

  value1 = (         l1   .and. ( .not. l2 ) )
  value2 = ( ( .not. l1 ) .and.         l2   )

  l4_xor = ( value1 .or. value2 )

  return
end
subroutine monomial_value ( m, n, e, x, value )

!*****************************************************************************80
!
!! MONOMIAL_VALUE evaluates a monomial.
!
!  Discussion:
!
!    This routine evaluates a monomial of the form
!
!      product ( 1 <= i <= m ) x(i)^e(i)
!
!    where the exponents are nonnegative integers.  Note that
!    if the combination 0^0 is encountered, it should be treated
!    as 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points at which the
!    monomial is to be evaluated.
!
!    Input, integer ( kind = 4 ) E(M), the exponents.
!
!    Input, real ( kind = 8 ) X(M,N), the point coordinates.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the monomial.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) e(m)
  integer ( kind = 4 ) i
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = 1.0D+00

  do i = 1, m
    if ( 0 /= e(i) ) then
      value(1:n) = value(1:n) * x(i,1:n) ** e(i)
    end if
  end do

  return
end
function polygon_area ( nv, v )

!*****************************************************************************80
!
!! POLYGON_AREA determines the area of a polygon.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NV, the number of vertices of the polygon.
!
!    Input, real ( kind = 8 ) V(2,NV), the vertex coordinates.
!
!    Output, real ( kind = 8 ) POLYGON_AREA, the area of the polygon.
!
  implicit none

  integer ( kind = 4 ) nv

  real ( kind = 8 ) area
  integer ( kind = 4 ) e(2)
  real ( kind = 8 ) polygon_area
  real ( kind = 8 ) v(2,nv)

  e(1) = 0
  e(2) = 0

  call polygon_monomial_integral ( nv, v, e, area )

  polygon_area = area

  return
end
function polygon_area2 ( n, x, y )

!*****************************************************************************80
!
!! POLYGON_AREA2 returns the area of a polygon.
!
!  Discussion:
!
!    The vertices should be listed in counter-clockwise order so that
!    the area will be positive.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2016
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of vertices.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the vertex coordinates.
!
!    Output, real ( kind = 8 ) POLYGON_AREA2, the area of the polygon.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) area
  integer ( kind = 4 ) i
  integer ( kind = 4 ) im1
  real ( kind = 8 ) polygon_area2
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  area = 0.0D+00
  im1 = n

  do i = 1, n
    area = area + x(im1) * y(i) - x(i) * y(im1)
    im1 = i
  end do

  area = 0.5D+00 * area

  polygon_area2 = area

  return
end
subroutine polygon_monomial_integral ( nv, v, e, nu_pq )

!*****************************************************************************80
!
!! POLYGON_MONOMIAL_INTEGRAL integrates a monomial over a polygon.
!
!  Discussion:
!
!    Nu(P,Q) = Integral ( x, y in polygon ) x^e(1) y^e(2) dx dy
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carsten Steger,
!    On the calculation of arbitrary moments of polygons,
!    Technical Report FGBV-96-05,
!    Forschungsgruppe Bildverstehen, Informatik IX,
!    Technische Universitaet Muenchen, October 1996.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NV, the number of vertices of the polygon.
!
!    Input, real ( kind = 8 ) V(2,NV), the vertex coordinates.
!
!    Input, integer ( kind = 4 ) E(2), the exponents of the monomial.
!
!    Output, real ( kind = 8 ) NU_PQ, the unnormalized moment Nu(P,Q).
!
  implicit none

  integer ( kind = 4 ) nv

  integer ( kind = 4 ) e(2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) nu_pq
  integer ( kind = 4 ) p
  integer ( kind = 4 ) q
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) s_pq
  real ( kind = 8 ) v(2,nv)
  real ( kind = 8 ) xi
  real ( kind = 8 ) xj
  real ( kind = 8 ) yi
  real ( kind = 8 ) yj

  p = e(1)
  q = e(2)

  nu_pq = 0.0D+00

  xj = v(1,nv)
  yj = v(2,nv)

  do i = 1, nv

    xi = v(1,i)
    yi = v(2,i)

    s_pq = 0.0D+00
    do k = 0, p
      do l = 0, q
        s_pq = s_pq &
          + r8_choose ( k + l, l ) * r8_choose ( p + q - k - l, q - l ) &
          * xi ** k * xj ** ( p - k ) &
          * yi ** l * yj ** ( q - l )
      end do
    end do

    nu_pq = nu_pq + ( xj * yi - xi * yj ) * s_pq

    xj = xi
    yj = yi

  end do

  nu_pq = nu_pq / real ( p + q + 2, kind = 8 ) &
    / real ( p + q + 1, kind = 8 ) &
    / r8_choose ( p + q, p )

  return
end
subroutine polygon_sample ( nv, v, n, seed, s )

!*****************************************************************************80
!
!! POLYGON_SAMPLE uniformly samples a polygon.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NV, the number of vertices.
!
!    Input, real ( kind = 8 ) V(2,NV), the vertices of the polygon, listed in
!    counterclockwise order.
!
!    Input, integer ( kind = 4 ) N, the number of points to create.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) S(2,N), the points.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) nv

  real ( kind = 8 ) area_cumulative(nv-2)
  real ( kind = 8 ) area_polygon
  real ( kind = 8 ) area_relative(nv-2)
  real ( kind = 8 ) area_triangle(nv-2)
  real ( kind = 8 ) area_percent
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r(2)
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) triangle_area
  integer ( kind = 4 ) triangles(3,nv-2)
  real ( kind = 8 ) s(2,n)
  real ( kind = 8 ) v(2,nv)
!
!  Triangulate the polygon.
!
  call polygon_triangulate ( nv, v(1,1:nv), v(2,1:nv), triangles )
!
!  Determine the areas of each triangle.
!
  do i = 1, nv - 2
    area_triangle(i) = triangle_area ( &
      v(1,triangles(1,i)), v(2,triangles(1,i)), &
      v(1,triangles(2,i)), v(2,triangles(2,i)), &
      v(1,triangles(3,i)), v(2,triangles(3,i)) )
  end do
!
!  Normalize the areas.
!
  area_polygon = sum ( area_triangle(1:nv-2) )
  area_relative(1:nv-2) = area_triangle(1:nv-2) / area_polygon
!
!  Replace each area by the sum of itself and all previous ones.
!
  area_cumulative(1) = area_relative(1)
  do i = 2, nv - 2
    area_cumulative(i) = area_relative(i) + area_cumulative(i-1)
  end do

  do j = 1, n
!
!  Choose triangle I at random, based on areas.
!
    area_percent = r8_uniform_01 ( seed )

    do k = 1, nv - 2

      i = k

      if ( area_percent <= area_cumulative(k) ) then
        exit
      end if

    end do
!
!  Now choose a point at random in triangle I.
!
    call r8vec_uniform_01 ( 2, seed, r )

    if ( 1.0D+00 < sum ( r(1:2) ) ) then
      r(1:2) = 1.0D+00 - r(1:2)
    end if

    s(1:2,j) = ( 1.0D+00 - r(1) - r(2) ) * v(1:2,triangles(1,i)) &
                         + r(1)          * v(1:2,triangles(2,i)) &
                                + r(2)   * v(1:2,triangles(3,i))
  end do

  return
end
subroutine polygon_triangulate ( n, x, y, triangles )

!*****************************************************************************80
!
!! POLYGON_TRIANGULATE determines a triangulation of a polygon.
!
!  Discussion:
!
!    There are N-3 triangles in the triangulation.
!
!    For the first N-2 triangles, the first edge listed is always an
!    internal diagonal.
!
!    Thanks to Gene Dial for pointing out a mistake in the area calculation,
!    10 September 2016.
!
!    Gene Dial requested an angle tolerance of about 1 millionth radian or 
!    5.7E-05 degrees, 26 June 2018.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2018
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of vertices.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of each vertex.
!
!    Output, integer ( kind = 4 ) TRIANGLES(3,N-2), the triangles of the 
!    triangulation.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) angle
  real ( kind = 8 ) angle_degree
  real ( kind = 8 ), parameter :: angle_tol = 5.7D-05
  real ( kind = 8 ) area
  logical ( kind = 4 ) diagonal
  logical ( kind = 4 ) ear(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i0
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) i4
  integer ( kind = 4 ) next_node(n)
  integer ( kind = 4 ) node
  integer ( kind = 4 ) node_m1
  integer ( kind = 4 ) node1
  integer ( kind = 4 ) node2
  integer ( kind = 4 ) node3
  real ( kind = 8 ) polygon_area2
  integer ( kind = 4 ) prev_node(n)
  integer ( kind = 4 ) triangle_num
  integer ( kind = 4 ) triangles(3,n-2)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  We must have at least 3 vertices.
!
  if ( n < 3 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POLYGON_TRIANGULATE - Fatal error!'
    write ( *, '(a)' ) '  N < 3.'
    stop 1
  end if
!
!  Consecutive vertices cannot be equal.
!
  node_m1 = n
  do node = 1, n
    if ( x(node_m1) == x(node) .and. y(node_m1) == y(node) ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POLYGON_TRIANGULATE - Fatal error!'
      write ( *, '(a)' ) '  Two consecutive nodes are identical.'
      stop 1
    end if
    node_m1 = node
  end do
!
!  No node can be the vertex of an angle less than 1 degree 
!  in absolute value.
!
  node1 = n

  do node2 = 1, n

    node3 = mod ( node2, n ) + 1

    angle = angle_degree ( &
      x(node1), y(node1), &
      x(node2), y(node2), &
      x(node3), y(node3) )

    if ( abs ( angle ) <= angle_tol ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYGON_TRIANGULATE - Fatal error!'
      write ( *, '(a,g14.6)' ) '  Polygon has an angle smaller than ', angle_tol
      write ( *, '(a,i4)' ) '  occurring at node ', node2
      stop 1
    end if

    node1 = node2

  end do
!
!  Area must be positive.
!
  area = polygon_area2 ( n, x, y )

  if ( area <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POLYGON_TRIANGULATE - Fatal error!'
    write ( *, '(a)' ) '  Polygon has zero or negative area.'
    stop 1
  end if
!
!  PREV and NEXT point to the previous and next nodes.
!
  i = 1
  prev_node(i) = n
  next_node(i) = i + 1

  do i = 2, n - 1
    prev_node(i) = i - 1
    next_node(i) = i + 1
  end do

  i = n
  prev_node(i) = i - 1
  next_node(i) = 1
!
!  EAR indicates whether the node and its immediate neighbors form an ear
!  that can be sliced off immediately.
!
  do i = 1, n
    ear(i) = diagonal ( prev_node(i), next_node(i), n, prev_node, next_node, &
      x, y )
  end do

  triangle_num = 0

  i2 = 1

  do while ( triangle_num < n - 3 )
!
!  If I2 is an ear, gather information necessary to carry out
!  the slicing operation and subsequent "healing".
!
    if ( ear(i2) ) then

      i3 = next_node(i2)
      i4 = next_node(i3)
      i1 = prev_node(i2)
      i0 = prev_node(i1)
!
!  Make vertex I2 disappear.
!
      next_node(i1) = i3
      prev_node(i3) = i1
!
!  Update the earity of I1 and I3, because I2 disappeared.
!
      ear(i1) = diagonal ( i0, i3, n, prev_node, next_node, x, y )
      ear(i3) = diagonal ( i1, i4, n, prev_node, next_node, x, y )
!
!  Add the diagonal [I3, I1, I2] to the list.
!
      triangle_num = triangle_num + 1
      triangles(1,triangle_num) = i3
      triangles(2,triangle_num) = i1
      triangles(3,triangle_num) = i2

    end if
!
!  Try the next vertex.
!
    i2 = next_node(i2)

  end do
!
!  The last triangle is formed from the three remaining vertices.
!
  i3 = next_node(i2)
  i1 = prev_node(i2)

  triangle_num = triangle_num + 1
  triangles(1,triangle_num) = i3
  triangles(2,triangle_num) = i1
  triangles(3,triangle_num) = i2

  return
end
function r8_choose ( n, k )

!*****************************************************************************80
!
!! R8_CHOOSE computes the binomial coefficient C(N,K) as an R8.
!
!  Discussion:
!
!    The value is calculated in such a way as to avoid overflow and
!    roundoff.  The calculation is done in R8 arithmetic.
!
!    The formula used is:
!
!      C(N,K) = N! / ( K! * (N-K)! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2008
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    ML Wolfson, HV Wright,
!    Algorithm 160:
!    Combinatorial of M Things Taken N at a Time,
!    Communications of the ACM,
!    Volume 6, Number 4, April 1963, page 161.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, K, are the values of N and K.
!
!    Output, real ( kind = 8 ) R8_CHOOSE, the number of combinations of N
!    things taken K at a time.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mn
  integer ( kind = 4 ) mx
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) value

  mn = min ( k, n - k )

  if ( mn < 0 ) then

    value = 0.0D+00

  else if ( mn == 0 ) then

    value = 1.0D+00

  else

    mx = max ( k, n - k )
    value = real ( mx + 1, kind = 8 )

    do i = 2, mn
      value = ( value * real ( mx + i, kind = 8 ) ) / real ( i, kind = 8 )
    end do

  end if

  r8_choose = value

  return
end
function r8_uniform_01 ( seed )

!*****************************************************************************80
!
!! R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer variable.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2^31 - 1 )
!      r8_uniform_01 = seed / ( 2^31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R8_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0. On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
  implicit none

  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_01 = real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
subroutine r8vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + 2147483647
    end if

    r(i) = real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! timestamp prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
function triangle_area ( xa, ya, xb, yb, xc, yc )

!*****************************************************************************80
!
!! TRIANGLE_AREA computes the signed area of a triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XA, YA, XB, YB, XC, YC, the coordinates of
!    the vertices of the triangle, given in counterclockwise order.
!
!    Output, real ( kind = 8 ) TRIANGLE_AREA, the signed area of the triangle.
!
  implicit none

  real ( kind = 8 ) triangle_area
  real ( kind = 8 ) value
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb
  real ( kind = 8 ) xc
  real ( kind = 8 ) ya
  real ( kind = 8 ) yb
  real ( kind = 8 ) yc

  value = 0.5D+00 * ( &
      ( xb - xa ) * ( yc - ya ) &
    - ( xc - xa ) * ( yb - ya ) )

  triangle_area = value

  return
end

