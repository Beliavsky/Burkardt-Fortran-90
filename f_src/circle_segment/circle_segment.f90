subroutine circle_segment_angle_from_chord ( r, c, p1, p2, theta )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_ANGLE_FROM_CHORD computes the angle of a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
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
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) C(2), the center of the circle.
!
!    Input, real ( kind = 8 ) P1(2), P2(2), the ends of the chord.
!
!    Output, real ( kind = 8 ) THETA, the angle of the circle segment.
!    0 <= THETA < 2 * PI.
!
  implicit none

  real ( kind = 8 ) c(2)
  real ( kind = 8 ) p1(2)
  real ( kind = 8 ) p2(2)
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_atan
  real ( kind = 8 ) theta
  real ( kind = 8 ) v1(2)
  real ( kind = 8 ) v2(2)

  call r8_fake_use ( r )
!
!  Compute the radial vectors V1 and V2.
!
  v1(1:2) = p1(1:2) - c(1:2)
  v2(1:2) = p2(1:2) - c(1:2)
!
!  The arc cosine will only give us an answer between 0 and PI.
!
  theta = r8_atan ( v2(2), v2(1) ) - r8_atan ( v1(2), v1(1) )
!
!  Force 0 <= THETA < 2 * PI.
!
  do while ( theta < 0.0D+00 )
    theta = theta + 2.0D+00 * pi
  end do

  do while ( 2.0D+00 * pi <= theta )
    theta = theta - 2.0D+00 * pi
  end do

  return
end
subroutine circle_segment_angle_from_chord_angles ( omega1, omega2, theta )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_ANGLE_FROM_CHORD_ANGLES computes angle of a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) OMEGA1, OMEGA2, the angles of the points P1 
!    and P2.  OMEGA1 <= OMEGA2.
!
!    Output, real ( kind = 8 ) THETA, the angle of the circle segment.
!    Essentially, THETA = OMEGA2 - OMEGA1.
!
  implicit none

  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) omega1
  real ( kind = 8 ) omega2
  real ( kind = 8 ) theta

  do while ( omega2 < omega1 )
    omega2 = omega2 + 2.0D+00 * pi
  end do

  theta = omega2 - omega1

  return
end
subroutine circle_segment_angle_from_height ( r, h, theta )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_ANGLE_FROM_HEIGHT computes the angle of a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) H, the "height" of the circle segment.
!    0 <= H <= 2 * R.
!
!    Output, real ( kind = 8 ) THETA, the angle of the circle segment.
!
  implicit none

  real ( kind = 8 ) h
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_asin
! real ( kind = 8 ) r8_acos
  real ( kind = 8 ) theta

  if ( h <= 0.0D+00 ) then

    theta = 0.0D+00

  else if ( h <= r ) then

!   theta = 2.0D+00 * r8_acos ( ( r - h ) / r )
    theta = 2.0D+00 * r8_asin ( sqrt ( r * r - ( r - h ) * ( r - h ) ) / r )

  else if ( h <= 2.0D+00 * r ) then

!   theta = 2.0D+00 * r8_acos ( ( r - h ) / r )
    theta = 2.0D+00 * r8_asin ( sqrt ( r * r - ( r - h ) * ( r - h ) ) / r )
    theta = 2.0D+00 * pi - theta

  else

    theta = 2.0D+00 * pi

  end if

  return
end
subroutine circle_segment_area_from_angle ( r, theta, area )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_AREA_FROM_ANGLE computes the area of a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) THETA, the angle of the circle segment.
!
!    Output, real ( kind = 8 ) AREA, the area of the circle segment.
!
  implicit none

  real ( kind = 8 ) area
  real ( kind = 8 ) r
  real ( kind = 8 ) theta

  area = r * r * ( theta - sin ( theta ) ) / 2.0D+00

  return
end
subroutine circle_segment_area_from_chord ( r, c, p1, p2, area )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_AREA_FROM_CHORD computes the area of a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) C(2), the center of the circle.
!
!    Input, real ( kind = 8 ) P1(2), P2(2), the ends of the chord.
!
!    Output, real ( kind = 8 ) AREA, the area of the circle segment.
!
  implicit none

  real ( kind = 8 ) area
  real ( kind = 8 ) c(2)
  real ( kind = 8 ) p1(2)
  real ( kind = 8 ) p2(2)
  real ( kind = 8 ) r
  real ( kind = 8 ) theta

  call circle_segment_angle_from_chord ( r, c, p1, p2, theta )

  area = r * r * ( theta - sin ( theta ) ) / 2.0D+00

  return
end
subroutine circle_segment_area_from_height ( r, h, area )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_AREA_FROM_HEIGHT computes the area of a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) H, the height of the circle segment.
!    0 <= H <= 2 * R.
!
!    Output, real ( kind = 8 ) AREA, the area of the circle segment.
!
  implicit none

  real ( kind = 8 ) area
  real ( kind = 8 ) h
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_asin
  real ( kind = 8 ) theta

  if ( h <= 0.0D+00 ) then

    area = 0.0D+00

  else if ( h <= r ) then

    theta = 2.0D+00 * r8_asin ( sqrt ( r * r - ( r - h ) * ( r - h ) ) / r )
    area = r * r * ( theta - sin ( theta ) ) / 2.0D+00

  else if ( h <= 2.0D+00 * r ) then

    theta = 2.0D+00 * r8_asin ( sqrt ( r * r - ( r - h ) * ( r - h ) ) / r )
    theta = 2.0D+00 * pi - theta
    area = r * r * ( theta - sin ( theta ) ) / 2.0D+00

  else

    area = pi * r * r

  end if

  return
end
subroutine circle_segment_area_from_sample ( r, c, p1, p2, n, seed, area )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_AREA_FROM_SAMPLE computes the area of a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) C(2,1), the center of the circle.
!
!    Input, real ( kind = 8 ) P1(2,1), P2(2,1), the ends of the chord.
!
!    Input, integer ( kind = 4 ) N, the number of sample points.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) AREA, the area of the circle segment.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) angle(n)
  real ( kind = 8 ) area
  real ( kind = 8 ) c(2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  real ( kind = 8 ) omega1
  real ( kind = 8 ) omega2
  real ( kind = 8 ) p(2)
  real ( kind = 8 ) p1(2)
  real ( kind = 8 ) p2(2)
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r2(n)
  real ( kind = 8 ) r8_atan
  real ( kind = 8 ) rmh
  integer ( kind = 4 ) seed
  real ( kind = 8 ) vdotp(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Determine the angles of the chord endpoints.
!
  omega1 = r8_atan ( p1(2) - c(2), p1(1) - c(1) )
  do while ( omega1 < 0.0D+00 )
    omega1 = omega1 + 2.0D+00 * pi
  end do

  omega2 = r8_atan ( p2(2) - c(2), p2(1) - c(1) );
  do while ( omega2 < omega1 )
    omega2 = omega2 + 2.0D+00 * pi
  end do
!
!  Get N random points in the circle.
!  To simplify angle measurement, take OMEGA1 as your smallest angle.
!  That way, the check OMEGA1 <= ANGLE <= OMEGA2 will be legitimate.
!
  call r8vec_uniform_01 ( n, seed, angle )
  angle(1:n) = omega1 + 2.0D+00 * pi * angle(1:n)
  
  call r8vec_uniform_01 ( n, seed, r2 )
  r2(1:n) = sqrt ( r2(1:n) )

  x(1:n) = c(1) + r2(1:n) * cos ( angle(1:n) )
  y(1:n) = c(2) + r2(1:n) * sin ( angle(1:n) )
!
!  Determine the vector that touches the circle segment base.
!
  p(1:2) = 0.5D+00 * ( p1(1:2) + p2(1:2) ) - c(1:2)

  rmh = sqrt ( p(1)**2 + p(2)**2 )
  p(1:2) = p(1:2) / rmh

  if ( pi < omega2 - omega1 ) then
    p(1:2) = - p(1:2)
    rmh =  - rmh
  end if
!
!  Compute the projection of each point onto P.
!
  vdotp(1:n) = ( x(1:n) - c(1) ) * p(1) + ( y(1:n) - c(2) ) * p(2)
!
!  Points in the segment lie in the sector, and project at least RMH onto P.
!
  m = 0
  do i = 1, n
    if ( omega1 < angle(i) .and. &
                  angle(i) < omega2 .and. &
         rmh < vdotp(i) ) then
      m = m + 1
    end if
  end do
!
!  The area of the segment is its relative share of the circle area.
!
  area = pi * r**2 * real ( m, kind = 8 ) / real ( n, kind = 8 )

  return
end
subroutine circle_segment_cdf ( r, h, h2, cdf )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_CDF computes a CDF related to a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!    Now, suppose we want to assign a cumulative density function or CDF
!    based on a variable H2 which measures the height of the circle segment
!    formed by an arbitrary point in the circle segment.  CDF(H2) will
!    measure the probability that a point drawn uniformly at random from
!    the circle segment defines a (smaller) circle segment of height H2.
!
!    If we can define this CDF, then we will be able to sample uniformly
!    from the circle segment, since our "Y" value can be determined from H2,
!    and our X value is chosen uniformly over the horizontal chord 
!    through (0,Y).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) H, the "height" of the circle segment.
!    0 <= H <= 2 * R.
!
!    Input, real ( kind = 8 ) H2, the "height" of the new circle segment 
!    defined by a given point in the circle segment.  0 <= H2 <= H.
!
!    Output, real ( kind = 8 ) CDF, the cumulative density function for H2, 
!    the probability that a point chosen at random in the circle segment 
!    would define a smaller circle segment of height H2 or less.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) cdf
  real ( kind = 8 ) h
  real ( kind = 8 ) h2
  real ( kind = 8 ) r

  if ( h2 <= 0.0D+00 ) then
    cdf = 0.0D+00
  else if ( h <= h2 ) then
    cdf = 1.0D+00
  else
    call circle_segment_area_from_height ( r, h,  a  )
    call circle_segment_area_from_height ( r, h2, a2 )
    cdf = a2 / a
  end if

  return
end
subroutine circle_segment_centroid_from_chord ( r, c, p1, p2, d )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_CENTROID_FROM_CHORD computes the centroid of a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!    For this function, we assume that the center of the circle is at (0,0),
!    that the chord is horizontal, and that the circle segment is at the top.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) C(2), the center of the circle.
!
!    Input, real ( kind = 8 ) P1(2), P2(2), the coordinates of the endpoints 
!    of the chord.
!
!    Output, real ( kind = 8 ) D(2), the coordinates of the centroid.
!
  implicit none

  real ( kind = 8 ) c(2)
  real ( kind = 8 ) d(2)
  real ( kind = 8 ) p1(2)
  real ( kind = 8 ) p2(2)
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) theta
  real ( kind = 8 ) thetah
  real ( kind = 8 ) v1(2)
!
!  Get the angle subtended by P1:P2.
!
  call circle_segment_angle_from_chord ( r, c, p1, p2, theta )
!
!  Construct V1, the vector from C to P1.
!
  v1(1:2) = p1(1:2) - c(1:2)
!
!  Rotate V1 through THETA / 2.
!
  thetah = theta / 2.0D+00

  d(1) = cos ( thetah ) * v1(1) - sin ( thetah ) * v1(2)
  d(2) = sin ( thetah ) * v1(1) + cos ( thetah ) * v1(2)
!
!  Scale this vector so it represents the distance to the centroid
!  relative to R.
!
  s = 4.0D+00 * ( sin ( theta / 2.0 ) ) ** 3 &
    / 3.0D+00 / ( theta - sin ( theta ) )

  d(1) = s * d(1)
  d(2) = s * d(2)
!
!  Add the center.
!
  d(1) = d(1) + c(1)
  d(2) = d(2) + c(2)

  return
end
subroutine circle_segment_centroid_from_height ( r, h, d )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_CENTROID_FROM_HEIGHT computes centroid of a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!    For this function, we assume that the center of the circle is at (0,0).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) H, the "height" of the circle segment.
!    0 <= H <= 2 * R.
!
!    Output, real ( kind = 8 ) D(2), the coordinates of the centroid.
!
  implicit none

  real ( kind = 8 ) d(2)
  real ( kind = 8 ) h
  real ( kind = 8 ) r
  real ( kind = 8 ) theta

  call circle_segment_angle_from_height ( r, h, theta )

  d(1) = 0.0D+00
  d(2) = 4.0D+00 * r * ( sin ( theta / 2.0D+00 ) ) ** 3 / 3.0D+00 &
    / ( theta - sin ( theta ) )

  return
end
subroutine circle_segment_centroid_from_sample ( r, c, p1, p2, n, seed, d )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_CENTROID_FROM_SAMPLE estimates a circle segment centroid.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) C(2), the center of the circle.
!
!    Input, real ( kind = 8 ) P1(2), P2(2), the ends of the chord.
!
!    Input, integer ( kind = 4 ) N, the number of sample points.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) D(2), the estimated centroid of the 
!    circle segment.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) c(2)
  real ( kind = 8 ) d(2)
  real ( kind = 8 ) p1(2)
  real ( kind = 8 ) p2(2)
  real ( kind = 8 ) r
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call circle_segment_sample_from_chord ( r, c, p1, p2, n, seed, x, y )

  d(1) = sum ( x(1:n) ) / real ( n, kind = 8 )
  d(2) = sum ( y(1:n) ) / real ( n, kind = 8 )

  return
end
subroutine circle_segment_contains_point ( r, c, omega1, omega2, xy, value )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_CONTAINS_POINT reports whether a point is in a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!    In this function, we allow the circle to have an arbitrary center C,
!    arbitrary radius R, and we describe the points P1 and P2 by specifying
!    their angles OMEGA1 and OMEGA2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) C(2), the center of the circle.
!
!    Input, real ( kind = 8 ) OMEGA1, OMEGA2, the angles of the two points on 
!    the circumference of the circle that define the circle segment.
!    OMEGA1 < OMEGA2 <= OMEGA1 + 2 * PI
!
!    Input, real ( kind = 8 ) XY(2), a point.
!
!    Output, integer ( kind = 4 ) VALUE, is TRUE if the point is inside 
!    the circle segment.
!
  implicit none

  real ( kind = 8 ) c(2)
  real ( kind = 8 ) h
  real ( kind = 8 ) omega1
  real ( kind = 8 ) omega2
  real ( kind = 8 ) omegah
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) theta
  real ( kind = 8 ) v(2)
  real ( kind = 8 ) v_omega
  real ( kind = 8 ) v_project
  real ( kind = 8 ) v_r
  integer ( kind = 4 ) value
  real ( kind = 8 ) xy(2)

  if ( r <= 0.0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CIRCLE_SEGMENT_CONTAINS_POINT - Fatal error!'
    write ( *, '(a)' ) '  R <= 0.0.'
    stop
  end if

  do while ( omega2 < omega1 )
    omega2 = omega2 + 2.0D+00 * pi
  end do
!
!  Compute the vector V = XY - C:
!
  v(1:2) = xy(1:2) - c(1:2)
!
!  a: Point must be inside the circle, so ||V|| <= R.
!
  v_r = sqrt ( v(1) ** 2 + v(2) ** 2 )

  if ( r < v_r ) then
    value = 0
    return
  end if
!
!  b: Angle made by the vector V must be between OMEGA1 and OMEGA2.
!
  v_omega = atan2 ( v(2), v(1) )

  do while ( omega1 <= v_omega + 2.0D+00 * pi )
    v_omega = v_omega - 2.0D+00 * pi
  end do

  do while ( v_omega + 2.0D+00 * pi <= omega1 )
    v_omega = v_omega + 2.0D+00 * pi
  end do

  if ( omega2 < v_omega ) then
    value = 0
    return
  end if
!
!  c: Projection of V onto unit centerline must be at least R-H.
!
  omegah = 0.5D+00 * ( omega1 + omega2 )
  v_project = v(1) * cos ( omegah ) + v(2) * sin ( omegah )

  theta = omega2 - omega1
  call circle_segment_height_from_angle ( r, theta, h )

  if ( v_project < r - h ) then
    value = 0
    return
  end if

  value = 1
  
  return
end
subroutine circle_segment_height_from_angle ( r, angle, h )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_HEIGHT_FROM_ANGLE: height of a circle segment from angle.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!    This function is given the radius R and angle of the segment, and
!    determines the corresponding height.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) ANGLE, the angle of the circle segment.
!    0 <= ANGLE <= 2.0 * PI.
!
!    Output, real ( kind = 8 ) H, the height of the circle segment.
!
  implicit none

  real ( kind = 8 ) angle
  real ( kind = 8 ) h
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r

  if ( angle < 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CIRCLE_SEGMENT_HEIGHT_FROM_ANGLE - Fatal error!'
    write ( *, '(a)' ) '  ANGLE < 0.0.'
    stop
  end if

  if ( angle == 0.0D+00 ) then
    h = 0.0D+00
    return
  end if

  if ( angle == 2.0D+00 * pi ) then
    h = 2.0D+00 * r
    return
  end if

  if ( 2.0D+00 * pi < angle ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CIRCLE_SEGMENT_HEIGHT_FROM_ANGLE - Fatal error!'
    write ( *, '(a)' ) '  2.0 * pi < ANGLE.'
    stop
  end if

  if ( r <= 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CIRCLE_SEGMENT_HEIGHT_FROM_ANGLE - Fatal error!'
    write ( *, '(a)' ) '  R <= 0.0.'
    stop
  end if

  if ( angle <= pi ) then
    h = r * ( 1.0D+00 - cos (                  angle   / 2.0D+00 ) )
  else
    h = r * ( 1.0D+00 + cos ( ( 2.0D+00 * pi - angle ) / 2.0D+00 ) )
  end if

  return
end
subroutine circle_segment_height_from_area ( r, area, h )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_HEIGHT_FROM_AREA: height of a circle segment from area.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!    This function is given the radius R and area of the segment, and
!    determines the corresponding height.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) AREA, the area of the circle segment.
!    0 <= AREA <= 2.0 * PI * R^2.
!
!    Output, real ( kind = 8 ) H, the height of the circle segment.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a1
  real ( kind = 8 ) a2
  real ( kind = 8 ) area
  real ( kind = 8 ) area_circle
  real ( kind = 8 ) eps
  real ( kind = 8 ) h
  real ( kind = 8 ) h1
  real ( kind = 8 ) h2
  integer ( kind = 4 ) it
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_epsilon

  if ( area < 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CIRCLE_SEGMENT_HEIGHT_FROM_AREA - Fatal error!'
    write ( *, '(a)' ) '  AREA < 0.0.'
    stop
  end if

  area_circle = 2.0D+00 * pi * r ** 2

  if ( area == 0.0D+00 ) then
    h = 0.0D+00
    return
  end if

  if ( area == area_circle ) then
    h = 2.0D+00 * r
    return
  end if

  if ( area_circle < area ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CIRCLE_SEGMENT_HEIGHT_FROM_AREA - Fatal error!'
    write ( *, '(a)' ) '  2.0 * pi * r^2 < AREA.'
    stop
  end if

  if ( r <= 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CIRCLE_SEGMENT_HEIGHT_FROM_AREA - Fatal error!'
    write ( *, '(a)' ) '  R <= 0.0.'
    stop
  end if

  h1 = 0.0D+00
  call circle_segment_area_from_height ( r, h1, a1 )
  h2 = 2.0D+00 * r
  call circle_segment_area_from_height ( r, h2, a2 )

  it = 0
  eps = r8_epsilon ( )

  do while ( it < 30 )

    h = 0.5D+00 * ( h1 + h2 )
    call circle_segment_area_from_height ( r, h, a )
    it = it + 1

    if ( abs ( a - area ) < sqrt ( eps ) * area ) then
      exit
    end if

    if ( a < area ) then
      h1 = h
      a1 = a
    else
      h2 = h
      a2 = a
    end if

  end do

  return
end
subroutine circle_segment_height_from_chord ( r, c, p1, p2, h )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_HEIGHT_FROM_CHORD: height of a circle segment from chord.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) C(2), the coordinates of the circle center.
!
!    Input, real ( kind = 8 ) P1(2), P2(2), the coordinates of the 
!    chord endpoints.
!
!    Output, real ( kind = 8 ) H, the height of the circle segment.
!
  implicit none

  real ( kind = 8 ) c(2)
  real ( kind = 8 ) h
  real ( kind = 8 ) p1(2)
  real ( kind = 8 ) p2(2)
  real ( kind = 8 ) r
  real ( kind = 8 ) theta

  call circle_segment_angle_from_chord ( r, c, p1, p2, theta )

  call circle_segment_height_from_angle ( r, theta, h )

  return
end
subroutine circle_segment_rotation_from_chord ( r, c, p1, p2, alpha )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_ROTATION_FROM_CHORD computes the rotation of a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 July 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) C(2), the center of the circle.
!
!    Input, real ( kind = 8 ) P1(2), P2(2), the ends of the chord.
!    Warning! If P1 = P2, we can't tell whether the segment is the whole
!    circle or none of it!
!
!    Output, real ( kind = 8 ) ALPHA, the rotation of the circle segment.
!    0 <= ALPHA < 2 * PI.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) c(2)
  real ( kind = 8 ) p1(2)
  real ( kind = 8 ) p2(2)
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_atan
  real ( kind = 8 ) rho1
  real ( kind = 8 ) rho2
  real ( kind = 8 ) theta
  real ( kind = 8 ) v1(2)
  real ( kind = 8 ) v2(2)

  call r8_fake_use ( r )
!
!  Compute the radial vectors V1 and V2.
!
  v1(1:2) = p1(1:2) - c(1:2)
  v2(1:2) = p2(1:2) - c(1:2)
!
!  Use R8_ATAN to guarantee that 0 <= RHO1, RHO2 <= 2 * PI.
!
  rho1 = r8_atan ( v1(2), v1(1) )
  rho2 = r8_atan ( v2(2), v2(1) )
!
!  Force RHO2 to be bigger than RHO1.
!
  do while ( rho2 <= rho1 )
    rho2 = rho2 + 2.0D+00 * pi
  end do
!
!  Compute THETA.
!
  theta = rho2 - rho1
!
!  ALPHA is RHO1, plus half of the angular distance between P1 and P2.
!
  alpha = rho1 + 0.5D+00 * theta

  do while ( 2.0D+00 * pi <= alpha )
    alpha = alpha - 2.0D+00 * pi
  end do

  return
end
subroutine circle_segment_sample_from_chord ( r, c, p1, p2, n, seed, x, y )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_SAMPLE_FROM_CHORD samples points from a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) C(2), the center of the circle.
!
!    Input, real ( kind = 8 ) P1(2), P2(2), the endpoints of the chord.
!
!    Input, integer ( kind = 4 ) N, the number of sample points.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), Y(N), the sample points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) c(2)
  real ( kind = 8 ) c2(2)
  real ( kind = 8 ) eta(n)
  real ( kind = 8 ) h
  real ( kind = 8 ) p1(2)
  real ( kind = 8 ) p2(2)
  real ( kind = 8 ) r
  integer ( kind = 4 ) seed
  real ( kind = 8 ) vc(2)
  real ( kind = 8 ) vr(2)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xi(n)
  real ( kind = 8 ) y(n)
!
!  Determine unit vectors VR and VC.
!  VR points to the center of the chord from the radius.
!  VC points along the chord, from P1 to P2.
!
  vr(1:2) = 0.5D+00 * ( p1(1:2) + p2(1:2) ) - c(1:2)
  vr(1:2) = vr(1:2) / sqrt ( vr(1)**2 + vr(2)**2 )
  vc(1:2) = p2(1:2) - p1(1:2)
  vc(1:2) = vc(1:2) / sqrt ( vc(1)**2 + vc(2)**2 )
!
!  Get the height of the circle segment.
!
  c2 = (/ 0.0D+00, 0.0D+00 /)
  call circle_segment_height_from_chord ( r, c2, p1, p2, h )
!
!  Sample (xi,eta) in the reference coordinates, where the chord
!  is horizontal.
!
  call circle_segment_sample_from_height ( r, h, n, seed, xi, eta )
!
!  XI is the left/right coordinate along VC.
!  ETA is the distance along VR.
!
  x(1:n) = c(1) + eta(1:n) * vr(1) + xi(1:n) * vc(1)
  y(1:n) = c(2) + eta(1:n) * vr(2) + xi(1:n) * vc(2)

  return
end
subroutine circle_segment_sample_from_height ( r, h, n, seed, x, y )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_SAMPLE_FROM_HEIGHT samples points from a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) H, the height of the circle segment.
!    0 <= H <= 2 * R.
!
!    Input, integer ( kind = 4 ) N, the number of sample points.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), Y(N), the sample points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) area
  real ( kind = 8 ) area2(n)
  real ( kind = 8 ) h
  real ( kind = 8 ) h2(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) wh(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call circle_segment_area_from_height ( r, h, area )
!
!  Pick CDF's randomly.
!
  call r8vec_uniform_01 ( n, seed, u )
!
!  Choose points randomly by choosing ordered areas randomly.
!
  area2(1:n) = u(1:n) * area
!
!  Each area corresponds to a height H2.  Find it.
!
  do i = 1, n
    call circle_segment_height_from_area ( r, area2(i), h2(i) )
  end do
!
!  Determine the half-width WH of the segment for each H2.
!
  wh(1:n) = sqrt ( h2(1:n) * ( 2.0 * r - h2(1:n) ) )
!
!  Choose an X position randomly in [-WH,+WH].
!
  call r8vec_uniform_01 ( n, seed, u )
  
  x(1:n) = ( 2.0D+00 * u(1:n) - 1.0 ) * wh(1:n)
!
!  Our circle center is at (0,0).  Our height of H2 is subtracted
!  from the height R at the peak of the circle.  Determine the Y
!  coordinate using this fact.
!
  y(1:n) = r - h2(1:n)

  return
end
subroutine circle_segment_width_from_height ( r, h, w )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_WIDTH_FROM_HEIGHT computes the width of a circle segment.
!
!  Discussion:
!
!    Begin with a circle of radius R.  Choose two points P1 and P2 on the
!    circle, and draw the chord P1:P2.  This chord divides the circle
!    into two pieces, each of which is called a circle segment.
!    Consider one of the pieces.  The "angle" of this segment is the angle 
!    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
!    the chord P1:P2 which is closest to C.  The "height" of the segment
!    is the distance from Q to the perimeter of the circle.  The "width"
!    of the circle segment is the length of P1:P2.
!
!    This function is given the radius R and height H of the segment, and
!    determines the corresponding width W.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the circle.
!    0 < R.
!
!    Input, real ( kind = 8 ) H, the height of the circle segment.
!    0 <= H <= 2 * R.
!
!    Output, real ( kind = 8 ) W, the width of the circle segment.
!
  implicit none

  real ( kind = 8 ) h
  real ( kind = 8 ) r
  real ( kind = 8 ) w

  w = 2.0D+00 * sqrt ( h * ( 2.0D+00 * r - h ) )

  return
end
subroutine filename_inc ( filename )

!*****************************************************************************80
!
!! FILENAME_INC increments a partially numeric filename.
!
!  Discussion:
!
!    It is assumed that the digits in the name, whether scattered or
!    connected, represent a number that is to be increased by 1 on
!    each call.  If this number is all 9's on input, the output number
!    is all 0's.  Non-numeric letters of the name are unaffected.
!
!    If the name is empty, then the routine stops.
!
!    If the name contains no digits, the empty string is returned.
!
!  Example:
!
!      Input            Output
!      -----            ------
!      'a7to11.txt'     'a7to12.txt'
!      'a7to99.txt'     'a8to00.txt'
!      'a9to99.txt'     'a0to00.txt'
!      'cat.txt'        ' '
!      ' '              STOP!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) FILENAME.
!    On input, a character string to be incremented.
!    On output, the incremented string.
!
  implicit none

  character c
  integer ( kind = 4 ) change
  integer ( kind = 4 ) digit
  character ( len = * ) filename
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lens

  lens = len_trim ( filename )

  if ( lens <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILENAME_INC - Fatal error!'
    write ( *, '(a)' ) '  The input string is empty.'
    stop
  end if

  change = 0

  do i = lens, 1, -1

    c = filename(i:i)

    if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

      change = change + 1

      digit = ichar ( c ) - 48
      digit = digit + 1

      if ( digit == 10 ) then
        digit = 0
      end if

      c = char ( digit + 48 )

      filename(i:i) = c

      if ( c /= '0' ) then
        return
      end if

    end if

  end do
!
!  No digits were found.  Return blank.
!
  if ( change == 0 ) then
    filename = ' '
    return
  end if

  return
end
subroutine gauss ( n, alpha, beta, x, w )

!*****************************************************************************80
!
!! GAUSS computes a Gauss quadrature rule.
!
!  Discussion:
!
!    Given a weight function W encoded by the first N recurrence coefficients 
!    ALPHA and BETA for the associated orthogonal polynomials, the call 
!      call gauss ( n, alpha, beta, x, w ) 
!    generates the nodes and weights of the N-point Gauss quadrature rule 
!    for the weight function W.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 July 2013
!
!  Author:
!
!    Original MATLAB version by Walter Gautschi.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Walter Gautschi,
!    Orthogonal Polynomials: Computation and Approximation,
!    Oxford, 2004,
!    ISBN: 0-19-850672-4,
!    LC: QA404.5 G3555.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the desired quadrature rule.
!
!    Input, real ( kind = 8 ) ALPHA(N), BETA(N), the alpha and beta recurrence 
!    coefficients for the othogonal polynomials associated with the
!    weight function.
!
!    Output, real ( kind = 8 ) X(N), W(N), the nodes and  weights of the desired 
!    quadrature rule.  The nodes are listed in increasing order.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ) beta(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) rot_num
  real ( kind = 8 ) t
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)
!
!  Define the tridiagonal Jacobi matrix.
!
  a(1:n,1:n) = 0.0D+00

  do i = 1, n
    a(i,i) = alpha(i)
  end do

  do i = 2, n
    t = sqrt ( beta(i) )
    a(i,i-1) = t
    a(i-1,i) = t
  end do
!
!  Get the eigenvectors and eigenvalues.
!
  it_max = 100

  call jacobi_eigenvalue ( n, a, it_max, v, x, it_num, rot_num )

  w(1:n) = beta(1) * v(1,1:n)**2

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen
 
  iunit = 0
 
  do i = 1, 99
 
    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )
 
      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if
 
  end do

  return
end
subroutine jacobi_eigenvalue ( n, a, it_max, v, d, it_num, rot_num )

!*****************************************************************************80
!
!! JACOBI_EIGENVALUE carries out the Jacobi eigenvalue iteration.
!
!  Discussion:
!
!    This function computes the eigenvalues and eigenvectors of a
!    real symmetric matrix, using Rutishauser's modfications of the classical
!    Jacobi rotation method with threshold pivoting. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 July 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix, which must be square, real,
!    and symmetric.
!
!    Input, integer ( kind = 4 ) IT_MAX, the maximum number of iterations.
!
!    Output, real ( kind = 8 ) V(N,N), the matrix of eigenvectors.
!
!    Output, real ( kind = 8 ) D(N), the eigenvalues, in descending order.
!
!    Output, integer ( kind = 4 ) IT_NUM, the total number of iterations.
!
!    Output, integer ( kind = 4 ) ROT_NUM, the total number of rotations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) bw(n)
  real ( kind = 8 ) c
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) g
  real ( kind = 8 ) gapq
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) p
  integer ( kind = 4 ) q
  integer ( kind = 4 ) rot_num
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) tau
  real ( kind = 8 ) term
  real ( kind = 8 ) termp
  real ( kind = 8 ) termq
  real ( kind = 8 ) theta
  real ( kind = 8 ) thresh
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) zw(n)

  do j = 1, n
    do i = 1, n
      if ( i == j ) then
        v(i,j) = 1.0D+00
      else
        v(i,j) = 0.0D+00
      end if
    end do
  end do

  do i = 1, n
    d(i) = a(i,i)
  end do

  bw(1:n) = d(1:n)
  zw(1:n) = 0.0D+00
  it_num = 0
  rot_num = 0

  do while ( it_num < it_max )

    it_num = it_num + 1
!
!  The convergence threshold is based on the size of the elements in
!  the strict upper triangle of the matrix.
!
    thresh = 0.0D+00
    do j = 1, n
      do i = 1, j - 1
        thresh = thresh + a(i,j) ** 2
      end do
    end do

    thresh = sqrt ( thresh ) / real ( 4 * n, kind = 8 )

    if ( thresh == 0.0D+00 ) then
      exit 
    end if

    do p = 1, n
      do q = p + 1, n

        gapq = 10.0D+00 * abs ( a(p,q) )
        termp = gapq + abs ( d(p) )
        termq = gapq + abs ( d(q) )
!
!  Annihilate tiny offdiagonal elements.
!
        if ( 4 < it_num .and. &
             termp == abs ( d(p) ) .and. &
             termq == abs ( d(q) ) ) then

          a(p,q) = 0.0D+00
!
!  Otherwise, apply a rotation.
!
        else if ( thresh <= abs ( a(p,q) ) ) then

          h = d(q) - d(p)
          term = abs ( h ) + gapq

          if ( term == abs ( h ) ) then
            t = a(p,q) / h
          else
            theta = 0.5D+00 * h / a(p,q)
            t = 1.0D+00 / ( abs ( theta ) + sqrt ( 1.0D+00 + theta * theta ) )
            if ( theta < 0.0D+00 ) then 
              t = - t
            end if
          end if

          c = 1.0D+00 / sqrt ( 1.0D+00 + t * t )
          s = t * c
          tau = s / ( 1.0D+00 + c )
          h = t * a(p,q)
!
!  Accumulate corrections to diagonal elements.
!
          zw(p) = zw(p) - h                  
          zw(q) = zw(q) + h
          d(p) = d(p) - h
          d(q) = d(q) + h

          a(p,q) = 0.0D+00
!
!  Rotate, using information from the upper triangle of A only.
!
          do j = 1, p - 1
            g = a(j,p)
            h = a(j,q)
            a(j,p) = g - s * ( h + g * tau )
            a(j,q) = h + s * ( g - h * tau )
          end do

          do j = p + 1, q - 1
            g = a(p,j)
            h = a(j,q)
            a(p,j) = g - s * ( h + g * tau )
            a(j,q) = h + s * ( g - h * tau )
          end do

          do j = q + 1, n
            g = a(p,j)
            h = a(q,j)
            a(p,j) = g - s * ( h + g * tau )
            a(q,j) = h + s * ( g - h * tau )
          end do
!
!  Accumulate information in the eigenvector matrix.
!
          do j = 1, n
            g = v(j,p)
            h = v(j,q)
            v(j,p) = g - s * ( h + g * tau )
            v(j,q) = h + s * ( g - h * tau )
          end do

          rot_num = rot_num + 1

        end if

      end do
    end do

    bw(1:n) = bw(1:n) + zw(1:n)
    d(1:n) = bw(1:n)
    zw(1:n) = 0.0D+00

  end do
!
!  Restore upper triangle of input matrix.
!
  do j = 1, n
    do i = 1, j - 1
      a(i,j) = a(j,i)
    end do
  end do
!
!  Ascending sort the eigenvalues and eigenvectors.
!
  do k = 1, n - 1

    m = k

    do l = k + 1, n
      if ( d(l) < d(m) ) then
        m = l
      end if
    end do

    if ( m /= k ) then

      t    = d(m)
      d(m) = d(k)
      d(k) = t

      w(1:n)   = v(1:n,m)
      v(1:n,m) = v(1:n,k)
      v(1:n,k) = w(1:n)

    end if

  end do

  return
end
subroutine r_jacobi ( n, a, b, alpha, beta )

!*****************************************************************************80
!
!! R_JACOBI computes recurrence coefficients for monic Jacobi polynomials.
!
!  Discussion:
!
!    This function generates the first N recurrence coefficients for monic 
!    Jacobi polynomials with parameters A and B. 
!
!    These polynomials are orthogonal on [-1,1] relative to the weight
!
!      w(x) = (1.0-x)^A * (1.0+x)^B. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2013
!
!  Author:
!
!    Original MATLAB version by Dirk Laurie, Walter Gautschi.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Walter Gautschi,
!    Orthogonal Polynomials: Computation and Approximation,
!    Oxford, 2004,
!    ISBN: 0-19-850672-4,
!    LC: QA404.5 G3555.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of coefficients desired.
!
!    Input, real ( kind = 8 ) A, B, the parameters for the Jacobi polynomial.
!    -1.0 < A, -1.0 < B.
!
!    Output, real ( kind = 8 ) ALPHA(N), BETA(N), the first N recurrence
!    coefficients.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) beta(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) i_r8
  real ( kind = 8 ) mu
  real ( kind = 8 ) nab
  real ( kind = 8 ) nu

  if ( a <= -1.0D+00 ) then 
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R_JACOBI - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of A.'
    stop
  end if

  if ( b <= -1.0D+00 ) then 
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R_JACOBI - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of B.'
    stop
  end if

  nu = ( b - a ) / ( a + b + 2.0D+00 )

  mu = 2.0D+00 ** ( a + b + 1.0D+00 ) &
    * gamma ( a + 1.0D+00 ) &
    * gamma ( b + 1.0D+00 ) &
    / gamma ( a + b + 2.0D+00 )

  alpha(1) = nu
  beta(1) = mu 

  if ( n == 1 ) then
    return
  end if

  do i = 2, n
    i_r8 = real ( i, kind = 8 )
    alpha(i) = ( b - a ) * ( b + a ) & 
      / ( 2.0D+00 * ( i_r8 - 1.0D+00 ) + a + b ) &
      / ( 2.0D+00 * i_r8 + a + b )
  end do

  beta(2) = 4.0D+00 * ( a + 1.0D+00 ) * ( b + 1.0D+00 ) &
    / ( a + b + 2.0D+00 ) ** 2 &
    / ( a + b + 3.0D+00 )

  do i = 3, n
    i_r8 = real ( i, kind = 8 )
    nab = 2.0D+00 * ( i_r8 - 1.0D+00 ) + a +  b
    beta(i) = 4.0D+00 * ( i_r8 - 1.0D+00 + a ) * ( i_r8 - 1.0D+00 + b ) &
      * ( i_r8 - 1.0D+00 ) * ( i_r8 - 1.0D+00 + a + b ) &
      / nab ** 2 &
      / ( nab + 1.0D+00 ) &
      / ( nab - 1.0D+00 )
  end do

  return
end
function r8_acos ( c )

!*****************************************************************************80
!
!! R8_ACOS computes the arc cosine function, with argument truncation.
!
!  Discussion:
!
!    If you call your system ACOS routine with an input argument that is
!    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant
!    surprise (I did).
!
!    This routine simply truncates arguments outside the range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) C, the argument.
!
!    Output, real ( kind = 8 ) R8_ACOS, an angle whose cosine is C.
!
  implicit none

  real ( kind = 8 ) c
  real ( kind = 8 ) c2
  real ( kind = 8 ) r8_acos

  c2 = c
  c2 = max ( c2, -1.0D+00 )
  c2 = min ( c2, +1.0D+00 )

  r8_acos = acos ( c2 )

  return
end
function r8_asin ( s )

!*****************************************************************************80
!
!! R8_ASIN computes the arc sine function, with argument truncation.
!
!  Discussion:
!
!    If you call your system ASIN routine with an input argument that is
!    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant 
!    surprise (I did).
!
!    This routine simply truncates arguments outside the range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) S, the argument.
!
!    Output, real ( kind = 8 ) R8_ASIN, an angle whose sine is S.
!
  implicit none

  real ( kind = 8 ) r8_asin
  real ( kind = 8 ) s
  real ( kind = 8 ) s2

  s2 = s
  s2 = max ( s2, -1.0D+00 )
  s2 = min ( s2, +1.0D+00 )

  r8_asin = asin ( s2 )

  return
end
function r8_atan ( y, x )

!*****************************************************************************80
!
!! R8_ATAN computes the inverse tangent of the ratio Y / X.
!
!  Discussion:
!
!    R8_ATAN returns an angle whose tangent is ( Y / X ), a job which
!    the built in functions ATAN and ATAN2 already do.
!
!    However:
!
!    * R8_ATAN always returns a positive angle, between 0 and 2 PI,
!      while ATAN and ATAN2 return angles in the interval [-PI/2,+PI/2]
!      and [-PI,+PI] respectively;
!
!    * R8_ATAN accounts for the signs of X and Y, (as does ATAN2).  The ATAN
!     function by contrast always returns an angle in the first or fourth
!     quadrants.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Y, X, two quantities which represent the
!    tangent of an angle.  If Y is not zero, then the tangent is (Y/X).
!
!    Output, real ( kind = 8 ) R8_ATAN, an angle between 0 and 2 * PI, whose
!    tangent is (Y/X), and which lies in the appropriate quadrant so that
!    the signs of its cosine and sine match those of X and Y.
!
  implicit none

  real ( kind = 8 ) abs_x
  real ( kind = 8 ) abs_y
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_atan
  real ( kind = 8 ) theta
  real ( kind = 8 ) theta_0
  real ( kind = 8 ) x
  real ( kind = 8 ) y
!
!  Special cases:
!
  if ( x == 0.0D+00 ) then

    if ( 0.0D+00 < y ) then
      theta = pi / 2.0D+00
    else if ( y < 0.0D+00 ) then
      theta = 3.0D+00 * pi / 2.0D+00
    else if ( y == 0.0D+00 ) then
      theta = 0.0D+00
    end if

  else if ( y == 0.0D+00 ) then

    if ( 0.0D+00 < x ) then
      theta = 0.0D+00
    else if ( x < 0.0D+00 ) then
      theta = pi
    end if
!
!  We assume that ATAN2 is correct when both arguments are positive.
!
  else

    abs_y = abs ( y )
    abs_x = abs ( x )

    theta_0 = atan2 ( abs_y, abs_x )

    if ( 0.0D+00 < x .and. 0.0D+00 < y ) then
      theta = theta_0
    else if ( x < 0.0D+00 .and. 0.0D+00 < y ) then
      theta = pi - theta_0
    else if ( x < 0.0D+00 .and. y < 0.0D+00 ) then
      theta = pi + theta_0
    else if ( 0.0D+00 < x .and. y < 0.0D+00 ) then
      theta = 2.0D+00 * pi - theta_0
    end if

  end if

  r8_atan = theta

  return
end
function r8_epsilon ( )

!*****************************************************************************80
!
!! R8_EPSILON returns the R8 roundoff unit.
!
!  Discussion:
!
!    The roundoff unit is a number R which is a power of 2 with the
!    property that, to the precision of the computer's arithmetic,
!      1 < 1 + R
!    but
!      1 = ( 1 + R / 2 )
!
!    FORTRAN90 provides the superior library routine
!
!      EPSILON ( X )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_EPSILON, the round-off unit.
!
  implicit none

  real ( kind = 8 ) r8_epsilon

  r8_epsilon = 2.220446049250313D-016

  return
end
subroutine r8_fake_use ( x )

!*****************************************************************************80
!
!! r8_fake_use pretends to use a variable.
!
!  Discussion:
!
!    Some compilers will issue a warning if a variable is unused.
!    Sometimes there's a good reason to include a variable in a program,
!    but not to use it.  Calling this function with that variable as
!    the argument will shut the compiler up.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the variable to be "used".
!
  implicit none

  real ( kind = 8 ) x

  if ( x /= x ) then
    write ( *, '(a)' ) '  r8_fake_use: variable is NAN.'
  end if

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
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_01 = real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
subroutine r8mat_uniform_01 ( m, n, seed, r )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_01 fills an R8MAT with unit pseudorandom numbers.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2004
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
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in
!    the array.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if

      r(i,j) = real ( seed, kind = 8 ) * 4.656612875D-10

    end do
  end do

  return
end
subroutine r8vec_linspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_LINSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
!
!    In other words, the interval is divided into N-1 even subintervals,
!    and the endpoints of intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A_FIRST, A_LAST, the first and last entries.
!
!    Output, real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n
      x(i) = ( real ( n - i,     kind = 8 ) * a   &
             + real (     i - 1, kind = 8 ) * b ) &
             / real ( n     - 1, kind = 8 )
    end do

  end if

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
    stop
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
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
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
!    06 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
subroutine tridisolve ( n, a, b, c, d, x )

!*****************************************************************************80
!
!! TRIDISOLVE solves a tridiagonal system of linear equations.
!
!  Discussion:
!
!    We can describe an NxN tridiagonal matrix by vectors A, B, and C, where
!    A and C are of length N-1.  In that case, a linear system can be
!    represented as
!                        b(1) * x(1) + c(1) * x(2)   = d(1),
!      a(j-1) * x(j-1) + b(j) * x(j) + c(j) * x(j+1) = d(j), j = 2:n-1,
!      a(n-1) * x(n-1) + b(n) * x(n)                 = d(n)
!
!    This function produces the solution vector X.
!
!    This function is derived from Cleve Moler's Matlab suite.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the linear system.
!
!    Input, real ( kind = 8 ) A(N-1), B(N), C(N-1), the matrix entries.
!
!    Input, real ( kind = 8 ) D(N), the right hand side.
!
!    Output, real ( kind = 8 ) X(N), the solution.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n-1)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) bi(n)
  real ( kind = 8 ) c(n-1)
  real ( kind = 8 ) d(n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) mu
  real ( kind = 8 ) x(n)

  x(1:n) = d(1:n)

  bi(1:n) = 1.0D+00 / b(1:n)

  do j = 1, n - 1
    mu = a(j) * bi(j)
    b(j+1) = b(j+1) - mu * c(j)
    x(j+1) = x(j+1) - mu * x(j)
  end do

  x(n) = x(n) * bi(n)
  do j = n - 1, 1, -1
    x(j) = ( x(j) - c(j) * x(j+1) ) * bi(j)
  end do

  return
end
