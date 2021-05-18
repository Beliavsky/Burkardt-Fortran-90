program main

!*****************************************************************************80
!
!! MAIN is the main program for TRIANGLE_PROPERTIES_TEST.
!
!  Discussion:
!
!    TRIANGLE_PROPERTIES_TEST tests TRIANGLE_PROPERTIES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_PROPERTIES_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Test the TRIANGLE_PROPERTIES library.'

  call triangle_angles_test ( )
  call triangle_area_test ( )
  call triangle_centroid_test ( )
  call triangle_circumcircle_test ( )
  call triangle_contains_point_test ( )
  call triangle_diameter_test ( )
  call triangle_edge_length_test ( )
  call triangle_incircle_test ( )
  call triangle_orientation_test ( )
  call triangle_orthocenter_test ( )
  call triangle_point_dist_test ( )
  call triangle_point_near_test ( )
  call triangle_quality_test ( )
  call triangle_reference_sample_test ( )
  call triangle_sample_test ( )
  call triangle_xsi_to_xy_test ( )
  call triangle_xy_to_xsi_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_PROPERTIES_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine triangle_angles_test ( )

!*****************************************************************************80
!
!! TRIANGLE_ANGLES_TEST tests TRIANGLE_ANGLES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angle(3)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ), dimension ( 2, 3 ) :: t = reshape ( (/ &
    0.0D+00, 1.0D+00, &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00 /), (/ 2, 3 /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_ANGLES_TEST'
  write ( *, '(a)' ) '  TRIANGLE_ANGLES computes the angles in a triangle.'

  call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

  call triangle_angles ( t, angle )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      Radians      Degrees'
  write ( *, '(a)' ) ' '
  do i = 1, 3
    write ( *, '(2x,g14.6,2x,g14.6)' ) angle(i), ( angle(i) ) * 180.0D+00 / r8_pi
  end do

  return
end
subroutine triangle_area_test ( )

!*****************************************************************************80
!
!! TRIANGLE_AREA_TEST tests TRIANGLE_AREA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) area
  real ( kind = 8 ), dimension ( 2, 3 ) :: t = reshape ( (/ &
    0.0D+00, 1.0D+00, &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00 /), (/ 2, 3 /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_AREA_TEST'
  write ( *, '(a)' ) '  TRIANGLE_AREA computes the area of a triangle.'

  call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

  call triangle_area ( t, area )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Triangle area is ', area

  return
end
subroutine triangle_centroid_test ( )

!*****************************************************************************80
!
!! TRIANGLE_CENTROID_TEST tests TRIANGLE_CENTROID;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  real ( kind = 8 ) centroid(2)
  real ( kind = 8 ) t(2,3)
  real ( kind = 8 ), dimension(2,3,test_num) :: t_test = reshape ( (/ &
         0.0D+00,  0.0D+00, &
         1.0D+00,  0.0D+00, &
         0.0D+00,  1.0D+00, &
         0.0D+00,  0.0D+00, &
         1.0D+00,  0.0D+00, &
         0.5D+00,  0.86602539D+00, &
         0.0D+00,  0.0D+00, &
         1.0D+00,  0.0D+00, &
         0.5D+00, 10.0D+00, &
         0.0D+00,  0.0D+00, &
         1.0D+00,  0.0D+00, &
        10.0D+00,  2.0D+00 /), (/ 2, 3, test_num /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_CENTROID_TEST'
  write ( *, '(a)' ) '  TRIANGLE_CENTROID computes the centroid of a triangle.'

  do test = 1, test_num

    t(1:2,1:3) = t_test(1:2,1:3,test)

    call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

    call triangle_centroid ( t, centroid )

    call r8vec_print ( 2, centroid, '  Centroid:' )
 
  end do

  return
end
subroutine triangle_circumcircle_test ( )

!*****************************************************************************80
!
!! TRIANGLE_CIRCUMCIRCLE_TEST tests TRIANGLE_CIRCUMCIRCLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  real ( kind = 8 ) pc(2)
  real ( kind = 8 ) r
  real ( kind = 8 ) t(2,3)
  real ( kind = 8 ), dimension(2,3,test_num) :: t_test = reshape ( (/ &
         0.0D+00,  0.0D+00, &
         1.0D+00,  0.0D+00, &
         0.0D+00,  1.0D+00, &
         0.0D+00,  0.0D+00, &
         1.0D+00,  0.0D+00, &
         0.5D+00,  0.86602539D+00, &
         0.0D+00,  0.0D+00, &
         1.0D+00,  0.0D+00, &
         0.5D+00, 10.0D+00, &
         0.0D+00,  0.0D+00, &
         1.0D+00,  0.0D+00, &
        10.0D+00,  2.0D+00 /), (/ 2, 3, test_num /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_CIRCUMCIRCLE_TEST'
  write ( *, '(a)' ) '  TRIANGLE_CIRCUMCIRCLE computes the circumcircle of a triangle.'

  do test = 1, test_num

    t(1:2,1:3) = t_test(1:2,1:3,test)

    call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

    call triangle_circumcircle ( t, r, pc )

    call r8vec_print ( 2, pc, '  Circumcenter' )

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  Circumradius: ', r

  end do

  return
end
subroutine triangle_contains_point_test ( )

!*****************************************************************************80
!
!! TRIANGLE_CONTAINS_POINT_TEST tests TRIANGLE_CONTAINS_POINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 7

  logical inside
  integer ( kind = 4 ) j
  real ( kind = 8 ) p(2)
  real ( kind = 8 ), dimension ( 2, test_num ) :: p_test = reshape ( (/ &
     0.25D+00,   0.25D+00, &
     0.75D+00,   0.25D+00, &
     1.00D+00,   1.00D+00, &
    11.00D+00,   0.50D+00, &
     0.00D+00,   1.00D+00, &
     0.50D+00, -10.00D+00, &
     0.60D+00,   0.60D+00 /), (/ 2, test_num /) )
  real ( kind = 8 ), dimension ( 2, 3 ) :: t = reshape ( (/ &
    0.0D+00, 1.0D+00, &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00 /), (/ 2, 3 /) )
  real ( kind = 8 ), dimension ( 2, 3 ) :: t2
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_CONTAINS_POINT_TEST'
  write ( *, '(a)' ) '  TRIANGLE_CONTAINS_POINT reports if a point '
  write ( *, '(a)' ) '  is inside a triangle.'

  call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X       Y     Inside'
  write ( *, '(a)' ) ' '

  do test = 1, test_num

    p(1:2) = p_test(1:2,test)
 
    call triangle_contains_point ( t, p, inside )

    write ( *, '(2x,2f8.3,5x,l1)' ) p(1:2), inside

  end do
!
!  Make a copy of the triangle with vertices in reverse order.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Repeat the test, but reverse the triangle vertex'
  write ( *, '(a)' ) '  ordering.'
 
  do j = 1, 3
    t2(1:2,j) = t(1:2,4-j)
  end do

  call r8mat_transpose_print ( 2, 3, t2, &
    '  Triangle vertices (reversed):' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X       Y     Inside'
  write ( *, '(a)' ) ' '

  do test = 1, test_num

    p(1:2) = p_test(1:2,test)
 
    call triangle_contains_point ( t2, p, inside )

    write ( *, '(2x,2f8.3,5x,l1)' ) p(1:2), inside

  end do
 
  return
end
subroutine triangle_diameter_test ( )

!*****************************************************************************80
!
!! TRIANGLE_DIAMETER_TEST tests TRIANGLE_DIAMETER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 3

  real ( kind = 8 ) diameter
  real ( kind = 8 ) t(2,3)
  real ( kind = 8 ), dimension(2,3,test_num) :: t_test = reshape ( (/ &
     4.0D+00, 2.0D+00, &
     1.0D+00, 5.0D+00, &
    -2.0D+00, 2.0D+00, &
     4.0D+00, 2.0D+00, &
     5.0D+00, 4.0D+00, &
     6.0D+00, 6.0D+00, &
     4.0D+00, 2.0D+00, &
     1.0D+00, 5.0D+00, &
     4.0D+00, 2.0D+00 /), (/ 2, 3, test_num /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_DIAMETER_TEST'
  write ( *, '(a)' ) '  TRIANGLE_DIAMETER computes the diameter of '
  write ( *, '(a)' ) '  the SMALLEST circle around the triangle.'

  do test = 1, test_num

    t(1:2,1:3) = t_test(1:2,1:3,test)

    call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

    call triangle_diameter ( t, diameter )

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  Diameter =       ', diameter

  end do

  return
end
subroutine triangle_edge_length_test ( )

!*****************************************************************************80
!
!! TRIANGLE_EDGE_LENGTH_TEST tests TRIANGLE_EDGE_LENGTH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 3

  real ( kind = 8 ) edge_length(3)
  real ( kind = 8 ) t(2,3)
  real ( kind = 8 ), dimension(2,3,test_num) :: t_test = reshape ( (/ &
     4.0D+00, 2.0D+00, &
     1.0D+00, 5.0D+00, &
    -2.0D+00, 2.0D+00, &
     4.0D+00, 2.0D+00, &
     5.0D+00, 4.0D+00, &
     6.0D+00, 6.0D+00, &
     4.0D+00, 2.0D+00, &
     1.0D+00, 5.0D+00, &
     4.0D+00, 2.0D+00 /), (/ 2, 3, test_num /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_EDGE_LENGTH_TEST'
  write ( *, '(a)' ) '  TRIANGLE_EDGE_LENGTH computes the edge lengths of a triangle.'

  do test = 1, test_num

    t(1:2,1:3) = t_test(1:2,1:3,test)

    call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

    call triangle_edge_length ( t, edge_length )

    call r8vec_print ( 3, edge_length, '  Edge lengths:' )

  end do

  return
end
subroutine triangle_incircle_test ( )

!*****************************************************************************80
!
!! TRIANGLE_INCIRCLE_TEST tests TRIANGLE_INCIRCLE;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) pc(2)
  real ( kind = 8 ) r
  real ( kind = 8 ), dimension ( 2, 3 ) :: t = reshape ( (/ &
    0.0D+00, 1.0D+00, &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00 /), (/ 2, 3 /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_INCIRCLE_TEST'
  write ( *, '(a)' ) '  TRIANGLE_INCIRCLE_2D computes the incircle of a triangle.'

  call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

  call triangle_incircle ( t, r, pc )

  call r8vec_print ( 2, pc, '  Incenter' )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Incircle radius is ', r

  return
end
subroutine triangle_orientation_test ( )

!*****************************************************************************80
!
!! TRIANGLE_ORIENTATION_TEST tests TRIANGLE_ORIENTATION.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  integer ( kind = 4 ) triangle_orientation
  integer ( kind = 4 ) i
  real ( kind = 8 ) t(2,3)
  real ( kind = 8 ), dimension(2,3,test_num) :: t_test = reshape ( (/ &
     4.0D+00,  2.0D+00, &
     1.0D+00,  5.0D+00, &
    -2.0D+00,  2.0D+00, &
     1.0D+00,  5.0D+00, &
     4.0D+00,  2.0D+00, &
     1.0D+00, -1.0D+00, &
     1.0D+00,  5.0D+00, &
     2.0D+00,  7.0D+00, &
     3.0D+00,  9.0D+00, &
     1.0D+00,  5.0D+00, &
     4.0D+00,  2.0D+00, &
     1.0D+00,  5.0D+00 /), (/ 2, 3, test_num /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_ORIENTATION_TEST'
  write ( *, '(a)' ) '  TRIANGLE_ORIENTATION determines the orientation of a triangle.'

  do test = 1, test_num

    t(1:2,1:3) = t_test(1:2,1:3,test)

    i = triangle_orientation ( t )

    call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

    write ( *, '(a)' ) ''

    if ( i == 0 ) then
      write ( *, '(a)' ) '  The points are counterclockwise.'
    else if ( i == 1 ) then
      write ( *, '(a)' ) '  The points are clockwise.'
    else if ( i == 2 ) then
      write ( *, '(a)' ) '  The points are colinear.'
    else if ( i == 3 ) then
      write ( *, '(a)' ) '  The points are not distinct.'
    else
      write ( *, '(a)' ) '  The return value makes no sense.'
    end if

  end do

  return
end
subroutine triangle_orthocenter_test ( )

!*****************************************************************************80
!
!! TRIANGLE_ORTHOCENTER_TEST tests TRIANGLE_ORTHOCENTER;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  logical flag
  real ( kind = 8 ) pc(2)
  real ( kind = 8 ) t(2,3)
  real ( kind = 8 ), dimension(2,3,test_num) :: t_test = reshape ( (/ &
     0.0D+00,  0.0D+00, &
     1.0D+00,  0.0D+00, &
     0.0D+00,  1.0D+00, &
     0.0D+00,  0.0D+00, &
     1.0D+00,  0.0D+00, &
     0.5D+00,  0.86602539D+00, &
     0.0D+00,  0.0D+00, &
     1.0D+00,  0.0D+00, &
     0.5D+00, 10.0D+00, &
     0.0D+00,  0.0D+00, &
     1.0D+00,  0.0D+00, &
    10.0D+00,  2.0D+00 /), (/ 2, 3, test_num /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_ORTHOCENTER_TEST'
  write ( *, '(a)' ) '  TRIANGLE_ORTHOCENTER_2D computes the orthocenter of a triangle.'

  do test = 1, test_num

    t(1:2,1:3) = t_test(1:2,1:3,test)

    call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

    call triangle_orthocenter ( t, pc, flag )

    call r8vec_print ( 2, pc, '  Orthocenter' )

  end do

  return
end
subroutine triangle_point_dist_test ( )

!*****************************************************************************80
!
!! TRIANGLE_POINT_DIST_TEST tests TRIANGLE_POINT_DIST;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 7

  real ( kind = 8 ) dist
  real ( kind = 8 ) p(2)
  real ( kind = 8 ), dimension(2,test_num) :: p_test = reshape ( (/ &
     0.25D+00,   0.25D+00, &
     0.75D+00,   0.25D+00, &
     1.00D+00,   1.00D+00, &
    11.00D+00,   0.50D+00, &
     0.00D+00,   1.00D+00, &
     0.50D+00, -10.00D+00, &
     0.60D+00,   0.60D+00 /), (/ 2, test_num /) )
  real ( kind = 8 ), dimension ( 2, 3 ) :: t = reshape ( (/ &
    0.0D+00, 1.0D+00, &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00 /), (/ 2, 3 /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_POINT_DIST_TEST'
  write ( *, '(a)' ) '  TRIANGLE_POINT_DIST computes the distance'
  write ( *, '(a)' ) '  from a point to a triangle.'

  call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '           P            DIST'
  write ( *, '(a)' ) ' '

  do test = 1, test_num

    p(1:2) = p_test(1:2,test)

    call triangle_point_dist ( t, p, dist ) 

    write ( *, '(2x,2f8.3,2x,f8.3)' ) p(1:2), dist

  end do
 
  return
end
subroutine triangle_point_near_test ( )

!*****************************************************************************80
!
!! TRIANGLE_POINT_NEAR_TEST tests TRIANGLE_POINT_NEAR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 7

  real ( kind = 8 ) dist
  real ( kind = 8 ) p(2)
  real ( kind = 8 ), dimension(2,test_num) :: p_test = reshape ( (/ &
     0.25D+00,   0.25D+00, &
     0.75D+00,   0.25D+00, &
     1.00D+00,   1.00D+00, &
    11.00D+00,   0.50D+00, &
     0.00D+00,   1.00D+00, &
     0.50D+00, -10.00D+00, &
     0.60D+00,   0.60D+00 /), (/ 2, test_num /) )
  real ( kind = 8 ) pn(2)
  real ( kind = 8 ), dimension ( 2, 3 ) :: t = reshape ( (/ &
    0.0D+00, 1.0D+00, &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00 /), (/ 2, 3 /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_POINT_NEAR_TEST'
  write ( *, '(a)' ) '  TRIANGLE_POINT_NEAR computes the nearest'
  write ( *, '(a)' ) '  point on a triangle to a given point.'

  call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '           P                PN'
  write ( *, '(a)' ) ' '

  do test = 1, test_num

    p(1:2) = p_test(1:2,test)

    call triangle_point_near ( t, p, pn, dist ) 

    write ( *, '(2x,2f8.3,2x,2f8.3)' ) p(1:2), pn(1:2)

  end do
 
  return
end
subroutine triangle_quality_test ( )

!*****************************************************************************80
!
!! TRIANGLE_QUALITY_TEST tests TRIANGLE_QUALITY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  real ( kind = 8 ) quality
  real ( kind = 8 ) t(2,3)
  real ( kind = 8 ), dimension (2,3,test_num) :: t_test = reshape ( (/ &
     0.0D+00,  0.0D+00, &
     1.0D+00,  0.0D+00, &
     0.0D+00,  1.0D+00, &
     0.0D+00,  0.0D+00, &
     1.0D+00,  0.0D+00, &
     0.5D+00,  0.86602539D+00, &
     0.0D+00,  0.0D+00, &
     1.0D+00,  0.0D+00, &
     0.5D+00, 10.0D+00, &
     0.0D+00,  0.0D+00, &
     1.0D+00,  0.0D+00, &
    10.0D+00,  2.0D+00 /), (/ 2, 3, test_num /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_QUALITY_TEST'
  write ( *, '(a)' ) '  TRIANGLE_QUALITY computes the quality of a triangle.'

  do test = 1, test_num

    t(1:2,1:3) = t_test(1:2,1:3,test)

    call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

    call triangle_quality ( t, quality )

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  Quality = ', quality

  end do

  return
end
subroutine triangle_reference_sample_test ( )

!*****************************************************************************80
!
!! TRIANGLE_REFERENCE_SAMPLE_TEST tests TRIANGLE_REFERENCE_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 10

  real ( kind = 8 ) p(2)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ), dimension(2,3) :: t = reshape ( (/ &
     0.0D+00, 0.0D+00, &
     1.0D+00, 0.0D+00, &
     0.0D+00, 1.0D+00 /), (/ 2, 3 /) )
  integer ( kind = 4 ) test
  real ( kind = 8 ) xsi(3)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_REFERENCE_SAMPLE_TEST'
  write ( *, '(a)' ) '  TRIANGLE_REFERENCE_SAMPLE samples the reference triangle.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Sample points (X,Y) and (XSI1,XSI2,XSI3) coordinates:'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    call triangle_reference_sample ( 1, seed, p )
    call triangle_xy_to_xsi ( t, p, xsi )
    write ( *, '(2x,2f8.4,4x,3f8.4)' ) p(1:2), xsi(1:3)
  end do

  return
end
subroutine triangle_sample_test ( )

!*****************************************************************************80
!
!! TRIANGLE_SAMPLE_TEST tests TRIANGLE_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 10

  real ( kind = 8 ) p(2)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ), dimension(2,3) :: t = reshape ( (/ &
     4.0D+00, 2.0D+00, &
     1.0D+00, 5.0D+00, &
    -2.0D+00, 2.0D+00 /), (/ 2, 3 /) )
  integer ( kind = 4 ) test
  real ( kind = 8 ) xsi(3)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_SAMPLE_TEST'
  write ( *, '(a)' ) '  TRIANGLE_SAMPLE samples points from a triangle.'

  call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Sample points (X,Y) and (XSI1,XSI2,XSI3) coordinates:'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    call triangle_sample ( t, 1, seed, p )
    call triangle_xy_to_xsi ( t, p, xsi )
    write ( *, '(2x,2f8.4,4x,3f8.4)' ) p(1:2), xsi(1:3)
  end do

  return
end
subroutine triangle_xsi_to_xy_test ( )

!*****************************************************************************80
!
!! TRIANGLE_XSI_TO_XY_TEST tests TRIANGLE_XSI_TO_XY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 10

  integer ( kind = 4 ) i
  real ( kind = 8 ) p(2)
  real ( kind = 8 ) p2(2)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ), dimension(2,3) :: t = reshape ( (/ &
     4.0D+00, 2.0D+00, &
     1.0D+00, 5.0D+00, &
    -2.0D+00, 2.0D+00 /), (/ 2, 3 /) )
  integer ( kind = 4 ) test
  real ( kind = 8 ) xsi(3)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_XSI_TO_XY_TEST'
  write ( *, '(a)' ) '  TRIANGLE_XSI_TO_XY converts XSI to XY coordinates.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  We verify that (X,Y) -> (XSI1,XSI2,XSI3) -> (X,Y)'
  write ( *, '(a)' ) '  works properly.'

  call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Sample points:'
  write ( *, '(a)' ) ' '

  do test = 1, test_num

    if ( test == 1 ) then
      do i = 1, 2
        p(i) = sum ( t(i,1:3) ) / 3.0D+00
      end do
    else if ( test == 2 ) then
      p(1) = 3.0D+00
      p(2) = 0.0D+00
    else
      call triangle_sample ( t, 1, seed, p )
    end if

    call triangle_xy_to_xsi ( t, p, xsi )

    call triangle_xsi_to_xy ( t, xsi, p2 )

    write ( *, '(2x,2f8.4,4x,3f8.4,2x,2f8.4)' ) p(1:2), xsi(1:3), p2(1:2)

  end do

  return
end
subroutine triangle_xy_to_xsi_test ( )

!*****************************************************************************80
!
!! TRIANGLE_XY_TO_XSI_TEST tests TRIANGLE_XY_TO_XSI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 10

  integer ( kind = 4 ) i
  real ( kind = 8 ) p(2)
  real ( kind = 8 ) p2(2)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ), dimension(2,3) :: t = reshape ( (/ &
     4.0D+00, 2.0D+00, &
     1.0D+00, 5.0D+00, &
    -2.0D+00, 2.0D+00 /), (/ 2, 3 /) )
  integer ( kind = 4 ) test
  real ( kind = 8 ) xsi(3)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_XY_TO_XSI_TEST'
  write ( *, '(a)' ) '  TRIANGLE_XY_TO_XSI converts XY to XSI coordinates.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  We verify that (X,Y) -> (XSI1,XSI2,XSI3) -> (X,Y)'
  write ( *, '(a)' ) '  works properly.'

  call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Sample points:'
  write ( *, '(a)' ) ' '

  do test = 1, test_num

    if ( test == 1 ) then
      do i = 1, 2
        p(i) = sum ( t(i,1:3) ) / 3.0D+00
      end do
    else if ( test == 2 ) then
      p(1) = 3.0D+00
      p(2) = 0.0D+00
    else
      call triangle_sample ( t, 1, seed, p )
    end if

    call triangle_xy_to_xsi ( t, p, xsi )

    call triangle_xsi_to_xy ( t, xsi, p2 )

    write ( *, '(2x,2f8.4,4x,3f8.4,2x,2f8.4)' ) p(1:2), xsi(1:3), p2(1:2)

  end do

  return
end
