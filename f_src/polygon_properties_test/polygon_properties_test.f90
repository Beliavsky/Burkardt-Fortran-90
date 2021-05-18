program main

!*****************************************************************************80
!
!! MAIN is the main program for POLYGON_PROPERTIES_TEST.
!
!  Discussion:
!
!    POLYGON_PROPERTIES_TEST tests the POLYGON_PROPERTIES library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_PROPERTIES_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the POLYGON_PROPERTIES library.'

  call polygon_angles_test ( )
  call polygon_area_test ( )
  call polygon_area_2_test ( )
  call polygon_centroid_test ( )
  call polygon_centroid_2_test ( )
  call polygon_contains_point_test ( )
  call polygon_contains_point_2_test ( )
  call polygon_contains_point_3_test ( )
  call polygon_diameter_test ( )
  call polygon_expand_test ( )
  call polygon_inrad_data_test ( )
  call polygon_integral_1_test ( )
  call polygon_integral_x_test ( )
  call polygon_integral_xx_test ( )
  call polygon_integral_xy_test ( )
  call polygon_integral_y_test ( )
  call polygon_integral_yy_test ( )
  call polygon_is_convex_test ( )
  call polygon_lattice_area_test ( )
  call polygon_outrad_data_test ( )
  call polygon_perimeter_test ( )
  call polygon_perimeter_quad_test ( )
  call polygon_point_dist_test ( )
  call polygon_point_near_test ( )
  call polygon_sample_test ( )
  call polygon_side_data_test ( )
  call polygon_triangulate_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_PROPERTIES_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine polygon_angles_test ( )

!*****************************************************************************80
!
!! POLYGON_ANGLES_TEST tests POLYGON_ANGLES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ) angle(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_degrees
  real ( kind = 8 ), dimension (2,n) :: v = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    2.0D+00, 1.0D+00, &
    3.0D+00, 0.0D+00, &
    3.0D+00, 2.0D+00, &
    1.0D+00, 2.0D+00 /), (/ 2, n /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_ANGLES_TEST'
  write ( *, '(a)' ) '  POLYGON_ANGLES computes the angles of a polygon.'

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of polygonal vertices = ', n

  call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

  call polygon_angles ( n, v, angle )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Polygonal angles in degrees:'
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i8,2x,g14.6)' ) i, r8_degrees ( angle(i) )
  end do

  return
end
subroutine polygon_area_test ( )

!*****************************************************************************80
!
!! POLYGON_AREA_TEST tests POLYGON_AREA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 2

  real ( kind = 8 ) area
  real ( kind = 8 ) area_exact
  real ( kind = 8 ), dimension ( test_num ) :: area_exact_test = (/ &
    2.0D+00, 6.0D+00 /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( test_num ) :: n_test = (/ 4, 8 /)
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: v

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_AREA_TEST'
  write ( *, '(a)' ) '  POLYGON_AREA computes the area of a polygon.'

  do test = 1, test_num

    n = n_test(test)
    area_exact = area_exact_test(test)

    allocate ( v(1:2,1:n) )

    if ( test == 1 ) then

      v(1:2,1:n) = reshape ( (/ &
        1.0D+00, 0.0D+00, &
        2.0D+00, 1.0D+00, &
        1.0D+00, 2.0D+00, &
        0.0D+00, 1.0D+00 /), (/ 2, n /) )

    else if ( test == 2 ) then

      v(1:2,1:n) = reshape ( (/ &
        0.0D+00, 0.0D+00, &
        3.0D+00, 0.0D+00, &
        3.0D+00, 3.0D+00, &
        2.0D+00, 3.0D+00, &
        2.0D+00, 1.0D+00, &
        1.0D+00, 1.0D+00, &
        1.0D+00, 2.0D+00, &
        0.0D+00, 2.0D+00 /), (/ 2, n /) )

    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Number of polygonal vertices = ', n

    call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

    call polygon_area ( n, v, area )

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Exact area is        ', area_exact
    write ( *, '(a,g14.6)' ) '  The computed area is ', area
 
    deallocate ( v )

  end do

  return
end
subroutine polygon_area_2_test ( )

!*****************************************************************************80
!
!! POLYGON_AREA_2_TEST tests POLYGON_AREA_2;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 2

  real ( kind = 8 ) area
  real ( kind = 8 ) area_exact
  real ( kind = 8 ), dimension ( test_num ) :: area_exact_test = (/ &
    2.0D+00, 6.0D+00 /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( test_num ) :: n_test = (/ 4, 8 /)
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: v

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_AREA_2_TEST'
  write ( *, '(a)' ) '  POLYGON_AREA_2 computes the area of a polygon.'

  do test = 1, test_num

    n = n_test(test)
    area_exact = area_exact_test(test)

    allocate ( v(1:2,1:n) )

    if ( test == 1 ) then

      v(1:2,1:n) = reshape ( (/ &
        1.0D+00, 0.0D+00, &
        2.0D+00, 1.0D+00, &
        1.0D+00, 2.0D+00, &
        0.0D+00, 1.0D+00 /), (/ 2, n /) )

    else if ( test == 2 ) then

      v(1:2,1:n) = reshape ( (/ &
        0.0D+00, 0.0D+00, &
        3.0D+00, 0.0D+00, &
        3.0D+00, 3.0D+00, &
        2.0D+00, 3.0D+00, &
        2.0D+00, 1.0D+00, &
        1.0D+00, 1.0D+00, &
        1.0D+00, 2.0D+00, &
        0.0D+00, 2.0D+00 /), (/ 2, n /) )

    end if

    call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

    call polygon_area_2 ( n, v, area )

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Exact area is        ', area_exact
    write ( *, '(a,g14.6)' ) '  The computed area is ', area
 
    deallocate ( v )

  end do

  return
end
subroutine polygon_centroid_test ( )

!*****************************************************************************80
!
!! POLYGON_CENTROID_TEST tests POLYGON_CENTROID.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) centroid(2)
  real ( kind = 8 ), dimension (2,n) :: v = reshape ( (/ &
    1.0D+00, 0.0D+00, &
    2.0D+00, 1.0D+00, &
    1.0D+00, 2.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, n /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_CENTROID_TEST'
  write ( *, '(a)' ) '  POLYGON_CENTROID computes the centroid of a polygon.'

  call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

  call polygon_centroid ( n, v, centroid )

  call r8vec_print ( 2, centroid, '  Centroid from POLYGON_CENTROID:' )

  return
end
subroutine polygon_centroid_2_test ( )

!*****************************************************************************80
!
!! POLYGON_CENTROID_2_TEST tests POLYGON_CENTROID_2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) centroid(2)
  real ( kind = 8 ), dimension (2,n) :: v = reshape ( (/ &
    1.0D+00, 0.0D+00, &
    2.0D+00, 1.0D+00, &
    1.0D+00, 2.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, n /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_CENTROID_2_TEST'
  write ( *, '(a)' ) '  POLYGON_CENTROID_2 computes the centroid of a polygon.'

  call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

  call polygon_centroid_2 ( n, v, centroid )

  call r8vec_print ( 2, centroid, '  Centroid from POLYGON_CENTROID_2:' )

  return
end
subroutine polygon_contains_point_test ( )

!*****************************************************************************80
!
!! POLYGON_CONTAINS_POINT_TEST tests POLYGON_CONTAINS_POINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: test_num = 4

  logical ( kind = 4 ) inside
  real ( kind = 8 ) p(2)
  real ( kind = 8 ), dimension (2,test_num) :: p_test = reshape ( (/ &
    1.0D+00,  1.0D+00, &
    3.0D+00,  4.0D+00, &
    0.0D+00,  2.0D+00, &
    0.5D+00, -0.25D+00 /), (/2, test_num /) )
  integer ( kind = 4 ) test
  real ( kind = 8 ), dimension(2,n) :: v = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    2.0D+00, 1.0D+00, &
    1.0D+00, 2.0D+00, &
    0.0D+00, 2.0D+00 /), (/ 2, n /) )
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_CONTAINS_POINT_TEST'
  write ( *, '(a)' ) '  POLYGON_CONTAINS_POINT determines if a point is in a polygon.'

  call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '          P          Inside?'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
 
    p(1:2) = p_test(1:2,test)
 
    call polygon_contains_point ( n, v, p, inside )

    write ( *, '(2x,2g14.6,4x,l1)' ) p(1:2), inside

  end do
 
  return
end
subroutine polygon_contains_point_2_test ( )

!*****************************************************************************80
!
!! POLYGON_CONTAINS_POINT_2_TEST tests POLYGON_CONTAINS_POINT_2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: test_num = 4

  logical ( kind = 4 ) inside
  real ( kind = 8 ) p(2)
  real ( kind = 8 ), dimension (2,test_num) :: p_test = reshape ( (/ &
    1.0D+00,  1.0D+00, &
    3.0D+00,  4.0D+00, &
    0.0D+00,  2.0D+00, &
    0.5D+00, -0.25D+00 /), (/2, test_num /) )
  integer ( kind = 4 ) test
  real ( kind = 8 ), dimension(2,n) :: v = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    2.0D+00, 1.0D+00, &
    1.0D+00, 2.0D+00, &
    0.0D+00, 2.0D+00 /), (/ 2, n /) )
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_CONTAINS_POINT_2_TEST'
  write ( *, '(a)' ) '  POLYGON_CONTAINS_POINT_2 determines if'
  write ( *, '(a)' ) '  a point is in a polygon.'

  call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '          P          In1  In2'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
 
    p(1:2) = p_test(1:2,test)
 
    call polygon_contains_point_2 ( n, v, p, inside )

    write ( *, '(2x,2g14.6,4x,l1)' ) p(1:2), inside

  end do
 
  return
end
subroutine polygon_contains_point_3_test ( )

!*****************************************************************************80
!
!! POLYGON_CONTAINS_POINT_3_TEST tests POLYGON_CONTAINS_POINT_3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: test_num = 4

  logical ( kind = 4 ) inside
  real ( kind = 8 ) p(2)
  real ( kind = 8 ), dimension (2,test_num) :: p_test = reshape ( (/ &
    1.0D+00,  1.0D+00, &
    3.0D+00,  4.0D+00, &
    0.0D+00,  2.0D+00, &
    0.5D+00, -0.25D+00 /), (/2, test_num /) )
  integer ( kind = 4 ) test
  real ( kind = 8 ), dimension(2,n) :: v = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    2.0D+00, 1.0D+00, &
    1.0D+00, 2.0D+00, &
    0.0D+00, 2.0D+00 /), (/ 2, n /) )
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_CONTAINS_POINT_3_TEST'
  write ( *, '(a)' ) '  POLYGON_CONTAINS_POINT_3 determines if a point is in a polygon.'

  call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '          P          Inside?'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
 
    p(1:2) = p_test(1:2,test)
 
    call polygon_contains_point_3 ( n, v, p, inside )

    write ( *, '(2x,2g14.6,4x,l1)' ) p(1:2), inside

  end do
 
  return
end
subroutine polygon_diameter_test ( )

!*****************************************************************************80
!
!! POLYGON_DIAMETER_TEST tests POLYGON_DIAMETER;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) diameter
  real ( kind = 8 ) :: diameter_exact = 2.0D+00
  real ( kind = 8 ), dimension ( 2, n ) :: v = reshape ( (/ &
    1.0D+00, 0.0D+00, &
    2.0D+00, 1.0D+00, &
    1.0D+00, 2.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, n /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_DIAMETER_TEST'
  write ( *, '(a)' ) '  POLYGON_DIAMETER computes the diameter of a polygon.'

  call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

  call polygon_diameter ( n, v, diameter )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Diameter ( computed ) ', diameter
  write ( *, '(a,g14.6)' ) '  Diameter ( exact )    ', diameter_exact
 
  return
end
subroutine polygon_expand_test ( )

!*****************************************************************************80
!
!! POLYGON_EXPAND_TEST tests POLYGON_EXPAND;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) h
  real ( kind = 8 ), dimension(2,n) :: v = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    5.0D+00, 1.0D+00, &
    2.0D+00, 4.0D+00, &
    1.0D+00, 3.0D+00 /), (/ 2, n /) )
  real ( kind = 8 ), dimension(2,n) :: w

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_EXPAND_TEST'
  write ( *, '(a)' ) '  POLYGON_EXPAND "expands" a polygon by an amount H.'

  h = 0.5D+00

  call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The expansion amount H = ', h

  call polygon_expand ( n, v, h, w )

  call r8mat_transpose_print ( 2, n, w, '  The expanded polygon:' )

  return
end
subroutine polygon_inrad_data_test ( )

!*****************************************************************************80
!
!! POLYGON_INRAD_DATA_TEST tests POLYGON_INRAD_DATA;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) area
  integer ( kind = 4 ) n
  real ( kind = 8 ) radin
  real ( kind = 8 ) radout
  real ( kind = 8 ) side

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_INRAD_DATA_TEST'
  write ( *, '(a)' ) '  POLYGON_INRAD_DATA uses the inradius of a regular polygon'
  write ( *, '(a)' ) '  to determine the area, outradius and side length.'

  do n = 3, 5

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Number of polygonal sides = ', n

    radin = 1.0D+00
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Assuming RADIN = ', radin
    call polygon_inrad_data ( n, radin, area, radout, side )
    write ( *, '(a,g14.6)' ) '    AREA =   ', area
    write ( *, '(a,g14.6)' ) '    RADOUT = ', radout
    write ( *, '(a,g14.6)' ) '    SIDE =   ', side

  end do

  return
end
subroutine polygon_integral_1_test ( )

!*****************************************************************************80
!
!! POLYGON_INTEGRAL_1_TEST tests POLYGON_INTEGRAL_1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3
 
  real ( kind = 8 ) result
  real ( kind = 8 ), dimension(2,n1) :: v1 = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    1.0D+00, 1.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, n1 /) )
  real ( kind = 8 ), dimension(2,n2) :: v2 = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    4.0D+00, 3.0D+00, &
    2.0D+00, 5.0D+00 /), (/ 2, n2 /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_INTEGRAL_1_TEST'
  write ( *, '(a)' ) '  POLYGON_INTEGRAL_1 integrates 1 over a polygon.'

  call r8mat_transpose_print ( 2, n1, v1, '  Vertices of polygon V1:' )

  call polygon_integral_1 ( n1, v1, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of 1 over V1', result

  call r8mat_transpose_print ( 2, n2, v2, '  Vertices of polygon V2:' )

  call polygon_integral_1 ( n2, v2, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of 1 over V2', result

  return
end
subroutine polygon_integral_x_test ( )

!*****************************************************************************80
!
!! POLYGON_INTEGRAL_X_TEST tests POLYGON_INTEGRAL_X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3
 
  real ( kind = 8 ) result
  real ( kind = 8 ), dimension(2,n1) :: v1 = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    1.0D+00, 1.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, n1 /) )
  real ( kind = 8 ), dimension(2,n2) :: v2 = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    4.0D+00, 3.0D+00, &
    2.0D+00, 5.0D+00 /), (/ 2, n2 /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_INTEGRAL_X_TEST'
  write ( *, '(a)' ) '  POLYGON_INTEGRAL_X integrates X over a polygon.'

  call r8mat_transpose_print ( 2, n1, v1, '  Vertices of polygon V1:' )

  call polygon_integral_x ( n1, v1, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of X over V1', result

  call r8mat_transpose_print ( 2, n2, v2, '  Vertices of polygon V2:' )

  call polygon_integral_x ( n2, v2, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of X over V2', result

  return
end
subroutine polygon_integral_xx_test ( )

!*****************************************************************************80
!
!! POLYGON_INTEGRAL_XX_TEST tests POLYGON_INTEGRAL_XX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3
 
  real ( kind = 8 ) result
  real ( kind = 8 ), dimension(2,n1) :: v1 = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    1.0D+00, 1.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, n1 /) )
  real ( kind = 8 ), dimension(2,n2) :: v2 = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    4.0D+00, 3.0D+00, &
    2.0D+00, 5.0D+00 /), (/ 2, n2 /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_INTEGRAL_XX_TEST'
  write ( *, '(a)' ) '  POLYGON_INTEGRAL_XX integrates X^2 over a polygon.'

  call r8mat_transpose_print ( 2, n1, v1, '  Vertices of polygon V1:' )

  call polygon_integral_xx ( n1, v1, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of X^2 over V1', result

  call r8mat_transpose_print ( 2, n2, v2, '  Vertices of polygon V2:' )

  call polygon_integral_xx ( n2, v2, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of X^2 over V2', result

  return
end
subroutine polygon_integral_xy_test ( )

!*****************************************************************************80
!
!! POLYGON_INTEGRAL_XY_TEST tests POLYGON_INTEGRAL_XY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3
 
  real ( kind = 8 ) result
  real ( kind = 8 ), dimension(2,n1) :: v1 = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    1.0D+00, 1.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, n1 /) )
  real ( kind = 8 ), dimension(2,n2) :: v2 = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    4.0D+00, 3.0D+00, &
    2.0D+00, 5.0D+00 /), (/ 2, n2 /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_INTEGRAL_XY_TEST'
  write ( *, '(a)' ) '  POLYGON_INTEGRAL_XY integrates X*Y over a polygon.'

  call r8mat_transpose_print ( 2, n1, v1, '  Vertices of polygon V1:' )

  call polygon_integral_xy ( n1, v1, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of X*Y over V1', result

  call r8mat_transpose_print ( 2, n2, v2, '  Vertices of polygon V2:' )

  call polygon_integral_xy ( n2, v2, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of X*Y over V2', result

  return
end
subroutine polygon_integral_y_test ( )

!*****************************************************************************80
!
!! POLYGON_INTEGRAL_Y_TEST tests POLYGON_INTEGRAL_Y.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3
 
  real ( kind = 8 ) result
  real ( kind = 8 ), dimension(2,n1) :: v1 = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    1.0D+00, 1.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, n1 /) )
  real ( kind = 8 ), dimension(2,n2) :: v2 = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    4.0D+00, 3.0D+00, &
    2.0D+00, 5.0D+00 /), (/ 2, n2 /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_INTEGRAL_Y_TEST'
  write ( *, '(a)' ) '  POLYGON_INTEGRAL_Y integrates Y over a polygon.'

  call r8mat_transpose_print ( 2, n1, v1, '  Vertices of polygon V1:' )

  call polygon_integral_y ( n1, v1, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of Y over V1', result

  call r8mat_transpose_print ( 2, n2, v2, '  Vertices of polygon V2:' )

  call polygon_integral_y ( n2, v2, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of Y over V2', result

  return
end
subroutine polygon_integral_yy_test ( )

!*****************************************************************************80
!
!! POLYGON_INTEGRAL_YY_TEST tests POLYGON_INTEGRAL_YY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3
 
  real ( kind = 8 ) result
  real ( kind = 8 ), dimension(2,n1) :: v1 = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    1.0D+00, 1.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, n1 /) )
  real ( kind = 8 ), dimension(2,n2) :: v2 = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    4.0D+00, 3.0D+00, &
    2.0D+00, 5.0D+00 /), (/ 2, n2 /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_INTEGRAL_YY_TEST'
  write ( *, '(a)' ) '  POLYGON_INTEGRAL_YY integrates Y^2 over a polygon.'

  call r8mat_transpose_print ( 2, n1, v1, '  Vertices of polygon V1:' )

  call polygon_integral_yy ( n1, v1, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of y^2 over V1', result

  call r8mat_transpose_print ( 2, n2, v2, '  Vertices of polygon V2:' )

  call polygon_integral_yy ( n2, v2, result )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a4,4x,g14.6)' ) '  Integral of y^2 over V2', result

  return
end
subroutine polygon_is_convex_test ( )

!*****************************************************************************80
!
!! POLYGON_IS_CONVEX_TEST tests POLYGON_IS_CONVEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10
  integer ( kind = 4 ), parameter :: test_num = 11
 
  real ( kind = 8 ) angle
  integer ( kind = 4 ) i
  character ( len = 80 ) message(-1:2)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) polygon_is_convex
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) result
  integer ( kind = 4 ) test
  character ( len = 255 ) title
  real ( kind = 8 ) v(2,n_max)

  message(-1) = 'The polygon is not convex.'
  message( 0) = 'The polygon is degenerate and convex.'
  message( 1) = 'The polygon is convex and counterclockwise.'
  message( 2) = 'The polygon is convex and clockwise.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_IS_CONVEX_TEST'
  write ( *, '(a)' ) '  POLYGON_IS_CONVEX determines if a polygon is convex.'

  do test = 1, test_num

    if ( test == 1 ) then
      n = 1
      v(1:2,1:n) = reshape ( (/ &
        0.0D+00, 0.0D+00 /), (/ 2, n /) )
      title = '  A point:'
    else if ( test == 2 ) then
      n = 2
      v(1:2,1:n) = reshape ( (/ &
        0.0D+00, 0.0D+00, &
        1.0D+00, 2.0D+00 /), (/ 2, n /) )
      title = '  A line:'
    else if ( test == 3 ) then
      n = 3
      v(1:2,1:n) = reshape ( (/ &
        0.0D+00, 0.0D+00, &
        2.0D+00, 0.0D+00, &
        1.0D+00, 0.0D+00 /), (/ 2, n /) )
      title = '  A triangle:'
    else if ( test == 4 ) then
      n = 3
      v(1:2,1:n) = reshape ( (/ &
        0.0D+00, 0.0D+00, &
        1.0D+00, 0.0D+00, &
        0.0D+00, 2.0D+00 /), (/ 2, n /) )
      title = '  A CCW triangle:'
    else if ( test == 5 ) then
      n = 3
      v(1:2,1:n) = reshape ( (/ &
        0.0D+00, 0.0D+00, &
        0.0D+00, 2.0D+00, &
        1.0D+00, 0.0D+00 /), (/ 2, n /) )
      title = '  A CW triangle:'
    else if ( test == 6 ) then
      n = 4
      v(1:2,1:n) = reshape ( (/ &
        1.0D+00, 0.0D+00, &
        2.0D+00, 0.0D+00, &
        3.0D+00, 1.0D+00, &
        0.0D+00, 1.0D+00 /), (/ 2, n /) )
      title = '  Polygon with large angle:'
    else if ( test == 7 ) then
      n = 5
      v(1:2,1:n) = reshape ( (/ &
        0.0D+00, 0.0D+00, &
        0.5D+00, 0.5D+00, &
        1.0D+00, 0.0D+00, &
        1.0D+00, 1.0D+00, &
        0.0D+00, 1.0D+00 /), (/ 2, n /) )
      title = '  Polygon with huge angle:'
    else if ( test == 8 ) then
      n = 5
      do i = 1, n
        angle = real ( i - 1, kind = 8 ) * 4.0D+00 * r8_pi &
          / real ( n, kind = 8 )
        v(1,i) = cos ( angle )
        v(2,i) = sin ( angle )
      end do
      title = '  A five-pointed star:'
    else if ( test == 9 ) then
      n = 6
      do i = 1, n
        angle = real ( i - 1, kind = 8 ) * 2.0D+00 * r8_pi &
          / real ( n, kind = 8 )
        v(1,i) = cos ( angle )
        v(2,i) = sin ( angle )
      end do
      title = '  A hexagon:'
    else if ( test == 10 ) then
      n = 6
      v(1:2,1:n) = reshape ( (/ &
        0.0D+00, 0.0D+00, &
        2.0D+00, 0.0D+00, &
        1.0D+00, 1.0D+00, &
        0.0D+00, 0.0D+00, &
        2.0D+00, 0.0D+00, &
        1.0D+00, 1.0D+00 /), (/ 2, n /) )
      title = '  A triangle twice:'
    else if ( test == 11 ) then
      n = 8
      v(1:2,1:n) = reshape ( (/ &
        1.0D+00, 0.0D+00, &
        3.0D+00, 0.0D+00, &
        3.0D+00, 3.0D+00, &
        0.0D+00, 3.0D+00, &
        0.0D+00, 1.0D+00, &
        2.0D+00, 1.0D+00, &
        2.0D+00, 2.0D+00, &
        1.0D+00, 2.0D+00 /), (/ 2, n /) )
      title = '  Square knot:'
    end if

    call r8mat_transpose_print ( 2, n, v, title )
    result = polygon_is_convex ( n, v )
    write ( *, '(2x,a)' ) message(result)

  end do

  return
end
subroutine polygon_lattice_area_test ( )

!*****************************************************************************80
!
!! POLYGON_LATTICE_AREA_TEST tests POLYGON_LATTICE_AREA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) area
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_LATTICE_AREA_TEST'
  write ( *, '(a)' ) '  POLYGON_LATTICE_AREA returns the area'
  write ( *, '(a)' ) '  of a polygon, measured in lattice points.'

  i = 5
  b = 6

  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  Number of interior lattice points = ', i
  write ( *, '(a,i4)' ) '  Number of boundary lattice points = ', b

  call polygon_lattice_area ( i, b, area )

  write ( *, '(a,g14.6)' ) '  Area of polygon is ', area

  return
end
subroutine polygon_outrad_data_test ( )

!*****************************************************************************80
!
!! POLYGON_OUTRAD_DATA_TEST tests POLYGON_OUTRAD_DATA;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) area
  integer ( kind = 4 ) n
  real ( kind = 8 ) radin
  real ( kind = 8 ) radout
  real ( kind = 8 ) side

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_OUTRAD_DATA_TEST'
  write ( *, '(a)' ) '  POLYGON_OUTRAD_DATA uses the inradius of a regular polygon'
  write ( *, '(a)' ) '  to compute area, inradius, and side length.'

  do n = 3, 5

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Number of polygonal sides = ', n

    radout = 1.0D+00
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Assuming RADOUT = ', radout
    call polygon_outrad_data ( n, radout, area, radin, side )
    write ( *, '(a,g14.6)' ) '    AREA =   ', area
    write ( *, '(a,g14.6)' ) '    RADIN =  ', radin
    write ( *, '(a,g14.6)' ) '    SIDE =   ', side

  end do

  return
end
subroutine polygon_perimeter_test ( )

!*****************************************************************************80
!
!! POLYGON_PERIMETER_TEST tests POLYGON_PERIMETER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3
 
  real ( kind = 8 ) perimeter
  real ( kind = 8 ), dimension(2,n1) :: v1 = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    1.0D+00, 1.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, n1 /) )
  real ( kind = 8 ), dimension(2,n2) :: v2 = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    4.0D+00, 3.0D+00, &
    2.0D+00, 5.0D+00 /), (/ 2, n2 /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_PERIMETER_TEST'
  write ( *, '(a)' ) '  POLYGON_PERIMETER computes the perimeter of a polygon.'

  call r8mat_transpose_print ( 2, n1, v1, '  Vertices of polygon V1:' )

  call polygon_perimeter ( n1, v1, perimeter )
  write ( *, '(a)' ) ''
  write ( *, '(a,4x,g14.6)' ) '  Perimeter of V1 = ', perimeter

  call r8mat_transpose_print ( 2, n2, v2, '  Vertices of polygon V2:' )

  call polygon_perimeter ( n2, v2, perimeter )
  write ( *, '(a)' ) ''
  write ( *, '(a,4x,g14.6)' ) '  Perimeter of V2 = ', perimeter

  return
end
subroutine polygon_perimeter_quad_test ( )

!*****************************************************************************80
!
!! POLYGON_PERIMETER_QUAD_TEST tests POLYGON_PERIMETER_QUAD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3
 
  real ( kind = 8 ), external :: f1
  real ( kind = 8 ), external :: fx2
  real ( kind = 8 ) hmax
  integer ( kind = 4 ) i
  real ( kind = 8 ), dimension(2,n1) :: v1 = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    1.0D+00, 1.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, n1 /) )
  real ( kind = 8 ), dimension(2,n2) :: v2 = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    4.0D+00, 3.0D+00, &
    2.0D+00, 5.0D+00 /), (/ 2, n2 /) )
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_PERIMETER_QUAD_TEST'
  write ( *, '(a)' ) '  POLYGON_PERIMETER_QUAD estimates the integral of'
  write ( *, '(a)' ) '  a function over the perimeter of a polygon using'
  write ( *, '(a)' ) '  the composite midpoint rule over each side.'

  call r8mat_transpose_print ( 2, n1, v1, '  Vertices of polygon V1:' )

  hmax = 0.5D+00
  call polygon_perimeter_quad ( n1, v1, hmax, f1, value )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6)' ) '  Using HMAX = ', hmax, ' estimated integral of 1 over perimeter = ', value

  write ( *, '(a)' ) ''
  hmax = 2.0D+00
  do i = 1, 3
    hmax = hmax / 2.0D+00
    call polygon_perimeter_quad ( n1, v1, hmax, fx2, value )
    write ( *, '(a,g14.6,a,g14.6)' ) '  Using HMAX = ', hmax, ' estimated integral of x^2 over perimeter = ', value
  end do

  call r8mat_transpose_print ( 2, n2, v2, '  Vertices of polygon V2:' )

  hmax = 0.5D+00
  call polygon_perimeter_quad ( n2, v2, hmax, f1, value )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6)' ) '  Using HMAX = ', hmax, ' estimated integral of 1 over perimeter = ', value

  write ( *, '(a)' ) ''
  hmax = 2.0D+00
  do i = 1, 3
    hmax = hmax / 2.0D+00
    call polygon_perimeter_quad ( n2, v2, hmax, fx2, value )
    write ( *, '(a,g14.6,a,g14.6)' ) '  Using HMAX = ', hmax, ' estimated integral of x^2 over perimeter = ', value
  end do

  return
end
function f1 ( x, y )

!*****************************************************************************80
!
!! F1 evaluates f(x,y) = 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) f1
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  f1 = 1.0D+00

  return
end
function fx2 ( x, y )

!*****************************************************************************80
!
!! FX2 evaluates f(x,y) = x^2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx2
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  fx2 = x ** 2

  return
end
subroutine polygon_point_dist_test ( )

!*****************************************************************************80
!
!! POLYGON_POINT_DIST_TEST tests POLYGON_POINT_DIST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3
 
  real ( kind = 8 ) dist
  real ( kind = 8 ) p(2)
  real ( kind = 8 ), dimension ( 2, 3 ) :: p_test = reshape ( (/ &
    4.0D+00,  5.0D+00, &
    2.0D+00,  3.0D+00, &
   -2.0D+00, -1.0D+00 /), (/ 2, 3 /) )
  real ( kind = 8 ), dimension(2,n) :: v = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    4.0D+00, 3.0D+00, &
    2.0D+00, 5.0D+00 /), (/ 2, n /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_POINT_DIST_TEST'
  write ( *, '(a)' ) '  POLYGON_POINT_DIST computes polygon-point distance.'

  call r8mat_transpose_print ( 2, n, v, '  Vertices of polygon:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X             Y             DIST'
  write ( *, '(a)' ) ''

  do test = 1, 3

    p(1:2) = p_test(1:2,test)
    call polygon_point_dist ( n, v, p, dist )
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) p(1:2), dist

  end do

  return
end
subroutine polygon_point_near_test ( )

!*****************************************************************************80
!
!! POLYGON_POINT_NEAR_TEST tests POLYGON_POINT_NEAR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3
 
  real ( kind = 8 ) dist
  real ( kind = 8 ) p(2)
  real ( kind = 8 ), dimension ( 2, 3 ) :: p_test = reshape ( (/ &
    4.0D+00,  5.0D+00, &
    2.0D+00,  3.0D+00, &
   -2.0D+00, -1.0D+00 /), (/ 2, 3 /) )
  real ( kind = 8 ) pn(2)
  real ( kind = 8 ), dimension(2,n) :: v = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    4.0D+00, 3.0D+00, &
    2.0D+00, 5.0D+00 /), (/ 2, n /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_POINT_NEAR_TEST'
  write ( *, '(a)' ) '  POLYGON_POINT_NEAR computes nearest point on polygon.'

  call r8mat_transpose_print ( 2, n, v, '  Vertices of polygon:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X             Y             XN             YN'
  write ( *, '(a)' ) ''

  do test = 1, 3

    p(1:2) = p_test(1:2,test)
    call polygon_point_near ( n, v, p, pn, dist )
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) p(1:2), pn(1:2)

  end do

  return
end
subroutine polygon_sample_test ( )

!*****************************************************************************80
!
!! POLYGON_SAMPLE_TEST tests POLYGON_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20
  integer ( kind = 4 ), parameter :: nv = 6

  integer ( kind = 4 ) seed
  real ( kind = 8 ), dimension(2,nv) :: v = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    2.0D+00, 0.0D+00, &
    2.0D+00, 1.0D+00, &
    1.0D+00, 1.0D+00, &
    1.0D+00, 2.0D+00, &
    0.0D+00, 1.0D+00 /), (/ 2, nv /) )
  real ( kind = 8 ) x(2,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_SAMPLE_TEST'
  write ( *, '(a)' ) '  POLYGON_SAMPLE samples a polygon.'

  seed = 123456789

  call polygon_sample ( nv, v, n, seed, x )

  call r8mat_transpose_print ( 2, n, x, '  Sample points:' )

  return
end
subroutine polygon_side_data_test ( )

!*****************************************************************************80
!
!! POLYGON_SIDE_DATA_TEST tests POLYGON_SIDE_DATA;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) area
  integer ( kind = 4 ) n
  real ( kind = 8 ) radin
  real ( kind = 8 ) radout
  real ( kind = 8 ) side

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_SIDE_DATA_TEST'
  write ( *, '(a)' ) '  POLYGON_SIDE_DATA uses the side length of a regular polygon'
  write ( *, '(a)' ) '  to compute the area, inradius, and outradius.'

  do n = 3, 5

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Number of polygonal sides = ', n

    side = 1.0D+00
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Assuming SIDE = ', side
    call polygon_side_data ( n, side, area, radin, radout )
    write ( *, '(a,g14.6)' ) '    AREA =   ', area
    write ( *, '(a,g14.6)' ) '    RADIN =  ', radin
    write ( *, '(a,g14.6)' ) '    RADOUT = ', radout

  end do

  return
end
subroutine polygon_triangulate_test ( )

!*****************************************************************************80
!
!! POLYGON_TRIANGULATE_TEST tests POLYGON_TRIANGULATE.
!
!  Discussion:
!
!    There are N-3 triangles in the triangulation.
!
!    For the first N-2 triangles, the first edge listed is always an
!    internal diagonal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2015
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) j
  integer ( kind = 4 ) triangles(3,n-2)
  real ( kind = 8 ), dimension ( n ) :: x = (/ &
    8.0D+00, 7.0D+00, 6.0D+00, 5.0D+00, 4.0D+00, &
    3.0D+00, 2.0D+00, 1.0D+00, 0.0D+00, 4.0D+00 /)
  real ( kind = 8 ), dimension ( n ) :: y = (/ &
    0.0D+00, 10.0D+00,  0.0D+00, 10.0D+00,  0.0D+00, &
   10.0D+00,  0.0D+00, 10.0D+00,  0.0D+00, -2.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYGON_TRIANGULATE_TEST'
  write ( *, '(a)' ) '  POLYGON_TRIANGULATE triangulates a polygon.'
  write ( *, '(a)' ) '  Here, we triangulate the comb_10 polygon.'

  call polygon_triangulate ( n, x, y, triangles )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Triangles:'
  write ( *, '(a)' ) ''
  do j = 1, n - 2
    write ( *, '(2x,i2,4x,i2,2x,i2,2x,i2)' ) j, triangles(1:3,j)
  end do

  return
end
