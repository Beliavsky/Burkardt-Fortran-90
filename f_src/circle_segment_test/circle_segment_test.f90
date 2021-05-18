program main

!*****************************************************************************80
!
!! MAIN is the main program for CIRCLE_SEGMENT_TEST.
!
!  Discussion:
!
!    CIRCLE_SEGMENT_TEST tests the CIRCLE_SEGMENT library.
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
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CIRCLE_SEGMENT_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the CIRCLE_SEGMENT library.'

  call test01 ( )
  call test05 ( )
  call test06 ( )
  call test07 ( )
  call test08 ( )
  call test09 ( )
  call test11 ( )
  call test13 ( )
  call test14 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CIRCLE_SEGMENT_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests CIRCLE_SEGMENT_AREA_FROM_HEIGHT.
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
  implicit none

  real ( kind = 8 ) area
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  real ( kind = 8 ) r

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) &
    '  CIRCLE_SEGMENT_AREA_FROM_HEIGHT computes the area of a circle segment.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '          R               H               Area'
  write ( *, '(a)' ) ' '
  r = 1.0D+00
  h = 1.0D+00
  do i = 0, 10
    call circle_segment_area_from_height ( r, h, area )
    write ( *, '(2x,f14.6,2x,f14.6,2x,f14.6)' ) r, h, area
    h = h / 2.0D+00
  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests the AREA and HEIGHT functions.
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
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) h
  real ( kind = 8 ) h2
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CIRCLE_SEGMENT_TEST05'
  write ( *, '(a)' ) '  For circle segment with a given radius R,'
  write ( *, '(a)' ) '  CIRCLE_SEGMENT_AREA_FROM_HEIGHT computes the area A, given the height.'
  write ( *, '(a)' ) '  CIRCLE_SEGMENT_HEIGHT_FROM_AREA computes height H, given the area.'
  write ( *, '(a)' ) '  Check that these functions are inverses of each other'
  write ( *, '(a)' ) '  using random values of R, A, and H.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        R             H      =>     A    =>       H2'
  write ( *, '(a)' ) ''

  seed = 123456789

  do test = 1, 5
    r = 5.0D+00 * r8_uniform_01 ( seed )
    h = 2.0D+00 * r * r8_uniform_01 ( seed )
    call circle_segment_area_from_height ( r, h, a )
    call circle_segment_height_from_area ( r, a, h2 )
    write ( *, '(2x,f12.6,2x,f12.6,2x,f12.6,2x,f12.6)' ) r, h, a, h2
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        R             A      =>     H    =>       A2'
  write ( *, '(a)' ) ''

  do test = 1, 5
    r = 5.0D+00 * r8_uniform_01 ( seed )
    a = pi * r * r * r8_uniform_01 ( seed )
    call circle_segment_height_from_area ( r, a, h )
    call circle_segment_area_from_height ( r, h, a2 )
    write ( *, '(2x,f12.6,2x,f12.6,2x,f12.6,2x,f12.6)' ) r, a, h, a2
  end do

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 samples using CIRCLE_SEGMENT_SAMPLE_FROM_HEIGHT.
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
  implicit none

  integer ( kind = 4 ), parameter :: an_num = 51
  integer ( kind = 4 ), parameter :: boundary_num = an_num + 1
  integer ( kind = 4 ), parameter :: data_num = 100

  real ( kind = 8 ) an(an_num)
  character ( len = 255 ) boundary_filename
  integer ( kind = 4 ) boundary_unit
  real ( kind = 8 ) boundary_x(boundary_num)
  real ( kind = 8 ) boundary_y(boundary_num)
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) data_x(data_num)
  real ( kind = 8 ) data_y(data_num)
  character ( len = 255 ) graphics_filename
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ) theta
  real ( kind = 8 ) thetah

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CIRCLE_SEGMENT_TEST06'
  write ( *, '(a)' ) '  CIRCLE_SEGMENT_SAMPLE_FROM_HEIGHT samples a circle segment.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i4,a)' ) '  Plot ', data_num, ' points from several segments.'
  write ( *, '(a)' ) ''

  r = 1.0D+00
  theta = pi

  data_filename = 'sample00_data.txt'
  boundary_filename = 'sample00_boundary.txt'
  command_filename = 'sample00_commands.txt'
  graphics_filename = 'sample00.png'

  do test = 1, 4

    call circle_segment_height_from_angle ( r, theta, h )

    thetah = theta / 2.0D+00
!
!  Create boundary.
!
    call r8vec_linspace ( an_num, -thetah, +thetah, an )
    an(1:an_num) = an(1:an_num) + 0.5D+00 * pi

    boundary_x(1:an_num) = r * cos ( an(1:an_num) )
    boundary_x(an_num+1) = boundary_x(1)
    boundary_y(1:an_num) = r * sin ( an(1:an_num) )
    boundary_y(an_num+1) = boundary_y(1)
    call get_unit ( boundary_unit )
    call filename_inc ( boundary_filename )
    open ( unit = boundary_unit, file = boundary_filename, status = 'replace' )
    do i = 1, boundary_num
      write ( boundary_unit, '(2x,g14.6,2x,g14.6)' ) boundary_x(i), boundary_y(i)
    end do
    close ( unit = boundary_unit )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Created boundary file "' // trim ( boundary_filename ) // '".'
!
!  Create data.
!
    call circle_segment_sample_from_height ( r, h, data_num, seed, data_x, data_y )

    call get_unit ( data_unit )
    call filename_inc ( data_filename )
    open ( unit = data_unit, file = data_filename, status = 'replace' )
    do i = 1, data_num
      write ( data_unit, '(2x,g14.6,2x,g14.6)' ) data_x(i), data_y(i)
    end do
    close ( unit = data_unit )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'
!
!  Create commands.
!
    call get_unit ( command_unit )
    call filename_inc ( command_filename )
    open ( unit = command_unit, file = command_filename, status = 'replace' )
    write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
    write ( command_unit, '(a)' ) '#'
    write ( command_unit, '(a)' ) '# Usage:'
    write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
    write ( command_unit, '(a)' ) '#'
    write ( command_unit, '(a)' ) 'set term png'
    call filename_inc ( graphics_filename )
    write ( command_unit, '(a)' ) 'set output "' // trim ( graphics_filename ) // '"'
    write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
    write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
    write ( command_unit, '(a)' ) 'set title "Circle Segment Sample"'
    write ( command_unit, '(a)' ) 'set grid'
    write ( command_unit, '(a)' ) 'set key off'
    write ( command_unit, '(a)' ) 'set size ratio -1'
    write ( command_unit, '(a)' ) 'set style data lines'
    write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
      '" using 1:2 with points lt 3 pt 3,\'
    write ( command_unit, '(a)' ) '    "' // trim ( boundary_filename ) // &
      '" using 1:2 lw 3 linecolor rgb "black"'
    write ( command_unit, '(a)' ) 'quit'
    close ( unit = command_unit )

    write ( *, '(a)' ) &
      '  Created command file "' // trim ( command_filename ) // '".'

    theta = theta / 2.0D+00

  end do
 
  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! TEST07 tests the ANGLE and HEIGHT functions.
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
  implicit none

  real ( kind = 8 ) h
  real ( kind = 8 ) h2
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t
  real ( kind = 8 ) t2
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST07'
  write ( *, '(a)' ) '  For circle segment with a given radius R,'
  write ( *, '(a)' ) '  CIRCLE_SEGMENT_ANGLE_FROM_HEIGHT computes the angle THETA, given the height.'
  write ( *, '(a)' ) '  CIRCLE_SEGMENT_HEIGHT_FROM_ANGLE computes height H, given the angle.'
  write ( *, '(a)' ) '  Check that these functions are inverses of each other'
  write ( *, '(a)' ) '  using random values of R, T, and H.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        R             H      =>     T    =>       H2'
  write ( *, '(a)' ) ''

  seed = 123456789

  do test = 1, 5
    r = 5.0D+00 * r8_uniform_01 ( seed )
    h = 2.0D+00 * r * r8_uniform_01 ( seed )
    call circle_segment_angle_from_height ( r, h, t )
    call circle_segment_height_from_angle ( r, t, h2 )
    write ( *, '(2x,f12.6,2x,f12.6,2x,f12.6,2x,f12.6)' ) r, h, t, h2
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        R             T      =>     H    =>       T2'
  write ( *, '(a)' ) ''
  do test = 1, 5
    r = 5.0D+00 * r8_uniform_01 ( seed )
    t = 2.0D+00 * pi * r8_uniform_01 ( seed )
    call circle_segment_height_from_angle ( r, t, h )
    call circle_segment_angle_from_height ( r, h, t2 )
    write ( *, '(2x,f12.6,2x,f12.6,2x,f12.6,2x,f12.6)' ) r, t, h, t2
  end do

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 tests CIRCLE_SEGMENT_CONTAINS_POINT.
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
  implicit none

  integer ( kind = 4 ), parameter :: n = 1000

  real ( kind = 8 ) area
  real ( kind = 8 ) area_est
  real ( kind = 8 ) c(2)
  integer ( kind = 4 ) inout(n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) omega1
  real ( kind = 8 ) omega2
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ) theta
  real ( kind = 8 ) xy(2,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST08'
  write ( *, '(a)' ) '  CIRCLE_SEGMENT_CONTAINS_POINT reports whether'
  write ( *, '(a)' ) '  a circle segment contains a point.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Pick a circle segment at random.'
  write ( *, '(a,i4,a)' ) '  Compute ', n, ' sample points in the surrounding box.'
  write ( *, '(a)' ) '  Compare the area of the segment to the percentage of points'
  write ( *, '(a)' ) '  contained in the circle segment.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       N       Omega1          Omega2           Area         Estimate'
  write ( *, '(a)' ) ' '

  r = 1.0D+00
  c(1) = 0.0D+00
  c(2) = 0.0D+00
  seed = 123456789

  do test = 1, 5

    omega1 = 2.0D+00 * pi * r8_uniform_01 ( seed )
    omega2 = 2.0D+00 * pi * r8_uniform_01 ( seed )
  
    if ( omega2 < omega1 ) then
      omega2 = omega2 + 2.0D+00 * pi
    end if

    call r8mat_uniform_01 ( 2, n, seed, xy )
    xy(1:2,1:n) = 2.0D+00 * xy(1:2,1:n) - 1.0D+00

    do j = 1, n
      call circle_segment_contains_point ( r, c, omega1, omega2, xy(1:2,j), inout(j) )
    end do

    call circle_segment_angle_from_chord_angles ( omega1, omega2, theta )
    call circle_segment_area_from_angle ( r, theta, area )
    area_est = 4.0D+00 * real ( sum ( inout(1:n) ), kind = 8 ) / real ( n, kind = 8 )

    write ( *, '(2x,i6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      n, omega1, omega2, area, area_est

  end do

  return
end
subroutine test09 ( )

!*****************************************************************************80
!
!! CIRCLE_SEGMENT_TEST09 looks at the area and centroid calculations.
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
  implicit none

  real ( kind = 8 ) a1
  real ( kind = 8 ) a2
  real ( kind = 8 ) a3
  real ( kind = 8 ) c(2)
  real ( kind = 8 ) d1(2)
  real ( kind = 8 ) d2(2)
  real ( kind = 8 ) d3(2)
  real ( kind = 8 ) h
  integer ( kind = 4 ) n
  real ( kind = 8 ) omega1
  real ( kind = 8 ) omega2
  real ( kind = 8 ) p1(2)
  real ( kind = 8 ) p2(2)
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  integer ( kind = 4 ) seed
  real ( kind = 8 ) theta

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CIRCLE_SEGMENT_TEST09'
  write ( *, '(a)' ) '  CIRCLE_SEGMENT_AREA_FROM_CHORD and'
  write ( *, '(a)' ) '  CIRCLE_SEGMENT_CENTROID_FROM_CHORD evaluate the area'
  write ( *, '(a)' ) '  and centroid of a circle segment, given R, C and P1:P2.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  CIRCLE_SEGMENT_AREA_FROM_SAMPLE and'
  write ( *, '(a)' ) '  CIRCLE_SEGMENT_CENTROID_FROM_SAMPLE give us Monte Carlo estimates.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  GQCIRCSEGM can estimate these values by quadrature.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Start easy, with R = 1, C = (0,0), and Theta centered.'

  seed = 123456789
  r = 1.0D+00
  c = (/ 0.0D+00, 0.0D+00 /)
  theta = pi / 4.0D+00
  call circle_segment_height_from_angle ( r, theta, h )
  omega1 = - theta / 2.0D+00
  omega2 = + theta / 2.0D+00
  p1(1) = c(1) + r * cos ( omega1 )
  p1(2) = c(2) + r * sin ( omega1 )
  p2(1) = c(1) + r * cos ( omega2 )
  p2(2) = c(2) + r * sin ( omega2 )

  call circle_segment_area_from_chord ( r, c, p1, p2, a1 )
  call circle_segment_centroid_from_chord ( r, c, p1, p2, d1 )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         Area          CentroidX    CentroidY'
  write ( *, '(a)' ) ''

  write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) a1, d1(1), d1(2)
!
!  This only works because the centroid of the height-based circle segment 
!  is easily transformed to the centroid of the chord based circle segment.
!
  call circle_segment_area_from_height ( r, h, a2 )
  call circle_segment_centroid_from_height ( r, h, d2 )

  write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) a2, d2(2), -d2(1)

  n = 10000
  call circle_segment_area_from_sample ( r, c, p1, p2, n, seed, a3 )
  call circle_segment_centroid_from_sample ( r, c, p1, p2, n, seed, d3 )
  write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) a3, d3(1), d3(2)

  return
end
subroutine test11 ( )

!*****************************************************************************80
!
!! TEST11 demonstrates CIRCLE_SEGMENT_ROTATION_FROM_CHORD.
!
!  Discussion:
!
!    We make a table of all pairs of angles that are multiples of pi/12.
!
!    For each pair, we compute the rotation, that is, the angle of the
!    central radius of the circle segment.  We print out the result in
!    terms of multiples of pi/12.
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
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) c(2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) p1(2)
  real ( kind = 8 ) p2(2)
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) rho1
  real ( kind = 8 ) rho2
  real ( kind = 8 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST11:'
  write ( *, '(a)' ) '  CIRCLE_SEGMENT_ROTATION_FROM_CHORD is given the endpoints'
  write ( *, '(a)' ) '  of a chord, and is asked to determine the angle of the'
  write ( *, '(a)' ) '  central radius vector.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  We make a table of all pairs of angles that are multiples'
  write ( *, '(a)' ) '  of pi/12, determine the corresponding chord endpoints, and'
  write ( *, '(a)' ) '  compute the rotation angle, also printed as a multiple of pi/12.'

  r = 2.0D+00
  c(1:2) = (/ 3.0D+00, 5.0D+00 /)
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     0.0   1.0   2.0   3.0   4.0   5.0   6.0   7.0' &
    // '   8.0   9.0  10.0  11.0  12.0'
  write ( *, '(a)' ) ''
  do i = 0, 12
    rho1 = real ( i, kind = 8 ) * pi / 6.0D+00
    p1(1:2) = c(1:2) + r * (/ cos ( rho1 ), sin ( rho1 ) /)
    write ( *, '(i2)', advance = 'no' ) i
    do j = 0, 12
      rho2 = real ( j, kind = 8 ) * pi / 6.0D+00
      p2(1:2) = c(1:2) + r * (/ cos ( rho2 ), sin ( rho2 ) /)
      call circle_segment_rotation_from_chord ( r, c, p1, p2, alpha )
      t = 6.0D+00 * alpha / pi
      write ( *, '(2x,f4.1)', advance = 'no' ) t 
    end do
    write ( *, '(a)' ) ' '
  end do

  return
end
subroutine test13 ( )

!*****************************************************************************80
!
!! TEST13 demonstrates GAUSS for quadrature rules.
!
!  Discussion:
!
!    Some recursion coefficients ALPHA and BETA are listed in Kautsky
!    and Elhay.
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
!  Reference
!
!    Jaroslav Kautsky, Sylvan Elhay,
!    Calculation of the Weights of Interpolatory Quadratures,
!    Numerische Mathematik,
!    Volume 40, Number 3, October 1982, pages 407-422.
!
  implicit none

  real ( kind = 8 ), allocatable :: alpha(:)
  real ( kind = 8 ), allocatable :: beta(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST13'
  write ( *, '(a)' ) '  GAUSS computes the points and weights for a'
  write ( *, '(a)' ) '  Gauss quadrature rule, given the ALPHA and BETA'
  write ( *, '(a)' ) '  recursion coefficients.'
!
!  Legendre rule.
!
  n = 10

  allocate ( alpha(1:n) )
  allocate ( beta(1:n) )

  do i = 1, n
    alpha(i) = 0.0D+00
    if ( i == 1 ) then
      beta(i) = 2.0D+00
    else
      beta(i) = 1.0D+00 / ( 4.0D+00 - 1.0D+00 / real ( ( i - 1 )**2, kind = 8 ) )
    end if
  end do

  allocate ( x(1:n) )
  allocate ( w(1:n) )

  call gauss ( n, alpha, beta, x, w )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  LEGENDRE RULE'
  write ( *, '(a)' ) '  Point   Weight'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,g14.6,2x,g14.6)' ) x(i), w(i)
  end do

  deallocate ( alpha )
  deallocate ( beta )
  deallocate ( w )
  deallocate ( x )
!
!  Hermite rule.
!
  n = 10

  allocate ( alpha(1:n) )
  allocate ( beta(1:n) )

  do i = 1, n
    alpha(i) = 0.0D+00
    if ( i == 1 ) then
      beta(i) = sqrt ( pi )
    else
      beta(i) = real ( i - 1, kind = 8 ) / 2.0D+00
    end if
  end do

  allocate ( x(1:n) )
  allocate ( w(1:n) )

  call gauss ( n, alpha, beta, x, w )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  HERMITE RULE'
  write ( *, '(a)' ) '  Point   Weight'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,g14.6,2x,g14.6)' ) x(i), w(i)
  end do

  deallocate ( alpha )
  deallocate ( beta )
  deallocate ( w )
  deallocate ( x )
!
!  Laguerre rule.
!
  n = 10

  allocate ( alpha(1:n) )
  allocate ( beta(1:n) )

  do i = 1, n
    alpha(i) = 2.0D+00 * real ( i, kind = 8 ) - 1.0D+00
    if ( i == 1 ) then
      beta(i) = 1.0D+00
    else
      beta(i) = real ( ( i - 1 ) ** 2, kind = 8 )
    end if
  end do

  allocate ( x(1:n) )
  allocate ( w(1:n) )

  call gauss ( n, alpha, beta, x, w )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  LAGUERRE RULE'
  write ( *, '(a)' ) '  Point   Weight'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,g14.6,2x,g14.6)' ) x(i), w(i)
  end do

  deallocate ( alpha )
  deallocate ( beta )
  deallocate ( w )
  deallocate ( x )

  return
end
subroutine test14 ( )

!*****************************************************************************80
!
!! TEST14 demonstrates R_JACOBI.
!
!  Discussion:
!
!    R_JACOBI returns recursion coefficients ALPHA and BETA for rules
!    using a Jacobi type weight w(x) = (1-x)^A * (1+x)^B.
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
!    John Burkardt
!
!  Reference
!
!    Walter Gautschi,
!    Orthogonal Polynomials: Computation and Approximation,
!    Oxford, 2004,
!    ISBN: 0-19-850672-4,
!    LC: QA404.5 G3555.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), allocatable :: alpha(:)
  real ( kind = 8 ) b
  real ( kind = 8 ), allocatable :: beta(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST14'
  write ( *, '(a)' ) '  R_JACOBI computes recursion coefficients ALPHA and BETA'
  write ( *, '(a)' ) '  Gauss quadrature rule, given the ALPHA and BETA'
  write ( *, '(a)' ) '  recursion coefficients.'
!
!  Legendre rule.
!
  n = 10

  a = 0.0D+00
  b = 0.0D+00
  allocate ( alpha(1:n) )
  allocate ( beta(1:n) )

  call r_jacobi ( n, a, b, alpha, beta )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Legendre weight'
  write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, '  B = ', b
  write ( *, '(a)' ) '  Alpha          Beta'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,g14.6,2x,g14.6)' ) alpha(i), beta(i)
  end do

  deallocate ( alpha )
  deallocate ( beta )
!
!  Chebyshev Type 1 rule.
!
  n = 10

  a = -0.5D+00
  b = -0.5D+00
  allocate ( alpha(1:n) )
  allocate ( beta(1:n) )

  call r_jacobi ( n, a, b, alpha, beta )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Chebyshev Type 1 weight'
  write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, '  B = ', b
  write ( *, '(a)' ) '  Alpha          Beta'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,g14.6,2x,g14.6)' ) alpha(i), beta(i)
  end do

  deallocate ( alpha )
  deallocate ( beta )
!
!  Chebyshev Type 2 rule.
!
  n = 10

  a = +0.5D+00
  b = +0.5D+00
  allocate ( alpha(1:n) )
  allocate ( beta(1:n) )

  call r_jacobi ( n, a, b, alpha, beta )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Chebyshev Type 2 weight'
  write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, '  B = ', b
  write ( *, '(a)' ) '  Alpha          Beta'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,g14.6,2x,g14.6)' ) alpha(i), beta(i)
  end do

  deallocate ( alpha )
  deallocate ( beta )
!
!  General Jacobi rule.
!
  n = 10

  a = +0.5D+00
  b = +1.5D+00
  allocate ( alpha(1:n) )
  allocate ( beta(1:n) )

  call r_jacobi ( n, a, b, alpha, beta )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  General Jacobi weight'
  write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, '  B = ', b
  write ( *, '(a)' ) '  Alpha          Beta'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,g14.6,2x,g14.6)' ) alpha(i), beta(i)
  end do

  deallocate ( alpha )
  deallocate ( beta )

  return
end
