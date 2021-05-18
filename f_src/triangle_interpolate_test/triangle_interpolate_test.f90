program main

!*****************************************************************************80
!
!! TRIANGLE_INTERPOLATE_TEST tests the TRIANGLE_INTERPOLATE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_INTERPOLATE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the TRIANGLE_INTERPOLATE library.'

  call triangle_interpolate_linear_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_INTERPOLATE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine triangle_interpolate_linear_test ( )

!*****************************************************************************80
!
!! TRIANGLE_INTERPOLATE_LINEAR_TEST tests TRIANGLE_INTERPOLATE_LINEAR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) j
  real ( kind = 8 ) p(2,n)
  real ( kind = 8 ) :: p1(2) = (/ 0.0D+00, 1.0D+00 /)
  real ( kind = 8 ) :: p2(2) = (/ 5.0D+00, 0.0D+00 /)
  real ( kind = 8 ) :: p3(2) = (/ 4.0D+00, 4.0D+00 /)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v(m,n)
  real ( kind = 8 ) :: v1(m) = (/ 1.0D+00, 0.0D+00, 0.0D+00 /)
  real ( kind = 8 ) :: v2(m) = (/ 0.0D+00, 1.0D+00, 0.0D+00 /)
  real ( kind = 8 ) :: v3(m) = (/ 0.0D+00, 0.0D+00, 1.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_INTERPOLATE_LINEAR_TEST'
  write ( *, '(a)' ) '  TRIANGLE_INTERPOLATE_LINEAR uses linear interpolation'
  write ( *, '(a)' ) '  on vertex data to estimate values in the interior.'
!
!  Get N sample points inside the triangle.
!
  seed = 123456789
  call uniform_in_triangle_map1 ( p1, p2, p3, n, seed, p )
!
!  Request an intepolated value for R, G and B at each point.
!
  call triangle_interpolate_linear ( m, n, p1, p2, p3, p, v1, v2, v3, v )
!
!  Report the data.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X               Y               V(1)' // &
    '            V(2)            V(3)'
  write ( *, '(a)' ) ''
  write ( *, '(5(2x,g14.6))' ) p1(1:2), v1(1:m)
  write ( *, '(5(2x,g14.6))' ) p2(1:2), v2(1:m)
  write ( *, '(5(2x,g14.6))' ) p3(1:2), v3(1:m)
  write ( *, '(a)' ) ''
  do j = 1, n
    write ( *, '(5(2x,g14.6))' ) p(1:2,j), v(1:m,j)
  end do

  return
end
