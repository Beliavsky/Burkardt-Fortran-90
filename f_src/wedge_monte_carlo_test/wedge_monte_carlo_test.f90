program main

!*****************************************************************************80
!
!! MAIN is the main program for WEDGE_MONTE_CARLO_TEST.
!
!  Discussion:
!
!    WEDGE_MONTE_CARLO_TEST tests the WEDGE_MONTE_CARLO library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WEDGE_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the WEDGE_MONTE_CARLO library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WEDGE_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 uses WEDGE01_SAMPLE with an increasing number of points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3

  integer ( kind = 4 ) e(m)
  integer ( kind = 4 ) :: e_test(m,8) = reshape ( (/ &
    0, 0, 0, &
    1, 0, 0, &
    0, 1, 0, &
    0, 0, 1, &
    2, 0, 0, &
    1, 1, 0, &
    0, 0, 2, &
    3, 0, 0 /), (/ m, 8 /) )
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) result(8)
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:,:)
  real ( kind = 8 ) wedge01_volume

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Use WEDGE01_SAMPLE for a Monte Carlo estimate of an'
  write ( *, '(a)' ) '  integral over the interior of the unit wedge in 3D.'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         N        1               X               Y ' // &
    '              Z                X^2            XY              Z^2    ' // &
    '        X^3'
  write ( *, '(a)' ) ' '

  n = 1

  do while ( n <= 65536 )

    allocate ( value(1:n) )
    allocate ( x(1:m,1:n) )

    call wedge01_sample ( n, seed, x )

    do j = 1, 8

      e(1:m) = e_test(1:m,j)

      call monomial_value ( m, n, e, x, value )

      result(j) = wedge01_volume ( ) * sum ( value(1:n) ) / real ( n, kind = 8 )

    end do

    write ( *, '(2x,i8,8(2x,g14.6))' ) n, result(1:8)

    deallocate ( value )
    deallocate ( x )

    n = 2 * n

  end do

  write ( *, '(a)' ) ' '

  do j = 1, 8

    e(1:m) = e_test(1:m,j)

    call wedge01_integral ( e, result(j) )

  end do

  write ( *, '(2x,a8,8(2x,g14.6))' ) '   Exact', result(1:8)

  return
end
