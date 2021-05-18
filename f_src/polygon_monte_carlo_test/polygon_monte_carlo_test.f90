program main

!*****************************************************************************80
!
!! MAIN is the main program for POLYGON_MONTE_CARLO_TEST.
!
!  Discussion:
!
!    POLYGON_MONTE_CARLO_TEST tests the POLYGON_MONTE_CARLO library.
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
  implicit none

  integer ( kind = 4 ), parameter :: nv1 = 4

  real ( kind = 8 ), dimension ( 2, nv1 ) :: v1 = reshape ( (/ &
    -1.0, -1.0, &
     1.0, -1.0, &
     1.0,  1.0, &
    -1.0,  1.0 /), (/ 2, nv1 /) )

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the POLYGON_MONTE_CARLO library.'

  call test01 ( nv1, v1 )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLYGON_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( nv, v )

!*****************************************************************************80
!
!! TEST01 estimates integrals over a polygon in 2D.
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
  implicit none

  integer ( kind = 4 ) nv

  integer ( kind = 4 ) e(2)
  integer ( kind = 4 ) :: e_test(2,7) = reshape ( (/ &
    0, 0, &
    2, 0, &
    0, 2, &
    4, 0, &
    2, 2, &
    0, 4, &
    6, 0 /), (/ 2, 7 /) )
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) polygon_area
  real ( kind = 8 ) result(7)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v(2,nv)
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Use POLYGON_SAMPLE to estimate integrals '
  write ( *, '(a)' ) '  over the interior of a polygon in 2D.'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '         N' // &
    '        1' // &
    '              X^2 ' // &
    '             Y^2' // &
    '             X^4' // &
    '           X^2Y^2' // &
    '             Y^4' // &
    '           X^6'
  write ( *, '(a)' ) ' '

  n = 1

  do while ( n <= 65536 )

    allocate ( value(1:n) )
    allocate ( x(1:2,1:n) )

    call polygon_sample ( nv, v, n, seed, x )

    do j = 1, 7

      e(1:2) = e_test(1:2,j)

      call monomial_value ( 2, n, e, x, value )

      result(j) = polygon_area ( nv, v ) * sum ( value(1:n) ) &
        / real ( n, kind = 8 )

    end do

    write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

    deallocate ( value )
    deallocate ( x )

    n = 2 * n

  end do

  write ( *, '(a)' ) ' '

  do j = 1, 7

    e(1:2) = e_test(1:2,j)

    call polygon_monomial_integral ( nv, v, e, result(j) )

  end do

  write ( *, '(2x,a8,7(2x,g14.6))' ) '   Exact', result(1:7)

  return
end
