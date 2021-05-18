program main

!*****************************************************************************80
!
!! MAIN is the main program for PYRAMID_MONTE_CARLO_TEST.
!
!  Discussion:
!
!    PYRAMID_MONTE_CARLO_TEST tests the PYRAMID_MONTE_CARLO library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PYRAMID_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the PYRAMID_MONTE_CARLO library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PYRAMID_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 estimates integrals over the unit pyramid in 3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: test_num = 10

  real ( kind = 8 ) pyramid01_volume
  integer ( kind = 4 ) e(m)
  integer ( kind = 4 ) :: e_test(m,test_num) = reshape ( (/ &
    0, 0, 0, &
    0, 0, 1, &
    2, 0, 0, &
    0, 2, 0, &
    0, 0, 2, &
    2, 0, 1, &
    0, 2, 1, &
    0, 0, 3, &
    2, 2, 0, &
    2, 0, 2 /), (/ m, test_num /) )
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) result(test_num)
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Use PYRAMID01_SAMPLE to estimate integrals '
  write ( *, '(a)' ) '  over the interior of the unit pyramid in 3D.'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '         N' // &
    '        1' // &
    '               Z' // &
    '             X^2 ' // &
    '             Y^2' // &
    '             Z^2' // &
    '            X^2Z' // &
    '            Y^2Z' // &
    '             Z^3' // &
    '          X^2Y^2' // &
    '          X^2Z^2'
  write ( *, '(a)' ) ' '

  n = 1

  do while ( n <= 65536 )

    allocate ( value(1:n) )
    allocate ( x(1:m,1:n) )

    call pyramid01_sample ( n, seed, x )

    do j = 1, test_num

      e(1:m) = e_test(1:m,j)

      call monomial_value ( m, n, e, x, value )

      result(j) = pyramid01_volume ( ) * sum ( value(1:n) ) &
        / real ( n, kind = 8 )

    end do

    write ( *, '(2x,i8,10(2x,g14.6))' ) n, result(1:10)

    deallocate ( value )
    deallocate ( x )

    n = 2 * n

  end do

  write ( *, '(a)' ) ' '

  do j = 1, 10

    e(1:m) = e_test(1:m,j)

    call pyramid01_integral ( e, result(j) )

  end do

  write ( *, '(2x,a8,10(2x,g14.6))' ) '   Exact', result(1:10)

  return
end
