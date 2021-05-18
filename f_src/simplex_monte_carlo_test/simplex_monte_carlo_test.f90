program main

!*****************************************************************************80
!
!! MAIN is the main program for SIMPLEX_MONTE_CARLO_TEST.
!
!  Discussion:
!
!    SIMPLEX_MONTE_CARLO_TEST tests the SIMPLEX_MONTE_CARLO library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SIMPLEX_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SIMPLEX_MONTE_CARLO library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SIMPLEX_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 uses SIMPLEX_UNIT_SAMPLE to estimate integrals in 3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3

  integer ( kind = 4 ) e(m)
  integer ( kind = 4 ) :: e_test(m,10) = reshape ( (/ &
    0, 0, 0, &
    1, 0, 0, &
    0, 1, 0, &
    0, 0, 1, &
    2, 0, 0, &
    1, 1, 0, &
    1, 0, 1, &
    0, 2, 0, &
    0, 1, 1, &
    0, 0, 2 /), (/ m, 10 /) )
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) result(10)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) simplex_unit_volume
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Use SIMPLEX_UNIT_SAMPLE for a Monte Carlo estimate of an'
  write ( *, '(a)' ) '  integral over the interior of the unit simplex in 3D.'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         N        1               X               Y ' // &
    '              Z               X^2              XY             XZ' // &
    '              Y^2             YZ               Z^2'
  write ( *, '(a)' ) ' '

  n = 1

  do while ( n <= 65536 )

    allocate ( value(1:n) )
    allocate ( x(1:m,1:n) )

    call simplex_unit_sample ( m, n, seed, x )

    do j = 1, 10

      e(1:m) = e_test(1:m,j)

      call monomial_value ( m, n, e, x, value )

      result(j) = simplex_unit_volume ( m ) * sum ( value(1:n) ) &
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

    call simplex_unit_monomial_integral ( m, e, result(j) )

  end do

  write ( *, '(2x,a8,10(2x,g14.6))' ) '   Exact', result(1:10)

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 uses SIMPLEX_UNIT_SAMPLE to estimate integrals in 6D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6

  integer ( kind = 4 ) e(m)
  integer ( kind = 4 ) :: e_test(m,7) = reshape ( (/ &
    0, 0, 0, 0, 0, 0, &
    1, 0, 0, 0, 0, 0, &
    0, 2, 0, 0, 0, 0, &
    0, 2, 2, 0, 0, 0, &
    0, 0, 0, 4, 0, 0, &
    2, 0, 0, 0, 2, 2, &
    0, 0, 0, 0, 0, 6 /), (/ m, 7 /) )
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) result(7)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) simplex_unit_volume
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Use SIMPLEX_UNIT_SAMPLE for a Monte Carlo estimate of an'
  write ( *, '(a)' ) '  integral over the interior of the unit simplex in 6D.'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '         N' // &
    '        1      ' // &
    '        U      ' // &
    '         V^2   ' // &
    '         V^2W^2' // &
    '         X^4   ' // &
    '         Y^2Z^2' // &
    '         Z^6'
  write ( *, '(a)' ) ' '

  n = 1

  do while ( n <= 65536 )

    allocate ( value(1:n) )
    allocate ( x(1:m,1:n) )

    call simplex_unit_sample ( m, n, seed, x )

    do j = 1, 7

      e(1:m) = e_test(1:m,j)

      call monomial_value ( m, n, e, x, value )

      result(j) = simplex_unit_volume ( m ) * sum ( value(1:n) ) &
        / real ( n, kind = 8 )

    end do

    write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

    deallocate ( value )
    deallocate ( x )

    n = 2 * n

  end do

  write ( *, '(a)' ) ' '

  do j = 1, 7

    e(1:m) = e_test(1:m,j)

    call simplex_unit_monomial_integral ( m, e, result(j) )

  end do

  write ( *, '(2x,a8,7(2x,g14.6))' ) '   Exact', result(1:7)

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 uses SIMPLEX_GENERAL_SAMPLE to estimate integrals in 3D.
!
!  Discussion:
!
!    Integration is over a general simplex.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3

  integer ( kind = 4 ) e(m)
  integer ( kind = 4 ) :: e_test(m,10) = reshape ( (/ &
    0, 0, 0, &
    1, 0, 0, &
    0, 1, 0, &
    0, 0, 1, &
    2, 0, 0, &
    1, 1, 0, &
    1, 0, 1, &
    0, 2, 0, &
    0, 1, 1, &
    0, 0, 2 /), (/ m, 10 /) )
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) result(10)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) simplex_general_volume
  real ( kind = 8 ) :: t(3,4) = reshape ( (/ &
    1.0, 0.0, 0.0, &
    2.0, 0.0, 0.0, &
    1.0, 2.0, 0.0, &
    1.0, 0.0, 3.0 /), (/ 3, 4 /) )
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  SIMPLEX_GENERAL_SAMPLE computes a Monte Carlo estimate'
  write ( *, '(a)' ) '  of an integral over the interior of a general simplex.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Simplex vertices:'
  write ( *, '(a)' ) ''
  do j = 1, 4
    write ( *, '(3g14.6)' ) t(1:3,j)
  end do

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         N        1               X               Y ' // &
    '              Z               X^2              XY             XZ' // &
    '              Y^2             YZ               Z^2'
  write ( *, '(a)' ) ' '

  n = 1

  do while ( n <= 65536 )

    allocate ( value(1:n) )
    allocate ( x(1:m,1:n) )

    call simplex_general_sample ( m, n, t, seed, x )

    do j = 1, 10

      e(1:m) = e_test(1:m,j)

      call monomial_value ( m, n, e, x, value )

      result(j) = simplex_general_volume ( m, t ) * sum ( value(1:n) ) &
        / real ( n, kind = 8 )

    end do

    write ( *, '(2x,i8,10(2x,g14.6))' ) n, result(1:10)

    deallocate ( value )
    deallocate ( x )

    n = 2 * n

  end do

  return
end
