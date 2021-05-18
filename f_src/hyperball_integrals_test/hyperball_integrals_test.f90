program main

!*****************************************************************************80
!
!! MAIN is the main program for HYPERBALL_INTEGRALS_TEST.
!
!  Discussion:
!
!    HYPERBALL_INTEGRALS_TEST tests the HYPERBALL_INTEGRALS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HYPERBALL_INTEGRALS_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the HYPERBALL_INTEGRALS library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HYPERBALL_INTEGRALS_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 uses HYPERBALL01_SAMPLE to compare exact and estimated integrals in 3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3

  integer ( kind = 4 ) e(m)
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  real ( kind = 8 ) hyperball01_volume
  integer ( kind = 4 ), parameter :: n = 4192
  real ( kind = 8 ) result
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 20
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Use the Monte Carlo method to estimate integrals over'
  write ( *, '(a)' ) '  the interior of the unit hyperball in M dimensions.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
!
!  Get sample points.
!
  seed = 123456789
  allocate ( x(1:m,1:n) )
  call hyperball01_sample ( m, n, seed, x )
  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of sample points used is ', n
!
!  Randomly choose exponents between 0 and 8.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  If any exponent is odd, the integral is zero.'
  write ( *, '(a)' ) '  We will restrict this test to randomly chosen even exponents.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Ex  Ey  Ez     MC-Estimate           Exact      Error'
  write ( *, '(a)' ) ''

  allocate ( value(1:n) )

  do test = 1, test_num

    call i4vec_uniform_ab ( m, 0, 4, seed, e )

    e(1:m) = e(1:m) * 2

    call monomial_value ( m, n, e, x, value )

    result = hyperball01_volume ( m ) * sum ( value(1:n) ) &
      / real ( n, kind = 8 )
    call hyperball01_monomial_integral ( m, e, exact )
    error = abs ( result - exact )

    write ( *, '(2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) &
      e(1:m), result, exact, error

  end do

  deallocate ( value )
  deallocate ( x )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 uses HYPERBALL01_SAMPLE to compare exact and estimated integrals in 6D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6

  integer ( kind = 4 ) e(m)
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  real ( kind = 8 ) hyperball01_volume
  integer ( kind = 4 ), parameter :: n = 4192
  real ( kind = 8 ) result
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 20
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Use the Monte Carlo method to estimate integrals over'
  write ( *, '(a)' ) '  the interior of the unit hyperball in M dimensions.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
!
!  Get sample points.
!
  seed = 123456789
  allocate ( x(1:m,1:n) )
  call hyperball01_sample ( m, n, seed, x )
  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of sample points used is ', n
!
!  Randomly choose exponents between 0 and 8.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  If any exponent is odd, the integral is zero.'
  write ( *, '(a)' ) '  We will restrict this test to randomly chosen even exponents.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  E1  E2  E3  E4  E5  E6     MC-Estimate           Exact      Error'
  write ( *, '(a)' ) ''

  allocate ( value(1:n) )

  do test = 1, test_num

    call i4vec_uniform_ab ( m, 0, 3, seed, e )

    e(1:m) = e(1:m) * 2

    call monomial_value ( m, n, e, x, value )

    result = hyperball01_volume ( m ) * sum ( value(1:n) ) &
      / real ( n, kind = 8 )
    call hyperball01_monomial_integral ( m, e, exact )
    error = abs ( result - exact )

    write ( *, '(6(2x,i2),2x,g14.6,2x,g14.6,2x,e10.2)' ) &
      e(1:m), result, exact, error

  end do

  deallocate ( value )
  deallocate ( x )

  return
end
