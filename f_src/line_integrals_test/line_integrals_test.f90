program main

!*****************************************************************************80
!
!! MAIN is the main program for LINE_INTEGRALS_TEST.
!
!  Discussion:
!
!    LINE_INTEGRALS_TEST tests the LINE_INTEGRALS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINE_INTEGRALS_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LINE_INTEGRALS library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINE_INTEGRALS_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 compares exact and estimated monomial integrals.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 1
  integer ( kind = 4 ), parameter :: n = 4192

  integer ( kind = 4 ) e
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  real ( kind = 8 ) line01_length
  real ( kind = 8 ) result
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 11
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Compare exact and estimated integrals '
  write ( *, '(a)' ) '  over the length of the unit line in 1D.'
!
!  Get sample points.
!
  seed = 123456789
  call line01_sample ( n, seed, x )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of sample points used is ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   E     MC-Estimate      Exact           Error'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    e = test - 1

    call monomial_value_1d ( n, e, x, value )

    result = line01_length ( ) * sum ( value(1:n) ) / real ( n, kind = 8 )
    call line01_monomial_integral ( e, exact )
    error = abs ( result - exact )

    write ( *, '(2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) &
      e, result, exact, error

  end do

  return
end
