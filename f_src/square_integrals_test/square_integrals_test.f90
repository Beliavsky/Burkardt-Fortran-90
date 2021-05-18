program main

!*****************************************************************************80
!
!! MAIN is the main program for SQUARE_INTEGRALS_TEST.
!
!  Discussion:
!
!    SQUARE_INTEGRALS_TEST tests the SQUARE_INTEGRALS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SQUARE_INTEGRALS_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SQUARE_INTEGRALS library.'

  call square01_monomial_integral_test ( )
  call squaresym_monomial_integral_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SQUARE_INTEGRALS_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine square01_monomial_integral_test ( )

!*****************************************************************************80
!
!! SQUARE01_MONOMIAL_INTEGRAL_TEST tests SQUARE01_MONOMIAL_INTEGRAL.
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

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ), parameter :: n = 4192

  integer ( kind = 4 ) e(m)
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  real ( kind = 8 ) result
  integer ( kind = 4 ) seed
  real ( kind = 8 ) square01_area
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 20
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SQUARE01_MONOMIAL_INTEGRAL_TEST'
  write ( *, '(a)' ) '  SQUARE01_MONOMIAL_INTEGRAL returns the exact integral'
  write ( *, '(a)' ) '  of a monomial over the interior of the unit square in 2D.'
!
!  Get sample points.
!
  seed = 123456789
  call square01_sample ( n, seed, x )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of sample points used is ', n
!
!  Randomly choose exponents.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Ex  Ey     MC-Estimate           Exact      Error'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    call i4vec_uniform_ab ( m, 0, 7, seed, e )

    call monomial_value ( m, n, e, x, value )

    result = square01_area ( ) * sum ( value(1:n) ) &
      / real ( n, kind = 8 )
    call square01_monomial_integral ( e, exact )
    error = abs ( result - exact )

    write ( *, '(2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) &
      e(1:m), result, exact, error

  end do

  return
end
subroutine squaresym_monomial_integral_test ( )

!*****************************************************************************80
!
!! SQUARESYM_MONOMIAL_INTEGRAL_TEST tests SQUARESYM_MONOMIAL_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ), parameter :: n = 4192

  integer ( kind = 4 ) e(m)
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  real ( kind = 8 ) result
  integer ( kind = 4 ) seed
  real ( kind = 8 ) squaresym_area
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 20
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SQUARESYM_MONOMIAL_INTEGRAL_TEST'
  write ( *, '(a)' ) '  SQUARESYM_MONOMIAL_INTEGRAL returns the exact integral'
  write ( *, '(a)' ) '  of a monomial over the interior of the '
  write ( *, '(a)' ) '  symmetric unit square in 2D.'
!
!  Get sample points.
!
  seed = 123456789
  call squaresym_sample ( n, seed, x )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of sample points used is ', n
!
!  Randomly choose exponents.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Ex  Ey     MC-Estimate           Exact      Error'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    call i4vec_uniform_ab ( m, 0, 7, seed, e )

    call monomial_value ( m, n, e, x, value )

    result = squaresym_area ( ) * sum ( value(1:n) ) &
      / real ( n, kind = 8 )
    call squaresym_monomial_integral ( e, exact )
    error = abs ( result - exact )

    write ( *, '(2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) &
      e(1:m), result, exact, error

  end do

  return
end
