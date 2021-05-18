program main

!*****************************************************************************80
!
!! MAIN is the main program for WEDGE_INTEGRALS_TEST.
!
!  Discussion:
!
!    WEDGE_INTEGRALS_TEST tests the WEDGE_INTEGRALS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WEDGE_INTEGRALS_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the WEDGE_INTEGRALS library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WEDGE_INTEGRALS_TEST'
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
!    19 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 500000

  integer ( kind = 4 ), parameter :: e_max = 6
  integer ( kind = 4 ) e1
  integer ( kind = 4 ) e2
  integer ( kind = 4 ) e3
  integer ( kind = 4 ) expon(m)
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  real ( kind = 8 ) q
  integer ( kind = 4 ) seed
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) wedge01_volume
  real ( kind = 8 ) x(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Compare exact and estimated integrals '
  write ( *, '(a)' ) '  over the unit wedge in 3D.'
!
!  Get sample points.
!
  seed = 123456789
  call wedge01_sample ( n, seed, x )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of sample points used is ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   E1  E2  E3     MC-Estimate      Exact           Error'
  write ( *, '(a)' ) ''
!
!  Check all monomials up to total degree E_MAX.
!
  do e3 = 0, e_max
    expon(3) = e3
    do e2 = 0, e_max - e3
      expon(2) = e2
      do e1 = 0, e_max - e3 - e2
        expon(1) = e1

        call monomial_value ( m, n, expon, x, value )

        q = wedge01_volume ( ) * sum ( value(1:n) ) / real ( n, kind = 8 )
        call wedge01_integral ( expon, exact )
        error = abs ( q - exact )

        write ( *, '(2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) &
          expon(1:3), q, exact, error

      end do
    end do
  end do

  return
end

