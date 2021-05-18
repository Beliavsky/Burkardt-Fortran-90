program main

!*****************************************************************************80
!
!! MAIN is the main program for CIRCLE_RULE_TEST.
!
!  Discussion:
!
!    CIRCLE_RULE_TEST tests the CIRCLE_RULE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) nt

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CIRCLE_RULE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the CIRCLE_RULE library.'

  nt = 8
  call test01 ( nt )

  nt = 32
  call test01 ( nt )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CIRCLE_RULE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( nt )

!*****************************************************************************80
!
!! TEST01 tests CIRCLE_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) nt

  integer ( kind = 4 ) e(2)
  integer ( kind = 4 ) e1
  integer ( kind = 4 ) e2
  real ( kind = 8 ) exact
  integer ( kind = 4 ) i
  real ( kind = 8 ) q
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) t(nt)
  real ( kind = 8 ) w(nt)
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  CIRCLE_RULE can compute a rule Q(f) for the unit circle'
  write ( *, '(a)' ) '  using NT equally spaced angles.'
  write ( *, '(a)' ) '  Estimate integrals I(f) where f = x^e(1) * y^e(2)'
  write ( *, '(a,i4,a)' ) '  using ', nt, ' points.'
!
!  Compute the quadrature rule.
!
  call circle_rule ( nt, w, t )
!
!  Apply it to integrands.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  E(1)  E(2)    I(f)            Q(f)' 
  write ( *, '(a)' ) ' '
!
!  Specify a monomial.
!
  do e1 = 0, 6, 2

    e(1) = e1

    do e2 = e1, 6, 2

      e(2) = e2

      q = 0.0D+00
      do i = 1, nt
        x = cos ( t(i) )
        y = sin ( t(i) )
        q = q + w(i) * x ** e(1) * y ** e(2)
      end do

      q = 2.0D+00 * r8_pi * q

      call circle01_monomial_integral ( e, exact )

      write ( *, '(3x,i2,3x,i2,2x,g14.6,2x,g14.6)' ) e(1), e(2), exact, q

    end do

  end do

  return
end

