program main

!*****************************************************************************80
!
!! MAIN is the main program for DISK_RULE_TEST.
!
!  Discussion:
!
!    DISK_RULE_TEST tests the DISK_RULE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DISK_RULE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the DISK_RULE library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DISK_RULE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests DISK_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nr = 4
  integer ( kind = 4 ), parameter :: nt = 8

  real ( kind = 8 ) area
  integer ( kind = 4 ) d
  integer ( kind = 4 ) e(2)
  integer ( kind = 4 ) e1
  integer ( kind = 4 ) e2
  real ( kind = 8 ) :: exact(15) = (/ &
       9.0D+00, &
       9.0D+00,            18.0D+00, &
     117.0D+00 / 4.0D+00,  18.0D+00,            225.0D+00 / 4.0D+00, &
     279.0D+00 / 4.0D+00, 117.0D+00 / 2.0D+00,  225.0D+00 / 4.0D+00, 387.0D+00 / 2.0D+00, &
    1773.0D+00 / 8.0D+00, 279.0D+00 / 2.0D+00, 1341.0D+00 / 8.0D+00, 387.0D+00 / 2.0D+00, 5769.0D+00 / 8.0D+00 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) q
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) rc
  real ( kind = 8 ) s
  real ( kind = 8 ) w(nr,nt)
  real ( kind = 8 ) x(nr,nt)
  real ( kind = 8 ) xc
  real ( kind = 8 ) y(nr,nt)
  real ( kind = 8 ) yc

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  DISK_RULE can compute a rule Q(f) for a general disk'
  write ( *, '(a)' ) '  with center (XC,YC) and radius RC,'
  write ( *, '(a)' ) '  using NT equally spaced angles and NR radial distances.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  NT = ', nt
  write ( *, '(a,i4)' ) '  NR = ', nr
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Estimate integrals I(f) where f = x^e(1) * y^e(2).'
!
!  Define the general disk.
!
  xc = 1.0D+00
  yc = 2.0D+00
  rc = 3.0D+00
!
!  Put in the factor of PI in the exact values.
!
  exact(1:15) = exact(1:15) * r8_pi
!
!  Compute the quadrature rule.
!
  call disk_rule ( nr, nt, xc, yc, rc, w, x, y )
!
!  Apply it to integrands.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  E(1)  E(2)    I(f)            Q(f)' 
  write ( *, '(a)' ) ' '
!
!  Specify a monomial.
!
  k = 0

  do d = 0, 4

    do e1 = d, 0, -1

      e2 = d - e1
      e(1) = e1
      e(2) = e2

      s = 0.0D+00
      do j = 1, nt
        do i = 1, nr
          s = s + w(i,j) * x(i,j) ** e(1) * y(i,j) ** e(2)
        end do
      end do

      area = r8_pi * rc * rc
      q = area * s

      call disk01_monomial_integral ( e, exact )

      k = k + 1
      write ( *, '(3x,i2,3x,i2,2x,g14.6,2x,g14.6)' ) e(1), e(2), exact(k), q

    end do

  end do

  return
end

