subroutine constants ( g, c, h )

!*****************************************************************************80
!
!! constants() stores, and returns constants "g", "c" and "h".
!
!  Discussion:
!
!    Calling [g,c,h]=constants() returns the values of g, c, and h.
!
!    Because the values never change, and don't need to be computed,
!    we use assignment statements here, instead of persistent data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2021
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    real ( kind = 8 ) G: gravitational constant m^3/s^2/kg
!
!    real ( kind = 8 ) C: light speed, m/s.
!
!    real ( kind = 8 ) H: Planck's constant, j s;
!
  implicit none

  real ( kind = 8 ) c
  real ( kind = 8 ) g
  real ( kind = 8 ) h

  g = 6.67384D-11;
  c = 2.99792458D+8;
  h = 6.626070040D-34;

  return
end

