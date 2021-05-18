program main

!*****************************************************************************80
!
!! MAIN is the main program for TRIANGLE_SVG_TEST.
!
!  Discussion:
!
!    TRIANGLE_SVG_TEST tests the TRIANGLE_SVG library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_SVG_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the TRIANGLE_SVG library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_SVG_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 calls TRIANGLE_SVG to plot a triangle and some points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: p_num = 13

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) f
  real ( kind = 8 ) g
  real ( kind = 8 ) h
  real ( kind = 8 ) p(2,p_num)
  character ( len = 255 ) plot_filename
  real ( kind = 8 ) t(2,3)

  h = 1.0D+00 / 3.0D+00
  a = 0.479308067841923D+00
  b = 0.260345966079038D+00
  c = 0.869739794195568D+00
  d = 0.065130102902216D+00
  e = 0.638444188569809D+00
  f = 0.312865496004875D+00
  g = 0.048690315425316D+00

  p(1,1:13) =   (/ h, a, b, b, c, d, d, e, e, f, f, g, g /)
  p(2,1:13) =   (/ h, b, a, b, d, c, d, f, g, e, g, e, f /)

  t = reshape ( (/ &
    0.0, 0.0, &
    1.0, 0.0, &
    0.0, 1.0 /), (/ 2, 3 /) )

  plot_filename = 'test01.svg'

  call triangle_svg ( plot_filename, t, p_num, p )

  return
end
