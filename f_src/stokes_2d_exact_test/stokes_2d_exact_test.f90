program main

!*****************************************************************************80
!
!! stokes_2d_exact_test tests the stokes_2d_exact library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'stokes_2d_exact_test'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the stokes_2d_exact library.'

  call uvp_stokes1_test ( )
  call resid_stokes1_test ( )
  call gnuplot_stokes1_test ( )

  call uvp_stokes2_test ( )
  call resid_stokes2_test ( )
  call gnuplot_stokes2_test ( )

  call uvp_stokes3_test ( )
  call resid_stokes3_test ( )
  call gnuplot_stokes3_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'stokes_2d_exact_test'
  write ( *, '(a)' ) '  Normal end of execution.'
  call timestamp ( )

  stop 0
end
subroutine uvp_stokes1_test ( )

!*****************************************************************************80
!
!! UVP_STOKES1_TEST samples the solution #1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 1000

  real ( kind = 8 ) p(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xy_hi
  real ( kind = 8 ) xy_lo
  real ( kind = 8 ) y(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UVP_STOKES1_TEST'
  write ( *, '(a)' ) '  Exact Stokes solution #1:'
  write ( *, '(a)' ) '  Estimate the range of velocity and pressure'
  write ( *, '(a)' ) '  using a region that is the unit square.'

  xy_lo = 0.0D+00
  xy_hi = 1.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

  call uvp_stokes1 ( n, x, y, u, v, p )

  write (  *, '(a)' ) ''
  write (  *, '(a)' ) '           Minimum       Maximum'
  write (  *, '(a)' ) ''
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', minval ( u ), maxval ( u )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', minval ( v ), maxval ( v )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', minval ( p ), maxval ( p )

  return
end
subroutine resid_stokes1_test ( )

!*****************************************************************************80
!
!! RESID_STOKES1_TEST samples the residual for solution #1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 1000

  real ( kind = 8 ) pr(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xy_hi
  real ( kind = 8 ) xy_lo
  real ( kind = 8 ) y(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RESID_STOKES1_TEST'
  write ( *, '(a)' ) '  Exact Stokes solution #1:'
  write ( *, '(a)' ) '  Sample the Stokes residuals'
  write ( *, '(a)' ) '  using a region that is the unit square.'

  xy_lo = 0.0D+00
  xy_hi = 1.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

  call resid_stokes1 ( n, x, y, ur, vr, pr )

  write (  *, '(a)' ) ''
  write (  *, '(a)' ) '           Minimum       Maximum'
  write (  *, '(a)' ) ''
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Ur:  ', &
    minval ( abs ( ur ) ), maxval ( abs ( ur ) )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Vr:  ', &
    minval ( abs ( vr ) ), maxval ( abs ( vr ) )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Pr:  ', &
    minval ( abs ( pr ) ), maxval ( abs ( pr ) )

  return
end
subroutine gnuplot_stokes1_test ( )

!*****************************************************************************80
!
!! GNUPLOT_STOKES1_TEST plots solution #1 on a regular grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: x_num = 21
  integer ( kind = 4 ), parameter :: y_num = 21

  character ( len = 255 ) header
  integer ( kind = 4 ) n
  real ( kind = 8 ) p(x_num,y_num)
  real ( kind = 8 ) s
  real ( kind = 8 ) u(x_num,y_num)
  real ( kind = 8 ) v(x_num,y_num)
  real ( kind = 8 ) x(x_num,y_num)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo
  real ( kind = 8 ) y(x_num,y_num)
  real ( kind = 8 ) y_hi
  real ( kind = 8 ) y_lo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GNUPLOT_STOKES1_TEST:'
  write ( *, '(a)' ) '  Exact Stokes solution #1:'
  write ( *, '(a)' ) '  Generate a Stokes velocity field on a regular grid.'
  write ( *, '(a)' ) '  Store in GNUPLOT data and command files.'
   
  x_lo = 0.0D+00
  x_hi = 1.0D+00

  y_lo = 0.0D+00
  y_hi = 1.0D+00

  call grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

  n = x_num * y_num

  call uvp_stokes1 ( n, x, y, u, v, p )

  header = 'stokes1'
  s = 4.0D+00
  call stokes_gnuplot ( header, n, x, y, u, v, s )

  return
end
subroutine uvp_stokes2_test ( )

!*****************************************************************************80
!
!! UVP_STOKES2_TEST samples the solution #2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 1000

  real ( kind = 8 ) p(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xy_hi
  real ( kind = 8 ) xy_lo
  real ( kind = 8 ) y(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UVP_STOKES2_TEST'
  write ( *, '(a)' ) '  Exact Stokes solution #2:'
  write ( *, '(a)' ) '  Estimate the range of velocity and pressure'
  write ( *, '(a)' ) '  using a region that is the unit square.'

  xy_lo = 0.0D+00
  xy_hi = 1.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

  call uvp_stokes2 ( n, x, y, u, v, p )

  write (  *, '(a)' ) ''
  write (  *, '(a)' ) '           Minimum       Maximum'
  write (  *, '(a)' ) ''
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', minval ( u ), maxval ( u )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', minval ( v ), maxval ( v )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', minval ( p ), maxval ( p ) 

  return
end
subroutine resid_stokes2_test ( )

!*****************************************************************************80
!
!! RESID_STOKES2_TEST samples the residual for solution #1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 1000

  real ( kind = 8 ) pr(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xy_hi
  real ( kind = 8 ) xy_lo
  real ( kind = 8 ) y(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RESID_STOKES2_TEST'
  write ( *, '(a)' ) '  Exact Stokes solution #2:'
  write ( *, '(a)' ) '  Sample the Stokes residuals'
  write ( *, '(a)' ) '  using a region that is the unit square.'

  xy_lo = 0.0D+00
  xy_hi = 1.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

  call resid_stokes2 ( n, x, y, ur, vr, pr )

  write (  *, '(a)' ) ''
  write (  *, '(a)' ) '           Minimum       Maximum'
  write (  *, '(a)' ) ''
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Ur:  ', &
    minval ( abs ( ur ) ), maxval ( abs ( ur ) )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Vr:  ', &
    minval ( abs ( vr ) ), maxval ( abs ( vr ) )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Pr:  ', &
    minval ( abs ( pr ) ), maxval ( abs ( pr ) )

  return
end
subroutine gnuplot_stokes2_test ( )

!*****************************************************************************80
!
!! GNUPLOT_STOKES2_TEST plots solution #2 on a regular grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: x_num = 21
  integer ( kind = 4 ), parameter :: y_num = 21

  character * ( 255 ) header
  integer ( kind = 4 ) n
  real ( kind = 8 ) p(x_num,y_num)
  real ( kind = 8 ) s
  real ( kind = 8 ) u(x_num,y_num)
  real ( kind = 8 ) v(x_num,y_num)
  real ( kind = 8 ) x(x_num,y_num)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo
  real ( kind = 8 ) y(x_num,y_num)
  real ( kind = 8 ) y_hi
  real ( kind = 8 ) y_lo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GNUPLOT_STOKES2_TEST:'
  write ( *, '(a)' ) '  Exact Stokes solution #2:'
  write ( *, '(a)' ) '  Generate a Stokes velocity field on a regular grid.'
  write ( *, '(a)' ) '  Store in GNUPLOT data and command files.'
   
  x_lo = 0.0D+00
  x_hi = 1.0D+00

  y_lo = 0.0D+00
  y_hi = 1.0D+00

  call grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

  n = x_num * y_num

  call uvp_stokes2 ( n, x, y, u, v, p )

  header = 'stokes2'
  s = 0.05D+00
  call stokes_gnuplot ( header, n, x, y, u, v, s )

  return
end
subroutine uvp_stokes3_test ( )

!*****************************************************************************80
!
!! UVP_STOKES3_TEST samples the solution #3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 1000

  real ( kind = 8 ) p(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xy_hi
  real ( kind = 8 ) xy_lo
  real ( kind = 8 ) y(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UVP_STOKES3_TEST'
  write ( *, '(a)' ) '  Exact Stokes solution #3:'
  write ( *, '(a)' ) '  Estimate the range of velocity and pressure'
  write ( *, '(a)' ) '  using a region that is [-1,+1]x[-1,+1].'

  xy_lo =-1.0D+00
  xy_hi =+1.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

  call uvp_stokes3 ( n, x, y, u, v, p )

  write (  *, '(a)' ) ''
  write (  *, '(a)' ) '           Minimum       Maximum'
  write (  *, '(a)' ) ''
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', minval ( u ), maxval ( u )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', minval ( v ), maxval ( v )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', minval ( p ), maxval ( p )

  return
end
subroutine resid_stokes3_test ( )

!*****************************************************************************80
!
!! RESID_STOKES3_TEST samples the residual for solution #3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 1000

  real ( kind = 8 ) pr(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xy_hi
  real ( kind = 8 ) xy_lo
  real ( kind = 8 ) y(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RESID_STOKES3_TEST'
  write ( *, '(a)' ) '  Exact Stokes solution #3:'
  write ( *, '(a)' ) '  Sample the Stokes residuals'
  write ( *, '(a)' ) '  using a region that is [-1,+1]x[-1,+1].'

  xy_lo =-1.0D+00
  xy_hi =+1.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
  call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

  call resid_stokes3 ( n, x, y, ur, vr, pr )

  write (  *, '(a)' ) ''
  write (  *, '(a)' ) '           Minimum       Maximum'
  write (  *, '(a)' ) ''
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Ur:  ', &
    minval ( abs ( ur ) ), maxval ( abs ( ur ) )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Vr:  ', &
    minval ( abs ( vr ) ), maxval ( abs ( vr ) )
  write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Pr:  ', &
    minval ( abs ( pr ) ), maxval ( abs ( pr ) )

  return
end
subroutine gnuplot_stokes3_test ( )

!*****************************************************************************80
!
!! GNUPLOT_STOKES3_TEST plots solution #3 on a regular grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: x_num = 21
  integer ( kind = 4 ), parameter :: y_num = 21

  character ( len = 255 ) header
  integer ( kind = 4 ) n
  real ( kind = 8 ) p(x_num,y_num)
  real ( kind = 8 ) s
  real ( kind = 8 ) u(x_num,y_num)
  real ( kind = 8 ) v(x_num,y_num)
  real ( kind = 8 ) x(x_num,y_num)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo
  real ( kind = 8 ) y(x_num,y_num)
  real ( kind = 8 ) y_hi
  real ( kind = 8 ) y_lo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GNUPLOT_STOKES3_TEST:'
  write ( *, '(a)' ) '  Exact Stokes solution #3:'
  write ( *, '(a)' ) '  Generate a Stokes velocity field on a regular grid.'
  write ( *, '(a)' ) '  Store in GNUPLOT data and command files.'
   
  x_lo = -1.0D+00
  x_hi = +1.0D+00

  y_lo = -1.0D+00
  y_hi = +1.0D+00

  call grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

  n = x_num * y_num

  call uvp_stokes3 ( n, x, y, u, v, p )

  header = 'stokes3'
  s = 0.05D+00
  call stokes_gnuplot ( header, n, x, y, u, v, s )

  return
end
