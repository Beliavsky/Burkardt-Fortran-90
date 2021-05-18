program main

!*****************************************************************************80
!
!! MAIN is the main program for NS3DE_TEST.
!
!  Discussion:
!
!    NS3DE_TEST tests the NS3DE library.
!
!  Location:
!
!    http://people.sc.fsu.edu/~jburkardt/f_src/navier_stokes_3d_exact/ns3de_test.f90
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NS3DE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the NS3DE library.'

  call uvwp_burgers_test ( )
  call resid_burgers_test ( )

  call uvwp_ethier_test ( )
  call resid_ethier_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NS3DE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  call timestamp ( )

  stop 0
end
subroutine uvwp_burgers_test ( )

!*****************************************************************************80
!
!! UVWP_BURGERS_TEST tests UVWP_BURGERS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) nu
  real ( kind = 8 ), allocatable :: p(:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: t(:)
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ), allocatable :: v(:)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xyz_hi
  real ( kind = 8 ) xyz_lo
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ), allocatable :: z(:)

  nu = 0.25D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UVWP_BURGERS_TEST'
  write ( *, '(a)' ) '  UVWP_BURGERS evaluates the Burgers solution.'
  write ( *, '(a)' ) '  Estimate the range of velocity and pressure'
  write ( *, '(a)' ) '  at the initial time T = 0, in a region that is the'
  write ( *, '(a)' ) '  cube centered at (0,0,0) with "radius" 1.0.'
  write ( *, '(a,g14.6)' ) '  Viscosity NU = ', nu

  n = 1000

  allocate ( p(1:n) )
  allocate ( u(1:n) )
  allocate ( v(1:n) )
  allocate ( w(1:n) )
  allocate ( x(1:n) )
  allocate ( y(1:n) )
  allocate ( z(1:n) )
  allocate ( t(1:n) )

  xyz_lo = -1.0D+00
  xyz_hi = +1.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, x )
  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, y )
  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, z )
  t(1:n) = 0.0D+00

  call uvwp_burgers ( nu, n, x, y, z, t, u, v, w, p )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '           Minimum       Maximum'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,2x,g14.6)' ) '  U:  ', minval ( u ), maxval ( u )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  V:  ', minval ( v ), maxval ( v )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  W:  ', minval ( w ), maxval ( w )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  P:  ', minval ( p ), maxval ( p )

  deallocate ( p )
  deallocate ( t )
  deallocate ( u )
  deallocate ( v )
  deallocate ( w )
  deallocate ( x )
  deallocate ( y )
  deallocate ( z )

  return
end
subroutine resid_burgers_test ( )

!*****************************************************************************80
!
!! RESID_BURGERS_TEST tests RESID_BURGERS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) nu
  real ( kind = 8 ), allocatable :: pr(:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: t(:)
  real ( kind = 8 ), allocatable :: ur(:)
  real ( kind = 8 ), allocatable :: vr(:)
  real ( kind = 8 ), allocatable :: wr(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xyz_hi
  real ( kind = 8 ) xyz_lo
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ), allocatable :: z(:)

  nu = 0.25D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RESID_BURGERS_TEST'
  write ( *, '(a)' ) '  RESID_BURGERS evaluates the Burgers residual.'
  write ( *, '(a)' ) '  Sample the Navier-Stokes residuals'
  write ( *, '(a)' ) '  at the initial time T = 0, using a region that is'
  write ( *, '(a)' ) '  the cube centered at (0,0,0) with "radius" 1.0,'
  write ( *, '(a,g14.6)' ) '  Viscosity NU = ', nu

  n = 1000

  allocate ( pr(1:n) )
  allocate ( ur(1:n) )
  allocate ( vr(1:n) )
  allocate ( wr(1:n) )
  allocate ( x(1:n) )
  allocate ( y(1:n) )
  allocate ( z(1:n) )
  allocate ( t(1:n) )

  xyz_lo = -1.0D+00
  xyz_hi = +1.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, x )
  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, y )
  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, z )
  t(1:n) = 0.0D+00

  call resid_burgers ( nu, n, x, y, z, t, ur, vr, wr, pr )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '           Minimum       Maximum'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,2x,g14.6)' ) '  Ur:  ', minval ( abs ( ur ) ), maxval ( abs ( ur ) )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  Vr:  ', minval ( abs ( vr ) ), maxval ( abs ( vr ) )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  Wr:  ', minval ( abs ( wr ) ), maxval ( abs ( wr ) )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  Pr:  ', minval ( abs ( pr ) ), maxval ( abs ( pr ) )

  deallocate ( pr )
  deallocate ( t )
  deallocate ( ur )
  deallocate ( vr )
  deallocate ( wr )
  deallocate ( x )
  deallocate ( y )
  deallocate ( z )

  return
end
subroutine uvwp_ethier_test ( )

!*****************************************************************************80
!
!! UVWP_ETHIER_TEST tests UVWP_ETHIER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) d
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: p(:)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: t(:)
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ), allocatable :: v(:)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xyz_hi
  real ( kind = 8 ) xyz_lo
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ), allocatable :: z(:)

  a = r8_pi / 4.0D+00
  d = r8_pi / 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UVWP_ETHIER_TEST'
  write ( *, '(a)' ) '  UVWP_ETHIER evaluates the Ethier solution.'
  write ( *, '(a)' ) '  Estimate the range of velocity and pressure'
  write ( *, '(a)' ) '  at the initial time T = 0, in a region that is the'
  write ( *, '(a)' ) '  cube centered at (0,0,0) with "radius" 1.0.'
  write ( *, '(a,g14.6)' ) '  Parameter A = ', a
  write ( *, '(a,g14.6)' ) '  Parameter D = ', d

  n = 1000

  allocate ( p(1:n) )
  allocate ( u(1:n) )
  allocate ( v(1:n) )
  allocate ( w(1:n) )
  allocate ( x(1:n) )
  allocate ( y(1:n) )
  allocate ( z(1:n) )
  allocate ( t(1:n) )

  xyz_lo = -1.0D+00
  xyz_hi = +1.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, x )
  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, y )
  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, z )
  t(1:n) = 0.0D+00

  call uvwp_ethier ( a, d, n, x, y, z, t, u, v, w, p )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '           Minimum       Maximum'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,2x,g14.6)' ) '  U:  ', minval ( u ), maxval ( u )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  V:  ', minval ( v ), maxval ( v )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  W:  ', minval ( w ), maxval ( w )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  P:  ', minval ( p ), maxval ( p )

  deallocate ( p )
  deallocate ( t )
  deallocate ( u )
  deallocate ( v )
  deallocate ( w )
  deallocate ( x )
  deallocate ( y )
  deallocate ( z )

  return
end
subroutine resid_ethier_test ( )

!*****************************************************************************80
!
!! RESID_ETHIER_TEST tests RESID_ETHIER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) d
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: pr(:)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: t(:)
  real ( kind = 8 ), allocatable :: ur(:)
  real ( kind = 8 ), allocatable :: vr(:)
  real ( kind = 8 ), allocatable :: wr(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xyz_hi
  real ( kind = 8 ) xyz_lo
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ), allocatable :: z(:)

  a = r8_pi / 4.0D+00
  d = r8_pi / 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RESID_ETHIER_TEST'
  write ( *, '(a)' ) '  RESID_ETHIER evaluates the Ethier residual.'
  write ( *, '(a)' ) '  Sample the Navier-Stokes residuals'
  write ( *, '(a)' ) '  at the initial time T = 0, using a region that is'
  write ( *, '(a)' ) '  the cube centered at (0,0,0) with "radius" 1.0,'
  write ( *, '(a,g14.6)' ) '  Parameter A = ', a
  write ( *, '(a,g14.6)' ) '  Parameter D = ', d

  n = 1000

  allocate ( pr(1:n) )
  allocate ( ur(1:n) )
  allocate ( vr(1:n) )
  allocate ( wr(1:n) )
  allocate ( x(1:n) )
  allocate ( y(1:n) )
  allocate ( z(1:n) )
  allocate ( t(1:n) )

  xyz_lo = -1.0D+00
  xyz_hi = +1.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, x )
  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, y )
  call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, z )
  t(1:n) = 0.0D+00

  call resid_ethier ( a, d, n, x, y, z, t, ur, vr, wr, pr )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '           Minimum       Maximum'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,2x,g14.6)' ) '  Ur:  ', minval ( abs ( ur ) ), maxval ( abs ( ur ) )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  Vr:  ', minval ( abs ( vr ) ), maxval ( abs ( vr ) )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  Wr:  ', minval ( abs ( wr ) ), maxval ( abs ( wr ) )
  write ( *, '(a,g14.6,2x,g14.6)' ) '  Pr:  ', minval ( abs ( pr ) ), maxval ( abs ( pr ) )

  deallocate ( pr )
  deallocate ( t )
  deallocate ( ur )
  deallocate ( vr )
  deallocate ( wr )
  deallocate ( x )
  deallocate ( y )
  deallocate ( z )

  return
end


