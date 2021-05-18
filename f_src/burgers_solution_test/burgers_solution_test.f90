program main

!*****************************************************************************80
!
!! MAIN is the main program for BURGERS_SOLUTION_TEST.
!
!  Discussion:
!
!    BURGERS_SOLUTION_TEST tests the BURGERS_SOLUTION library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BURGERS_SOLUTION_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BURGERS_SOLUTION library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BURGERS_SOLUTION_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests sets up a small test case.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: vtn = 11
  integer ( kind = 4 ), parameter :: vxn = 11

  character ( len = 80 ) filename
  real ( kind = 8 ) nu
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) thi
  real ( kind = 8 ) tlo
  real ( kind = 8 ) vu(vxn,vtn)
  real ( kind = 8 ) vt(vtn)
  real ( kind = 8 ) vx(vxn)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo

  nu = 0.01D+00 / r8_pi

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  BURGERS_VISCOUS_TIME_EXACT1 computes'
  write ( *, '(a)' ) '  exact solution #1 to the Burgers equation.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Viscosity NU = ', nu
  write ( *, '(a,i4)' ) '  NX = ', vxn
  write ( *, '(a,i4)' ) '  NT = ', vtn

  xlo = -1.0D+00
  xhi = +1.0D+00
  call r8vec_even ( vxn, xlo, xhi, vx )
  call r8vec_print ( vxn, vx, '  X grid points:' )

  tlo = 0.0D+00
  thi = 3.0D+00 / r8_pi
  call r8vec_even ( vtn, tlo, thi, vt )
  call r8vec_print ( vtn, vt, '  T grid points:' )

  call burgers_viscous_time_exact1 ( nu, vxn, vx, vtn, vt, vu )

  call r8mat_print ( vxn, vtn, vu, '  U(X,T) at grid points:' )

  filename = 'burgers_solution_test01.txt'

  call r8mat_write ( filename, vxn, vtn, vu )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Data written to file "' // trim ( filename ) // '".'

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests sets up a finer test case.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: vtn = 41
  integer ( kind = 4 ), parameter :: vxn = 41

  character ( len = 80 ) filename
  real ( kind = 8 ) nu
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) thi
  real ( kind = 8 ) tlo
  real ( kind = 8 ) vu(vxn,vtn)
  real ( kind = 8 ) vt(vtn)
  real ( kind = 8 ) vx(vxn)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo

  nu = 0.01D+00 / r8_pi

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  BURGERS_VISCOUS_TIME_EXACT1 computes'
  write ( *, '(a)' ) '  exact solution #1 to the Burgers equation.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Viscosity NU = ', nu
  write ( *, '(a,i4)' ) '  NX = ', vxn
  write ( *, '(a,i4)' ) '  NT = ', vtn

  xlo = -1.0D+00
  xhi = +1.0D+00
  call r8vec_even ( vxn, xlo, xhi, vx )
  call r8vec_print ( vxn, vx, '  X grid points:' )

  tlo = 0.0D+00
  thi = 3.0D+00 / r8_pi
  call r8vec_even ( vtn, tlo, thi, vt )
  call r8vec_print ( vtn, vt, '  T grid points:' )

  call burgers_viscous_time_exact1 ( nu, vxn, vx, vtn, vt, vu )

  filename = 'burgers_solution_test02.txt'

  call r8mat_write ( filename, vxn, vtn, vu )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Data written to file "' // trim ( filename ) // '".'

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests sets up a small test case.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: vtn = 11
  integer ( kind = 4 ), parameter :: vxn = 11

  character ( len = 80 ) filename
  real ( kind = 8 ) nu
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) thi
  real ( kind = 8 ) tlo
  real ( kind = 8 ) vu(vxn,vtn)
  real ( kind = 8 ) vt(vtn)
  real ( kind = 8 ) vx(vxn)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo

  nu = 0.5

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  BURGERS_VISCOUS_TIME_EXACT2 computes'
  write ( *, '(a)' ) '  exact solution #2 to the Burgers equation.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Viscosity NU = ', nu
  write ( *, '(a,i4)' ) '  NX = ', vxn
  write ( *, '(a,i4)' ) '  NT = ', vtn

  xlo = 0.0D+00
  xhi = 2.0D+00 * r8_pi
  call r8vec_even ( vxn, xlo, xhi, vx )
  call r8vec_print ( vxn, vx, '  X grid points:' )

  tlo = 0.0D+00
  thi = 1.0D+00
  call r8vec_even ( vtn, tlo, thi, vt )
  call r8vec_print ( vtn, vt, '  T grid points:' )

  call burgers_viscous_time_exact2 ( nu, vxn, vx, vtn, vt, vu )

  call r8mat_print ( vxn, vtn, vu, '  U(X,T) at grid points:' )

  filename = 'burgers_solution_test03.txt'

  call r8mat_write ( filename, vxn, vtn, vu )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Data written to file "' // trim ( filename ) // '".'

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests sets up a finer test case.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: vtn = 41
  integer ( kind = 4 ), parameter :: vxn = 41

  character ( len = 80 ) filename
  real ( kind = 8 ) nu
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) thi
  real ( kind = 8 ) tlo
  real ( kind = 8 ) vu(vxn,vtn)
  real ( kind = 8 ) vt(vtn)
  real ( kind = 8 ) vx(vxn)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo

  nu = 0.5

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  BURGERS_VISCOUS_TIME_EXACT2 computes'
  write ( *, '(a)' ) '  exact solution #2 to the Burgers equation.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Viscosity NU = ', nu
  write ( *, '(a,i4)' ) '  NX = ', vxn
  write ( *, '(a,i4)' ) '  NT = ', vtn

  xlo = 0.0D+00
  xhi = 2.0D+00 * r8_pi
  call r8vec_even ( vxn, xlo, xhi, vx )
  call r8vec_print ( vxn, vx, '  X grid points:' )

  tlo = 0.0D+00
  thi = 1.0D+00
  call r8vec_even ( vtn, tlo, thi, vt )
  call r8vec_print ( vtn, vt, '  T grid points:' )

  call burgers_viscous_time_exact2 ( nu, vxn, vx, vtn, vt, vu )

  filename = 'burgers_solution_test04.txt'

  call r8mat_write ( filename, vxn, vtn, vu )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Data written to file "' // trim ( filename ) // '".'

  return
end
