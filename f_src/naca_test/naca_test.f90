program main

!*****************************************************************************80
!
!! MAIN is the main program for NACA_TEST.
!
!  Discussion:
!
!    NACA_TEST tests the NACA library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NACA_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the NACA library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NACA_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests NACA4_SYMMETRIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 51

  real ( kind = 8 ) c
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  real ( kind = 8 ) r8vec_max
  real ( kind = 8 ) r8vec_min
  real ( kind = 8 ) ratio
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ) xy(2,2*n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  NACA4_SYMMETRIC evaluates y(x) for a NACA'
  write ( *, '(a)' ) '  symmetric airfoil defined by a 4-digit code.'

  c = 10.0D+00
  t = 0.15D+00
  call r8vec_linspace ( n, 0.0D+00, c, x )
  call naca4_symmetric ( t, c, n, x, y )
!
!  Reorganize data into a single object.
!
  xy(1,1:n) = x(1:n)
  xy(2,1:n) = -y(1:n)
  xy(1,n+1:2*n) = x(n:1:-1)
  xy(2,n+1:2*n) = y(n:1:-1)
!
!  Determine size ratio.
!
  x_min = r8vec_min ( n, x )
  x_max = r8vec_max ( n, x )
  y_max = r8vec_max ( n, y )
  y_min = - y_max
  ratio = ( y_max - y_min ) / ( x_max - x_min )
!
!  Save data to a file.
!
  data_filename = 'symmetric_data.txt'
  call r8mat_write ( data_filename, 2, 2 * n, xy )
  write ( *, '(a)' ) '  Data saved in file "' // trim ( data_filename ) // '"'
!
!  Create the command file.
!
  command_filename = 'symmetric_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a,g14.6)' ) 'set size ratio ', ratio
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set output "symmetric.png"'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
  write ( command_unit, '(a)' ) 'set title "NACA Symmetric Airfoil"'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) &
    // '" using 1:2 with lines lw 3'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Created command file "' &
    // trim ( command_filename ) // '".'

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests NACA4_CAMBERED.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real M, the maximum camber.
!    0 < M.
!
!    Input, real P, the location of maximum camber.
!    0.0 < P < 1.0.
!
!    Input, real T, the maximum relative thickness.
!    0.0 < T <= 1.0.
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 51

  real ( kind = 8 ) c
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  real ( kind = 8 ) m
  real ( kind = 8 ) p
  real ( kind = 8 ) r8vec_max
  real ( kind = 8 ) r8vec_min
  real ( kind = 8 ) ratio
  real ( kind = 8 ) t
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ) xc(n)
  real ( kind = 8 ) xl(n)
  real ( kind = 8 ) xu(n)
  real ( kind = 8 ) xy(2,2*n)
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min
  real ( kind = 8 ) yl(n)
  real ( kind = 8 ) yu(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  NACA4_CAMBERED evaluates (xu,yu) and (xl,yl) for a NACA'
  write ( *, '(a)' ) '  cambered airfoil defined by a 4-digit code.'

  m = 0.02D+00
  p = 0.4D+00
  t = 0.12D+00
  c = 10.0D+00

  call r8vec_linspace ( n, 0.0D+00, c, xc )
  call naca4_cambered ( m, p, t, c, n, xc, xu, yu, xl, yl )
!
!  Reorganize data into a single object.
!
  xy(1,1:n) = xl(1:n)
  xy(2,1:n) = yl(1:n)
  xy(1,n+1:2*n) = xu(n:1:-1)
  xy(2,n+1:2*n) = yu(n:1:-1)
!
!  Determine size ratio.
!
  x_min = min ( r8vec_min ( n, xl ), r8vec_min ( n, xu ) )
  x_max = max ( r8vec_max ( n, xl ), r8vec_max ( n, xu ) )
  y_min = min ( r8vec_min ( n, yl ), r8vec_min ( n, yu ) )
  y_max = max ( r8vec_max ( n, yl ), r8vec_max ( n, yu ) )
  ratio = ( y_max - y_min ) / ( x_max - x_min )
!
!  Save data to a file.
!
  data_filename = 'cambered_data.txt'
  call r8mat_write ( data_filename, 2, 2 * n, xy )
  write ( *, '(a)' ) '  Data saved in file "' // trim ( data_filename ) // '"'
!
!  Create the command file.
!
  command_filename = 'cambered_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a,g14.6)' ) 'set size ratio ', ratio
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set output "cambered.png"'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
  write ( command_unit, '(a)' ) 'set title "NACA Cambered Airfoil"'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) &
    // '" using 1:2 with lines lw 3'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Created command file "' &
    // trim ( command_filename ) // '".'

  return
end

