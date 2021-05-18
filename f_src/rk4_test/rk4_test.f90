program main

!*****************************************************************************80
!
!! MAIN is the main program for RK4_TEST.
!
!  Discussion:
!
!    RK4_TEST tests RK4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 April 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'RK4_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test RK4.'

  call rk4_predator_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'RK4_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine rk4_predator_test ( )

!*****************************************************************************80
!
!! rk4_predator_test tests RK4 on the predator prey ODE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 January 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ), parameter :: n = 1000

  external predator_deriv
  real ( kind = 8 ) t(n+1)
  real ( kind = 8 ) tspan(2)
  real ( kind = 8 ) y(n+1,m)
  real ( kind = 8 ) y0(m)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'rk4_predator_test'
  write ( *, '(a)' ) '  Use rk4() to solve the predator prey ODE.'

  tspan(1) = 0.0
  tspan(2) = 5.0
  y0(1) = 5000.0
  y0(2) = 100.0
  
  call rk4 ( predator_deriv, tspan, y0, n, m, t, y )

  call predator_phase_plot ( n, m, t, y )

  return
end
subroutine predator_deriv ( t, y, value )

!*****************************************************************************80
!
!! predator_deriv returns the right hand side of the predator ODE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) t, y(:): the arguments.
!
!  Output:
!
!    real ( kind = 8 ) value(:): the value of the derivative.
!
  implicit none

  real ( kind = 8 ) dfdt
  real ( kind = 8 ) drdt
  real ( kind = 8 ) f
  real ( kind = 8 ) r
  real ( kind = 8 ) t
  real ( kind = 8 ) value(2)
  real ( kind = 8 ) y(2)

  call r8_fake_use ( t )

  r = y(1)
  f = y(2)

  drdt =   2.0 * r - 0.001 * r * f
  dfdt = -10.0 * f + 0.002 * r * f

  value(1) = drdt
  value(2) = dfdt

  return
end
subroutine predator_phase_plot ( n, m, t, y )

!*****************************************************************************80
!
!! predator_phase_plot makes a phase plot of the results.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) n: the number of steps to take.
!
!    integer ( kind = 4 ) m: the number of variables.
!
!    real ( kind = 8 ) t(n+1), y(n+1,m): the times and solution values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = 8 ) :: header = 'predator'
  integer ( kind = 4 ) i
  real ( kind = 8 ) t(n+1)
  real ( kind = 8 ) y(n+1,m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'predator_phase_plot:'
  write ( *, '(a)' ) '  Write command and data files that can be used'
  write ( *, '(a)' ) '  by gnuplot for a predator-prey phase plot.'
!
!  Create the data file.
!
  call get_unit ( data_unit )

  data_filename = header // '_data.txt'

  open ( unit = data_unit, file = data_filename, status = 'replace' )

  do i = 1, n + 1
    write ( data_unit, '(5(2x,g14.6))' ) t(i), y(i,1), y(i,2)
  end do

  close ( unit = data_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  predator_phase_plot: data stored in "' &
    // trim ( data_filename ) // '".'
!
!  Create the command file.
!
  call get_unit ( command_unit )

  command_filename = trim ( header ) // '_commands.txt'

  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( header ) // '.png"'
  write ( command_unit, '(a)' ) 'set xlabel "<-- Predator -->"'
  write ( command_unit, '(a)' ) 'set ylabel "<-- Prey -->"'
  write ( command_unit, '(a)' ) 'set title "Predator-prey solved by rk4()"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 2:3 with lines'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  predator_phase_plot: plot commands stored in "' &
    // trim ( command_filename ) // '".'

  return
end

