program main

!*****************************************************************************80
!
!! main is the main program for euler_test.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'euler_test'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test euler().'

  n = 50
  call euler_humps_test ( n )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'euler_test:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop ( 0 )
end
subroutine euler_humps_test ( n )

!*****************************************************************************80
!
!! euler_humps_test calls the Euler code.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N: the number of steps to take.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 1
  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ), parameter :: header = 'euler_humps'
  external humps_deriv
  integer ( kind = 4 ) i
  real ( kind = 8 ) t(n+1)
  real ( kind = 8 ) tspan(2)
  real ( kind = 8 ) y(n+1,1)
  real ( kind = 8 ) y0(m)
  real ( kind = 8 ) y2(n+1,1)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'euler_humps_test:'

  tspan(1) = 0.0D+00
  tspan(2) = 2.0D+00
  call humps_exact ( 1, tspan(1), y0 )

  call euler ( humps_deriv, tspan, y0, n, m, t, y )

  call humps_exact ( n+1, t, y2 )
!
!  Create the data file.
!
  call get_unit ( data_unit )

  data_filename = header // '_data.txt'

  open ( unit = data_unit, file = data_filename, status = 'replace' )

  do i = 1, n + 1
    write ( data_unit, '(3(2x,g14.6))' ) t(i), y(i,1), y2(i,1)
  end do

  close ( unit = data_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  euler_humps_test: data stored in "' &
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
  write ( command_unit, '(a)' ) 'set output "' // trim ( header ) // '_test.png"'
  write ( command_unit, '(a)' ) 'set xlabel "<-- T -->"'
  write ( command_unit, '(a)' ) 'set ylabel "<-- Y(T) -->"'
  write ( command_unit, '(a)' ) 'set title "euler: Humps ODE"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 with lines lw 3 lt rgb "red", \'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:3 with lines lw 3 lt rgb "blue"'

  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  euler_humps_test: plot commands stored in "' &
    // trim ( command_filename ) // '".'

  return
end
subroutine humps_deriv ( x, y, yp )

!*****************************************************************************80
!
!! humps_deriv evaluates the right hand side of the humps ODE.
!
!  Discussion:
!
!    y = 1.0 / ( ( x - 0.3 )^2 + 0.01 )
!      + 1.0 / ( ( x - 0.9 )^2 + 0.04 )
!      - 6.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) x, y: the argument.
!
!  Output:
!
!    real ( kind = 8 ) yp: the value of the derivative at x.
!
  implicit none

  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) yp

  call r8_fake_use ( y )

  yp = - 2.0D+00 * ( x - 0.3D+00 ) / ( ( x - 0.3D+00 )**2 + 0.01D+00 )**2 &
       - 2.0D+00 * ( x - 0.9D+00 ) / ( ( x - 0.9D+00 )**2 + 0.04D+00 )**2

  return
end
subroutine humps_exact ( n, x, y )

!*****************************************************************************80
!
!! humps_exact evaluates the humps function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) n: the number of evaluation points.
!
!    real ( kind = 8 ) x(n): the evaluation points.
!
!  Output:
!
!    real ( kind = 8 ) y(n): the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  write ( *, * ) 'x(1) = ', x(1)
  y = 1.0D+00 / ( ( x - 0.3D+00 )**2 + 0.01D+00 ) &
    + 1.0D+00 / ( ( x - 0.9D+00 )**2 + 0.04D+00 ) &
    - 6.0D+00

  return
end

