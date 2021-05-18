program main

!*****************************************************************************80
!
!! stiff_ode_test tests stiff_ode.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) lambda
  integer ( kind = 4 ) n
  real ( kind = 8 ) t0
  real ( kind = 8 ) tstop
  real ( kind = 8 ) y0

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'stiff_ode_test'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test stiff_ode using euler(), euler_backward(), midpoint().'

  call stiff_parameters ( lambda, t0, y0 )

  tstop = 1.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Parameter values:'
  write ( *, '(a,g14.6)' ) '    lambda = ', lambda
  write ( *, '(a,g14.6)' ) '    t0     = ', t0
  write ( *, '(a,g14.6)' ) '    y0     = ', y0

  n = 27

  call stiff_euler_test ( t0, tstop, n )
  call stiff_euler_backward_test ( t0, tstop, n )
  call stiff_midpoint_test ( t0, tstop, n )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'stiff_ode_test'
  write ( *, '(a)' ) '  Normal end of execution.'
  call timestamp ( )

  stop ( 0 )
end
subroutine stiff_euler_test ( t0, tstop, n )

!*****************************************************************************80
!
!! stiff_euler_test() tests stiff_euler().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) T0, TSTOP: the first and last times.
!
!    integer ( kind = 4 ) N: the number of steps to take.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: n2 = 101

  real ( kind = 8 ) t0
  real ( kind = 8 ) t1(n+1)
  real ( kind = 8 ) t2(n2)
  real ( kind = 8 ) tstop
  real ( kind = 8 ) y1(n+1)
  real ( kind = 8 ) y2(n2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'stiff_euler_test'
  write ( *, '(a)' ) '  Solve stiff ODE using the Euler method.'

  call stiff_euler ( n, t1, y1 )

  call r8vec_linspace ( n2, t0, tstop, t2 )

  call stiff_exact ( n2, t2, y2 )

  call plot2 ( n+1, t1, y1, n2, t2, y2, 'stiff_euler', &
    'Stiff ODE: euler method' )

  return
end
subroutine stiff_euler_backward_test ( t0, tstop, n )

!*****************************************************************************80
!
!! stiff_euler_backward_test() tests stiff_euler_backward().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) T0, TSTOP: the first and last times.
!
!    integer ( kind = 4 ) N: the number of steps to take.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: n2 = 101

  real ( kind = 8 ) t0
  real ( kind = 8 ) t1(n+1)
  real ( kind = 8 ) t2(n2)
  real ( kind = 8 ) tstop
  real ( kind = 8 ) y1(n+1)
  real ( kind = 8 ) y2(n2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'stiff_backward_euler_test'
  write ( *, '(a)' ) '  Solve stiff ODE using the backward Euler method.'

  call stiff_euler_backward ( n, t1, y1 )

  call r8vec_linspace ( n2, t0, tstop, t2 )
  call stiff_exact ( n2, t2, y2 )

  call plot2 ( n+1, t1, y1, n2, t2, y2, 'stiff_euler_backward', &
    'Stiff ODE: backward euler method' )

  return
end
subroutine stiff_midpoint_test ( t0, tstop, n )

!*****************************************************************************80
!
!! stiff_midpoint_test tests stiff_midpoint().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) T0, TSTOP: the first and last times.
!
!    integer ( kind = 4 ) N: the number of steps to take.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: n2 = 101

  real ( kind = 8 ) t0
  real ( kind = 8 ) t1(n+1)
  real ( kind = 8 ) t2(n2)
  real ( kind = 8 ) tstop
  real ( kind = 8 ) y1(n+1)
  real ( kind = 8 ) y2(n2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'stiff_midpoint_test'
  write ( *, '(a)' ) '  Solve stiff ODE using the midpoint method.'

  call stiff_midpoint ( n, t1, y1 )

  call r8vec_linspace ( n2, t0, tstop, t2 )
  call stiff_exact ( n2, t2, y2 )

  call plot2 ( n+1, t1, y1, n2, t2, y2, 'stiff_midpoint', &
    'Stiff ODE: midpoint method' )

  return
end
subroutine plot2 ( n1, t1, y1, n2, t2, y2, header, title )

!*****************************************************************************80
!
!! plot2() plots two curves together.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N1: the size of the first data set.
!
!    real ( kind = 8 ) T1(N1), Y1(N1), the first dataset.
!
!    integer ( kind = 4 ) N2: the size of the second data set.
!
!    real ( kind = 8 ) T2(N2), Y2(N2), the secod dataset.
!
!    character ( len = * ) HEADER: an identifier for the data.
!
!    character ( len = * ) TITLE: a title to appear in the plot.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data1_filename
  character ( len = 255 ) data2_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ) header
  integer ( kind = 4 ) i
  character ( len = * ) title
  real ( kind = 8 ) t1(n1)
  real ( kind = 8 ) t2(n2)
  real ( kind = 8 ) y1(n1)
  real ( kind = 8 ) y2(n2)
!
!  Create the data files.
!
  call get_unit ( data_unit )
  data1_filename = header // '_data1.txt'
  open ( unit = data_unit, file = data1_filename, status = 'replace' )
  do i = 1, n1
    write ( data_unit, '(5(2x,g14.6))' ) t1(i), y1(i)
  end do
  close ( unit = data_unit )

  call get_unit ( data_unit )
  data2_filename = header // '_data2.txt'
  open ( unit = data_unit, file = data2_filename, status = 'replace' )
  do i = 1, n2
    write ( data_unit, '(5(2x,g14.6))' ) t2(i), y2(i)
  end do
  close ( unit = data_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  plot2: data stored in "' &
    // trim ( data1_filename ) // '" and "' // trim ( data2_filename ) // '".'
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
  write ( command_unit, '(a)' ) 'set xlabel "<-- T -->"'
  write ( command_unit, '(a)' ) 'set ylabel "<-- Y(T) -->"'
  write ( command_unit, '(a)' ) 'set title "' // trim ( title ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data1_filename ) // &
    '" using 1:2 with lines lw 3 lt rgb "red",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data2_filename ) // &
    '" using 1:2 with lines lw 3 lt rgb "blue"'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  plot2: plot commands stored in "' &
    // trim ( command_filename ) // '".'

  return
end
 
