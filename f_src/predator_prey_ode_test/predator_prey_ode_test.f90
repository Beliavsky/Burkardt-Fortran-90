program main

!*****************************************************************************80
!
!! predator_prey_ode_test tests predator_prey_ode.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 October 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  real ( kind = 8 ) delta
  real ( kind = 8 ) gamma
  integer ( kind = 4 ) n
  real ( kind = 8 ) p0(2)
  real ( kind = 8 ) tspan(2)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'predator_prey_ode_test:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test predator_prey_ode using euler, midpoint.'

  call predator_prey_parameters ( alpha, beta, gamma, delta )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  parameter values:'
  write ( *, '(a,g14.6)' ) '    alpha = ', alpha
  write ( *, '(a,g14.6)' ) '    beta  = ', beta
  write ( *, '(a,g14.6)' ) '    gamma = ', gamma
  write ( *, '(a,g14.6)' ) '    delta = ', delta

  tspan(1) = 0.0D+00
  tspan(2) = 5.0D+00
  p0(1) = 5000.0D+00
  p0(2) = 100.0D+00
  n = 200

  call predator_prey_euler ( tspan, p0, n )
  call predator_prey_midpoint ( tspan, p0, n )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'predator_prey_ode_test:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop ( 0 )
end
subroutine euler ( dydt, tspan, y0, n, m, t, y )

!*****************************************************************************80
!
!! euler approximates the solution to an ODE using Euler's method.
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
!    external dydt: a subroutine that evaluates the right
!    hand side of the ODE.
!
!    real ( kind = 8 ) tspan(2): contains the initial and final times.
!
!    real ( kind = 8 ) y0(m): a column vector containing the initial condition.
!
!    integer ( kind = 4 ) n: the number of steps to take.
!
!    integer ( kind = 4 ) m: the number of variables.
!
!  Output:
!
!    real ( kind = 8 ) t(n+1), y(n+1,m): the times and solution values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) dt
  real ( kind = 8 ) dy(m)
  external dydt
  integer ( kind = 4 ) i
  real ( kind = 8 ) t(n+1)
  real ( kind = 8 ) tfirst
  real ( kind = 8 ) tlast
  real ( kind = 8 ) tspan(2)
  real ( kind = 8 ) y(n+1,m)
  real ( kind = 8 ) y0(m)

  tfirst = tspan(1)
  tlast = tspan(2)
  dt = ( tlast - tfirst ) / real ( n, kind = 8 )
  t(1) = tspan(1)
  y(1,1:m) = y0(1:m)

  do i = 1, n
    t(i+1) = t(i) + dt
    call dydt ( t(i), y(i,1:m), dy )
    y(i+1,1:m) = y(i,1:m) + dt * dy(1:m)
  end do

  return
end
subroutine midpoint_fixed ( dydt, tspan, y0, n, m, t, y )

!*****************************************************************************80
!
!! midpoint_fixed uses a fixed-point midpoint method to solve an ODE.
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
!  Input:
!
!    external dydt: a subroutine that evaluates the right
!    hand side of the ODE.
!
!    real ( kind = 8 ) tspan(2): contains the initial and final times.
!
!    real ( kind = 8 ) y0(m): a column vector containing the initial condition.
!
!    integer ( kind = 4 ) n: the number of steps to take.
!
!    integer ( kind = 4 ) m: the number of variables.
!
!  Output:
!
!    real ( kind = 8 ) t(n+1), y(n+1,m): the times and solution values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) dt
  external dydt
  real ( kind = 8 ) f(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) j
  real ( kind = 8 ) t(n+1)
  real ( kind = 8 ) theta
  real ( kind = 8 ) tm
  real ( kind = 8 ) tspan(2)
  real ( kind = 8 ) y(n+1,m)
  real ( kind = 8 ) y0(m)
  real ( kind = 8 ) ym(m)

  dt = ( tspan(2) - tspan(1) ) / n

  it_max = 10
  theta = 0.5D+00

  t(1) = tspan(1)
  y(1,1:m) = y0(1:m)

  do i = 1, n
    tm = t(i) + theta * dt 
    ym(1:m) = y(i,1:m)
    do j = 1, it_max
      call dydt ( tm, ym(1:m), f )
      ym(1:m) = y(i,1:m) + theta * dt * f(1:m)
    end do
    t(i+1) = t(i) + dt
    y(i+1,1:m) = (           1.0D+00 / theta ) * ym(1:m) &
               + ( 1.0D+00 - 1.0D+00 / theta ) * y(i,1:m)
  end do

  return
end
subroutine predator_prey_euler ( tspan, p0, n )

!*****************************************************************************80
!
!! predator_prey_euler solves the predator-prey system using euler().
!
!  Discussion:
!
!    The physical system under consideration is a pair of animal populations.
!
!    The PREY reproduce rapidly for each animal alive at the beginning of the
!    year, two more will be born by the end of the year.  The prey do not have
!    a natural death rate instead, they only die by being eaten by the predator.
!    Every prey animal has 1 chance in 1000 of being eaten in a given year by
!    a given predator.
!
!    The PREDATORS only die of starvation, but this happens very quickly.
!    If unfed, a predator will tend to starve in about 1/10 of a year.
!    On the other hand, the predator reproduction rate is dependent on
!    eating prey, and the chances of this depend on the number of available prey.
!
!    The resulting differential equations can be written:
!
!      PREY(0) = 5000         
!      PRED(0) =  100
!
!      d PREY / dT =    2 * PREY(T) - 0.001 * PREY(T) * PRED(T)
!      d PRED / dT = - 10 * PRED(T) + 0.002 * PREY(T) * PRED(T)
!
!    Here, the initial values (5000,100) are a somewhat arbitrary starting point.
!
!    The pair of ordinary differential equations that result have an interesting
!    behavior.  For certain choices of the interaction coefficients (such as
!    those given here), the populations of predator and prey will tend to 
!    a periodic oscillation.  The two populations will be out of phase the number
!    of prey will rise, then after a delay, the predators will rise as the prey
!    begins to fall, causing the predator population to crash again.
!
!    There is a conserved quantity, which here would be:
!      E(r,f) = 0.002 r + 0.001 f - 10 ln(r) - 2 ln(f)
! 
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2020
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    George Lindfield, John Penny,
!    Numerical Methods Using MATLAB,
!    Second Edition,
!    Prentice Hall, 1999,
!    ISBN: 0-13-012641-1,
!    LC: QA297.P45.
!
!  Input:
!
!    real ( kind = 8 ) TSPAN(2) = [ T0, TMAX ], the initial and final times.
!    A reasonable value might be [ 0, 5 ].
!
!    real ( kind = 8 ) P0(2) = [ PREY, PRED ], the initial number of prey and predators.
!    A reasonable value might be [ 5000, 100 ].
!
!    integer ( kind = 4 ) N: the number of time steps.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ), parameter :: header = 'predator_prey_euler'
  integer ( kind = 4 ) i
  real ( kind = 8 ) p0(m)
  real ( kind = 8 ) pout(n+1,m)
  external predator_prey_deriv
  real ( kind = 8 ) t(n+1)
  real ( kind = 8 ) tspan(2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'predator_prey_euler'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A pair of ordinary differential equations for a population'
  write ( *, '(a)' ) '  of predators and prey are solved using euler().'

  call euler ( predator_prey_deriv, tspan, p0, n, m, t, pout )
!
!  Create the data file.
!
  call get_unit ( data_unit )

  data_filename = header // '_data.txt'

  open ( unit = data_unit, file = data_filename, status = 'replace' )

  do i = 1, n + 1
    write ( data_unit, '(5(2x,g14.6))' ) t(i), pout(i,1), pout(i,2)
  end do

  close ( unit = data_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  predator_prey_euler: data stored in "' &
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
  write ( command_unit, '(a)' ) 'set xlabel "<-- Prey -->"'
  write ( command_unit, '(a)' ) 'set ylabel "<-- Predator -->"'
  write ( command_unit, '(a)' ) 'set title "euler: predator prey ODE"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 2:3 with lines lw 3'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  predator_prey_euler: plot commands stored in "' &
    // trim ( command_filename ) // '".'

  return
end
subroutine predator_prey_midpoint ( tspan, p0, n )

!*****************************************************************************80
!
!! predator_prey_midpoint solves the predator-prey system using midpoint_fixed().
!
!  Discussion:
!
!    The physical system under consideration is a pair of animal populations.
!
!    The PREY reproduce rapidly for each animal alive at the beginning of the
!    year, two more will be born by the end of the year.  The prey do not have
!    a natural death rate instead, they only die by being eaten by the predator.
!    Every prey animal has 1 chance in 1000 of being eaten in a given year by
!    a given predator.
!
!    The PREDATORS only die of starvation, but this happens very quickly.
!    If unfed, a predator will tend to starve in about 1/10 of a year.
!    On the other hand, the predator reproduction rate is dependent on
!    eating prey, and the chances of this depend on the number of available prey.
!
!    The resulting differential equations can be written:
!
!      PREY(0) = 5000         
!      PRED(0) =  100
!
!      d PREY / dT =    2 * PREY(T) - 0.001 * PREY(T) * PRED(T)
!      d PRED / dT = - 10 * PRED(T) + 0.002 * PREY(T) * PRED(T)
!
!    Here, the initial values (5000,100) are a somewhat arbitrary starting point.
!
!    The pair of ordinary differential equations that result have an interesting
!    behavior.  For certain choices of the interaction coefficients (such as
!    those given here), the populations of predator and prey will tend to 
!    a periodic oscillation.  The two populations will be out of phase the number
!    of prey will rise, then after a delay, the predators will rise as the prey
!    begins to fall, causing the predator population to crash again.
!
!    There is a conserved quantity, which here would be:
!      E(r,f) = 0.002 r + 0.001 f - 10 ln(r) - 2 ln(f)
! 
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2020
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    George Lindfield, John Penny,
!    Numerical Methods Using MATLAB,
!    Second Edition,
!    Prentice Hall, 1999,
!    ISBN: 0-13-012641-1,
!    LC: QA297.P45.
!
!  Input:
!
!    real ( kind = 8 ) TSPAN = [ T0, TMAX ], the initial and final times.
!    A reasonable value might be [ 0, 5 ].
!
!    real ( kind = 8 ) P0 = [ PREY, PRED ], the initial number of prey and predators.
!    A reasonable value might be [ 5000, 100 ].
!
!    integer ( kind = 4 ) N: the number of time steps.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ), parameter :: header = 'predator_prey_midpoint'
  integer ( kind = 4 ) i
  real ( kind = 8 ) p0(m)
  real ( kind = 8 ) pout(n+1,m)
  external predator_prey_deriv
  real ( kind = 8 ) t(n+1)
  real ( kind = 8 ) tspan(2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'predator_prey_midpoint '
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A pair of ordinary differential equations for a population'
  write ( *, '(a)' ) '  of predators and prey are solved using midpoint_fixed().'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The exact solution shows periodic behavior, with a fixed'
  write ( *, '(a)' ) '  period and amplitude.'

  call midpoint_fixed ( predator_prey_deriv, tspan, p0, n, m, t, pout )
!
!  Create the data file.
!
  call get_unit ( data_unit )

  data_filename = header // '_data.txt'

  open ( unit = data_unit, file = data_filename, status = 'replace' )

  do i = 1, n + 1
    write ( data_unit, '(5(2x,g14.6))' ) t(i), pout(i,1), pout(i,2)
  end do

  close ( unit = data_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  predator_prey_midpoint: data stored in "' &
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
  write ( command_unit, '(a)' ) 'set xlabel "<-- Prey -->"'
  write ( command_unit, '(a)' ) 'set ylabel "<-- Predator -->"'
  write ( command_unit, '(a)' ) 'set title "midpoint: predator prey ODE"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 2:3 with lines lw 3'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  predator_prey_midpoint: plot commands stored in "' &
    // trim ( command_filename ) // '".'

  return
end


