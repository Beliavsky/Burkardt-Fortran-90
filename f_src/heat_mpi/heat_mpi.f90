program main

!*****************************************************************************80
!
!!  MAIN is the main program for HEAT_MPI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 April 2008
!
!  Author:
! 
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323,
!    LC: QA76.642.G76.
!
!    Marc Snir, Steve Otto, Steven Huss-Lederman, David Walker, 
!    Jack Dongarra,
!    MPI: The Complete Reference,
!    Volume I: The MPI Core,
!    Second Edition,
!    MIT Press, 1998,
!    ISBN: 0-262-69216-3,
!     LC: QA76.642.M65.
!
  use mpi

  integer ( kind = 4 ) id
  integer ( kind = 4 ) ierr
  integer ( kind = 4 ) p
  real ( kind = 8 ) wtime

  call MPI_Init ( ierr )

  call MPI_Comm_rank ( MPI_COMM_WORLD, id, ierr )

  call MPI_Comm_size ( MPI_COMM_WORLD, p, ierr )

  if ( id == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HEAT_MPI:'
    write ( *, '(a)' ) '  FORTRAN90/MPI version.'
    write ( *, '(a)' ) '  Solve the 1D time-dependent heat equation.'
  end if
!
!  Record the starting time.
!
  if ( id == 0 ) then
    wtime = MPI_Wtime ( )
  end if

  call update ( id, p )
!
!  Record the final time.
!
  if ( id == 0 ) then
    wtime = MPI_Wtime ( ) - wtime
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Wall clock elapsed seconds = ', wtime
  end if
!
!  Terminate MPI.
!
  call MPI_Finalize ( ierr )
!
!  Terminate.
!
  if ( id == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HEAT_MPI:'
    write ( *, '(a)' ) '  Normal end of execution.'
  end if

  stop
end
subroutine update ( id, p )

!*****************************************************************************80
!
!! UPDATE computes the solution of the heat equation.
!
!  Discussion:
!
!    If there is only one processor, then the program writes the
!    values of X and H to files.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 April 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ID, the id of this processor.
!
!    Input, integer ( kind = 4 ) P, the number of processors.
!
  use mpi

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ) boundary_condition
  real ( kind = 8 ) cfl
  real ( kind = 8 ) h(0:n+1)
  integer ( kind = 4 ) h_file
  real ( kind = 8 ) h_new(0:n+1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) id
  real ( kind = 8 ) initial_condition
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j_max
  integer ( kind = 4 ) j_min
  real ( kind = 8 ) k
  integer ( kind = 4 ) p
  real ( kind = 8 ) rhs
  integer ( kind = 4 ) status(MPI_STATUS_SIZE)
  integer ( kind = 4 ) tag
  real ( kind = 8 ) time
  real ( kind = 8 ) time_delta
  real ( kind = 8 ) time_max
  real ( kind = 8 ) time_min
  real ( kind = 8 ) time_new
  real ( kind = 8 ) x(0:n+1)
  real ( kind = 8 ) x_delta
  integer ( kind = 4 ) x_file
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min

  h_file = 11
  j_max = 400
  j_min = 0
  k = 0.002D+00
  x_file = 12
  time_max = 10.0D+00
  time_min = 0.0D+00
  x_max = 1.0D+00
  x_min = 0.0D+00
!
!  Have process 0 print out some information.
!
  if ( id == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Compute an approximate solution to the time dependent'
    write ( *, '(a)' ) '  one dimensional heat equation:'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    dH/dt - K * d2H/dx2 = f(x,t)'
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6,a,g14.6)' ) &
    '  for ', x_min, ' = x_min < x < x_max = ', x_max
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6,a,g14.6)' ) &
    '  and ', time_min, ' = time_min < t <= t_max = ', time_max
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Boundary conditions are specified at x_min and x_max.'
    write ( *, '(a)' ) '  Initial conditions are specified at time_min.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The finite difference method is used to discretize'
    write ( *, '(a)' ) '  the differential equation.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8,a)' ) '  This uses ', p * n, ' equally spaced points in X'
    write ( *, '(a,i8,a)' ) '  and ', j_max, ' equally spaced points in time.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8,a)' ) &
      '  Parallel execution is done using ', p, ' processors.'
    write ( *, '(a)' ) '  Domain decomposition is used.'
    write ( *, '(a,i8,a)' ) '  Each processor works on ', n, ' nodes,'
    write ( *, '(a)' ) &
      '  and shares some information with its immediate neighbors.'
  end if
!
!  Set the X coordinates of the N nodes.
!  We don't actually need ghost values of X but we'll throw them in
!  as X(0) and X(N+1).
!
  do i = 0, n + 1
    x(i) = ( dble (         id * n + i - 1 ) * x_max   &
           + dble ( p * n - id * n - i     ) * x_min ) &
           / dble ( p * n              - 1 )
  end do
!
!  In single processor mode, write out the X coordinates for display.
!
  if ( p == 1 ) then

    open ( unit = x_file, file = 'x_data.txt', status = 'unknown' )

    write ( x_file, '(11f14.6)' ) x(1:n)

    close ( unit = x_file )

  end if
!
!  Set the values of H at the initial time.
!
  time = time_min

  h(0) = 0.0D+00
  do i = 1, n
    h(i) = initial_condition ( x(i), time )
  end do
  h(n+1) = 0.0D+00
  
  time_delta = ( time_max - time_min ) / dble ( j_max - j_min )
  x_delta = ( x_max - x_min ) / dble ( p * n - 1 )
!
!  Check the CFL condition, have processor 0 print out its value,
!  and quit if it is too large.
!
  cfl = k * time_delta / x_delta / x_delta

  if ( id == 0 ) then 
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'UPDATE'
    write ( *, '(a,g14.6)' ) '  CFL stability criterion value = ', cfl 
  end if

  if ( 0.5D+00 <= cfl ) then
    if ( id == 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'UPDATE - Warning!'
      write ( *, '(a)' ) '  Computation cancelled!'
      write ( *, '(a)' ) '  CFL condition failed.'
      write ( *, '(a,g14.6)' ) '  0.5 <= K * dT / dX / dX = ', cfl
    end if
    return
  end if
!
!  In single processor mode, write out the values of H.
!
  if ( p == 1 ) then

    open ( unit = h_file, file = 'h_data.txt', status = 'unknown' )

    write ( h_file, '(11f14.6)' ) h(1:n)

  end if
!
!  Compute the values of H at the next time, based on current data.
!
  do j = 1, j_max

    time_new = ( dble (         j - j_min ) * time_max   &
               + dble ( j_max - j         ) * time_min ) &
               / dble ( j_max     - j_min )
!
!  Send H(1) to ID-1.
!
    if ( 0 < id ) then
      tag = 1
      call MPI_Send ( h(1), 1, MPI_DOUBLE_PRECISION, id-1, tag, &
        MPI_COMM_WORLD, ierr )
    end if
!
!  Receive H(N+1) from ID+1.
!
    if ( id < p - 1 ) then
      tag = 1
      call MPI_Recv ( h(n+1), 1,  MPI_DOUBLE_PRECISION, id+1, tag, &
        MPI_COMM_WORLD, status, ierr )
    end if
!
!  Send H(N) to ID+1.
!
    if ( id < p - 1 ) then
      tag = 2
      call MPI_Send ( h(n), 1, MPI_DOUBLE_PRECISION, id+1, tag, &
        MPI_COMM_WORLD, ierr )
    end if
!
!  Receive H(0) from ID-1.
!
    if ( 0 < id ) then
      tag = 2
      call MPI_Recv ( h(0), 1, MPI_DOUBLE_PRECISION, id-1, tag, &
        MPI_COMM_WORLD, status, ierr )
    end if
!
!  Update the temperature based on the four point stencil.
!
    do i = 1, n
      h_new(i) = h(i) &
        + ( time_delta * k / x_delta / x_delta ) &
        * ( h(i-1) - 2.0D+00 * h(i) + h(i+1) ) &
        + time_delta * rhs ( x(i), time )
    end do
!
!  H at the extreme left and right boundaries was incorrectly computed
!  using the differential equation.  Replace that calculation by
!  the boundary conditions.
!
    if ( 0 == id ) then
      h_new(1) = boundary_condition ( x(1), time_new )
    end if

    if ( id == p - 1 ) then
      h_new(n) = boundary_condition ( x(n), time_new )
    end if
!
!  Update time and temperature.
!
    time = time_new
    h(1:n) = h_new(1:n)
!
!  In single processor mode, add current solution data to output file.
!
    if ( p == 1 ) then
      write ( h_file, '(11f14.6)' ) h(1:n)
    end if

  end do

  if ( p == 1 ) then
    close ( unit = h_file )
  end if

  return
end
function boundary_condition ( x, time )

!*****************************************************************************80
!
!! BOUNDARY_CONDITION evaluates the boundary conditions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 April 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, TIME, the position and time.
!
!    Output, real ( kind = 8 ) BOUNDARY_CONDITION, the boundary condition.
!
  implicit none

  real ( kind = 8 ) boundary_condition
  real ( kind = 8 ) time
  real ( kind = 8 ) x
!
!  Left condition:
!
  if ( x < 0.5D+00 ) then
    boundary_condition = 100.0D+00 + 10.0D+00 * sin ( time )
  else
    boundary_condition = 75.0D+00
  end if

  return
end
function initial_condition ( x, time )

!*****************************************************************************80
!
!! INITIAL_CONDITION evaluates the initial conditions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 April 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, TIME, the position and time.
!
!    Output, real ( kind = 8 ) INITIAL_CONDITION, the initial condition.
!
  implicit none

  real ( kind = 8 ) initial_condition
  real ( kind = 8 ) time
  real ( kind = 8 ) x

  initial_condition = 95.0D+00

  return
end
function rhs ( x, time )

!*****************************************************************************80
!
!! RHS evaluates the right hand side of the differential equation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 April 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, TIME, the position and time.
!
!    Output, real ( kind = 8 ) RHS, the right hand side.
!
  real ( kind = 8 ) rhs
  real ( kind = 8 ) time
  real ( kind = 8 ) x

  rhs = 0.0D+00

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
