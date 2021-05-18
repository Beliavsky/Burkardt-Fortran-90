program main

!*****************************************************************************80
!
!! MAIN is the main program for FD1D_PREDATOR_PREY.
!
!  Discussion:
!
!    This program sets up and solves a finite difference 1D predator
!    prey system.
!
!    The nondimensional problem has the form
!
!      du/dt =         del u + ( 1 - u ) * u        - v * h(u/alpha)
!
!      dv/dt = delta * del v     - gamma * v + beta * v * h(u/alpha)
!
!    with initial conditions:
!
!      u(x,0) = u0(x)
!      v(x,0) = v0(x)
!
!    and boundary conditions at the left and right endpoints of [A,B]:
!
!      du/dx = 0
!      dv/dx = 0
!
!    The Type II functional response employed here is
!
!      h(eta) = eta / ( 1 + eta )
!
!    The parameters ALPHA, BETA, GAMMA and DELTA are strictly positive.
!
!    The user must input a value H specifying the desired space step
!    to be used in discretizing the space dimension.
!
!    A finite difference scheme is employed to integrate the problem
!    from time 0 to a maximum time T.  The user must input the value
!    T, as well as an appropriate time step DELT.
!
!  Example:
!
!    A typical input for this problem is:
!
!      ALPHA =   0.3
!      BETA  =   2.0
!      GAMMA =   0.8
!      DELTA =   1.0
!      A     =   0.0
!      B     = 200.0
!      H     =   0.5
!      T     =  40.0
!      DELT  =   0.0104
!
!    with the following initial values of U and V supplied in
!    auxiliary subroutines:
!
!      u0(1:n) = exp ( - ( x(1:n) - 100.0 )^2 ) / 5.0
!      v0(1:n) = 2.0 / 5.0
!
!  Modified:
!
!    08 June 2013
!
!  Author:
!
!    Original MATLAB version by Marcus Garvie,
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Marcus Garvie,
!    Computational Algorithms for Spatially Extended Predator-Prey
!    Systems with a Holling Type II Functional Response,
!    To appear.
!
!  Parameters:
!
!    User input, real ( kind = 8 ) ALPHA, the value of the parameter ALPHA.
!
!    User input, real ( kind = 8 ) BETA, the value of the parameter BETA.
!
!    User input, real ( kind = 8 ) GAMMA, the value of the parameter GAMMA.
!
!    User input, real ( kind = 8 ) DELTA, the value of the parameter DELTA.
!
!    User input, real ( kind = 8 ) A, B, the left and right endpoints
!    of the interval.
!
!    User input, real ( kind = 8 ) H, the "space step", the desired
!    spacing between
!    nodes in [A,B].
!
!    User input, real ( kind = 8 ) T, the final time.  The problem is to
!    be integrated from 0 to T.
!
!    User input, real ( kind = 8 ) DELT, the time step to use.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: b1
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: b2
  real ( kind = 8 ) beta
  integer ( kind = 4 ) bigj
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) delt
  real ( kind = 8 ) delta
  real ( kind = 8 ), allocatable, dimension ( : ) :: f
  real ( kind = 8 ), allocatable, dimension ( : ) :: g
  real ( kind = 8 ) gamma
  integer ( kind = 4 ) gauss
  real ( kind = 8 ) h
  real ( kind = 8 ), allocatable, dimension ( : ) :: hhat
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ), parameter :: it_max = 25
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nt
  real ( kind = 8 ) t
  integer ( kind = 4 ) time_steps
  real ( kind = 8 ), allocatable, dimension ( : ) :: u
  real ( kind = 8 ), allocatable, dimension ( : ) :: u_rhs
  real ( kind = 8 ), allocatable, dimension ( : ) :: v
  real ( kind = 8 ), allocatable, dimension ( : ) :: v_rhs
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FD1D_PREDATOR_PREY'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  A one dimensional finite difference algorithm'
  write ( *, '(a)' ) '  for a predator-prey system.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Enter parameter alpha:'
  read ( *, * ) alpha
  write ( *, '(a)' ) '  Enter parameter beta:'
  read ( *, * ) beta
  write ( *, '(a)' ) '  Enter parameter gamma:'
  read ( *, * ) gamma
  write ( *, '(a)' ) '  Enter parameter delta:'
  read ( *, * ) delta

  write ( *, '(a)' ) '  Enter a in [a,b]:'
  read ( *, * ) a
  write ( *, '(a)' ) '  Enter b in [a,b]:'
  read ( *, * ) b

  write ( *, '(a)' ) '  Enter space-step h:'
  read ( *, * ) h
  write ( *, '(a)' ) '  Enter maximum time t:'
  read ( *, * ) t
  write ( *, '(a)' ) '  Enter time-step Delta t:'
  read ( *, * ) delt
  write ( *, '(a)' ) '  Enter 0 for direct Gauss solution, '
  write ( *, '(a)' ) '        1 for iterative Jacobi:'
  read ( *, * ) gauss

  if ( gauss /= 0 ) then
    gauss = 1
  end if
!
!  Calculate some constants.
!
  mu = delt / ( h * h )
  bigj = nint ( ( b - a ) / h )
!
!  N = number of degrees of freedom for each dependent variable.
!
  n = bigj + 1

  time_steps = nint ( t / delt )
!
!  Initialization
!
  allocate ( b1(3,1:n) )
  allocate ( b2(3,1:n) )
  allocate ( f(1:n) )
  allocate ( g(1:n) )
  allocate ( hhat(1:n) )
  allocate ( u(1:n) )
  allocate ( u_rhs(1:n) )
  allocate ( v(1:n) )
  allocate ( v_rhs(1:n) )
  allocate ( x(1:n) )
!
!  Set the coordinates of the nodes in [A,B].
!
  do i = 1, n
    x(i) = ( real ( n - i     , kind = 8 ) * a   &
           + real (     i - 1 , kind = 8 ) * b ) &
           / real ( n     - 1 , kind = 8 )

  end do
!
!  Call the user-supplied routines to get initial values for U and V.
!
  call u_init ( n, x, u )
  call v_init ( n, x, v )
!
!  Construct the matrix L, without the 1/h^2 factor.
!  For convenience, store it in B1.
!
  b1(1:3,1:n) = 0.0D+00
  b1(1,2) = -2.0D+00
  b1(3,n-1) = -2.0D+00
  b1(1,3:n) = -1.0D+00
  b1(2,1:n) = +2.0D+00
  b1(3,1:n-2) = -1.0D+00
!
!  Construct the matrices B1 & B2.
!
  b1(1:3,1:n) = mu * b1(1:3,1:n)
  b2(1:3,1:n) = delta * b1(1:3,1:n)

  b1(2,1:n) = b1(2,1:n) + 1.0D+00
  b2(2,1:n) = b2(2,1:n) + 1.0D+00
!
!  If we are using Gauss elimination, then LU factor the matrices B1 and B2.
!
  if ( gauss == 0 ) then

    call d3_np_fa ( n, b1, info )

    if ( info /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_PREDATOR_PREY - Fatal error!'
      write ( *, '(a)' ) '  Matrix B1 is singular.'
      stop
    end if

    call d3_np_fa ( n, b2, info )

    if ( info /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_PREDATOR_PREY - Fatal error!'
      write ( *, '(a)' ) '  Matrix B2 is singular.'
      stop
    end if

  end if
!
!  March forward in time.
!
  do nt = 1, time_steps
!
!  Evaluate the modified functional response.
!
    hhat(1:n) = u(1:n) / ( alpha + abs ( u(1:n) ) )
!
!  Update the right-hand-side of the linear system.
!
    f(1:n) = u(1:n) - u(1:n) * abs ( u(1:n) ) - v(1:n) * hhat(1:n)
    g(1:n) = beta * v(1:n) * hhat(1:n) - gamma * v(1:n)
    u_rhs(1:n) = u(1:n) + delt * f(1:n)
    v_rhs(1:n) = v(1:n) + delt * g(1:n)
!
!  Solve the linear systems for U and V.
!
    if ( gauss == 0 ) then

      call d3_np_sl ( n, b1, u_rhs, 0 )
      call d3_np_sl ( n, b2, v_rhs, 0 )

      u(1:n) = u_rhs(1:n)
      v(1:n) = v_rhs(1:n)

    else

      call d3_jac_sl ( n, b1, u_rhs, u, it_max, 0 )

      call d3_jac_sl ( n, b2, v_rhs, v, it_max, 0 )

    end if

  end do
!
!  Create a graphics data file.
!
  data_filename = 'uv_data.txt'
  call get_unit ( data_unit )

  open (  unit = data_unit, file = 'uv_data.txt', status = 'replace' )
  do i = 1, n
    write ( data_unit, '(3g14.6)' ) x(i), u(i), v(i)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created graphics data file "' &
    // trim ( data_filename ) // '".'
!
!  Create graphics command file.
!
  call get_unit ( command_unit )
  command_filename = 'uv_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "uv.png"'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "Population"'
  write ( command_unit, '(a)' ) 'set title "Predator and Prey Distributions"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "blue",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:3 lw 3 linecolor rgb "red"'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'
!
!  Free memory.
!
  deallocate ( b1 )
  deallocate ( b2 )
  deallocate ( f )
  deallocate ( g )
  deallocate ( hhat )
  deallocate ( u )
  deallocate ( u_rhs )
  deallocate ( v )
  deallocate ( v_rhs )
  deallocate ( x )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FD1D_PREDATOR_PREY:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine d3_jac_sl ( n, a, b, x, it_max, job )

!*****************************************************************************80
!
!! D3_JAC_SL solves a D3 system using Jacobi iteration.
!
!  Discussion:
!
!    The D3 storage format is used for a tridiagonal matrix.
!    The superdiagonal is stored in entries (1,2:N), the diagonal in
!    entries (2,1:N), and the subdiagonal in (3,1:N-1).  Thus, the
!    original matrix is "collapsed" vertically into the array.
!
!    This routine simply applies a given number of steps of the
!    iteration to an input approximate solution.  On first call, you can
!    simply pass in the zero vector as an approximate solution.  If
!    the returned value is not acceptable, you may call again, using
!    it as the starting point for additional iterations.
!
!  Example:
!
!    Here is how a D3 matrix of order 5 would be stored:
!
!       *  A12 A23 A34 A45
!      A11 A22 A33 A44 A55
!      A21 A32 A43 A54  *
!
!  Modified:
!
!    20 September 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be at least 2.
!
!    Input, real ( kind = 8 ) A(3,N), the D3 matrix.
!
!    Input, real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!    Input/output, real ( kind = 8 ) X(N), an approximate solution
!    to the system.
!
!    Input, integer ( kind = 4 ) IT_MAX, the maximum number of iterations.
!
!    Input, integer ( kind = 4 ) JOB, specifies the system to solve.
!    0, solve A * x = b.
!    nonzero, solve A' * x = b.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(3,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) job
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xnew(n)
!
!  No diagonal matrix entry can be zero.
!
  do i = 1, n
    if ( a(2,i) == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'D3_JAC_SL - Fatal error!'
      write ( *, '(a,i6)' ) '  Zero diagonal entry, index = ', i
      return
    end if
  end do

  if ( job == 0 ) then

    do it_num = 1, it_max

      xnew(1) =   b(1)                   - a(3,1) * x(2)
      do i = 2, n - 1
        xnew(i) = b(i) - a(1,i) * x(i-1) - a(3,i) * x(i+1)
      end do
      xnew(n) =   b(n) - a(1,n) * x(n-1)

      xnew(1:n) = xnew(1:n) / a(2,1:n)

      x(1:n) = xnew(1:n)

    end do

  else

    do it_num = 1, it_max

      xnew(1) =   b(1)                     - a(1,2) * x(2)
      do i = 2, n - 1
        xnew(i) = b(i) - a(3,i-1) * x(i-1) - a(1,i+1) * x(i+1)
      end do
      xnew(n) =   b(n) - a(3,n-1) * x(n-1)

      xnew(1:n) = xnew(1:n) / a(2,1:n)

      x(1:n) = xnew(1:n)

    end do

  end if

  return
end
subroutine d3_np_fa ( n, a, info )

!*****************************************************************************80
!
!! D3_NP_FA factors a D3 matrix without pivoting.
!
!  Discussion:
!
!    The D3 storage format is used for a tridiagonal matrix.
!    The superdiagonal is stored in entries (1,2:N), the diagonal in
!    entries (2,1:N), and the subdiagonal in (3,1:N-1).  Thus, the
!    original matrix is "collapsed" vertically into the array.
!
!    Because this routine does not use pivoting, it can fail even when
!    the matrix is not singular, and it is liable to make larger
!    errors.
!
!    D3_NP_FA and D3_NP_SL may be preferable to the corresponding
!    LINPACK routine DGTSL for tridiagonal systems, which factors and solves
!    in one step, and does not save the factorization.
!
!  Example:
!
!    Here is how a D3 matrix of order 5 would be stored:
!
!       *  A12 A23 A34 A45
!      A11 A22 A33 A44 A55
!      A21 A32 A43 A54  *
!
!  Modified:
!
!    02 November 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be at least 2.
!
!    Input/output, real ( kind = 8 ) A(3,N).
!    On input, the tridiagonal matrix.  On output, factorization information.
!
!    Output, integer ( kind = 4 ) INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(3,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info

  info = 0

  do i = 1, n-1

    if ( a(2,i) == 0.0D+00 ) then
      info = i
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'D3_NP_FA - Fatal error!'
      write ( *, '(a,i6)' ) '  Zero pivot on step ', info
      return
    end if
!
!  Store the multiplier in L.
!
    a(3,i) = a(3,i) / a(2,i)
!
!  Modify the diagonal entry in the next column.
!
    a(2,i+1) = a(2,i+1) - a(3,i) * a(1,i+1)

  end do

  if ( a(2,n) == 0.0D+00 ) then
    info = n
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'D3_NP_FA - Fatal error!'
    write ( *, '(a,i6)' ) '  Zero pivot on step ', info
    return
  end if

  return
end
subroutine d3_np_sl ( n, a_lu, b, job )

!*****************************************************************************80
!
!! D3_NP_SL solves a D3 system factored by D3_NP_FA.
!
!  Discussion:
!
!    The D3 storage format is used for a tridiagonal matrix.
!    The superdiagonal is stored in entries (1,2:N), the diagonal in
!    entries (2,1:N), and the subdiagonal in (3,1:N-1).  Thus, the
!    original matrix is "collapsed" vertically into the array.
!
!  Example:
!
!    Here is how a D3 matrix of order 5 would be stored:
!
!       *  A12 A23 A34 A45
!      A11 A22 A33 A44 A55
!      A21 A32 A43 A54  *
!
!  Modified:
!
!    02 November 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be at least 2.
!
!    Input, real ( kind = 8 ) A_LU(3,N), the LU factors from D3_NP_FA.
!
!    Input/output, real ( kind = 8 ) B(N).
!    On input, B contains the right hand side of the linear system.
!    On output, B contains the solution of the linear system.
!
!    Input, integer ( kind = 4 ) JOB, specifies the system to solve.
!    0, solve A * x = b.
!    nonzero, solve A' * x = b.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a_lu(3,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) job

  if ( job == 0 ) then
!
!  Solve L * Y = B.
!
    do i = 2, n
      b(i) = b(i) - a_lu(3,i-1) * b(i-1)
    end do
!
!  Solve U * X = Y.
!
    do i = n, 1, -1
      b(i) = b(i) / a_lu(2,i)
      if ( 1 < i ) then
        b(i-1) = b(i-1) - a_lu(1,i) * b(i)
      end if
    end do

  else
!
!  Solve U' * Y = B
!
    do i = 1, n
      b(i) = b(i) / a_lu(2,i)
      if ( i < n ) then
        b(i+1) = b(i+1) - a_lu(1,i+1) * b(i)
      end if
    end do
!
!  Solve L' * X = Y.
!
    do i = n-1, 1, -1
      b(i) = b(i) - a_lu(3,i) * b(i+1)
    end do

  end if

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen
 
  iunit = 0
 
  do i = 1, 99
 
    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )
 
      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if
 
  end do

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
