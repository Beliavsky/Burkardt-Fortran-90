program main

!*****************************************************************************80
!
!! MAIN is the main program for STRING_SIMULATION.
!
!  Discussion:
!
!    This program solves the 1D wave equation of the form:
!
!      Utt = c^2 Uxx
!
!    over the spatial interval (X1,X2) and time interval (T1,T2),
!    with initial conditions:
!
!      U(T1,X)  = U_T1(X),
!      Ut(T1,X) = UT_T1(X),
!
!    and boundary conditions of Dirichlet type:
!
!      U(T,X1) = U_X1(T),
!      U(T,X2) = U_X2(T).
!
!    The value C represents the propagation speed of waves.
!
!    The program uses the finite difference method, and marches
!    forward in time, solving for all the values of U at the next
!    time step by using the values known at the previous two time steps.
!
!    Central differences may be used to approximate both the time
!    and space derivatives in the original differential equation.
!
!    Thus, assuming we have available the approximated values of U
!    at the current and previous times, we may write a discretized
!    version of the wave equation as follows:
!
!      Uxx(T,X) = ( U(T,   X+dX) - 2 U(T,X) + U(T,   X-dX) ) / dX^2
!      Utt(T,X) = ( U(T+dt,X   ) - 2 U(T,X) + U(T-dt,X   ) ) / dT^2
!
!    If we multiply the first term by C^2 and solve for the single
!    unknown value U(T+dt,X), we have:
!
!      U(T+dT,X) =        (     C^2 * dT^2 / dX^2 ) * U(T,   X+dX)
!                  +  2 * ( 1 - C^2 * dT^2 / dX^2 ) * U(T,   X   )
!                  +      (     C^2 * dT^2 / dX^2 ) * U(T,   X-dX)
!                  -                                  U(T-dT,X   )
!
!    (Equation to advance from time T to time T+dT, except for FIRST step)
!
!    However, on the very first step, we only have the values of U
!    for the initial time, but not for the previous time step.
!    In that case, we use the initial condition information for dUdT
!    which can be approximated by a central difference that involves
!    U(T+dT,X) and U(T-dT,X):
!
!      dU/dT(T,X) = ( U(T+dT,X) - U(T-dT,X) ) / ( 2 * dT )
!
!    and so we can estimate U(T-dT,X) as
!
!      U(T-dT,X) = U(T+dT,X) - 2 * dT * dU/dT(T,X)
!
!    If we replace the "missing" value of U(T-dT,X) by the known values
!    on the right hand side, we now have U(T+dT,X) on both sides of the
!    equation, so we have to rearrange to get the formula we use
!    for just the first time step:
!
!      U(T+dT,X) =   1/2 * (     C^2 * dT^2 / dX^2 ) * U(T,   X+dX)
!                  +       ( 1 - C^2 * dT^2 / dX^2 ) * U(T,   X   )
!                  + 1/2 * (     C^2 * dT^2 / dX^2 ) * U(T,   X-dX)
!                  +  dT *                         dU/dT(T,   X   )
!
!    (Equation to advance from time T to time T+dT for FIRST step.)
!
!    It should be clear now that the quantity ALPHA = C * DT / DX will affect
!    the stability of the calculation.  If it is greater than 1, then
!    the middle coefficient 1-C^2 DT^2 / DX^2 is negative, and the
!    sum of the magnitudes of the three coefficients becomes unbounded.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) ALPHA, the CFL stability parameter.
!
!    Local, real ( kind = 8 ) C, the wave speed.
!
!    Local, real ( kind = 8 ) DT, the time step.
!
!    Local, real ( kind = 8 ) DX, the spatial step.
!
!    Local, integer ( kind = 4 ) M, the number of time steps.
!
!    Local, integer ( kind = 4 ) N, the number of spatial intervals.
!
!    Local, real ( kind = 8 ) T1, T2, the initial and final times.
!
!    Local, real ( kind = 8 ) U(M+1,N+1), the computed solution.
!
!    Local, real ( kind = 8 ) X1, X2, the left and right spatial endpoints.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 30
  integer ( kind = 4 ), parameter :: n = 40

  real ( kind = 8 ) alpha
  real ( kind = 8 ) :: c = 0.25D+00
  integer ( kind = 4 ) command_unit
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) dt
  real ( kind = 8 ) dx
  real ( kind = 8 ) f
  real ( kind = 8 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) t
  real ( kind = 8 ) :: t1 = 0.0D+00
  real ( kind = 8 ) :: t2 = 3.0D+00
  real ( kind = 8 ) u(0:m,0:n)
  real ( kind = 8 ) x
  real ( kind = 8 ) :: x1 = 0.0D+00
  real ( kind = 8 ) :: x2 = 1.0D+00

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'STRING_SIMULATION:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Simulate the behavior of a vibrating string.'

  dx = ( x2 - x1 ) / real ( n, kind = 8 )
  dt = ( t2 - t1 ) / real ( m, kind = 8 )
  alpha = ( c * dt / dx ) ** 2
  write ( *, '(a,g14.6)' ) '  ALPHA = ( C * dT / dX )^2 = ', alpha
!
!  Warn the user if ALPHA will cause an unstable computation.
!
  if ( 1.0D+00 < alpha ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Warning!'
    write ( *, '(a)' ) '  ALPHA is greater than 1.'
    write ( *, '(a)' ) '  The computation is unstable.'
  end if
!
!  Time step 0: 
!  Use the initial condition for U.
!
  u(0,0) = 0.0D+00
  do j = 1, n - 1
    x = real ( j, kind = 8 ) * dx
    u(0,j) = f ( x )
  end do
  u(0,n) = 0.0D+00
!
!  Time step 1:
!  Use the initial condition for dUdT.
!
  u(1,0) = 0.0D+00
  do j = 1, n - 1
    x = real ( j, kind = 8 ) * dx
    u(1,j) = &
        ( alpha / 2.0D+00 ) * u(0,j-1) &
      + ( 1.0D+00 - alpha ) * u(0,j)   &
      + ( alpha / 2.0D+00 ) * u(0,j+1) &
      + dt * g ( x )
  end do
  u(1,n) = 0.0D+00
!
!  Time steps 2 through M:
!
  do i = 2, m
    u(i,0) = 0.0D+00
    do j = 1, n - 1
      u(i,j) = &
                                alpha   * u(i-1,j-1) &
        + 2.0D+00 * ( 1.0D+00 - alpha ) * u(i-1,j)   &
        +                       alpha   * u(i-1,j+1) &
        -                                 u(i-2,j)
    end do
    u(i,n) = 0.0D+00
  end do
!
!  Write data file.
!
  call get_unit ( data_unit )
  open ( unit = data_unit, file = 'string_data.txt', status = 'replace' )

  do i = 0, m
    t = real ( i, kind = 8 ) * dt
    do j = 0, n
      x = real ( j, kind = 8 ) * dx
      write ( data_unit, '(f10.4,2x,f10.4,2x,f10.4)' ) x, t, u(i,j)
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Plot data written to the file "string_data.txt".'
!
!  Write gnuplot command file.
!
  call get_unit ( command_unit )
  open ( unit = command_unit, file = 'string_commands.txt', status = 'replace' )

  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "string.png"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---Time--->"'
  write ( command_unit, '(a,i4,a)' ) 'splot "string_data.txt" using 1:2:3 with lines'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Gnuplot command data written to the file "string_commands.txt".'
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'STRING_SIMULATION:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop
end
function f ( x )

!*****************************************************************************80
!
!! F supplies the initial condition.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the location.
!
!    Output, real ( kind = 8 ) F, the value of the solution at time 0 and location X.
!
  implicit none

  real ( kind = 8 ) f
  real ( kind = 8 ) x

  if ( 0.25D+00 <= x .and. x <= 0.50D+00 ) then
    f = ( x - 0.25D+00 ) * ( 0.50D+00 - x )
  else
    f = 0.0D+00
  end if

  return
end
function g ( x )

!*****************************************************************************80
!
!! G supplies the initial derivative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the location.
!
!    Output, real ( kind = 8 ) G, the value of the time derivative of the solution 
!    at time 0 and location X.
!
  implicit none

  real ( kind = 8 ) g
  real ( kind = 8 ) x

  g = 0.0D+00

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
!    06 August 2005
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

