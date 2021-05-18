program main

!*****************************************************************************80
!
!! FD1D_ADVECTION_FTCS solves the advection equation using the FTCS method.
!
!  Discussion:
!
!    The FTCS method is unstable for the advection problem.
!
!    Given a smooth initial condition, successive FTCS approximations will
!    exhibit erroneous oscillations of increasing magnitude.
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
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) dt
  real ( kind = 8 ) dx
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_wrap
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jm1
  integer ( kind = 4 ) jp1
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) nt
  integer ( kind = 4 ) nt_step
  real ( kind = 8 ) t
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ), allocatable :: unew(:)
  real ( kind = 8 ), allocatable :: x(:)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FD1D_ADVECTION_FTCS:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Solve the constant-velocity advection equation in 1D,'
  write ( *, '(a)' ) '    du/dt = - c du/dx'
  write ( *, '(a)' ) '  over the interval:'
  write ( *, '(a)' ) '    0.0 <= x <= 1.0'
  write ( *, '(a)' ) '  with periodic boundary conditions, and'
  write ( *, '(a)' ) '  with a given initial condition'
  write ( *, '(a)' ) '    u(0,x) = (10x-4)^2 (6-10x)^2 for 0.4 <= x <= 0.6'
  write ( *, '(a)' ) '           = 0 elsewhere.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  We use a method known as FTCS:'
  write ( *, '(a)' ) '   FT: Forward Time  : du/dt = (u(t+dt,x)-u(t,x))/dt'
  write ( *, '(a)' ) '   CS: Centered Space: du/dx = (u(t,x+dx)-u(t,x-dx))/2/dx'

  nx = 101
  dx = 1.0D+00 / real ( nx - 1, kind = 8 )
  allocate ( x(1:nx) )
  a = 0.0D+00
  b = 1.0D+00
  call r8vec_linspace ( nx, a, b, x )
  nt = 1000
  dt = 1.0D+00 / real ( nt, kind = 8 )
  c = 1.0D+00

  allocate ( u(1:nx) )
  call initial_condition ( nx, x, u )
!
!  Open data file, and write solutions as they are computed.
!
  call get_unit ( data_unit )
  data_filename = 'fd1d_advection_ftcs_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )

  t = 0.0D+00
!
!  As far as I can tell, GNUPLOT does not offer a way to show the data as
!  a sequence of functions u(*,t), that is, as "snapshots" of a continuous
!  curve at successive discrete times.  Instead, GNUPLOT wants to make 
!  a surface.
!  GNUPLOT's graphics are too poor for such a surface to be intelligible.
!  However, while GNUPLOT does not offer any command that will suppress the
!  connecting lines for same x, successive t, you can trick it by writing
!  the curves to the data file, but making one curve have a different number
!  of data points in it!  Hence, (this is a classic example of a KLUDGE)
!  we write the first point of the first curve twice, and presto! our graph
!  is uncluttered enough that we can figure out what is going on from time
!  step to time step.  It's wonderful that GNUPLOT is free and portable,
!  and it's sad that its graphics are so limited and inexpressive.  Also,
!  any manual that consists almost entirely of discussing the SET command
!  is an obvious indicator that the interface needs to be reconsidered.
!
  write ( data_unit, '(f10.4,2x,f10.4,2x,f10.4)' ) x(1), t, u(1)
  do j = 1, nx
    write ( data_unit, '(f10.4,2x,f10.4,2x,f10.4)' ) x(j), t, u(j)
  end do
  write ( data_unit, '(a)' ) ''

  nt_step = 100

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of nodes NX = ', nx
  write ( *, '(a,i6)' ) '  Number of time steps NT = ', nt
  write ( *, '(a,g14.6)' ) '  Constant velocity C = ', c

  allocate ( unew(1:nx) )

  do i = 1, nt

    do j = 1, nx
      jm1 = i4_wrap ( j - 1, 1, nx )
      jp1 = i4_wrap ( j + 1, 1, nx )
      unew(j) = u(j) - c * dt / dx / 2.0D+00 * ( u(jp1) - u(jm1) )
    end do

    u(1:nx) = unew(1:nx)

    if ( i == nt_step ) then
      t = real ( i, kind = 8 ) * dt
      do j = 1, nx
        write ( data_unit, '(f10.4,2x,f10.4,2x,f10.4)' ) x(j), t, u(j)
      end do
      write ( data_unit, '(a)' ) ''
      nt_step = nt_step + 100
    end if

  end do
!
!  Close the data file once the computation is done.
!
  close ( unit = data_unit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '  Plot data written to the file "' // trim ( data_filename ) // '".'
!
!  Write gnuplot command file.
!
  call get_unit ( command_unit )
  command_filename = 'fd1d_advection_ftcs_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "fd1d_advection_ftcs.png"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---Time--->"'
  write ( command_unit, '(a,i4,a)' ) 'splot "' // trim ( data_filename ) &
    // '" using 1:2:3 with lines'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Gnuplot command data written to the file "' &
    // trim ( command_filename ) // '".'
!
!  Free memory.
!
  deallocate ( u )
  deallocate ( unew )
  deallocate ( x )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FD1D_ADVECTION_FTCS'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop
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
function i4_modp ( i, j )

!*****************************************************************************80
!
!! I4_MODP returns the nonnegative remainder of I4 division.
!
!  Discussion:
!
!    If
!      NREM = I4_MODP ( I, J )
!      NMULT = ( I - NREM ) / J
!    then
!      I = J * NMULT + NREM
!    where NREM is always nonnegative.
!
!    The MOD function computes a result with the same sign as the
!    quantity being divided.  Thus, suppose you had an angle A,
!    and you wanted to ensure that it was between 0 and 360.
!    Then mod(A,360) would do, if A was positive, but if A
!    was negative, your result would be between -360 and 0.
!
!    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
!
!    An I4 is an integer ( kind = 4 ) value.
!
!  Example:
!
!        I     J     MOD I4_MODP    Factorization
!
!      107    50       7       7    107 =  2 *  50 + 7
!      107   -50       7       7    107 = -2 * -50 + 7
!     -107    50      -7      43   -107 = -3 *  50 + 43
!     -107   -50      -7      43   -107 =  3 * -50 + 43
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the number to be divided.
!
!    Input, integer ( kind = 4 ) J, the number that divides I.
!
!    Output, integer ( kind = 4 ) I4_MODP, the nonnegative remainder when I is
!    divided by J.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) j
  integer ( kind = 4 ) value

  if ( j == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4_MODP - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal divisor J = ', j
    stop
  end if

  value = mod ( i, j )

  if ( value < 0 ) then
    value = value + abs ( j )
  end if

  i4_modp = value

  return
end
function i4_wrap ( ival, ilo, ihi )

!*****************************************************************************80
!
!! I4_WRAP forces an I4 to lie between given limits by wrapping.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ) value.
!
!    There appears to be a bug in the GFORTRAN compiler which can lead to
!    erroneous results when the first argument of I4_WRAP is an expression.
!    In particular:
!
!    do i = 1, 3
!      if ( test ) then
!        i4 = i4_wrap ( i + 1, 1, 3 )
!      end if
!    end do
!
!    was, when I = 3, returning I4 = 3.  So I had to replace this with
!
!    do i = 1, 3
!      if ( test ) then
!        i4 = i + 1
!        i4 = i4_wrap ( i4, 1, 3 )
!      end if
!    end do
!
!  Example:
!
!    ILO = 4, IHI = 8
!
!    I  Value
!
!    -2     8
!    -1     4
!     0     5
!     1     6
!     2     7
!     3     8
!     4     4
!     5     5
!     6     6
!     7     7
!     8     8
!     9     4
!    10     5
!    11     6
!    12     7
!    13     8
!    14     4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IVAL, a value.
!
!    Input, integer ( kind = 4 ) ILO, IHI, the desired bounds.
!
!    Output, integer ( kind = 4 ) I4_WRAP, a "wrapped" version of the value.
!
  implicit none

  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) i4_wrap
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) value
  integer ( kind = 4 ) wide

  jlo = min ( ilo, ihi )
  jhi = max ( ilo, ihi )

  wide = jhi - jlo + 1

  if ( wide == 1 ) then
    value = jlo
  else
    value = jlo + i4_modp ( ival - jlo, wide )
  end if

  i4_wrap = value

  return
end
subroutine initial_condition ( nx, x, u )

!*****************************************************************************80
!
!! INITIAL_CONDITION sets the initial condition.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, the number of nodes.
!
!    Input, real ( kind = 8 ) X(NX), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) U(NX), the value of the initial condition.
!
  implicit none

  integer ( kind = 4 ) nx

  integer ( kind = 4 ) i
  real ( kind = 8 ) u(nx)
  real ( kind = 8 ) x(nx)

  do i = 1, nx

    if  ( 0.4D+00 <= x(i) .and. x(i) <= 0.6D+00 ) then
      u(i) = ( 10.0D+00 * x(i) - 4.0D+00 ) ** 2 &
           * ( 6.0D+00 - 10.0D+00 * x(i) ) ** 2
    else
      u(i) = 0.0D+00
    end if

  end do

  return
end
subroutine r8vec_linspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_LINSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
!
!    In other words, the interval is divided into N-1 even subintervals,
!    and the endpoints of intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A_FIRST, A_LAST, the first and last entries.
!
!    Output, real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n
      x(i) = ( real ( n - i,     kind = 8 ) * a   &
             + real (     i - 1, kind = 8 ) * b ) &
             / real ( n     - 1, kind = 8 )
    end do

  end if

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
