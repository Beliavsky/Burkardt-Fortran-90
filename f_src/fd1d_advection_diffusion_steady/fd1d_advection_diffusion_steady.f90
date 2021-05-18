program main

!*****************************************************************************80
!
!! FD1D_ADVECTION_DIFFUSION_STEADY solves steady advection diffusion equation.
!
!  Discussion:
!
!    The steady advection diffusion equation has the form:
!
!      v ux - k * uxx = 0
!
!    where V (the advection velocity) and K (the diffusivity) are positive 
!    constants, posed in the region
!
!      a = 0 < x < 1 = b
!
!    with boundary conditions
!
!      u(0) = 0, u(1) = 1.
!
!    The discrete solution is unreliable when dx > 2 * k / v / ( b - a ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), allocatable :: a3(:,:)
  real ( kind = 8 ) b
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) dx
  real ( kind = 8 ), allocatable :: f(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) k
  integer ( kind = 4 ) nx
  real ( kind = 8 ) r
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ) v
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FD1D_ADVECTION_DIFFUSION_STEADY:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Solve the 1D steady advection diffusion equation:,'
  write ( *, '(a)' ) '    v du/dx - k d2u/dx2 = 0'
  write ( *, '(a)' ) '  with constant, positive velocity V and diffusivity K'
  write ( *, '(a)' ) '  over the interval:'
  write ( *, '(a)' ) '    0.0 <= x <= 1.0'
  write ( *, '(a)' ) '  with boundary conditions:'
  write ( *, '(a)' ) '    u(0) = 0, u(1) = 1.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Use finite differences'
  write ( *, '(a)' ) '   d u/dx  = (u(t,x+dx)-u(t,x-dx))/2/dx'
  write ( *, '(a)' ) '   d2u/dx2 = (u(x+dx)-2u(x)+u(x-dx))/dx^2'
!
!  Physical constants.
!
  v = 1.0D+00
  k = 0.05D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Diffusivity K = ', k
  write ( *, '(a,g14.6)' ) '  Velocity V    = ', v
!
!  Spatial discretization.
!
  nx = 101
  a = 0.0D+00
  b = 1.0D+00
  dx = ( b - a ) / real ( nx - 1, kind = 8 )
  allocate ( x(1:nx) )
  call r8vec_linspace ( nx, a, b, x )

  write ( *, '(a,i4)' ) '  Number of nodes NX = ', nx
  write ( *, '(a,g14.6)' ) '  DX = ', dx
  write ( *, '(a,g14.6)' ) '  Maximum safe DX is ', 2.0D+00 * k / v / ( b - a )
!
!  Set up the tridiagonal linear system corresponding to the boundary 
!  conditions and advection-diffusion equation.
!
  allocate ( a3(1:nx,1:3) )
  a3(1:nx,1:3) = 0.0D+00

  allocate ( f(1:nx) )
  f(1:nx) = 0.0D+00

  a3(1,2) = 1.0D+00
  f(1) = 0.0D+00

  do i = 2, nx - 1
    a3(i,1) = - v / dx / 2.0D+00 -           k / dx / dx
    a3(i,2) =                    + 2.0D+00 * k / dx / dx
    a3(i,3) = + v / dx / 2.0D+00 -           k / dx / dx
    f(i) = 0.0D+00
  end do

  a3(nx,2) = 1.0D+00
  f(nx) = 1.0D+00

  allocate ( u(1:nx) )

  call trisolve ( nx, a3, f, u )
!
!  The exact solution to the differential equation is known.
!
  r = v * ( b - a ) / k

  allocate ( w(1:nx) )
  w(1:nx) = ( 1.0D+00 - exp ( r * x(1:nx) ) ) / ( 1.0D+00 - exp ( r ) )
!
!  Write data file.
!
  call get_unit ( data_unit )
  data_filename = 'fd1d_advection_diffusion_steady_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do j = 1, nx
    write ( data_unit, '(f10.4,2x,f10.4,2x,f10.4)' ) x(j), u(j), w(j)
  end do
  close ( unit = data_unit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '  Gnuplot data written to file "' // trim ( data_filename ) // '".'
!
!  Write command file.
!
  call get_unit ( command_unit )
  command_filename = 'fd1d_advection_diffusion_steady_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "fd1d_advection_diffusion_steady.png"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---U(X)--->"'
  write ( command_unit, '(a)' ) 'set title "Exact: green line, Approx: red dots"'
  write ( command_unit, '(a,i4,a)' ) 'plot "' // trim ( data_filename ) &
    // '" using 1:2 with points pt 7 ps 2, \'
  write ( command_unit, '(a)' ) '"" using 1:3 with lines lw 3'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Gnuplot commands written to "' &
    // trim ( command_filename ) // '".'
!
!  Free memory.
!
  deallocate ( a3 )
  deallocate ( f )
  deallocate ( u )
  deallocate ( w )
  deallocate ( x )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FD1D_ADVECTION_DIFFUSION_STEADY'
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
subroutine trisolve ( n, a, b, x )

!*****************************************************************************80
!
!! TRISOLVE factors and solves a tridiagonal system.
!
!  Discussion:
!
!    The three nonzero diagonals of the N by N matrix are stored as 3
!    columns of an N by 3 matrix.
!
!  Example:
!
!    Here is how a tridiagonal matrix of order 5 would be stored:
!
!       *  A11 A12
!      A21 A22 A23
!      A32 A33 A34
!      A43 A44 A45
!      A54 A55  *
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the linear system.
!
!    Input/output, real ( kind = 8 ) A(N,3).
!    On input, the tridiagonal matrix.
!    On output, the data in these vectors has been overwritten
!    by factorization information.
!
!    Input, real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!    Output, real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,3)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xmult
!
!  The diagonal entries can't be zero.
!
  do i = 1, n
    if ( a(i,2) == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRISOLVE - Fatal error!'
      write ( *, '(a,i8,a)' ) '  A(', i, ',2) = 0.'
      stop 1
    end if
  end do

  x(1:n) = b(1:n)

  do i = 2, n
    xmult = a(i,1) / a(i-1,2)
    a(i,2) = a(i,2) - xmult * a(i-1,3)
    x(i)   = x(i)   - xmult * x(i-1)
  end do

  x(n) = x(n) / a(n,2)
  do i = n-1, 1, -1
    x(i) = ( x(i) - a(i,3) * x(i+1) ) / a(i,2)
  end do

  return
end
