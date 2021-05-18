subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 January 2008
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
function humps_antideriv ( x )

!*****************************************************************************80
!
!! humps_antideriv evaluates the antiderivative of the humps function.
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
!    06 August 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) x: the argument.
!
!  Output:
!
!    real ( kind = 8 ) humps_antideriv: the value of the antiderivative at x.
!
  implicit none

  real ( kind = 8 ) humps_antideriv
  real ( kind = 8 ) x
  real ( kind = 8 ) ya

  ya = ( 1.0D+00 / 0.1D+00 ) * atan ( ( x - 0.3D+00 ) / 0.1D+00 ) &
     + ( 1.0D+00 / 0.2D+00 ) * atan ( ( x - 0.9D+00 ) / 0.2D+00 ) &
     - 6.0D+00 * x

  humps_antideriv = ya

  return
end
function humps_deriv ( x )

!*****************************************************************************80
!
!! humps_deriv evaluates the derivative of the humps function.
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
!    06 August 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) x: the argument.
!
!  Output:
!
!    real ( kind = 8 ) humps_deriv: the value of the derivative at x.
!
  implicit none

  real ( kind = 8 ) humps_deriv
  real ( kind = 8 ) x
  real ( kind = 8 ) yp

  yp = - 2.0D+00 * ( x - 0.3D+00 ) / ( ( x - 0.3D+00 )**2 + 0.01D+00 )**2 &
       - 2.0D+00 * ( x - 0.9D+00 ) / ( ( x - 0.9D+00 )**2 + 0.04D+00 )**2

  humps_deriv = yp

  return
end
function humps_deriv2 ( x )

!*****************************************************************************80
!
!! humps_deriv2 evaluates the second derivative of the humps function.
!
!  Discussion:
!
!    y = 1.0 / ( ( x - 0.3 )^2 + 0.01 )
!      + 1.0 / ( ( x - 0.9 )^2 + 0.04 )
!      - 6.0
!
!    yp = - 2.0 * ( x - 0.3 ) / ( ( x - 0.3 )^2 + 0.01 )^2
!         - 2.0 * ( x - 0.9 ) / ( ( x - 0.9 )^2 + 0.04 )^2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) x: the argument.
!
!  Output:
!
!    real ( kind = 8 ) humps_deriv2: the value of the second derivative at x.
!
  implicit none

  real ( kind = 8 ) humps_deriv2
  real ( kind = 8 ) u1
  real ( kind = 8 ) u1p
  real ( kind = 8 ) u2
  real ( kind = 8 ) u2p
  real ( kind = 8 ) v1
  real ( kind = 8 ) v1p
  real ( kind = 8 ) v2
  real ( kind = 8 ) v2p
  real ( kind = 8 ) x
  real ( kind = 8 ) ypp

  u1 = - 2.0D+00 * ( x - 0.3D+00 )
  v1 = ( ( x - 0.3D+00 )**2 + 0.01D+00 )**2
  u2 = - 2.0D+00 * ( x - 0.9D+00 )
  v2 = ( ( x - 0.9D+00 )**2 + 0.04D+00 )**2

  u1p = - 2.0D+00
  v1p = 2.0D+00 * ( ( x - 0.3D+00 )**2 + 0.01D+00 ) * 2.0D+00 * ( x - 0.3D+00 ) 
  u2p = - 2.0D+00
  v2p = 2.0D+00 * ( ( x - 0.9D+00 )**2 + 0.04D+00 ) * 2.0D+00 * ( x - 0.9D+00 )

  ypp = ( u1p * v1 - u1 * v1p ) / v1 / v1 &
      + ( u2p * v2 - u2 * v2p ) / v2 / v2

  humps_deriv2 = ypp

  return
end
function humps_fun ( x )

!*****************************************************************************80
!
!! humps_fun evaluates the humps function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) x: the evaluation point.
!
!  Output:
!
!    real ( kind = 8 ) humps_fun: the function value.
!
  implicit none

  real ( kind = 8 ) humps_fun
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  y = 1.0D+00 / ( ( x - 0.3D+00 )**2 + 0.01D+00 ) &
    + 1.0D+00 / ( ( x - 0.9D+00 )**2 + 0.04D+00 ) &
    - 6.0D+00

  humps_fun = y

  return
end
function humps_ode ( x, y )

!*****************************************************************************80
!
!! humps_ode evaluates the derivative of the humps function for an ODE solver.
!
!  Discussion:
!
!    This verion of "humps_deriv" appends the input argument "y", as expected 
!    by most ODE solving software.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) x: the argument.
!
!    real ( kind = 8 ) y: the value of the dependent variable.
!
!  Output:
!
!    real ( kind = 8 ) humps_ode: the value of the derivative of the humps function.
!
  implicit none

  real ( kind = 8 ) humps_ode
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) yp

  yp = - 1.0D+00 / ( ( x - 0.3D+00 )**2 + 0.01D+00 )**2 &
       * 2.0D+00 * ( x - 0.3D+00 ) &
       - 1.0D+00 / ( ( x - 0.9D+00 )**2 + 0.04D+00 )**2 &
       * 2.0D+00 * ( x - 0.9D+00 )

  humps_ode = yp

  return
end
subroutine plot_xy ( n, x, y, prefix )

!*****************************************************************************80
!
!! plot_xy plots xy data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) n : the number of data points.
!
!    real ( kind = 8 ) x(n), y(n): the data points.
!
!    character ( len = * ) prefix: the prefix for the plot names.
!
  implicit none

  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  integer ( kind = 4 ) i
  character ( len = 255 ) output_filename
  character ( len = * ) prefix
  character ( len = 255 ) prefix2
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Create the data file.
!
  data_filename = trim ( prefix ) // '_data.txt'
  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, n
    write ( data_unit, '(2x,g14.6,2x,g14.6)' ) x(i), y(i)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '  Created graphics data file "' // trim ( data_filename ) // '".'
!
!  Plot the selected data.
!
  command_filename = trim ( prefix ) // '_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set nokey'
  output_filename = trim ( prefix ) // '.png'
  write ( command_unit, '(a)' ) &
    'set output "' // trim ( output_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---Y(X)--->"'
  call s_escape_tex2 ( prefix, prefix2 )
  write ( command_unit, '(a)' ) 'set title "' // trim ( prefix2 ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) &
    // '" using 1:2 lw 3 linecolor rgb "blue"'

  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created graphics command file "' // trim ( command_filename ) // '".'

  return
end
subroutine s_escape_tex2 ( s1, s2 )

!*****************************************************************************80
!
!! S_ESCAPE_TEX2 de-escapes TeX escape sequences.
!
!  Discussion:
!
!    In particular, every occurrence of the characters '\', '_',
!    '^', '{' and '}' will be replaced by '\\', '\_', '\^',
!    '\{' and '\}'.  A TeX interpreter, on seeing these character
!    strings, is then likely to return the original characters.
!
!    In some cases, it seems that TWO backslashes are needed.
!    This version of the function provides them.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the string to be de-escaped.
!
!    Output, character ( len = * ) S2, a copy of the string,
!    modified to avoid TeX escapes.
!
  implicit none

  character ch
  character ( len = * ) s1
  integer ( kind = 4 ) s1_length
  integer ( kind = 4 ) s1_pos
  character ( len = * ) s2
  integer ( kind = 4 ) s2_pos

  s1_length = len_trim ( s1 )

  s1_pos = 0
  s2_pos = 0
  s2 = ' '

  do while ( s1_pos < s1_length )

    s1_pos = s1_pos + 1
    ch = s1(s1_pos:s1_pos)

    if ( ch == '\' .or. &
         ch == '_' .or. &
         ch == '^' .or. &
         ch == '{' .or. &
         ch == '}' ) then

      s2_pos = s2_pos + 1
      s2(s2_pos:s2_pos) = '\'
      s2_pos = s2_pos + 1
      s2(s2_pos:s2_pos) = '\'

    end if

    s2_pos = s2_pos + 1
    s2(s2_pos:s2_pos) = ch

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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end

