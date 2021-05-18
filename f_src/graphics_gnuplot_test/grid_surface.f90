program main

!*****************************************************************************80
!
!! grid_surface draws a surface from a table Z(X,Y).
!
!  Discussion:
!
!    Here, the grid is a 41x41 array of equally spaced points in [-2,2]x[-2,2].
!
!    and Z(X,Y) is a function which can be evaluated by:
!
!      Z = exp ( - ( X^2 + Y^2 ) ) * cos ( 0.25 * X )
!        * sin ( Y ) * cos ( 2 * (X^2 + Y^2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 April 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none
  
  integer ( kind = 4 ), parameter :: m = 41
  integer ( kind = 4 ), parameter :: n = 41

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = 12 ) :: header = 'grid_surface'
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r2
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'grid_surface:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Make a surface plot Z(X,Y) for a 41x41 table.'
!
!  Create the data file.
!
  call get_unit ( data_unit )

  data_filename = header // '_data.txt'

  open ( unit = data_unit, file = data_filename, status = 'replace' )

  do i = 1, m
    if ( 1 < i ) then
      write ( data_unit, '(a)' )
    end if
    x = ( real ( m - i,     kind = 8 ) * ( -2.0 ) & 
        + real (     i - 1, kind = 8 ) * (  2.0 ) ) & 
        / real ( m     - 1, kind = 8 )
    do j = 1, n
      y = ( real ( n - j,     kind = 8 ) * ( -2.0 ) & 
          + real (     j - 1, kind = 8 ) * (  2.0 ) ) & 
          / real ( n     - 1, kind = 8 )
      r2 = x**2 + y**2
      z = exp ( - r2 ) * cos ( 0.25 * x ) * sin ( y ) * cos ( 2 * r2 )
      write ( data_unit, '(3(2x,g14.6))' ) x, y, z
    end do
  end do

  close ( unit = data_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  grid_surface: data stored in "' &
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
  write ( command_unit, '(a)' ) 'set xlabel "X"'
  write ( command_unit, '(a)' ) 'set ylabel "Y"'
  write ( command_unit, '(a)' ) 'set zlabel "Z"'
  write ( command_unit, '(a)' ) 'set title "Grid Surface"'
  write ( command_unit, '(a)' ) 'set view 77, 77'
  write ( command_unit, '(a)' ) 'set xyplane -0.5'
  write ( command_unit, '(a)' ) 'set hidden3d'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'splot "' // trim ( data_filename ) // &
    '" with lines'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  grid_surface: plot commands stored in "' &
    // trim ( command_filename ) // '".'
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'grid_surface:'
  write ( *, '(a)' ) '  Normal end of execution.'
  call timestamp ( )

  stop ( 0 )
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
!! timestamp prints the current YMDHMS date as a time stamp.
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

