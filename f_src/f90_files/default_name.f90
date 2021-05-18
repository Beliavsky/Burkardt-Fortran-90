program main

!*****************************************************************************80
!
!! MAIN is the main program for DEFAULT_NAME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: unit_num = 4

  character ( len = 255 ) filename
  integer ( kind = 4 ) i
  integer ( kind = 4 ) output
  integer ( kind = 4 ), dimension ( unit_num ) :: unit_test = (/ &
    8, 11, 34, 99 /)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DEFAULT_NAME:'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  The FORTRAN90 open() statement is used to access'
  write ( *, '(a)' ) '  files to be read or written.  Input to the statement'
  write ( *, '(a)' ) '  includes a filename (what the operating system calls'
  write ( *, '(a)' ) '  the file) and a unit number (a numeric identifier'
  write ( *, '(a)' ) '  used by the program, instead of the filename.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The user may omit the filename, in which case'
  write ( *, '(a)' ) '  FORTRAN assumes a default name, based on the unit'
  write ( *, '(a)' ) '  number.  For example, a file associated with unit'
  write ( *, '(a)' ) '  20 might have a default name of "FORT.20" or "fort.20"'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The actual default names are not specified by the'
  write ( *, '(a)' ) '  FORTRAN standard and so are dependent on the compiler.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  This program opens several anonymous files for writing'
  write ( *, '(a)' ) '  and thus enables the user to figure out the naming'
  write ( *, '(a)' ) '  scheme that is used.'
  write ( *, '(a)' ) ' '

  do i = 1, unit_num

    output = unit_test(i)

    open ( unit = output, status = 'unknown' )
    inquire ( unit = output, name = filename )
    write ( output, '(a)' ) 'DEFAULT_NAME wrote this output.'
    write ( output, '(a,i2)' ) 'This output was written to unit ', output
    close ( unit = unit_test(i) )

    write ( *, '(a)' ) &
      '  Created the anonymous file "' // trim ( filename ) // '".'

  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DEFAULT_NAME:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
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
