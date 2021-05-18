program main

!*****************************************************************************80
!
!! MAIN is the main program for f90_stop_test1
!
!  Discussion:
!
!    The FORTRAN STOP statement can include a text message indicating
!    the reason for program termination, such as an error condition, or
!    satisfactory conclusion.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 May 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) t

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'f90_stop_test1'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Demonstrate how a FORTRAN90 program can print a'
  write ( *, '(a)' ) '  message when a STOP is executed.'
!
!  Add up the squares, but stop if we exceed 250.
!
  t = 0
  do i = 1, 100
    t = t + i * i
    if ( 250 < t ) then
      stop 'f90_stop_test1: fatal error, sum exceeds 250.'
    end if
  end do
!
!  If the final sum is less than 1000, then that's an error too.
!
  if ( t < 1000 ) then
    stop 'f90_stop_test1: fatal error, sum in less than 1000.'
  end if
!
!  If no error occurred, we have normal execution.
!
  call timestamp ( )

  stop 'f90_stop_test1: normal end of execution.'
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
