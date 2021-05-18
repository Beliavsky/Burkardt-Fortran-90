program main

!*****************************************************************************80
!
!! MAIN is the main program for f90_stop_test2.
!
!  Discussion:
!
!    C and C++ programs can return an integer value signalling the status
!    of the program's execution.  By convention, a return value of 0 indicates
!    that the program executed successfully, while a nonzero value indicates
!    some kind of error.
!
!    The return value of such a program can be retrieved and used to make
!    decisions.  In particular, if the execution of the program is part of
!    a script, then the failure of the program can be detected, and the
!    script can terminate gracefully.
!
!    For an example in the BASH shell, we might have a script that reads:
!
!      ./prog
!      if [ $? -ne 0 ]; then
!        echo "Errors while running prog."
!        exit
!      fi
!
!    Here, $? is a BASH symbol that returns the most recent program status
!    value, and the script exits if that value is not zero.
!
!    FORTRAN has always had the option to include a constant integer value 
!    as part of the STOP statement.  At least some FORTRAN compilers can
!    treat this value as a  program status value that is returned to the
!    calling environment, and hence can signal whether certain errors
!    have occurred.
!
!    Here, we show a simple example in which the program is guaranteed
!    to fail, and in that case will return the arbitrary but nonzero value 13.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2013
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
  write ( *, '(a)' ) 'f90_stop_test2'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Demonstrate how a FORTRAN90 program can return an'
  write ( *, '(a)' ) '  integer program status value to the calling environment.'
!
!  Add up the squares, but stop if we exceed 250.
!
  t = 0
  do i = 1, 100
    t = t + i * i
    if ( 250 < t ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'f90_stop_test2 - Fatal error!'
      write ( *, '(a)' ) '  Sum exceeds 250.'
      stop 13
    end if
  end do
!
!  If the final sum is less than 1000, then that's an error too.
!
  if ( t < 1000 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'f90_stop_test2 - Fatal error!'
    write ( *, '(a)' ) '  Sum is less than 1000.'
    stop 99
  end if
!
!  If no error occurred, we have normal execution.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'f90_stop_test2:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
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
