program main

!*****************************************************************************80
!
!! MAIN is the main program for CELLULAR_AUTOMATON.
!
!  Discussion:
!
!    This program carries out iterations of the 1D cellular automaton
!    known as rule 30.
!
!    Given an initial linear array of 0's and 1's, rule 30 produces a new
!    array using the rules:
!
!      111  110  101  100  011  010  001  000
!       V    V    V    V    V    V    V    V
!       0    0    0    1    1    1    1    0     
!
!    Note that there are 256 = 2^8 possible ways to fill in this output
!    chart, and that rule 30 gets its index by the fact that
!    (0,0,0,1,1,1,1,0) can be interpreted as the binary representation of 30.
!
!    For instance, if the current values of X(4), X(5) and X(6) are
!    0, 1 and 1, respectively, then the new value of X(5) will be 1.
!
!    The first and last entries of the array must be treated specially, since
!    they don't have a left or right neighbor.  One simple treatment is 
!    to assume that there are phantom neighbors whose values are both 0.
!    Another is to enforce periodic boundary conditions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    A New Kind of Science,
!    Wolfram Media, 2002,
!    ISBN13: 978-1579550080,
!    LC: QA267.5.C45.W67.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) step_num
  character, allocatable :: x(:)
  character, allocatable :: x_old(:)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CELLULAR_AUTOMATON:'
  write ( *, '(a)' ) '  FORTRAN90 version.'

  n = 80
  step_num = 80

  allocate ( x(0:n+1) )
  allocate ( x_old(0:n+1) )

  x(0:n+1) = ' '
  x(40) = '*'

  write ( *, '(80a)' ) x(1:n)

  do j = 1, step_num

    x_old(0:n+1) = x(0:n+1)

    do i = 1, n
!
!  The transformation rules are:
!
!  111  110  101  100  011  010  001  000
!   |    |    |    |    |    |    |    |
!   0    0    0    1    1    1    1    0
!
!  which means this rule has binary code 00011110 = 16 + 8 + 4 + 2 = 30
!
      if ( ( x_old(i-1) == ' ' .and. &
             x_old(i)   == ' ' .and. &
             x_old(i+1) == '*' ) .or. &
           ( x_old(i-1) == ' ' .and. &
             x_old(i)   == '*' .and. &
             x_old(i+1) == ' ' ) .or. &
           ( x_old(i-1) == ' ' .and. &
             x_old(i)   == '*' .and. &
             x_old(i+1) == '*' ) .or. &
           ( x_old(i-1) == '*' .and. &
             x_old(i)  == ' ' .and. &
             x_old(i+1) == ' ' ) ) then
        x(i) = '*'
      else
        x(i) = ' '
      end if

    end do
!
!  Enforce periodic boundary conditions.
!
    x(0) = x(n)
    x(n+1) = x(1)

    write ( *, '(80a)' ) x(1:n)

  end do
!
!  Free memory.
!
  deallocate ( x )
  deallocate ( x_old )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CELLULAR_AUTOMATON:'
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
