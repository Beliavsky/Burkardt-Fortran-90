subroutine backbin_rc ( n, reject, n2, choice )

!*****************************************************************************80
!
!! BACKBIN_RC uses reverse communication for binary backtracking.
!
!  Discussion:
!
!    If this procedure returns a solution with N2 = N, which is acceptable
!    to the user, then a full solution has been found.
!
!    If this procedure returns N2 = -1, no more potential solutions are
!    available to consider.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the full solution.
!
!    Input, logical REJECT, is TRUE if the proposed partial solution
!    in the first N2 entries of CHOICE must be rejected.
!
!    Input/output, integer ( kind = 4 ) N2, the length of the current
!    partial solution.  On first call for a given problem, the user
!    should set N2 to -1.  If the program has exhausted the search space,
!    the value of N2 will be returned as -1.
!
!    Input/output, integer ( kind = 4 ) CHOICE(N), indicates the current
!    partial solution in entries 1 through N2, which will contain 0 or 1.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) choice(n)
  integer ( kind = 4 ) n2
  logical reject
!
!  N2 = -1 means an initialization call.
!
  if ( n2 == -1 ) then

    choice(1:n) = -1
    n2 = 1
    choice(n2) = 1
!
!  1 <= FOCUS means we asked the user to evaluate CHOICE(1:N2).
!
!  N2 = N means we returned a full prospective solution
!  so in any case we must increment CHOICE.
!
!  Returning REJECT = 1 means no solution begins this way
!  so we must increment CHOICE.
!
  else if ( n2 == n .or. reject ) then

    do while ( 1 < n2 )
      if ( choice(n2) == 1 ) then
        choice(n2) = 0
        exit
      end if
      choice(n2) = -1
      n2 = n2 - 1
    end do
!
!  Have we exhausted the solution space?
!
    if ( n2 == 1 ) then
      if ( choice(n2) == 1 ) then
        choice(n2) = 0
      else
        choice(n2) = -1
        n2 = -1
      end if
    end if
!
!  N2 < N and not REJECT means we can increment N2.
!
  else

    n2 = n2 + 1
    choice(n2) = 1

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
