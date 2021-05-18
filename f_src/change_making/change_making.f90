subroutine change_making_list ( coin_num, coin_value, target, a )

!*****************************************************************************80
!
!! CHANGE_MAKING_LIST solves the change making problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 June 2020
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) COIN_NUM, the number of coin denomiations.
!
!    Input, integer ( kind = 4 ) COIN_VALUE(COIN_NUM), the value of each coin.
!    These values should be positive integers.
!
!    Input, integer ( kind = 4 ) TARGET, the desired sum.
!
!    Output, integer ( kind = 4 ) A(0:TARGET), A(T) lists the smallest number 
!    of coins needed to form the sum T, or "Inf" if it is not possible to form
!    this sum.
!
  implicit none

  integer ( kind = 4 ) coin_num
  integer ( kind = 4 ) target

  integer ( kind = 4 ) a(0:target)
  integer ( kind = 4 ) coin_value(coin_num)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j

  a(0) = 0
  a(1:target) = i4_huge
!
!  If T is the value of a coin, then A(T) is 1.
!
  do i = 1, coin_num
    if ( coin_value(i) <= target ) then
      a(coin_value(i)) = 1
    end if
  end do
!
!  To compute A(T) in general, consider getting there by adding
!  one coin of value V, and looking at A(T-V).
!
  do j = 1, target
    do i = 1, coin_num
      if ( 0 <= j - coin_value(i) ) then
        a(j) = min ( a(j) - 1, a(j-coin_value(i)) ) + 1
      end if
    end do
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
