function ch_to_rot13 ( ch )

!*****************************************************************************80
!
!! CH_TO_ROT13 converts a character to its ROT13 equivalent.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions, which
!    guarantees the ASCII collating sequence.
!
!    Two applications of CH_TO_ROT13 to a character will return the original.
!
!    As a further scrambling, digits are similarly rotated using
!    a "ROT5" scheme.
!
!  Example:
!
!    Input:  Output:
!
!    a       n
!    C       P
!    J       W
!    1       6
!    5       0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character to be converted.
!
!    Output, character CH_TO_ROT13, the ROT13 equivalent of the character.
!
  implicit none

  character ch
  character ch_to_rot13
  integer ( kind = 4 ) itemp

  itemp = iachar ( ch )
!
!  [0:4] -> [5:9]
!
  if ( 48 <= itemp .and. itemp <= 52 ) then
    itemp = itemp + 5
!
!  [5:9] -> [0:4]
!
  else if ( 53 <= itemp .and. itemp <= 57 ) then
    itemp = itemp - 5
!
!  [A:M] -> [N:Z]
!
  else if ( 65 <= itemp .and. itemp <= 77 ) then
    itemp = itemp + 13
!
!  [N:Z] -> [A:M]
!
  else if ( 78 <= itemp .and. itemp <= 90 ) then
    itemp = itemp - 13
!
!  [a:m] -> [n:z]
!
  else if ( 97 <= itemp .and. itemp <= 109 ) then
    itemp = itemp + 13
!
!  [n:z] -> [a:m]
!
  else if ( 110 <= itemp .and. itemp <= 122 ) then
    itemp = itemp - 13
  end if

  ch_to_rot13 = achar ( itemp )

  return
end
subroutine s_quote ( s1, mark, s2 )

!*****************************************************************************80
!
!! S_QUOTE "quotes" a string.
!
!  Discussion:
!
!    Actually, it simply puts the string MARK before and after the string S1.
!
!    Sometimes, when you print a string, you want to put quote marks around it.
!    This is one way to do that.
!
!  Examples:
!
!    S1        MARK  S2
!    --------  ----  ----------
!    Hi, Bob!  "     "Hi, Bob!"
!    De        Loop  LoopDeLoop
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!   30 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, a string to be "quoted".
!
!    Input, character ( len = * ) MARK, the "quote mark".
!
!    Output, character ( len = * ) S2, the "quoted" string.
!
  implicit none

  character ( len = * ) mark
  character ( len = * ) s1
  character ( len = * ) s2

  s2 = trim ( mark ) // trim ( s1 ) // trim ( mark )

  return
end
subroutine s_to_rot13 ( s1, s2 )

!*****************************************************************************80
!
!! S_TO_ROT13 "rotates" the alphabetical characters in a string by 13 positions.
!
!  Discussion:
!
!    Two applications of the routine will return the original string.
!
!  Example:
!
!    Input:                      Output:
!
!    abcdefghijklmnopqrstuvwxyz  nopqrstuvwxyzabcdefghijklm
!    Cher                        Pure
!    James Thurston Howell       Wnzrf Guhefgba Ubjryy
!    0123456789                  5678901234
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, a string to be "rotated".
!
!    Output, character ( len = * ) S2, the rotated string.
!
  implicit none

  character ch_to_rot13
  integer ( kind = 4 ) i
  character ( len = * ) s1
  integer ( kind = 4 ) s1_length
  character ( len = * ) s2

  s1_length = len_trim ( s1 )
  s2 = ''

  do i = 1, s1_length
    s2(i:i) = ch_to_rot13 ( s1(i:i) )
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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end