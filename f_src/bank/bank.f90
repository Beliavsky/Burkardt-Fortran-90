function bank_check_digit_calculate ( s )

!*****************************************************************************80
!
!! BANK_CHECK_DIGIT_CALCULATE returns the check digit of a bank checksum.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string containing at least 8 digits.
!    Dashes and other characters will be ignored.  A 9th digit may be
!    included, but it will be ignored.
!
!    Output, integer ( kind = 4 ) BANK_CHECK_DIGIT_CALCULATE, the check digit.
!
  implicit none

  integer ( kind = 4 ) bank_check_digit_calculate
  integer ( kind = 4 ) d
  integer ( kind = 4 ) dvec(8)
  integer ( kind = 4 ) n
  character ( len = * ) s

  n = 8
  call s_to_digits ( s, n, dvec )

  d = 3 * sum ( dvec(1:7:3) ) &
    + 7 * sum ( dvec(2:8:3) ) &
    +     sum ( dvec(3:6:3) )

  d = mod ( d, 10 )

  d = mod ( 10 - d, 10 )

  bank_check_digit_calculate = d

  return
end
function bank_is_valid ( s )

!*****************************************************************************80
!
!! BANK_IS_VALID reports whether a bank checksum is valid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string containing 9 digits.
!    Dashes and other characters will be ignored.
!
!    Output, logical ( kind = 4 ) BANK_IS_VALID, is TRUE if the string
!    is valid.
!
  implicit none

  integer ( kind = 4 ) bank_check_digit_calculate
  logical ( kind = 4 ) bank_is_valid
  integer ( kind = 4 ) d1
  integer ( kind = 4 ) d2
  integer ( kind = 4 ) dvec(9)
  integer ( kind = 4 ) n
  character ( len = * ) s
  logical ( kind = 4 ) value

  n = 9
  call s_to_digits ( s, n, dvec )

  d1 = bank_check_digit_calculate ( s )
  d2 = dvec(9)

  if ( d1 == d2 ) then
    value = .true.
  else
    value = .false.
  end if

  bank_is_valid = value

  return
end
function ch_is_digit ( ch )

!*****************************************************************************80
!
!! CH_IS_DIGIT is TRUE if a character is a decimal digit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character to be analyzed.
!
!    Output, logical ( kind = 4 ) CH_IS_DIGIT, is TRUE if the character is 
!    a digit.
!
  implicit none

  character ch
  logical ( kind = 4 ) ch_is_digit

  if ( lle ( '0', ch ) .and. lle ( ch, '9' ) ) then
    ch_is_digit = .true.
  else
    ch_is_digit = .false.
  end if

  return
end
subroutine ch_to_digit ( ch, digit )

!*****************************************************************************80
!
!! CH_TO_DIGIT returns the value of a base 10 digit.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Example:
!
!     CH  DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    ...  ...
!    '9'    9
!    'X'   -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the decimal digit, '0' through '9'.
!
!    Output, integer ( kind = 4 ) DIGIT, the corresponding value.
!    If CH was 'illegal', then DIGIT is -1.
!
  implicit none

  character ch
  integer ( kind = 4 ) digit

  if ( lle ( '0', ch ) .and. lle ( ch, '9' ) ) then

    digit = iachar ( ch ) - 48

  else

    digit = - 1

  end if

  return
end
subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,a,2x,i12)' ) i, ':', a(i)
  end do

  return
end
subroutine s_to_digits ( s, n, dvec )

!*****************************************************************************80
!
!! S_TO_DIGITS extracts N digits from a string.
!
!  Discussion:
!
!    The string may include spaces, letters, and dashes, but only the
!    digits 0 through 9 will be extracted.
!
!  Example:
!
!    S  => 34E94-70.6
!    N  => 5
!    D <=  (/ 3, 4, 9, 4, 7 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string.
!
!    Input, integer ( kind = 4 ) N, the number of digits to extract.
!
!    Output, integer ( kind = 4 ) DVEC(N), the extracted digits.
!
  implicit none

  integer ( kind = 4 ) n

  character c
  logical ( kind = 4 ) ch_is_digit
  integer ( kind = 4 ) d
  integer ( kind = 4 ) d_pos
  integer ( kind = 4 ) dvec(n)
  integer ( kind = 4 ) lenc
  character ( len = * ) s
  integer ( kind = 4 ) s_pos

  lenc = len_trim ( s )

  s_pos = 0
  d_pos = 0

  do while ( d_pos < n )

    s_pos = s_pos + 1

    if ( lenc < s_pos ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'S_TO_DIGITS - Fatal error!'
      write ( *, '(a)' ) '  Could not read enough data from string.'
      stop 1
    end if

    c = s(s_pos:s_pos)

    if ( ch_is_digit ( c ) ) then
      d_pos = d_pos + 1
      call ch_to_digit ( c, d )
      dvec(d_pos) = d
    end if

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
