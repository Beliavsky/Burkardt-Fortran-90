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
function luhn_check_digit_calculate ( s )

!*****************************************************************************80
!
!! LUHN_CHECK_DIGIT_CALCULATE determines the Luhn check digit of a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2015
!
!  Reference:
!
!    https://en.wikipedia.org/wiki/Luhn_algorithm
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string of digits to be checked.
!
!    Output, integer ( kind = 4 ) VALUE, the Luhn check digit for this string.
!
  implicit none

  integer ( kind = 4 ) luhn_check_digit_calculate
  integer ( kind = 4 ) luhn_checksum
  character ( len = * ) s
  integer ( kind = 4 ) s_len
  character ( len = 255 ) s2
  integer ( kind = 4 ) value

  s_len = len_trim ( s )
  s2 = ''
  s2(1:s_len) = s
  s2(s_len+1:s_len+1) = '0'

  value = luhn_checksum ( s2(1:s_len+1) )

  if ( value .ne. 0 ) then
    value = 10 - value
  end if

  luhn_check_digit_calculate = value
  
  return
end
function luhn_checksum ( s )

!*****************************************************************************80
!
!! LUHN_CHECKSUM determines the Luhn checksum of a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2015
!
!  Reference:
!
!    https://en.wikipedia.org/wiki/Luhn_algorithm
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string of digits to be checked.
!
!    Output, integer ( kind = 4 ) VALUE, is the Luhn checksum for this string.
!
  implicit none

  integer ( kind = 4 ) d2
  integer ( kind = 4 ), allocatable :: dvec(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) luhn_checksum
  integer ( kind = 4 ) n
  character ( len = * ) s
  integer ( kind = 4 ) s_digits_count
  integer ( kind = 4 ) value
!
!  Count the digits in S.
!
  n = s_digits_count ( s )
!
!  Extract the digits from S.
!
  allocate ( dvec(1:n) )
  call s_to_digits ( s, n, dvec )
!
!  Starting with the N-th digit, and going down by 2's, 
!  add the digit to the sum.
!
  value = sum ( dvec(n:1:-2) )
!
!  Starting with the (N-1)-th digit, and going down by 2's, 
!  double the digit, and ADD THE DIGITS to the sum.
!
  do i = n - 1, 1, -2
    d2 = 2 * dvec(i)
    value = value + ( d2 / 10 ) + mod ( d2, 10 )
  end do

  value = mod ( value, 10 )

  luhn_checksum = value

  deallocate ( dvec )

  return
end
function luhn_is_valid ( s )

!*****************************************************************************80
!
!! LUHN_IS_VALID determines whether a string with Luhn check digit is valid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2015
!
!  Reference:
!
!    https://en.wikipedia.org/wiki/Luhn_algorithm
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string of digits to be checked.
!
!    Output, logical ( kind = 4 ) VALUE, TRUE if the string is valid.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) luhn_checksum
  logical ( kind = 4 ) luhn_is_valid
  character ( len = * ) s
  logical ( kind = 4 ) value

  d = luhn_checksum ( s )

  if ( d == 0 ) then
    value = .true.
  else
    value = .false.
  end if

  luhn_is_valid = value
  
  return
end
function s_digits_count ( s )

!*****************************************************************************80
!
!! S_DIGITS_COUNT counts the digits in a string.
!
!  Discussion:
!
!    The string may include spaces, letters, and dashes, but only the
!    digits 0 through 9 will be counted.
!
!  Example:
!
!    S  => 34E94-70.6
!    N <=  7
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
!    Output, integer ( kind = 4 ) S_DIGITS_COUNT, the number of digits.
!
  implicit none

  character c
  logical ( kind = 4 ) ch_is_digit
  integer ( kind = 4 ) n
  character ( len = * ) s
  integer ( kind = 4 ) s_digits_count
  integer ( kind = 4 ) s_len
  integer ( kind = 4 ) s_pos

  s_len = len_trim ( s )

  s_pos = 0
  n = 0

  do while ( s_pos < s_len )

    s_pos = s_pos + 1

    c = s(s_pos:s_pos)

    if ( ch_is_digit ( c ) ) then
      n = n + 1
    end if

  end do

  s_digits_count = n

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
