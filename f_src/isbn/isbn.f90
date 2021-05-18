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
function ch_is_isbn_digit ( ch )

!*****************************************************************************80
!
!! CH_IS_ISBN_DIGIT is TRUE if a character is an ISBN digit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character to be analyzed.
!
!    Output, logical ( kind = 4 ) CH_IS_ISBN_DIGIT, is TRUE if the character
!    is an ISBN digit.
!
  implicit none

  character ch
  logical ( kind = 4 ) ch_is_isbn_digit
  logical ( kind = 4 ) value

  if ( lle ( '0', ch ) .and. lle ( ch, '9' ) ) then
    value = .true.
  else if ( ch == 'x' .or. ch == 'X' ) then
    value = .true.
  else
    value = .false.
  end if

  ch_is_isbn_digit = value

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
!    Input, character CH, the decimal digit, '0' through '9' are legal.
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
function i4_to_isbn_digit ( i )

!*****************************************************************************80
!
!! I4_TO_ISBN_DIGIT converts an I4 to an ISBN digit.
!
!  Discussion:
!
!    Only the integers 0 through 10 can be input.  The representation
!    of 10 is 'X'.
!
!    An I4 is an integer ( kind = 4 ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, an integer between 0 and 10.
!
!    Output, character I4_TO_ISBN_DIGIT, the ISBN character code of the integer.
!    If I is illegal, the value '?' is returned.
!
  implicit none

  integer ( kind = 4 ) i
  character i4_to_isbn_digit
  character value

       if ( i == 0 ) then
    value = '0'
  else if ( i == 1 ) then
    value = '1'
  else if ( i == 2 ) then
    value = '2'
  else if ( i == 3 ) then
    value = '3'
  else if ( i == 4 ) then
    value = '4'
  else if ( i == 5 ) then
    value = '5'
  else if ( i == 6 ) then
    value = '6'
  else if ( i == 7 ) then
    value = '7'
  else if ( i == 8 ) then
    value = '8'
  else if ( i == 9 ) then
    value = '9'
  else if ( i == 10 ) then
    value = 'X'
  else
    value = '?'
  end if

  i4_to_isbn_digit = value

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
function isbn_check_digit_calculate ( s )

!*****************************************************************************80
!
!! ISBN_CHECK_DIGIT_CALCULATE determines the check digit for an ISBN.
!
!  Discussion:
!
!    ISBN stands for International Standard Book Number.  A unique ISBN
!    is assigned to each new book.  The ISBN includes 10 digits.  There is
!    an initial digit, then a dash, then a set of digits which are a
!    code for the publisher, another digit, and then the check digit:
!
!      initial-publisher-book-check
!
!    The number of digits used for the publisher and book codes can vary,
!    but the check digit is always one digit, and the total number of
!    digits is always 10.
!
!    The check digit is interesting, because it is a way of trying to
!    make sure that an ISBN has not been incorrectly copied.  Specifically,
!    if the ISBN is correct, then its ten digits will satisfy
!
!       10 * A + 9 * B + 8 * C + 7 * D + 6 * E
!      + 5 * F * 4 * G * 3 * H + 2 * I +     J  = 0 mod 11.
!
!    Here, we've taken 'A' to represent the first digit and 'J' the
!    last (which is the check digit).  In order for the code to work,
!    the value of J must be allowed to be anything from 0 to 10.  In
!    cases where J works out to be 10, the special digit 'X' is used.
!    An 'X' digit can only occur in this last check-digit position
!    on an ISBN.
!
!  Example:
!
!    S  => 0-8493-9640-?
!    D <=  9
!
!    meaning the ISBN is 0-8493-9640-9
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
!    Input, character ( len = * ) S, a string.  Dashes and spaces and other
!    nonnumeric data is ignored, but at least 9 digits are expected, and only
!    the first 9 digits will be examined.
!
!    Output, character ISBN_CHECK_DIGIT_CALCULATE, the ISBN 
!    check digit that should be appended to form the full 10 digit ISBN.
!
  implicit none

  character c
  integer ( kind = 4 ) d
  integer ( kind = 4 ) dvec(9)
  character isbn_check_digit_calculate
  integer ( kind = 4 ) i
  character i4_to_isbn_digit
  integer ( kind = 4 ) n
  character ( len = * ) s
!
!  Extract first 9 digits.
!
  n = 9
  call s_to_digits ( s, n, dvec )
!
!  Compute the check digit.
!
  d = 0
  do i = 1, 9
    d = d + ( 11 - i ) * dvec(i)
  end do

  d = mod ( 11 - mod ( d, 11 ), 11 )
!
!  Convert digits 0 through 9, 10 to characters '0' through '9', 'X'.
!
  c = i4_to_isbn_digit ( d )

  isbn_check_digit_calculate = c

  return
end
function isbn_digit_to_i4 ( c )

!*****************************************************************************80
!
!! ISBN_DIGIT_TO_I4 converts an ISBN digit into an integer.
!
!  Discussion:
!
!    The characters '0' through '9' stand for themselves, but
!    the character 'X' or 'x' stands for 10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the ISBN character code to be converted.
!
!    Output, integer ( kind = 4 ) ISBN_DIGIT_TO_I4, the numeric value of the 
!    character code, between 0 and 10.  This value is returned as -1 if C is
!    not a valid character code.
!
  implicit none

  character c
  integer ( kind = 4 ) isbn_digit_to_i4
  integer ( kind = 4 ) value

       if ( c == '0' ) then
    value = 0
  else if ( c == '1' ) then
    value = 1
  else if ( c == '2' ) then
    value = 2
  else if ( c == '3' ) then
    value = 3
  else if ( c == '4' ) then
    value = 4
  else if ( c == '5' ) then
    value = 5
  else if ( c == '6' ) then
    value = 6
  else if ( c == '7' ) then
    value = 7
  else if ( c == '8' ) then
    value = 8
  else if ( c == '9' ) then
    value = 9
  else if ( c == 'X' .or. c == 'x' ) then
    value = 10
  else
    value = -1
  end if

  isbn_digit_to_i4 = value

  return
end
function isbn_is_valid ( s )

!*****************************************************************************80
!
!! ISBN_IS_VALID reports whether an ISBN is valid.
!
!  Discussion:
!
!    ISBN stands for International Standard Book Number.  A unique ISBN
!    is assigned to each new book.  The ISBN includes 10 digits.  There is
!    an initial digit, then a dash, then a set of digits which are a
!    code for the publisher, another digit, and then the check digit:
!
!      initial-publisher-book-check
!
!    The number of digits used for the publisher and book codes can vary,
!    but the check digit is always one digit, and the total number of
!    digits is always 10.
!
!    The check digit is interesting, because it is a way of trying to
!    make sure that an ISBN has not been incorrectly copied.  Specifically,
!    if the ISBN is correct, then its ten digits will satisfy
!
!       10 * A + 9 * B + 8 * C + 7 * D + 6 * E
!      + 5 * F * 4 * G * 3 * H + 2 * I +     J  = 0 mod 11.
!
!    Here, we've taken 'A' to represent the first digit and 'J' the
!    last (which is the check digit).  In order for the code to work,
!    the value of J must be allowed to be anything from 0 to 10.  In
!    cases where J works out to be 10, the special digit 'X' is used.
!    An 'X' digit can only occur in this last check-digit position
!    on an ISBN.
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
!    Input, character ( len = * ) S, a string containing 12 digits.
!    Dashes and other characters will be ignored.
!
!    Output, logical ( kind = 4 ) ISBN_IS_VALID, is TRUE if the string is valid.
!
  implicit none

  character c1
  integer ( kind = 4 ) d1
  integer ( kind = 4 ) d2
  integer ( kind = 4 ) dvec(10)
  character isbn_check_digit_calculate
  logical ( kind = 4 ) isbn_is_valid
  integer ( kind = 4 ) isbn_digit_to_i4
  integer ( kind = 4 ) n
  character ( len = * ) s
  logical ( kind = 4 ) value

  n = 10
  call s_to_isbn_digits ( s, n, dvec )

  c1 = isbn_check_digit_calculate ( s )
  d1 = isbn_digit_to_i4 ( c1 )

  d2 = dvec(10)

  if ( d1 == d2 ) then
    value = .true.
  else
    value = .false.
  end if

  isbn_is_valid = value

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
subroutine s_to_isbn_digits ( s, n, dvec )

!*****************************************************************************80
!
!! S_TO_ISBN_DIGITS extracts N ISBN digits from a string.
!
!  Discussion:
!
!    The string may include spaces, letters, and dashes, but only the
!    digits '0' through '9' and 'X' will be extracted.
!
!  Example:
!
!    S  => 34E9X4-70.6
!    N  => 5
!    D <=  (/ 3, 4, 9, 10, 4 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 September 2015
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
  logical ( kind = 4 ) ch_is_isbn_digit
  integer ( kind = 4 ) d_pos
  integer ( kind = 4 ) dvec(n)
  integer ( kind = 4 ) isbn_digit_to_i4
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
      write ( *, '(a)' ) 'S_TO_ISBN_DIGITS - Fatal error!'
      write ( *, '(a)' ) '  Could not read enough data from string.'
      stop 1
    end if

    c = s(s_pos:s_pos)

    if ( ch_is_isbn_digit ( c ) ) then
      d_pos = d_pos + 1
      dvec(d_pos) = isbn_digit_to_i4 ( c )
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
