program main

!*****************************************************************************80
!
!! MAIN is the main program for ISBN_TEST.
!
!  Discussion:
!
!    ISBN_PRB tests the ISBN library.
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
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ISBN_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ISBN library.'

  call ch_is_digit_test ( )
  call ch_is_isbn_digit_test ( )
  call ch_to_digit_test ( )
  call i4_to_isbn_digit_test ( )
  call isbn_check_digit_calculate_test ( )
  call isbn_digit_to_i4_test ( )
  call isbn_is_valid_test ( )
  call s_to_digits_test ( )
  call s_to_isbn_digits_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ISBN_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine ch_is_digit_test ( )

!*****************************************************************************80
!
!! CH_IS_DIGIT_TEST tests CH_IS_DIGIT.
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
  implicit none

  character c
  character, dimension ( 13 ) :: c_test = (/ &
    '0', '1', '2', '3', '4', &
    '5', '6', '7', '8', '9', &
    'X', '?', ' ' /)
  logical ( kind = 4 ) ch_is_digit
  integer ( kind = 4 ) i
  logical ( kind = 4 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CH_IS_DIGIT_TEST'
  write ( *, '(a)' ) '  CH_IS_DIGIT is TRUE if a character represents a digit.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   C      CH_IS_DIGIT(C)'
  write ( *, '(a)' ) ''

  do i = 1, 13
    c = c_test(i)
    value = ch_is_digit ( c )
    write ( *, '(2x,a,10x,l1)' ) '"' // c // '"', value
  end do

  return
end
subroutine ch_is_isbn_digit_test ( )

!*****************************************************************************80
!
!! CH_IS_ISBN_DIGIT_TEST tests CH_IS_ISBN_DIGIT.
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
  implicit none

  character c
  character, dimension ( 16 ) :: c_test = (/ &
    '0', '1', '2', '3', '4', &
    '5', '6', '7', '8', '9', &
    'X', 'x', 'Y', '*', '?', ' ' /)
  logical ( kind = 4 ) ch_is_isbn_digit
  integer ( kind = 4 ) i
  logical ( kind = 4 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CH_IS_ISBN_DIGIT_TEST'
  write ( *, '(a)' ) '  CH_IS_ISBN_DIGIT is TRUE if a character '
  write ( *, '(a)' ) '  represents an ISBN digit.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   C      CH_IS_ISBN_DIGIT(C)'
  write ( *, '(a)' ) ''

  do i = 1, 16
    c = c_test(i)
    value = ch_is_isbn_digit ( c )
    write ( *, '(2x,a,10x,l1)' ) '"' // c // '"', value
  end do

  return
end
subroutine ch_to_digit_test ( )

!*****************************************************************************80
!
!! CH_TO_DIGIT_TEST tests CH_TO_DIGIT.
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
  implicit none

  character c
  character, dimension (13 ):: c_test = (/ &
    '0', '1', '2', '3', '4', &
    '5', '6', '7', '8', '9', &
    'X', '?', ' ' /)

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CH_TO_DIGIT_TEST'
  write ( *, '(a)' ) '  CH_TO_DIGIT: character -> decimal digit'
  write ( *, '(a)' ) ' '

  do i = 1, 13
    c = c_test(i)
    call ch_to_digit ( c, i2 )
    write ( *, '(2x,a,5x,i8)' ) '"' // c // '"', i2
  end do

  return
end
subroutine i4_to_isbn_digit_test ( )

!*****************************************************************************80
!
!! I4_TO_ISBN_DIGIT_TEST tests I4_TO_ISBN_DIGIT.
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
  implicit none

  character c
  integer ( kind = 4 ) i
  character i4_to_isbn_digit

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_TO_ISBN_DIGIT_TEST'
  write ( *, '(a)' ) '  I4_TO_ISBN_DIGIT converts a digit to an ISBN character.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I4     I4_TO_ISBN_DIGIT(I4)'
  write ( *, '(a)' ) ''

  do i = 0, 11
    c = i4_to_isbn_digit ( i )
    write ( *, '(2x,i2,12x,a3)' ) i, '"' // c // '"'
  end do

  return
end
subroutine isbn_check_digit_calculate_test ( )

!*****************************************************************************80
!
!! ISBN_CHECK_DIGIT_CALCULATE_TEST tests ISBN_CHECK_DIGIT_CALCULATE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 2015
!
!  Author:
!
!    John Burkardt
!
  character c1
  character c2
  character ( len = 255 ) s1
  character isbn_check_digit_calculate

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ISBN_CHECK_DIGIT_CALCULATE_TEST'
  write ( *, '(a)' ) '  ISBN_CHECK_DIGIT_CALCULATE calculates the 10-th digit'
  write ( *, '(a)' ) '  (the check digit) of a 10-digit ISBN.'
  write ( *, '(a)' ) ''
!
!  Supply the full code, with dashes.
!
  s1 = '0-306-40615-2'
  c1 = '2'
  c2 = isbn_check_digit_calculate ( s1 )
  write ( *, '(a,a1,a,a1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', c2, ', expecting ', c1
!
!  Supply a partial code, with spaces.
!
  s1 = '0 8493 9640'
  c1 = '9'
  c2 = isbn_check_digit_calculate ( s1 )
  write ( *, '(a,a1,a,a1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', c2, ', expecting ', c1
!
!  Supply a partial code, no spaces.
!
  s1 = '158488059'
  c1 = '7'
  c2 = isbn_check_digit_calculate ( s1 )
  write ( *, '(a,a1,a,a1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', c2, ', expecting ', c1
!
!  Supply a partial code, no spaces.
!
  s1 = '246897531'
  c1 = '6'
  c2 = isbn_check_digit_calculate ( s1 )
  write ( *, '(a,a1,a,a1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', c2, ', expecting ', c1
!
!  Supply a partial code, no spaces.
!
  s1 = '135798642'
  c1 = '4'
  c2 = isbn_check_digit_calculate ( s1 )
  write ( *, '(a,a1,a,a1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', c2, ', expecting ', c1

  return
end
subroutine isbn_digit_to_i4_test ( )

!*****************************************************************************80
!
!! ISBN_DIGIT_TO_I4_TEST tests ISBN_DIGIT_TO_I4.
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
  implicit none

  character c
  character, dimension ( 16 ) :: c_test = (/ &
    '0', '1', '2', '3', '4', &
    '5', '6', '7', '8', '9', &
    'X', 'x', 'Y', '*', '?', ' ' /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4
  integer isbn_digit_to_i4

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ISBN_DIGIT_TO_I4_TEST'
  write ( *, '(a)' ) '  ISBN_DIGIT_TO_I4 converts an ISBN digit to an integer.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   C      ISBN_DIGIT_TO_I4(C)'
  write ( *, '(a)' ) ''

  do i = 1, 16
    c = c_test(i)
    i4 = isbn_digit_to_i4 ( c )
    write ( *, '(2x,a3,12x,i2)' ) '"' // c // '"', i4
  end do

  return
end
subroutine isbn_is_valid_test ( )

!*****************************************************************************80
!
!! ISBN_IS_VALID_TEST tests ISBN_IS_VALID.
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
  character ( len = 255 ) s1
  logical ( kind = 4 ) isbn_is_valid
  logical ( kind = 4 ) value1
  logical ( kind = 4 ) value2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ISBN_IS_VALID_TEST'
  write ( *, '(a)' ) '  ISBN_IS_VALID reports whether a ISBN is valid.'
  write ( *, '(a)' ) ''
!
!  Supply a valid code, with dashes.
!
  s1 = '0-306-40615-2'
  value1 = .true.
  value2 = isbn_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) &
    '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1
!
!  Modify one digit.
!
  s1 = '0-326-40615-2'
  value1 = .false.
  value2 = isbn_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) &
    '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1
!
!  Supply a valid code, with spaces.
!
  s1 = '0 8493 9640 9'
  value1 = .true.
  value2 = isbn_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) &
    '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1
!
!  Modify the check digit.
!
  s1 = '0 8493 9640 3'
  value1 = .false.
  value2 = isbn_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) &
    '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1
!
!  Supply a valid code, with a final digit of 'X'.
!
  s1 = '0-3870-9654-X'
  value1 = .true.
  value2 = isbn_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) &
    '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1
!
!  Supply a valid code, with a final digit of 'x'.
!
  s1 = '0-201-38597-x'
  value1 = .true.
  value2 = isbn_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) &
    '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1

  return
end
subroutine s_to_digits_test ( )

!*****************************************************************************80
!
!! S_TO_DIGITS_TEST tests S_TO_DIGITS.
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
  implicit none

  integer ( kind = 4 ) dvec(20)
  integer ( kind = 4 ) n
  character ( len = 255 ) s

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'S_TO_DIGITS_TEST'
  write ( *, '(a)' ) '  S_TO_DIGITS: string -> digit vector'

  s = '34E94-70.6'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test string: "' // trim ( s ) // '"'
  n = 5
  call s_to_digits ( s, n, dvec )
  call i4vec_print ( n, dvec, '  Extracted 5 digits:' )

  s = '34E94-70.6'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test string: "' // trim ( s ) // '"'
  n = 7
  call s_to_digits ( s, n, dvec )
  call i4vec_print ( n, dvec, '  Extracted 7 digits:' )

  return
end
subroutine s_to_isbn_digits_test ( )

!*****************************************************************************80
!
!! S_TO_ISBN_DIGITS_TEST tests S_TO_ISBN_DIGITS.
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
  implicit none

  integer ( kind = 4 ) dvec(20)
  integer ( kind = 4 ) n
  character ( len = 255 ) s

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'S_TO_ISBN_DIGITS_TEST'
  write ( *, '(a)' ) '  S_TO_ISBN_DIGITS: string -> ISBN digit vector'

  s = '34E9X-70.6'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test string: "' // trim ( s ) // '"'
  n = 5
  call s_to_isbn_digits ( s, n, dvec )
  call i4vec_print ( n, dvec, '  Extracted 5 digits:' )

  s = '34E9X-70.6'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test string: "' // trim ( s ) // '"'
  n = 7
  call s_to_isbn_digits ( s, n, dvec )
  call i4vec_print ( n, dvec, '  Extracted 7 digits:' )

  return
end

