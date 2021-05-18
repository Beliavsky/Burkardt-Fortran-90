program main

!*****************************************************************************80
!
!! MAIN is the main program for LUHN_TEST.
!
!  Discussion:
!
!    LUHN_TEST tests the LUHN library.
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

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LUHN_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LUHN library.'

  call ch_is_digit_test ( )
  call ch_to_digit_test ( )
  call s_digits_count_test ( )
  call s_to_digits_test ( )
  call luhn_check_digit_calculate_test ( )
  call luhn_is_valid_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LUHN_TEST'
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
    write ( *, '(2x,a,2x,l1)' ) '"' // c // '"', value
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
  integer ( kind = 4 ) d
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CH_TO_DIGIT_TEST'
  write ( *, '(a)' ) '  CH_TO_DIGIT: character -> decimal digit'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   C      CH_TO_DIGIT(C)'
  write ( *, '(a)' ) ''

  do i = 1, 13
    c = c_test(i)
    call ch_to_digit ( c, d )
    write ( *, '(2x,a,2x,i8)' ) '"' // c // '"', d
  end do

  return
end
subroutine luhn_check_digit_calculate_test ( )

!*****************************************************************************80
!
!! LUHN_CHECK_DIGIT_CALCULATE_TEST tests LUHN_CHECK_DIGIT_CALCULATE.
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
  integer ( kind = 4 ) d1
  integer ( kind = 4 ) d2
  integer ( kind = 4 ) luhn_check_digit_calculate
  character ( len = 255 ) s1
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LUHN_CHECK_DIGIT_CALCULATE_TEST'
  write ( *, '(a)' ) '  LUHN_CHECK_DIGIT_CALCULATE calculates the check digit'
  write ( *, '(a)' ) '  for a string of digits'
  write ( *, '(a)' ) ''

  s1 = '7992739871'
  d1 = 3
  d2 = luhn_check_digit_calculate ( s1 )
  write ( *, '(a,i1,a,i1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', d2, ', expecting ', d1

  s1 = '9876234510'
  d1 = 0
  d2 = luhn_check_digit_calculate ( s1 )
  write ( *, '(a,i1,a,i1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', d2, ', expecting ', d1

  s1 = '246897531'
  d1 = 9
  d2 = luhn_check_digit_calculate ( s1 )
  write ( *, '(a,i1,a,i1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', d2, ', expecting ', d1

  s1 = '135798642'
  d1 = 9
  d2 = luhn_check_digit_calculate ( s1 )
  write ( *, '(a,i1,a,i1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', d2, ', expecting ', d1

  return
end
subroutine luhn_is_valid_test ( )

!*****************************************************************************80
!
!! LUHN_IS_VALID_TEST tests LUHN_IS_VALID.
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
  logical ( kind = 4 ) luhn_is_valid
  character ( len = 255 ) s1
  logical ( kind = 4 ) value1
  logical ( kind = 4 ) value2
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LUHN_IS_VALID_TEST'
  write ( *, '(a)' ) '  LUHN_IS_VALID determines whether a string with final'
  write ( *, '(a)' ) '  Luhn check digit is valid.'
  write ( *, '(a)' ) ''

  s1 = '79927398713'
  value1 = .true.
  value2 = luhn_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1
!
!  Change a digit.
!
  s1 = '79924398713'
  value1 = .false.
  value2 = luhn_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1
!
!  Change a check digit.
!
  s1 = '79927398711'
  value1 = .false.
  value2 = luhn_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1

  return
end
subroutine s_digits_count_test ( )

!*****************************************************************************80
!
!! S_DIGITS_COUNT_TEST tests S_DIGITS_COUNT.
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

  integer ( kind = 4 ) n
  character ( len = 255 ) s
  integer ( kind = 4 ) s_digits_count

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'S_DIGITS_COUNT_TEST'
  write ( *, '(a)' ) '  S_DIGITS_COUNT counts the digits in a string.'

  s = '34E94-70.6'
  n = s_digits_count ( s )
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a)' ) '  We count ', n, ' digits in "' // trim ( s ) // '"'

  s = 'Not a one!'
  n = s_digits_count ( s )
  write ( *, '(a,i2,a)' ) '  We count ', n, ' digits in "' // trim ( s ) // '"'

  s = '!8*k >>>> & SEVEN-0.3'
  n = s_digits_count ( s )
  write ( *, '(a,i2,a)' ) '  We count ', n, ' digits in "' // trim ( s ) // '"'

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
