program main

!*****************************************************************************80
!
!! MAIN is the main program for UPC_TEST.
!
!  Discussion:
!
!    UPC_TEST tests the UPC library.
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
  write ( *, '(a)' ) 'UPC_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the UPC library.'

  call ch_is_digit_test ( )
  call ch_to_digit_test ( )
  call s_to_digits_test ( )
  call upc_check_digit_calculate_test ( )
  call upc_is_valid_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UPC_TEST'
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
subroutine upc_check_digit_calculate_test ( )

!*****************************************************************************80
!
!! UPC_CHECK_DIGIT_CALCULATE_TEST tests UPC_CHECK_DIGIT_CALCULATE.
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

  integer ( kind = 4 ) d1
  integer ( kind = 4 ) d2
  character ( len = 255 ) s1
  integer ( kind = 4 ) upc_check_digit_calculate

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UPC_CHECK_DIGIT_CALCULATE_TEST'
  write ( *, '(a)' ) '  UPC_CHECK_DIGIT_CALCULATE calculates the 12-th digit'
  write ( *, '(a)' ) '  (the check digit) of a UPC.'
  write ( *, '(a)' ) ''
!
!  Supply the full code, with dashes.
!
  s1 = '6-39382-00039-3'
  d1 = 3
  d2 = upc_check_digit_calculate ( s1 )
  write ( *, '(a,i1,a,i1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', d2, ', expecting ', d1
!
!  Supply a partial code, with spaces.
!
  s1 = '0 43000 18170'
  d1 = 6
  d2 = upc_check_digit_calculate ( s1 )
  write ( *, '(a,i1,a,i1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', d2, ', expecting ', d1
!
!  Supply a partial code, no spaces.
!
  s1 = '30074660601'
  d1 = 7
  d2 = upc_check_digit_calculate ( s1 )
  write ( *, '(a,i1,a,i1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', d2, ', expecting ', d1
!
!  Supply a partial code, no spaces.
!
  s1 = '24689753124'
  d1 = 5
  d2 = upc_check_digit_calculate ( s1 )
  write ( *, '(a,i1,a,i1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', d2, ', expecting ', d1
!
!  Supply a partial code, no spaces.
!
  s1 = '13579864213'
  d1 = 9
  d2 = upc_check_digit_calculate ( s1 )
  write ( *, '(a,i1,a,i1)' ) '  Check digit of "' // trim ( s1 ) // '" is ', d2, ', expecting ', d1

  return
end
subroutine upc_is_valid_test ( )

!*****************************************************************************80
!
!! UPC_IS_VALID_TEST tests UPC_IS_VALID.
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

  character ( len = 255 ) s1
  logical ( kind = 4 ) upc_is_valid
  logical ( kind = 4 ) value1
  logical ( kind = 4 ) value2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UPC_IS_VALID_TEST'
  write ( *, '(a)' ) '  UPC_IS_VALID reports whether a UPC is valid.'
  write ( *, '(a)' ) ''
!
!  Supply a valid code, with dashes.
!
  s1 = '6-39382-00039-3'
  value1 = .true.
  value2 = upc_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1
!
!  Modify one digit.
!
  s1 = '6-39352-00039-3'
  value1 = .false.
  value2 = upc_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1
!
!  Supply a valid code, with spaces.
!
  s1 = '0 43000 18170 6'
  value1 = .true.
  value2 = upc_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1
!
!  Modify the check digit.
!
  s1 = '0 43000 18170 9'
  value1 = .false.
  value2 = upc_is_valid ( s1 )
  write ( *, '(a,l1,a,l1)' ) '  Validity of "' // trim ( s1 ) // '" is ', value2, ', expecting ', value1

  return
end
