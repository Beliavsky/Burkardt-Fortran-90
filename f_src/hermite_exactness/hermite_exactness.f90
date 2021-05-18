program main

!*****************************************************************************80
!
!! MAIN is the main program for HERMITE_EXACTNESS.
!
!  Discussion:
!
!    This program investigates a standard Gauss-Hermite quadrature rule
!    by using it to integrate monomials over (-oo,+oo), and comparing the
!    approximate result to the known exact value.
!
!    The user specifies:
!    * the "root" name of the R, W and X files that specify the rule;
!    * DEGREE_MAX, the maximum monomial degree to be checked.
!    * the OPTION (unweighted/physicist weight/probabilist weight)
!
!    OPTION indicates the weight function and normalization:
!    0, Integral ( -oo < x < +oo ) x^n exp(-x*x)               dx.
!    1, Integral ( -oo < x < +oo ) x^n exp(-x*x)               dx.
!    2, Integral ( -oo < x < +oo ) x^n exp(-x*x/2)             dx.
!    3, Integral ( -oo < x < +oo ) x^n exp(-x*x)   / sqrt (pi) dx.
!    4, Integral ( -oo < x < +oo ) x^n exp(-x*x/2) / sqrt(2pi) dx.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 August 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) arg_num
  integer ( kind = 4 ) degree
  integer ( kind = 4 ) degree_max
  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) dim_num2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iarg
  integer ( kind = 4 ) iargc
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) last
  integer ( kind = 4 ) option
  integer ( kind = 4 ) order
  integer ( kind = 4 ) point_num
  real ( kind = 8 ) quad_error
  character ( len = 255 ) quad_filename
  character ( len = 255 ) quad_r_filename
  character ( len = 255 ) quad_w_filename
  character ( len = 255 ) quad_x_filename
  real ( kind = 8 ), allocatable, dimension ( : ) :: r
  character ( len = 255 ) string
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HERMITE_EXACTNESS'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Investigate the polynomial exactness of a Gauss-Hermite'
  write ( *, '(a)' ) '  quadrature rule by integrating weighted '
  write ( *, '(a)' ) &
    '  monomials up to a given degree over the (-oo,+oo) interval.'
!
!  Get the number of command line arguments.
!
  arg_num = iargc ( )
!
!  Get the quadrature file root name:
!
  if ( 1 <= arg_num ) then

    iarg = 1
    call getarg ( iarg, quad_filename )

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HERMITE_EXACTNESS:'
    write ( *, '(a)' ) '  Enter the "root" name of the quadrature files.'

    read ( *, '(a)' ) quad_filename

  end if
!
!  Create the names of:
!    the quadrature X file;
!    the quadrature W file;
!    the quadrature R file;
!
  quad_x_filename = trim ( quad_filename ) // '_x.txt'
  quad_w_filename = trim ( quad_filename ) // '_w.txt'
  quad_r_filename = trim ( quad_filename ) // '_r.txt'
!
!  The second command line argument is the maximum degree.
!
  if ( 2 <= arg_num ) then

    iarg = 2
    call getarg ( iarg, string )
    call s_to_i4 ( string, degree_max, ierror, last )

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HERMITE_EXACTNESS:'
    write ( *, '(a)' ) '  Please enter the maximum degree to check.'

    read ( *, * ) degree_max

  end if
!
!  The third command line argument is OPTION.
!
  if ( 3 <= arg_num ) then

    iarg = 3
    call getarg ( iarg, string )
    call s_to_i4 ( string, option, ierror, last )

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HERMITE_EXACTNESS:'
    write ( *, '(a)' ) '  OPTION specifies the weight function rho(x):'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  0, rho(x) = 1'
    write ( *, '(a)' ) '  1, rho(x) = exp ( -x*x  )'
    write ( *, '(a)' ) '  2, rho(x) = exp ( -x*x/2)'
    write ( *, '(a)' ) '  3, rho(x) = exp ( -x*x  ) / sqrt( pi)'
    write ( *, '(a)' ) '  4, rho(x) = exp ( -x*x/2) / sqrt(2pi)'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Please enter OPTION:'

    read ( *, * ) option

  end if

  if ( option < 0 .or. 4 < option ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HERMITE_EXACTNESS - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of OPTION.'
    stop 1
  end if
!
!  Summarize the input.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HERMITE_EXACTNESS: User input:'
  write ( *, '(a)' ) '  Quadrature rule X file = "' &
    // trim ( quad_x_filename ) // '".'
  write ( *, '(a)' ) '  Quadrature rule W file = "' &
    // trim ( quad_w_filename ) // '".'
  write ( *, '(a)' ) '  Quadrature rule R file = "' &
    // trim ( quad_r_filename ) // '".'
  write ( *, '(a,i8)' ) '  Maximum degree to check = ', &
    degree_max
!
!  Read the X file.
!
  call r8mat_header_read ( quad_x_filename, dim_num, order )

  if ( dim_num /= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HERMITE_EXACTNESS - Fatal error!'
    write ( *, '(a)' ) '  The spatial dimension should be 1.'
    write ( *, '(a,i8)' ) &
      '  The implicit input dimension was DIM_NUM = ', dim_num
    stop 1
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension = ', dim_num
  write ( *, '(a,i8)' ) '  Number of points  = ', order

  allocate ( x(order) )

  call r8mat_data_read ( quad_x_filename, dim_num, order, x )
!
!  Read the W file.
!
  call r8mat_header_read ( quad_w_filename, dim_num2, point_num )

  if ( dim_num2 /= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HERMITE_EXACTNESS - Fatal error!'
    write ( *, '(a)' ) '  The quadrature weight file should have exactly'
    write ( *, '(a)' ) '  one value on each line.'
    stop 1
  end if

  if ( point_num /= order ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HERMITE_EXACTNESS - Fatal error!'
    write ( *, '(a)' ) '  The quadrature weight file should have exactly'
    write ( *, '(a)' ) '  the same number of lines as the abscissa file.'
    stop 1
  end if

  allocate ( w(order) )

  call r8mat_data_read ( quad_w_filename, 1, order, w )
!
!  Read the R file.
!
  call r8mat_header_read ( quad_r_filename, dim_num2, point_num )

  if ( dim_num2 /= dim_num ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HERMITE_EXACTNESS - Fatal error!'
    write ( *, '(a)' ) '  The quadrature region file should have the'
    write ( *, '(a)' ) '  same number of values on each line as the'
    write ( *, '(a)' ) '  abscissa file does.'
    stop 1
  end if

  if ( point_num /= 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HERMITE_EXACTNESS - Fatal error!'
    write ( *, '(a)' ) '  The quadrature region file should have two lines.'
    stop 1
  end if

  allocate ( r(2) )

  call r8mat_data_read ( quad_r_filename, dim_num, 2, r )
!
!  Print the input quadrature rule.
!
  write ( *, '(a)'       ) ' '
  write ( *, '(a)'       ) '  The quadrature rule to be tested is'
  write ( *, '(a)'       ) '  a Gauss-Hermite rule'
  write ( *, '(a,i8)'    ) '  of ORDER = ', order
  write ( *, '(a)'       ) ' '
  if ( option == 0 ) then
    write ( *, '(a)' ) '  OPTION = 0, the unweighted rule for:'
    write ( *, '(a)' ) '  Integral ( -oo < x < +oo ) f(x) dx'
  else if ( option == 1 ) then
    write ( *, '(a)' ) '  OPTION = 1, the physicist weighted rule for:'
    write ( *, '(a)' ) '  Integral ( -oo < x < +oo ) f(x) * exp(-x*x) dx'
  else if ( option == 2 ) then
    write ( *, '(a)' ) '  OPTION = 2, the probabilist weighted rule for:'
    write ( *, '(a)' ) '  Integral ( -oo < x < +oo ) f(x) * exp(-x*x/2) dx'
  else if ( option == 3 ) then
    write ( *, '(a)' ) &
      '  OPTION = 3, the physicist normalized weighted rule for:'
    write ( *, '(a)' ) &
      '  Integral ( -oo < x < +oo ) f(x) * exp(-x*x)/sqrt(pi) dx'
  else if ( option == 4 ) then
    write ( *, '(a)' ) &
      '  OPTION = 4, the probabilist normalized weighted rule for:'
    write ( *, '(a)' ) &
      '  Integral ( -oo < x < +oo ) f(x) * exp(-x*x/2)/sqrt(2pi) dx'
  end if
  write ( *, '(a)'       ) ' '
  write ( *, '(a)'       ) '  Weights W:'
  write ( *, '(a)'       ) ' '
  do i = 1, order
    write ( *, '(a,i2,a,g24.16)' ) '  w(', i, ') = ', w(i)
  end do
  write ( *, '(a)'       ) ' '
  write ( *, '(a)'       ) '  Abscissas X:'
  write ( *, '(a)'       ) ' '
  do i = 1, order
    write ( *, '(a,i2,a,g24.16)' ) '  x(', i, ') = ', x(i)
  end do
  write ( *, '(a)'       ) ' '
  write ( *, '(a)'       ) '  Region R:'
  write ( *, '(a)'       ) ' '

  do i = 1, 2
    write ( *, '(a,i2,a,g24.16)' ) '  r(', i, ') = ', r(i)
  end do
!
!  Explore the monomials.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  A Gauss-Hermite rule would be able to exactly'
  write ( *, '(a,i8)' ) &
    '  integrate monomials up to and including degree = ', 2 * order - 1
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Degree          Error'
  write ( *, '(a)' ) ' '

  do degree = 0, degree_max

    call hermite_monomial_quadrature ( degree, order, option, w, x, quad_error )

    write ( *, '(2x,i2,2x,f24.16)' ) degree, quad_error

  end do
!
!  Free memory.
!
  deallocate ( w )
  deallocate ( x )
  deallocate ( r )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HERMITE_EXACTNESS:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine ch_cap ( c )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character C, the character to capitalize.
!
  implicit none

  character c
  integer ( kind = 4 ) itemp

  itemp = ichar ( c )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if

  return
end
function ch_eqi ( c1, c2 )

!*****************************************************************************80
!
!! CH_EQI is a case insensitive comparison of two characters for equality.
!
!  Example:
!
!    CH_EQI ( 'A', 'a' ) is .TRUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C1, C2, the characters to compare.
!
!    Output, logical ( kind = 4 ) CH_EQI, the result of the comparison.
!
  implicit none

  logical ( kind = 4 ) ch_eqi
  character c1
  character c1_cap
  character c2
  character c2_cap

  c1_cap = c1
  c2_cap = c2

  call ch_cap ( c1_cap )
  call ch_cap ( c2_cap )

  if ( c1_cap == c2_cap ) then
    ch_eqi = .true.
  else
    ch_eqi = .false.
  end if

  return
end
subroutine ch_to_digit ( c, digit )

!*****************************************************************************80
!
!! CH_TO_DIGIT returns the value of a base 10 digit.
!
!  Example:
!
!     C   DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    ...  ...
!    '9'    9
!    ' '    0
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
!    Input, character C, the decimal digit, '0' through '9' or blank
!    are legal.
!
!    Output, integer ( kind = 4 ) DIGIT, the corresponding value.  
!    If C was 'illegal', then DIGIT is -1.
!
  implicit none

  character c
  integer ( kind = 4 ) digit

  if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

    digit = ichar ( c ) - 48

  else if ( c == ' ' ) then

    digit = 0

  else

    digit = -1

  end if

  return
end
subroutine file_column_count ( input_file_name, column_num )

!*****************************************************************************80
!
!! FILE_COLUMN_COUNT counts the number of columns in the first line of a file.
!
!  Discussion:
!
!    The file is assumed to be a simple text file.
!
!    Most lines of the file is presumed to consist of COLUMN_NUM words,
!    separated by spaces.  There may also be some blank lines, and some 
!    comment lines,
!    which have a "#" in column 1.
!
!    The routine tries to find the first non-comment non-blank line and
!    counts the number of words in that line.
!
!    If all lines are blanks or comments, it goes back and tries to analyze
!    a comment line.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) INPUT_FILE_NAME, the name of the file.
!
!    Output, integer ( kind = 4 ) COLUMN_NUM, the number of columns in the file.
!
  implicit none

  integer ( kind = 4 ) column_num
  logical ( kind = 4 ) got_one
  character ( len = * ) input_file_name
  integer ( kind = 4 ) input_status
  integer ( kind = 4 ) input_unit
  character ( len = 255 ) line
!
!  Open the file.
!
  call get_unit ( input_unit )

  open ( unit = input_unit, file = input_file_name, status = 'old', &
    form = 'formatted', access = 'sequential', iostat = input_status )

  if ( input_status /= 0 ) then
    column_num = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILE_COLUMN_COUNT - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the input file "' &
      // trim ( input_file_name ) // '" on unit ', input_unit
    return
  end if
!
!  Read one line, but skip blank lines and comment lines.
!
  got_one = .false.

  do

    read ( input_unit, '(a)', iostat = input_status ) line

    if ( input_status /= 0 ) then
      exit
    end if

    if ( len_trim ( line ) == 0 ) then
      cycle
    end if

    if ( line(1:1) == '#' ) then
      cycle
    end if

    got_one = .true.
    exit

  end do

  if ( .not. got_one ) then

    rewind ( input_unit )

    do

      read ( input_unit, '(a)', iostat = input_status ) line

      if ( input_status /= 0 ) then
        exit
      end if

      if ( len_trim ( line ) == 0 ) then
        cycle
      end if

      got_one = .true.
      exit

    end do

  end if

  close ( unit = input_unit )

  if ( .not. got_one ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILE_COLUMN_COUNT - Warning!'
    write ( *, '(a)' ) '  The file does not seem to contain any data.'
    column_num = -1
    return
  end if

  call s_word_count ( line, column_num )

  return
end
subroutine file_row_count ( input_file_name, row_num )

!*****************************************************************************80
!
!! FILE_ROW_COUNT counts the number of row records in a file.
!
!  Discussion:
!
!    It does not count lines that are blank, or that begin with a
!    comment symbol '#'.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) INPUT_FILE_NAME, the name of the input file.
!
!    Output, integer ( kind = 4 ) ROW_NUM, the number of rows found.
!
  implicit none

  integer ( kind = 4 ) bad_num
  integer ( kind = 4 ) comment_num
  integer ( kind = 4 ) ierror
  character ( len = * ) input_file_name
  integer ( kind = 4 ) input_status
  integer ( kind = 4 ) input_unit
  character ( len = 255 ) line
  integer ( kind = 4 ) record_num
  integer ( kind = 4 ) row_num

  call get_unit ( input_unit )

  open ( unit = input_unit, file = input_file_name, status = 'old', &
    iostat = input_status )

  if ( input_status /= 0 ) then
    row_num = -1;
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILE_ROW_COUNT - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the input file "' // &
      trim ( input_file_name ) // '" on unit ', input_unit
    stop 1
  end if

  comment_num = 0
  row_num = 0
  record_num = 0
  bad_num = 0

  do

    read ( input_unit, '(a)', iostat = input_status ) line

    if ( input_status /= 0 ) then
      ierror = record_num
      exit
    end if

    record_num = record_num + 1

    if ( line(1:1) == '#' ) then
      comment_num = comment_num + 1
      cycle
    end if

    if ( len_trim ( line ) == 0 ) then
      comment_num = comment_num + 1
      cycle
    end if

    row_num = row_num + 1

  end do

  close ( unit = input_unit )

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
subroutine hermite_integral ( n, option, value )

!*****************************************************************************80
!
!! HERMITE_INTEGRAL evaluates a monomial Hermite integral.
!
!  Discussion:
!
!    H(n,1) = Integral ( -oo < x < +oo ) x^n exp(-x^2) dx
!    H(n,1) is 0 for n odd.
!    H(n,1) = (n-1)!! * sqrt(pi) / 2^(n/2) for n even.
!
!    H(n,2) = Integral ( -oo < x < +oo ) x^n exp(-x^2/2) dx
!    H(n,2) is 0 for n odd.
!    H(n,2) = (n-1)!! * sqrt(2*pi) for n even.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the integral.  
!    0 <= N.
!
!    Input, integer ( kind = 4 ) OPTION, the integral has the form:
!    0, Integral ( -oo < x < +oo ) x^n exp(-x*x)               dx.
!    1, Integral ( -oo < x < +oo ) x^n exp(-x*x)               dx.
!    2, Integral ( -oo < x < +oo ) x^n exp(-x*x/2)             dx.
!    3, Integral ( -oo < x < +oo ) x^n exp(-x*x)   / sqrt (pi) dx.
!    4, Integral ( -oo < x < +oo ) x^n exp(-x*x/2) / sqrt(2pi) dx.
!
!    Output, real ( kind = 8 ) VALUE, the value of the integral.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) option
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) value

  if ( n < 0 ) then

    value = - huge ( value )

  else if ( mod ( n, 2 ) == 1 ) then

    value = 0.0D+00

  else if ( option == 0 ) then

    value = r8_factorial2 ( n - 1 ) * sqrt ( r8_pi ) / 2.0D+00 ** ( n / 2 )

  else if ( option == 1 ) then

    value = r8_factorial2 ( n - 1 ) * sqrt ( r8_pi ) / 2.0D+00 ** ( n / 2 )

  else if ( option == 2 ) then

    value = r8_factorial2 ( n - 1 ) * sqrt ( 2.0D+00 * r8_pi )

  else if ( option == 3 ) then

    value = r8_factorial2 ( n - 1 ) / 2.0D+00 ** ( n / 2 )

  else if ( option == 4 ) then

    value = r8_factorial2 ( n - 1 )
  end if

  return
end
subroutine hermite_monomial_quadrature ( expon, order, option, w, x, &
  quad_error )

!*****************************************************************************80
!
!! HERMITE_MONOMIAL_QUADRATURE applies a quadrature rule to a monomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 June 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) EXPON, the exponent.
!
!    Input, integer ( kind = 4 ) ORDER, the number of points in the rule.
!
!    Input, integer ( kind = 4 ) OPTION, the integral has the form:
!    0, Integral ( -oo < x < +oo ) x^n exp(-x*x)               dx.
!    1, Integral ( -oo < x < +oo ) x^n exp(-x*x)               dx.
!    2, Integral ( -oo < x < +oo ) x^n exp(-x*x/2)             dx.
!    3, Integral ( -oo < x < +oo ) x^n exp(-x*x)   / sqrt (pi) dx.
!    4, Integral ( -oo < x < +oo ) x^n exp(-x*x/2) / sqrt(2pi) dx.
!
!    Input, real ( kind = 8 ) W(ORDER), the quadrature weights.
!
!    Input, real ( kind = 8 ) X(ORDER), the quadrature points.
!
!    Output, real ( kind = 8 ) QUAD_ERROR, the quadrature error.
!
  implicit none

  integer ( kind = 4 ) order

  real ( kind = 8 ) exact
  integer ( kind = 4 ) expon
  integer ( kind = 4 ) option
  real ( kind = 8 ) quad
  real ( kind = 8 ) quad_error
  real ( kind = 8 ) value(order)
  real ( kind = 8 ) w(order)
  real ( kind = 8 ) x(order)
!
!  Get the exact value of the integral of the unscaled monomial.
!
  call hermite_integral ( expon, option, exact )
!
!  Evaluate the unweighted monomial at the quadrature points.
!
  if ( option == 0 ) then
    value(1:order) = exp ( - x(1:order)**2 ) * x(1:order)**expon
  else if ( option == 1 ) then
    value(1:order) = x(1:order)**expon
  else if ( option == 2 ) then
    value(1:order) = x(1:order)**expon
  else if ( option == 3 ) then
    value(1:order) = x(1:order)**expon
  else if ( option == 4 ) then
    value(1:order) = x(1:order)**expon
  end if
!
!  Compute the weighted sum.
!
  quad = dot_product ( w, value )
!
!  Absolute error for cases where exact integral is zero,
!  Relative error otherwise.
!
  if ( exact == 0.0D+00 ) then
    quad_error = abs ( quad )
  else
    quad_error = abs ( quad - exact ) / abs ( exact )
  end if

  return
end
function r8_factorial2 ( n )

!*****************************************************************************80
!
!! R8_FACTORIAL2 computes the double factorial function.
!
!  Discussion:
!
!    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
!                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
!
!  Example:
!
!     N    N!!
!
!     0     1
!     1     1
!     2     2
!     3     3
!     4     8
!     5    15
!     6    48
!     7   105
!     8   384
!     9   945
!    10  3840
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 September 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the argument of the double factorial 
!    function.  If N is less than 1, R8_FACTORIAL2 is returned as 1.0.
!
!    Output, real ( kind = 8 ) R8_FACTORIAL2, the value of N!!.
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ) r8_n
  real ( kind = 8 ) value

  if ( n < 1 ) then

    value = 1.0D+00

  else

    r8_n = real ( n, kind = 8 )
    value = 1.0D+00

    do while ( 1.0D+00 < r8_n )
      value = value * r8_n
      r8_n = r8_n - 2.0D+00
    end do

  end if

  r8_factorial2 = value

  return
end
subroutine r8mat_data_read ( input_filename, m, n, table )

!*****************************************************************************80
!
!! R8MAT_DATA_READ reads data from an R8MAT file.
!
!  Discussion:
!
!    The file may contain more than N points, but this routine will
!    return after reading N of them.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) INPUT_FILENAME, the name of the input file.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Output, real ( kind = 8 ) TABLE(M,N), the table data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) ierror
  character ( len = * ) input_filename
  integer ( kind = 4 ) input_status
  integer ( kind = 4 ) input_unit
  integer ( kind = 4 ) j
  character ( len = 255 ) line
  real ( kind = 8 ) table(m,n)
  real ( kind = 8 ) x(m)

  ierror = 0

  call get_unit ( input_unit )

  open ( unit = input_unit, file = input_filename, status = 'old', &
    iostat = input_status )

  if ( input_status /= 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_DATA_READ - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the input file "' // &
      trim ( input_filename ) // '" on unit ', input_unit
    stop 1
  end if

  j = 0

  do while ( j < n )

    read ( input_unit, '(a)', iostat = input_status ) line

    if ( input_status /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_DATA_READ - Fatal error!'
      write ( *, '(a)' ) '  Error while reading lines of data.'
      write ( *, '(a,i8)' ) '  Number of values expected per line M = ', m
      write ( *, '(a,i8)' ) '  Number of data lines read, J =         ', j
      write ( *, '(a,i8)' ) '  Number of data lines needed, N =       ', n
      stop 1
    end if

    if ( line(1:1) == '#' .or. len_trim ( line ) == 0 ) then
      cycle
    end if

    call s_to_r8vec ( line, m, x, ierror )

    if ( ierror /= 0 ) then
      cycle
    end if

    j = j + 1

    table(1:m,j) = x(1:m)

  end do

  close ( unit = input_unit )

  return
end
subroutine r8mat_header_read ( input_filename, m, n )

!*****************************************************************************80
!
!! R8MAT_HEADER_READ reads the header from an R8MAT file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) INPUT_FILENAME, the name of the input file.
!
!    Output, integer ( kind = 4 ) M, spatial dimension.
!
!    Output, integer ( kind = 4 ) N, the number of points.
!
  implicit none

  character ( len = * ) input_filename
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  call file_column_count ( input_filename, m )

  if ( m <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_HEADER_READ - Fatal error!'
    write ( *, '(a)' ) '  There was some kind of I/O problem while trying'
    write ( *, '(a)' ) '  to count the number of data columns in'
    write ( *, '(a)' ) '  the file "' // trim ( input_filename ) // '".'
    stop 1
  end if

  call file_row_count ( input_filename, n )

  if ( n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_HEADER_READ - Fatal error!'
    write ( *, '(a)' ) '  There was some kind of I/O problem while trying'
    write ( *, '(a)' ) '  to count the number of data rows in'
    write ( *, '(a)' ) '  the file "' // trim ( input_filename ) // '".'
    stop 1
  end if

  return
end
subroutine s_to_i4 ( s, ival, ierror, length )

!*****************************************************************************80
!
!! S_TO_I4 reads an I4 from a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be examined.
!
!    Output, integer ( kind = 4 ) IVAL, the integer value read from the string.
!    If the string is blank, then IVAL will be returned 0.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag.
!    0, no error.
!    1, an error occurred.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters of S 
!    used to make IVAL.
!
  implicit none

  character c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) istate
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) length
  character ( len = * ) s

  ierror = 0
  istate = 0
  isgn = 1
  ival = 0

  do i = 1, len_trim ( s )

    c = s(i:i)
!
!  Haven't read anything.
!
    if ( istate == 0 ) then

      if ( c == ' ' ) then

      else if ( c == '-' ) then
        istate = 1
        isgn = -1
      else if ( c == '+' ) then
        istate = 1
        isgn = + 1
      else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        istate = 2
        ival = ichar ( c ) - ichar ( '0' )
      else
        ierror = 1
        return
      end if
!
!  Have read the sign, expecting digits.
!
    else if ( istate == 1 ) then

      if ( c == ' ' ) then

      else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        istate = 2
        ival = ichar ( c ) - ichar ( '0' )
      else
        ierror = 1
        return
      end if
!
!  Have read at least one digit, expecting more.
!
    else if ( istate == 2 ) then

      if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        ival = 10 * ival + ichar ( c ) - ichar ( '0' )
      else
        ival = isgn * ival
        length = i - 1
        return
      end if

    end if

  end do
!
!  If we read all the characters in the string, see if we're OK.
!
  if ( istate == 2 ) then
    ival = isgn * ival
    length = len_trim ( s )
  else
    ierror = 1
    length = 0
  end if

  return
end
subroutine s_to_r8 ( s, dval, ierror, length )

!*****************************************************************************80
!
!! S_TO_R8 reads an R8 from a string.
!
!  Discussion:
!
!    The routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the number.
!
!    Legal input is:
!
!       1 blanks,
!       2 '+' or '-' sign,
!       2.5 blanks
!       3 integer part,
!       4 decimal point,
!       5 fraction part,
!       6 'E' or 'e' or 'D' or 'd', exponent marker,
!       7 exponent sign,
!       8 exponent integer part,
!       9 exponent decimal point,
!      10 exponent fraction part,
!      11 blanks,
!      12 final comma or semicolon,
!
!    with most quantities optional.
!
!  Example:
!
!    S                 DVAL
!
!    '1'               1.0
!    '     1   '       1.0
!    '1A'              1.0
!    '12,34,56'        12.0
!    '  34 7'          34.0
!    '-1E2ABCD'        -100.0
!    '-1X2ABCD'        -1.0
!    ' 2E-1'           0.2
!    '23.45'           23.45
!    '-4.2E+2'         -420.0
!    '17d2'            1700.0
!    '-14e-2'         -0.14
!    'e2'              100.0
!    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal real.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, real ( kind = 8 ) DVAL, the value read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    1, 2, 6 or 7, the input number was garbled.  The
!    value of IERROR is the last type of input successfully
!    read.  For instance, 1 means initial blanks, 2 means
!    a plus or minus sign, and so on.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read
!    to form the number, including any terminating
!    characters such as a trailing comma or blanks.
!
  implicit none

  character c
  logical ch_eqi
  real ( kind = 8 ) dval
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ihave
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) iterm
  integer ( kind = 4 ) jbot
  integer ( kind = 4 ) jsgn
  integer ( kind = 4 ) jtop
  integer ( kind = 4 ) length
  integer ( kind = 4 ) nchar
  integer ( kind = 4 ) ndig
  real ( kind = 8 ) rbot
  real ( kind = 8 ) rexp
  real ( kind = 8 ) rtop
  character ( len = * ) s

  nchar = len_trim ( s )

  ierror = 0
  dval = 0.0D+00
  length = -1
  isgn = 1
  rtop = 0
  rbot = 1
  jsgn = 1
  jtop = 0
  jbot = 1
  ihave = 1
  iterm = 0

  do

    length = length + 1

    if ( nchar < length+1 ) then
      exit
    end if

    c = s(length+1:length+1)
!
!  Blank character.
!
    if ( c == ' ' ) then

      if ( ihave == 2 ) then

      else if ( ihave == 6 .or. ihave == 7 ) then
        iterm = 1
      else if ( 1 < ihave ) then
        ihave = 11
      end if
!
!  Comma.
!
    else if ( c == ',' .or. c == ';' ) then

      if ( ihave /= 1 ) then
        iterm = 1
        ihave = 12
        length = length + 1
      end if
!
!  Minus sign.
!
    else if ( c == '-' ) then

      if ( ihave == 1 ) then
        ihave = 2
        isgn = -1
      else if ( ihave == 6 ) then
        ihave = 7
        jsgn = -1
      else
        iterm = 1
      end if
!
!  Plus sign.
!
    else if ( c == '+' ) then

      if ( ihave == 1 ) then
        ihave = 2
      else if ( ihave == 6 ) then
        ihave = 7
      else
        iterm = 1
      end if
!
!  Decimal point.
!
    else if ( c == '.' ) then

      if ( ihave < 4 ) then
        ihave = 4
      else if ( 6 <= ihave .and. ihave <= 8 ) then
        ihave = 9
      else
        iterm = 1
      end if
!
!  Scientific notation exponent marker.
!
    else if ( ch_eqi ( c, 'E' ) .or. ch_eqi ( c, 'D' ) ) then

      if ( ihave < 6 ) then
        ihave = 6
      else
        iterm = 1
      end if
!
!  Digit.
!
    else if (  ihave < 11 .and. lle ( '0', c ) .and. lle ( c, '9' ) ) then

      if ( ihave <= 2 ) then
        ihave = 3
      else if ( ihave == 4 ) then
        ihave = 5
      else if ( ihave == 6 .or. ihave == 7 ) then
        ihave = 8
      else if ( ihave == 9 ) then
        ihave = 10
      end if

      call ch_to_digit ( c, ndig )

      if ( ihave == 3 ) then
        rtop = 10.0D+00 * rtop + real ( ndig, kind = 8 )
      else if ( ihave == 5 ) then
        rtop = 10.0D+00 * rtop + real ( ndig, kind = 8 )
        rbot = 10.0D+00 * rbot
      else if ( ihave == 8 ) then
        jtop = 10 * jtop + ndig
      else if ( ihave == 10 ) then
        jtop = 10 * jtop + ndig
        jbot = 10 * jbot
      end if
!
!  Anything else is regarded as a terminator.
!
    else
      iterm = 1
    end if
!
!  If we haven't seen a terminator, and we haven't examined the
!  entire string, go get the next character.
!
    if ( iterm == 1 ) then
      exit
    end if

  end do
!
!  If we haven't seen a terminator, and we have examined the
!  entire string, then we're done, and LENGTH is equal to NCHAR.
!
  if ( iterm /= 1 .and. length+1 == nchar ) then
    length = nchar
  end if
!
!  Number seems to have terminated.  Have we got a legal number?
!  Not if we terminated in states 1, 2, 6 or 7!
!
  if ( ihave == 1 .or. ihave == 2 .or. ihave == 6 .or. ihave == 7 ) then
    ierror = ihave
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'S_TO_R8 - Serious error!'
    write ( *, '(a)' ) '  Illegal or nonnumeric input:'
    write ( *, '(a)' ) '    ' // trim ( s )
    return
  end if
!
!  Number seems OK.  Form it.
!
  if ( jtop == 0 ) then
    rexp = 1.0D+00
  else
    if ( jbot == 1 ) then
      rexp = 10.0D+00 ** ( jsgn * jtop )
    else
      rexp = 10.0D+00 ** ( real ( jsgn * jtop, kind = 8 ) &
        / real ( jbot, kind = 8 ) )
    end if
  end if

  dval = real ( isgn, kind = 8 ) * rexp * rtop / rbot

  return
end
subroutine s_to_r8vec ( s, n, rvec, ierror )

!*****************************************************************************80
!
!! S_TO_R8VEC reads an R8VEC from a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be read.
!
!    Input, integer ( kind = 4 ) N, the number of values expected.
!
!    Output, real ( kind = 8 ) RVEC(N), the values read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    -K, could not read data for entries -K through N.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) lchar
  real ( kind = 8 ) rvec(n)
  character ( len = * ) s

  i = 0
  ierror = 0
  ilo = 1

  do while ( i < n )

    i = i + 1

    call s_to_r8 ( s(ilo:), rvec(i), ierror, lchar )

    if ( ierror /= 0 ) then
      ierror = -i
      exit
    end if

    ilo = ilo + lchar

  end do

  return
end
subroutine s_word_count ( s, nword )

!*****************************************************************************80
!
!! S_WORD_COUNT counts the number of "words" in a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be examined.
!
!    Output, integer ( kind = 4 ) NWORD, the number of "words" in the string.
!    Words are presumed to be separated by one or more blanks.
!
  implicit none

  logical blank
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lens
  integer ( kind = 4 ) nword
  character ( len = * ) s

  nword = 0
  lens = len ( s )

  if ( lens <= 0 ) then
    return
  end if

  blank = .true.

  do i = 1, lens

    if ( s(i:i) == ' ' ) then
      blank = .true.
    else if ( blank ) then
      nword = nword + 1
      blank = .false.
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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
