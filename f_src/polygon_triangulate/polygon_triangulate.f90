function angle_degree ( x1, y1, x2, y2, x3, y3 )

!*****************************************************************************80
!
!! ANGLE_DEGREE returns the degree angle defined by three points.
!
!  Discussion:
!
!        P1
!        /
!       /
!      /
!     /
!    P2--------->P3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real X1, Y1, X2, Y2, X3, Y3, the coordinates of the points
!    P1, P2, P3.
!
!    Output, real VALUE, the angle swept out by the rays, measured
!    in degrees.  0 <= VALUE < 360.  If either ray has zero length,
!    then VALUE is set to 0.
!
  implicit none

  real ( kind = 8 ) angle_degree
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) value
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) y
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3

  x = ( x3 - x2 ) * ( x1 - x2 ) + ( y3 - y2 ) * ( y1 - y2 )

  y = ( x3 - x2 ) * ( y1 - y2 ) - ( y3 - y2 ) * ( x1 - x2 )

  if ( x == 0.0D+00 .and. y == 0.0D+00 ) then

    value = 0.0D+00

  else

    value = atan2 ( y, x )

    if ( value < 0.0D+00 ) then
      value = value + 2.0D+00 * r8_pi
    end if

    value = 180.0D+00 * value / r8_pi

  end if

  angle_degree = value

  return
end
function between ( xa, ya, xb, yb, xc, yc )

!*****************************************************************************80
!
!! BETWEEN is TRUE if vertex C is between vertices A and B.
!
!  Discussion:
!
!    The points must be (numerically) collinear.
!
!    Given that condition, we take the greater of XA - XB and YA - YB
!    as a "scale" and check where C's value lies.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XA, YA, XB, YB, XC, YC, the coordinates of 
!    the vertices.
!
!    Output, logical ( kind = 4 ) BETWEEN, is TRUE if C is between A and B.
!
  implicit none

  logical ( kind = 4 ) between
  logical ( kind = 4 ) collinear
  logical ( kind = 4 ) value
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb
  real ( kind = 8 ) xc
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ) ya
  real ( kind = 8 ) yb
  real ( kind = 8 ) yc
  real ( kind = 8 ) ymax
  real ( kind = 8 ) ymin

  if ( .not. collinear ( xa, ya, xb, yb, xc, yc ) ) then
    value = .false.
  else if ( abs ( ya - yb ) < abs ( xa - xb ) ) then
    xmax = max ( xa, xb )
    xmin = min ( xa, xb )
    value = ( xmin <= xc .and. xc <= xmax )
  else
    ymax = max ( ya, yb )
    ymin = min ( ya, yb )
    value = ( ymin <= yc .and. yc <= ymax )
  end if

  between = value

  return
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
!! CH_TO_DIGIT returns the integer value of a base 10 digit.
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
!    Output, integer ( kind = 4 ) DIGIT, the corresponding integer value.
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
function collinear ( xa, ya, xb, yb, xc, yc )

!*****************************************************************************80
!
!! COLLINEAR returns a measure of collinearity for three points.
!
!  Discussion:
!
!    In order to deal with collinear points whose coordinates are not
!    numerically exact, we compare the area of the largest square
!    that can be created by the line segment between two of the points
!    to (twice) the area of the triangle formed by the points.
!
!    If the points are collinear, their triangle has zero area.
!    If the points are close to collinear, then the area of this triangle
!    will be small relative to the square of the longest segment.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2016
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XA, YA, XB, YB, XC, YC, the coordinates of 
!    the vertices.
!
!    Output, logical ( kind = 4 ) COLLINEAR, is TRUE if the points are judged 
!    to be collinear.
!
  implicit none

  real ( kind = 8 ) area
  logical ( kind = 4 ) collinear
  real ( kind = 8 ), parameter :: r8_eps = 2.220446049250313D-016
  real ( kind = 8 ) side_ab_sq
  real ( kind = 8 ) side_bc_sq
  real ( kind = 8 ) side_ca_sq
  real ( kind = 8 ) side_max_sq
  real ( kind = 8 ) triangle_area
  logical ( kind = 4 ) value
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb
  real ( kind = 8 ) xc
  real ( kind = 8 ) ya
  real ( kind = 8 ) yb
  real ( kind = 8 ) yc

  area = triangle_area ( xa, ya, xb, yb, xc, yc )

  side_ab_sq = ( xa - xb ) ** 2 + ( ya - yb ) ** 2
  side_bc_sq = ( xb - xc ) ** 2 + ( yb - yc ) ** 2
  side_ca_sq = ( xc - xa ) ** 2 + ( yc - ya ) ** 2

  side_max_sq = max ( side_ab_sq, max ( side_bc_sq, side_ca_sq ) )

  if ( side_max_sq <= r8_eps ) then
    value = .true.
  else if ( 2.0D+00 * abs ( area ) <= r8_eps * side_max_sq ) then
    value = .true.
  else
    value = .false.
  end if

  collinear = value

  return
end
function diagonal ( im1, ip1, n, prev_node, next_node, x, y )

!*****************************************************************************80
!
!! DIAGONAL: VERTEX(IM1) to VERTEX(IP1) is a proper internal diagonal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IM1, IP1, the indices of two vertices.
!
!    Input, integer ( kind = 4 ) N, the number of vertices.
!
!    Input, integer ( kind = 4 ) PREV_NODE(N), the previous neighbor of 
!    each vertex.
!
!    Input, integer ( kind = 4 ) NEXT_NODE(N), the next neighbor of each vertex.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of each vertex.
!
!    Output, logical ( kind = 4 ) DIAGONAL, the value of the test.
!
  implicit none

  integer ( kind = 4 ) n

  logical ( kind = 4 ) diagonal
  logical ( kind = 4 ) diagonalie
  integer ( kind = 4 ) im1
  logical ( kind = 4 ) in_cone
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) next_node(n)
  integer ( kind = 4 ) prev_node(n)
  logical ( kind = 4 ) value1
  logical ( kind = 4 ) value2
  logical ( kind = 4 ) value3
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  value1 = in_cone ( im1, ip1, n, prev_node, next_node, x, y )
  value2 = in_cone ( ip1, im1, n, prev_node, next_node, x, y )
  value3 = diagonalie ( im1, ip1, n, next_node, x, y )

  diagonal = ( value1 .and. value2 .and. value3 )

  return
end
function diagonalie ( im1, ip1, n, next_node, x, y )

!*****************************************************************************80
!
!! DIAGONALIE is true if VERTEX(IM1):VERTEX(IP1) is a proper diagonal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 May 2014
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IM1, IP1, the indices of two vertices.
!
!    Input, integer ( kind = 4 ) N, the number of vertices.
!
!    Input, integer ( kind = 4 ) NEXT_NODE(N), the next neighbor of each vertex.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of each vertex.
!
!    Output, logical ( kind = 4 ) DIAGONALIE, the value of the test.
!
  implicit none

  integer ( kind = 4 ) n

  logical ( kind = 4 ) diagonalie
  integer ( kind = 4 ) first
  integer ( kind = 4 ) im1
  logical ( kind = 4 ) intersect
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jp1
  integer ( kind = 4 ) next_node(n)
  logical ( kind = 4 ) value
  logical ( kind = 4 ) value2
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  first = im1
  j = first
  jp1 = next_node(first)

  value = .true.
!
!  For each edge VERTEX(J):VERTEX(JP1) of the polygon:
!
  do
!
!  Skip any edge that includes vertex IM1 or IP1.
!
    if ( j == im1 .or. j == ip1 .or. jp1 == im1 .or. jp1 == ip1 ) then

    else

      value2 = intersect ( x(im1), y(im1), x(ip1), y(ip1), x(j), y(j), &
        x(jp1), y(jp1) )

      if ( value2 ) then
        value = .false.
        exit
      end if

    end if

    j = jp1
    jp1 = next_node(j)

    if ( j == first ) then
      exit
    end if

  end do

  diagonalie = value

  return
end
subroutine file_column_count ( input_filename, column_num )

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
!    Input, character ( len = * ) INPUT_FILENAME, the name of the file.
!
!    Output, integer ( kind = 4 ) COLUMN_NUM, the number of columns in the file.
!
  implicit none

  integer ( kind = 4 ) column_num
  logical got_one
  character ( len = * ) input_filename
  integer ( kind = 4 ) input_status
  integer ( kind = 4 ) input_unit
  character ( len = 255 ) line
!
!  Open the file.
!
  call get_unit ( input_unit )

  open ( unit = input_unit, file = input_filename, status = 'old', &
    form = 'formatted', access = 'sequential', iostat = input_status )

  if ( input_status /= 0 ) then
    column_num = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILE_COLUMN_COUNT - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the input file "' &
      // trim ( input_filename ) // '" on unit ', input_unit
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
subroutine file_row_count ( input_filename, row_num )

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
!    Input, character ( len = * ) INPUT_FILENAME, the name of the input file.
!
!    Output, integer ( kind = 4 ) ROW_NUM, the number of rows found.
!
  implicit none

  integer ( kind = 4 ) bad_num
  integer ( kind = 4 ) comment_num
  integer ( kind = 4 ) ierror
  character ( len = * ) input_filename
  integer ( kind = 4 ) input_status
  integer ( kind = 4 ) input_unit
  character ( len = 255 ) line
  integer ( kind = 4 ) record_num
  integer ( kind = 4 ) row_num

  call get_unit ( input_unit )

  open ( unit = input_unit, file = input_filename, status = 'old', &
    iostat = input_status )

  if ( input_status /= 0 ) then
    row_num = -1
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILE_ROW_COUNT - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the input file "' // &
      trim ( input_filename ) // '" on unit ', input_unit
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
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2008
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
  logical ( kind = 4 ) lopen

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
subroutine i4mat_transpose_print ( m, n, a, title )

!*****************************************************************************80
!
!! I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, integer ( kind = 4 ) A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  character ( len = * ) title

  call i4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine i4mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, integer ( kind = 4 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 10
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  character ( len = 8 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

    i2hi = i2lo + incx - 1
    i2hi = min ( i2hi, m )
    i2hi = min ( i2hi, ihi )

    inc = i2hi + 1 - i2lo

    write ( *, '(a)' ) ' '

    do i = i2lo, i2hi
      i2 = i + 1 - i2lo
      write ( ctemp(i2), '(i8)' ) i
    end do

    write ( *, '(''  Row '',10a8)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Col'
    write ( *, '(a)' ) ' '

    j2lo = max ( jlo, 1 )
    j2hi = min ( jhi, n )

    do j = j2lo, j2hi

      do i2 = 1, inc

        i = i2lo - 1 + i2

        write ( ctemp(i2), '(i8)' ) a(i,j)

      end do

      write ( *, '(i5,a,10a8)' ) j, ':', ( ctemp(i), i = 1, inc )

    end do

  end do

  return
end
subroutine i4mat_write ( output_filename, m, n, table )

!*****************************************************************************80
!
!! I4MAT_WRITE writes an I4MAT file.
!
!  Discussion:
!
!    An I4MAT is an array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the output file name.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, integer ( kind = 4 ) TABLE(M,N), the data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  character ( len = * ) output_filename
  integer ( kind = 4 ) output_status
  integer ( kind = 4 ) output_unit
  character ( len = 30 ) string
  integer ( kind = 4 ) table(m,n)
!
!  Open the file.
!
  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_filename, &
    status = 'replace', iostat = output_status )

  if ( output_status /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4MAT_WRITE - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the output file "' // &
      trim ( output_filename ) // '" on unit ', output_unit
    output_unit = -1
    stop 1
  end if
!
!  Create a format string.
!
  if ( 0 < m .and. 0 < n ) then

    write ( string, '(a1,i8,a4)' ) '(', m, 'i10)'
!
!  Write the data.
!
    do j = 1, n
      write ( output_unit, string ) table(1:m,j)
    end do

  end if
!
!  Close the file.
!
  close ( unit = output_unit )

  return
end
function in_cone ( im1, ip1, n, prev_node, next_node, x, y )

!*****************************************************************************80
!
!! IN_CONE is TRUE if the diagonal VERTEX(IM1):VERTEX(IP1) is strictly internal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IM1, IP1, the indices of two vertices.
!
!    Input, integer ( kind = 4 ) N, the number of vertices.
!
!    Input, integer ( kind = 4 ) PREV_NODE(N), the previous neighbor of 
!    each vertex.
!
!    Input, integer ( kind = 4 ) NEXT_NODE(N), the next neighbor of each vertex.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of each vertex.
!
!    Output, logical ( kind = 4 ) IN_CONE, the value of the test.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) im1
  integer ( kind = 4 ) im2
  logical ( kind = 4 ) in_cone
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) next_node(n)
  integer ( kind = 4 ) prev_node(n)
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) t3
  real ( kind = 8 ) t4
  real ( kind = 8 ) t5
  real ( kind = 8 ) triangle_area
  logical ( kind = 4 ) value
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  im2 = prev_node(im1)
  i = next_node(im1)

  t1 = triangle_area ( x(im1), y(im1), x(i), y(i), x(im2), y(im2) )

  if ( 0.0D+00 <= t1 ) then

    t2 = triangle_area ( x(im1), y(im1), x(ip1), y(ip1), x(im2), y(im2) )
    t3 = triangle_area ( x(ip1), y(ip1), x(im1), y(im1), x(i), y(i) )
    value = ( ( 0.0D+00 < t2 ) .and. ( 0.0D+00 < t3 ) )

  else

    t4 = triangle_area ( x(im1), y(im1), x(ip1), y(ip1), x(i), y(i) )
    t5 = triangle_area ( x(ip1), y(ip1), x(im1), y(im1), x(im2), y(im2) )
    value = .not. ( ( 0.0D+00 <= t4 ) .and. ( 0.0D+00 <= t5 ) )

  end if

  in_cone = value

  return
end
function intersect ( xa, ya, xb, yb, xc, yc, xd, yd )

!*****************************************************************************80
!
!! INTERSECT is true if lines VA:VB and VC:VD intersect.
!
!  Discussion:
!
!    Thanks to Gene Dial for correcting the call to intersect_prop(),
!    08 September 2016.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2016
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XA, YA, XB, YB, XC, YC, XD, YD, the X and Y 
!    coordinates of the four vertices.
!
!    Output, logical ( kind = 4 ) VALUE, the value of the test.
!
  implicit none

  logical ( kind = 4 ) between
  logical ( kind = 4 ) intersect
  logical ( kind = 4 ) intersect_prop
  logical ( kind = 4 ) value
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb
  real ( kind = 8 ) xc
  real ( kind = 8 ) xd
  real ( kind = 8 ) ya
  real ( kind = 8 ) yb
  real ( kind = 8 ) yc
  real ( kind = 8 ) yd

  if ( intersect_prop ( xa, ya, xb, yb, xc, yc, xd, yd ) ) then
    value = .true.
  else if ( between ( xa, ya, xb, yb, xc, yc ) ) then
    value = .true.
  else if ( between ( xa, ya, xb, yb, xd, yd ) ) then
    value = .true.
  else if ( between ( xc, yc, xd, yd, xa, ya ) ) then
    value = .true.
  else if ( between ( xc, yc, xd, yd, xb, yb ) ) then
    value = .true.
  else
    value = .false.
  end if

  intersect = value

  return
end
function intersect_prop ( xa, ya, xb, yb, xc, yc, xd, yd )

!*****************************************************************************80
!
!! INTERSECT_PROP is TRUE if lines VA:VB and VC:VD have a proper intersection.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XA, YA, XB, YB, XC, YC, XD, YD, the X and Y 
!    coordinates of the four vertices.
!
!    Output, logical ( kind = 4 ) INTERSECT_PROP, the result of the test.
!
  implicit none

  logical ( kind = 4 ) collinear
  logical ( kind = 4 ) intersect_prop
  logical ( kind = 4 ) l4_xor
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) t3
  real ( kind = 8 ) t4
  real ( kind = 8 ) triangle_area
  logical ( kind = 4 ) value
  logical ( kind = 4 ) value1
  logical ( kind = 4 ) value2
  logical ( kind = 4 ) value3
  logical ( kind = 4 ) value4
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb
  real ( kind = 8 ) xc
  real ( kind = 8 ) xd
  real ( kind = 8 ) ya
  real ( kind = 8 ) yb
  real ( kind = 8 ) yc
  real ( kind = 8 ) yd

  if ( collinear ( xa, ya, xb, yb, xc, yc ) ) then
    value = .false.
  else if ( collinear ( xa, ya, xb, yb, xd, yd ) ) then
    value = .false.
  else if ( collinear ( xc, yc, xd, yd, xa, ya ) ) then
    value = .false.
  else if ( collinear ( xc, yc, xd, yd, xb, yb ) ) then
    value = .false.
  else

    t1 = triangle_area ( xa, ya, xb, yb, xc, yc )
    t2 = triangle_area ( xa, ya, xb, yb, xd, yd )
    t3 = triangle_area ( xc, yc, xd, yd, xa, ya )
    t4 = triangle_area ( xc, yc, xd, yd, xb, yb )

    value1 = ( 0.0D+00 < t1 )
    value2 = ( 0.0D+00 < t2 )
    value3 = ( 0.0D+00 < t3 )
    value4 = ( 0.0D+00 < t4 )

    value = ( l4_xor ( value1, value2 ) ) .and. ( l4_xor ( value3, value4 ) )
 
  end if

  intersect_prop = value

  return
end
function l4_xor ( l1, l2 )

!*****************************************************************************80
!
!! L4_XOR returns the exclusive OR of two L4's.
!
!  Discussion:
!
!    An L4 is a logical ( kind = 4 ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!   John Burkardt
!
!  Parameters:
!
!    Input, logical ( kind = 4 ) L1, L2, two values whose exclusive OR 
!    is needed.
!
!    Output, logical ( kind = 4 ) L4_XOR, the exclusive OR of L1 and L2.
!
  implicit none

  logical ( kind = 4 ) l1
  logical ( kind = 4 ) l2
  logical ( kind = 4 ) l4_xor
  logical ( kind = 4 ) value1
  logical ( kind = 4 ) value2

  value1 = (         l1   .and. ( .not. l2 ) )
  value2 = ( ( .not. l1 ) .and.         l2   )

  l4_xor = ( value1 .or. value2 )

  return
end
function polygon_area2 ( n, x, y )

!*****************************************************************************80
!
!! POLYGON_AREA2 returns the area of a polygon.
!
!  Discussion:
!
!    The vertices should be listed in counter-clockwise order so that
!    the area will be positive.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2016
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of vertices.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the vertex coordinates.
!
!    Output, real ( kind = 8 ) POLYGON_AREA2, the area of the polygon.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) area
  integer ( kind = 4 ) i
  integer ( kind = 4 ) im1
  real ( kind = 8 ) polygon_area2
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  area = 0.0D+00
  im1 = n

  do i = 1, n
    area = area + x(im1) * y(i) - x(i) * y(im1)
    im1 = i
  end do

  area = 0.5D+00 * area

  polygon_area2 = area

  return
end
subroutine polygon_triangulate ( n, x, y, triangles )

!*****************************************************************************80
!
!! POLYGON_TRIANGULATE determines a triangulation of a polygon.
!
!  Discussion:
!
!    There are N-3 triangles in the triangulation.
!
!    For the first N-2 triangles, the first edge listed is always an
!    internal diagonal.
!
!    Thanks to Gene Dial for pointing out a mistake in the area calculation,
!    10 September 2016.
!
!    Gene Dial requested an angle tolerance of about 1 millionth radian or 
!    5.7E-05 degrees, 26 June 2018.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2018
!
!  Author:
!
!    Original C version by Joseph ORourke.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry in C,
!    Cambridge, 1998,
!    ISBN: 0521649765,
!    LC: QA448.D38.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of vertices.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of each vertex.
!
!    Output, integer ( kind = 4 ) TRIANGLES(3,N-2), the triangles of the 
!    triangulation.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) angle
  real ( kind = 8 ) angle_degree
  real ( kind = 8 ), parameter :: angle_tol = 5.7D-05
  real ( kind = 8 ) area
  logical ( kind = 4 ) diagonal
  logical ( kind = 4 ) ear(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i0
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) i4
  integer ( kind = 4 ) next_node(n)
  integer ( kind = 4 ) node
  integer ( kind = 4 ) node_m1
  integer ( kind = 4 ) node1
  integer ( kind = 4 ) node2
  integer ( kind = 4 ) node3
  real ( kind = 8 ) polygon_area2
  integer ( kind = 4 ) prev_node(n)
  integer ( kind = 4 ) triangle_num
  integer ( kind = 4 ) triangles(3,n-2)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  We must have at least 3 vertices.
!
  if ( n < 3 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POLYGON_TRIANGULATE - Fatal error!'
    write ( *, '(a)' ) '  N < 3.'
    stop 1
  end if
!
!  Consecutive vertices cannot be equal.
!
  node_m1 = n
  do node = 1, n
    if ( x(node_m1) == x(node) .and. y(node_m1) == y(node) ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POLYGON_TRIANGULATE - Fatal error!'
      write ( *, '(a)' ) '  Two consecutive nodes are identical.'
      stop 1
    end if
    node_m1 = node
  end do
!
!  No node can be the vertex of an angle less than 1 degree 
!  in absolute value.
!
  node1 = n

  do node2 = 1, n

    node3 = mod ( node2, n ) + 1

    angle = angle_degree ( &
      x(node1), y(node1), &
      x(node2), y(node2), &
      x(node3), y(node3) )

    if ( abs ( angle ) <= angle_tol ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYGON_TRIANGULATE - Fatal error!'
      write ( *, '(a,g14.6)' ) '  Polygon has an angle smaller than ', angle_tol
      write ( *, '(a,i4)' ) '  occurring at node ', node2
      stop 1
    end if

    node1 = node2

  end do
!
!  Area must be positive.
!
  area = polygon_area2 ( n, x, y )

  if ( area <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POLYGON_TRIANGULATE - Fatal error!'
    write ( *, '(a)' ) '  Polygon has zero or negative area.'
    stop 1
  end if
!
!  PREV and NEXT point to the previous and next nodes.
!
  i = 1
  prev_node(i) = n
  next_node(i) = i + 1

  do i = 2, n - 1
    prev_node(i) = i - 1
    next_node(i) = i + 1
  end do

  i = n
  prev_node(i) = i - 1
  next_node(i) = 1
!
!  EAR indicates whether the node and its immediate neighbors form an ear
!  that can be sliced off immediately.
!
  do i = 1, n
    ear(i) = diagonal ( prev_node(i), next_node(i), n, prev_node, next_node, &
      x, y )
  end do

  triangle_num = 0

  i2 = 1

  do while ( triangle_num < n - 3 )
!
!  If I2 is an ear, gather information necessary to carry out
!  the slicing operation and subsequent "healing".
!
    if ( ear(i2) ) then

      i3 = next_node(i2)
      i4 = next_node(i3)
      i1 = prev_node(i2)
      i0 = prev_node(i1)
!
!  Make vertex I2 disappear.
!
      next_node(i1) = i3
      prev_node(i3) = i1
!
!  Update the earity of I1 and I3, because I2 disappeared.
!
      ear(i1) = diagonal ( i0, i3, n, prev_node, next_node, x, y )
      ear(i3) = diagonal ( i1, i4, n, prev_node, next_node, x, y )
!
!  Add the diagonal [I3, I1, I2] to the list.
!
      triangle_num = triangle_num + 1
      triangles(1,triangle_num) = i3
      triangles(2,triangle_num) = i1
      triangles(3,triangle_num) = i2

    end if
!
!  Try the next vertex.
!
    i2 = next_node(i2)

  end do
!
!  The last triangle is formed from the three remaining vertices.
!
  i3 = next_node(i2)
  i1 = prev_node(i2)

  triangle_num = triangle_num + 1
  triangles(1,triangle_num) = i3
  triangles(2,triangle_num) = i1
  triangles(3,triangle_num) = i2

  return
end
subroutine r8mat_data_read ( input_filename, m, n, table )

!*****************************************************************************80
!
!! R8MAT_DATA_READ reads data from an R8MAT file.
!
!  Discussion:
!
!    An R8MAT is an array of R8 values.
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
!    Output, real ( kind = 8 ) TABLE(M,N), the data.
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
!  Discussion:
!
!    An R8MAT is an array of R8 values.
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
  if ( iterm /= 1 .and. length + 1 == nchar ) then
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
function triangle_area ( xa, ya, xb, yb, xc, yc )

!*****************************************************************************80
!
!! TRIANGLE_AREA computes the signed area of a triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XA, YA, XB, YB, XC, YC, the coordinates of
!    the vertices of the triangle, given in counterclockwise order.
!
!    Output, real ( kind = 8 ) TRIANGLE_AREA, the signed area of the triangle.
!
  implicit none

  real ( kind = 8 ) triangle_area
  real ( kind = 8 ) value
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb
  real ( kind = 8 ) xc
  real ( kind = 8 ) ya
  real ( kind = 8 ) yb
  real ( kind = 8 ) yc

  value = 0.5D+00 * ( &
      ( xb - xa ) * ( yc - ya ) &
    - ( xc - xa ) * ( yb - ya ) )

  triangle_area = value

  return
end

