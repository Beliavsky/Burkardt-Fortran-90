program main

!*****************************************************************************80
!
!! MAIN is the main program for STRIPACK_VORONOI
!
!  Discussion:
!
!    STRIPACK_VORONOI is an VORONOI interface for STRIPACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 January 2013
!
!  Author:
!
!    John Burkardt
!
!  Usage:
!
!    stripack_voronoi xyz_file
!
  implicit none

  integer ( kind = 4 ) arg_num
  integer ( kind = 4 ), parameter :: dim_num = 3
  integer ( kind = 4 ) dim_num1
  integer ( kind = 4 ), allocatable :: first(:)
  integer ( kind = 4 ) iarg
  integer ( kind = 4 ) iargc
  integer ( kind = 4 ), allocatable :: lend(:)
  integer ( kind = 4 ), allocatable :: list(:)
  integer ( kind = 4 ), allocatable :: listc(:)
  integer ( kind = 4 ), allocatable :: lptr(:)
  integer ( kind = 4 ) node_num
  integer ( kind = 4 ) nt
  integer ( kind = 4 ), allocatable :: order(:)
  integer ( kind = 4 ) order_sum
  character ( len = 255 ) :: voronoi_filename = 'voronoi.txt'
  real ( kind = 8 ), allocatable :: xc(:)
  real ( kind = 8 ), allocatable :: xyz(:,:)
  real ( kind = 8 ), allocatable :: xyzv(:,:)
  character ( len = 255 ) :: xyz_filename = ' '
  real ( kind = 8 ), allocatable :: yc(:)
  real ( kind = 8 ), allocatable :: zc(:)

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'STRIPACK_VORONOI'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Read a file of (X,Y,Z) coordinates of points on'
  write ( *, '(a)' ) '  the unit sphere.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Call STRIPACK to compute the Delaunay triangulation.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Write a file of triplets (N1,N2,N3) of nodes defining'
  write ( *, '(a)' ) '  the Delaunay triangulation.'
!
!  Get the number of command line arguments.
!
  arg_num = iargc ( )
!
!  If at least one command line argument, it is the node coordinate file.
!
  if ( 1 <= arg_num ) then

    iarg = 1
    call getarg ( iarg, xyz_filename )

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'STRIPACK_VORONOI:'
    write ( *, '(a)' ) '  Please enter the name of the node coordinate file.'

    read ( *, '(a)' ) xyz_filename

  end if
!
!  Read the XYZ data.
!
  call r8mat_header_read ( xyz_filename, dim_num1, node_num )

  if ( dim_num1 /= dim_num ) then
    write ( *, * ) ' '
    write ( *, '(a)' ) 'STRIPACK_VORONOI - Fatal error!'
    write ( *, '(a)' ) '  Input data is not 3D.'
    stop
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Read the header of "' &
    // trim ( xyz_filename ) //'".'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension = ', dim_num
  write ( *, '(a,i8)' ) '  Number of nodes   = ', node_num

  allocate ( xyz(1:dim_num,1:node_num) )

  call r8mat_data_read ( xyz_filename, dim_num, node_num, xyz )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Read the node coordinate data in "' &
    // trim ( xyz_filename ) //'".'

  call r8mat_transpose_print_some ( dim_num, node_num, xyz, &
    1, 1, dim_num, 5, '  Initial part of the node coordinate array:' )
!
!  Now we compute the Voronoi information on the sphere.
!
  allocate ( lend(1:node_num) )
  allocate ( listc(1:6*(node_num-2)) )
  allocate ( lptr(1:6*(node_num-2)) )
  allocate ( xc(1:2*(node_num-2)) )
  allocate ( yc(1:2*(node_num-2)) )
  allocate ( zc(1:2*(node_num-2)) )

  call voronoi_get ( node_num, xyz(1,:), xyz(2,:), xyz(3,:), nt, xc, yc, zc, &
    lend, listc, lptr )
!
!  Gather the Voronoi vertices and print them.
!
  allocate ( xyzv(1:3,1:nt) )

  xyzv(1,1:nt) = xc(1:nt)
  xyzv(2,1:nt) = yc(1:nt)
  xyzv(3,1:nt) = zc(1:nt)

  voronoi_filename = 'voronoi.xyz'
  call r8mat_write ( voronoi_filename, 3, nt, xyzv )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Wrote the Voronoi vertices to "' &
    // trim ( voronoi_filename ) // '".'
!
!  Get the order of each Voronoi polygon.
!
  allocate ( order(1:node_num) )
  call voronoi_order ( node_num, lend, lptr, order )

  call i4vec_print ( node_num, order, '  Voronoi polygon orders:' )
!
!  Get the Voronoi polygons as a list.
!
  order_sum = sum ( order(1:node_num) )
  allocate ( first(1:node_num+1) )
  allocate ( list(1:order_sum) )

  call voronoi_polygons ( node_num, order_sum, lend, listc, lptr, first, list )

! call i4list_print ( node_num, first, order_sum, list, '  Voronoi polygons:' )

  voronoi_filename = 'voronoi.xyzf'
  call i4list_write ( voronoi_filename, node_num, first, order_sum, list )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Wrote the Voronoi vertex lists to "' &
    // trim ( voronoi_filename ) //'".'
!
!  Free memory.
!
  deallocate ( first )
  deallocate ( lend )
  deallocate ( list )
  deallocate ( listc )
  deallocate ( lptr )
  deallocate ( order )
  deallocate ( xc )
  deallocate ( xyz )
  deallocate ( xyzv )
  deallocate ( yc )
  deallocate ( zc )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'STRIPACK_VORONOI'
  write ( *, '(a)' ) '  Normal end of execution.'

  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine ch_cap ( ch )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions,
!    which guarantee the ASCII collating sequence.
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
!    Input/output, character CH, the character to capitalize.
!
  implicit none

  character ch
  integer ( kind = 4 ) itemp

  itemp = iachar ( ch )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    ch = achar ( itemp - 32 )
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
!    CH_EQI ( 'A', 'a' ) is TRUE.
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
!    Output, logical CH_EQI, the result of the comparison.
!
  implicit none

  character c1
  character c1_cap
  character c2
  character c2_cap
  logical ch_eqi

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
!    Input, character CH, the decimal digit, '0' through '9' or blank
!    are legal.
!
!    Output, integer ( kind = 4 ) DIGIT, the corresponding value.
!    If CH was 'illegal', then DIGIT is -1.
!
  implicit none

  character ch
  integer ( kind = 4 ) digit

  if ( lle ( '0', ch ) .and. lle ( ch, '9' ) ) then

    digit = iachar ( ch ) - 48

  else if ( ch == ' ' ) then

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
  logical got_one
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
    stop
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
subroutine i4list_print ( n, first, list_num, list, title )

!*****************************************************************************80
!
!! I4LIST_PRINT prints an I4LIST.
!
!  Discussion:
!
!    An I4LIST is a list of integers grouped into N segments.
!    An index vector locates the first entry of each segment.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of segments.
!
!    Input, integer ( kind = 4 ) FIRST(N+1), indexes the first entry
!    of each segment.
!
!    Input, integer ( kind = 4 ) LIST_NUM, the number of entries.
!
!    Input, integer ( kind = 4 ) LIST(LIST_NUM), the data.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) list_num
  integer ( kind = 4 ) n

  integer ( kind = 4 ) first(n+1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) list(list_num)
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n

    do jlo = first(i), first(i+1) - 1, 5
      jhi = min ( jlo + 4, first(i+1) - 1 )
      if ( jlo == first(i) ) then
        write ( *, '(i5,a,5(2x,i8))' ) i, ':', list(jlo:jhi)
      else
        write ( *, '(6x,  5(2x,i8))' )         list(jlo:jhi)
      end if
    end do

  end do

  return
end
subroutine i4list_write ( output_filename, n, first, list_num, list )

!*****************************************************************************80
!
!! I4LIST_PRINT prints an I4LIST.
!
!  Discussion:
!
!    An I4LIST is a list of integers grouped into N segments.
!    An index vector locates the first entry of each segment.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the name of the file.
!
!    Input, integer ( kind = 4 ) N, the number of segments.
!
!    Input, integer ( kind = 4 ) FIRST(N+1), indexes the first entry
!    of each segment.
!
!    Input, integer ( kind = 4 ) LIST_NUM, the number of entries.
!
!    Input, integer ( kind = 4 ) LIST(LIST_NUM), the data.
!
  implicit none

  integer ( kind = 4 ) list_num
  integer ( kind = 4 ) n

  character ( len = * ) output_filename
  integer ( kind = 4 ) first(n+1)
  character ( len = 80 ) format_string
  integer ( kind = 4 ) i
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) list(list_num)
  integer ( kind = 4 ) order_max
  integer ( kind = 4 ) output_unit

  order_max = 0
  do i = 1, n
    order_max = max ( order_max, first(i+1) - first(i) )
  end do

  write ( format_string, '(a1,i8,a3)' ) '(', order_max, 'i5)'

  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_filename, status = 'replace' )

  do i = 1, n

    jlo = first(i)
    jhi = first(i+1) - 1
    write ( output_unit, format_string ) list(jlo:jhi)

  end do

  close ( unit = output_unit )

  return
end
subroutine i4mat_transpose_print ( m, n, a, title )

!*****************************************************************************80
!
!! I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
!
!  Discussion:
!
!    An I4MAT is a rectangular array of I4's.
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
!    An I4MAT is a rectangular array of integer values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 February 2005
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
!    Input, integer ILO, JLO, the first row and column to print.
!
!    Input, integer IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

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

      write ( *, '(i5,1x,10a8)' ) j, ( ctemp(i), i = 1, inc )

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
!    Input, integer ( kind = 4 ) TABLE(M,N), the table data.
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
    stop
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,a,2x,i12)' ) i, ':', a(i)
  end do

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
    stop
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
      stop
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
    stop
  end if

  call file_row_count ( input_filename, n )

  if ( n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_HEADER_READ - Fatal error!'
    write ( *, '(a)' ) '  There was some kind of I/O problem while trying'
    write ( *, '(a)' ) '  to count the number of data rows in'
    write ( *, '(a)' ) '  the file "' // trim ( input_filename ) // '".'
    stop
  end if

  return
end
subroutine r8mat_transpose_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is a two dimensional matrix of double precision real values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

    i2hi = i2lo + incx - 1
    i2hi = min ( i2hi, m )
    i2hi = min ( i2hi, ihi )

    inc = i2hi + 1 - i2lo

    write ( *, '(a)' ) ' '

    do i = i2lo, i2hi
      i2 = i + 1 - i2lo
      write ( ctemp(i2), '(i8,6x)' ) i
    end do

    write ( *, '(''  Row   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Col'
    write ( *, '(a)' ) ' '

    j2lo = max ( jlo, 1 )
    j2hi = min ( jhi, n )

    do j = j2lo, j2hi

      do i2 = 1, inc
        i = i2lo - 1 + i2
        write ( ctemp(i2), '(g14.6)' ) a(i,j)
      end do

      write ( *, '(i5,1x,5a14)' ) j, ( ctemp(i), i = 1, inc )

    end do

  end do

  return
end
subroutine r8mat_write ( output_filename, m, n, table )

!*****************************************************************************80
!
!! R8MAT_WRITE writes an R8MAT file.
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
!    31 May 2009
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
!    Input, real ( kind = 8 ) TABLE(M,N), the data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  character ( len = * ) output_filename
  integer ( kind = 4 ) output_status
  integer ( kind = 4 ) output_unit
  character ( len = 30 ) string
  real ( kind = 8 ) table(m,n)
!
!  Open the file.
!
  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_filename, &
    status = 'replace', iostat = output_status )

  if ( output_status /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_WRITE - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the output file "' // &
      trim ( output_filename ) // '" on unit ', output_unit
    output_unit = -1
    stop
  end if
!
!  Create a format string.
!
!  For less precision in the output file, try:
!
!                                            '(', m, 'g', 14, '.', 6, ')'
!
  if ( 0 < m .and. 0 < n ) then

    write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 24, '.', 16, ')'
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

  logical ch_eqi
  character c
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
subroutine voronoi_get ( n, x, y, z, nt, xc, yc, zc, lend, listc, lptr )

!*****************************************************************************80
!
!! VORONOI_GET calls STRIPACK routines to get Voronoi information.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 June 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), Z(N), the coordinates of points 
!    on the sphere.
!
!    Output, integer ( kind = 4 ) NT, the number of Delaunay triangles
!    and Voronoi vertices.
!
!    Output, real ( kind = 8 ) XC(6*(N-2)), YC(6*(N-2)), ZC(6*(N-2)), the 
!    coordinates of the vertices.
!
!    Output, integer ( kind = 4 ) LEND(N), points to the "first" vertex in the
!    Voronoi polygon around a particular node.
!
!    Output, integer ( kind = 4 ) LISTC(6*(N-2)), the Voronoi vertex indices.
!
!    Output, integer ( kind = 4 ) LPTR(6*(N-2)), given a vertex, returns the 
!    next vertex in the Voronoi polygon.  (The vertex numbering is done
!    in such a way that the physical vertex has three distinct indices,
!    depending on which polygon we are considering.  Thus, it is always
!    possible to answer the question "which is the next vertex from this
!    one?" because the vertex index also tells us what polygon we are in.)
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: nrow = 9

  real ( kind = 8 ) ds(n)
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) iwk(2*n)
  integer ( kind = 4 ) lbtri(6,n)
  integer ( kind = 4 ) lend(n)
  integer ( kind = 4 ) list(6*(n-2))
  integer ( kind = 4 ) listc(6*(n-2)) 
  integer ( kind = 4 ) lnew
  integer ( kind = 4 ) lptr(6*(n-2))
  integer ( kind = 4 ) ltri(nrow,2*(n-2))
  integer ( kind = 4 ) nb
  integer ( kind = 4 ) nt
  real ( kind = 8 ) rc(2*(n-2))
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xc(2*(n-2))
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) yc(2*(n-2))
  real ( kind = 8 ) z(n)
  real ( kind = 8 ) zc(2*(n-2))
!
!  Create the triangulation.
!
  call trmesh ( n, x, y, z, list, lptr, lend, lnew, iwk, iwk(n+1), ds, ierror )

  if ( ierror == -2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VORONOI_GET - Fatal error!'
    write ( *, '(a)' ) '  Error in TRMESH.'
    write ( *, '(a)' ) '  The first 3 nodes are collinear.'
    stop
  end if

  if ( 0 < ierror ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VORONOI_GET - Fatal error!'
    write ( *, '(a)' ) '  Error in TRMESH.'
    write ( *, '(a)' ) '  Duplicate nodes encountered.'
    stop
  end if
!
!  Create a triangle list.
!
  call trlist ( n, list, lptr, lend, nrow, nt, ltri, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VORONOI_GET - Fatal error!'
    write ( *, '(a)' ) '  Error in TRLIST.'
    stop
  end if

  call i4mat_transpose_print ( nrow, nt, ltri, '  Vertices/Triangles/Arcs:' )
!
!  Construct the Voronoi diagram.
!
!  Note that the triangulation data structure is altered if NB > 0.
!
  call crlist ( n, n, x, y, z, list, lend, lptr, lnew, &
    lbtri, listc, nb, xc, yc, zc, rc, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VORONOI_GET - Fatal error!'
    write ( *, '(a)' ) '  Error in CRLIST.'
    write ( *, '(a,i8)' ) '  IERROR = ', ierror
    stop
  end if

  return
end
subroutine voronoi_order ( n, lend, lptr, order )

!*****************************************************************************80
!
!! VORONOI_ORDER computes the order of each polygon in a Voronoi diagram.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes, and Voronoi polygons.
!
!    Input, integer ( kind = 4 ) LEND(N), points to the "first" vertex in the
!    Voronoi polygon around a particular node.
!
!    Input, integer ( kind = 4 ) LPTR(6*(N-2)), given a vertex, returns the next
!    vertex in the Voronoi polygon.  (The vertex numbering is done
!    in such a way that the physical vertex has three distince indices,
!    depending on which polygon we are considering.  Thus, it is always
!    possible to answer the question "which is the next vertex from this
!    one?" because the vertex index also tells us what polygon we are in.)
!
!    Output, integer ( kind = 4 ) ORDER(N), the order of each polygon.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) lend(n)
  integer ( kind = 4 ) lptr(6*(n-2))
  integer ( kind = 4 ) node
  integer ( kind = 4 ) node_last
  integer ( kind = 4 ) node_new
  integer ( kind = 4 ) node_stop
  integer ( kind = 4 ) order(n)

  do node = 1, n

    order(node) = 0;

    node_stop = lend(node)
    node_new = node_stop

    do

      node_last = node_new
      node_new = lptr(node_last)

      order(node) = order(node) + 1

      if ( node_new == node_stop ) then
        exit
      end if

    end do

  end do

  return
end
subroutine voronoi_polygons ( n, list_num, lend, listc, lptr, first, list )

!*****************************************************************************80
!
!! VORONOI_POLYGONS creates a list of Voronoi polygons.
!
!  Discussion:
!
!    STRIPACK defines a data structure recording the location of
!    the vertices of the Voronoi diagram, and their connectivity.
!    The purpose of this routine is to construct a simplified data structure
!    that lists the indices of the Voronoi vertices that form each 
!    Voronoi polygon.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes, and Voronoi polygons.
!
!    Input, integer ( kind = 4 ) LIST_NUM, the number of entries to be stored
!    in LIST.
!
!    Input, integer ( kind = 4 ) LEND(N), points to the "first" vertex in the
!    Voronoi polygon around a particular node.
!
!    Input, integer ( kind = 4 ) LISTC(6*(N-2)), the Voronoi vertex indices.
!
!    Input, integer ( kind = 4 ) LPTR(6*(N-2)), given a vertex, returns the 
!    next vertex in the Voronoi polygon.  (The vertex numbering is done
!    in such a way that the physical vertex has three distince indices,
!    depending on which polygon we are considering.  Thus, it is always
!    possible to answer the question "which is the next vertex from this
!    one?" because the vertex index also tells us what polygon we are in.)
!
!    Output, integer FIRST(N+1), for each polygon, points to the location
!    in LIST of the index.
!
!    Output, integer LIST(LIST_NUM), the list of vertices that form each 
!    polygon.
!
  implicit none

  integer ( kind = 4 ) list_num
  integer ( kind = 4 ) n

  integer ( kind = 4 ) first(n+1)
  integer ( kind = 4 ) lend(n)
  integer ( kind = 4 ) list(list_num)
  integer ( kind = 4 ) listc(6*(n-2))
  integer ( kind = 4 ) lptr(6*(n-2))
  integer ( kind = 4 ) node
  integer ( kind = 4 ) node_new
  integer ( kind = 4 ) node_stop
  integer ( kind = 4 ) used

  used = 0

  do node = 1, n

    first(node) = used + 1

    node_stop = lend(node)
    node_new = node_stop

    used = used + 1
    list(used) = listc(node_new)

    do

      node_new = lptr(node_new)

      if ( node_new == node_stop ) then
        exit
      end if

      used = used + 1
      list(used) = listc(node_new)

    end do

  end do

  first(n+1) = used + 1

  return
end
subroutine voronoi_traverse ( n, x, y, z, xc, yc, zc, lend, listc, lptr )

!*****************************************************************************80
!
!! VORONOI_TRAVERSE traverses the polygons in a Voronoi diagram.
!
!  Discussion:
!
!    STRIPACK defines a data structure recording the location of
!    the vertices of the Voronoi diagram, and their connectivity.
!    The purpose of this routine is to "visit" each polygon, and,
!    in fact, each subtriangle of each polygon.  Such a procedure
!    would be done when estimating an integral by quadrature, for instance.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 June 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes, and Voronoi polygons.
!
!    Input, real ( kind = 8 ) X(N), Y(N), Z(N), the coordinates of the nodes.
!
!    Input, real ( kind = 8 ) XC(6*(N-2)), YC(6*(N-2)), ZC(6*(N-2)), the 
!    coordinates of the vertices.
!
!    Input, integer ( kind = 4 ) LEND(N), points to the "first" vertex in the
!    Voronoi polygon around a particular node.
!
!    Input, integer ( kind = 4 ) LISTC(6*(N-2)), the Voronoi vertex indices.
!
!    Input, integer ( kind = 4 ) LPTR(6*(N-2)), given a vertex, returns the 
!    next vertex in the Voronoi polygon.  (The vertex numbering is done
!    in such a way that the physical vertex has three distince indices,
!    depending on which polygon we are considering.  Thus, it is always
!    possible to answer the question "which is the next vertex from this
!    one?" because the vertex index also tells us what polygon we are in.)
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) area_polygon
  real ( kind = 8 ) area_triangle
  real ( kind = 8 ) areas
  integer ( kind = 4 ) index_polygon
  integer ( kind = 4 ) index_triangle
  integer ( kind = 4 ) lend(n)
  integer ( kind = 4 ) listc(6*(n-2))
  integer ( kind = 4 ) lptr(6*(n-2))
  integer ( kind = 4 ) node
  integer ( kind = 4 ) node_last
  integer ( kind = 4 ) node_new
  integer ( kind = 4 ) node_stop
  integer ( kind = 4 ) order
  real ( kind = 8 ) r
  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)
  integer ( kind = 4 ) vertex_last
  integer ( kind = 4 ) vertex_new
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xc(6*(n-2))
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) yc(6*(n-2))
  real ( kind = 8 ) z(n)
  real ( kind = 8 ) zc(6*(n-2))

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VORONOI_TRAVERSE'
  write ( *, '(a)' ) '  Visit each Voronoi polygon.'
  write ( *, '(a)' ) '  Compute the (spherical) area of each subtriangle'
  write ( *, '(a)' ) '  Add up to get the area of each polygon.'
!
!  To access every polygon, start by accessing a particular node.
!
!  The Voronoi polygon around a node NODE has a pointer LEND(NODE) to the 
!  first (or last) vertex of the Voronoi polygon around NODE.
!
!  To access all the vertices of the polygon in order, start at the
!  special vertex, and then repeatedly use the LPTR array to get the
!  next vertex on the polygon.  Stop when you return to LEND(NODE).
!
!  To subdivide the polygon into triangles, use NODE, VERTEX_LAST,
!  and VERTEX.
!
!  To get the coordinates of these points:
!
!    NODE ==>        X(NODE),         Y(NODE),         Z(NODE).
!
!    VERTEX_LAST ==> XC(VERTEX_LAST), YC(VERTEX_LAST), ZC(VERTEX_LAST)
!    VERTEX      ==> XC(VERTEX     ), YC(VERTEX     ), ZC(VERTEX     ) 
!
  index_polygon = 0

  do node = 1, n

    area_polygon = 0.0D+00
    index_triangle = 0
    order = 0
    
    write ( *, '(a)' ) ' '
    write ( *, '(a,i4)' ) '  Polygon ', node

    node_stop = lend(node)

    node_new = node_stop

    vertex_new = listc(node_new)
!
!  Each iteration of this DO walks along one side of the polygon,
!  considering the subtriangle NODE --> VERTEX_LAST --> VERTEX.
!
    do

      index_triangle = index_triangle + 1
      order = order + 1

      node_last = node_new
      node_new = lptr(node_last)

      vertex_last = vertex_new
      vertex_new = listc(node_new)
!
!  Here is a good place to process information about the polygon side 
!
!   VERTEX_LAST --> VERTEX 
!
!  or about the subtriangle
!
!   NODE --> VERTEX_LAST --> VERTEX.
!
      r = 1.0D+00
      v1(1:3) = (/ x(node),         y(node),         z(node)         /)
      v2(1:3) = (/ xc(vertex_last), yc(vertex_last), zc(vertex_last) /)
      v3(1:3) = (/ xc(vertex_new),  yc(vertex_new),  zc(vertex_new)      /)

      area_triangle = areas ( v1, v2, v3 )

      area_polygon = area_polygon + area_triangle

      write ( *, '(a,2x,i8,2x,a,2x,g14.6)' ) &
        '  Subtriangle ', index_triangle, '  area = ', area_triangle
!
!  Now if we have reached the vertex where we started, we are done with
!  this polygon.
!
      if ( node_new == node_stop ) then
        exit
      end if

    end do

    write ( *, '(a,2x,19x,g14.6)' ) '  Polygon area =', area_polygon

  end do

  return
end
