program main

!*****************************************************************************80
!
!! MAIN is the main program for GMSH_TO_FEM.
!
!  Discussion:
!
!    GMSH_TO_FEM converts mesh data from GMSH to FEM format.
!
!  Usage:
!
!    gmsh_to_fem prefix
!
!    where 'prefix' is the common filename prefix:
!
!    * 'prefix'.msh contains the Gmsh mesh file.
!    * 'prefix'_nodes.txt will contain the node coordinates.
!    * 'prefix'_elements.txt will contain the element node connectivity.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) arg_num
  integer ( kind = 4 ), allocatable :: element_node(:,:)
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order
  character ( len = 255 ) fem_element_filename
  character ( len = 255 ) fem_node_filename
  character ( len = 255 ) gmsh_filename
  integer ( kind = 4 ) iarg
  integer ( kind = 4 ) iargc
  integer ( kind = 4 ) m
  integer ( kind = 4 ) node_num;
  real ( kind = 8 ), allocatable :: node_x(:,:)
  character ( len = 255 ) prefix

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GMSH_TO_FEM'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Read a mesh description created by GMSH:'
  write ( *, '(a)' ) '  * "prefix".msh, the mesh data file.'
  write ( *, '(a)' ) '  Write two simple FEM files listing nodes and elements.'
  write ( *, '(a)' ) '  * "prefix"_nodes.txt, node coordinates.'
  write ( *, '(a)' ) '  * "prefix"_elements.txt, element connectivity.'
!
!  Get the number of command line arguments.
!
  arg_num = iargc ( )
!
!  Get the filename prefix.
!
  if ( arg_num < 1 ) then

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Please enter the filename prefix.'

    read ( *, '(a)' ) prefix

  else 

    iarg = 1
    call getarg ( iarg, prefix )

  end if
!
!  Create the filenames.
!
  gmsh_filename = trim ( prefix ) // '.msh'
  fem_node_filename = trim ( prefix ) // '_nodes.txt'
  fem_element_filename = trim ( prefix ) // '_elements.txt'
!
!  Read GMSH sizes.
!
  call gmsh_size_read ( gmsh_filename, node_num, m, element_num, &
    element_order )
!
!  Report sizes.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Size information from GMSH files:'
  write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
  write ( *, '(a,i4)' ) '  Number of nodes NODE_NUM = ', node_num
  write ( *, '(a,i4)' ) '  Number of elements ELEMENT_NUM = ', element_num
  write ( *, '(a,i4)' ) '  Element order ELEMENT_ORDER = ', element_order
!
!  Allocate memory.
!
  allocate ( node_x(m,node_num) )
  allocate ( element_node(1:element_order,1:element_num) )
!
!  Read GMSH data.
!
  call gmsh_data_read ( gmsh_filename, m, node_num, node_x, &
    element_order, element_num, element_node )
!
!  Write data.
!
  call r8mat_write ( fem_node_filename, m, node_num, node_x )

  call i4mat_write ( fem_element_filename, element_order, element_num, &
    element_node )
!
!  Free memory.
!
  deallocate ( element_node )
  deallocate ( node_x )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GMSH_TO_FEM:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop
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
subroutine gmsh_data_read ( gmsh_filename, node_dim, node_num, node_x, &
  element_order, element_num, element_node )

!*****************************************************************************80
!
!! GMSH_DATA_READ reads sizes from a GMSH file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) GMSH_FILENAME, the GMSH filename.
!
!    Input, integer ( kind = 4 ) NODE_DIM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_X(NODE_DIM,NODE_NUM), the node coordinates.
!
!    Input, integer ( kind = 4 ) ELEMENT_ORDER, the order of the elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
!    the nodes that make up each element.
!

  implicit none

  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order
  integer ( kind = 4 ) node_dim
  integer ( kind = 4 ) node_num

  integer ( kind = 4 ) element_node(element_order,element_num)
  character ( len = * ) gmsh_filename
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_dummy
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) input
  integer ( kind = 4 ) input_stat
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) length
  integer ( kind = 4 ) level
  real ( kind = 8 ) node_x(node_dim,node_num)
  real ( kind = 8 ), parameter :: r8_big = 1.0D+30
  logical s_begin
  character ( len = 255 ) text
  real ( kind = 8 ) x

  call get_unit ( input )

  open ( unit = input, file = gmsh_filename, status = 'old', &
    iostat = input_stat )

  if ( input_stat /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
    write ( *, '(a)' ) '  Could not open input file "' // &
      trim ( gmsh_filename ) // '"'
    stop 1
  end if

  level = 0

  do
    read ( input, '(a)', iostat = input_stat ) text

    if ( input_stat /= 0 ) then
      write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
      write ( *, '(a)' ) '  Error while seeking node coordinates.'
      stop 1
    end if

    if ( level == 0 ) then
      if ( s_begin ( text(1:6), '$Nodes' ) ) then
        level = 1
        j = 0
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, i4_dummy, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:9), '$EndNodes' ) ) then
        exit
      else
        j = j + 1
        call s_to_i4 ( text, indx, ierror, length )
        text = text(length+1:)
        do i = 1, node_dim
          call s_to_r8 ( text, x, ierror, length )
          text = text(length+1:)
          node_x(i,j) = x
        end do      
      end if
    end if

  end do
!
!  Now read element information.
!
  level = 0

  do

    read ( input, '(a)', iostat = input_stat ) text

    if ( input_stat /= 0 ) then
      write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
      write ( *, '(a)' ) '  Error while seeking element connectivity.'
      stop 1
    end if

    if ( level == 0 ) then
      if ( s_begin ( text(1:9), '$Elements' ) ) then
        level = 1
        j = 0
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, i4_dummy, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:12), '$EndElements' ) ) then
        exit
      else
        j = j + 1
        k = 0
        do k = 1, 5
          call s_to_i4 ( text, i4_dummy, ierror, length )
          text = text(length+1:)
        end do
        do i = 1, element_order
          call s_to_i4 ( text, k, ierror, length )
          text = text(length+1:)
          element_node(i,j) = k
        end do
      end if
    end if

  end do

  close ( unit = input )

  return
end
subroutine gmsh_size_read ( gmsh_filename, node_num, node_dim, element_num, &
  element_order )

!*****************************************************************************80
!
!! GMSH_SIZE_READ reads sizes from a GMSH file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) GMSH_FILENAME, the GMSH filename.
!
!    Output, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Output, integer ( kind = 4 ) NODE_DIM, the spatial dimension.
!
!    Output, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Output, integer ( kind = 4 ) ELEMENT_ORDER, the order of the elements.
!
  implicit none

  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order
  character ( len = * ) gmsh_filename
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) input
  integer ( kind = 4 ) input_stat
  integer ( kind = 4 ) k
  integer ( kind = 4 ) length
  integer ( kind = 4 ) level
  integer ( kind = 4 ) node_dim
  integer ( kind = 4 ) node_num
  real ( kind = 8 ), parameter :: r8_big = 1.0D+30
  logical s_begin
  character ( len = 255 ) text
  real ( kind = 8 ) x
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ) y
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min
  real ( kind = 8 ) z
  real ( kind = 8 ) z_max
  real ( kind = 8 ) z_min

  node_num = 0
  node_dim = 0

  x_max = - r8_big
  x_min = + r8_big
  y_max = - r8_big
  y_min = + r8_big
  z_max = - r8_big
  z_min = + r8_big

  call get_unit ( input )

  open ( unit = input, file = gmsh_filename, status = 'old', &
    iostat = input_stat )

  if ( input_stat /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GMSH_SIZE_READ - Fatal error!'
    write ( *, '(a)' ) '  Could not open input file "' // &
      trim ( gmsh_filename ) // '"'
    stop 1
  end if

  level = 0

  do

    read ( input, '(a)', iostat = input_stat ) text

    if ( level == 0 ) then
      if ( s_begin ( text(1:6), '$Nodes' ) ) then
        level = 1
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, node_num, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:9), '$EndNodes' ) ) then
        exit
      else
        call s_to_i4 ( text, indx, ierror, length )
        text = text(length+1:)
        call s_to_r8 ( text, x, ierror, length )
        x_min = min ( x_min, x )
        x_max = max ( x_max, x )
        text = text(length+1:)
!
!  Need to check that we actually were able to read an R8 here.
!
        call s_to_r8 ( text, y, ierror, length )
        y_min = min ( y_min, y )
        y_max = max ( y_max, y )
        text = text(length+1:)
        call s_to_r8 ( text, z, ierror, length )
        text = text(length+1:)
        z_min = min ( z_min, z )
        z_max = max ( z_max, z )
      end if
    end if

  end do
!
!  Make a very simple guess as to the dimensionality of the data.
!
  node_dim = 3
  if ( z_max == z_min ) then
    node_dim = 2
    if ( y_max == y_min ) then
      node_dim = 1
    end if
  end if
!
!  Now read element information.
!
  level = 0

  do

    read ( input, '(a)', iostat = input_stat ) text

    if ( level == 0 ) then
      if ( s_begin ( text(1:9), '$Elements' ) ) then
        level = 1
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, element_num, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:12), '$EndElements' ) ) then
        exit
      else
        k = 0
        do 
          call s_to_i4 ( text, indx, ierror, length )
          text = text(length+1:)
          if ( ierror /= 0 ) then
            exit
          end if
          k = k + 1
        end do
        element_order = k - 5
        exit
      end if
    end if

  end do

  close ( unit = input )

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
    stop 1
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
function s_begin ( s1, s2 )

!*****************************************************************************80
!
!! S_BEGIN is TRUE if one string matches the beginning of the other.
!
!  Discussion:
!
!    The strings are compared, ignoring blanks, spaces and capitalization.
!
!  Example:
!
!     S1              S2      S_BEGIN
!
!    'Bob'          'BOB'     TRUE
!    '  B  o b '    ' bo b'   TRUE
!    'Bob'          'Bobby'   TRUE
!    'Bobo'         'Bobb'    FALSE
!    ' '            'Bob'     FALSE    (Do not allow a blank to match
!                                       anything but another blank string.)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to be compared.
!
!    Output, logical ( kind = 4 ) S_BEGIN, is TRUE if the strings match up to
!    the end of the shorter string, ignoring case.
!
  implicit none

  logical ( kind = 4 ) ch_eqi
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  logical ( kind = 4 ) s_begin
  character ( len = * ) s1
  integer ( kind = 4 ) s1_length
  character ( len = * ) s2
  integer ( kind = 4 ) s2_length

  s1_length = len_trim ( s1 )
  s2_length = len_trim ( s2 )
!
!  If either string is blank, then both must be blank to match.
!  Otherwise, a blank string matches anything, which is not
!  what most people want.
!
  if ( s1_length == 0 .or. s2_length == 0 ) then

    if ( s1_length == 0 .and. s2_length == 0 ) then
      s_begin = .true.
    else
      s_begin = .false.
    end if

    return

  end if

  i1 = 0
  i2 = 0
!
!  Find the next nonblank in S1.
!
  do

    do

      i1 = i1 + 1

      if ( s1_length < i1 ) then
        s_begin = .true.
        return
      end if

      if ( s1(i1:i1) /= ' ' ) then
        exit
      end if

    end do
!
!  Find the next nonblank in S2.
!
    do

      i2 = i2 + 1

      if ( s2_length < i2 ) then
        s_begin = .true.
        return
      end if

      if ( s2(i2:i2) /= ' ' ) then
        exit
      end if

    end do
!
!  If the characters match, get the next pair.
!
    if ( .not. ch_eqi ( s1(i1:i1), s2(i2:i2) ) ) then
      exit
    end if

  end do

  s_begin = .false.

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
  logical ( kind = 4 ) ch_eqi
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
