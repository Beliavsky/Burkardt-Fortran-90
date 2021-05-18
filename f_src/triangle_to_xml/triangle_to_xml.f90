program main

!*****************************************************************************80
!
!! MAIN is the main program for TRIANGLE_TO_XML.
!
!  Discussion:
!
!    TRIANGLE_TO_XML converts mesh data from TRIANGLE format to XML format.
!
!  Usage:
!
!    triangle_to_xml prefix
!
!    where 'prefix' is the common filename prefix:
!
!    * 'prefix'.node contains the triangle node coordinates,
!    * 'prefix'.ele contains the triangle element node connectivity.
!    * 'prefix'.xml will contain the XML mesh file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) arg_num
  real ( kind = 8 ), allocatable :: element_att(:,:)
  integer ( kind = 4 ) element_att_num
  integer ( kind = 4 ), allocatable :: element_node(:,:)
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order
  integer ( kind = 4 ) iarg
  integer ( kind = 4 ) iargc
  integer ( kind = 4 ) m
  real ( kind = 8 ), allocatable :: node_att(:,:)
  integer ( kind = 4 ) node_att_num
  integer ( kind = 4 ), allocatable :: node_marker(:,:)
  integer ( kind = 4 ) node_marker_num
  integer ( kind = 4 ) node_num;
  real ( kind = 8 ), allocatable :: node_x(:,:)
  character ( len = 255 ) prefix
  character ( len = 255 ) triangle_element_filename
  character ( len = 255 ) triangle_node_filename
  character ( len = 255 ) xml_filename

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_TO_XML'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Read a mesh description created by TRIANGLE:'
  write ( *, '(a)' ) '  * "prefix".node, node coordinates.'
  write ( *, '(a)' ) '  * "prefix".ele, element connectivity.'
  write ( *, '(a)' ) '  Write a corresponding XML mesh file.'
  write ( *, '(a)' ) '  * "prefix".xml, element connectivity.'
!
!  Get the number of command line arguments.
!
  arg_num = iargc ( )
!
!  Get the filename prefix.
!
  if ( arg_num < 1 ) then

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRIANGLE_TO_XML:'
    write ( *, '(a)' ) '  Please enter the filename prefix.'

    read ( *, '(a)' ) prefix

  else 

    iarg = 1
    call getarg ( iarg, prefix )

  end if
!
!  Create the filenames.
!
  triangle_node_filename = trim ( prefix ) // '.node'
  triangle_element_filename = trim ( prefix ) // '.ele'
  xml_filename = trim ( prefix ) // '.xml'
!
!  Read the triangle node size information.
!
  call triangle_node_size_read ( triangle_node_filename, node_num, m, node_att_num, &
    node_marker_num )
!
!  Read the triangle element size information.
!
  call triangle_element_size_read ( triangle_element_filename, element_num, &
    element_order, element_att_num )
!
!  Report sizes.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Size information from TRIANGLE files:'
  write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
  write ( *, '(a,i4)' ) '  Number of nodes NODE_NUM = ', node_num
  write ( *, '(a,i4)' ) '  NODE_ATT_NUM = ', node_att_num
  write ( *, '(a,i4)' ) '  NODE_MARKER_NUM = ', node_marker_num
  write ( *, '(a,i4)' ) '  Number of elements ELEMENT_NUM = ', element_num
  write ( *, '(a,i4)' ) '  Element order ELEMENT_ORDER = ', element_order
  write ( *, '(a,i4)' ) '  ELEMENT_ATT_NUM = ', element_att_num
!
!  Allocate memory.
!
  allocate ( node_att(1:node_att_num,1:node_num) )
  allocate ( node_marker(1:node_marker_num,1:node_num) )
  allocate ( node_x(m,node_num) )
  allocate ( element_node(1:element_order,1:element_num) )
  allocate ( element_att(1:element_att_num,1:element_num) )
!
!  Read TRIANGLE data.
!
  call triangle_node_data_read ( triangle_node_filename, node_num, m, node_att_num, &
    node_marker_num, node_x, node_att, node_marker )

  call triangle_element_data_read ( triangle_element_filename, element_num, &
    element_order, element_att_num, element_node, element_att )
!
!  Write XML data.
!
  call xml_mesh2d_write ( xml_filename, m, node_num, node_x, &
    element_order, element_num, element_node )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Created XML file "' // trim ( xml_filename ) // '".'
!
!  Free memory.
!
  deallocate ( element_att )
  deallocate ( element_node )
  deallocate ( node_att )
  deallocate ( node_marker )
  deallocate ( node_x )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_TO_FEM:'
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
subroutine mesh_base_zero ( node_num, element_order, element_num, element_node )

!*****************************************************************************80
!
!! MESH_BASE_ZERO ensures that the element definition is zero-based.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 October 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, integer ( kind = 4 ) ELEMENT_ORDER, the order of the elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Input/output, integer ( kind = 4 ) ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM),
!    the element definitions.
!
  implicit none

  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order

  integer ( kind = 4 ) element_node(element_order,element_num)
  integer ( kind = 4 ) node_max
  integer ( kind = 4 ) node_min
  integer ( kind = 4 ) node_num

  node_min = node_num + 1
  node_max = -1

  node_min = minval ( element_node(1:element_order,1:element_num) )
  node_max = maxval ( element_node(1:element_order,1:element_num) )

  if ( node_min == 0 .and. node_max == node_num - 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' )'MESH_BASE_ZERO:'
    write ( *, '(a)' )'  The element indexing appears to be 0-based!'
    write ( *, '(a)' )'  No conversion is necessary.'
  else if ( node_min == 1 .and. node_max == node_num  ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' )'MESH_BASE_ZERO:'
    write ( *, '(a)' )'  The element indexing appears to be 1-based!'
    write ( *, '(a)' )'  This will be converted to 0-based.'
    element_node(1:element_order,1:element_num) = &
      element_node(1:element_order,1:element_num) - 1
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MESH_BASE_ZERO - Warning!'
    write ( *, '(a)' ) '  The element indexing is not of a recognized type.'
    write ( *, '(a,i8)' ) '  NODE_MIN = ', node_min
    write ( *, '(a,i8)' ) '  NODE_MAX = ', node_max
    write ( *, '(a,i8)' ) '  NODE_NUM = ', node_num
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
subroutine s_to_i4vec ( s, n, ivec, ierror )

!*****************************************************************************80
!
!! S_TO_I4VEC reads an I4VEC from a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2003
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
!    Output, integer ( kind = 4 ) IVEC(N), the values read from the string.
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
  integer ( kind = 4 ) ivec(n)
  integer ( kind = 4 ) length
  character ( len = * ) s

  i = 0
  ierror = 0
  ilo = 1

  do while ( i < n )

    i = i + 1

    call s_to_i4 ( s(ilo:), ivec(i), ierror, length )

    if ( ierror /= 0 ) then
      ierror = -i
      exit
    end if

    ilo = ilo + length

  end do

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
subroutine triangle_element_data_read ( element_filename, element_num, &
  element_order, element_att_num, element_node, element_att )

!*****************************************************************************80
!
!! TRIANGLE_ELEMENT_DATA_READ reads the data from an element file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) ELEMENT_FILENAME, the name of the file.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_ORDER, the order of the elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_ATT_NUM, number of element attributes 
!    listed on each node record.
!
!    Output, integer ( kind = 4 ) ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
!    the indices of the nodes that make up each element.
!
!    Output, real ( kind = 8 ) ELEMENT_ATT(ELEMENT_ATT_NUM,ELEMENT_NUM), the 
!    attributes of each element.
!
  implicit none

  integer ( kind = 4 ) element_att_num
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order

  integer ( kind = 4 ) element
  real ( kind = 8 ) element_att(element_att_num,element_num)
  character ( len = * ) element_filename
  integer ( kind = 4 ) element_node(element_order,element_num)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) input
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) length
  character ( len = 255 ) text
  real ( kind = 8 ) value

  element = 0

  call get_unit ( input )

  open ( unit = input, file = element_filename, status = 'old', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRIANGLE_ELEMENT_DATA_READ - Fatal error!'
    write ( *, '(a)' ) '  Unable to open file.'
    stop 1
  end if

  do

    read ( input, '(a)', iostat = ios ) text

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TRIANGLE_ELEMENT_DATA_READ - Fatal error!'
      write ( *, '(a)' ) '  Unexpected end of file while reading.'
      stop 1
    end if

    if ( len_trim ( text ) == 0 ) then
      cycle
    end if

    if ( text(1:1) == '#' ) then
      cycle
    end if

    if ( element == 0 ) then

      call s_to_i4 ( text, i1, ierror, length )
      text = text(length+1:)
      call s_to_i4 ( text, i2, ierror, length )
      text = text(length+1:)
      call s_to_i4 ( text, i3, ierror, length )
      text = text(length+1:)

    else

      call s_to_i4 ( text, ival, ierror, length )
      text = text(length+1:)

      do i = 1, element_order
        call s_to_i4 ( text, ival, ierror, length )
        text = text(length+1:)
        element_node(i,element) = ival
      end do

      do i = 1, element_att_num
        call s_to_r8 ( text, value, length, ierror )
        text = text(length+1:)
        element_att(i,element) = value
      end do

    end if

    element = element + 1

    if ( element_num < element ) then
      exit
    end if

  end do

  close ( unit = input )

  return
end
subroutine triangle_element_size_read ( element_filename, element_num, &
  element_order, element_att_num )

!*****************************************************************************80
!
!! TRIANGLE_ELEMENT_SIZE_READ reads the header information from an element file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) ELEMENT_FILENAME, the name of the 
!    element file.
!
!    Output, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Output, integer ( kind = 4 ) ELEMENT_ORDER, the order of the elements.
!
!    Output, integer ( kind = 4 ) ELEMENT_ATT_NUM, the number of 
!    element attributes.
!
  implicit none

  integer ( kind = 4 ) element_att_num
  character ( len = * ) element_filename
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) input
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) length
  character ( len = 255 ) text

  call get_unit ( input )

  open ( unit = input, file = element_filename, status = 'old', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRIANGLE_ELEMENT_SIZE_READ - Fatal error!'
    write ( *, '(a)' ) '  Unable to open file.'
    stop 1
  end if

  do

    read ( input, '(a)', iostat = ios ) text

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ELEMENT_SIZE_READ - Fatal error!'
      write ( *, '(a)' ) '  Unexpected end of file while reading.'
      stop 1
    end if

    if ( len_trim ( text ) == 0 ) then
      cycle
    end if

    if ( text(1:1) == '#' ) then
      cycle
    end if

    call s_to_i4 ( text, element_num, ierror, length )
    text = text(length+1:)
    call s_to_i4 ( text, element_order, ierror, length )
    text = text(length+1:)
    call s_to_i4 ( text, element_att_num, ierror, length )
    text = text(length+1:)

    exit

  end do

  close ( unit = input )

  return
end
subroutine triangle_node_data_read ( node_filename, node_num, node_dim, &
  node_att_num, node_marker_num, node_coord, node_att, node_marker )

!*****************************************************************************80
!
!! TRIANGLE_NODE_DATA_READ reads the data from a node file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
! Parameters:
!
!   Input, character ( len = * ) NODE_FILENAME, the name of the node file.
!
!   Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!   Input, integer ( kind = 4 ) NODE_DIM, the spatial dimension.
!
!   Input, integer ( kind = 4 ) NODE_ATT_NUM, number of node attributes 
!   listed on each node record.
!
!   Input, integer ( kind = 4 ) NODE_MARKER_NUM, 1 if every node record 
!   includes a final boundary marker value.
!
!   Output, real ( kind = 8 ) NODE_COORD(NODE_DIM,NODE_NUM), the nodal 
!   coordinates.
!
!   Output, real ( kind = 8 ) NODE_ATT(NODE_ATT_NUM,NODE_NUM), the nodal 
!   attributes.
!
!   Output, integer ( kind = 4 ) NODE_MARKER(NODE_MARKER_NUM,NODE_NUM), the 
!   node markers.
!
  implicit none

  integer ( kind = 4 ) node_att_num
  integer ( kind = 4 ) node_dim
  integer ( kind = 4 ) node_marker_num
  integer ( kind = 4 ) node_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) input
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) length
  integer ( kind = 4 ) node
  real ( kind = 8 ) node_att(node_att_num,node_num)
  real ( kind = 8 ) node_coord(node_dim,node_num)
  character ( len = * ) node_filename
  integer ( kind = 4 ) node_marker(node_marker_num,node_num)
  character ( len = 255 ) text
  real ( kind = 8 ) value

  node = 0

  call get_unit ( input )

  open ( unit = input, file = node_filename, status = 'old', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRIANGLE_NODE_DATA_READ - Fatal error!'
    write ( *, '(a)' ) '  Unable to open file.'
    stop 1
  end if

  do

    read ( input, '(a)', iostat = ios ) text

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TRIANGLE_NODE_DATA_READ - Fatal error!'
      write ( *, '(a)' ) '  Unexpected end of file while reading.'
      stop 1
    end if

    if ( len_trim ( text ) == 0 ) then
      cycle
    end if

    if ( text(1:1) == '#' ) then
      cycle
    end if
!
!  Ignore the dimension line.
!
    if ( node == 0 ) then

    else

      call s_to_i4 ( text, ival, ierror, length )
      text = text(length+1:)

      do i = 1, node_dim
        call s_to_r8 ( text, value, ierror, length )
        text = text(length+1:)
        node_coord(i,node) = value
      end do

      do i = 1, node_att_num
        call s_to_r8 ( text, value, ierror, length )
        text = text(length+1:)
        node_att(i,node) = value;
      end do

      do i = 1, node_marker_num
        call s_to_i4 ( text, ival, ierror, length )
        text = text(length+1:)
        node_marker(i,node) = ival
      end do

    end if

    node = node + 1

    if ( node_num < node ) then
      exit
    end if

  end do

  close ( unit = input )

  return
end
subroutine triangle_node_size_read ( node_filename, node_num, node_dim, &
  node_att_num, node_marker_num )

!*****************************************************************************80
!
!! TRIANGLE_NODE_SIZE_READ reads the header information from a node file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) NODE_FILENAME, the name of the node file.
!
!    Output, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Output, integer ( kind = 4 ) NODE_DIM, the spatial dimension.
!
!    Output, integer ( kind = 4 ) NODE_ATT_NUM, number of node attributes 
!    listed on each node record.
!
!    Output, integer ( kind = 4 ) NODE_MARKER_NUM, 1 if every node record 
!    includes a final boundary marker value.
!
  implicit none

  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) input
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) length
  integer ( kind = 4 ) node_att_num
  integer ( kind = 4 ) node_dim
  character ( len = * ) node_filename
  integer ( kind = 4 ) node_marker_num
  integer ( kind = 4 ) node_num
  character ( len = 255 ) text

  call get_unit ( input )

  open ( unit = input, file = node_filename, status = 'old', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRIANGLE_NODE_SIZE_READ - Fatal error!'
    write ( *, '(a)' ) '  Unable to open file.'
    stop 1
  end if

  do

    read ( input, '(a)', iostat = ios ) text

    if ( ios /= 0 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TRIANGLE_NODE_SIZE_READ - Fatal error!'
      write ( *, '(a)' ) '  Unexpected end of file while reading.'
      stop 1
    end if

    if ( len_trim ( text ) == 0 ) then
      cycle
    end if

    if ( text(1:1) == '#' ) then
      cycle
    end if

    call s_to_i4 ( text, node_num, ierror, length )
    text = text(length+1:)
    call s_to_i4 ( text, node_dim, ierror, length )
    text = text(length+1:)
    call s_to_i4 ( text, node_att_num, ierror, length )
    text = text(length+1:)
    call s_to_i4 ( text, node_marker_num, ierror, length )
    text = text(length+1:)

    exit

  end do

  close ( unit = input )

  return
end
subroutine xml_mesh2d_write ( xml_filename, m, node_num, node_x, &
  element_order, element_num, element_node )

!*****************************************************************************80
!
!! XML_MESH2D_WRITE writes 2D mesh data as a DOLFIN XML mesh file.
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
!  Reference:
!
!    Anders Logg, Kent-Andre Mardal, Garth Wells,
!    Automated Solution of Differential Equations by the Finite Element
!    Method: The FEniCS Book,
!    Lecture Notes in Computational Science and Engineering,
!    Springer, 2011,
!    ISBN13: 978-3642230981
!
!  Parameters:
!
!    Input, character * ( * ) XML_FILENAME, the name of the XML file 
!    to create.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_XY(M,NODE_NUM), the node coordinates.
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
  integer ( kind = 4 ) m
  integer ( kind = 4 ) node_num

  integer ( kind = 4 ) element
  integer ( kind = 4 ) element_node(element_order,element_num)
  character * ( * ) xml_filename
  integer ( kind = 4 ) xml_unit
  integer ( kind = 4 ) node
  real ( kind = 8 ) node_x(m,node_num)
  character ( len = 80 ) s1
  character ( len = 80 ) s2
  character ( len = 80 ) s3
  character ( len = 80 ) s4
!
!  Force 0-based indexing.
!
  call mesh_base_zero ( node_num, element_order, element_num, element_node )
!
!  Open the file.
!
  call get_unit ( xml_unit )

  open ( unit = xml_unit, file = xml_filename, status = 'replace' )
!
!  Write the data.
!
  write ( xml_unit, '(a)' ) '<?xml version="1.0" encoding="UTF-8"?>'
  write ( xml_unit, '(a)' ) ''
  write ( xml_unit, '(a)' ) '<dolfin xmlns:dolfin="http://www.fenics.org/dolfin/">'
  write ( s1, '(i1) ' ) m
  s1 = adjustl ( s1 )
  write ( xml_unit, '(a)' ) '  <mesh celltype="triangle" dim="' // trim ( s1 ) // '">'

  write ( s1, '(i6)' ) node_num
  s1 = adjustl ( s1 )
  write ( xml_unit, '(a)' ) '    <vertices size="' // trim ( s1 ) // '">'

  do node = 1, node_num

    write ( s1, '(i6)' ) node - 1
    s1 = adjustl ( s1 )
    write ( s2, '(g14.6)' ) node_x(1,node)
    s2 = adjustl ( s2 )
    write ( s3, '(g14.6)' ) node_x(2,node)
    s3 = adjustl ( s3 )
    write ( xml_unit, '(a)' ) '      <vertex index ="' // trim ( s1 ) // &
      '" x ="' // trim ( s2 ) // '" y ="' // trim ( s3 ) // '"/>'

  end do
  write ( xml_unit, '(a)' ) '    </vertices>'

  write ( s1, '(i6)' ) element_num
  s1 = adjustl ( s1 )
  write ( xml_unit, '(a)' ) '    <cells size="' // trim ( s1 ) // '">'

  do element = 1, element_num

    write ( s1, '(i6)' ) element - 1
    s1 = adjustl ( s1 )
    write ( s2, '(i6)' ) element_node(1,element)
    s2 = adjustl ( s2 )
    write ( s3, '(i6)' ) element_node(2,element)
    s3 = adjustl ( s3 )
    write ( s4, '(i6)' ) element_node(3,element)
    s4 = adjustl ( s4 )

    write ( xml_unit, '(a)' ) &
      '      <triangle index ="' // trim ( s1 ) // &
      '" v0 ="' // trim ( s2 ) // &
      '" v1 ="' // trim ( s3 ) // &
      '" v2 ="' // trim ( s4 ) // '"/>'

  end do
  write ( xml_unit, '(a)' ) '    </cells>'
  write ( xml_unit, '(a)' ) '  </mesh>'
  write ( xml_unit, '(a)' ) '</dolfin>'

  close ( unit = xml_unit )

  return
end
