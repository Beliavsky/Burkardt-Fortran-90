subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 
!    which is not currently associated with an I/O device.  A free FORTRAN unit
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
subroutine r8_to_i4 ( xmin, xmax, x, ixmin, ixmax, ix )

!*****************************************************************************80
!
!! R8_TO_I4 maps X in [XMIN, XMAX] to integer IX in [IXMIN, IXMAX].
!
!  Formula:
!
!    IX := IXMIN + ( IXMAX - IXMIN ) * ( X - XMIN ) / ( XMAX - XMIN )
!    IX := min ( IX, max ( IXMIN, IXMAX ) )
!    IX := max ( IX, min ( IXMIN, IXMAX ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XMIN, XMAX, the range.  XMAX and
!    XMIN must not be equal.  It is not necessary that XMIN be less than XMAX.
!
!    Input, real ( kind = 8 ) X, the number to be converted.
!
!    Input, integer ( kind = 4 ) IXMIN, IXMAX, the allowed range of the output
!    variable.  IXMAX corresponds to XMAX, and IXMIN to XMIN.
!    It is not necessary that IXMIN be less than IXMAX.
!
!    Output, integer ( kind = 4 ) IX, the value in the range [IXMIN,IXMAX] that
!    corresponds to X.
!
  implicit none

  integer ( kind = 4 ) ix
  integer ( kind = 4 ) ixmax
  integer ( kind = 4 ) ixmin
  real ( kind = 8 ) temp
  real ( kind = 8 ) x
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  if ( xmax == xmin ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_TO_I4 - Fatal error!'
    write ( *, '(a)' ) '  XMAX = XMIN, making a zero divisor.'
    write ( *, '(a,g14.6)' ) '  XMAX = ', xmax
    write ( *, '(a,g14.6)' ) '  XMIN = ', xmin
    stop 1
  end if

  temp = &
      ( ( xmax - x        ) * real ( ixmin, kind = 8 )  &
      + (        x - xmin ) * real ( ixmax, kind = 8 ) ) &
      / ( xmax     - xmin )

  if ( 0.0D+00 <= temp ) then
    temp = temp + 0.5D+00
  else
    temp = temp - 0.5D+00
  end if

  ix = int ( temp )

  return
end
subroutine r8mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_PRINT prints an R8MAT.
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
!    12 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME prints some of an R8MAT.
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
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8,6x)' ) j
    end do

    write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( a(i,j) == real ( int ( a(i,j) ), kind = 8 ) ) then
          write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
        else
          write ( ctemp(j2), '(g14.6)' ) a(i,j)
        end if

      end do

      write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end
subroutine s_blank_delete ( s )

!*****************************************************************************80
!
!! S_BLANK_DELETE removes blanks from a string, left justifying the remainder.
!
!  Discussion:
!
!    All TAB characters are also removed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  implicit none

  character ch
  integer ( kind = 4 ) get
  integer ( kind = 4 ) put
  character ( len = * ) s
  integer ( kind = 4 ) s_length
  character, parameter :: tab = achar ( 9 )

  put = 0
  s_length = len_trim ( s )

  do get = 1, s_length

    ch = s(get:get)

    if ( ch /= ' ' .and. ch /= tab ) then
      put = put + 1
      s(put:put) = ch
    end if

  end do

  s(put+1:s_length) = ' '

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
subroutine triangle_svg ( plot_filename, t, p_num, p )

!*****************************************************************************80
!
!! TRIANGLE_SVG plots a triangle and points in SVG format.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) PLOT_FILENAME, the name of the output file.
!
!    Input, real ( kind = 8 ) T(2,3), points forming a triangle.
!
!    Input, integer ( kind = 4 ) P_NUM, the number of points.
!
!    Input, real ( kind = 8 ) P(2,P_NUM), the points.
!
  implicit none

  integer ( kind = 4 ) p_num

  integer ( kind = 4 ) i4
  integer ( kind = 4 ) i4_max
  integer ( kind = 4 ) i4_min
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j4
  integer ( kind = 4 ) j4_max
  integer ( kind = 4 ) j4_min
  integer ( kind = 4 ) output
  real ( kind = 8 ) p(2,p_num)
  character ( len = * ) plot_filename
  integer ( kind = 4 ) r
  character ( len = 255 ) string
  real ( kind = 8 ) t(2,3)
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ) x_scale
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min
  real ( kind = 8 ) y_scale
!
!  Determine SCALE, the maximum data range.
!
  t1 = maxval ( p(1,1:p_num) )
  t2 = maxval ( t(1,1:3) )
  x_max = max ( t1, t2 )
  t1 = minval ( p(1,1:p_num) )
  t2 = minval ( t(1,1:3) )
  x_min = min ( t1, t2 )
  x_scale = x_max - x_min
  x_max = x_max + 0.05D+00 * x_scale
  x_min = x_min - 0.05D+00 * x_scale
  x_scale = x_max - x_min

  t1 = maxval ( p(2,1:p_num) )
  t2 = maxval ( t(2,1:3) )
  y_max = max ( t1, t2 )
  t1 = minval ( p(2,1:p_num) )
  t2 = minval ( t(2,1:3) )
  y_min = min ( t1, t2 )
  y_scale = y_max - y_min
  y_max = y_max + 0.05D+00 * y_scale
  y_min = y_min - 0.05D+00 * y_scale
  y_scale = y_max - y_min

  i4_min = 1
  j4_min = 1
  if ( x_scale < y_scale ) then
    i4_max = int ( 0.5D+00 + 500.0D+00 * x_scale / y_scale )
    j4_max = 500
  else
    i4_max = 500
    j4_max = int ( 0.5D+00 + 500.0D+00 * y_scale / x_scale )
  end if
!
!  Open the file.
!
  call get_unit ( output )

  open ( unit = output, file = plot_filename, status = 'replace', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRIANGLE_SVG - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    stop 1
  end if
!
!  Write that junk.
!
  write ( output, '(a)' ) '<?xml version = "1.0" standalone="no"?>'
  write ( output, '(a)' ) ''
  write ( output, '(a)' ) '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"'
  write ( output, '(a)' ) &
    '  "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'
  write ( output, '(a)' ) ''
  write ( output, '(a)' ) '<svg'
  write ( string, '(a,i3,a)' ) 'width="', i4_max, '"'
  call s_blank_delete ( string )
  write ( output, '(2x,a)' ) trim ( string )

  write ( string, '(a,i3,a)' ) 'height="', j4_max, '"'
  call s_blank_delete ( string )
  write ( output, '(2x,a)' ) trim ( string )

  write ( string, '(a,i3,a1,i3,a1,i3,a1,i3,a)' ) &
    '  viewbox="', i4_min, ',', j4_min, ',',  i4_max, ',', j4_max, '"'
  call s_blank_delete ( string )
  write ( output, '(2x,a)' ) trim ( string )
  write ( output, '(a)' ) '  xmlns="http://www.w3.org/2000/svg"'
  write ( output, '(a)' ) '  version="1.1"'
  write ( output, '(a)' ) '>'
  write ( output, '(a)' ) '  <desc>'
  write ( output, '(a)' ) '    Triangulation created by triangle_svg.f90'
  write ( output, '(a)' ) '  </desc>'
!
!  Draw the triangle.
!
  write ( output, '(a)' ) '  <polygon'
  write ( output, '(a)' ) '    fill="pink"'
  write ( output, '(a)' ) '    stroke="black"'
  write ( output, '(a)' ) '    stroke-width="2"'
  write ( output, '(a)' ) '    points="'

  do j = 1, 3
    call r8_to_i4 ( x_min, x_max, t(1,j), i4_min, i4_max, i4 )
    call r8_to_i4 ( y_max, y_min, t(2,j), j4_min, j4_max, j4 )
    write ( string, '(i3,a,i3)' ) i4, ',', j4
    call s_blank_delete ( string )
    write ( output, '(6x,a)' ) trim ( string )
  end do

  write ( output, '(a)' ) '  "/>'
!
!  Draw points.
!
  do j = 1, p_num

    call r8_to_i4 ( x_min, x_max, p(1,j), i4_min, i4_max, i4 )
    call r8_to_i4 ( y_max, y_min, p(2,j), j4_min, j4_max, j4 )
    r = 5

    write ( output, '(a)' ) '  <circle'
    write ( string, '(a,i3,a)' ) 'cx="', i4, '"'
    call s_blank_delete ( string )
    write ( output, '(4x,a)' ) trim ( string )
    write ( string, '(a,i3,a)' ) 'cy="', j4, '"'
    call s_blank_delete ( string )
    write ( output, '(4x,a)' ) trim ( string )
    write ( string, '(a,i3,a)' ) 'r="', r, '"'
    call s_blank_delete ( string )
    write ( output, '(4x,a)' ) trim ( string )
    write ( output, '(a)' ) '    fill="blue"'
    write ( output, '(a)' ) '    stroke="black"'
    write ( output, '(a)' ) '    stroke-width="2"'
    write ( output, '(a)' ) '  />'

  end do
!
!  End of plot.
!
  write ( output, '(a)' ) '</svg>'

  close ( unit = output )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Graphics data written to file "' &
    // trim ( plot_filename ) // '"'

  return
end
