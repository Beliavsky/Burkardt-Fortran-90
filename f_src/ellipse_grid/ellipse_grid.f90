subroutine ellipse_grid ( n, r, c, ng, xy )

!*****************************************************************************80
!
!! ELLIPSE_GRID generates grid points inside an ellipse.
!
!  Discussion:
!
!    The ellipse is specified as
!
!      ( ( X - C1 ) / R1 )^2 + ( ( Y - C2 ) / R2 )^2 = 1
!
!    The user supplies a number N.  There will be N+1 grid points along
!    the shorter axis.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!
!    Input, real ( kind = 8 ) R(2), the half axis lengths.
!
!    Input, real ( kind = 8 ) C(2), the center of the ellipse.
!
!    Input, integer ( kind = 8 ) NG, the number of grid points 
!    inside the ellipse.
!
!    Output, real ( kind = 8 ) XY(2,NG), the grid points.
!
  implicit none

  integer ( kind = 4 ) ng

  real ( kind = 8 ) c(2)
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_ceiling
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ni
  integer ( kind = 4 ) nj
  integer ( kind = 4 ) p
  real ( kind = 8 ) r(2)
  real ( kind = 8 ) x
  real ( kind = 8 ) xy(2,ng)
  real ( kind = 8 ) y

  if ( r(1) < r(2) ) then
    h = 2.0D+00 * r(1) / real ( 2 * n + 1, kind = 8 )
    ni = n
    nj = i4_ceiling ( r(2) / r(1) ) * n
  else
    h = 2.0D+00 * r(2) / real ( 2 * n + 1, kind = 8 )
    nj = n
    ni = i4_ceiling ( r(1) / r(2) ) * n
  end if

  p = 0

  do j = 0, nj

    i = 0
    x = c(1)
    y = c(2) + real ( j, kind = 8 ) * h
    p = p + 1
    xy(1,p) = x
    xy(2,p) = y

    if ( 0 < j ) then
      p = p + 1
      xy(1,p) = x
      xy(2,p) = 2.0D+00 * c(2) - y
    end if

    do

      i = i + 1
      x = c(1) + real ( i, kind = 8 ) * h

      if ( 1.0D+00 < ( ( x - c(1) ) / r(1) ) ** 2 &
                   + ( ( y - c(2) ) / r(2) ) ** 2 ) then
        exit
      end if

      p = p + 1
      xy(1,p) = x
      xy(2,p) = y
      p = p + 1
      xy(1,p) = 2.0D+00 * c(1) - x
      xy(2,p) = y

      if ( 0 < j ) then
        p = p + 1
        xy(1,p) = x
        xy(2,p) = 2.0D+00 * c(2) - y
        p = p + 1
        xy(1,p) = 2.0D+00 * c(1) - x
        xy(2,p) = 2.0D+00 * c(2) - y
      end if

    end do

  end do

  return
end
subroutine ellipse_grid_count ( n, r, c, ng )

!*****************************************************************************80
!
!! ELLIPSE_GRID_COUNT counts the grid points inside an ellipse.
!
!  Discussion:
!
!    The ellipse is specified as
!
!      ( ( X - C1 ) / R1 )^2 + ( ( Y - C2 ) / R2 )^2 = 1
!
!    The user supplies a number N.  There will be N+1 grid points along
!    the shorter axis.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!
!    Input, real ( kind = 8 ) R(2), the half axis lengths.
!
!    Input, real ( kind = 8 ) C(2), the center of the ellipse.
!
!    Output, integer ( kind = 4 ) NG, the number of grid points inside 
!    the ellipse.
!
  implicit none

  real ( kind = 8 ) c(2)
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_ceiling
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ng
  integer ( kind = 4 ) ni
  integer ( kind = 4 ) nj
  integer ( kind = 4 ) p
  real ( kind = 8 ) r(2)
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( r(1) < r(2) ) then
    h = 2.0D+00 * r(1) / real ( 2 * n + 1, kind = 8 )
    ni = n
    nj = i4_ceiling ( r(2) / r(1) ) * n
  else
    h = 2.0D+00 * r(2) / real ( 2 * n + 1, kind = 8 )
    nj = n
    ni = i4_ceiling ( r(1) / r(2) ) * n
  end if

  p = 0

  do j = 0, nj

    i = 0
    x = c(1)
    y = c(2) + real ( j, kind = 8 ) * h
    p = p + 1

    if ( 0 < j ) then
      p = p + 1
    end if

    do

      i = i + 1
      x = c(1) + real ( i, kind = 8 ) * h

      if ( 1.0D+00 < ( ( x - c(1) ) / r(1) ) ** 2 &
                   + ( ( y - c(2) ) / r(2) ) ** 2 ) then
        exit
      end if

      p = p + 1
      p = p + 1
 
      if ( 0 < j ) then
        p = p + 1
        p = p + 1
      end if

    end do

  end do

  ng = p

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
function i4_ceiling ( r )

!*****************************************************************************80
!
!! I4_CEILING rounds an R8 "up" (towards +oo) to the next I4.
!
!  Example:
!
!    R     Value
!
!   -1.1  -1
!   -1.0  -1
!   -0.9   0
!    0.0   0
!    5.0   5
!    5.1   6
!    5.9   6
!    6.0   6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the value to be rounded up.
!
!    Output, integer ( kind = 4 ) I4_CEILING, the rounded value.
!
  implicit none

  integer ( kind = 4 ) i4_ceiling
  real ( kind = 8 ) r
  integer ( kind = 4 ) value

  value = int ( r )
  if ( real ( value, kind = 8 ) < r ) then
    value = value + 1
  end if

  i4_ceiling = value

  return
end
subroutine r82vec_print_part ( n, a, max_print, title )

!*****************************************************************************80
!
!! R82VEC_PRINT_PART prints "part" of an R82VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, real ( kind = 8 ) A(2,N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * )  title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)
    end do
    write ( *, '(a)' ) '  ........  ..............  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,a)' ) i, ':', a(1:2,i), &
      '...more entries...'

  end if

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
