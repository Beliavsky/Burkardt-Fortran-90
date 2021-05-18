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
!    An I4MAT is a rectangular array of I4's.
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

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
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

  return
end
subroutine sphere_llt_grid_display ( ng, xg, line_num, line_data, prefix )

!*****************************************************************************80
!
!! SPHERE_LLT_GRID_DISPLAY displays an LLT grid on a sphere.
!
!  Discussion:
!
!    A SPHERE LLT grid imposes a grid of triangles on a sphere,
!    using latitude and longitude lines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NG, the number of points.
!
!    Input, real ( kind = 8 ) XG(3,NG), the points.
!
!    Input, integer ( kind = 8 ) LINE_NUM, the number of grid lines.
!
!    Input, integer ( kind = 8 ) LINE_DATA(2,LINE_NUM), contains pairs of 
!    point indices for line segments that make up the grid.
!
!    Input, character ( len = * ) PREFIX, a prefix for the filenames.
!
  implicit none

  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) ng

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) l
  integer ( kind = 4 ) line_data(2,line_num)
  character ( len = 255 ) line_filename
  integer ( kind = 4 ) line_unit
  character ( len = 255 ) node_filename
  integer ( kind = 4 ) node_unit
  character ( len = 255 ) plot_filename
  character ( len = * ) prefix
  real ( kind = 8 ) xg(3,ng)
!
!  Create graphics data files.
!
  call get_unit ( node_unit )
  node_filename = trim ( prefix ) // '_nodes.txt'
  open ( unit = node_unit, file = node_filename, status = 'replace' )
  do j = 1, ng
    write ( node_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) xg(1:3,j)
  end do
  close ( unit = node_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created node file "' // trim ( node_filename ) // '".'

  call get_unit ( line_unit )
  line_filename = trim ( prefix ) // '_lines.txt'
  open ( unit = line_unit, file = line_filename, status = 'replace' )
  do l = 1, line_num
    if ( 1 < l ) then
      write ( line_unit, '(a)' ) ''
      write ( line_unit, '(a)' ) ''
    end if
    j1 = line_data(1,l)
    j2 = line_data(2,l)
    write ( line_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) xg(1:3,j1)
    write ( line_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) xg(1:3,j2)
  end do
  close ( unit = line_unit )
  write ( *, '(a)' ) '  Created line file "' // trim ( line_filename ) // '".'
!
!  Create graphics command file.
!
  call get_unit ( command_unit )
  command_filename = trim ( prefix ) // '_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  plot_filename = trim ( prefix ) // '.png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
  write ( command_unit, '(a)' ) 'set title "' // trim ( prefix ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set key off'
  write ( command_unit, '(a)' ) 'set style data points'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set view equal xyz'
  write ( command_unit, '(a)' ) 'splot "' // &
    trim ( line_filename ) // &
    '" with lines lw 3, \'
  write ( command_unit, '(a)' ) '     "' // &
    trim ( node_filename ) // '" with points pt 7 lt 0'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  return
end
subroutine sphere_llt_grid_line_count ( lat_num, long_num, line_num )

!*****************************************************************************80
!
!! SPHERE_LLT_GRID_LINE_COUNT counts latitude/longitude triangle grid lines.
!
!  Discussion:
!
!    An LLT grid is a grid of triangles bounded by latitude and longitude 
!    lines over the surface of a sphere in 3D.
!
!    The number returned is the number of pairs of points to be connected.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) LAT_NUM, LONG_NUM, the number of latitude and
!    longitude lines to draw.  The latitudes do not include the North and South
!    poles, which will be included automatically, so LAT_NUM = 5, for instance,
!    will result in points along 7 lines of latitude.
!
!    Output, integer ( kind = 4 ) LINE_NUM, the number of grid lines.
!
  implicit none

  integer ( kind = 4 ) lat_num
  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) long_num

  line_num = long_num * ( lat_num + 1 ) &
           + long_num *   lat_num &
           + long_num * ( lat_num - 1 )

  return
end
subroutine sphere_llt_grid_lines ( lat_num, long_num, line_num, line )

!*****************************************************************************80
!
!! SPHERE_LLT_GRID_LINES: latitude/longitude triangle grid lines.
!
!  Discussion:
!
!    A SPHERE LLT grid imposes a grid of triangles on a sphere,
!    using latitude and longitude lines.
!
!    The point numbering system is the same used in SPHERE_LLT_POINTS,
!    and that routine may be used to compute the coordinates of the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) LAT_NUM, LONG_NUM, the number of latitude and
!    longitude lines to draw.  The latitudes do not include the North and South
!    poles, which will be included automatically, so LAT_NUM = 5, for instance,
!    will result in points along 7 lines of latitude.
!
!    Input, integer ( kind = 4 ) LINE_NUM, the number of grid lines.
!
!    Output, integer ( kind = 4 ) LINE(2,LINE_NUM), contains pairs of point 
!    indices for line segments that make up the grid.
!
  implicit none

  integer ( kind = 4 ) line_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lat_num
  integer ( kind = 4 ) l
  integer ( kind = 4 ) line(2,line_num)
  integer ( kind = 4 ) long_num
  integer ( kind = 4 ) new
  integer ( kind = 4 ) newcol
  integer ( kind = 4 ) old

  l = 0
!
!  "Vertical" lines.
!
  do j = 0, long_num - 1

    old = 1
    new = j + 2

    l = l + 1
    line(1:2,l) = (/ old, new /)

    do i = 1, lat_num - 1

      old = new
      new = old + long_num

      l = l + 1
      line(1:2,l) = (/ old, new /)

    end do

    old = new

    l = l + 1
    line(1:2,l) = (/ old, 1 + lat_num * long_num + 1 /)

  end do
!
!  "Horizontal" lines.
!
  do i = 1, lat_num

    new = 1 + ( i - 1 ) * long_num + 1

    do j = 0, long_num - 2
      old = new
      new = old + 1
      l = l + 1
      line(1:2,l) = (/ old, new /)
    end do

    old = new
    new = 1 + ( i - 1 ) * long_num + 1
    l = l + 1
    line(1:2,l) = (/ old, new /)

  end do
!
!  "Diagonal" lines.
!
  do j = 0, long_num - 1

    old = 1
    new = j + 2
    newcol = j

    do i = 1, lat_num - 1

      old = new
      new = old + long_num + 1
      newcol = newcol + 1
      if ( long_num - 1 < newcol ) then
        newcol = 0
        new = new - long_num
      end if

      l = l + 1
      line(1:2,l) = (/ old, new /)

    end do

  end do

  return
end
subroutine sphere_llt_grid_point_count ( lat_num, long_num, point_num )

!*****************************************************************************80
!
!! SPHERE_LLT_GRID_POINT_COUNT counts points for a latitude/longitude grid.
!
!  Discussion:
!
!    An LLT grid is a grid of triangles defined by latitude and longitude
!    lines over the surface of a sphere in 3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) LAT_NUM, LONG_NUM, the number of latitude 
!    and longitude lines to draw.  The latitudes do not include the North and 
!    South poles, which will be included automatically, so LAT_NUM = 5, for 
!    instance, will result in points along 7 lines of latitude.
!
!    Output, integer ( kind = 4 ) POINT_NUM, the number of grid points.
!
  implicit none

  integer ( kind = 4 ) lat_num
  integer ( kind = 4 ) long_num
  integer ( kind = 4 ) point_num

  point_num = 2 + lat_num * long_num

  return
end
subroutine sphere_llt_grid_points ( r, pc, lat_num, long_num, point_num, p )

!*****************************************************************************80
!
!! SPHERE_LLT_GRID_POINTS: points for a latitude/longitude triangle grid.
!
!  Discussion:
!
!    A SPHERE LLT grid imposes a grid of triangles on a sphere,
!    using latitude and longitude lines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the sphere.
!
!    Input, real ( kind = 8 ) PC(3), the center of the sphere.
!
!    Input, integer ( kind = 4 ) LAT_NUM, LONG_NUM, the number of latitude 
!    and longitude lines to draw.  The latitudes do not include the North and 
!    South poles, which will be included automatically, so LAT_NUM = 5, for 
!    instance, will result in points along 7 lines of latitude.
!
!    Input, integer ( kind = 4 ) POINT_NUM, the number of points.
!
!    Output, real ( kind = 8 ) P(3,POINT_NUM), the grid points.
!
  implicit none

  integer ( kind = 4 ) lat_num
  integer ( kind = 4 ) long_num
  integer ( kind = 4 ) point_num

  integer ( kind = 4 ) lat
  integer ( kind = 4 ) lon
  integer ( kind = 4 ) n
  real ( kind = 8 ) p(3,point_num)
  real ( kind = 8 ) pc(3)
  real ( kind = 8 ) phi
  real ( kind = 8 ) r
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) theta

  n = 0
!
!  The north pole.
!
  theta = 0.0D+00
  phi = 0.0D+00
  n = n + 1
  p(1,n) = pc(1) + r * sin ( phi ) * cos ( theta )
  p(2,n) = pc(2) + r * sin ( phi ) * sin ( theta )
  p(3,n) = pc(3) + r * cos ( phi )
!
!  Do each intermediate ring of latitude.
!
  do lat = 1, lat_num

    phi = real ( lat,         kind = 8 ) * r8_pi &
        / real ( lat_num + 1, kind = 8 )
!
!  Along that ring of latitude, compute points at various longitudes.
!
    do lon = 0, long_num - 1

      theta = real ( lon,      kind = 8 ) * 2.0D+00 * r8_pi &
            / real ( long_num, kind = 8 )

      n = n + 1
      p(1,n) = pc(1) + r * sin ( phi ) * cos ( theta )
      p(2,n) = pc(2) + r * sin ( phi ) * sin ( theta )
      p(3,n) = pc(3) + r * cos ( phi )

    end do
  end do
!
!  The south pole.
!
  theta = 0.0D+00
  phi = r8_pi
  n = n + 1
  p(1,n) = pc(1) + r * sin ( phi ) * cos ( theta )
  p(2,n) = pc(2) + r * sin ( phi ) * sin ( theta )
  p(3,n) = pc(3) + r * cos ( phi )

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
