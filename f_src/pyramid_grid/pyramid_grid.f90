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
function pyramid_grid_size ( n )

!*****************************************************************************80
!
!! PYRAMID_GRID_SIZE sizes a pyramid grid.
!
!  Discussion:
!
!    0:  x
!
!    1:  x  x
!        x  x
!
!    2:  x  x  x
!        x  x  x
!        x  x  x
!
!    3:  x  x  x  x
!        x  x  x  x
!        x  x  x  x
!        x  x  x  x
!
!    N  Size
!
!    0     1
!    1     5 = 1 + 4
!    2    14 = 1 + 4 + 9
!    3    30 = 1 + 4 + 9 + 16
!    4    55 = 1 + 4 + 9 + 16 + 25
!    5    91 = 1 + 4 + 9 + 16 + 25 + 36
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!
!    Output, integer ( kind = 4 ) PYRAMID_GRID_SIZE, the number of
!    nodes in the grid of size N.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) np1
  integer ( kind = 4 ) pyramid_grid_size
  integer ( kind = 4 ) value

  np1 = n + 1

  value = ( np1 * ( np1 + 1 ) * ( 2 * np1 + 1 ) ) / 6

  pyramid_grid_size = value

  return
end
subroutine pyramid_unit_grid ( n, ng, pg )

!*****************************************************************************80
!
!! PYRAMID_UNIT_GRID computes grid points in the unit pyramid.
!
!  Discussion:
!
!    The unit pyramid has base (-1,-1,0), (+1,1,0), (+1,+1,0), (-1,+1,0)
!    and vertex (0,0,1).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!
!    Input, integer ( kind = 4 ) NG, the number of nodes to generate,
!    as determined by pyramid_grid_size().
!
!    Output, real ( kind = 8 ) PG(3,NG), the grid point coordinates.
!
  implicit none

  integer ( kind = 4 ) ng

  integer ( kind = 4 ) g
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) n
  real ( kind = 8 ) pg(3,ng)

  g = 0

  do k = n, 0, -1
    hi = n - k
    lo = - hi
    do j = lo, hi, 2
      do i = lo, hi, 2
        g = g + 1
        pg(1,g) = real ( i, kind = 8 ) / real ( n, kind = 8 )
        pg(2,g) = real ( j, kind = 8 ) / real ( n, kind = 8 )
        pg(3,g) = real ( k, kind = 8 ) / real ( n, kind = 8 )
      end do
    end do
  end do

  return
end
subroutine pyramid_unit_grid_plot ( n, ng, pg, header )

!*****************************************************************************80
!
!! PYRAMID_UNIT_GRID_PLOT sets up a GNUPLOT plot of a unit pyramid grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!
!    Input, integer ( kind = 4 ) NG, the number of nodes to generate,
!    as determined by pyramid_grid_size().
!
!    Input, real ( kind = 8 ) PG(3,NG), the grid point coordinates.
!
!    Input, character ( len = * ) HEADER, the header for the files.
!
  implicit none

  integer ( kind = 4 ) ng

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = * ) header
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  character ( len = 255 ) node_filename
  integer ( kind = 4 ) node_unit
  real ( kind = 8 ) pg(3,ng)
  character ( len = 255 ) plot_filename
  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)
  real ( kind = 8 ) v4(3)
  real ( kind = 8 ) v5(3)
  character ( len = 255 ) vertex_filename
  integer ( kind = 4 ) vertex_unit
!
!  Create the vertex file.
!
  call pyramid_unit_vertices ( v1, v2, v3, v4, v5 )

  call get_unit ( vertex_unit )
  vertex_filename = trim ( header ) // '_vertices.txt'
  open ( unit = vertex_unit, file = vertex_filename, &
    status = 'replace' )

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v2(1), v2(2), v2(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v3(1), v3(2), v3(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v4(1), v4(2), v4(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v5(1), v5(2), v5(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v2(1), v2(2), v2(3)
  write ( vertex_unit, '(a)' ) ''

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v1(1), v1(2), v1(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v2(1), v2(2), v2(3)
  write ( vertex_unit, '(a)' ) ''

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v1(1), v1(2), v1(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v3(1), v3(2), v3(3)
  write ( vertex_unit, '(a)' ) ''

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v1(1), v1(2), v1(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v4(1), v4(2), v4(3)
  write ( vertex_unit, '(a)' ) ''

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v1(1), v1(2), v1(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v5(1), v5(2), v5(3)

  close ( unit = vertex_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created vertex file "' // &
    trim ( vertex_filename ) // '".'
!
!  Create the node file.
!
  call get_unit ( node_unit )
  node_filename = trim ( header ) // '_nodes.txt'
  open ( unit = node_unit, file = node_filename, &
    status = 'replace' )
  do j = 1, ng
    write ( node_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) pg(1:3,j)
  end do
  close ( unit = node_unit )
  write ( *, '(a)' ) '  Created node file "' // &
    trim ( node_filename ) // '".'
!
!  Create the command file.
!
  call get_unit ( command_unit )
  command_filename = trim ( header ) // '_commands.txt'
  open ( unit = command_unit, file = command_filename, &
    status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // &
    trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  plot_filename = trim ( header ) // '.png'
  write ( command_unit, '(a)' ) 'set output "' // &
    trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
  write ( command_unit, '(a)' ) &
    'set title "' // trim ( header ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set key off'
  write ( command_unit, '(a)' ) 'set view equal xyz'
  write ( command_unit, '(a)' ) 'set view 80, 40'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'splot "' // &
    trim ( vertex_filename ) // &
    '" with lines lw 3, \'
  write ( command_unit, '(a)' ) '     "' // &
    trim ( node_filename ) // '" with points pt 7 lt 0'
  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  return
end
subroutine pyramid_unit_vertices ( v1, v2, v3, v4, v5 )

!*****************************************************************************80
!
!! PYRAMID_UNIT_VERTICES returns the vertices of the unit pyramid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) V1(3), V2(3), V3(3), V4(3), V5(3), the vertices.
!
  implicit none

  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)
  real ( kind = 8 ) v4(3)
  real ( kind = 8 ) v5(3)

  v1(1:3) = (/  0.0D+00,  0.0D+00, +1.0D+00 /)
  v2(1:3) = (/ -1.0D+00, -1.0D+00,  0.0D+00 /)
  v3(1:3) = (/ +1.0D+00, -1.0D+00,  0.0D+00 /)
  v4(1:3) = (/ +1.0D+00, +1.0D+00,  0.0D+00 /)
  v5(1:3) = (/ -1.0D+00, +1.0D+00,  0.0D+00 /)

  return
end
subroutine r8_print ( r, title )

!*****************************************************************************80
!
!! R8_PRINT prints an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the value.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  real ( kind = 8 ) r
  character ( len = * ) title

  write ( *, '(a,2x,g14.6)' ) trim ( title ), r

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

      write ( *, '(i5,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

    end do

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
