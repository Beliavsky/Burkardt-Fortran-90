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
subroutine wedge_grid ( n, ng, g )  

!*****************************************************************************80
!
!! WEDGE_GRID computes grid points in the unit wedge in 3D.
!
!  Discussion:
!
!    The interior of the unit wedge in 3D is defined by the constraints:
!      0 <= X
!      0 <= Y
!           X + Y <= 1
!     -1 <= Z <= +1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!    0 <= N.
!
!    Input, integer ( kind = 4 ) NG, the number of grid points.
!    This can be computed by WEDGE_GRID_SIZE, or else determined by
!    NG =(N+1)*((N+1)*(N+2))/2.
!
!    Output, real ( kind = 8 ) G(3,NG), the coordinates
!    of the grid points.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ng

  integer ( kind = 4 ) i
  real ( kind = 8 ) ir
  integer ( kind = 4 ) j
  real ( kind = 8 ) jr
  integer ( kind = 4 ) k
  real ( kind = 8 ) kr
  real ( kind = 8 ) nr
  integer ( kind = 4 ) p
  real ( kind = 8 ) g(3,ng)

  if ( n == 0 ) then
    g(1,1) = 0.5D+00
    g(2,1) = 0.5D+00
    g(3,1) = 0.0D+00
    return
  end if

  p = 0
  nr = real ( n, kind = 8 )

  do k = 0, n
    kr = real ( 2 * k - n, kind = 8 ) / nr
    do j = 0, n
      jr = real ( j, kind = 8 ) / nr
      do i = 0, n - j
        ir = real ( i, kind = 8 ) / nr
        p = p + 1
        g(1,p) = ir
        g(2,p) = jr
        g(3,p) = kr
      end do
    end do
  end do

  return
end
subroutine wedge_grid_size ( n, ng )  

!*****************************************************************************80
!
!! WEDGE_GRID_SIZE counts the points in a grid of the unit wedge in 3D.
!
!  Discussion:
!
!    The interior of the unit wedge in 3D is defined by the constraints:
!      0 <= X
!      0 <= Y
!           X + Y <= 1
!     -1 <= Z <= +1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!    0 <= N.
!
!    Output, integer ( kind = 4 ) NG, the number of grid points.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ng

  ng = ( n + 1 ) * ( ( n + 1 ) * ( n + 2 ) ) / 2

  return
end
subroutine wedge_grid_plot ( n, ng, g, header )

!*****************************************************************************80
!
!! WEDGE_GRID_PLOT sets up a GNUPLOT plot of a unit wedge grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of subintervals.
!
!    Input, integer ( kind = 4 ) NG, the number of nodes.
!
!    Input, real ( kind = 8 ) G(3,NG), the grid point coordinates.
!
!    Input, character ( len = * ) HEADER, the header for the files.
!
  implicit none

  integer ( kind = 4 ) ng

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  real ( kind = 8 ) g(3,ng)
  character ( len = * ) header
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  character ( len = 255 ) node_filename
  integer ( kind = 4 ) node_unit
  character ( len = 255 ) plot_filename
  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)
  real ( kind = 8 ) v4(3)
  real ( kind = 8 ) v5(3)
  real ( kind = 8 ) v6(3)
  character ( len = 255 ) vertex_filename
  integer ( kind = 4 ) vertex_unit
!
!  Create the vertex file.
!
  call wedge_vertices ( v1, v2, v3, v4, v5, v6 )

  call get_unit ( vertex_unit )
  vertex_filename = trim ( header ) // '_vertices.txt'
  open ( unit = vertex_unit, file = vertex_filename, &
    status = 'replace' )

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v1(1), v1(2), v1(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v2(1), v2(2), v2(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v3(1), v3(2), v3(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v1(1), v1(2), v1(3)
  write ( vertex_unit, '(a)' ) ''

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v4(1), v4(2), v4(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v5(1), v5(2), v5(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v6(1), v6(2), v6(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v4(1), v4(2), v4(3)
  write ( vertex_unit, '(a)' ) ''

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v1(1), v1(2), v1(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v4(1), v4(2), v4(3)
  write ( vertex_unit, '(a)' ) ''

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v2(1), v2(2), v2(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v5(1), v5(2), v5(3)
  write ( vertex_unit, '(a)' ) ''

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v3(1), v3(2), v3(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v6(1), v6(2), v6(3)
  write ( vertex_unit, '(a)' ) ''

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
    write ( node_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) g(1:3,j)
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
  write ( command_unit, '(a)' ) '#set view equal xyz'
  write ( command_unit, '(a)' ) 'set view 80, 85'
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
subroutine wedge_vertices ( v1, v2, v3, v4, v5, v6 )

!*****************************************************************************80
!
!! WEDGE_VERTICES returns the vertices of the unit wege.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) V1(3), V2(3), V3(3), V4(3), V5(3), V6(3),
!    the vertices.
!
  implicit none

  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)
  real ( kind = 8 ) v4(3)
  real ( kind = 8 ) v5(3)
  real ( kind = 8 ) v6(3)

  v1(1:3) = (/  0.0D+00,  0.0D+00, -1.0D+00 /)
  v2(1:3) = (/  1.0D+00,  0.0D+00, -1.0D+00 /)
  v3(1:3) = (/  0.0D+00,  1.0D+00, -1.0D+00 /)
  v4(1:3) = (/  0.0D+00,  0.0D+00, +1.0D+00 /)
  v5(1:3) = (/  1.0D+00,  0.0D+00, +1.0D+00 /)
  v6(1:3) = (/  0.0D+00,  1.0D+00, +1.0D+00 /)

  return
end
