subroutine d2xy ( m, d, x, y )

!*****************************************************************************80
!
!! D2XY converts a 1D Hilbert coordinate to a 2D Cartesian coordinate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the index of the Hilbert curve.
!    The number of cells is N=2^M.
!    0 < M.
!
!    Input, integer ( kind = 4 ) D, the Hilbert coordinate of the cell.
!    0 <= D < N * N.
!
!    Output, integer ( kind = 4 ) X, Y, the Cartesian coordinates of the cell.
!    0 <= X, Y < N.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rx
  integer ( kind = 4 ) ry
  integer ( kind = 4 ) s
  integer ( kind = 4 ) t
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  n = 2 ** m

  x = 0
  y = 0
  t = d
  s = 1

  do while ( s < n )

    rx = mod ( t / 2, 2 )
    if ( rx == 0 ) then
      ry = mod ( t, 2 )
    else
      ry = mod ( ieor ( t, rx ), 2 )
    end if
    call rot ( s, x, y, rx, ry )
    x = x + s * rx
    y = y + s * ry
    t = t / 4

    s = s * 2

  end do

  return
end
subroutine rot ( n, x, y, rx, ry ) 

!*****************************************************************************80
!
!! ROT rotates and flips a quadrant appropriately.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of a side of the square.  
!    N must be a power of 2.
!
!    Input/output, integer ( kind = 4 ) X, Y, the coordinates of a point.
!
!    Input, integer ( kind = 4 ) RX, RY, values of 0 or 1 which indicate
!    whether reflection or flips should be carried out.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rx
  integer ( kind = 4 ) ry
  integer ( kind = 4 ) t
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  if ( ry == 0 ) then
!
!  Reflect.
!
    if ( rx == 1 ) then
      x = n - 1 - x
      y = n - 1 - y
    end if
!
!  Flip.
!
    t = x
    x = y
    y = t

  end if

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
subroutine xy2d ( m, x, y, d )

!*****************************************************************************80
!
!! XY2D converts a 2D Cartesian coordinate to a 1D Hilbert coordinate.
!
!  Discussion:
!
!    It is assumed that a square has been divided into an NxN array of cells,
!    where N is a power of 2.
!
!    Cell (0,0) is in the lower left corner, and (N-1,N-1) in the upper 
!    right corner.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the index of the Hilbert curve.
!    The number of cells is N=2^M.
!    0 < M.
!
!    Input, integer ( kind = 4 ) X, Y, the Cartesian coordinates of a cell.
!    0 <= X, Y < N.
!
!    Output, integer ( kind = 4 ) D, the Hilbert coordinate of the cell.
!    0 <= D < N * N.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rx
  integer ( kind = 4 ) ry
  integer ( kind = 4 ) s
  integer ( kind = 4 ) x
  integer ( kind = 4 ) xcopy
  integer ( kind = 4 ) y
  integer ( kind = 4 ) ycopy

  xcopy = x
  ycopy = y

  d = 0
  n = 2 ** m

  s = n / 2

  do while ( 0 < s )

    if ( iand ( xcopy, s ) > 0 ) then
      rx = 1
    else
      rx = 0
    end if

    if ( iand ( ycopy, s ) > 0 ) then
      ry = 1
    else
      ry = 0
    end if

    d = d + s * s * ( ieor ( 3 * rx, ry ) )

    call rot ( s, xcopy, ycopy, rx, ry )

    s = s / 2

  end do

  return
end
