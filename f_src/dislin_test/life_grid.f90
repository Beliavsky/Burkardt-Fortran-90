program main

!*****************************************************************************80
!
!! LIFE_GRID uses DISLIN to draw a grid for the game of Life.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Helmut Michels,
!    The Data Plotting Software DISLIN - version 10.4,
!    Shaker Media GmbH, January 2010,
!    ISBN13: 978-3-86858-517-9.
!
  use dislin

  implicit none

  character ( len = 60 ) ctit
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) pat
  real ( kind = 8 ) r
  real ( kind = 8 ) x
  real ( kind = 8 ) xvec(2)
  real ( kind = 8 ) y
  real ( kind = 8 ) yvec(2)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LIFE_GRID:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Use DISLIN routines to plot a grid for Life.'
!
!  Specify the format of the output file.
!
  call metafl ( 'png' )
!
!  Indicate that new data overwrites old data.
!
  call filmod ( 'delete' )
!
!  Specify the name of the output graphics file.
!
  call setfil ( 'life_grid.png' )
!
!  Choose the page size and orientation.
!  'USA' is 2160 plot units wide and 2790 plot units high.
!  'P' requests PROFILE rather than LANDSCAPE orientation.
!
  call setpag ( 'usap' )
!
!  For PNG output, reverse the default black background to white.
!
  call scrmod ( 'reverse' )
!
!  Open DISLIN.
!
  call disini ( )
!
!  Plot a border around the page.
!
  call pagera ( )
!
!  Use the COMPLEX font.
!
  call complx ( )
!
!  Use a color table, which is required if we want to do color graphics.
!  For this color table, in particular,
!  1 = black,
!  2 = red,
!  3 = green,
!  4 = blue.
!
  call setvlt ( 'small' )
!
!  Define the X and Y sizes of the axis system in plot units.
!
  call axslen ( 1000, 1000 )
!
!  Specify how the lower X, left Y, upper X and right Y axes are labeled.
!
  call setgrf ( 'line', 'line', 'line', 'line' )
!
!  Set the axis origin 500 plot units to the right, and 1500 plot units DOWN.
!
  call axspos ( 500, 1500 )
!
!  Relate the physical coordinates to the axes.
!
  call graf ( 0.0D+00, 100.0D+00, 0.0D+00, 0.5D+00, 0.0D+00, 100.0D+00, 0.0D+00, 0.5D+00 )
!
!  Draw 21 horizontal lines.
!
  do j = 0, 100, 5
    y = real ( j, kind = 8 )
    xvec(1) = 0.0D+00
    xvec(2) = 100.0D+00
    yvec(1) = y
    yvec(2) = y
    call curve ( xvec, yvec, 2 )
  end do
!
!  Draw 21 vertical lines.
!
  do i = 0, 100, 5
    x = real ( i, kind = 8 )
    xvec(1) = x
    xvec(2) = x
    yvec(1) = 0.0D+00
    yvec(2) = 100.0D+00
    call curve ( xvec, yvec, 2 )
  end do
!
!  Select the shading pattern.
!
  pat = 16
  call shdpat ( pat )
!
!  Select color 3 (green) from the color table.
!
  call setclr ( 3 )
!
!  Draw one circle near the origin.
!
  x = 2.5D+00
  y = 2.5D+00
  r = 2.0D+00
  call rlcirc ( x, y, r )
!
!  Select color 2 (red).
!
  call setclr ( 2 )
!
!  Draw a glider.
!
  x = 7.5D+00
  y = 37.5D+00
  r = 2.0D+00
  call rlcirc ( x, y, r )

  x = 12.5D+00
  y = 37.5D+00
  r = 2.0D+00
  call rlcirc ( x, y, r )

  x = 12.5D+00
  y = 47.5D+00
  r = 2.0D+00
  call rlcirc ( x, y, r )

  x = 17.5D+00
  y = 37.5D+00
  r = 2.0D+00
  call rlcirc ( x, y, r )

  x = 17.5D+00
  y = 42.5D+00
  r = 2.0D+00
  call rlcirc ( x, y, r )
!
!  Select color 4 (blue)
!
  call setclr ( 4 )
!
!  Select open shading pattern.
!
  pat = 0
  call shdpat ( pat )
!
!  Draw three open circles.
!
  x = 62.5D+00
  y = 62.5D+00
  r = 2.0D+00
  call rlcirc ( x, y, r )

  x = 67.5D+00
  y = 57.5D+00
  r = 2.0D+00
  call rlcirc ( x, y, r )

  x = 72.5D+00
  y = 52.5D+00
  r = 2.0D+00
  call rlcirc ( x, y, r )
!
!  Select character height in plot units.
!
  call height ( 50 )
!
!  Select color 1 (black) from the color table.
!
  call setclr ( 1 )
!
!  Define axis system titles.
!
  ctit = 'Grid for Game of Life'
  call titlin ( ctit, 1 )
!
!  Draw the title.
!
  call title ( )
!
!  End this plot.
!
  call endgrf ( )
!
!  Close DISLIN.
!
  call disfin ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LIFE_GRID:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
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
