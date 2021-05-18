program main

!*****************************************************************************80
!
!! EX04 demonstrates various interpolation methods for data.
!
!  Discussion:
!
!    Create a plot containing six subplots.  Each subplot shows the same
!    data, interpolated in a different way.
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
!    This FORTRAN90 version by John Burkardt
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

  character ( len = 6 ), dimension (6) :: cpol = (/ &
    'Spline', 'Stem  ', 'Bars  ', 'Step  ', 'Stairs', 'Linear' /)
  character ( len = 60 ) ctit
  integer ( kind = 4 ) i
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) :: nya = 2700
  real ( kind = 8 ), dimension (16) :: x = (/ &
     0.0,  1.0,  3.0,  4.5,  6.0, &
     8.0,  9.0, 11.0, 12.0, 12.5, &
    13.0, 15.0, 16.0, 17.0, 19.0, &
    20.0 /)
  real ( kind = 8 ), dimension (16) :: y = (/ &
     2.0,  4.0,  4.5,  3.0,  1.0, &
     7.0,  2.0,  3.0,  5.0,  2.0, &
     2.5,  2.0,  4.0,  6.0,  5.5, &
     4.0 /)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX04:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Demonstrate the various interpolation'
  write ( *, '(a)' ) '  methods available for (X,Y) data.'
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
  call setfil ( 'ex04.png' )
!
!  Choose the page size and orientation.
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
!  Mark data points on the curve, incrementing by 1.
!
  call incmrk ( 1 )
!
!  Use a plot symbol height measured in plot coordinates.
!
  call hsymbl ( 25 )
!
!  Define axis system titles.
!
  ctit = 'Interpolation methods'
  call titlin ( ctit, 1 )
!
!  Define the X and Y sizes of the axis system.
!
  call axslen ( 1500, 350 )
!
!  Specify how the lower X, left Y, upper X and right Y axes are labeled.
!
  call setgrf ( 'line', 'line', 'line', 'line' )
!
!  The subplots are drawn from bottom to top.
!
  do i = 1, 6

    call axspos ( 350, nya-(i-1)*350 )
    call polcrv ( cpol(i) )
    call marker ( 0 )

    call graf ( 0.0D+00, 20.0D+00, 0.0D+00, 5.0D+00, 0.0D+00, 10.0D+00, 0.0D+00, 5.0D+00 )
    nx = nxposn ( 1.0D+00 )
    ny = nyposn ( 8.0D+00 )
    call messag ( cpol(i), nx, ny )
    call curve ( x, y, 16 )

    if ( i == 6 ) then
      call height ( 50 )
      call title ( )
    end if

    call endgrf ( )

  end do
!
!  Close DISLIN.
!
  call disfin ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX04:'
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
