program main

!*****************************************************************************80
!
!! EX07B demonstrates demonstrates 3D bar graphs.
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

  integer ( kind = 4 ), parameter :: n = 18

  character ( len = 80 ) :: cbuf
  integer ( kind = 4 ) i
  integer ( kind = 4 ), dimension (n) :: icray = (/ &
     30,  30,  30,  30,  30,  30, 100, 100, 100, 100, &
    100, 100, 170, 170, 170, 170, 170, 170 /)
  real ( kind = 8 ), dimension (n) :: xray  = (/ &
    1.0, 3.0, 8.0, 1.5, 9.0, 6.3, 5.8, 2.3, 8.1, 3.5, &
    2.2, 8.7, 9.2, 4.8, 3.4, 6.9, 7.5, 3.8 /)
  real ( kind = 8 ), dimension (n) :: xwray
  real ( kind = 8 ), dimension (n) :: yray  = (/ &
    5.0, 8.0, 3.5, 2.0, 7.0, 1.0, 4.3, 7.2, 6.0, 8.5, &
    4.1, 5.0, 7.3, 2.8, 1.6, 8.9, 9.5, 3.2 /)
  real ( kind = 8 ), dimension (n) :: ywray
  real ( kind = 8 ), dimension (n) :: z1ray = (/ &
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
  real ( kind = 8 ), dimension (n) :: z2ray = (/ &
    4.0, 5.0, 3.0, 2.0, 3.5, 4.5, 2.0, 1.6, 3.8, 4.7, &
    2.1, 3.5, 1.9, 4.2, 4.9, 2.8, 3.6, 4.3 /)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX07B:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Demonstrate the creation of 3D bar graphs.'

  do i = 1, n
    xwray(i) = 0.5
    ywray(i) = 0.5
  end do
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
  call setfil ( 'ex07b.png' )
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
!  Use the standard HARDWARE font.
!
  call hwfont ( )
  call axspos ( 200, 2600 )
  call axslen ( 1800, 1800 )

  call name ( 'x-axis', 'x' )
  call name ( 'y-axis', 'y' )
  call name ( 'z-axis', 'z' )

  call titlin ( '3D bars / bars3d', 3 )

  call labl3d ( 'hori' )

  call graf3d ( 0.0D+00, 10.0D+00, 0.0D+00, 2.0D+00, 0.0D+00, 10.0D+00, &
    0.0D+00, 2.0D+00, 0.0D+00, 5.0D+00, 0.0D+00, 1.0D+00 )

  call grid3d ( 1, 1, 'bottom' )

  call bars3d ( xray, yray, z1ray, z2ray, xwray, ywray, icray, n )

  call legini ( cbuf, 3, 20 )
  call legtit ( ' ' )
  call legpos ( 1300, 1100 )
  call leglin ( cbuf, 'first', 1 )
  call leglin ( cbuf, 'second', 2 )
  call leglin ( cbuf, 'third', 3 )
  call legend ( cbuf, 3 )

  call height ( 50 )
  call title ( )
!
!  Close DISLIN.
!
  call disfin ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX07B:'
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
