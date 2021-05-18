program main

!*****************************************************************************80
!
!! EX07 demonstrates demonstrates 3D bar graphs and pie charts.
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

  character ( len = 80 ) cbuf
  integer ( kind = 4 ), dimension (5) :: ic1ray = (/ &
    50, 150, 100, 200, 175 /)
  integer ( kind = 4 ), dimension (5) :: ic2ray = (/ &
    50, 150, 100, 200, 175 /)
  real ( kind = 8 ), dimension (5) :: xray  = (/ &
    2.0, 4.0, 6.0, 8.0, 10.0 /)
  real ( kind = 8 ), dimension (5) :: y1ray = (/ &
    0.0, 0.0, 0.0, 0.0, 0.0 /)
  real ( kind = 8 ), dimension (5) :: y2ray = (/ &
    3.2, 1.5, 2.0, 1.0, 3.0 /)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX07:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Demonstrate the creation of 3D bar '
  write ( *, '(a)' ) '  and pie charts.'
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
  call setfil ( 'ex07.png' )
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

  call titlin ( '3D bar graph / 3D pie chart', 2 )
  call htitle ( 40 )

  call shdpat ( 16)
  call axslen ( 1500, 1000 )
  call axspos ( 300, 1400 )

  call barwth ( 0.5D+00 )
  call bartyp ( '3dvert' )
  call labels ( 'second', 'bars' )
  call labpos ( 'outside', 'bars' )
  call labclr ( 255, 'bars' )
  call graf ( 0.0D+00, 12.0D+00, 0.0D+00, 2.0D+00, 0.0D+00, 5.0D+00, 0.0D+00, 1.0D+00 )
  call title ( )
  call color( 'red' )
  call bars ( xray, y1ray, y2ray, 5 )
  call endgrf ( )

  call shdpat ( 16 )
  call labels ( 'data', 'pie' )
  call labclr ( 255, 'pie' )
  call chnpie ( 'none' )
  call pieclr ( ic1ray, ic2ray, 5 )
  call pietyp ( '3d' )
  call axspos ( 300, 2700 )
  call piegrf ( cbuf, 0, y2ray, 5 )
!
!  Close DISLIN.
!
  call disfin ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX07:'
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


