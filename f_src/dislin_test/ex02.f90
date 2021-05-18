program main

!*****************************************************************************80
!
!! EX02 demonstrates the use of POLAR to plot (R,Theta) data.
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

  integer ( kind = 4 ), parameter :: m = 10
  integer ( kind = 4 ), parameter :: n = 300

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) step
  real ( kind = 8 ) x2(m)
  real ( kind = 8 ) xray(n)
  real ( kind = 8 ) y2(m)
  real ( kind = 8 ) yray(n)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX02:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Demonstrate the use of POLAR to plot '
  write ( *, '(a)' ) '  (R,Theta) data.'

  step = 360.0D+00 / real ( n - 1, kind = 8 )

  do i = 1, n
    a = real ( i - 1, kind = 8 ) * step
    a = a * pi / 180.0D+00
    yray(i) = a
    xray(i) = sin ( 5.0D+00 * a )
  end do

  do i = 1, m
    x2(i) = real ( i, kind = 8 )
    y2(i) = real ( i, kind = 8 )
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
  call setfil ( 'ex02.png' )
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

  call titlin ( 'Polar Plots', 2 )
  call ticks ( 3, 'Y' )
  call axends ( 'NOENDS', 'X' )
  call labdig ( -1, 'Y' )
  call axslen ( 1000, 1000 )
  call axsorg ( 1050, 900 )

  call polar ( 1.0D+00, 0.0D+00, 0.2D+00, 0.0D+00, 30.0D+00 )
  call curve ( xray, yray, n )
  call htitle ( 50 )
  call title ( )
  call endgrf ( )

  call labdig ( -1, 'X' )
  call axsorg ( 1050, 2250 )
  call labtyp ( 'VERT', 'Y' )
  call barwth ( 5.0D+00 )
  
  call polar ( 10.0D+00, 0.0D+00, 2.0D+00, 0.0D+00, 30.0D+00 )
  call barwth ( -5.0D+00 )
  call polcrv ( 'FBARS' )
  call curve ( x2, y2, m )
!
!  Close DISLIN.
!
  call disfin ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX02:'
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
