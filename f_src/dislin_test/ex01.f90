program main

!*****************************************************************************80
!
!! EX01 demonstrates the use of CURVE to plot (X,Y) data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 August 2018
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

  integer ( kind = 4 ), parameter :: n = 100

  integer ( kind = 4 ) i
  integer ( kind = 4 ) nthk
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) xray(n)
  real ( kind = 8 ) y1ray(n)
  real ( kind = 8 ) y2ray(n)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX01:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Demonstrate the use of CURVE to plot (X,Y) data.'
!
!  Set up the X and Y data for the plot.
!
  do i = 1, n
    xray(i) = real (  i - 1, kind = 8 ) * 360.0D+00 / real ( n - 1, kind = 8 )
  end do

  y1ray(1:n) = sin ( r8_pi * xray(1:n) / 180.0D+00 )
  y2ray(1:n) = cos ( r8_pi * xray(1:n) / 180.0D+00 )
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
  call setfil ( 'ex01.png' )
!
!  Choose the page size and orientation.
!
  call setpag ( 'usal' )
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
  call axspos ( 450, 1800 )
  call axslen ( 2200, 1200 )

  call name ( 'X-axis', 'X' )
  call name ( 'Y-axis', 'Y' )

  call labdig ( -1, 'x' )
  call ticks ( 10, 'xy' )

  call titlin ( 'EX01: Demonstrate CURVE', 1 )
  call titlin ( 'sin(x), cos(x)', 3 )

  call graf ( 0.0D+00, 360.0D+00, 0.0D+00, 90.0D+00, -1.0D+00, 1.0D+00, &
    -1.0D+00, 0.5D+00 )
  call title ( )
!
!  Draw XRAY versus Y1RAY in red.
!
  call color ( 'red' )
  call curve ( xray, y1ray, n )
!
!  Make the second curve thicker.
!
  nthk = 10
  call thkcrv ( nthk )
!
!  Draw XRAY versus Y2RAY in green.
!
  call color ( 'green' )
  call curve ( xray, y2ray, n )

  call color ( 'fore' ) 
  call dash ( )
  call xaxgit ( )
!
!  Close DISLIN.
!
  call disfin ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX01:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
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
