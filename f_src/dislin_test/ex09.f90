program main

!*****************************************************************************80
!
!! EX09 demonstrates 3D color plots.
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

  integer ( kind = 4 ), parameter :: n = 100

  real ( kind = 8 ) fpi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) step
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) zmat(n,n)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX09:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Demonstrate the creation of a '
  write ( *, '(a)' ) '  3D color plot.'

  fpi = pi / 180.0D+00
  step = 360.0D+00 / real ( n - 1, kind = 8 )

  do i = 1, n
    x = real ( i - 1, kind = 8 ) * step
    do j = 1, n
      y = real ( j - 1, kind = 8 ) * step
      zmat(i,j) = 2.0D+00 * sin ( x * fpi ) * sin ( y * fpi )
    end do
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
  call setfil ( 'ex09.png' )
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
!  Use the standard HARDWARE font.
!
  call hwfont ( )

  call titlin ( '3D color plot of the function', 2 )
  call titlin ( 'f(x,y) = 2 * sin(x) * sin(y)', 4 )

  call name ( 'x-axis', 'x' )
  call name ( 'y-axis', 'y' )
  call name ( 'z-axis', 'z' )

  call intax ( )
  call autres ( n, n )
  call axspos ( 300, 1850 )
  call ax3len ( 2200, 1400, 1400 )

  call graf3 ( 0.0D+00, 360.0D+00, 0.0D+00, 90.0D+00, &
    0.0D+00, 360.0D+00, 0.0D+00, 90.0D+00, &
    -2.0D+00, 2.0D+00, -2.0D+00, 1.0D+00 )

  call crvmat ( zmat, n, n, 1, 1 )

  call height ( 50 )
  call title ( )
  call mpaepl ( 3 )
!
!  Close DISLIN.
!
  call disfin ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX09:'
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
