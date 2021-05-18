program main

!*****************************************************************************80
!
!! EX11 demonstrates a contour plot.
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

  integer ( kind = 4 ), parameter :: n = 50

  real ( kind = 8 ) fpi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) step
  real ( kind = 8 ) xray(n)
  real ( kind = 8 ) yray(n)
  real ( kind = 8 ) zlev
  real ( kind = 8 ) zmat(n,n)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX11:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Demonstrate the creation of '
  write ( *, '(a)' ) '  a contour plot.'

  fpi = pi / 180.0D+00
  step = 360.0 / real ( n - 1, kind = 8 )

  do i = 1, n
    xray(i) = real ( i - 1, kind = 8 ) * step
    yray(i) = real ( i - 1, kind = 8 ) * step
  end do

  do i = 1, n
    do j = 1, n
      zmat(i,j) = 2.0D+00 * sin ( xray(i) * fpi ) * sin ( yray(j) * fpi )
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
  call setfil ( 'ex11.png' )
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

  call titlin ( 'Contour plot', 1 )
  call titlin ( 'f(x,y) = 2 * sin(x) * sin(y)', 3 )

  call name ( 'x-axis', 'x' )
  call name ( 'y-axis', 'y' )

  call intax ( )
  call axspos ( 450, 2670 )

  call graf ( 0.0D+00, 360.0D+00, 0.0D+00, 90.0D+00, &
    0.0D+00, 360.0D+00, 0.0D+00, 90.0D+00 )

  call height ( 30 )

  do i = 1, 9

    zlev = -2.0D+00 + real ( i - 1, kind = 8 ) * 0.5D+00
    call setclr ( i * 25 )

    if ( i == 5 ) then
      call labels ( 'none', 'contur' )
    else
      call labels ( 'float', 'contur' )
    end if

    call contur ( xray, n, yray, n, zmat, zlev )

  end do

  call height ( 50 )
  call color ( 'fore' )
  call title ( )
!
!  Close DISLIN.
!
  call disfin ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX11:'
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
