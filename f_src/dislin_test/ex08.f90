program main

!*****************************************************************************80
!
!! EX08 demonstrates various shading patterns.
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

  character ( len = 2 ) cstr
  character ( len = 60 ) ctit
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iclr
  integer ( kind = 4 ) ii
  integer ( kind = 4 ), dimension ( 4 ) :: ix = (/ 0, 300, 300, 0 /)
  integer ( kind = 4 ) ixp(4)
  integer ( kind = 4 ), dimension ( 4 ) :: iy = (/ 0, 0, 400, 400 /)
  integer ( kind = 4 ) iyp(4)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) nl
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) nx0
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) ny0

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX08:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Demonstrate the use of shading patterns.'

  ctit = 'Shading Patterns (AREAF)'
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
  call setfil ( 'ex08.png' )
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
  call setvlt ( 'SMALL' )

  call height ( 50 )
  nl = nlmess ( ctit )
  nx = ( 2970 - nl ) / 2
  call messag ( ctit, nx, 200 )

  nx0 = 335
  ny0 = 350

  do i = 1, 3

    ny = ny0 + ( i - 1 ) * 600

    do j = 1, 6

      iclr = ( i - 1 ) * 6 + j - 1
      iclr = mod ( iclr, 15 )

      if ( iclr == 0 ) then
        iclr = 15
      end if

      call setclr ( iclr )

      nx = nx0 + ( j - 1 ) * 400
      ii = ( i - 1 ) * 6 + j - 1
      call shdpat ( ii )
      write ( cstr, '(i2)' ) ii

      do k = 1, 4
        ixp(k) = ix(k) + nx
        iyp(k) = iy(k) + ny
      end do

      call areaf ( ixp, iyp, 4 )

      nl = nlmess ( cstr )
      nx = nx + ( 300 - nl ) / 2
      call messag ( cstr, nx, ny+460 )

    end do

  end do
!
!  Close DISLIN.
!
  call disfin ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX08:'
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
