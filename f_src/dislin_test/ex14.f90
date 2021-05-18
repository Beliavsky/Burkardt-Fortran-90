program main

!*****************************************************************************80
!
!! EX14 demonstrates the use of TeX instructions for math formulas.
!
!  Discussion:
!
!    On Unix systems, the backslash character is interpreted as a special
!    symbol BEFORE THE COMPILER SEES IT, unless it is inside a double-quote
!    string.
!
!    If you know the program will be used on a UNIX system, you can 'simply'
!    type TWO backslash characters whenever you mean one.
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

  character ( len = 80 ) cstr
  integer ( kind = 4 ) nl

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX14:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Demonstrate the use of TeX instructions to'
  write ( *, '(a)' ) '  create mathematical formulas.'
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
  call setfil ( 'ex14.png' )
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
  call height ( 40 )

  cstr = 'TeX instructions for mathematical formulas'
  nl = nlmess ( cstr )
  call messag ( cstr, (2100 - nl)/2, 100 )

  call texmod ( 'on' )
  call messag ( '$\frac{1}{x+y}$', 150, 400 )
  call messag ( '$\frac{a^2 - b^2}{a+b} = a - b$', 1200, 400 )

  call messag ( '$r = \red{\sqrt{x^2 + y^2}}', 150, 700 )
  call messag ( '$\cos \phi = \frac{x}{\sqrt{x^2 + y^2}}$', 1200, 700 )

  call messag ( '$\gamma(x) = \int_0^\infty e^{-t}t^{x-1}dt$', 150, 1000 )
  call messag ( '$\lim_{x \to \infty} (1 + \frac{1}{x})^x = e$', 1200, 1000 )

  call messag ( '$\mu = \sum_{i=1}^n x_i p_i$', 150, 1300 )
  call messag ( '$\mu = \int_{-\infty}^ \infty x f(x) dx$', 1200, 1300 )

  call messag ( '$\overline{x} = \frac{1}{n} \sum_{i=1}^n x_i$', 150, 1600 )
  call messag ( '$s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \overline{x})^2$', &
    1200, 1600 )

  call messag ( '$\sqrt[n]{\frac{x^n - y^n}{1 + u^{2n}}}$', 150, 1900 )
  call messag ( '$\sqrt[3]{-q + \sqrt{q^2 + p^3}}$', 1200, 1900 )

  call messag ( '$\int \frac{dx}{1+x^2} = \arctan x + c$', 150, 2200)
  call messag ( '$\int \frac{dx}{\sqrt{1+x^2}} = {\rm arcsinh} x + c$', &
    1200, 2200 )

  call messag ( '$\overline{p_1p_2} = \sqrt{(x_2-x_1)^2 + (y_2-y_1)^2}$', &
    150, 2500 )
  call messag ( '$x = \frac{x_1 + \lambda x_2}{1 + \lambda}$', 1200, 2500 )
!
!  Close DISLIN.
!
  call disfin ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EX14:'
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
