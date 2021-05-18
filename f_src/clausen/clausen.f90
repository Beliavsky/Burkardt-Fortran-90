function clausen ( x )

!*****************************************************************************80
!
!! CLAUSEN evaluates the Clausen function Cl2(x).
!
!  Discussion:
!
!    Note that the first coefficient, a0 in Koelbig's paper, 
!    is doubled here, to account for a different convention in
!    Chebyshev coefficients.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Kurt Koelbig,
!    Chebyshev coefficients for the Clausen function Cl2(x),
!    Journal of Computational and Applied Mathematics,
!    Volume 64, Number 3, 1995, pages 295-297.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) CLAUSEN, the value of the function.
!
  implicit none
!
!  Chebyshev expansion for -pi/2 < x < +pi/2.
!
  real ( kind = 8 ) :: c1(19) = (/ &
    0.05590566394715132269D+00, &
    0.00000000000000000000D+00, &
    0.00017630887438981157D+00, &
    0.00000000000000000000D+00, &
    0.00000126627414611565D+00, &
    0.00000000000000000000D+00, &
    0.00000001171718181344D+00, &
    0.00000000000000000000D+00, &
    0.00000000012300641288D+00, &
    0.00000000000000000000D+00, &
    0.00000000000139527290D+00, &
    0.00000000000000000000D+00, &
    0.00000000000001669078D+00, &
    0.00000000000000000000D+00, &
    0.00000000000000020761D+00, &
    0.00000000000000000000D+00, &
    0.00000000000000000266D+00, &
    0.00000000000000000000D+00, &
    0.00000000000000000003D+00 /)
!
!  Chebyshev expansion for pi/2 < x < 3pi/2.
!
  real ( kind = 8 ) :: c2(32) = (/ &
    0.00000000000000000000D+00, &
   -0.96070972149008358753D+00, &
    0.00000000000000000000D+00, &
    0.04393661151911392781D+00, &
    0.00000000000000000000D+00, &
    0.00078014905905217505D+00, &
    0.00000000000000000000D+00, &
    0.00002621984893260601D+00, &
    0.00000000000000000000D+00, &
    0.00000109292497472610D+00, &
    0.00000000000000000000D+00, &
    0.00000005122618343931D+00, &
    0.00000000000000000000D+00, &
    0.00000000258863512670D+00, &
    0.00000000000000000000D+00, &
    0.00000000013787545462D+00, &
    0.00000000000000000000D+00, &
    0.00000000000763448721D+00, &
    0.00000000000000000000D+00, &
    0.00000000000043556938D+00, &
    0.00000000000000000000D+00, &
    0.00000000000002544696D+00, &
    0.00000000000000000000D+00, &
    0.00000000000000151561D+00, &
    0.00000000000000000000D+00, &
    0.00000000000000009172D+00, &
    0.00000000000000000000D+00, &
    0.00000000000000000563D+00, &
    0.00000000000000000000D+00, &
    0.00000000000000000035D+00, &
    0.00000000000000000000D+00, &
    0.00000000000000000002D+00 /)
  real ( kind = 8 ) clausen
  integer ( kind = 4 ), parameter :: n1 = 19
  integer ( kind = 4 ), parameter :: n2 = 30
  real ( kind = 8 ) r8_csevl
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_wrap
  real ( kind = 8 ) value
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb
  real ( kind = 8 ) xc
!
!  The function is periodic.  Wrap X into [-pi/2, 3pi/2].
!
  xa = - 0.5 * r8_pi
  xb =   0.5 * r8_pi
  xc =   1.5 * r8_pi
  x2 = r8_wrap ( x, xa, xc )
!
!  Choose the appropriate expansion.
!
  if ( x2 < xb ) then
    x3 = 2.0D+00 * x2 / r8_pi
    value = x2 - x2 * log ( abs ( x2 ) ) &
      + 0.5D+00 * x2 ** 3 * r8_csevl ( x3, c1, n1 )
  else
    x3 = 2.0D+00 * x2 / r8_pi - 2.0D+00
    value = r8_csevl ( x3, c2, n2 )
  end if

  clausen = value

  return
end
subroutine clausen_values ( n_data, x, fx )

!*****************************************************************************80
!
!! CLAUSEN_VALUES returns some values of the Clausen's integral.
!
!  Discussion:
!
!    The function is defined by:
!
!      CLAUSEN(x) = integral ( 0 <= t <= x ) -ln ( 2 * sin ( t / 2 ) ) dt
!
!    The data was reported by McLeod.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Allan McLeod,
!    Algorithm 757:
!    MISCFUN: A software package to compute uncommon special functions,
!    ACM Transactions on Mathematical Software,
!    Volume 22, Number 3, September 1996, pages 288-301.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
     0.14137352886760576684D-01, &
     0.13955467081981281934D+00, &
    -0.38495732156574238507D+00, &
     0.84831187770367927099D+00, &
     0.10139591323607685043D+01, &
    -0.93921859275409211003D+00, &
     0.72714605086327924743D+00, &
     0.43359820323553277936D+00, &
    -0.98026209391301421161D-01, &
    -0.56814394442986978080D+00, &
    -0.70969701784448921625D+00, &
     0.99282013254695671871D+00, &
    -0.98127747477447367875D+00, &
    -0.64078266570172320959D+00, &
     0.86027963733231192456D+00, &
     0.39071647608680211043D+00, &
     0.47574793926539191502D+00, &
     0.10105014481412878253D+01, &
     0.96332089044363075154D+00, &
    -0.61782699481929311757D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
      0.0019531250D+00, &
      0.0312500000D+00, &
     -0.1250000000D+00, &
      0.5000000000D+00, &
      1.0000000000D+00, &
     -1.5000000000D+00, &
      2.0000000000D+00, &
      2.5000000000D+00, &
     -3.0000000000D+00, &
      4.0000000000D+00, &
      4.2500000000D+00, &
     -5.0000000000D+00, &
      5.5000000000D+00, &
      6.0000000000D+00, &
      8.0000000000D+00, &
    -10.0000000000D+00, &
     15.0000000000D+00, &
     20.0000000000D+00, &
    -30.0000000000D+00, &
     50.0000000000D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
function r8_csevl ( x, a, n )

!*****************************************************************************80
!
!! R8_CSEVL evaluates a Chebyshev series.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 September 2011
!
!  Author:
!
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Roger Broucke,
!    Algorithm 446:
!    Ten Subroutines for the Manipulation of Chebyshev Series,
!    Communications of the ACM,
!    Volume 16, Number 4, April 1973, pages 254-256.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Input, real ( kind = 8 ) A(N), the Chebyshev coefficients.
!
!    Input, integer ( kind = 4 ) N, the number of Chebyshev coefficients.
!
!    Output, real ( kind = 8 ) R8_CSEVL, the Chebyshev series evaluated at X.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_csevl
  real ( kind = 8 ) twox
  real ( kind = 8 ) x

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_CSEVL - Fatal error!'
    write ( *, '(a)' ) '  Number of terms <= 0.'
    stop 1
  end if

  if ( 1000 < n ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_CSEVL - Fatal error!'
    write ( *, '(a)' ) '  1000 < Number of terms.'
    stop 1
  end if

  if ( x < -1.1D+00 .or. 1.1D+00 < x ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_CSEVL - Fatal error!'
    write ( *, '(a)' ) '  X outside [-1,+1]'
    write ( *, '(a,g14.6)' ) '  X = ', x
    stop 1
  end if

  twox = 2.0D+00 * x
  b1 = 0.0D+00
  b0 = 0.0D+00

  do i = n, 1, -1
    b2 = b1
    b1 = b0
    b0 = twox * b1 - b2 + a(i)
  end do

  r8_csevl = 0.5D+00 * ( b0 - b2 )

  return
end
function r8_wrap ( r, rlo, rhi )

!*****************************************************************************80
!
!! R8_WRAP forces an R8 to lie between given limits by wrapping.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!  Example:
!
!    RLO = 4.0, RHI = 8.0
!
!     R  Value
!
!    -2     8
!    -1     4
!     0     5
!     1     6
!     2     7
!     3     8
!     4     4
!     5     5
!     6     6
!     7     7
!     8     8
!     9     4
!    10     5
!    11     6
!    12     7
!    13     8
!    14     4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 July 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, a value.
!
!    Input, real ( kind = 8 ) RLO, RHI, the desired bounds.
!
!    Output, real ( kind = 8 ) R8_WRAP, a "wrapped" version of the value.
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_wrap
  real ( kind = 8 ) rhi
  real ( kind = 8 ) rhi2
  real ( kind = 8 ) rlo
  real ( kind = 8 ) rlo2
  real ( kind = 8 ) rwide
  real ( kind = 8 ) value
!
!  Guarantee RLO2 < RHI2.
!
  rlo2 = min ( rlo, rhi )
  rhi2 = max ( rlo, rhi )
!
!  Find the width.
!
  rwide = rhi2 - rlo2
!
!  Add enough copies of (RHI2-RLO2) to R so that the
!  result ends up in the interval RLO2 - RHI2.
!
  if ( rwide == 0.0D+00 ) then
    value = rlo
  else if ( r < rlo2 ) then
    n = int ( ( rlo2 - r ) / rwide ) + 1
    value = r + n * rwide
    if ( value == rhi ) then
      value = rlo
    end if
  else
    n = int ( ( r - rlo2 ) / rwide )
    value = r - n * rwide
    if ( value == rlo ) then
      value = rhi
    end if
  end if

  r8_wrap = value

  return
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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
