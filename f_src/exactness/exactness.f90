subroutine chebyshev1_exactness ( n, x, w, p_max )

!*****************************************************************************80
!
!! CHEBYSHEV1_EXACTNESS: monomial exactness for the Chebyshev1 integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!
!    Input, real ( kind = 8 ) X(N), the quadrature points.
!
!    Input, real ( kind = 8 ) W(N), the quadrature weights.
!
!    Input, integer ( kind = 4 ) P_MAX, the maximum exponent.
!    0 <= P_MAX.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) e
  integer ( kind = 4 ) p
  integer ( kind = 4 ) p_max
  real ( kind = 8 ) q
  real ( kind = 8 ) s
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Quadrature rule for the Chebyshev1 integral.'
  write ( *, '(a,i3)' ) '  Rule has order N = ', n
  write ( *, '(a)' ) '  Degree          Relative Error'
  write ( *, '(a)' ) ''

  do p = 0, p_max

    call chebyshev1_integral ( p, s )

    v(1:n) = x(1:n) ** p

    q = dot_product ( w, v )

    if ( s == 0.0D+00 ) then
      e = abs ( q )
    else
      e = abs ( q - s ) / abs ( s )
    end if

    write ( *, '(2x,i6,2x,f24.16)' ) p, e

  end do

  return
end
subroutine chebyshev1_integral ( expon, exact )

!*****************************************************************************80
!
!! CHEBYSHEV1_INTEGRAL evaluates the Chebyshev type 1 integral of a monomial.
!
!  Discussion:
!
!    The integral:
!
!      integral ( -1 <= x <= +1 ) x^n / sqrt ( 1 - x * x ) dx
!
!    This routine is given the value of the exponent, and returns the
!    exact value of the integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) EXPON, the exponent.
!
!    Output, real ( kind = 8 ) EXACT, the value of the exact integral.
!
  implicit none

  real ( kind = 8 ) bot
  real ( kind = 8 ) exact
  integer ( kind = 4 ) expon
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) top
!
!  Get the exact value of the integral.
!
  if ( mod ( expon, 2 ) == 0 ) then

    top = 1
    bot = 1
    do i = 2, expon, 2
      top = top * ( i - 1 )
      bot = bot *   i
    end do
    
    exact = r8_pi * real ( top, kind = 8 ) / real ( bot, kind = 8 )

  else

    exact = 0.0D+00
    
  end if

  return
end
subroutine chebyshev2_exactness ( n, x, w, p_max )

!*****************************************************************************80
!
!! CHEBYSHEV2_EXACTNESS: monomial exactness for the Chebyshev2 integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!
!    Input, real ( kind = 8 ) X(N), the quadrature points.
!
!    Input, real ( kind = 8 ) W(N), the quadrature weights.
!
!    Input, integer ( kind = 4 ) P_MAX, the maximum exponent.
!    0 <= P_MAX.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) e
  integer ( kind = 4 ) p
  integer ( kind = 4 ) p_max
  real ( kind = 8 ) q
  real ( kind = 8 ) s
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Quadrature rule for the Chebyshev2 integral.'
  write ( *, '(a,i3)' ) '  Rule has order N = ', n
  write ( *, '(a)' ) '  Degree          Relative Error'
  write ( *, '(a)' ) ''

  do p = 0, p_max

    call chebyshev2_integral ( p, s )

    v(1:n) = x(1:n) ** p

    q = dot_product ( w, v )

    if ( s == 0.0D+00 ) then
      e = abs ( q )
    else
      e = abs ( q - s ) / abs ( s )
    end if

    write ( *, '(2x,i6,2x,f24.16)' ) p, e

  end do

  return
end
subroutine chebyshev2_integral ( expon, exact )

!*****************************************************************************80
!
!! CHEBYSHEV2_INTEGRAL evaluates a monomial Chebyshev type 2 integral.
!
!  Discussion:
!
!    The integral:
!
!      integral ( -1 <= x <= +1 ) x^n * sqrt ( 1 - x * x ) dx
!
!    This routine is given the value of the exponent, and returns the
!    exact value of the integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) EXPON, the exponent.
!
!    Output, real ( kind = 8 ) EXACT, the value of the exact integral.
!
  implicit none

  real ( kind = 8 ) bot
  real ( kind = 8 ) exact
  integer ( kind = 4 ) expon
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) top
!
!  Get the exact value of the integral.
!
  if ( mod ( expon, 2 ) == 0 ) then

    top = 1
    bot = 1
    do i = 2, expon, 2
      top = top * ( i - 1 )
      bot = bot *   i
    end do

    bot = bot * real ( expon + 2, kind = 8 )

    exact = r8_pi * real ( top, kind = 8 ) / real ( bot, kind = 8 )

  else

    exact = 0.0D+00
    
  end if

  return
end
subroutine gegenbauer_exactness ( n, x, w, p_max, lambda )

!*****************************************************************************80
!
!! GEGENBAUER_EXACTNESS: monomial exactness for the Gegenbauer integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!
!    Input, real ( kind = 8 ) X(N), the quadrature points.
!
!    Input, real ( kind = 8 ) W(N), the quadrature weights.
!
!    Input, integer ( kind = 4 ) P_MAX, the maximum exponent.
!    0 <= P_MAX.
!
!    Input, real ( kind = 8 ) LAMBDA, the parameter.
!    -1/2 < LAMBDA,
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) e
  real ( kind = 8 ) lambda
  integer ( kind = 4 ) p
  integer ( kind = 4 ) p_max
  real ( kind = 8 ) q
  real ( kind = 8 ) s
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Quadrature rule for the Gegenbauer integral.'
  write ( *, '(a,g14.6)' ) '  Lambda = ', lambda
  write ( *, '(a,i3)' ) '  Rule has order N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Degree          Relative Error'
  write ( *, '(a)' ) ''

  do p = 0, p_max

    call gegenbauer_integral ( p, lambda, s )

    v(1:n) = x(1:n) ** p

    q = dot_product ( w, v )

    if ( s == 0.0D+00 ) then
      e = abs ( q )
    else
      e = abs ( q - s ) / abs ( s )
    end if

    write ( *, '(2x,i6,2x,f24.16)' ) p, e

  end do

  return
end
subroutine gegenbauer_integral ( p, lambda, s )

!*****************************************************************************80
!
!! GEGENBAUER_INTEGRAL evaluates a monomial integral with Gegenbauer weight.
!
!  Discussion:
!
!    The integral:
!
!      integral ( -1 <= x < +1 ) x^p * ( 1 - x^2 )^(lambda-1/2) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) P, the exponent.
!    0 <= P.
!
!    Input, real ( kind = 8 ) LAMBDA, the exponent term.
!    -1/2 < LAMBDA.
!
!    Output, real ( kind = 8 ) S, the value of the integral.
!
  implicit none

  real ( kind = 8 ) lambda
  integer ( kind = 4 ) p
  real ( kind = 8 ) s

  if ( mod ( p, 2 ) == 0 ) then
    s = gamma ( p / 2.0D+00 + 0.5D+00 ) * gamma ( lambda + 0.5D+00 ) &
      / gamma ( p / 2.0D+00 + lambda + 1.0D+00 )
  else
    s = 0.0D+00
  end if

  return
end
subroutine hermite_exactness ( n, x, w, p_max )

!*****************************************************************************80
!
!! HERMITE_EXACTNESS: monomial exactness for the Hermite integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!
!    Input, real ( kind = 8 ) X(N), the quadrature points.
!
!    Input, real ( kind = 8 ) W(N), the quadrature weights.
!
!    Input, integer ( kind = 4 ) P_MAX, the maximum exponent.
!    0 <= P_MAX.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) e
  integer ( kind = 4 ) p
  integer ( kind = 4 ) p_max
  real ( kind = 8 ) q
  real ( kind = 8 ) s
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Quadrature rule for the Hermite integral.'
  write ( *, '(a,i3)' ) '  Rule has order N = ', n
  write ( *, '(a)' ) '  Degree          Relative Error'
  write ( *, '(a)' ) ''

  do p = 0, p_max

    call hermite_integral ( p, s )

    v(1:n) = x(1:n) ** p

    q = dot_product ( w, v )

    if ( s == 0.0D+00 ) then
      e = abs ( q )
    else
      e = abs ( q - s ) / abs ( s )
    end if

    write ( *, '(2x,i6,2x,f24.16)' ) p, e

  end do

  return
end
subroutine hermite_integral ( p, s )

!*****************************************************************************80
!
!! HERMITE_INTEGRAL evaluates a monomial Hermite integral.
!
!  Discussion:
!
!    Integral ( -oo < x < +oo ) x^p exp(-x^2) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) P, the exponent. 
!    0 <= P.
!
!    Output, real ( kind = 8 ) S, the value of the integral.
!
  implicit none

  integer ( kind = 4 ) p
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) s

  if ( mod ( p, 2 ) == 0 ) then
    s = r8_factorial2 ( p - 1 ) * sqrt ( r8_pi ) / 2.0D+00 ** ( p / 2 )
  else
    s = 0.0D+00
  end if

  return
end
subroutine laguerre_exactness ( n, x, w, p_max )

!*****************************************************************************80
!
!! LAGUERRE_EXACTNESS: monomial exactness for the Laguerre integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!
!    Input, real ( kind = 8 ) X(N), the quadrature points.
!
!    Input, real ( kind = 8 ) W(N), the quadrature weights.
!
!    Input, integer ( kind = 4 ) P_MAX, the maximum exponent.
!    0 <= P_MAX.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) e
  integer ( kind = 4 ) p
  integer ( kind = 4 ) p_max
  real ( kind = 8 ) q
  real ( kind = 8 ) s
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Quadrature rule for the Laguerre integral.'
  write ( *, '(a,i3)' ) '  Rule has order N = ', n
  write ( *, '(a)' ) '  Degree          Relative Error'
  write ( *, '(a)' ) ''

  do p = 0, p_max

    call laguerre_integral ( p, s )

    v(1:n) = x(1:n) ** p

    q = dot_product ( w, v )

    if ( s == 0.0D+00 ) then
      e = abs ( q )
    else
      e = abs ( q - s ) / abs ( s )
    end if

    write ( *, '(2x,i6,2x,f24.16)' ) p, e

  end do

  return
end
subroutine laguerre_integral ( p, s )

!*****************************************************************************80
!
!! LAGUERRE_INTEGRAL evaluates a monomial Laguerre integral.
!
!  Discussion:
!
!    The integral:
!
!      integral ( 0 <= x < +oo ) x^p * exp ( -x ) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) P, the exponent.
!    0 <= P.
!
!    Output, real ( kind = 8 ) S, the value of the integral.
!
  implicit none

  integer ( kind = 4 ) p
  real ( kind = 8 ) r8_factorial
  real ( kind = 8 ) s

  s = r8_factorial ( p )

  return
end
subroutine legendre_exactness ( n, x, w, p_max )

!*****************************************************************************80
!
!! LEGENDRE_EXACTNESS: monomial exactness for the Legendre integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!
!    Input, real ( kind = 8 ) X(N), the quadrature points.
!
!    Input, real ( kind = 8 ) W(N), the quadrature weights.
!
!    Input, integer ( kind = 4 ) P_MAX, the maximum exponent.
!    0 <= P_MAX.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) e
  integer ( kind = 4 ) p
  integer ( kind = 4 ) p_max
  real ( kind = 8 ) q
  real ( kind = 8 ) s
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Quadrature rule for the Legendre integral.'
  write ( *, '(a,i3)' ) '  Rule has order N = ', n
  write ( *, '(a)' ) '  Degree          Relative Error'
  write ( *, '(a)' ) ''

  do p = 0, p_max

    call legendre_integral ( p, s )

    v(1:n) = x(1:n) ** p

    q = dot_product ( w, v )

    if ( s == 0.0D+00 ) then
      e = abs ( q )
    else
      e = abs ( q - s ) / abs ( s )
    end if

    write ( *, '(2x,i6,2x,f24.16)' ) p, e

  end do

  return
end
subroutine legendre_integral ( p, s )

!*****************************************************************************80
!
!! LEGENDRE_INTEGRAL evaluates a monomial Legendre integral.
!
!  Discussion:
!
!    To test a Legendre quadrature rule, we use it to approximate the
!    integral of a monomial:
!
!      integral ( -1 <= x <= +1 ) x^p dx
!
!    This routine is given the value of the exponent, and returns the
!    exact value of the integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) P, the power.
!
!    Output, real ( kind = 8 ) S, the value of the exact integral.
!
  implicit none

  integer ( kind = 4 ) p
  real ( kind = 8 ) s
!
!  Get the exact value of the integral.
!
  if ( mod ( p, 2 ) == 0 ) then
  
    s = 2.0D+00 / real ( p + 1, kind = 8 )
	
  else
  
    s = 0.0D+00
	
  end if

  return
end
function r8_factorial ( n )

!*****************************************************************************80
!
!! R8_FACTORIAL computes the factorial of N.
!
!  Discussion:
!
!    factorial ( N ) = product ( 1 <= I <= N ) I
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the argument of the factorial function.
!    If N is less than 1, the function value is returned as 1.
!
!    Output, real ( kind = 8 ) R8_FACTORIAL, the factorial of N.
!
  implicit none

  real ( kind = 8 ) r8_factorial
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n

  r8_factorial = 1.0D+00

  do i = 1, n
    r8_factorial = r8_factorial * real ( i, kind = 8 )
  end do

  return
end
function r8_factorial2 ( n )

!*****************************************************************************80
!
!! R8_FACTORIAL2 computes the double factorial function.
!
!  Discussion:
!
!    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
!                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
!
!  Example:
!
!     N    N!!
!
!     0     1
!     1     1
!     2     2
!     3     3
!     4     8
!     5    15
!     6    48
!     7   105
!     8   384
!     9   945
!    10  3840
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 September 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the argument of the double factorial 
!    function.  If N is less than 1, R8_FACTORIAL2 is returned as 1.0.
!
!    Output, real ( kind = 8 ) R8_FACTORIAL2, the value of N!!.
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ) r8_n
  real ( kind = 8 ) value

  if ( n < 1 ) then

    value = 1.0D+00

  else

    r8_n = real ( n, kind = 8 )
    value = 1.0D+00

    do while ( 1.0D+00 < r8_n )
      value = value * r8_n
      r8_n = r8_n - 2.0D+00
    end do

  end if

  r8_factorial2 = value

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
