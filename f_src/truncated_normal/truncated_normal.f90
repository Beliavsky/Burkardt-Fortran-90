function i4_uniform_ab ( a, b, seed )

!*****************************************************************************80
!
!! I4_UNIFORM_AB returns a scaled pseudorandom I4 between A and B.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ) value.
!
!    The pseudorandom number will be scaled to be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) I4_UNIFORM_AB, a number between A and B.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  real ( kind = 4 ) r
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) value

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r = real ( seed, kind = 4 ) * 4.656612875E-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
  r = ( 1.0E+00 - r ) * ( real ( min ( a, b ), kind = 4 ) - 0.5E+00 ) & 
    +             r   * ( real ( max ( a, b ), kind = 4 ) + 0.5E+00 )
!
!  Use rounding to convert R to an integer between A and B.
!
  value = nint ( r, kind = 4 )

  value = max ( value, min ( a, b ) )
  value = min ( value, max ( a, b ) )

  i4_uniform_ab = value

  return
end
subroutine normal_01_cdf ( x, cdf )

!*****************************************************************************80
!
!! NORMAL_01_CDF evaluates the Normal 01 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    AG Adams,
!    Algorithm 39,
!    Areas Under the Normal Curve,
!    Computer Journal,
!    Volume 12, pages 197-198, 1969.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ), parameter :: a1 = 0.398942280444D+00
  real ( kind = 8 ), parameter :: a2 = 0.399903438504D+00
  real ( kind = 8 ), parameter :: a3 = 5.75885480458D+00
  real ( kind = 8 ), parameter :: a4 = 29.8213557808D+00
  real ( kind = 8 ), parameter :: a5 = 2.62433121679D+00
  real ( kind = 8 ), parameter :: a6 = 48.6959930692D+00
  real ( kind = 8 ), parameter :: a7 = 5.92885724438D+00
  real ( kind = 8 ), parameter :: b0 = 0.398942280385D+00
  real ( kind = 8 ), parameter :: b1 = 3.8052D-08
  real ( kind = 8 ), parameter :: b2 = 1.00000615302D+00
  real ( kind = 8 ), parameter :: b3 = 3.98064794D-04
  real ( kind = 8 ), parameter :: b4 = 1.98615381364D+00
  real ( kind = 8 ), parameter :: b5 = 0.151679116635D+00
  real ( kind = 8 ), parameter :: b6 = 5.29330324926D+00
  real ( kind = 8 ), parameter :: b7 = 4.8385912808D+00
  real ( kind = 8 ), parameter :: b8 = 15.1508972451D+00
  real ( kind = 8 ), parameter :: b9 = 0.742380924027D+00
  real ( kind = 8 ), parameter :: b10 = 30.789933034D+00
  real ( kind = 8 ), parameter :: b11 = 3.99019417011D+00
  real ( kind = 8 ) cdf
  real ( kind = 8 ) q
  real ( kind = 8 ) x
  real ( kind = 8 ) y
!
!  |X| <= 1.28.
!
  if ( abs ( x ) <= 1.28D+00 ) then

    y = 0.5D+00 * x * x

    q = 0.5D+00 - abs ( x ) * ( a1 - a2 * y / ( y + a3 - a4 / ( y + a5 &
      + a6 / ( y + a7 ) ) ) )
!
!  1.28 < |X| <= 12.7
!
  else if ( abs ( x ) <= 12.7D+00 ) then

    y = 0.5D+00 * x * x

    q = exp ( - y ) * b0 / ( abs ( x ) - b1 &
      + b2 / ( abs ( x ) + b3 &
      + b4 / ( abs ( x ) - b5 &
      + b6 / ( abs ( x ) + b7 &
      - b8 / ( abs ( x ) + b9 &
      + b10 / ( abs ( x ) + b11 ) ) ) ) ) )
!
!  12.7 < |X|
!
  else

    q = 0.0D+00

  end if
!
!  Take account of negative X.
!
  if ( x < 0.0D+00 ) then
    cdf = q
  else
    cdf = 1.0D+00 - q
  end if

  return
end
subroutine normal_01_cdf_inv ( p, x )

!*****************************************************************************80
!
!! NORMAL_01_CDF_INV inverts the standard normal CDF.
!
!  Discussion:
!
!    The result is accurate to about 1 part in 10^16.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2015
!
!  Author:
!
!    Original FORTRAN77 version by Michael Wichura.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Michael Wichura,
!    Algorithm AS241:
!    The Percentage Points of the Normal Distribution,
!    Applied Statistics,
!    Volume 37, Number 3, pages 477-484, 1988.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the value of the cumulative probability
!    densitity function.  0 < P < 1.  If P is outside this range, an
!    "infinite" value will be returned.
!
!    Output, real ( kind = 8 ) X, the normal deviate value
!    with the property that the probability of a standard normal deviate being
!    less than or equal to the value is P.
!
  implicit none

  real ( kind = 8 ), parameter, dimension ( 8 ) :: a = (/ &
    3.3871328727963666080D+00, &
    1.3314166789178437745D+02, &
    1.9715909503065514427D+03, &
    1.3731693765509461125D+04, &
    4.5921953931549871457D+04, &
    6.7265770927008700853D+04, &
    3.3430575583588128105D+04, &
    2.5090809287301226727D+03 /)
  real ( kind = 8 ), parameter, dimension ( 8 ) :: b = (/ &
    1.0D+00, &
    4.2313330701600911252D+01, &
    6.8718700749205790830D+02, &
    5.3941960214247511077D+03, &
    2.1213794301586595867D+04, &
    3.9307895800092710610D+04, &
    2.8729085735721942674D+04, &
    5.2264952788528545610D+03 /)
  real ( kind = 8 ), parameter, dimension ( 8 ) :: c = (/ &
    1.42343711074968357734D+00, &
    4.63033784615654529590D+00, &
    5.76949722146069140550D+00, &
    3.64784832476320460504D+00, &
    1.27045825245236838258D+00, &
    2.41780725177450611770D-01, &
    2.27238449892691845833D-02, &
    7.74545014278341407640D-04 /)
  real ( kind = 8 ), parameter :: const1 = 0.180625D+00
  real ( kind = 8 ), parameter :: const2 = 1.6D+00
  real ( kind = 8 ), parameter, dimension ( 8 ) :: d = (/ &
    1.0D+00, &
    2.05319162663775882187D+00, &
    1.67638483018380384940D+00, &
    6.89767334985100004550D-01, &
    1.48103976427480074590D-01, &
    1.51986665636164571966D-02, &
    5.47593808499534494600D-04, &
    1.05075007164441684324D-09 /)
  real ( kind = 8 ), parameter, dimension ( 8 ) :: e = (/ &
    6.65790464350110377720D+00, &
    5.46378491116411436990D+00, &
    1.78482653991729133580D+00, &
    2.96560571828504891230D-01, &
    2.65321895265761230930D-02, &
    1.24266094738807843860D-03, &
    2.71155556874348757815D-05, &
    2.01033439929228813265D-07 /)
  real ( kind = 8 ), parameter, dimension ( 8 ) :: f = (/ &
    1.0D+00, &
    5.99832206555887937690D-01, &
    1.36929880922735805310D-01, &
    1.48753612908506148525D-02, &
    7.86869131145613259100D-04, &
    1.84631831751005468180D-05, &
    1.42151175831644588870D-07, &
    2.04426310338993978564D-15 /)
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) r
  real ( kind = 8 ) r8poly_value_horner
  real ( kind = 8 ), parameter :: split1 = 0.425D+00
  real ( kind = 8 ), parameter :: split2 = 5.0D+00
  real ( kind = 8 ) x

  if ( p <= 0.0D+00 ) then
    x = - huge ( x )
    return
  end if

  if ( 1.0D+00 <= p ) then
    x = huge ( x )
    return
  end if

  q = p - 0.5D+00

  if ( abs ( q ) <= split1 ) then

    r = const1 - q * q
    x = q * r8poly_value_horner ( 7, a, r ) &
          / r8poly_value_horner ( 7, b, r )

  else

    if ( q < 0.0D+00 ) then
      r = p
    else
      r = 1.0D+00 - p
    end if

    if ( r <= 0.0D+00 ) then

      x = huge ( x )

    else

      r = sqrt ( - log ( r ) )

      if ( r <= split2 ) then

        r = r - const2
        x = r8poly_value_horner ( 7, c, r ) &
          / r8poly_value_horner ( 7, d, r )

      else

        r = r - split2
        x = r8poly_value_horner ( 7, e, r ) &
          / r8poly_value_horner ( 7, f, r )

      end if

    end if

    if ( q < 0.0D+00 ) then
      x = -x
    end if

  end if

  return
end
subroutine normal_01_cdf_values ( n_data, x, fx )

!*****************************************************************************80
!
!! NORMAL_01_CDF_VALUES returns some values of the Normal 01 CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = NormalDistribution [ 0, 1 ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2004
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
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

  integer ( kind = 4 ), parameter :: n_max = 17

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.5000000000000000D+00, &
    0.5398278372770290D+00, &
    0.5792597094391030D+00, &
    0.6179114221889526D+00, &
    0.6554217416103242D+00, &
    0.6914624612740131D+00, &
    0.7257468822499270D+00, &
    0.7580363477769270D+00, &
    0.7881446014166033D+00, &
    0.8159398746532405D+00, &
    0.8413447460685429D+00, &
    0.9331927987311419D+00, &
    0.9772498680518208D+00, &
    0.9937903346742239D+00, &
    0.9986501019683699D+00, &
    0.9997673709209645D+00, &
    0.9999683287581669D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.0000000000000000D+00, &
    0.1000000000000000D+00, &
    0.2000000000000000D+00, &
    0.3000000000000000D+00, &
    0.4000000000000000D+00, &
    0.5000000000000000D+00, &
    0.6000000000000000D+00, &
    0.7000000000000000D+00, &
    0.8000000000000000D+00, &
    0.9000000000000000D+00, &
    0.1000000000000000D+01, &
    0.1500000000000000D+01, &
    0.2000000000000000D+01, &
    0.2500000000000000D+01, &
    0.3000000000000000D+01, &
    0.3500000000000000D+01, &
    0.4000000000000000D+01 /)

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
subroutine normal_01_mean ( mean )

!*****************************************************************************80
!
!! NORMAL_01_MEAN returns the mean of the Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) mean

  mean = 0.0D+00

  return
end
subroutine normal_01_moment ( order, value )

!*****************************************************************************80
!
!! NORMAL_01_MOMENT evaluates moments of the Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the moment.
!    0 <= ORDER.
!
!    Output, real ( kind = 8 ) VALUE, the value of the moment.
!
  implicit none

  integer ( kind = 4 ) order
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ) value

  if ( mod ( order, 2 ) == 0 ) then
    value = r8_factorial2 ( order - 1 )
  else
    value = 0.0D+00
  end if

  return
end
subroutine normal_01_pdf ( x, pdf )

!*****************************************************************************80
!
!! NORMAL_01_PDF evaluates the Normal 01 PDF.
!
!  Discussion:
!
!    The Normal 01 PDF is also called the "Standard Normal" PDF, or
!    the Normal PDF with 0 mean and standard deviation 1.
!
!    PDF(X) = exp ( - 0.5 * X^2 ) / sqrt ( 2 * PI )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  pdf = exp ( -0.5D+00 * x * x ) / sqrt ( 2.0D+00 * r8_pi )

  return
end
subroutine normal_01_sample ( seed, x )

!*****************************************************************************80
!
!! NORMAL_01_SAMPLE samples the standard normal probability distribution.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    The Box-Muller method is used, which is efficient, but
!    generates two values at a time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the standard normal PDF.
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ), save :: used = -1
  real ( kind = 8 ) x
  real ( kind = 8 ), save :: y = 0.0D+00

  if ( used == -1 ) then
    used = 0
  end if
!
!  If we've used an even number of values so far, generate two more,
!  return one and save one.
!
  if ( mod ( used, 2 ) == 0 ) then

    do

      r1 = r8_uniform_01 ( seed )

      if ( r1 /= 0.0D+00 ) then
        exit
      end if

    end do

    r2 = r8_uniform_01 ( seed )

    x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * r8_pi * r2 )
    y = sqrt ( - 2.0D+00 * log ( r1 ) ) * sin ( 2.0D+00 * r8_pi * r2 )
!
!  Otherwise, return the second, saved, value.
!
  else

    x = y

  end if

  used = used + 1

  return
end
subroutine normal_01_variance ( variance )

!*****************************************************************************80
!
!! NORMAL_01_VARIANCE returns the variance of the Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) variance

  variance = 1.0D+00

  return
end
subroutine normal_ms_cdf ( x, mu, sigma, cdf )

!*****************************************************************************80
!
!! NORMAL_MS_CDF evaluates the Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters.
!    0.0D+00 < SIGMA.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  y = ( x - mu ) / sigma

  call normal_01_cdf ( y, cdf )

  return
end
subroutine normal_ms_cdf_inv ( cdf, mu, sigma, x )

!*****************************************************************************80
!
!! NORMAL_MS_CDF_INV inverts the Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0D+00 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters.
!    0.0D+00 < SIGMA.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'NORMAL_MS_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    write ( *, '(a,g14.6)' ) '  CDF = ', cdf
    stop 1
  end if

  call normal_01_cdf_inv ( cdf, x2 )

  x = mu + sigma * x2

  return
end
subroutine normal_ms_mean ( mu, sigma, mean )

!*****************************************************************************80
!
!! NORMAL_MS_MEAN returns the mean of the Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters.
!    0.0D+00 < SIGMA.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma

  mean = mu

  return
end
subroutine normal_ms_moment ( order, mu, sigma, value )

!*****************************************************************************80
!
!! NORMAL_MS_MOMENT evaluates moments of the Normal PDF.
!
!  Discussion:
!
!    The formula was posted by John D Cook.
!
!    Order  Moment
!    -----  ------
!      0    1
!      1    mu
!      2    mu^2 +         sigma^2
!      3    mu^3 +  3 mu   sigma^2
!      4    mu^4 +  6 mu^2 sigma^2 +   3      sigma^4
!      5    mu^5 + 10 mu^3 sigma^2 +  15 mu   sigma^4
!      6    mu^6 + 15 mu^4 sigma^2 +  45 mu^2 sigma^4 +  15      sigma^6
!      7    mu^7 + 21 mu^5 sigma^2 + 105 mu^3 sigma^4 + 105 mu   sigma^6
!      8    mu^8 + 28 mu^6 sigma^2 + 210 mu^4 sigma^4 + 420 mu^2 sigma^6 
!           + 105 sigma^8
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the moment.
!    0 <= ORDER.
!
!    Input, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Input, real ( kind = 8 ) SIGMA, the standard deviation of the distribution.
!
!    Output, real ( kind = 8 ) VALUE, the value of the central moment.
!
  implicit none

  integer ( kind = 4 ) j
  integer ( kind = 4 ) j_hi
  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ) sigma
  real ( kind = 8 ) value

  j_hi = ( order / 2 )

  value = 0.0D+00
  do j = 0, j_hi
    value = value &
      + r8_choose ( order, 2 * j ) &
      * r8_factorial2 ( 2 * j - 1 ) &
      * mu ** ( order - 2 * j ) * sigma ** ( 2 * j )
  end do

  return
end
subroutine normal_ms_moment_central ( order, mu, sigma, value )

!*****************************************************************************80
!
!! NORMAL_MS_MOMENT_CENTRAL evaluates central moments of the Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the moment.
!    0 <= ORDER.
!
!    Input, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Input, real ( kind = 8 ) SIGMA, the standard deviation of the distribution.
!
!    Output, real ( kind = 8 ) VALUE, the value of the central moment.
!
  implicit none

  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ) sigma
  real ( kind = 8 ) value

  if ( mod ( order, 2 ) == 0 ) then
    value = r8_factorial2 ( order - 1 ) * sigma ** order
  else
    value = 0.0D+00
  end if

  return
end
subroutine normal_ms_moment_central_values ( order, mu, sigma, value )

!*****************************************************************************80
!
!! NORMAL_MS_MOMENT_CENTRAL_VALUES: moments 0 through 10 of the Normal PDF.
!
!  Discussion:
!
!    The formula was posted by John D Cook.
!
!    Order  Moment
!    -----  ------
!      0    1
!      1    0
!      2    sigma^2
!      3    0
!      4    3 sigma^4
!      5    0
!      6    15 sigma^6
!      7    0
!      8    105 sigma^8
!      9    0
!     10    945 sigma^10
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the moment.
!    0 <= ORDER <= 10.
!
!    Input, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Input, real ( kind = 8 ) SIGMA, the standard deviation of the distribution.
!
!    Output, real ( kind = 8 ) VALUE, the value of the central moment.
!
  implicit none

  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) sigma
  real ( kind = 8 ) value

  if ( order == 0 ) then
    value = 1.0D+00
  else if ( order == 1 ) then
    value = 0.0D+00
  else if ( order == 2 ) then
    value = sigma ** 2
  else if ( order == 3 ) then
    value = 0.0D+00
  else if ( order == 4 ) then
    value = 3.0D+00 * sigma ** 4
  else if ( order == 5 ) then
    value = 0.0D+00
  else if ( order == 6 ) then
    value = 15.0D+00 * sigma ** 6
  else if ( order == 7 ) then
    value = 0.0D+00
  else if ( order == 8 ) then
    value = 105.0D+00 * sigma ** 8
  else if ( order == 9 ) then
    value = 0.0D+00
  else if ( order == 10 ) then
    value = 945.0D+00 * sigma ** 10
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'NORMAL_AB_MOMENT_CENTRAL_VALUES - Fatal error!'
    write ( *, '(a)' ) '  Only ORDERS 0 through 10 are available.'
    stop 1
  end if

  return
end
subroutine normal_ms_moment_values ( order, mu, sigma, value )

!*****************************************************************************80
!
!! NORMAL_MS_MOMENT_VALUES evaluates moments 0 through 8 of the Normal PDF.
!
!  Discussion:
!
!    The formula was posted by John D Cook.
!
!    Order  Moment
!    -----  ------
!      0    1
!      1    mu
!      2    mu^2 +         sigma^2
!      3    mu^3 +  3 mu   sigma^2
!      4    mu^4 +  6 mu^2 sigma^2 +   3      sigma^4
!      5    mu^5 + 10 mu^3 sigma^2 +  15 mu   sigma^4
!      6    mu^6 + 15 mu^4 sigma^2 +  45 mu^2 sigma^4 +  15      sigma^6
!      7    mu^7 + 21 mu^5 sigma^2 + 105 mu^3 sigma^4 + 105 mu   sigma^6
!      8    mu^8 + 28 mu^6 sigma^2 + 210 mu^4 sigma^4 + 420 mu^2 sigma^6 
!           + 105 sigma^8
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the moment.
!    0 <= ORDER <= 8.
!
!    Input, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Input, real ( kind = 8 ) SIGMA, the standard deviation of the distribution.
!
!    Output, real ( kind = 8 ) VALUE, the value of the central moment.
!
  implicit none

  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) sigma
  real ( kind = 8 ) value

  if ( order == 0 ) then
    value = 1.0D+00
  else if ( order == 1 ) then
    value = mu
  else if ( order == 2 ) then
    value = mu ** 2 + sigma ** 2
  else if ( order == 3 ) then
    value = mu ** 3 + 3.0D+00 * mu * sigma ** 2
  else if ( order == 4 ) then
    value = mu ** 4 + 6.0D+00 * mu ** 2 * sigma ** 2 + 3.0D+00 * sigma ** 4
  else if ( order == 5 ) then
    value = mu ** 5 + 10.0D+00 * mu ** 3 * sigma ** 2 &
      + 15.0D+00 * mu * sigma ** 4
  else if ( order == 6 ) then
    value = mu ** 6 + 15.0D+00 * mu ** 4 * sigma ** 2 &
      + 45.0D+00 * mu ** 2 * sigma ** 4 &
      + 15.0D+00 * sigma ** 6
  else if ( order == 7 ) then
    value = mu ** 7 + 21.0D+00 * mu ** 5 * sigma ** 2 &
      + 105.0D+00 * mu ** 3 * sigma ** 4 &
      + 105.0D+00 * mu * sigma ** 6
  else if ( order == 8 ) then
    value = mu ** 8 + 28.0D+00 * mu ** 6 * sigma ** 2 &
      + 210.0D+00 * mu ** 4 * sigma ** 4 &
      + 420.0D+00 * mu ** 2 * sigma ** 6 + 105.0D+00 * sigma ** 8
  else
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'NORMAL_MOMENT_VALUES - Fatal error!'
    write ( *, '(a)' ) '  Only ORDERS 0 through 8 are available.'
    stop 1
  end if

  return
end
subroutine normal_ms_pdf ( x, mu, sigma, pdf )

!*****************************************************************************80
!
!! NORMAL_MS_PDF evaluates the Normal PDF.
!
!  Discussion:
!
!    PDF(MU,SIGMA;X)
!      = exp ( - 0.5D+00 * ( ( X - MU ) / SIGMA )^2 ) 
!        / ( SIGMA * sqrt ( 2 * PI ) )
!
!    The normal PDF is also known as the Gaussian PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters.
!    0.0D+00 < SIGMA.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) x01

  x01 = ( x - mu ) / sigma

  pdf = exp ( - 0.5D+00 * x01 * x01 )  / ( sigma * sqrt ( 2.0D+00 * r8_pi ) )

  return
end
subroutine normal_ms_sample ( mu, sigma, seed, x )

!*****************************************************************************80
!
!! NORMAL_MS_SAMPLE samples the Normal PDF.
!
!  Discussion:
!
!    The Box-Muller method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters.
!    0.0D+00 < SIGMA.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) x01

  call normal_01_sample ( seed, x01 )

  x = mu + sigma * x01

  return
end
subroutine normal_ms_variance ( mu, sigma, variance )

!*****************************************************************************80
!
!! NORMAL_MS_VARIANCE returns the variance of the Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters.
!    0.0D+00 < SIGMA.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance

  variance = sigma * sigma

  return
end
function r8_choose ( n, k )

!*****************************************************************************80
!
!! R8_CHOOSE computes the binomial coefficient C(N,K) as an R8.
!
!  Discussion:
!
!    The value is calculated in such a way as to avoid overflow and
!    roundoff.  The calculation is done in R8 arithmetic.
!
!    The formula used is:
!
!      C(N,K) = N! / ( K! * (N-K)! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2008
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    ML Wolfson, HV Wright,
!    Algorithm 160:
!    Combinatorial of M Things Taken N at a Time,
!    Communications of the ACM,
!    Volume 6, Number 4, April 1963, page 161.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, K, are the values of N and K.
!
!    Output, real ( kind = 8 ) R8_CHOOSE, the number of combinations of N
!    things taken K at a time.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mn
  integer ( kind = 4 ) mx
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) value

  mn = min ( k, n - k )

  if ( mn < 0 ) then

    value = 0.0D+00

  else if ( mn == 0 ) then

    value = 1.0D+00

  else

    mx = max ( k, n - k )
    value = real ( mx + 1, kind = 8 )

    do i = 2, mn
      value = ( value * real ( mx + i, kind = 8 ) ) / real ( i, kind = 8 )
    end do

  end if

  r8_choose = value

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
!     N Value
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
!    function.  If N is less than 1, the value is returned as 1.0.
!
!    Output, real ( kind = 8 ) R8_FACTORIAL2, the value.
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ) r8_n

  if ( n < 1 ) then
    r8_factorial2 = 1.0D+00
    return
  end if

  r8_n = real ( n, kind = 8 )
  r8_factorial2 = 1.0D+00

  do while ( 1.0D+00 < r8_n )
    r8_factorial2 = r8_factorial2 * r8_n
    r8_n = r8_n - 2.0D+00
  end do

  return
end
subroutine r8_factorial2_values ( n_data, n, f )

!*****************************************************************************80
!
!! R8_FACTORIAL2_VALUES returns values of the double factorial function.
!
!  Discussion:
!
!    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
!                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
!
!    In Mathematica, the function can be evaluated by:
!
!      n!!
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
!    18 August 2004
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996,
!    ISBN: 0-8493-2479-3,
!    LC: QA47.M315.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) N, the argument of the function.
!
!    Output, real ( kind = 8 ) F, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 16

  real ( kind = 8 ), save, dimension ( n_max ) :: f_vec = (/ &
          1.0D+00, &
          1.0D+00, &
          2.0D+00, &
          3.0D+00, &
          8.0D+00, &
         15.0D+00, &
         48.0D+00, &
        105.0D+00, &
        384.0D+00, &
        945.0D+00, &
       3840.0D+00, &
      10395.0D+00, &
      46080.0D+00, &
     135135.0D+00, &
     645120.0D+00, &
    2027025.0D+00 /)
  real ( kind = 8 ) f
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) n
  integer ( kind = 4 ), save, dimension ( n_max ) :: n_vec = (/ &
     0, &
     1,  2,  3,  4,  5, &
     6,  7,  8,  9, 10, &
    11, 12, 13, 14, 15 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    f = 0.0D+00
  else
    n = n_vec(n_data)
    f = f_vec(n_data)
  end if

  return
end
function r8_mop ( i )

!*****************************************************************************80
!
!! R8_MOP returns the I-th power of -1 as an R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 November 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the power of -1.
!
!    Output, real ( kind = 8 ) R8_MOP, the I-th power of -1.
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_mop

  if ( mod ( i, 2 ) == 0 ) then
    r8_mop = + 1.0D+00
  else
    r8_mop = - 1.0D+00
  end if

  return
end
function r8_uniform_01 ( seed )

!*****************************************************************************80
!
!! R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer variable.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2^31 - 1 )
!      r8_uniform_01 = seed / ( 2^31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R8_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0. On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
  implicit none

  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_01 = real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
subroutine r8poly_print ( n, a, title )

!*****************************************************************************80
!
!! R8POLY_PRINT prints out a polynomial.
!
!  Discussion:
!
!    The power sum form is:
!
!      p(x) = a(0) + a(1) * x + ... + a(n-1) * x^(n-1) + a(n) * x^(n)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of A.
!
!    Input, real ( kind = 8 ) A(0:N), the polynomial coefficients.
!    A(0) is the constant term and
!    A(N) is the coefficient of X^N.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(0:n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) mag
  character plus_minus
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( a(n) < 0.0D+00 ) then
    plus_minus = '-'
  else
    plus_minus = ' '
  end if

  mag = abs ( a(n) )

  if ( 2 <= n ) then
    write ( *, '( ''  p(x) = '', a1, g14.6, '' * x ^ '', i3 )' ) &
      plus_minus, mag, n
  else if ( n == 1 ) then
    write ( *, '( ''  p(x) = '', a1, g14.6, '' * x'' )' ) &
      plus_minus, mag
  else if ( n == 0 ) then
    write ( *, '( ''  p(x) = '', a1, g14.6 )' ) plus_minus, mag
  end if

  do i = n - 1, 0, -1

    if ( a(i) < 0.0D+00 ) then
      plus_minus = '-'
    else
      plus_minus = '+'
    end if

    mag = abs ( a(i) )

    if ( mag /= 0.0D+00 ) then

      if ( 2 <= i ) then
        write ( *, ' ( ''         '', a1, g14.6, '' * x ^ '', i3 )' ) &
          plus_minus, mag, i
      else if ( i == 1 ) then
        write ( *, ' ( ''         '', a1, g14.6, '' * x'' )' ) plus_minus, mag
      else if ( i == 0 ) then
        write ( *, ' ( ''         '', a1, g14.6 )' ) plus_minus, mag
      end if
    end if

  end do

  return
end
function r8poly_value_horner ( m, c, x )

!*****************************************************************************80
!
!! R8POLY_VALUE_HORNER evaluates a polynomial using Horner's method.
!
!  Discussion:
!
!    The polynomial 
!
!      p(x) = c0 + c1 * x + c2 * x^2 + ... + cm * x^m
!
!    is to be evaluated at the value X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the degree.
!
!    Input, real ( kind = 8 ) C(0:M), the polynomial coefficients.  
!    C(I) is the coefficient of X^I.
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) R8POLY_VALUE_HORNER, the polynomial value.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) c(0:m)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8poly_value_horner
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  value = c(m)
  do i = m - 1, 0, -1
    value = value * x + c(i)
  end do

  r8poly_value_horner = value

  return
end
subroutine r8vec_linspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_LINSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
!
!    In other words, the interval is divided into N-1 even subintervals,
!    and the endpoints of intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the first and last entries.
!
!    Output, real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n
      x(i) = ( real ( n - i,     kind = 8 ) * a   &
             + real (     i - 1, kind = 8 ) * b ) &
             / real ( n     - 1, kind = 8 )
    end do

  end if

  return
end
subroutine r8vec_max ( n, a, amax )

!*****************************************************************************80
!
!! R8VEC_MAX returns the maximum value in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) AMAX, the value of the largest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax

  amax = maxval ( a(1:n) )

  return
end
subroutine r8vec_mean ( n, x, mean )

!*****************************************************************************80
!
!! R8VEC_MEAN returns the mean of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(N), the vector whose mean is desired.
!
!    Output, real ( kind = 8 ) MEAN, the mean, or average,
!    of the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) mean
  real ( kind = 8 ) x(n)

  mean = sum ( x(1:n) ) / real ( n, kind = 8 )

  return
end
subroutine r8vec_min ( n, a, amin )

!*****************************************************************************80
!
!! R8VEC_MIN returns the minimum value of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) AMIN, the value of the smallest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amin

  amin = minval ( a(1:n) )

  return
end
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

  return
end
subroutine r8vec_variance ( n, x, variance )

!*****************************************************************************80
!
!! R8VEC_VARIANCE returns the variance of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(N), the vector whose variance is desired.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) mean
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(n)

  call r8vec_mean ( n, x, mean )

  variance = sum ( ( x(1:n) - mean ) ** 2 )

  if ( 1 < n ) then
    variance = variance / real ( n - 1, kind = 8 )
  else
    variance = 0.0D+00
  end if

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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
subroutine truncated_normal_ab_cdf ( x, mu, s, a, b, cdf )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_CDF evaluates the truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) s
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  if ( x < a ) then
  
    cdf = 0.0D+00
    
  else if ( x <= b ) then
  
    alpha = ( a - mu ) / s
    beta = ( b - mu ) / s
    xi = ( x - mu ) / s

    call normal_01_cdf ( alpha, alpha_cdf )
    call normal_01_cdf ( beta, beta_cdf )
    call normal_01_cdf ( xi, xi_cdf )

    cdf = ( xi_cdf - alpha_cdf ) / ( beta_cdf - alpha_cdf )

  else
  
    cdf = 1.0D+00
    
  end if
  
  return
end
subroutine truncated_normal_ab_cdf_values ( n_data, mu, sigma, a, b, x, fx )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_CDF_VALUES: values of the Truncated Normal CDF.
!
!  Discussion:
!
!    The Normal distribution, with mean Mu and standard deviation Sigma,
!    is truncated to the interval [A,B].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the standard deviation
!    of the distribution.
!
!    Output, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), save, dimension ( n_max ) :: b_vec = (/ &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
      0.3371694242213513D+00, &
      0.3685009225506048D+00, &
      0.4006444233448185D+00, &
      0.4334107066903040D+00, &
      0.4665988676496338D+00, &
      0.5000000000000000D+00, &
      0.5334011323503662D+00, &
      0.5665892933096960D+00, &
      0.5993555766551815D+00, &
      0.6314990774493952D+00, &
      0.6628305757786487D+00 /)
  real ( kind = 8 ) mu
  real ( kind = 8 ), save, dimension ( n_max ) :: mu_vec = (/ &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ), save, dimension ( n_max ) :: sigma_vec = (/ &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
       90.0D+00, &
       92.0D+00, &
       94.0D+00, &
       96.0D+00, &
       98.0D+00, &
      100.0D+00, &
      102.0D+00, &
      104.0D+00, &
      106.0D+00, &
      108.0D+00, &
      110.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0.0D+00
    b = 0.0D+00
    mu = 0.0D+00
    sigma = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    b = b_vec(n_data)
    mu = mu_vec(n_data)
    sigma = sigma_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine truncated_normal_ab_cdf_inv ( cdf, mu, sigma, a, b, x )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_CDF_INV inverts the truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0D+00 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  alpha = ( a - mu ) / sigma
  beta = ( b - mu ) / sigma

  call normal_01_cdf ( alpha, alpha_cdf )
  call normal_01_cdf ( beta, beta_cdf )

  xi_cdf = ( beta_cdf - alpha_cdf ) * cdf + alpha_cdf
  call normal_01_cdf_inv ( xi_cdf, xi )

  x = mu + sigma * xi

  return
end
subroutine truncated_normal_ab_mean ( mu, sigma, a, b, mean )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_MEAN returns the mean of the truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) alpha_pdf
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) beta_pdf
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma

  alpha = ( a - mu ) / sigma
  beta = ( b - mu ) / sigma

  call normal_01_cdf ( alpha, alpha_cdf )
  call normal_01_cdf ( beta, beta_cdf )

  call normal_01_pdf ( alpha, alpha_pdf )
  call normal_01_pdf ( beta, beta_pdf )

  mean = mu + sigma * ( alpha_pdf - beta_pdf ) / ( beta_cdf - alpha_cdf )

  return
end
subroutine truncated_normal_ab_moment ( order, mu, sigma, a, b, moment )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_MOMENT: moments of the truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Phoebus Dhrymes,
!    Moments of Truncated Normal Distributions,
!    May 2005.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the moment.
!    0 <= ORDER.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!    0.0 < SIGMA.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!    A < B.
!
!    Output, real ( kind = 8 ) MOMENT, the moment of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a_h
  real ( kind = 8 ) a_cdf
  real ( kind = 8 ) a_pdf
  real ( kind = 8 ) b
  real ( kind = 8 ) b_h
  real ( kind = 8 ) b_cdf
  real ( kind = 8 ) b_pdf
  real ( kind = 8 ) ir
  real ( kind = 8 ) irm1
  real ( kind = 8 ) irm2
  real ( kind = 8 ) moment
  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  integer ( kind = 4 ) r
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) sigma

  if ( order < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  ORDER < 0.'
    stop 1
  end if

  if ( sigma <= 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  SIGMA <= 0.0.'
    stop 1
  end if

  if ( b <= a ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  B <= A.'
    stop 1
  end if

  a_h = ( a - mu ) / sigma
  call normal_01_pdf ( a_h, a_pdf )
  call normal_01_cdf ( a_h, a_cdf )

  if ( a_cdf == 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  PDF/CDF ratio fails, because A_CDF is too small.'
    write ( *, '(a,g14.6)' ) '  A_PDF = ', a_pdf
    write ( *, '(a,g14.6)' ) '  A_CDF = ', a_cdf
    stop 1
  end if

  b_h = ( b - mu ) / sigma
  call normal_01_pdf ( b_h, b_pdf )
  call normal_01_cdf ( b_h, b_cdf )

  if ( b_cdf == 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  PDF/CDF ratio fails, because B_CDF is too small.'
    write ( *, '(a,g14.6)' ) '  B_PDF = ', b_pdf
    write ( *, '(a,g14.6)' ) '  B_CDF = ', b_cdf
    stop 1
  end if

  moment = 0.0D+00
  irm2 = 0.0D+00
  irm1 = 0.0D+00

  do r = 0, order

    if ( r == 0 ) then
      ir = 1.0D+00
    else if ( r == 1 ) then
      ir = - ( b_pdf - a_pdf ) / ( b_cdf - a_cdf )
    else
      ir = real ( r - 1, kind = 8 ) * irm2 &
        - ( b_h ** ( r - 1 ) * b_pdf - a_h ** ( r - 1 ) * a_pdf ) &
        / ( b_cdf - a_cdf )
    end if

    moment = moment + r8_choose ( order, r ) * mu ** ( order - r ) &
      * ( sigma ** r ) * ir

    irm2 = irm1
    irm1 = ir

  end do

  return
end
subroutine truncated_normal_ab_pdf ( x, mu, sigma, a, b, pdf )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_PDF evaluates the truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_pdf

  if ( x < a ) then
  
    pdf = 0.0D+00
    
  else if ( x <= b ) then
  
    alpha = ( a - mu ) / sigma
    beta = ( b - mu ) / sigma
    xi = ( x - mu ) / sigma

    call normal_01_cdf ( alpha, alpha_cdf )
    call normal_01_cdf ( beta, beta_cdf )
    call normal_01_pdf ( xi, xi_pdf )

    pdf = xi_pdf / ( beta_cdf - alpha_cdf ) / sigma

  else
  
    pdf = 0.0D+00
    
  end if
  
  return
end
subroutine truncated_normal_ab_pdf_values ( n_data, mu, sigma, a, b, x, fx )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_PDF_VALUES: values of the Truncated Normal PDF.
!
!  Discussion:
!
!    The Normal distribution, with mean Mu and standard deviation Sigma,
!    is truncated to the interval [A,B].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the standard deviation 
!    of the distribution.
!
!    Output, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), save, dimension ( n_max ) :: b_vec = (/ &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
       0.01543301171801836D+00, &
       0.01588394472270638D+00, &
       0.01624375997031919D+00, &
       0.01650575046469259D+00, &
       0.01666496869385951D+00, &
       0.01671838200940538D+00, &
       0.01666496869385951D+00, &
       0.01650575046469259D+00, &
       0.01624375997031919D+00, &
       0.01588394472270638D+00, &
       0.01543301171801836D+00 /)
  real ( kind = 8 ) mu
  real ( kind = 8 ), save, dimension ( n_max ) :: mu_vec = (/ &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ), save, dimension ( n_max ) :: sigma_vec = (/ &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
       90.0D+00, &
       92.0D+00, &
       94.0D+00, &
       96.0D+00, &
       98.0D+00, &
      100.0D+00, &
      102.0D+00, &
      104.0D+00, &
      106.0D+00, &
      108.0D+00, &
      110.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0.0D+00
    b = 0.0D+00
    mu = 0.0D+00
    sigma = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    b = b_vec(n_data)
    mu = mu_vec(n_data)
    sigma = sigma_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine truncated_normal_ab_sample ( mu, sigma, a, b, seed, x )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_SAMPLE samples the truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) sigma
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  alpha = ( a - mu ) / sigma
  beta = ( b - mu ) / sigma

  call normal_01_cdf ( alpha, alpha_cdf )
  call normal_01_cdf ( beta, beta_cdf )

  u = r8_uniform_01 ( seed )
  xi_cdf = alpha_cdf + u * ( beta_cdf - alpha_cdf )
  call normal_01_cdf_inv ( xi_cdf, xi )

  x = mu + sigma * xi

  return
end
subroutine truncated_normal_ab_variance ( mu, sigma, a, b, variance )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_VARIANCE: variance of the truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) alpha_pdf
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) beta_pdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance

  alpha = ( a - mu ) / sigma
  beta = ( b - mu ) / sigma

  call normal_01_pdf ( alpha, alpha_pdf )
  call normal_01_pdf ( beta, beta_pdf )

  call normal_01_cdf ( alpha, alpha_cdf )
  call normal_01_cdf ( beta, beta_cdf )

  variance = sigma * sigma * ( 1.0D+00 &
    + ( alpha * alpha_pdf - beta * beta_pdf ) / ( beta_cdf - alpha_cdf ) &
    - ( ( alpha_pdf - beta_pdf ) / ( beta_cdf - alpha_cdf ) ) ** 2 )

  return
end
subroutine truncated_normal_a_cdf ( x, mu, sigma, a, cdf )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_CDF evaluates the lower truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  if ( x < a ) then
  
    cdf = 0.0D+00
    
  else
  
    alpha = ( a - mu ) / sigma
    xi = ( x - mu ) / sigma

    call normal_01_cdf ( alpha, alpha_cdf )
    call normal_01_cdf ( xi, xi_cdf )

    cdf = ( xi_cdf - alpha_cdf ) / ( 1.0D+00 - alpha_cdf )

  end if
  
  return
end
subroutine truncated_normal_a_cdf_values ( n_data, mu, sigma, a, x, fx )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_CDF_VALUES: values of the lower Truncated Normal CDF.
!
!  Discussion:
!
!    The Normal distribution, with mean Mu and standard deviation Sigma,
!    is truncated to the interval [A,+oo).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the standard deviation 
!    of the distribution.
!
!    Output, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
       0.3293202045481688D+00, &
       0.3599223134505957D+00, &
       0.3913175216041539D+00, &
       0.4233210140873113D+00, &
       0.4557365629792204D+00, &
       0.4883601253415709D+00, &
       0.5209836877039214D+00, &
       0.5533992365958304D+00, &
       0.5854027290789878D+00, &
       0.6167979372325460D+00, &
       0.6474000461349729D+00 /)
  real ( kind = 8 ) mu
  real ( kind = 8 ), save, dimension ( n_max ) :: mu_vec = (/ &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ), save, dimension ( n_max ) :: sigma_vec = (/ &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
       90.0D+00, &
       92.0D+00, &
       94.0D+00, &
       96.0D+00, &
       98.0D+00, &
      100.0D+00, &
      102.0D+00, &
      104.0D+00, &
      106.0D+00, &
      108.0D+00, &
      110.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0.0D+00
    mu = 0.0D+00
    sigma = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    mu = mu_vec(n_data)
    sigma = sigma_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine truncated_normal_a_cdf_inv ( cdf, mu, sigma, a, x )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_CDF_INV inverts the lower truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0D+00 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  alpha = ( a - mu ) / sigma

  call normal_01_cdf ( alpha, alpha_cdf )

  xi_cdf = ( 1.0D+00 - alpha_cdf ) * cdf + alpha_cdf
  call normal_01_cdf_inv ( xi_cdf, xi )

  x = mu + sigma * xi

  return
end
subroutine truncated_normal_a_mean ( mu, sigma, a, mean )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_MEAN returns the mean of the lower truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) alpha_pdf
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma

  alpha = ( a - mu ) / sigma

  call normal_01_cdf ( alpha, alpha_cdf )

  call normal_01_pdf ( alpha, alpha_pdf )

  mean = mu + sigma * alpha_pdf / ( 1.0D+00 - alpha_cdf )

  return
end
subroutine truncated_normal_a_moment ( order, mu, sigma, a, moment )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_MOMENT: moments of the lower truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Phoebus Dhrymes,
!    Moments of Truncated Normal Distributions,
!    May 2005.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the moment.
!    0 <= ORDER.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!    0.0 < S.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) MOMENT, the moment of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) moment
  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) r8_mop
  real ( kind = 8 ) sigma

  call truncated_normal_b_moment ( order, - mu, sigma, - a, moment )
  moment = r8_mop ( order ) * moment

  return
end
subroutine truncated_normal_a_pdf ( x, mu, sigma, a, pdf )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_PDF evaluates the lower truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_pdf

  if ( x < a ) then
  
    pdf = 0.0D+00
    
  else
  
    alpha = ( a - mu ) / sigma
    xi = ( x - mu ) / sigma

    call normal_01_cdf ( alpha, alpha_cdf )
    call normal_01_pdf ( xi, xi_pdf )

    pdf = xi_pdf / ( 1.0D+00 - alpha_cdf ) / sigma

  end if
  
  return
end
subroutine truncated_normal_a_pdf_values ( n_data, mu, sigma, a, x, fx )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_PDF_VALUES: values of the lower Truncated Normal PDF.
!
!  Discussion:
!
!    The Normal distribution, with mean Mu and standard deviation Sigma,
!    is truncated to the interval [A,+oo).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the standard deviation 
!    of the distribution.
!
!    Output, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00, &
       50.0D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
       0.01507373507401876D+00, &
       0.01551417047139894D+00, &
       0.01586560931024694D+00, &
       0.01612150073158793D+00, &
       0.01627701240029317D+00, &
       0.01632918226724295D+00, &
       0.01627701240029317D+00, &
       0.01612150073158793D+00, &
       0.01586560931024694D+00, &
       0.01551417047139894D+00, &
       0.01507373507401876D+00 /)
  real ( kind = 8 ) mu
  real ( kind = 8 ), save, dimension ( n_max ) :: mu_vec = (/ &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ), save, dimension ( n_max ) :: sigma_vec = (/ &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
       90.0D+00, &
       92.0D+00, &
       94.0D+00, &
       96.0D+00, &
       98.0D+00, &
      100.0D+00, &
      102.0D+00, &
      104.0D+00, &
      106.0D+00, &
      108.0D+00, &
      110.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0.0D+00
    mu = 0.0D+00
    sigma = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    mu = mu_vec(n_data)
    sigma = sigma_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine truncated_normal_a_sample ( mu, sigma, a, seed, x )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_SAMPLE samples the lower truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) sigma
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  alpha = ( a - mu ) / sigma

  call normal_01_cdf ( alpha, alpha_cdf )

  u = r8_uniform_01 ( seed )
  xi_cdf = alpha_cdf + u * ( 1.0D+00 - alpha_cdf )
  call normal_01_cdf_inv ( xi_cdf, xi )

  x = mu + sigma * xi

  return
end
subroutine truncated_normal_a_variance ( mu, sigma, a, variance )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_VARIANCE: variance of the lower truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) alpha_pdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance

  alpha = ( a - mu ) / sigma

  call normal_01_pdf ( alpha, alpha_pdf )

  call normal_01_cdf ( alpha, alpha_cdf )

  variance = sigma * sigma * ( 1.0D+00 &
    + ( alpha * alpha_pdf ) / ( 1.0D+00 - alpha_cdf ) &
    - ( alpha_pdf / ( 1.0D+00 - alpha_cdf ) ) ** 2 )

  return
end
subroutine truncated_normal_b_cdf ( x, mu, sigma, b, cdf )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_CDF evaluates the upper truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  if ( x <= b ) then
  
    beta = ( b - mu ) / sigma
    xi = ( x - mu ) / sigma

    call normal_01_cdf ( beta, beta_cdf )
    call normal_01_cdf ( xi, xi_cdf )

    cdf = xi_cdf / beta_cdf

  else
  
    cdf = 1.0D+00
    
  end if
  
  return
end
subroutine truncated_normal_b_cdf_values ( n_data, mu, sigma, b, x, fx )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_CDF_VALUES: values of the upper Truncated Normal CDF.
!
!  Discussion:
!
!    The Normal distribution, with mean Mu and standard deviation Sigma,
!    is truncated to the interval (-oo,B].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the standard deviation
!    of the distribution.
!
!    Output, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11

  real ( kind = 8 ) b
  real ( kind = 8 ), save, dimension ( n_max ) :: b_vec = (/ &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
       0.3525999538650271D+00, &
       0.3832020627674540D+00, &
       0.4145972709210122D+00, &
       0.4466007634041696D+00, &
       0.4790163122960786D+00, &
       0.5116398746584291D+00, &
       0.5442634370207796D+00, &
       0.5766789859126887D+00, &
       0.6086824783958461D+00, &
       0.6400776865494043D+00, &
       0.6706797954518312D+00 /)
  real ( kind = 8 ) mu
  real ( kind = 8 ), save, dimension ( n_max ) :: mu_vec = (/ &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ), save, dimension ( n_max ) :: sigma_vec = (/ &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
       90.0D+00, &
       92.0D+00, &
       94.0D+00, &
       96.0D+00, &
       98.0D+00, &
      100.0D+00, &
      102.0D+00, &
      104.0D+00, &
      106.0D+00, &
      108.0D+00, &
      110.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    b = 0.0D+00
    mu = 0.0D+00
    sigma = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    b = b_vec(n_data)
    mu = mu_vec(n_data)
    sigma = sigma_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine truncated_normal_b_cdf_inv ( cdf, mu, sigma, b, x )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_CDF_INV inverts the upper truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0D+00 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  beta = ( b - mu ) / sigma

  call normal_01_cdf ( beta, beta_cdf )

  xi_cdf = beta_cdf * cdf
  call normal_01_cdf_inv ( xi_cdf, xi )

  x = mu + sigma * xi

  return
end
subroutine truncated_normal_b_mean ( mu, sigma, b, mean )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_MEAN returns the mean of the upper truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) beta_pdf
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma

  beta = ( b - mu ) / sigma

  call normal_01_cdf ( beta, beta_cdf )

  call normal_01_pdf ( beta, beta_pdf )

  mean = mu - sigma * beta_pdf / beta_cdf

  return
end
subroutine truncated_normal_b_moment ( order, mu, sigma, b, moment )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_MOMENT: moments of the upper truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Phoebus Dhrymes,
!    Moments of Truncated Normal Distributions,
!    May 2005.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the moment.
!    0 <= ORDER.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!    0.0 < SIGMA.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) MOMENT, the moment of the PDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) f
  real ( kind = 8 ) h
  real ( kind = 8 ) h_cdf
  real ( kind = 8 ) h_pdf
  real ( kind = 8 ) ir
  real ( kind = 8 ) irm1
  real ( kind = 8 ) irm2
  real ( kind = 8 ) moment
  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  integer ( kind = 4 ) r
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) sigma

  if ( order < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  ORDER < 0.'
    stop 1
  end if

  h = ( b - mu ) / sigma
  call normal_01_pdf ( h, h_pdf )
  call normal_01_cdf ( h, h_cdf )

  if ( h_cdf == 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  CDF((B-MU)/SIGMA) = 0.'
    stop 1
  end if

  f = h_pdf / h_cdf

  moment = 0.0D+00
  irm2 = 0.0D+00
  irm1 = 0.0D+00

  do r = 0, order

    if ( r == 0 ) then
      ir = 1.0D+00
    else if ( r == 1 ) then
      ir = - f
    else
      ir = - h ** ( r - 1 ) * f + real ( r - 1, kind = 8 ) * irm2
    end if

    moment = moment + r8_choose ( order, r ) * mu ** ( order - r ) &
      * ( sigma ** r ) * ir

    irm2 = irm1
    irm1 = ir

  end do

  return
end
subroutine truncated_normal_b_pdf ( x, mu, sigma, b, pdf )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_PDF evaluates the upper truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_pdf

  if ( x <= b ) then
  
    beta = ( b - mu ) / sigma
    xi = ( x - mu ) / sigma

    call normal_01_cdf ( beta, beta_cdf )
    call normal_01_pdf ( xi, xi_pdf )

    pdf = xi_pdf / beta_cdf / sigma

  else
  
    pdf = 0.0D+00
    
  end if
  
  return
end
subroutine truncated_normal_b_pdf_values ( n_data, mu, sigma, b, x, fx )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_PDF_VALUES: values of the upper Truncated Normal PDF.
!
!  Discussion:
!
!    The Normal distribution, with mean Mu and standard deviation Sigma,
!    is truncated to the interval (-oo,B].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the standard deviation
!    of the distribution.
!
!    Output, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11

  real ( kind = 8 ) b
  real ( kind = 8 ), save, dimension ( n_max ) :: b_vec = (/ &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00, &
       150.0D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
       0.01507373507401876D+00, &
       0.01551417047139894D+00, &
       0.01586560931024694D+00, &
       0.01612150073158793D+00, &
       0.01627701240029317D+00, &
       0.01632918226724295D+00, &
       0.01627701240029317D+00, &
       0.01612150073158793D+00, &
       0.01586560931024694D+00, &
       0.01551417047139894D+00, &
       0.01507373507401876D+00 /)
  real ( kind = 8 ) mu
  real ( kind = 8 ), save, dimension ( n_max ) :: mu_vec = (/ &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00, &
       100.0D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ), save, dimension ( n_max ) :: sigma_vec = (/ &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00, &
       25.0D+00 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
       90.0D+00, &
       92.0D+00, &
       94.0D+00, &
       96.0D+00, &
       98.0D+00, &
      100.0D+00, &
      102.0D+00, &
      104.0D+00, &
      106.0D+00, &
      108.0D+00, &
      110.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    b = 0.0D+00
    mu = 0.0D+00
    sigma = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    b = b_vec(n_data)
    mu = mu_vec(n_data)
    sigma = sigma_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine truncated_normal_b_sample ( mu, sigma, b, seed, x )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_SAMPLE samples the upper truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) sigma
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  beta = ( b - mu ) / sigma

  call normal_01_cdf ( beta, beta_cdf )

  u = r8_uniform_01 ( seed )
  xi_cdf = u * beta_cdf
  call normal_01_cdf_inv ( xi_cdf, xi )

  x = mu + sigma * xi

  return
end
subroutine truncated_normal_b_variance ( mu, sigma, b, variance )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_VARIANCE: variance of the upper truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) beta_pdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance

  beta = ( b - mu ) / sigma

  call normal_01_pdf ( beta, beta_pdf )

  call normal_01_cdf ( beta, beta_cdf )

  variance = sigma * sigma * ( 1.0D+00 &
    - ( beta * beta_pdf ) / beta_cdf &
    - ( beta_pdf / beta_cdf ) ** 2 )

  return
end
