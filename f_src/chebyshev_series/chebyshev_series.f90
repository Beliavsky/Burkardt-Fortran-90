subroutine echebser0 ( x, coef, nc, y0 )

!*****************************************************************************80
!
!! ECHEBSER0 evaluates a Chebyshev series.
!
!  Discussion:
!
!    This function implements a modification and extension of 
!    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
!    gives an example for treating the first derivative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 April 2014
!
!  Author:
!
!    Manfred Zimmer
!
!  Reference:
!
!    Charles Clenshaw,
!    Mathematical Tables, Volume 5,
!    Chebyshev series for mathematical functions,
!    London, 1962.
!
!    Gerhard Maess,
!    Vorlesungen ueber Numerische Mathematik II, Analysis,
!    Berlin, Akademie_Verlag, 1984-1988,
!    ISBN: 978-3764318840,
!    LC: QA297.M325.  
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!    -1 <= X <= +1.
!
!    Input, real ( kind = 8 ) COEF(NC), the Chebyshev series.
!
!    Input, integer ( kind = 4 ) NC, the number of terms in the series.
!    0 < NC.
!
!    Output, real ( kind = 8 ) Y0, the value of the Chebyshev series at X.
!
  implicit none

  integer ( kind = 4 ) nc

  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) coef(nc)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) y0

  b0 = coef(nc)
  b1 = 0.0D+00
  b2 = 0.0D+00

  x2 = 2.0D+00 * x

  do i = nc - 1, 1, -1
    b2 = b1
    b1 = b0
    b0 = coef(i) - b2 + x2 * b1
  end do

  y0 = 0.5D+00 * ( b0 - b2 )

  return
end
subroutine echebser1 ( x, coef, nc, y0, y1 )

!*****************************************************************************80
!
!! ECHEBSER1 evaluates a Chebyshev series and first derivative.
!
!  Discussion:
!
!    This function implements a modification and extension of 
!    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
!    gives an example for treating the first derivative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 April 2014
!
!  Author:
!
!    Manfred Zimmer
!
!  Reference:
!
!    Charles Clenshaw,
!    Mathematical Tables, Volume 5,
!    Chebyshev series for mathematical functions,
!    London, 1962.
!
!    Gerhard Maess,
!    Vorlesungen ueber Numerische Mathematik II, Analysis,
!    Berlin, Akademie_Verlag, 1984-1988,
!    ISBN: 978-3764318840,
!    LC: QA297.M325.  
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!    -1 <= X <= +1.
!
!    Input, real ( kind = 8 ) COEF(NC), the Chebyshev series.
!
!    Input, integer ( kind = 4 ) NC, the number of terms in the series.
!    0 < NC.
!
!    Output, real ( kind = 8 ) Y0, the value of the Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y1, the value of the 1st derivative of the
!    Chebyshev series at X.
!
  implicit none

  integer ( kind = 4 ) nc

  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) c0
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) coef(nc)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1

  b0 = coef(nc)
  b1 = 0.0D+00
  b2 = 0.0D+00

  c0 = coef(nc)
  c1 = 0.0D+00
  c2 = 0.0D+00

  x2 = 2.0D+00 * x

  do i = nc - 1, 1, -1

    b2 = b1
    b1 = b0
    b0 = coef(i) - b2 + x2 * b1

    if ( 1 < i ) then
      c2 = c1
      c1 = c0
      c0 = b0 - c2 + x2 * c1
    end if

  end do

  y0 = 0.5D+00 * ( b0 - b2 )
  y1 = c0 - c2

  return
end
subroutine echebser2 ( x, coef, nc, y0, y1, y2 )

!*****************************************************************************80
!
!! ECHEBSER2 evaluates a Chebyshev series and two derivatives.
!
!  Discussion:
!
!    This function implements a modification and extension of 
!    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
!    gives an example for treating the first derivative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 April 2014
!
!  Author:
!
!    Manfred Zimmer
!
!  Reference:
!
!    Charles Clenshaw,
!    Mathematical Tables, Volume 5,
!    Chebyshev series for mathematical functions,
!    London, 1962.
!
!    Gerhard Maess,
!    Vorlesungen ueber Numerische Mathematik II, Analysis,
!    Berlin, Akademie_Verlag, 1984-1988,
!    ISBN: 978-3764318840,
!    LC: QA297.M325.  
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!    -1 <= X <= +1.
!
!    Input, real ( kind = 8 ) COEF(NC), the Chebyshev series.
!
!    Input, integer ( kind = 4 ) NC, the number of terms in the series.
!    0 < NC.
!
!    Output, real ( kind = 8 ) Y0, the value of the Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y1, the value of the 1st derivative of the
!    Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y2, the value of the 2nd derivative of the
!    Chebyshev series at X.
!
  implicit none

  integer ( kind = 4 ) nc

  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) c0
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) coef(nc)
  real ( kind = 8 ) d0
  real ( kind = 8 ) d1
  real ( kind = 8 ) d2
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2

  b0 = coef(nc)
  b1 = 0.0D+00
  b2 = 0.0D+00
  c0 = coef(nc)
  c1 = 0.0D+00
  c2 = 0.0D+00
  d0 = coef(nc)
  d1 = 0.0D+00
  d2 = 0.0D+00

  x2 = 2.0D+00 * x

  do i = nc - 1, 1, -1

    b2 = b1
    b1 = b0
    b0 = coef(i) - b2 + x2 * b1

    if ( 1 < i ) then
      c2 = c1
      c1 = c0
      c0 = b0 - c2 + x2 * c1
    end if

    if ( 2 < i ) then
      d2 = d1
      d1 = d0
      d0 = c0 - d2 + x2 * d1
    end if

  end do

  y0 = 0.5D+00 * ( b0 - b2 )
  y1 = c0 - c2
  y2 = ( d0 - d2 ) * 4.0D+00

  return
end
subroutine echebser3 ( x, coef, nc, y0, y1, y2, y3 )

!*****************************************************************************80
!
!! ECHEBSER3 evaluates a Chebyshev series and three derivatives.
!
!  Discussion:
!
!    This function implements a modification and extension of 
!    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
!    gives an example for treating the first derivative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 April 2014
!
!  Author:
!
!    Manfred Zimmer
!
!  Reference:
!
!    Charles Clenshaw,
!    Mathematical Tables, Volume 5,
!    Chebyshev series for mathematical functions,
!    London, 1962.
!
!    Gerhard Maess,
!    Vorlesungen ueber Numerische Mathematik II, Analysis,
!    Berlin, Akademie_Verlag, 1984-1988,
!    ISBN: 978-3764318840,
!    LC: QA297.M325.  
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!    -1 <= X <= +1.
!
!    Input, real ( kind = 8 ) COEF(NC), the Chebyshev series.
!
!    Input, integer ( kind = 4 ) NC, the number of terms in the series.
!    0 < NC.
!
!    Output, real ( kind = 8 ) Y0, the value of the Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y1, the value of the 1st derivative of the
!    Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y2, the value of the 2nd derivative of the
!    Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y3, the value of the 3rd derivative of the
!    Chebyshev series at X.
!
  implicit none

  integer ( kind = 4 ) nc

  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) c0
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) coef(nc)
  real ( kind = 8 ) d0
  real ( kind = 8 ) d1
  real ( kind = 8 ) d2
  real ( kind = 8 ) e0
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3

  b0 = coef(nc)
  b1 = 0.0D+00
  b2 = 0.0D+00
  c0 = coef(nc)
  c1 = 0.0D+00
  c2 = 0.0D+00
  d0 = coef(nc)
  d1 = 0.0D+00
  d2 = 0.0D+00
  e0 = coef(nc)
  e1 = 0.0D+00
  e2 = 0.0D+00

  x2 = 2.0D+00 * x

  do i = nc - 1, 1, -1

    b2 = b1
    b1 = b0
    b0 = coef(i) - b2 + x2 * b1

    if ( 1 < i ) then
      c2 = c1
      c1 = c0
      c0 = b0 - c2 + x2 * c1
    end if

    if ( 2 < i ) then
      d2 = d1
      d1 = d0
      d0 = c0 - d2 + x2 * d1
    end if

    if ( 3 < i ) then
      e2 = e1
      e1 = e0
      e0 = d0 - e2 + x2 * e1
    end if

  end do

  y0 = 0.5D+00 * ( b0 - b2 )
  y1 = c0 - c2
  y2 = ( d0 - d2 ) * 4.0D+00
  y3 = ( e0 - e2 ) * 24.0D+00

  return
end
subroutine echebser4 ( x, coef, nc, y0, y1, y2, y3, y4 )

!*****************************************************************************80
!
!! ECHEBSER4 evaluates a Chebyshev series and four derivatives.
!
!  Discussion:
!
!    This function implements a modification and extension of 
!    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
!    gives an example for treating the first derivative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 April 2014
!
!  Author:
!
!    Manfred Zimmer
!
!  Reference:
!
!    Charles Clenshaw,
!    Mathematical Tables, Volume 5,
!    Chebyshev series for mathematical functions,
!    London, 1962.
!
!    Gerhard Maess,
!    Vorlesungen ueber Numerische Mathematik II, Analysis,
!    Berlin, Akademie_Verlag, 1984-1988,
!    ISBN: 978-3764318840,
!    LC: QA297.M325.  
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!    -1 <= X <= +1.
!
!    Input, real ( kind = 8 ) COEF(NC), the Chebyshev series.
!
!    Input, integer ( kind = 4 ) NC, the number of terms in the series.
!    0 < NC.
!
!    Output, real ( kind = 8 ) Y0, Y1, Y2, Y3, Y4, the value of the 
!    Chebyshev series and its first four derivatives at X.
!
  implicit none

  integer ( kind = 4 ) nc

  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) c0
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) coef(nc)
  real ( kind = 8 ) d0
  real ( kind = 8 ) d1
  real ( kind = 8 ) d2
  real ( kind = 8 ) e0
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) f0
  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) y4

  b0 = coef(nc)
  b1 = 0.0D+00
  b2 = 0.0D+00
  c0 = coef(nc)
  c1 = 0.0D+00
  c2 = 0.0D+00
  d0 = coef(nc)
  d1 = 0.0D+00
  d2 = 0.0D+00
  e0 = coef(nc)
  e1 = 0.0D+00
  e2 = 0.0D+00
  f0 = coef(nc)
  f1 = 0.0D+00
  f2 = 0.0D+00

  do i = nc - 1, 1, -1

    b2 = b1
    b1 = b0
    b0 = coef(i) - b2 + 2.0D+00 * x * b1

    if ( 1 < i ) then
      c2 = c1
      c1 = c0
      c0 = b0 - c2 + 2.0D+00 * x * c1
    end if

    if ( 2 < i ) then
      d2 = d1
      d1 = d0
      d0 = c0 - d2 + 2.0D+00 * x * d1
    end if

    if ( 3 < i ) then
      e2 = e1
      e1 = e0
      e0 = d0 - e2 + 2.0D+00 * x * e1
    end if

    if ( 4 < i ) then
      f2 = f1
      f1 = f0
      f0 = e0 - f2 + 2.0D+00 * x * f1
    end if

  end do

  y0 = ( b0 - b2 )            / 2.0D+00
  y1 =   c0 - c2
  y2 = ( d0 - d2 ) *  2.0D+00 * 2.0D+00
  y3 = ( e0 - e2 ) *  6.0D+00 * 4.0D+00
  y4 = ( f0 - f2 ) * 24.0D+00 * 8.0D+00

  return
end
subroutine evenchebser0 ( x, coef, nc, y0 )

!*****************************************************************************80
!
!! EVENCHEBSER0 evaluates an even Chebyshev series.
!
!  Discussion:
!
!    This function implements Clenshaw's modification of his
!    algorithm for even series.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 April 2014
!
!  Author:
!
!    Manfred Zimmer
!
!  Reference:
!
!    Charles Clenshaw,
!    Mathematical Tables, Volume 5,
!    Chebyshev series for mathematical functions,
!    London, 1962.
!
!    Gerhard Maess,
!    Vorlesungen ueber Numerische Mathematik II, Analysis,
!    Berlin, Akademie_Verlag, 1984-1988,
!    ISBN: 978-3764318840,
!    LC: QA297.M325.  
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!    -1 <= X <= +1.
!
!    Input, real ( kind = 8 ) COEF(NC), the Chebyshev series.
!
!    Input, integer ( kind = 4 ) NC, the number of terms in the series.
!    0 < NC.
!
!    Output, real ( kind = 8 ) Y0, the value of the Chebyshev series at X.
!
  implicit none

  integer ( kind = 4 ) nc

  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) coef(nc)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) y0

  b0 = coef(nc)
  b1 = 0.0D+00
  b2 = 0.0D+00

  x2 = 4.0D+00 * x * x - 2.0D+00

  do i = nc - 1, 1, -1

    b2 = b1
    b1 = b0
    b0 = coef(i) - b2 + x2 * b1

  end do

  y0 = 0.5D+00 * ( b0 - b2 )

  return
end
subroutine evenchebser1 ( x, coef, nc, y0, y1 )

!*****************************************************************************80
!
!! EVENCHEBSER1 evaluates an even Chebyshev series and first derivative.
!
!  Discussion:
!
!    This function implements a modification and extension of 
!    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
!    gives an example for treating the first derivative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 April 2014
!
!  Author:
!
!    Manfred Zimmer
!
!  Reference:
!
!    Charles Clenshaw,
!    Mathematical Tables, Volume 5,
!    Chebyshev series for mathematical functions,
!    London, 1962.
!
!    Gerhard Maess,
!    Vorlesungen ueber Numerische Mathematik II, Analysis,
!    Berlin, Akademie_Verlag, 1984-1988,
!    ISBN: 978-3764318840,
!    LC: QA297.M325.  
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!    -1 <= X <= +1.
!
!    Input, real ( kind = 8 ) COEF(NC), the Chebyshev series.
!
!    Input, integer ( kind = 4 ) NC, the number of terms in the series.
!    0 < NC.
!
!    Output, real ( kind = 8 ) Y0, the value of the Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y1, the value of the 1st derivative of the
!    Chebyshev series at X.
!
  implicit none

  integer ( kind = 4 ) nc

  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) c0
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) coef(nc)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1

  b0 = coef(nc)
  b1 = 0.0D+00
  b2 = 0.0D+00
  c0 = coef(nc)
  c1 = 0.0D+00
  c2 = 0.0D+00

  x2 = 4.0D+00 * x * x - 2.0D+00

  do i = nc - 1, 1, -1

    b2 = b1
    b1 = b0
    b0 = coef(i) - b2 + x2 * b1

    if ( 1 < i ) then
      c2 = c1
      c1 = c0
      c0 = b0 - c2 + x2 * c1
    end if

  end do

  y0 = 0.5D+00 * ( b0 - b2 )
  y1 = ( c0 - c2 ) * 4.0D+00 * x

  return
end
subroutine evenchebser2 ( x, coef, nc, y0, y1, y2 )

!*****************************************************************************80
!
!! EVENCHEBSER2 evaluates an even Chebyshev series and first two derivatives.
!
!  Discussion:
!
!    This function implements a modification and extension of
!    Maess's algorithm.  Table 6.5.1 on page 164 of the reference
!    gives an example for treating the first derivative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2014
!
!  Author:
!
!    Manfred Zimmer
!
!  Reference:
!
!    Charles Clenshaw,
!    Mathematical Tables, Volume 5,
!    Chebyshev series for mathematical functions,
!    London, 1962.
!
!    Gerhard Maess,
!    Vorlesungen ueber Numerische Mathematik II, Analysis,
!    Berlin, Akademie_Verlag, 1984-1988,
!    ISBN: 978-3764318840,
!    LC: QA297.M325.  
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!    -1 <= X <= +1.
!
!    Input, real ( kind = 8 ) COEF(NC), the Chebyshev series.
!
!    Input, integer ( kind = 4 ) NC, the number of terms in the series.
!    0 < NC.
!
!    Output, real ( kind = 8 ) Y0, the value of the Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y1, the value of the 1st derivative of the
!    Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y2, the value of the 2nd derivative of the
!    Chebyshev series at X.
!
  implicit none

  integer ( kind = 4 ) nc

  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) c0
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) coef(nc)
  real ( kind = 8 ) d0
  real ( kind = 8 ) d1
  real ( kind = 8 ) d2
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2

  b0 = coef(nc)
  b1 = 0.0D+00
  b2 = 0.0D+00
  c0 = coef(nc)
  c1 = 0.0D+00
  c2 = 0.0D+00
  d0 = coef(nc)
  d1 = 0.0D+00
  d2 = 0.0D+00

  x2 = 4.0D+00 * x * x - 2.0D+00

  do i = nc - 1, 1, -1

    b2 = b1
    b1 = b0
    b0 = coef(i) - b2 + x2 * b1

    if ( 1 < i ) then
      c2 = c1
      c1 = c0
      c0 = b0 - c2 + x2 * c1
    end if

    if ( 2 < i ) then
      d2 = d1
      d1 = d0
      d0 = c0 - d2 + x2 * d1
    end if

  end do

  y0 = 0.5D+00 * ( b0 - b2 )
  y1 = ( c0 - c2 ) * 4.0D+00 * x
  y2 = ( d0 - d2 ) * 64.0D+00 * x * x + ( c0 - c2 ) * 4.0D+00

  return
end
subroutine oddchebser0 ( x, coef, nc, y0 )

!*****************************************************************************80
!
!! ODDCHEBSER0 evaluates an odd Chebyshev series.
!
!  Discussion:
!
!    This function implements Clenshaw's modification of  his algorithm
!    for odd series.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 April 2014
!
!  Author:
!
!    Manfred Zimmer
!
!  Reference:
!
!    Charles Clenshaw,
!    Mathematical Tables, Volume 5,
!    Chebyshev series for mathematical functions,
!    London, 1962.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!    -1 <= X <= +1.
!
!    Input, real ( kind = 8 ) COEF(NC), the Chebyshev series.
!
!    Input, integer ( kind = 4 ) NC, the number of terms in the series.
!    0 < NC.
!
!    Output, real ( kind = 8 ) Y0, the value of the Chebyshev series at X.
!
  implicit none

  integer ( kind = 4 ) nc

  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) coef(nc)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) y0

  b0 = coef(nc)
  b1 = 0.0D+00
  b2 = 0.0D+00

  x2 = 4.0D+00 * x * x - 2.0D+00

  do i = nc - 1, 1, -1

    b2 = b1
    b1 = b0
    b0 = coef(i) - b2 + x2 * b1

  end do

  y0 = ( b0 - b1 ) * x

  return
end
subroutine oddchebser1 ( x, coef, nc, y0, y1 )

!*****************************************************************************80
!
!! ODDCHEBSER1 evaluates an odd Chebyshev series and the first derivative.
!
!  Discussion:
!
!    This function implements a modification and extension of
!    Clenshaw's algorithm. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2014
!
!  Author:
!
!    Manfred Zimmer
!
!  Reference:
!
!    Charles Clenshaw,
!    Mathematical Tables, Volume 5,
!    Chebyshev series for mathematical functions,
!    London, 1962.
!
!    Gerhard Maess,
!    Vorlesungen ueber Numerische Mathematik II, Analysis,
!    Berlin, Akademie_Verlag, 1984-1988,
!    ISBN: 978-3764318840,
!    LC: QA297.M325.  
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!    -1 <= X <= +1.
!
!    Input, real ( kind = 8 ) COEF(NC), the Chebyshev series.
!
!    Input, integer ( kind = 4 ) NC, the number of terms in the series.
!    0 < NC.
!
!    Output, real ( kind = 8 ) Y0, the value of the Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y1, the value of the 1st derivative of the
!    Chebyshev series at X.
!
  implicit none

  integer ( kind = 4 ) nc

  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) c0
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) coef(nc)
  real ( kind = 8 ) coefi
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1

  coefi = 2.0D+00 * coef(nc)
  b0 = coefi
  b1 = 0.0D+00
  b2 = 0.0D+00
  c0 = coefi
  c1 = 0.0D+00
  c2 = 0.0D+00

  x2 = 4.0D+00 * x * x - 2.0D+00

  do i = nc - 1, 1, -1

    b2 = b1
    b1 = b0
    coefi = 2.0D+00 * coef(i) - coefi
    b0 = coefi - b2 + x2 * b1

    if ( 1 < i ) then
      c2 = c1
      c1 = c0
      c0 = b0 - c2 + x2 * c1
    end if

  end do

  y0 = ( b0 - b2 ) * 0.5D+00 * x
  y1 = ( c0 - c2 ) * 4.0D+00 * x * x + ( b0 - b2 ) * 0.5D+00

  return
end
subroutine oddchebser2 ( x, coef, nc, y0, y1, y2 )

!*****************************************************************************80
!
!! ODDCHEBSER2 evaluates an odd Chebyshev series and first two derivatives.
!
!  Discussion:
!
!    This function implements a modification and extension of
!    Clenshaw's algorithm.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2014
!
!  Author:
!
!    Manfred Zimmer
!
!  Reference:
!
!    Charles Clenshaw,
!    Mathematical Tables, Volume 5,
!    Chebyshev series for mathematical functions,
!    London, 1962.
!
!    Gerhard Maess,
!    Vorlesungen ueber Numerische Mathematik II, Analysis,
!    Berlin, Akademie_Verlag, 1984-1988,
!    ISBN: 978-3764318840,
!    LC: QA297.M325.  
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!    -1 <= X <= +1.
!
!    Input, real ( kind = 8 ) COEF(NC), the Chebyshev series.
!
!    Input, integer ( kind = 4 ) NC, the number of terms in the series.
!    0 < NC.
!
!    Output, real ( kind = 8 ) Y0, the value of the Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y1, the value of the 1st derivative of the
!    Chebyshev series at X.
!
!    Output, real ( kind = 8 ) Y2, the value of the 2nd derivative of the
!    Chebyshev series at X.
!
  implicit none

  integer ( kind = 4 ) nc

  real ( kind = 8 ) b0
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  real ( kind = 8 ) c0
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) coef(nc)
  real ( kind = 8 ) coefi
  real ( kind = 8 ) d0
  real ( kind = 8 ) d1
  real ( kind = 8 ) d2
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2

  coefi = 2.0D+00 * coef(nc)
  b0 = coefi
  b1 = 0.0D+00
  b2 = 0.0D+00
  c0 = coefi
  c1 = 0.0D+00
  c2 = 0.0D+00
  d0 = coefi
  d1 = 0.0D+00
  d2 = 0.0D+00

  x2 = 4.0D+00 * x * x - 2.0D+00

  do i = nc - 1, 1, -1

    b2 = b1
    b1 = b0
    coefi = 2.0D+00 * coef(i) - coefi
    b0 = coefi - b2 + x2 * b1

    if ( 1 < i ) then
      c2 = c1
      c1 = c0
      c0 = b0 - c2 + x2 * c1
    end if

    if ( 2 < i ) then
      d2 = d1
      d1 = d0
      d0 = c0 - d2 + x2 * d1
    end if

  end do

  x2 = x * x

  y0 = ( b0 - b2 ) * 0.5D+00 * x
  y1 = ( c0 - c2 ) * 4.0D+00 * x2 + ( b0 - b2 ) * 0.5D+00
  y2 = ( ( d0 - d2 ) * 64.0D+00 * x2 + ( c0 - c2 ) * 12.0D+00 ) * x

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
