subroutine fx1 ( n, x, f )

!*****************************************************************************80
!
!! FX1 is the 1D example #1.
!
!  Discussion:
!
!    This is example 3.1 in the reference.
!
!    The function should be plotted over [-1.0,+1.0].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Polynomial fitting for edge detection in irregularly sampled signals 
!    and images,
!    SIAM Journal on Numerical Analysis,
!    Volume 43, Number 1, 2006, pages 259-279.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
!  Local parameters:
!
!    Local, real STEEP, controls the steepness of the slope.
!    The default value is a moderate 5.  For a sharp rise, use 25 instead.  
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) steep
  real ( kind = 8 ) x(n)

  steep = 5.0D+00

  do i = 1, n

    if ( x(i) < 0.0D+00 ) then
      f(i) = cos ( 3.0D+00 * r8_pi * x(i) );
    else if ( 0.0D+00 <= x(i) ) then
      f(i) = - 1.0D+00 + 2.0D+00 / ( 1.0D+00 + 3.0D+00 &
        * exp ( - steep * ( 2.0D+00 * x(i) - 1.0D+00 ) ) )
    end if

  end do

  return
end
subroutine fx2 ( n, x, f )

!*****************************************************************************80
!
!! FX2 is the 1D example #2.
!
!  Discussion:
!
!    The function should be plotted over [-1,+1].
!
!    The "internal" coordinate range will be [-2.0,6.0*pi].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Polynomial fitting for edge detection in irregularly sampled signals 
!    and images,
!    SIAM Journal on Numerical Analysis,
!    Volume 43, Number 1, 2006, pages 259-279.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x2
!
!  Map from the convenient range [-1,+1] to the physical range [-2,6pi].
!
  do i = 1, n

    x2 = ( ( 1.0D+00 - x(i) ) * ( - 2.0D+00 )  &
         + ( 1.0D+00 + x(i) ) * 6.0D+00 * r8_pi ) &
         /   2.0D+00

    if ( x2 < 0.0D+00 ) then
      f(i) = exp ( x2 )
    else if ( 0.0D+00 <= x2 .and. x2 < 3.0D+00 * r8_pi / 2.0D+00 ) then
      f(i) = - exp ( - x2 )
    else if ( 3.0D+00 * r8_pi / 2.0D+00 <= x2 ) then
      f(i) = -1.5D+00 * sin ( x2 )
    end if

  end do

  return
end
subroutine fx3 ( n, x, f )

!*****************************************************************************80
!
!! FX3 is the 1D example #3.
!
!  Discussion:
!
!    The function should be plotted over [-1.0,+1.0].
!
!    Internally, this range is mapped to [-3.0,+3.0].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Polynomial fitting for edge detection in irregularly sampled signals 
!    and images,
!    SIAM Journal on Numerical Analysis,
!    Volume 43, Number 1, 2006, pages 259-279.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x2
!
!  Map from the convenient range [-1,+1] to the physical range [-3,+3].
!
  do i = 1, n

    x2 = ( ( 1.0D+00 - x(i) ) * ( -3.0D+00 )   &
         + ( 1.0D+00 + x(i) ) * ( +3.0D+00 ) ) &
         /   2.0D+00

    if ( -2.0D+00 <= x2 .and. x2 <= -1.0D+00 ) then
      f(i) = 1.0D+00
    else if ( -0.5D+00 <= x2 .and. x2 <= 0.5D+00 ) then
      f(i) = 0.5D+00 + 4.0D+00 * ( x2 + 0.5D+00 ) ** 2
    else if ( 1.25D+00 <= x2 .and. 3.0D+00 * x2 <= 7.0D+00 ) then
      f(i) = 3.0D+00 * ( 2.0D+00 - x2 )
    else
      f(i) = 0.0D+00
    end if

  end do

  return
end
subroutine fx4 ( n, x, f )

!*****************************************************************************80
!
!! FX4 is the 1D example #4.
!
!  Discussion:
!
!    The function should be plotted over [0.0,+1.0].
!
!    The function is continuous, but the derivative has a discontinuity at 0.5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Polynomial fitting for edge detection in irregularly sampled signals 
!    and images,
!    SIAM Journal on Numerical Analysis,
!    Volume 43, Number 1, 2006, pages 259-279.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x2
!
!  Convert from -1 <= x <= 1 to 0 <= x <= 1:
!
  do i = 1, n

    x2 = ( x(i) + 1.0D+00 ) / 2.0D+00

    if ( x2 <= 0.5D+00 ) then
      f(i) = - ( x2 - 0.5D+00 ) + sin ( 4.0D+00 * r8_pi * x2 ) / 6.0D+00
    else if ( 0.5D+00 < x2 ) then
      f(i) =   ( x2 - 0.5D+00 ) + sin ( 4.0D+00 * r8_pi * x2 ) / 6.0D+00
    end if

  end do

  return
end
subroutine fx5 ( n, x, f )

!*****************************************************************************80
!
!! FX5 is the 1D example #5.
!
!  Discussion:
!
!    The function should be plotted over [-1.0,+1.0].
!
!    The function actually has no discontinuities, but does have a
!    steep rise.  The local parameter S controls the steepness of the rise.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Polynomial fitting for edge detection in irregularly sampled signals 
!    and images,
!    SIAM Journal on Numerical Analysis,
!    Volume 43, Number 1, 2006, pages 259-279.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) steep
  real ( kind = 8 ) x(n)

  steep = 20.0D+00

  f(1:n) = tanh ( steep * x(1:n) )

  return
end
subroutine fx6 ( n, x, f )

!*****************************************************************************80
!
!! FX6 is the 1D example #6.
!
!  Discussion:
!
!    This is example 2.1 in the reference.
!
!    The function should be plotted over [0.0,+1.0].
!
!    The function has a discontinuous first derivative at 1/2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Determining the location of discontinuities in the derivatives
!    of functions,
!    Applied Numerical Mathematics,
!    Volume 58, 2008, pages 577-592.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)

  f(1:n) = sin ( 2.0D+00 * r8_pi * x(1:n) ) / 6.0D+00

  do i = 1, n
    if ( x(i) < 0.5D+00 ) then
      f(i) = f(i) - ( x(i) - 0.5D+00 )
    else
      f(i) = f(i) + ( x(i) - 0.5D+00 )
    end if
  end do

  return
end
subroutine fx7 ( n, x, f )

!*****************************************************************************80
!
!! FX7 is the 1D example #7.
!
!  Discussion:
!
!    This is example 2.1 in the reference.
!
!    The function should be plotted over [0.0,+1.0].
!
!    The function has a discontinuous second derivative at 1/2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Determining the location of discontinuities in the derivatives
!    of functions,
!    Applied Numerical Mathematics,
!    Volume 58, 2008, pages 577-592.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)

  f(1:n) = sin ( 2.0D+00 * r8_pi * x(1:n) ) / 6.0D+00

  do i = 1, n
    if ( x(i) < 0.5D+00 ) then
      f(i) = f(i) - 0.5D+00 * ( x(i) - 0.5D+00 ) ** 2
    else
      f(i) = f(i) + 0.5D+00 * ( x(i) - 0.5D+00 ) ** 2
    end if
  end do

  return
end
subroutine fxy1 ( n, x, y, f )

!*****************************************************************************80
!
!! FXY1 is the 2D example #1.
!
!  Discussion:
!
!    This is example 4.1 in the reference.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Polynomial fitting for edge detection in irregularly sampled signals 
!    and images,
!    SIAM Journal on Numerical Analysis,
!    Volume 43, Number 1, 2006, pages 259-279.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1,  n

    f(i) = x(i) * y(i) + cos ( 2.0D+00 * r8_pi * x(i) ** 2 ) &
      - sin ( 2.0D+00 * r8_pi * x(i) ** 2 )

    if ( 0.25D+00 < x(i) ** 2 + y(i) ** 2 ) then
      f(i) = f(i) + 10.0D+00 * x(i) - 5.0D+00
    end if

  end do

  return
end
subroutine fxy2 ( n, x, y, f )

!*****************************************************************************80
!
!! FXY2 is the 2D example #2.
!
!  Discussion:
!
!    This is example 4.2 in the reference.
!
!    It is known as the Shepp-Logan phantom.
!
!    It should be plotted on [-1,+1] x [-1,+1].
!
!    Note that the Archibald reference incorrectly describes the divisor
!    of x in the second ellipse as 0.06624, when it should be 0.6624.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Polynomial fitting for edge detection in irregularly sampled signals 
!    and images,
!    SIAM Journal on Numerical Analysis,
!    Volume 43, Number 1, 2006, pages 259-279.
!
!    Larry Shepp, Ben Logan,
!    The Fourier reconstruction of a head section,
!    IEEE Transactions on Nuclear Science,
!    Volume  NS-21, June 1974, pages 21-43.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
!  Local parameters:
!
!    Local, integer CHOICE:
!    1, use Archibald's (and Shepp and Logan's) level values;
!    2, use Matlab's level values;
!    3, use Matlab's enhanced contrast level values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) c(4)
  real ( kind = 8 ), dimension ( 4 ) :: c1 = (/ &
    2.0D+00, -0.98D+00, -0.02D+00, +0.01D+00 /) 
  real ( kind = 8 ), dimension ( 4 ) :: c2 = (/ &
    1.0D+00, -0.98D+00, -0.02D+00, +0.01D+00 /) 
  real ( kind = 8 ), dimension ( 4 ) :: c3 = (/ &
    1.0D+00, -0.8D+00, -0.2D+00, +0.1D+00 /) 
  integer ( kind = 4 ) choice
  real ( kind = 8 ) eta1
  real ( kind = 8 ) eta2
  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xi1
  real ( kind = 8 ) xi2
  real ( kind = 8 ) y(n)

  choice = 3

  if ( choice == 1 ) then
    call r8vec_copy ( 4, c1, c )
  else if ( choice == 2 ) then
    call r8vec_copy ( 4, c2, c )
  else
    call r8vec_copy ( 4, c3, c )
  end if

  do i = 1, n

    f(i) = 0.0D+00

    xi1  =   ( x(i) - 0.22D+00 ) * cos ( 0.4D+00 * r8_pi ) &
             + y(i)              * sin ( 0.4D+00 * r8_pi )
    eta1 = - ( x(i) - 0.22D+00 ) * sin ( 0.4D+00 * r8_pi ) &
             + y(i)              * cos ( 0.4D+00 * r8_pi )

    xi2  =   ( x(i) + 0.22D+00 ) * cos ( 0.6D+00 * r8_pi ) &
             + y(i)              * sin ( 0.6D+00 * r8_pi )
    eta2 = - ( x(i) + 0.22D+00 ) * sin ( 0.6D+00 * r8_pi ) &
             + y(i)              * cos ( 0.6D+00 * r8_pi )

    if ( ( x(i) / 0.69D+00 )**2 + ( y(i) / 0.92D+00 )**2 <= 1.0D+00 ) then
      f(i) = f(i) + c(1)
    end if

    if ( ( x(i) / 0.6624D+00 )**2 &
       + ( ( y(i) + 0.0184D+00 ) / 0.874D+00 )**2 <= 1.0D+00 ) then
      f(i) = f(i) + c(2)
    end if

    if ( ( xi1 / 0.31D+00 )**2 + ( eta1 / 0.11D+00 )**2 <= 1.0D+00 .or. &
         ( xi2 / 0.41D+00 )**2 + ( eta2 / 0.16D+00 )**2 <= 1.0D+00 ) then
      f(i) = f(i) + c(3)
    end if

    if ( ( ( ( x(i) - 0.35D+00 )  / 0.3D+00   )**2 &
         + (   y(i)               / 0.6D+00   )**2 <= 1.0D+00 ) .or. &
         ( (   x(i)               / 0.21D+00  )**2 &
         + ( ( y(i) - 0.35D+00  ) / 0.25D+00  )**2 <= 1.0D+00 ) .or. &
         ( (   x(i)               / 0.046D+00 )**2 &
         + ( ( y(i) - 0.1D+00   ) / 0.046D+00 )**2 <= 1.0D+00 ) .or. &
         ( (   x(i)               / 0.046D+00 )**2 &
         + ( ( y(i) + 0.1D+00   ) / 0.046D+00 )**2 <= 1.0D+00 ) .or. &
         ( ( ( x(i) + 0.08D+00  ) / 0.046D+00 )**2 &
         + ( ( y(i) + 0.605D+00 ) / 0.023D+00 )**2 <= 1.0D+00 ) .or. &
         ( (   x(i)               / 0.023D+00 )**2 &
         + ( ( y(i) + 0.605D+00 ) / 0.023D+00 )**2 <= 1.0D+00 ) .or. &
         ( ( ( x(i) - 0.06D+00  ) / 0.023D+00 )**2 &
         + ( ( y(i) + 0.605D+00 ) / 0.023D+00 )**2 <= 1.0D+00 ) ) then
      f(i) = f(i) + c(4)
    end if

  end do

  return
end
subroutine fxy3 ( n, x, y, f )

!*****************************************************************************80
!
!! FXY3 is the 2D example #3.
!
!  Discussion:
!
!    This is example 3.2 in the reference.
!
!    It is known as the modified two-dimensional Harten function.
!
!    It should be plotted on [-1,+1] x [-1,+1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Determining the location of discontinuities in the derivatives
!    of functions,
!    Applied Numerical Mathematics,
!    Volume 58, 2008, pages 577-592.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r(n)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  r(1:n) = ( 4.0 * x(1:n) ** 2 + 4.0 * y(1:n) ** 2 - 1.0D+00 ) / 6.0D+00

  do i = 1, n

    if ( 3.0D+00 * r(i) <= -1.0D+00 ) then
      f(i) = - r(i) * sin ( 0.5D+00 * r8_pi * r(i) ** 2 )
    else if ( 3.0D+00 * r(i) < 1.0D+00 ) then
      f(i) = abs ( sin ( 2.0D+00 * r8_pi * r(i) ) )
    else
      f(i) = 2.0D+00 * r(i) - 1.0D+00 - sin ( 3.0D+00 * r8_pi * r(i) ) / 6.0D+00
    end if

  end do

  return
end
subroutine fxy4 ( n, x, y, f )

!*****************************************************************************80
!
!! FXY4 is the 2D example #4.
!
!  Discussion:
!
!    This is example 3.1 in the reference.
!
!    It is known as the discontinuous medium wave function.
!
!    Here, we are computing the first component of the solution, P(X,Y).
!
!    It should be plotted on (x,y) in [-1,0]x[0,0.1].
!
!    The second variable y actually represents time.
!
!    Note that in the reference, the formula reads:
!     f(i) = 2.0D+00 * rhor * cr / ( rhol * cl + rhor * cr ) 
!          * sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cr ) )
!    but I believe this should be:
!     f(i) = 2.0D+00 * rhor * cr / ( rhol * cl + rhor * cr ) 
!          * sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cl ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Determining the location of discontinuities in the derivatives
!    of functions,
!    Applied Numerical Mathematics,
!    Volume 58, 2008, pages 577-592.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ), parameter :: cl = 0.87879D+00
  real ( kind = 8 ), parameter :: cr = 1.0D+00
  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: omega = 12.0D+00
  real ( kind = 8 ), parameter :: rhol = 0.55556D+00
  real ( kind = 8 ), parameter :: rhor = 1.0D+00
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1, n
    if ( x(i) <= -0.5D+00 ) then
      f(i) = sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cl ) ) &
           - ( rhol * cl - rhor * cr ) / ( rhol * cl + rhor * cr )      &
           * sin ( r8_pi * omega * ( y(i) + ( x(i) + 0.5D+00 ) / cl ) )
    else
      f(i) = 2.0D+00 * rhor * cr / ( rhol * cl + rhor * cr ) &
           * sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cl ) )
    end if
  end do

  return
end
subroutine fxy5 ( n, x, y, f )

!*****************************************************************************80
!
!! FXY5 is the 2D example #5.
!
!  Discussion:
!
!    This is example 3.1 in the reference.
!
!    It is known as the discontinuous medium wave function.
!
!    Here, we are computing the second component of the solution, U(X,Y).
!
!    It should be plotted on (x,y) in [-1,0]x[0,0.1].
!
!    The second variable y actually represents time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Determining the location of discontinuities in the derivatives
!    of functions,
!    Applied Numerical Mathematics,
!    Volume 58, 2008, pages 577-592.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ), parameter :: cl = 0.87879D+00
  real ( kind = 8 ), parameter :: cr = 1.0D+00
  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: omega = 12.0D+00
  real ( kind = 8 ), parameter :: rhol = 0.55556D+00
  real ( kind = 8 ), parameter :: rhor = 1.0D+00
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1, n
    if ( x(i) <= -0.5D+00 ) then
      f(i) = sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cl ) ) &
           + ( rhol * cl - rhor * cr ) / ( rhol * cl + rhor * cr ) &
           / ( rhol * cl ) &
           * sin ( r8_pi * omega * ( y(i) + ( x(i) + 0.5D+00 ) / cl ) )
    else
      f(i) = 2.0D+00 / ( rhol * cl + rhor * cr ) &
           * sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cl ) )
    end if
  end do

  return
end
subroutine fxyz1 ( n, x, y, z, f )

!*****************************************************************************80
!
!! FXYZ1 is the 3D example #1.
!
!  Discussion:
!
!    This example is known as the 3D Shepp-Logan phantom.
!
!    It should be plotted on [-1,+1] x [-1,+1.5] x [-1.5,+1.5].
!
!    Seventeen objects are modeled by ellipses of various gray levels,
!    including:
!
!     1: Outer skull
!     2: Inner skull
!     3: Left eye
!     4: Right eye
!     5: Nose
!     6: Mouth
!     7: Left ear
!     8: Right ear
!     9: Left small tumor
!    10: Center small tumor
!    11: Right small tumor
!    12: Old f
!    13: Old g
!    14: Old e
!    15: Right ventricle
!    16: Left ventricle
!    17: Blood clot
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Larry Shepp,
!    Computerized tomography and nuclear magnetic resonance,
!    Journal of Computer Assisted Tomography,
!    Volume 4, Number 1, February 1980, pages 94-107.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), Z(N), the arguments.
!
!    Output, real ( kind = 8 ) F(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ), dimension ( 17 ) :: a1 = (/ &
          0.7233,  0.7008,  0.1270,  0.1270,  0.1270, &
          0.4575,  0.0635,  0.0635,  0.0460,  0.0230, &
          0.0230,  0.0460,  0.2100,  0.1100,  0.1600, &
          0.1600,  0.0300 /)
  real ( kind = 8 ), dimension ( 17 ) :: a2 = (/ &
          0.9644,  0.9246,  0.1270,  0.1270,  0.3400, &
          0.6099,  0.3175,  0.3175,  0.0230,  0.0230, &
          0.0460,  0.0460,  0.2581,  0.2500,  0.3100, &
          0.4100,  0.2000 /)
  real ( kind = 8 ), dimension ( 17 ) :: a3 = (/ &
          1.2700,  1.2241,  0.1270,  0.1270,  0.1700, &
          0.5080,  0.3175,  0.3175,  0.0230,  0.0460, &
          0.0230,  0.0460,  0.2581,  0.2300,  0.2540, &
          0.3810,  0.2000 /)
  real ( kind = 8 ) c
  real ( kind = 8 ) f(n)
  real ( kind = 8 ), dimension ( 17 ) :: g = (/ &
          2.0000, -0.9800, -1.0000, -1.0000,  1.5000, &
         -1.0000,  1.0000,  1.0000,  0.0100,  0.0100, &
          0.0100,  0.0100,  0.0100,  0.0100, -0.0200, &
         -0.0200,  0.0300 /)
  integer ( kind = 4 ) e
  integer ( kind = 4 ) i
  real ( kind = 8 ), dimension ( 17 ) :: v11 = (/ &
          1.0000,  1.0000,  1.0000,  1.0000,  1.0000, &
          1.0000,  0.9903, -0.9903,  1.0000,  1.0000, &
          1.0000,  1.0000,  1.0000,  1.0000,  0.9511, &
         -0.9511,  0.9192 /)
  real ( kind = 8 ), dimension ( 17 ) :: v12 = (/ &
          0.0000,  0.0000,  0.0000,  0.0000,  0.0000, &
          0.0000, -0.1085, -0.1085,  0.0000,  0.0000, &
          0.0000,  0.0000,  0.0000,  0.0000, -0.3090, &
         -0.3090, -0.3381 /)
  real ( kind = 8 ), dimension ( 17 ) :: v13 = (/ &
          0.0000,  0.0000,  0.0000,  0.0000,  0.0000, &
          0.0000, -0.0865, -0.0865,  0.0000,  0.0000, &
          0.0000,  0.0000,  0.0000,  0.0000,  0.0000, &
          0.0000,  0.2020 /)
  real ( kind = 8 ), dimension ( 17 ) :: v21 = (/ &
          0.0000,  0.0000,  0.0000,  0.0000,  0.0000, &
          0.0000,  0.1089, -0.1089,  0.0000,  0.0000, &
          0.0000,  0.0000,  0.0000,  0.0000,  0.3090, &
         -0.3090,  0.3452 /)
  real ( kind = 8 ), dimension ( 17 ) :: v22 = (/ &
          1.0000,  1.0000,  1.0000,  1.0000,  0.5446, &
          1.0000,  0.9941,  0.9941,  1.0000,  1.0000, &
          1.0000,  1.0000,  1.0000,  1.0000,  0.9511, &
          0.9511,  0.9385 /)
  real ( kind = 8 ), dimension ( 17 ) :: v23 = (/ &
          0.0000,  0.0000,  0.0000,  0.0000, -0.8387, &
          0.0000,  0.0000,  0.0000,  0.0000,  0.0000, &
          0.0000,  0.0000,  0.0000,  0.0000,  0.0000, &
          0.0000,  0.0000 /)
  real ( kind = 8 ), dimension ( 17 ) :: v31 = (/ &
          0.0000,  0.0000,  0.0000,  0.0000,  0.0000, &
          0.0000,  0.0860, -0.0860,  0.0000,  0.0000, &
          0.0000,  0.0000,  0.0000,  0.0000,  0.0000, &
          0.0000,  0.1896 /)
  real ( kind = 8 ), dimension ( 17 ) :: v32 = (/ &
          0.0000,  0.0000,  0.0000,  0.0000,  0.8387, &
          0.0000, -0.0094, -0.0094,  0.0000,  0.0000, &
          0.0000,  0.0000,  0.0000,  0.0000,  0.0000, &
          0.0000, -0.0697 /)
  real ( kind = 8 ), dimension ( 17 ) :: v33 = (/ &
          1.0000,  1.0000,  1.0000,  1.0000,  0.5446, &
          1.0000,  0.9963,  0.9963,  1.0000,  1.0000, &
          1.0000,  1.0000,  1.0000,  1.0000,  1.0000, &
          1.0000, -0.9794 /)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)
  real ( kind = 8 ), dimension ( 17 ) :: x0 = (/ &
          0.0000,  0.0000,  0.2583, -0.2583,  0.0000, &
          0.0000,  0.7076, -0.7076, -0.0800,  0.0000, &
          0.0600,  0.0000,  0.0000,  0.0000,  0.2200, &
         -0.2200,  0.5600 /)
  real ( kind = 8 ), dimension ( 17 ) :: y0 = (/ &
          0.0000, -0.0184,  0.7534,  0.7534,  1.1398, &
          0.0000, -0.1378, -0.1378, -0.6050, -0.6050, &
         -0.6050,  0.1000, -0.1000,  0.3500,  0.0000, &
          0.0000, -0.4000 /)
  real ( kind = 8 ), dimension ( 17 ) :: z0 = (/ &
          0.0000, -0.0185,  0.0000,  0.0000, -0.1957, &
         -0.7620, -0.1905, -0.1905,  0.3810,  0.3810, &
          0.3810,  0.3810,  0.1270,  0.3810,  0.3810, &
          0.3810,  0.3810 /)

  do i = 1, n

    f(i) = 0.0D+00

    do e = 1, 17

      c = ( ( ( x(i) - x0(e) ) * v11(e) &
            + ( y(i) - y0(e) ) * v12(e) &
            + ( z(i) - z0(e) ) * v13(e) ) / a1(e) )**2 &
        + ( ( ( x(i) - x0(e) ) * v21(e) &
            + ( y(i) - y0(e) ) * v22(e) &
            + ( z(i) - z0(e) ) * v23(e) ) / a2(e) )**2 &
        + ( ( ( x(i) - x0(e) ) * v31(e) &
            + ( y(i) - y0(e) ) * v32(e) &
            + ( z(i) - z0(e) ) * v33(e) ) / a3(e) )**2

      if ( c <= 1.0D+00 ) then
        f(i) = f(i) + g(e)
      end if

    end do

  end do

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
subroutine r8vec_copy ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_COPY copies an R8VEC.
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
!    17 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the vectors.
!
!    Input, real ( kind = 8 ) A1(N), the vector to be copied.
!
!    Output, real ( kind = 8 ) A2(N), a copy of A1.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)

  a2(1:n) = a1(1:n)

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
!    Input, real ( kind = 8 ) A_FIRST, A_LAST, the first and last entries.
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
subroutine r8vec_uniform_ab ( n, a, b, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_AB returns a scaled pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Each dimension ranges from A to B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 May 2007
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
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

  end do

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
