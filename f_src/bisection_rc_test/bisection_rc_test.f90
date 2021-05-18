program main

!*****************************************************************************80
!
!! MAIN is the main program for BISECTION_RC_TEST.
!
!  Location:
!
!    http://people.sc.fsu.edu/~jburkardt/f_src/bisection_rc/bisection_rc_test.f90
!
!  Discussion:
!
!    BISECTION_RC_TEST tests the BISECTION_RC library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BISECTION_RC_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the BISECTION_RC library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BISECTION_RC_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests BISECTION_RC, evaluating the function in a separate routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) dx
  real ( kind = 8 ) dx_tol
  real ( kind = 8 ), external :: f01
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx_tol
  integer ( kind = 4 ) it
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) job
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Demonstrate BISECTION_RC on a simple example.'
  write ( *, '(a)' ) '  The function is evaluated in a separate routine.'

  fx_tol = 1.0D-08
  dx_tol = 1.0D-06
  it = 0
  it_max = 30
  job = 0
  a = 0.0D+00
  b = 1.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I      X               FX              DX'
  write ( *, '(a)' ) ' '

  do

    call bisection_rc ( a, b, x, fx, job )

    if ( job < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Error return.'
      exit
    end if

    it = it + 1

    fx = f01 ( x )

    if ( it <= 2 ) then
      dx = abs ( b - a )
    else
      dx = 0.5D+00 * abs ( b - a )
    end if

    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) it, x, fx, dx

    if ( abs ( fx ) <= fx_tol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Function is small.'
      exit
    end if

    if ( dx <= dx_tol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Interval is tiny.'
      exit
    end if

    if ( it_max <= it ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Reached iteration limit.'
      exit
    end if

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, ' F(A) = ', f01 ( a )
  write ( *, '(a,g14.6,a,g14.6)' ) '  X = ', x, ' F(X) = ', f01 ( x )
  write ( *, '(a,g14.6,a,g14.6)' ) '  B = ', b, ' F(B) = ', f01 ( b )

  return
end
function f01 ( x )

!*****************************************************************************80
!
!! F01 evaluates the function f(x) = cos ( x ) - x which is zero around 0.74
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F01, the function value.
!
  implicit none

  real ( kind = 8 ) f01
  real ( kind = 8 ) x

  f01 = cos ( x ) - x

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests BISECTION_RC, evaluating the function within the routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) dx
  real ( kind = 8 ) dx_tol
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx_tol
  integer ( kind = 4 ) it
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) job
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Demonstrate BISECTION_RC on a simple example.'
  write ( *, '(a)' ) '  The function is evaluated within this routine.'

  fx_tol = 1.0D-09
  dx_tol = 1.0D-09
  it = 0
  it_max = 30
  job = 0
  a = 0.0D+00
  b = 1.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I      X               FX              DX'
  write ( *, '(a)' ) ' '

  do

    call bisection_rc ( a, b, x, fx, job )

    if ( job < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Error return.'
      exit
    end if

    it = it + 1

    fx = cos ( 100.0D+00 * x ) - 4.0D+00 * erf ( 30.0D+00 * x - 10.0D+00 )

    if ( it <= 2 ) then
      dx = abs ( b - a )
    else
      dx = 0.5 * abs ( b - a )
    end if

    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) it, x, fx, dx

    if ( abs ( fx ) <= fx_tol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Function is small.'
      exit
    end if

    if ( dx <= dx_tol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Interval is tiny.'
      exit
    end if

    if ( it_max <= it ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Reached iteration limit.'
      exit
    end if

  end do

  write ( *, '(a)' ) ' '
  fx = cos ( 100.0D+00 * a ) - 4.0D+00 * erf ( 30.0D+00 * a - 10.0D+00 )
  write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, ' F(A) = ', fx
  fx = cos ( 100.0D+00 * x ) - 4.0D+00 * erf ( 30.0D+00 * x - 10.0D+00 )
  write ( *, '(a,g14.6,a,g14.6)' ) '  X = ', x, ' F(X) = ', fx
  fx = cos ( 100.0D+00 * b ) - 4.0D+00 * erf ( 30.0D+00 * b - 10.0D+00 )
  write ( *, '(a,g14.6,a,g14.6)' ) '  B = ', b, ' F(B) = ', fx

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests BISECTION_RC, to invert the cardioid CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: alpha = 0.0D+00
  real ( kind = 8 ) b
  real ( kind = 8 ), parameter :: beta = 0.25D+00
  real ( kind = 8 ) cdf
  real ( kind = 8 ) dx
  real ( kind = 8 ) dx_tol
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx_tol
  integer ( kind = 4 ) it
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) job
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Demonstrate BISECTION_RC on a probability example.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The cardioid probability density function has a'
  write ( *, '(a)' ) '  cumulative density function of the form:'
  write ( *, '(a)' ) '    CDF(X) = ( pi + x - alpha + 2 beta * sin ( x - alpha ) ) / ( 2 * pi )'
  write ( *, '(a)' ) '  where alpha and beta are parameters, and x is a value'
  write ( *, '(a)' ) '  in the range -pi <= x <= +pi.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  CDF(X) is the probability that a random sample will have'
  write ( *, '(a)' ) '  a value less than or equal to X.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  As X moves from -pi to +pi,'
  write ( *, '(a)' ) '  the CDF rises from 0 (no probability)'
  write ( *, '(a)' ) '  to 1 (certain probability).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Assuming that:'
  write ( *, '(a,g14.6)' ) '  * ALPHA = ', alpha 
  write ( *, '(a,g14.6)' ) '  * BETA =  ', beta 
  write ( *, '(a)' ) '  determine the value X where the Cardioid CDF is exactly 0.75.'

  fx_tol = 1.0D-06
  dx_tol = 1.0D-08
  it = 0
  it_max = 30
  job = 0
  a = - r8_pi
  b = + r8_pi

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I      X               FX              DX'
  write ( *, '(a)' ) ' '

  do

    call bisection_rc ( a, b, x, fx, job )

    if ( job < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Error return.'
      exit
    end if

    it = it + 1

    cdf = ( r8_pi + x - alpha + 2.0D+00 * beta * sin ( x - alpha ) ) / ( 2.0D+00 * r8_pi )
    fx = cdf - 0.75D+00

    if ( it <= 2 ) then
      dx = abs ( b - a )
    else
      dx = 0.5 * abs ( b - a )
    end if

    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) it, x, fx, dx

    if ( abs ( fx ) <= fx_tol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Function is small.'
      exit
    end if

    if ( dx <= dx_tol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Interval is tiny.'
      exit
    end if

    if ( it_max <= it ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Reached iteration limit.'
      exit
    end if

  end do

  write ( *, '(a)' ) ' '
  cdf = ( r8_pi + a - alpha + 2.0D+00 * beta * sin ( a - alpha ) ) / ( 2.0D+00 * r8_pi )
  fx = cdf - 0.75D+00
  write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, ' F(A) = ', fx
  cdf = ( r8_pi + x - alpha + 2.0D+00 * beta * sin ( x - alpha ) ) / ( 2.0D+00 * r8_pi )
  fx = cdf - 0.75D+00
  write ( *, '(a,g14.6,a,g14.6)' ) '  X = ', x, ' F(X) = ', fx
  cdf = ( r8_pi + b - alpha + 2.0D+00 * beta * sin ( b - alpha ) ) / ( 2.0D+00 * r8_pi )
  fx = cdf - 0.75D+00
  write ( *, '(a,g14.6,a,g14.6)' ) '  B = ', b, ' F(B) = ', fx

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Look at the actual cardioid CDF value now:'
  write ( *, '(a)' ) ''
  cdf = ( r8_pi + x - alpha + 2.0D+00 * beta * sin ( x - alpha ) ) / ( 2.0D+00 * r8_pi )
  write ( *, '(a,g12.6,a,g14.6)' ) '  Cardioid(', x, ') = ', cdf

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests BISECTION_RC for the pipe freezing problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cleve Moler,
!    Numerical Computing with MATLAB,
!    SIAM, 2004,
!    ISBN13: 978-0-898716-60-3,
!    LC: QA297.M625,
!    ebook: http://www.mathworks.com/moler/chapters.html
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b
  real ( kind = 8 ) dx
  real ( kind = 8 ) dx_tol
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx_tol
  integer ( kind = 4 ) it
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) job
  real ( kind = 8 ) t
  real ( kind = 8 ) tc
  real ( kind = 8 ) ti
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BISECTION_RC_TEST04'
  write ( *, '(a)' ) '  The freezing pipe problem.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  At the beginning of a cold spell, the soil is at a uniform'
  write ( *, '(a)' ) '  temperature of Ti.  The cold spell applies a uniform air'
  write ( *, '(a)' ) '  temperature of Tc, which begins to cool the soil.'
  write ( *, '(a)' ) '  As a function of depth x and time t, the soil temperature'
  write ( *, '(a)' ) '  will now cool down as:'
  write ( *, '(a)' ) '    ( T(x,t) - Tc ) / ( Ti - Tc ) = erf ( 0.5 * x / sqrt ( alpha * t ) ).'
  write ( *, '(a)' ) '  where:'
  write ( *, '(a)' ) '    Ti =  20 degrees centigrade,'
  write ( *, '(a)' ) '    Tc = -15 degrees centigrade,'
  write ( *, '(a)' ) '    alpha = 0.000000138 meter^2 / second, thermal conductivity;'
  write ( *, '(a)' ) '    and erf() is the error function.'
  write ( *, '(a)' ) '  Water freezes at 0 degrees centigrade.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  What depth x in meters must a water pipe be buried so that it will'
  write ( *, '(a)' ) '  not freeze even if this cold snap lasts for 60 days?'
!
!  Problem parameters.
!
  ti = 20.0D+00
  tc = -15.0D+00
  t = 60.0D+00 * 24.0D+00 * 60.0D+00 * 60.0D+00
  alpha = 0.000000138D+00
!
!  Iteration parameters.
!
  fx_tol = 1.0D-09
  dx_tol = 1.0D-09
  it = 0
  it_max = 30
  job = 0
  fx = 0.0D+00
!
!  Initial guess for interval.
!
  a = 0.0D+00
  b = 1000.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I      X               FX              DX'
  write ( *, '(a)' ) ' '

  do

    call bisection_rc ( a, b, x, fx, job )

    if ( job < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Error return.'
      exit
    end if

    it = it + 1

    fx = tc + ( ti - tc ) * erf ( 0.5D+00 * x / sqrt ( alpha * t ) )

    if ( it <= 2 ) then
      dx = abs ( b - a )
    else
      dx = 0.5 * abs ( b - a )
    end if

    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) it, x, fx, dx

    if ( abs ( fx ) <= fx_tol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Function is small.'
      exit
    end if

    if ( dx <= dx_tol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Interval is tiny.'
      exit
    end if

    if ( it_max <= it ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Reached iteration limit.'
      exit
    end if

  end do

  write ( *, '(a)' ) ''
  fx = tc + ( ti - tc ) * erf ( 0.5D+00 * a / sqrt ( alpha * t ) )
  write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, ', F(A) = ', fx
  fx = tc + ( ti - tc ) * erf ( 0.5D+00 * x / sqrt ( alpha * t ) )
  write ( *, '(a,g14.6,a,g14.6)' ) '  X = ', x, ', F(X) = ', fx
  fx = tc + ( ti - tc ) * erf ( 0.5D+00 * b / sqrt ( alpha * t ) )
  write ( *, '(a,g14.6,a,g14.6)' ) '  B = ', b, ', F(B) = ', fx

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests BISECTION_RC for Kepler's problem.
!
!  Discussion:
!
!    Kepler's equation has the form:
!
!      X = M + E * sin ( X )
!
!    X represents the eccentric anomaly of a planet, the angle between the
!    perihelion (the point on the orbit nearest to the sun) through the sun 
!    to the center of the ellipse, and the line from the center of the ellipse
!    to the planet.
!
!    There are two parameters, E and M:
!
!    * E is the eccentricity of the orbit, which should be between 0 and 1.0;
!
!    * M is the angle from the perihelion made by a fictitious planet traveling
!      on a circular orbit centered at the sun, and traveling at a constant
!      angular velocity equal to the average angular velocity of the true
!      planet.  M is usually between 0 and 180 degrees, but can have any value.
!
!    For convenience, X and M are measured in degrees.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cleve Moler,
!    Numerical Computing with MATLAB,
!    SIAM, 2004,
!    ISBN13: 978-0-898716-60-3,
!    LC: QA297.M625,
!    ebook: http://www.mathworks.com/moler/chapters.html
!
  implicit none

  real ( kind = 8 ) ad
  real ( kind = 8 ) ar
  real ( kind = 8 ) bd
  real ( kind = 8 ) br
  real ( kind = 8 ) dx
  real ( kind = 8 ) dx_tol
  real ( kind = 8 ) e
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx_tol
  integer ( kind = 4 ) it
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) job
  real ( kind = 8 ) md
  real ( kind = 8 ) mr
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) xd
  real ( kind = 8 ) xr

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  The Kepler equation.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Kepler''s equation has the form';
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    X = M + E * sin ( X )'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  X represents the eccentric anomaly of a planet, the angle between the'
  write ( *, '(a)' ) '  perihelion (the point on the orbit nearest to the sun) through the sun'
  write ( *, '(a)' ) '  to the center of the ellipse, and the line from the center of the ellipse'
  write ( *, '(a)' ) '  to the planet.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  There are two parameters, E and M:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  * E is the eccentricity of the orbit, which should be between 0 and 1.0;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  * M is the angle from the perihelion made by a fictitious planet traveling'
  write ( *, '(a)' ) '    on a circular orbit centered at the sun, and traveling at a constant'
  write ( *, '(a)' ) '    angular velocity equal to the average angular velocity of the true'
  write ( *, '(a)' ) '    planet.  M is usually between 0 and 180 degrees, but can have any value.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  For convenience, X and M are measured in degrees.'
!
!  Problem parameters.
!
  md = 24.851090D+00
  mr = md * r8_pi / 180.0D+00
  e = 0.1D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Given eccentricity E = ', e
  write ( *, '(a,g14.6,a)' ) '  Given angle M = ', md, ' (degrees)'
  write ( *, '(a,g14.6,a)' ) '                = ', mr, ' (radians)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Given E and M, find corresponding X.'
!
!  Iteration parameters.
!
  fx_tol = 1.0D-09
  dx_tol = 1.0D-09
  it = 0
  it_max = 30
  job = 0
  fx = 0.0D+00
!
!  Initial guess for interval.
!
  ad = 0.0D+00
  bd = 180.0D+00

  ar = ad * r8_pi / 180.0D+00
  br = bd * r8_pi / 180.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I      X               FX              DX'
  write ( *, '(a)' ) ' '

  do

    call bisection_rc ( ar, br, xr, fx, job )

    if ( job < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Error return.'
      exit
    end if

    it = it + 1

    fx = xr - mr - e * sin ( xr )

    if ( it <= 2 ) then
      dx = abs ( br - ar )
    else
      dx = 0.5 * abs ( br - ar )
    end if

    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) it, xr, fx, dx

    if ( abs ( fx ) <= fx_tol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Function is small.'
      exit
    end if

    if ( dx <= dx_tol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Interval is tiny.'
      exit
    end if

    if ( it_max <= it ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Reached iteration limit.'
      exit
    end if

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  In Radians:'
  write ( *, '(a)' ) ''
  fx = ar - mr - e * sin ( ar );
  write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', ar, ', F(A) = ', fx
  fx = xr - mr - e * sin ( xr );
  write ( *, '(a,g14.6,a,g14.6)' ) '  X = ', xr, ', F(X) = ', fx
  fx = br - mr - e * sin ( br );
  write ( *, '(a,g14.6,a,g14.6)' ) '  B = ', br, ', F(B) = ', fx

  ad = ar * 180.0D+00 / r8_pi
  xd = xr * 180.0D+00 / r8_pi
  bd = br * 180.0D+00 / r8_pi

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  In Degrees:'
  write ( *, '(a)' ) ''
  fx = ( ad - md ) * r8_pi / 180.0D+00 - e * sin ( ad * r8_pi / 180.0D+00 );
  write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', ad, ', F(A) = ', fx
  fx = ( xd - md ) * r8_pi / 180.0D+00 - e * sin ( xd * r8_pi / 180.0D+00 );
  write ( *, '(a,g14.6,a,g14.6)' ) '  X = ', xd, ', F(X) = ', fx
  fx = ( bd - md ) * r8_pi / 180.0D+00 - e * sin ( bd * r8_pi / 180.0D+00);
  write ( *, '(a,g14.6,a,g14.6)' ) '  B = ', bd, ', F(B) = ', fx

  return
end

