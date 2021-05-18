function alngam ( xvalue, ifault )

!*****************************************************************************80
!
!! ALNGAM computes the logarithm of the gamma function.
!
!  Modified:
!
!    13 January 2008
!
!  Author:
!
!    Original FORTRAN77 version by Allan Macleod.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Allan Macleod,
!    Algorithm AS 245,
!    A Robust and Reliable Algorithm for the Logarithm of the Gamma Function,
!    Applied Statistics,
!    Volume 38, Number 2, 1989, pages 397-402.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XVALUE, the argument of the Gamma function.
!
!    Output, integer ( kind = 4 ) IFAULT, error flag.
!    0, no error occurred.
!    1, XVALUE is less than or equal to 0.
!    2, XVALUE is too big.
!
!    Output, real ( kind = 8 ) ALNGAM, the logarithm of the gamma function of X.
!
  implicit none

  real ( kind = 8 ) alngam
  real ( kind = 8 ), parameter :: alr2pi = 0.918938533204673D+00
  integer ( kind = 4 ) ifault
  real ( kind = 8 ), dimension ( 9 ) :: r1 = (/ &
    -2.66685511495D+00, &
    -24.4387534237D+00, &
    -21.9698958928D+00, &
     11.1667541262D+00, &
     3.13060547623D+00, &
     0.607771387771D+00, &
     11.9400905721D+00, &
     31.4690115749D+00, &
     15.2346874070D+00 /)
  real ( kind = 8 ), dimension ( 9 ) :: r2 = (/ &
    -78.3359299449D+00, &
    -142.046296688D+00, &
     137.519416416D+00, &
     78.6994924154D+00, &
     4.16438922228D+00, &
     47.0668766060D+00, &
     313.399215894D+00, &
     263.505074721D+00, &
     43.3400022514D+00 /)
  real ( kind = 8 ), dimension ( 9 ) :: r3 = (/ &
    -2.12159572323D+05, &
     2.30661510616D+05, &
     2.74647644705D+04, &
    -4.02621119975D+04, &
    -2.29660729780D+03, &
    -1.16328495004D+05, &
    -1.46025937511D+05, &
    -2.42357409629D+04, &
    -5.70691009324D+02 /)
  real ( kind = 8 ), dimension ( 5 ) :: r4 = (/ &
     0.279195317918525D+00, &
     0.4917317610505968D+00, &
     0.0692910599291889D+00, &
     3.350343815022304D+00, &
     6.012459259764103D+00 /)
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ), parameter :: xlge = 5.10D+05
  real ( kind = 8 ), parameter :: xlgst = 1.0D+30
  real ( kind = 8 ) xvalue
  real ( kind = 8 ) y

  x = xvalue
  alngam = 0.0D+00
!
!  Check the input.
!
  if ( xlgst <= x ) then
    ifault = 2
    return
  end if

  if ( x <= 0.0D+00 ) then
    ifault = 1
    return
  end if

  ifault = 0
!
!  Calculation for 0 < X < 0.5 and 0.5 <= X < 1.5 combined.
!
  if ( x < 1.5D+00 ) then

    if ( x < 0.5D+00 ) then

      alngam = - log ( x )
      y = x + 1.0D+00
!
!  Test whether X < machine epsilon.
!
      if ( y == 1.0D+00 ) then
        return
      end if

    else

      alngam = 0.0D+00
      y = x
      x = ( x - 0.5D+00 ) - 0.5D+00

    end if

    alngam = alngam + x * (((( &
        r1(5)   * y &
      + r1(4) ) * y &
      + r1(3) ) * y &
      + r1(2) ) * y &
      + r1(1) ) / (((( &
                  y &
      + r1(9) ) * y &
      + r1(8) ) * y &
      + r1(7) ) * y &
      + r1(6) )

    return

  end if
!
!  Calculation for 1.5 <= X < 4.0.
!
  if ( x < 4.0D+00 ) then

    y = ( x - 1.0D+00 ) - 1.0D+00

    alngam = y * (((( &
        r2(5)   * x &
      + r2(4) ) * x &
      + r2(3) ) * x &
      + r2(2) ) * x &
      + r2(1) ) / (((( &
                  x &
      + r2(9) ) * x &
      + r2(8) ) * x &
      + r2(7) ) * x &
      + r2(6) )
!
!  Calculation for 4.0 <= X < 12.0.
!
  else if ( x < 12.0D+00 ) then

    alngam = (((( &
        r3(5)   * x &
      + r3(4) ) * x &
      + r3(3) ) * x &
      + r3(2) ) * x &
      + r3(1) ) / (((( &
                  x &
      + r3(9) ) * x &
      + r3(8) ) * x &
      + r3(7) ) * x &
      + r3(6) )
!
!  Calculation for 12.0 <= X.
!
  else

    y = log ( x )
    alngam = x * ( y - 1.0D+00 ) - 0.5D+00 * y + alr2pi

    if ( x <= xlge ) then

      x1 = 1.0D+00 / x
      x2 = x1 * x1

      alngam = alngam + x1 * ( ( &
             r4(3)   * &
        x2 + r4(2) ) * &
        x2 + r4(1) ) / ( ( &
        x2 + r4(5) ) * &
        x2 + r4(4) )

    end if

  end if

  return
end
function alnorm ( x, upper )

!*****************************************************************************80
!
!! ALNORM computes the cumulative density of the standard normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2008
!
!  Author:
!
!    Original FORTRAN77 version by David Hill.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    David Hill,
!    Algorithm AS 66:
!    The Normal Integral,
!    Applied Statistics,
!    Volume 22, Number 3, 1973, pages 424-427.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, is one endpoint of the semi-infinite interval
!    over which the integration takes place.
!
!    Input, logical UPPER, determines whether the upper or lower
!    interval is to be integrated:
!    .TRUE.  => integrate from X to + Infinity;
!    .FALSE. => integrate from - Infinity to X.
!
!    Output, real ( kind = 8 ) ALNORM, the integral of the standard normal
!    distribution over the desired interval.
!
  implicit none

  real ( kind = 8 ), parameter :: a1 = 5.75885480458D+00
  real ( kind = 8 ), parameter :: a2 = 2.62433121679D+00
  real ( kind = 8 ), parameter :: a3 = 5.92885724438D+00
  real ( kind = 8 ) alnorm
  real ( kind = 8 ), parameter :: b1 = -29.8213557807D+00
  real ( kind = 8 ), parameter :: b2 = 48.6959930692D+00
  real ( kind = 8 ), parameter :: c1 = -0.000000038052D+00
  real ( kind = 8 ), parameter :: c2 = 0.000398064794D+00
  real ( kind = 8 ), parameter :: c3 = -0.151679116635D+00
  real ( kind = 8 ), parameter :: c4 = 4.8385912808D+00
  real ( kind = 8 ), parameter :: c5 = 0.742380924027D+00
  real ( kind = 8 ), parameter :: c6 = 3.99019417011D+00
  real ( kind = 8 ), parameter :: con = 1.28D+00
  real ( kind = 8 ), parameter :: d1 = 1.00000615302D+00
  real ( kind = 8 ), parameter :: d2 = 1.98615381364D+00
  real ( kind = 8 ), parameter :: d3 = 5.29330324926D+00
  real ( kind = 8 ), parameter :: d4 = -15.1508972451D+00
  real ( kind = 8 ), parameter :: d5 = 30.789933034D+00
  real ( kind = 8 ), parameter :: ltone = 7.0D+00
  real ( kind = 8 ), parameter :: p = 0.398942280444D+00
  real ( kind = 8 ), parameter :: q = 0.39990348504D+00
  real ( kind = 8 ), parameter :: r = 0.398942280385D+00
  logical up
  logical upper
  real ( kind = 8 ), parameter :: utzero = 18.66D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  up = upper
  z = x

  if ( z < 0.0D+00 ) then
    up = .not. up
    z = - z
  end if

  if ( ltone < z .and. ( ( .not. up ) .or. utzero < z ) ) then

    if ( up ) then
      alnorm = 0.0D+00
    else
      alnorm = 1.0D+00
    end if

    return

  end if

  y = 0.5D+00 * z * z

  if ( z <= con ) then

    alnorm = 0.5D+00 - z * ( p - q * y &
      / ( y + a1 + b1 &
      / ( y + a2 + b2 &
      / ( y + a3 ))))

  else

    alnorm = r * exp ( - y ) &
      / ( z + c1 + d1 &
      / ( z + c2 + d2 &
      / ( z + c3 + d3 &
      / ( z + c4 + d4 &
      / ( z + c5 + d5 &
      / ( z + c6 ))))))

  end if

  if ( .not. up ) then
    alnorm = 1.0D+00 - alnorm
  end if

  return
end
function alogam ( x, ifault )

!*****************************************************************************80
!
!! ALOGAM computes the logarithm of the Gamma function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 1999
!
!  Author:
!
!    Malcolm Pike, David Hill
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Malcolm Pike, David Hill,
!    Algorithm 291: 
!    Logarithm of Gamma Function,
!    Communications of the ACM,
!    Volume 9, Number 9, September 1966, page 684.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the Gamma function.
!    X should be greater than 0.
!
!    Output, integer ( kind = 4 ) IFAULT, error flag.
!    0, no error.
!    1, X <= 0.
!
!    Output, real ( kind = 8 ) ALOGAM, the logarithm of the Gamma 
!    function of X.
!
  implicit none

  real ( kind = 8 ) alogam
  real ( kind = 8 ) f
  integer ( kind = 4 ) ifault
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  if ( x <= 0.0D+00 ) then
    ifault = 1
    alogam = 0.0D+00
    return
  end if

  ifault = 0
  y = x

  if ( x < 7.0D+00 ) then

    f = 1.0D+00
    z = y

    do while ( z < 7.0D+00 )
      f = f * z
      z = z + 1.0D+00
    end do

    y = z
    f = - log ( f )

  else

    f = 0.0D+00

  end if

  z = 1.0D+00 / y / y
    
  alogam = f + ( y - 0.5D+00 ) * log ( y ) - y &
    + 0.918938533204673D+00 + &
    ((( &
    - 0.000595238095238D+00   * z &
    + 0.000793650793651D+00 ) * z &
    - 0.002777777777778D+00 ) * z &
    + 0.083333333333333D+00 ) / y

  return
end
function digamma ( x )

!*****************************************************************************80
!
!! DIGAMMA calculates DIGAMMA ( X ) = d ( LOG ( GAMMA ( X ) ) ) / dX
!
!  Modified:
!
!    03 June 2013
!
!  Author:
!
!    Original FORTRAN77 version by Jose Bernardo.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jose Bernardo,
!    Algorithm AS 103:
!    Psi ( Digamma ) Function,
!    Applied Statistics,
!    Volume 25, Number 3, 1976, pages 315-317.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the digamma function.
!    0 < X.
!
!    Output, real ( kind = 8 ) DIGAMMA, the value of the digamma function at X.
!
  implicit none

  real ( kind = 8 ), parameter :: euler_mascheroni = 0.57721566490153286060D+00
  real ( kind = 8 ) digamma
  real ( kind = 8 ) r
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
!
!  Check the input.
!
  if ( x <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DIGAMMA - Fatal error!'
    write ( *, '(a)' ) '  X <= 0.'
    stop
  end if
!
!  Initialize.
!
  x2 = x
  digamma = 0.0D+00
!
!  Approximation for small argument.
!
  if ( x2 <= 0.00001D+00 ) then
    digamma = - euler_mascheroni - 1.0D+00 / x2
    return
  end if
!
!  Reduce to DIGAMMA(X + N).
!
  do while ( x2 < 8.5D+00 )
    digamma = digamma - 1.0D+00 / x2
    x2 = x2 + 1.0D+00
  end do
!
!  Use Stirling's (actually de Moivre's) expansion.
!
  r = 1.0D+00 / x2
  digamma = digamma + log ( x2 ) - 0.5D+00 * r
  r = r * r
  digamma = digamma &
    - r * ( 1.0D+00 / 12.0D+00 &
    - r * ( 1.0D+00 / 120.0D+00 &
    - r *   1.0D+00 / 252.0D+00 ) )

  return
end
subroutine dirichlet_check ( n, a )

!*****************************************************************************80
!
!! DIRICHLET_CHECK checks the parameters of the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be nonnegative, and at least one should be positive.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  logical positive

  positive = .false.

  do i = 1, n

    if ( a(i) < 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DIRICHLET_CHECK - Fatal error!'
      write ( *, '(a)' ) '  A(I) < 0.'
      write ( *, '(a,i6)' ) '  For I = ', i
      write ( *, '(a,g14.6)' ) '  A(I) = ', a(i)
      stop
    else if ( 0.0D+00 < a(i) ) then
      positive = .true.
    end if

  end do

  if ( .not. positive ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DIRICHLET_CHECK - Fatal error!'
    write ( *, '(a)' ) '  All parameters are zero!'
    stop
  end if

  return
end
subroutine dirichlet_estimate ( k, n, x, ix, init, alpha, rlogl, v, g, &
  niter, s, eps, work, ifault )

!*****************************************************************************80
!
!! DIRICHLET_ESTIMATE estimates the parameters of a Dirichlet distribution.
!
!  Discussion:
!
!    This routine requires several auxilliary routines:
!
!      ALOGAM (CACM algorithm 291 or AS 245),
!      DIGAMA (AS 103),
!      GAMMAD (AS 239),
!      PPCHI2 (AS 91),
!      TRIGAM (AS 121).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 1999
!
!  Author:
!
!    Original FORTRAN77 version by A Naryanan.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    A. Naryanan,
!    Algorithm AS 266:
!    Maximum Likelihood Estimation of the Parameters of the
!    Dirichlet Distribution,
!    Applied Statistics,
!    Volume 40, Number 2, 1991, pages 365-374.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the number of parameters.
!    2 <= K.
!
!    Input, integer ( kind = 4 ) N, the number of observations.
!    K < N.
!
!    Input, real ( kind = 8 ) X(IX,K), contains the N by K array of samples
!    from the distribution.  X(I,J) is the J-th component of
!    the I-th sample.
!
!    Input, integer ( kind = 4 ) IX, the leading dimension of the array X.
!    N <= IX.
!
!    Input, integer ( kind = 4 ) INIT, specifies how the parameter estimates
!    are to be initialized:
!    1, use the method of moments;
!    2, initialize each ALPHA to the minimum of X;
!    otherwise, the input values of ALPHA already contain estimates.
!
!    Input/output, real ( kind = 8 ) ALPHA(K).
!    On input, if INIT is not 1 or 2, then ALPHA must contain
!    initial estimates for the parameters.
!    On output, with IFAULT = 0, ALPHA contains the computed
!    estimates for the parameters.
!
!    Output, real ( kind = 8 ) RLOGL, the value of the log-likelihood function
!    at the solution point.
!
!    Output, real ( kind = 8 ) V(K*(K+1)/2); V(J*(J-1)/2+I) contains the
!    covariance between ALPHA(I) and ALPHA(J), for I = 1 to J, J = 1 to K.
!
!    Output, real ( kind = 8 ) G(K), contains an estimate of the derivative of
!    the log-likelihood with respect to each component of ALPHA.
!
!    Output, integer ( kind = 4 ) NITER, contains the number of Newton-Raphson
!    iterations performed.
!
!    Output, real ( kind = 8 ) S, the value of the chi-squared statistic.
!
!    Output, real ( kind = 8 ) EPS, contains the probability that the 
!    chi-squared statistic is less than S.
!
!    Workspace, real ( kind = 8 ) WORK(2*K).
!
!    Output, integer ( kind = 4 ) IFAULT, error indicator.
!    0, no error, the results were computed successfully;
!    1, K < 2;
!    2, N <= K;
!    3, IX < N;
!    4, if X(I,J) <= 0 for any I or J, or if
!       ABS ( Sum ( 1 <= J <= K ) X(I,J) - 1 ) >= GAMMA = 0.001;
!    5, if IFAULT is returned nonzero from the chi-square
!       routine PPCHI2;
!    6, if ALPHA(J) <= 0 for any J during any step of the iteration;
!    7, if MAXIT iterations were carried out but convergence
!       was not achieved.
!
  implicit none

  integer ( kind = 4 ) ix
  integer ( kind = 4 ) k

  real ( kind = 8 ) alngam
  real ( kind = 8 ) alpha(k)
  real ( kind = 8 ), parameter :: alpha_min = 0.00001D+00
  real ( kind = 8 ) an
  real ( kind = 8 ) beta
  real ( kind = 8 ) chi2
  real ( kind = 8 ) digamma
  real ( kind = 8 ) eps
  real ( kind = 8 ) g(k)
  real ( kind = 8 ), parameter :: gamma = 0.0001D+00
  real ( kind = 8 ) gammad
  real ( kind = 8 ) gg
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) ifault
  integer ( kind = 4 ) ifault2
  integer ( kind = 4 ) init
  integer ( kind = 4 ), parameter :: it_max = 100
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) j
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) n
  integer ( kind = 4 ) niter
  real ( kind = 8 ) ppchi2
  real ( kind = 8 ) rk
  real ( kind = 8 ) rlogl
  real ( kind = 8 ) s
  real ( kind = 8 ) sum1
  real ( kind = 8 ) sum2
  real ( kind = 8 ) temp
  real ( kind = 8 ) trigamma
  real ( kind = 8 ) v((k*(k+1))/2)
  real ( kind = 8 ) varp1
  real ( kind = 8 ) work(2*k)
  real ( kind = 8 ) x(ix,k)
  real ( kind = 8 ) x_min
  real ( kind = 8 ) x11
  real ( kind = 8 ) x12

  ifault2 = 0
!
!  Check the input arguments.
!
  if ( k < 2 ) then
    ifault = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DIRICHLET_ESTIMATE - Fatal error!'
    write ( *, '(a)' ) '  K < 2.'
    stop
  end if

  if ( n <= k ) then
    ifault = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DIRICHLET_ESTIMATE - Fatal error!'
    write ( *, '(a)' ) '  N <= K.'
    stop
  end if

  if ( ix < n ) then
    ifault = 3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DIRICHLET_ESTIMATE - Fatal error!'
    write ( *, '(a)' ) '  IX < N.'
    stop
  end if

  do i = 1, n

    do j = 1, k
      if ( x(i,j) <= 0.0D+00 ) then
        niter = i
        ifault = 4
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIRICHLET_ESTIMATE - Fatal error!'
        write ( *, '(a)' ) '  X(I,J) <= 0.'
        stop
      end if
    end do

    sum2 = sum ( x(i,1:k) )

    if ( gamma <= abs ( sum2 - 1.0D+00 ) ) then
      ifault = 4
      niter = i
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DIRICHLET_ESTIMATE - Fatal error!'
      write ( *, '(a)' ) '  Row I does not sum to 1.'
      stop
    end if

  end do

  ifault = 0

  an = real ( n, kind = 8 )
  rk = real ( k, kind = 8 )
  niter = 0
!
!  Calculate initial estimates using the method of moments.
!
  if ( init == 1 ) then

    do j = 1, k - 1
      alpha(j) = sum ( x(1:n,j) ) / an
    end do

    alpha(k) = 1.0D+00 - sum ( alpha(1:k-1) )

    x12 = 0.0D+00
    do i = 1, n
      x12 = x12 + x(i,1) ** 2
    end do

    x12 = x12 / an
    varp1 = x12 - alpha(1) ** 2

    x11 = ( alpha(1) - x12 ) / varp1
    alpha(1:k) = x11 * alpha(1:k)
!
!  Calculate initial estimates using Ronning's suggestion.
!
  else if ( init == 2 ) then

    x_min = x(1,1)
    do j = 1, k
      do i = 1, n
        x_min = min ( x_min, x(i,j) )
      end do
    end do

    x_min = max ( x_min, alpha_min )

    alpha(1:k) = x_min

  end if
!
!  Check whether any ALPHA's are negative or zero.
!
  do j = 1, k
    if ( alpha(j) <= 0.0D+00 ) then
      ifault = 6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DIRICHLET_ESTIMATE - Fatal error!'
      write ( *, '(a,i6)' ) '  For J = ', j
      write ( *, '(a,g14.6)' ) '  ALPHA(J) = ', alpha(j)
      write ( *, '(a)' ) '  but ALPHA(J) must be positive.'
      stop
    end if
  end do
!
!  Calculate N * log ( G(J) ) for J = 1,2,...,K and store in WORK array.
!
  do j = 1, k
    work(j) = sum ( log ( x(1:n,j) ) )
  end do
!
!  Call Algorithm AS 91 to compute CHI2, the chi-squared value.
!
  gg = alngam ( rk / 2.0D+00, ifault )

  chi2 = ppchi2 ( gamma, rk, gg, ifault )

  if ( ifault /= 0 ) then
    ifault = 5
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DIRICHLET_ESTIMATE - Fatal error!'
    write ( *, '(a)' ) '  PPCHI2 returns error code.'
    stop
  end if
!
!  Carry out the Newton iteration.
!
  do it_num = 1, it_max

    sum2 = sum ( alpha(1:k) )

    sum1 = 0.0D+00
    do j = 1, k
      work(k+j) = trigamma ( alpha(j) )
      sum1 = sum1 + 1.0D+00 / work(k+j)
    end do

    beta = trigamma ( sum2 )
    beta = an * beta / ( 1.0D+00 - beta * sum1 )

    temp = digamma ( sum2 )

    do j = 1, k
      g(j) = an * ( temp - digamma ( alpha(j) ) ) + work(j)
    end do
!
!  Calculate the lower triangle of the Variance-Covariance matrix V.
!
    sum2 = beta / an**2
    do i = 1, k
      do j = 1, i
        kk = j + ( i * ( i - 1 ) ) / 2
        v(kk) = sum2 / ( work(k+i) * work(k+j) )
        if ( i == j ) then
          v(kk) = v(kk) + 1.0D+00 / ( an * work(k+j) )
        end if
      end do
    end do
!
!  Postmultiply the Variance-Covariance matrix V by G and store
!  in the last K elements of WORK.
!
    do i = 1, k

      sum2 = 0.0D+00
      i2 = ( i * ( i - 1 ) ) / 2
      do j = 1, i - 1
        sum2 = sum2 + v(i2+j) * g(j)
      end do
      do j = i + 1, k
        sum2 = sum2 + v(i+(j*(j-1))/2) * g(j)
      end do

      work(k+i) = sum2 + v((i*(i+1))/2) * g(i)

    end do
!
!  Update the ALPHA's.
!
    niter = it_num

    do j = 1, k
      alpha(j) = alpha(j) + work(k+j)
      alpha(j) = max ( alpha(j), alpha_min )
    end do

    do j = 1, k
      if ( alpha(j) <= 0.0D+00 ) then
        ifault = 6
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIRICHLET_ESTIMATE - Fatal error!'
        write ( *, '(a,i6)' ) '  Newton iteration ', it_num
        write ( *, '(a)' ) '  Computed ALPHA(J) <= 0.'
        write ( *, '(a,i6)' ) '  J = ', j
        write ( *, '(a,g14.6)' ) '  ALPHA(J) = ', alpha(j)
        stop
      end if
    end do
!
!  Test for convergence.
!
    s = dot_product ( g(1:k), work(k+1:k+k) )

    if ( s < chi2 ) then
      eps = gammad ( s / 2.0D+00, rk / 2.0D+00, ifault2 )

      sum2 = sum ( alpha(1:k) )

      rlogl = 0.0D+00
      do j = 1, k
        rlogl = rlogl + ( alpha(j) - 1.0D+00 ) * work(j) - an * &
          alngam ( alpha(j), ifault2 )
      end do

      rlogl = rlogl + an * alngam ( sum2, ifault2 )

      return

    end if

  end do

  ifault = 7

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DIRICHLET_ESTIMATE - Fatal error!'
  write ( *, '(a)' ) '  No convergence.'

  stop
end
subroutine dirichlet_mean ( n, a, mean )

!*****************************************************************************80
!
!! DIRICHLET_MEAN returns the means of the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be nonnegative, and at least one should be positive.
!
!    Output, real ( kind = 8 ) MEAN(N), the means of the PDF.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean(n)

  call dirichlet_check ( n, a )

  mean(1:n) = a(1:n) / sum ( a(1:n) )

  return
end
subroutine dirichlet_mix_mean ( comp_max, comp_num, elem_num, a, comp_weight, &
  mean )

!*****************************************************************************80
!
!! DIRICHLET_MIX_MEAN returns the means of a Dirichlet mixture PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) COMP_MAX, the leading dimension of A, which
!    must be at least COMP_NUM.
!
!    Input, integer ( kind = 4 ) COMP_NUM, the number of components in the
!    Dirichlet mixture density, that is, the number of distinct Dirichlet PDF's
!    that are mixed together.
!
!    Input, integer ( kind = 4 ) ELEM_NUM, the number of elements of an
!    observation.
!
!    Input, real ( kind = 8 ) A(COMP_MAX,ELEM_NUM), the probabilities for
!    element ELEM_NUM in component COMP_NUM.
!    Each A(I,J) should be greater than or equal to 0.0.
!
!    Input, real ( kind = 8 ) COMP_WEIGHT(COMP_NUM), the mixture weights of
!    the densities.
!    These do not need to be normalized.  The weight of a given component is
!    the relative probability that that component will be used to generate
!    the sample.
!
!    Output, real ( kind = 8 ) MEAN(ELEM_NUM), the means for each element.
!
  implicit none

  integer ( kind = 4 ) comp_max
  integer ( kind = 4 ) comp_num
  integer ( kind = 4 ) elem_num

  real ( kind = 8 ) a(comp_max,elem_num)
  real ( kind = 8 ) a_sum(comp_num)
  integer ( kind = 4 ) comp_i
  real ( kind = 8 ) comp_weight(comp_num)
  real ( kind = 8 ) comp_weight_sum
  integer ( kind = 4 ) elem_i
  real ( kind = 8 ) mean(elem_num)
!
!  Check.
!
  do comp_i = 1, comp_num

    do elem_i = 1, elem_num
      if ( a(comp_i,elem_i) < 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIRICHLET_MIX_MEAN - Fatal error!'
        write ( *, '(a)' ) '  A(COMP,ELEM) < 0.'
        write ( *, '(a,i6)' ) '  COMP = ', comp_i
        write ( *, '(a,i6)' ) '  ELEM = ', elem_i
        write ( *, '(a,g14.6)' ) '  A(COMP,ELEM) = ', a(comp_i,elem_i)
        stop
      end if
    end do

  end do

  comp_weight_sum = sum ( comp_weight(1:comp_num) )

  do comp_i = 1, comp_num
    a_sum(comp_i) = 0.0D+00
    do elem_i = 1, elem_num
      a_sum(comp_i) = a_sum(comp_i) + a(comp_i,elem_i)
    end do
  end do

  do elem_i = 1, elem_num
    mean(elem_i) = 0.0D+00
    do comp_i = 1, comp_num
      mean(elem_i) = mean(elem_i) + comp_weight(comp_i) * a(comp_i,elem_i) &
        / a_sum(comp_i)
    end do
    mean(elem_i) = mean(elem_i) / comp_weight_sum
  end do

  return
end
subroutine dirichlet_mix_sample ( comp_max, comp_num, elem_num, a, &
  comp_weight, seed, comp, x )

!*****************************************************************************80
!
!! DIRICHLET_MIX_SAMPLE samples a Dirichlet mixture PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) COMP_MAX, the leading dimension of A, which 
!    must be at least COMP_NUM.
!
!    Input, integer ( kind = 4 ) COMP_NUM, the number of components in the
!    Dirichlet mixture density, that is, the number of distinct Dirichlet PDF's
!    that are mixed together.
!
!    Input, integer ( kind = 4 ) ELEM_NUM, the number of elements of an
!    observation.
!
!    Input, real ( kind = 8 ) A(COMP_MAX,ELEM_NUM), the probabilities for
!    element ELEM_NUM in component COMP_NUM.
!    Each A(I,J) should be greater than or equal to 0.0.
!
!    Input, real ( kind = 8 ) COMP_WEIGHT(COMP_NUM), the mixture weights of
!    the densities.
!    These do not need to be normalized.  The weight of a given component is
!    the relative probability that that component will be used to generate
!    the sample.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, integer ( kind = 4 ) COMP, the index of the component of the
!    Dirichlet mixture that was chosen to generate the sample.
!
!    Output, real ( kind = 8 ) X(ELEM_NUM), a sample of the PDF.
!
  implicit none

  integer ( kind = 4 ) comp_max
  integer ( kind = 4 ) comp_num
  integer ( kind = 4 ) elem_num

  real ( kind = 8 ) a(comp_max,elem_num)
  integer ( kind = 4 ) comp
  integer ( kind = 4 ) comp_i
  real ( kind = 8 ) comp_weight(comp_num)
  real ( kind = 8 ) comp_weight_sum
  integer ( kind = 4 ) elem_i
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sum2
  real ( kind = 8 ) x(elem_num)
!
!  Check.
!
  do comp_i = 1, comp_num

    do elem_i = 1, elem_num
      if ( a(comp_i,elem_i) < 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIRICHLET_MIX_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  A(COMP,ELEM) < 0.'
        write ( *, '(a,i6)' ) '  COMP = ', comp_i
        write ( *, '(a,i6)' ) '  ELEM = ', elem_i
        write ( *, '(a,g14.6)' ) '  A(COMP,ELEM) = ', a(comp_i,elem_i)
        stop
      end if
    end do

  end do
!
!  Choose a particular density MIX.
!
  comp_weight_sum = sum ( comp_weight(1:comp_num) )

  r = r8_uniform_ab ( 0.0D+00, comp_weight_sum, seed )

  comp = 1
  sum2 = 0.0D+00

  do while ( comp < comp_num )

    sum2 = sum2 + comp_weight(comp)

    if ( r <= sum2 ) then
      exit
    end if

    comp = comp + 1

  end do
!
!  Sample density COMP.
!
  do elem_i = 1, elem_num
    call gamma_sample ( a(comp,elem_i), 1.0D+00, seed, x(elem_i) )
  end do
!
!  Normalize the result.
!
  x(1:elem_num) = x(1:elem_num) / sum ( x(1:elem_num) )

  return
end
subroutine dirichlet_sample ( n, a, seed, x )

!*****************************************************************************80
!
!! DIRICHLET_SAMPLE samples the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 169.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be nonnegative, and at least one should be
!    positive.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), a sample of the PDF.  The entries 
!    of X should sum to 1.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  call dirichlet_check ( n, a )

  do i = 1, n
    call gamma_sample ( a(i), 1.0D+00, seed, x(i) )
  end do
!
!  Normalize the result.
!
  x(1:n) = x(1:n) / sum ( x(1:n) )

  return
end
subroutine dirichlet_variance ( n, a, variance )

!*****************************************************************************80
!
!! DIRICHLET_VARIANCE returns the variances of the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be nonnegative, and at least one should be positive.
!
!    Output, real ( kind = 8 ) VARIANCE(N), the variances of the PDF.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_sum
  integer ( kind = 4 ) i
  real ( kind = 8 ) variance(n)

  call dirichlet_check ( n, a )

  a_sum = sum ( a(1:n) )

  do i = 1, n
    variance(i) = a(i) * ( a_sum - a(i) ) / ( a_sum**2 * ( a_sum + 1.0D+00 ) )
  end do

  return
end
subroutine exponential_01_sample ( seed, x )

!*****************************************************************************80
!
!! EXPONENTIAL_01_SAMPLE samples the Exponential PDF with parameters 0, 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  cdf = r8_uniform_01 ( seed )

  a = 0.0D+00
  b = 1.0D+00
  call exponential_cdf_inv ( cdf, a, b, x )

  return
end
subroutine exponential_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! EXPONENTIAL_CDF_INV inverts the Exponential CDF.
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
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x
!
!  Check.
!
  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'EXPONENTIAL_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.0'
    stop
  end if

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'EXPONENTIAL_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop
  end if

  x = a - b * log ( 1.0D+00 - cdf )

  return
end
function gamain ( x, p, ifault )

!*****************************************************************************80
!
!! GAMAIN computes the incomplete gamma ratio.
!
!  Discussion:
!
!    A series expansion is used if P > X or X <= 1.  Otherwise, a
!    continued fraction approximation is used.
!
!  Modified:
!
!    17 January 2008
!
!  Author:
!
!    G Bhattacharjee
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    G Bhattacharjee,
!    Algorithm AS 32:
!    The Incomplete Gamma Integral,
!    Applied Statistics,
!    Volume 19, Number 3, 1970, pages 285-287.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, P, the parameters of the incomplete 
!    gamma ratio.  0 <= X, and 0 < P.
!
!    Output, integer ( kind = 4 ) IFAULT, error flag.
!    0, no errors.
!    1, P <= 0.
!    2, X < 0.
!    3, underflow.
!    4, error return from the Log Gamma routine.
!
!    Output, real ( kind = 8 ) GAMAIN, the value of the incomplete
!    gamma ratio.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: acu = 1.0D-08
  real ( kind = 8 ) alngam
  real ( kind = 8 ) an
  real ( kind = 8 ) arg
  real ( kind = 8 ) b
  real ( kind = 8 ) dif
  real ( kind = 8 ) factor
  real ( kind = 8 ) g
  real ( kind = 8 ) gamain
  real ( kind = 8 ) gin
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifault
  real ( kind = 8 ), parameter :: oflo = 1.0D+37
  real ( kind = 8 ) p
  real ( kind = 8 ) pn(6)
  real ( kind = 8 ) rn
  real ( kind = 8 ) term
  real ( kind = 8 ), parameter :: uflo = 1.0D-37
  real ( kind = 8 ) x
!
!  Check the input.
!
  if ( p <= 0.0D+00 ) then
    ifault = 1
    gamain = 0.0D+00
    return
  end if

  if ( x < 0.0D+00 ) then
    ifault = 2
    gamain = 0.0D+00
    return
  end if

  if ( x == 0.0D+00 ) then
    ifault = 0
    gamain = 0.0D+00
    return
  end if

  g = alngam ( p, ifault )

  if ( ifault /= 0 ) then
    ifault = 4
    gamain = 0.0D+00
    return
  end if

  arg = p * log ( x ) - x - g

  if ( arg < log ( uflo ) ) then
    ifault = 3
    gamain = 0.0D+00
    return
  end if

  ifault = 0
  factor = exp ( arg )
!
!  Calculation by series expansion.
!
  if ( x <= 1.0D+00 .or. x < p ) then

    gin = 1.0D+00
    term = 1.0D+00
    rn = p

    do

      rn = rn + 1.0D+00
      term = term * x / rn
      gin = gin + term

      if ( term <= acu ) then
        exit
      end if

    end do

    gamain = gin * factor / p
    return

  end if
!
!  Calculation by continued fraction.
!
  a = 1.0D+00 - p
  b = a + x + 1.0D+00
  term = 0.0D+00

  pn(1) = 1.0D+00
  pn(2) = x
  pn(3) = x + 1.0D+00
  pn(4) = x * b

  gin = pn(3) / pn(4)

  do

    a = a + 1.0D+00
    b = b + 2.0D+00
    term = term + 1.0D+00
    an = a * term
    do i = 1, 2
      pn(i+4) = b * pn(i+2) - an * pn(i)
    end do

    if ( pn(6) /= 0.0D+00 ) then

      rn = pn(5) / pn(6)
      dif = abs ( gin - rn )
!
!  Absolute error tolerance satisfied?
!
      if ( dif <= acu ) then
!
!  Relative error tolerance satisfied?
!
        if ( dif <= acu * rn ) then
          gamain = 1.0D+00 - factor * gin
          exit
        end if

      end if

      gin = rn

    end if

    do i = 1, 4
      pn(i) = pn(i+2)
    end do

    if ( oflo <= abs ( pn(5) ) ) then

      do i = 1, 4
        pn(i) = pn(i) / oflo
      end do

    end if

  end do

  return
end
subroutine gamma_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! GAMMA_SAMPLE samples the Gamma PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 1999
!
!  Author:
!
!    Original FORTRAN77 version by Joachim Ahrens, Ulrich Dieter.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Computer Methods for Sampling from Gamma, Beta, Poisson and
!    Binomial Distributions,
!    Computing,
!    Volume 12, Number 3, September 1974, pages 223-246.
!
!    Joachim Ahrens, Ulrich Dieter,
!    Generating Gamma Variates by a Modified Rejection Technique,
!    Communications of the ACM,
!    Volume 25, Number 1, January 1982, pages 47-54.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ), parameter :: a1 =  0.3333333D+00
  real ( kind = 8 ), parameter :: a2 = -0.2500030D+00
  real ( kind = 8 ), parameter :: a3 =  0.2000062D+00
  real ( kind = 8 ), parameter :: a4 = -0.1662921D+00
  real ( kind = 8 ), parameter :: a5 =  0.1423657D+00
  real ( kind = 8 ), parameter :: a6 = -0.1367177D+00
  real ( kind = 8 ), parameter :: a7 =  0.1233795D+00
  real ( kind = 8 ), parameter :: e1 = 1.0D+00
  real ( kind = 8 ), parameter :: e2 = 0.4999897D+00
  real ( kind = 8 ), parameter :: e3 = 0.1668290D+00
  real ( kind = 8 ), parameter :: e4 = 0.0407753D+00
  real ( kind = 8 ), parameter :: e5 = 0.0102930D+00
  real ( kind = 8 ), parameter :: q1 =  0.04166669D+00
  real ( kind = 8 ), parameter :: q2 =  0.02083148D+00
  real ( kind = 8 ), parameter :: q3 =  0.00801191D+00
  real ( kind = 8 ), parameter :: q4 =  0.00144121D+00
  real ( kind = 8 ), parameter :: q5 = -0.00007388D+00
  real ( kind = 8 ), parameter :: q6 =  0.00024511D+00
  real ( kind = 8 ), parameter :: q7 =  0.00024240D+00

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) bcoef
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) q0
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_normal_01
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) s
  real ( kind = 8 ) s2
  real ( kind = 8 ) si
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) x
!
!  Allow A = 0.
!
  if ( a == 0.0D+00 ) then
    x = 0.0D+00
    return
  end if
!
!  A < 1.
!
  if ( a < 1.0D+00 ) then

    do

      p = r8_uniform_01 ( seed )
      p = ( 1.0D+00 + 0.3678794D+00 * a ) * p

      call exponential_01_sample ( seed, e )

      if ( 1.0D+00 <= p ) then

        x = - log ( ( 1.0D+00 + 0.3678794D+00 * a - p ) / a )

        if ( ( 1.0D+00 - a ) * log ( x ) <= e ) then
          x = x / b
          return
        end if

      else

        x = exp ( log ( p ) / a )

        if ( x <= e ) then
          x = x / b
          return
        end if

      end if

    end do
!
!  1 <= A.
!
  else

    s2 = a - 0.5D+00
    s = sqrt ( s2 )
    d = sqrt ( 32.0D+00 ) - 12.0D+00 * s

    t = r8_normal_01 ( seed )
    x = s + 0.5D+00 * t

    if ( 0.0D+00 <= t ) then
      x = ( x * x ) / b
      return
    end if

    u = r8_uniform_01 ( seed )

    if ( d * u <= t * t * t ) then
      x = x / b

      return
    end if

    r = 1.0D+00 / a
    q0 = ( ( ( ( ( ( &
           q7   * r &
         + q6 ) * r &
         + q5 ) * r &
         + q4 ) * r &
         + q3 ) * r &
         + q2 ) * r &
         + q1 ) * r

    if ( a <= 3.686D+00 ) then
      bcoef = 0.463D+00 + s - 0.178D+00 * s2
      si = 1.235D+00
      c = 0.195D+00 / s - 0.079D+00 + 0.016D+00 * s
    else if ( a <= 13.022D+00 ) then
      bcoef = 1.654D+00 + 0.0076D+00 * s2
      si = 1.68D+00 / s + 0.275D+00
      c = 0.062D+00 / s + 0.024D+00
    else
      bcoef = 1.77D+00
      si = 0.75D+00
      c = 0.1515D+00 / s
    end if

    if ( 0.0D+00 < sqrt ( a - 0.5D+00 ) + 0.5D+00 * t ) then

      v = 0.5D+00 * t / s

      if ( 0.25D+00 < abs ( v ) ) then
        q = q0 - s * t + 0.25D+00 * t * t + 2.0D+00 * s2 * log ( 1.0D+00 + v )
      else
        q = q0 + 0.5D+00 * t * t * ( ( ( ( ( ( &
               a7   * v &
             + a6 ) * v &
             + a5 ) * v &
             + a4 ) * v &
             + a3 ) * v &
             + a2 ) * v &
             + a1 ) * v
      end if

      if ( log ( 1.0D+00 - u ) <= q ) then
        x = x / b
        return
      end if

    end if

    do

      call exponential_01_sample ( seed, e )

      u = r8_uniform_01 ( seed )
      u = 2.0D+00 * u - 1.0D+00
      t = bcoef + sign ( si * e, u )

      if ( - 0.7187449D+00 <= t ) then

        v = 0.5D+00 * t / s

        if ( 0.25D+00 < abs ( v ) ) then
          q = q0 - s * t + 0.25D+00 * t * t + 2.0D+00 * s2 * log ( 1.0D+00 + v )
        else
          q = q0 + 0.5D+00 * t * t * ( ( ( ( ( ( &
                 a7   * v &
               + a6 ) * v &
               + a5 ) * v &
               + a4 ) * v &
               + a3 ) * v &
               + a2 ) * v &
               + a1 ) * v
        end if

        if ( 0.0D+00 < q ) then

          if ( 0.5D+00 < q ) then
            w = exp ( q ) - 1.0D+00
          else
            w = ( ( ( ( &
                   e5   * q &
                 + e4 ) * q &
                 + e3 ) * q &
                 + e2 ) * q &
                 + e1 ) * q
          end if

          if ( c * abs ( u ) <= w * exp ( e - 0.5D+00 * t * t ) ) then
            x = ( s + 0.5D+00 * t )**2 / b
            return
          end if

        end if

      end if

    end do

  end if

  return
end
function gammad ( x, p, ifault )

!*****************************************************************************80
!
!! GAMMAD computes the Incomplete Gamma Integral
!
!  Auxiliary functions:
!
!    ALOGAM = logarithm of the gamma function, 
!    ALNORM = algorithm AS66
!
!  Modified:
!
!    20 January 2008
!
!  Author:
!
!    Original FORTRAN77 version by B Shea.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    B Shea,
!    Algorithm AS 239:
!    Chi-squared and Incomplete Gamma Integral,
!    Applied Statistics,
!    Volume 37, Number 3, 1988, pages 466-473.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, P, the parameters of the incomplete 
!    gamma ratio.  0 <= X, and 0 < P.
!
!    Output, integer ( kind = 4 ) IFAULT, error flag.
!    0, no error.
!    1, X < 0 or P <= 0.
!
!    Output, real ( kind = 8 ) GAMMAD, the value of the incomplete 
!    Gamma integral.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alnorm
  real ( kind = 8 ) alngam
  real ( kind = 8 ) an
  real ( kind = 8 ) arg
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: elimit = - 88.0D+00
  real ( kind = 8 ) gammad
  integer ( kind = 4 ) ifault
  real ( kind = 8 ), parameter :: oflo = 1.0D+37
  real ( kind = 8 ) p
  real ( kind = 8 ), parameter :: plimit = 1000.0D+00
  real ( kind = 8 ) pn1
  real ( kind = 8 ) pn2
  real ( kind = 8 ) pn3
  real ( kind = 8 ) pn4
  real ( kind = 8 ) pn5
  real ( kind = 8 ) pn6
  real ( kind = 8 ) rn
  real ( kind = 8 ), parameter :: tol = 1.0D-14
  logical upper
  real ( kind = 8 ) x
  real ( kind = 8 ), parameter :: xbig = 1.0D+08

  gammad = 0.0D+00
!
!  Check the input.
!
  if ( x < 0.0D+00 ) then
    ifault = 1
    return
  end if

  if ( p <= 0.0D+00 ) then
    ifault = 1
    return
  end if

  ifault = 0

  if ( x == 0.0D+00 ) then
    gammad = 0.0D+00
    return
  end if
!
!  If P is large, use a normal approximation.
!
  if ( plimit < p ) then

    pn1 = 3.0D+00 * sqrt ( p ) * ( ( x / p )**( 1.0D+00 / 3.0D+00 ) &
    + 1.0D+00 / ( 9.0D+00 * p ) - 1.0D+00 )

    upper = .false.
    gammad = alnorm ( pn1, upper )
    return

  end if
!
!  If X is large set GAMMAD = 1.
!
  if ( xbig < x ) then
    gammad = 1.0D+00
    return
  end if
!
!  Use Pearson's series expansion.
!  (Note that P is not large enough to force overflow in ALOGAM).
!  No need to test IFAULT on exit since P > 0.
!
  if ( x <= 1.0D+00 .or. x < p ) then

    arg = p * log ( x ) - x - alngam ( p + 1.0D+00, ifault )
    c = 1.0D+00
    gammad = 1.0D+00
    a = p

    do

      a = a + 1.0D+00
      c = c * x / a
      gammad = gammad + c

      if ( c <= tol ) then
        exit
      end if

    end do

    arg = arg + log ( gammad )

    if ( elimit <= arg ) then
      gammad = exp ( arg )
    else
      gammad = 0.0D+00
    end if
!
!  Use a continued fraction expansion.
!
  else 

    arg = p * log ( x ) - x - alngam ( p, ifault )
    a = 1.0D+00 - p
    b = a + x + 1.0D+00
    c = 0.0D+00
    pn1 = 1.0D+00
    pn2 = x
    pn3 = x + 1.0D+00
    pn4 = x * b
    gammad = pn3 / pn4

    do

      a = a + 1.0D+00
      b = b + 2.0D+00
      c = c + 1.0D+00
      an = a * c
      pn5 = b * pn3 - an * pn1
      pn6 = b * pn4 - an * pn2

      if ( pn6 /= 0.0D+00 ) then

        rn = pn5 / pn6

        if ( abs ( gammad - rn ) <= min ( tol, tol * rn ) ) then
          exit
        end if

        gammad = rn

      end if

      pn1 = pn3
      pn2 = pn4
      pn3 = pn5
      pn4 = pn6
!
!  Rescale terms in continued fraction if terms are large.
!
      if ( oflo <= abs ( pn5 ) ) then
        pn1 = pn1 / oflo
        pn2 = pn2 / oflo
        pn3 = pn3 / oflo
        pn4 = pn4 / oflo
      end if

    end do

    arg = arg + log ( gammad )

    if ( elimit <= arg ) then
      gammad = 1.0D+00 - exp ( arg )
    else
      gammad = 1.0D+00
    end if

  end if

  return
end
function gammds ( x, p, ifault )

!*****************************************************************************80
!
!! GAMMDS computes the incomplete Gamma integral.
!
!  Discussion:
!
!    The parameters must be positive.  An infinite series is used.
!
!  Auxiliary function:
!
!    ALNGAM = CACM algorithm 291
!
!  Modified:
!
!    22 January 2008
!
!  Author:
!
!    Chi Leung Lau
!    Modifications by John Burkardt
!
!  Reference:
!
!    Chi Leung Lau,
!    Algorithm AS 147:
!    A Simple Series for the Incomplete Gamma Integral,
!    Applied Statistics,
!    Volume 29, Number 1, 1980, pages 113-114.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, P, the arguments of the incomplete
!    Gamma integral.  X and P must be greater than 0.
!
!    Output, integer ( kind = 4 ) IFAULT, error flag.
!    0, no errors.
!    1, X <= 0 or P <= 0.
!    2, underflow during the computation.
!
!    Output, real ( kind = 8 ) GAMMDS, the value of the incomplete
!    Gamma integral.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alngam
  real ( kind = 8 ) arg
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: e = 1.0D-09
  real ( kind = 8 ) f
  real ( kind = 8 ) gammds
  integer ifault
  integer ifault2
  real ( kind = 8 ) p
  real ( kind = 8 ), parameter :: uflo = 1.0D-37
  real ( kind = 8 ) x
!
!  Check the input.
!
  if ( x <= 0.0D+00 ) then
    ifault = 1
    gammds = 0.0D+00
    return
  end if

  if ( p <= 0.0D+00 ) then
    ifault = 1
    gammds = 0.0D+00
    return
  end if
!
!  ALNGAM is the natural logarithm of the gamma function.
!
  ifault2 = 0
  arg = p * log ( x ) - alngam ( p + 1.0D+00, ifault2 ) - x

  if ( arg < log ( uflo ) ) then
    gammds = 0.0D+00
    ifault = 2
    return
  end if

  f = exp ( arg )

  if ( f == 0.0D+00 ) then
    gammds = 0.0D+00
    ifault = 2
    return
  end if

  ifault = 0
!
!  Series begins.
!
  c = 1.0D+00
  gammds = 1.0D+00
  a = p

  do

    a = a + 1.0D+00
    c = c * x / a
    gammds = gammds + c

    if ( c <= e * gammds ) then
      exit
    end if

  end do

  gammds = gammds * f

  return
end
function lngamma ( z, ier )

!*****************************************************************************80
!
!! LNGAMMA computes Log(Gamma(X)) using a Lanczos approximation.
!
!  Discussion:
!
!    This algorithm is not part of the Applied Statistics algorithms.
!    It is slower but gives 14 or more significant decimal digits
!    accuracy, except around X = 1 and X = 2.   The Lanczos series from
!    which this algorithm is derived is interesting in that it is a
!    convergent series approximation for the gamma function, whereas
!    the familiar series due to De Moivre (and usually wrongly called
!    Stirling's approximation) is only an asymptotic approximation, as
!    is the true and preferable approximation due to Stirling.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 1999
!
!  Author:
!
!    Original FORTRAN77 version by Alan Miller.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Christian Lanczos,
!    A precision approximation of the gamma function,
!    SIAM Journal on Numerical Analysis, B,
!    Volume 1, 1964, pages 86-96.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Z, the argument of the Gamma function.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, no error occurred.
!    1, Z is less than or equal to 0.
!
!    Output, real ( kind = 8 ) LNGAMMA, the logarithm of the gamma 
!    function of Z.
!
  implicit none

  real ( kind = 8 ), parameter :: lnsqrt2pi = 0.9189385332046727D+00

  real ( kind = 8 ), parameter, dimension ( 9 ) :: a = (/ &
            0.9999999999995183D+00, &
          676.5203681218835D+00, &
       - 1259.139216722289D+00, &
          771.3234287757674D+00, &
        - 176.6150291498386D+00, &
           12.50734324009056D+00, &
          - 0.1385710331296526D+00, &
            0.9934937113930748D-05, &
            0.1659470187408462D-06 /)
  integer ( kind = 4 ) ier
  integer ( kind = 4 ) j
  real ( kind = 8 ) lngamma
  real ( kind = 8 ) tmp
  real ( kind = 8 ) z

  if ( z <= 0.0D+00 ) then
    ier = 1
    lngamma = 0.0D+00
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LNGAMMA - Fatal error!'
    write ( *, '(a)' ) '  Z <= 0.'
    stop
  end if

  ier = 0

  lngamma = 0.0D+00
  tmp = z + 7.0D+00
  do j = 9, 2, -1
    lngamma = lngamma + a(j) / tmp
    tmp = tmp - 1.0D+00
  end do

  lngamma = lngamma + a(1)
  lngamma = log ( lngamma ) + lnsqrt2pi - ( z + 6.5D+00 ) + ( z - 0.5D+00 ) * &
    log ( z + 6.5D+00 )

  return
end
subroutine normp ( z, p, q, pdf )

!*****************************************************************************80
!
!! NORMP computes the cumulative density of the standard normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 1999
!
!  Author:
!
!    Original FORTRAN77 version by Alan Miller.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thacher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Z, divides the real line into two semi-infinite
!    intervals, over each of which the standard normal distribution
!    is to be integrated.
!
!    Output, real ( kind = 8 ) P, Q, the integrals of the standard normal
!    distribution over the intervals ( - Infinity, Z] and
!    [Z, + Infinity ), respectively.
!
!    Output, real ( kind = 8 ) PDF, the value of the standard normal
!    distribution at Z.
!
  implicit none

  real ( kind = 8 ), parameter :: cutoff = 7.071D+00
  real ( kind = 8 ), parameter :: p0 = 220.2068679123761D+00
  real ( kind = 8 ), parameter :: p1 = 221.2135961699311D+00
  real ( kind = 8 ), parameter :: p2 = 112.0792914978709D+00
  real ( kind = 8 ), parameter :: p3 = 33.91286607838300D+00
  real ( kind = 8 ), parameter :: p4 = 6.373962203531650D+00
  real ( kind = 8 ), parameter :: p5 = 0.7003830644436881D+00
  real ( kind = 8 ), parameter :: p6 = 0.03526249659989109D+00
  real ( kind = 8 ), parameter :: q0 = 440.4137358247522D+00
  real ( kind = 8 ), parameter :: q1 = 793.8265125199484D+00
  real ( kind = 8 ), parameter :: q2 = 637.3336333788311D+00
  real ( kind = 8 ), parameter :: q3 = 296.5642487796737D+00
  real ( kind = 8 ), parameter :: q4 = 86.78073220294608D+00
  real ( kind = 8 ), parameter :: q5 = 16.06417757920695D+00
  real ( kind = 8 ), parameter :: q6 = 1.755667163182642D+00
  real ( kind = 8 ), parameter :: q7 = 0.08838834764831844D+00
  real ( kind = 8 ), parameter :: root2pi = 2.506628274631001D+00

  real ( kind = 8 ) expntl
  real ( kind = 8 ) p
  real ( kind = 8 ) pdf
  real ( kind = 8 ) q
  real ( kind = 8 ) z
  real ( kind = 8 ) zabs

  zabs = abs ( z )
!
!  37 < |Z|.
!
  if ( 37.0D+00 < zabs ) then

    pdf = 0.0D+00
    p = 0.0D+00
!
!  |Z| <= 37.
!
  else

    expntl = exp ( - 0.5D+00 * zabs**2 )
    pdf = expntl / root2pi
!
!  |Z| < CUTOFF = 10 / sqrt(2).
!
    if ( zabs < cutoff ) then

      p = expntl * (((((( &
             p6   * zabs &
           + p5 ) * zabs &
           + p4 ) * zabs &
           + p3 ) * zabs &
           + p2 ) * zabs &
           + p1 ) * zabs &
           + p0 ) / ((((((( &
             q7   * zabs &
           + q6 ) * zabs &
           + q5 ) * zabs &
           + q4 ) * zabs &
           + q3 ) * zabs &
           + q2 ) * zabs &
           + q1 ) * zabs &
           + q0 )
!
!  CUTOFF <= |Z|.
!
    else

      p = pdf / ( &
           zabs + 1.0D+00 / ( &
           zabs + 2.0D+00 / ( &
           zabs + 3.0D+00 / ( &
           zabs + 4.0D+00 / ( &
           zabs + 0.65D+00 )))))

    end if

  end if

  if ( z < 0.0D+00 ) then
    q = 1.0D+00 - p
  else
    q = p
    p = 1.0D+00 - q
  end if

  return
end
subroutine nprob ( z, p, q, pdf )

!*****************************************************************************80
!
!! NPROB computes the cumulative density of the standard normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 1999
!
!  Author:
!
!    Original FORTRAN77 version by AG Adams.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    AG Adams,
!    Algorithm 39:
!    Areas Under the Normal Curve,
!    Computer Journal,
!    Volume 12, 1969, pages 197-198.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Z, divides the line into two semi-infinite
!    intervals, over each of which the standard normal distribution
!    is to be integrated.
!
!    Output, real ( kind = 8 ) P, Q, the integrals of the standard normal
!    distribution over the intervals ( -oo, Z] and
!    [Z, +oo ), respectively.
!
!    Output, real ( kind = 8 ) PDF, the value of the standard normal
!    distribution at Z.
!
  implicit none

  real ( kind = 8 ), parameter :: a0 = 0.5D+00
  real ( kind = 8 ), parameter :: a1 = 0.398942280444D+00
  real ( kind = 8 ), parameter :: a2 = 0.399903438504D+00
  real ( kind = 8 ), parameter :: a3 = 5.75885480458D+00
  real ( kind = 8 ), parameter :: a4 = 29.8213557808D+00
  real ( kind = 8 ), parameter :: a5 = 2.62433121679D+00
  real ( kind = 8 ), parameter :: a6 = 48.6959930692D+00
  real ( kind = 8 ), parameter :: a7 = 5.92885724438D+00
  real ( kind = 8 ), parameter :: b0 = 0.398942280385D+00
  real ( kind = 8 ), parameter :: b1 = 0.000000038052D+00
  real ( kind = 8 ), parameter :: b2 = 1.00000615302D+00
  real ( kind = 8 ), parameter :: b3 = 0.000398064794D+00
  real ( kind = 8 ), parameter :: b4 = 1.98615381364D+00
  real ( kind = 8 ), parameter :: b5 = 0.151679116635D+00
  real ( kind = 8 ), parameter :: b6 = 5.29330324926D+00
  real ( kind = 8 ), parameter :: b7 = 4.8385912808D+00
  real ( kind = 8 ), parameter :: b8 = 15.1508972451D+00
  real ( kind = 8 ), parameter :: b9 = 0.742380924027D+00
  real ( kind = 8 ), parameter :: b10 = 30.789933034D+00
  real ( kind = 8 ), parameter :: b11 = 3.99019417011D+00

  real ( kind = 8 ) p
  real ( kind = 8 ) pdf
  real ( kind = 8 ) q
  real ( kind = 8 ) y
  real ( kind = 8 ) z
  real ( kind = 8 ) zabs

  zabs = abs ( z )
!
!  |Z| between 0 and 1.28
!
  if ( abs ( z ) <= 1.28D+00 ) then

    y = a0 * z**2
    pdf = exp ( - y ) * b0

    q = a0 - zabs * ( a1 - a2 * y &
         / ( y + a3 - a4 &
         / ( y + a5 + a6 &
         / ( y + a7 ))))
!
!  |Z| between 1.28 and 12.7
!
  else if ( abs ( z ) <= 12.7D+00 ) then

    y = a0 * z**2
    pdf = exp ( - y ) * b0

    q = pdf &
         / ( zabs - b1 + b2 &
         / ( zabs + b3 + b4 &
         / ( zabs - b5 + b6 &
         / ( zabs + b7 - b8 &
         / ( zabs + b9 + b10 &
         / ( zabs + b11 ))))))
!
!  Z far out in tail.
!
  else

    q = 0.0D+00
    pdf = 0.0D+00

  end if

  if ( z < 0.0D+00 ) then
    p = q
    q = 1.0D+00 - p
  else
    p = 1.0D+00 - q
  end if

  return
end
function ppchi2 ( p, v, g, ifault )

!*****************************************************************************80
!
!! PPCHI2 evaluates the percentage points of the Chi-squared PDF.
!
!  Discussion
!
!    Incorporates the suggested changes in AS R85 (vol.40(1),
!    pages 233-5, 1991) which should eliminate the need for the limited
!    range for P, though these limits have not been removed
!    from the routine.
!
!  Modified:
!
!    01 January 2008
!
!  Author:
!
!    Original FORTRAN77 version by Donald Best, DE Roberts.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Donald Best, DE Roberts,
!    Algorithm AS 91:
!    The Percentage Points of the Chi-Squared Distribution,
!    Applied Statistics,
!    Volume 24, Number 3, 1975, pages 385-390.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P,  value of the chi-squared cumulative
!    probability density function.
!    0.000002 <= P <= 0.999998.
!
!    Input, real ( kind = 8 ) V, the parameter of the chi-squared probability
!    density function.
!    0 < V.
!
!    Input, real ( kind = 8 ) G, the value of log ( Gamma ( V / 2 ) ).
!
!    Output, integer IFAULT, is nonzero if an error occurred.
!    0, no error.
!    1, P is outside the legal range.
!    2, V is not positive.
!    3, an error occurred in GAMMAD.
!    4, the result is probably as accurate as the machine will allow.
!
!    Output, real ( kind = 8 ) PPCHI2, the value of the chi-squared random
!    deviate with the property that the probability that a chi-squared random
!    deviate with parameter V is less than or equal to PPCHI2 is P.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: aa = 0.6931471806D+00
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: c1 = 0.01D+00
  real ( kind = 8 ), parameter :: c2 = 0.222222D+00
  real ( kind = 8 ), parameter :: c3 = 0.32D+00
  real ( kind = 8 ), parameter :: c4 = 0.4D+00
  real ( kind = 8 ), parameter :: c5 = 1.24D+00
  real ( kind = 8 ), parameter :: c6 = 2.2D+00
  real ( kind = 8 ), parameter :: c7 = 4.67D+00
  real ( kind = 8 ), parameter :: c8 = 6.66D+00
  real ( kind = 8 ), parameter :: c9 = 6.73D+00
  real ( kind = 8 ), parameter :: c10 = 13.32D+00
  real ( kind = 8 ), parameter :: c11 = 60.0D+00
  real ( kind = 8 ), parameter :: c12 = 70.0D+00
  real ( kind = 8 ), parameter :: c13 = 84.0D+00
  real ( kind = 8 ), parameter :: c14 = 105.0D+00
  real ( kind = 8 ), parameter :: c15 = 120.0D+00
  real ( kind = 8 ), parameter :: c16 = 127.0D+00
  real ( kind = 8 ), parameter :: c17 = 140.0D+00
  real ( kind = 8 ), parameter :: c18 = 175.0D+00
  real ( kind = 8 ), parameter :: c19 = 210.0D+00
  real ( kind = 8 ), parameter :: c20 = 252.0D+00
  real ( kind = 8 ), parameter :: c21 = 264.0D+00
  real ( kind = 8 ), parameter :: c22 = 294.0D+00
  real ( kind = 8 ), parameter :: c23 = 346.0D+00
  real ( kind = 8 ), parameter :: c24 = 420.0D+00
  real ( kind = 8 ), parameter :: c25 = 462.0D+00
  real ( kind = 8 ), parameter :: c26 = 606.0D+00
  real ( kind = 8 ), parameter :: c27 = 672.0D+00
  real ( kind = 8 ), parameter :: c28 = 707.0D+00
  real ( kind = 8 ), parameter :: c29 = 735.0D+00
  real ( kind = 8 ), parameter :: c30 = 889.0D+00
  real ( kind = 8 ), parameter :: c31 = 932.0D+00
  real ( kind = 8 ), parameter :: c32 = 966.0D+00
  real ( kind = 8 ), parameter :: c33 = 1141.0D+00
  real ( kind = 8 ), parameter :: c34 = 1182.0D+00
  real ( kind = 8 ), parameter :: c35 = 1278.0D+00
  real ( kind = 8 ), parameter :: c36 = 1740.0D+00
  real ( kind = 8 ), parameter :: c37 = 2520.0D+00
  real ( kind = 8 ), parameter :: c38 = 5040.0D+00
  real ( kind = 8 ) ch
  real ( kind = 8 ), parameter :: e = 0.5D-06
  real ( kind = 8 ) g
  real ( kind = 8 ) gammad
  integer ( kind = 4 ) i
  integer ( kind = 4 ) if1
  integer ( kind = 4 ) ifault
  integer ( kind = 4 ), parameter :: maxit = 20
  real ( kind = 8 ) ppchi2
  real ( kind = 8 ) p
  real ( kind = 8 ), parameter :: pmax = 0.999998D+00
  real ( kind = 8 ), parameter :: pmin = 0.000002D+00
  real ( kind = 8 ) p1
  real ( kind = 8 ) p2
  real ( kind = 8 ) ppnd
  real ( kind = 8 ) q
  real ( kind = 8 ) s1
  real ( kind = 8 ) s2
  real ( kind = 8 ) s3
  real ( kind = 8 ) s4
  real ( kind = 8 ) s5
  real ( kind = 8 ) s6
  real ( kind = 8 ) t
  real ( kind = 8 ) v
  real ( kind = 8 ) x
  real ( kind = 8 ) xx
!
!  Test arguments and initialize.
!
  ppchi2 = - 1.0D+00

  if ( p < pmin .or. pmax < p ) then
    ifault = 1
    return
  end if

  if ( v <= 0.0D+00 ) then
    ifault = 2
    return
  end if

  ifault = 0
  xx = 0.5D+00 * v
  c = xx - 1.0D+00
!
!  Starting approximation for small chi-squared
!
  if ( v < - c5 * log ( p ) ) then

    ch = ( p * xx * exp ( g + xx * aa ) ) ** ( 1.0D+00 / xx )

    if ( ch < e ) then
      ppchi2 = ch
      return
    end if
!
!  Starting approximation for V less than or equal to 0.32
!
  else if ( v <= c3 ) then

    ch = c4
    a = log ( 1.0D+00 - p )

    do

      q = ch
      p1 = 1.0D+00 + ch * ( c7 + ch )
      p2 = ch * (c9 + ch * ( c8 + ch ) )

      t = - 0.5D+00 + (c7 + 2.0D+00 * ch ) / p1 - ( c9 + ch * ( c10 + &
      3.0D+00 * ch ) ) / p2

      ch = ch - ( 1.0D+00 - exp ( a + g + 0.5D+00 * ch + c * aa ) * &
      p2 / p1) / t

      if ( abs ( q / ch - 1.0D+00 ) <= c1 ) then
        exit
      end if

    end do

  else
!
!  Call to algorithm AS 111 - note that P has been tested above.
!  AS 241 could be used as an alternative.
!
    x = ppnd ( p, ifault )
!
!  Starting approximation using Wilson and Hilferty estimate
!
    p1 = c2 / v
    ch = v * ( x * sqrt ( p1 ) + 1.0D+00 - p1)**3
!
!  Starting approximation for P tending to 1.
!
    if ( c6 * v + 6.0D+00 < ch ) then
       ch = - 2.0D+00 * ( log ( 1.0D+00 - p ) - c * log ( 0.5D+00 * ch ) + g )
    end if

  end if
!
!  Call to algorithm AS 239 and calculation of seven term
!  Taylor series
!
  do i = 1, maxit

    q = ch
    p1 = 0.5D+00 * ch
    p2 = p - gammad ( p1, xx, if1 )

    if ( if1 /= 0 ) then
      ifault = 3
      return
    end if

    t = p2 * exp ( xx * aa + g + p1 - c * log ( ch ) )
    b = t / ch
    a = 0.5D+00 * t - b * c
    s1 = ( c19 + a * ( c17 + a * ( c14 + a * ( c13 + a * ( c12 + &
    c11 * a ))))) / c24
    s2 = ( c24 + a * ( c29 + a * ( c32 + a * ( c33 + c35 * a )))) / c37
    s3 = ( c19 + a * ( c25 + a * ( c28 + c31 * a ))) / c37
    s4 = ( c20 + a * ( c27 + c34 * a) + c * ( c22 + a * ( c30 + c36 * a ))) &
    / c38
    s5 = ( c13 + c21 * a + c * ( c18 + c26 * a )) / c37
    s6 = ( c15 + c * ( c23 + c16 * c )) / c38
    ch = ch + t * ( 1.0D+00 + 0.5D+00 * t * s1 - b * c * ( s1 - b * &
    ( s2 - b * ( s3 - b * ( s4 - b * ( s5 - b * s6 ))))))

    if ( e < abs ( q / ch - 1.0D+00 ) ) then
       ppchi2 = ch
       return
    end if

  end do

 ifault = 4
 ppchi2 = ch

  return
end
function ppnd ( p, ifault )

!*****************************************************************************80
!
!! PPND produces the normal deviate value corresponding to lower tail area = P.
!
!  Modified:
!
!    21 January 2008
!
!  Author:
!
!    Original FORTRAN77 version by J Beasley, S Springer.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    J Beasley, S Springer,
!    Algorithm AS 111:
!    The Percentage Points of the Normal Distribution,
!    Applied Statistics,
!    Volume 26, Number 1, 1977, pages 118-121.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the value of the cumulative probability
!    densitity function.  0 < P < 1.
!
!    Output, integer ( kind = 4 ) IFAULT, error flag.
!    0, no error.
!    1, P <= 0 or P >= 1.  PPND is returned as 0.
!
!    Output, real ( kind = 8 ) PPND, the normal deviate value with the property
!    that the probability of a standard normal deviate being less than or
!    equal to PPND is P.
!
  implicit none

  real ( kind = 8 ), parameter :: a0 = 2.50662823884D+00
  real ( kind = 8 ), parameter :: a1 = -18.61500062529D+00
  real ( kind = 8 ), parameter :: a2 = 41.39119773534D+00
  real ( kind = 8 ), parameter :: a3 = -25.44106049637D+00
  real ( kind = 8 ), parameter :: b1 = -8.47351093090D+00
  real ( kind = 8 ), parameter :: b2 = 23.08336743743D+00
  real ( kind = 8 ), parameter :: b3 = -21.06224101826D+00
  real ( kind = 8 ), parameter :: b4 = 3.13082909833D+00
  real ( kind = 8 ), parameter :: c0 = -2.78718931138D+00
  real ( kind = 8 ), parameter :: c1 = -2.29796479134D+00
  real ( kind = 8 ), parameter :: c2 = 4.85014127135D+00
  real ( kind = 8 ), parameter :: c3 = 2.32121276858D+00
  real ( kind = 8 ), parameter :: d1 = 3.54388924762D+00
  real ( kind = 8 ), parameter :: d2 = 1.63706781897D+00
  integer ( kind = 4 ) ifault
  real ( kind = 8 ) p
  real ( kind = 8 ) ppnd
  real ( kind = 8 ) r
  real ( kind = 8 ), parameter :: split = 0.42D+00
  real ( kind = 8 ) value

  ifault = 0
!
!  0.08 < P < 0.92
!
  if ( abs ( p - 0.5D+00 ) <= split ) then

    r = ( p - 0.5D+00 ) * ( p - 0.5D+00 )

    value = ( p - 0.5D+00 ) * ( ( ( &
        a3   * r &
      + a2 ) * r &
      + a1 ) * r &
      + a0 ) / ( ( ( ( &
        b4   * r &
      + b3 ) * r &
      + b2 ) * r &
      + b1 ) * r &
      + 1.0D+00 )
!
!  P < 0.08 or P > 0.92,
!  R = min ( P, 1-P )
!
  else if ( 0.0D+00 < p .and. p < 1.0D+00 ) then

    if ( 0.5D+00 < p ) then
      r = sqrt ( - log ( 1.0D+00 - p ) )
    else
      r = sqrt ( - log ( p ) )
    end if

    value = ( ( ( &
        c3   * r &
      + c2 ) * r &
      + c1 ) * r &
      + c0 ) / ( ( &
        d2   * r &
      + d1 ) * r &
      + 1.0D+00 )

    if ( p < 0.5D+00 ) then
      value = - value
    end if
!
!  P <= 0.0 or 1.0 <= P
!
  else

    ifault = 1
    value = 0.0D+00

  end if

  ppnd = value

  return
end
function ppnd16 ( p, ifault )

!*****************************************************************************80
!
!! PPND16 produces the normal deviate value with lower tail area = P.
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
!    28 March 1999
!
!  Author:
!
!    Original FORTRAN77 version by Michael Wichura.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Michael Wichura,
!    Algorithm AS 241:
!    The Percentage Points of the Normal Distribution,
!    Applied Statistics,
!    Volume 37, Number 3, 1988, pages 477-484.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the value of the cumulative probability
!    densitity function.  0 < P < 1.
!
!    Output, integer ( kind = 4 ) IFAULT, error flag.
!    0, no error.
!    1, P <= 0 or 1 <= P.
!
!    Output, real ( kind = 8 ) PPND16, the normal deviate value with the
!    property that the probability of a standard normal deviate being
!    less than or equal to PPND16 is P.
!
  implicit none

  real ( kind = 8 ), parameter :: a0 = 3.3871328727963666080d+00
  real ( kind = 8 ), parameter :: a1 = 1.3314166789178437745d+02
  real ( kind = 8 ), parameter :: a2 = 1.9715909503065514427d+03
  real ( kind = 8 ), parameter :: a3 = 1.3731693765509461125d+04
  real ( kind = 8 ), parameter :: a4 = 4.5921953931549871457d+04
  real ( kind = 8 ), parameter :: a5 = 6.7265770927008700853d+04
  real ( kind = 8 ), parameter :: a6 = 3.3430575583588128105d+04
  real ( kind = 8 ), parameter :: a7 = 2.5090809287301226727d+03
  real ( kind = 8 ), parameter :: b1 = 4.2313330701600911252d+01
  real ( kind = 8 ), parameter :: b2 = 6.8718700749205790830d+02
  real ( kind = 8 ), parameter :: b3 = 5.3941960214247511077d+03
  real ( kind = 8 ), parameter :: b4 = 2.1213794301586595867d+04
  real ( kind = 8 ), parameter :: b5 = 3.9307895800092710610d+04
  real ( kind = 8 ), parameter :: b6 = 2.8729085735721942674d+04
  real ( kind = 8 ), parameter :: b7 = 5.2264952788528545610d+03
  real ( kind = 8 ), parameter :: c0 = 1.42343711074968357734d+00
  real ( kind = 8 ), parameter :: c1 = 4.63033784615654529590d+00
  real ( kind = 8 ), parameter :: c2 = 5.76949722146069140550d+00
  real ( kind = 8 ), parameter :: c3 = 3.64784832476320460504d+00
  real ( kind = 8 ), parameter :: c4 = 1.27045825245236838258d+00
  real ( kind = 8 ), parameter :: c5 = 2.41780725177450611770d-01
  real ( kind = 8 ), parameter :: c6 = 2.27238449892691845833d-02
  real ( kind = 8 ), parameter :: c7 = 7.74545014278341407640d-04
  real ( kind = 8 ), parameter :: const1 = 0.180625d+00
  real ( kind = 8 ), parameter :: const2 = 1.6d+00
  real ( kind = 8 ), parameter :: d1 = 2.05319162663775882187d+00
  real ( kind = 8 ), parameter :: d2 = 1.67638483018380384940d+00
  real ( kind = 8 ), parameter :: d3 = 6.89767334985100004550d-01
  real ( kind = 8 ), parameter :: d4 = 1.48103976427480074590d-01
  real ( kind = 8 ), parameter :: d5 = 1.51986665636164571966d-02
  real ( kind = 8 ), parameter :: d6 = 5.47593808499534494600d-04
  real ( kind = 8 ), parameter :: d7 = 1.05075007164441684324d-09
  real ( kind = 8 ), parameter :: e0 = 6.65790464350110377720d+00
  real ( kind = 8 ), parameter :: e1 = 5.46378491116411436990d+00
  real ( kind = 8 ), parameter :: e2 = 1.78482653991729133580d+00
  real ( kind = 8 ), parameter :: e3 = 2.96560571828504891230d-01
  real ( kind = 8 ), parameter :: e4 = 2.65321895265761230930d-02
  real ( kind = 8 ), parameter :: e5 = 1.24266094738807843860d-03
  real ( kind = 8 ), parameter :: e6 = 2.71155556874348757815d-05
  real ( kind = 8 ), parameter :: e7 = 2.01033439929228813265d-07
  real ( kind = 8 ), parameter :: f1 = 5.99832206555887937690d-01
  real ( kind = 8 ), parameter :: f2 = 1.36929880922735805310d-01
  real ( kind = 8 ), parameter :: f3 = 1.48753612908506148525d-02
  real ( kind = 8 ), parameter :: f4 = 7.86869131145613259100d-04
  real ( kind = 8 ), parameter :: f5 = 1.84631831751005468180d-05
  real ( kind = 8 ), parameter :: f6 = 1.42151175831644588870d-07
  real ( kind = 8 ), parameter :: f7 = 2.04426310338993978564d-15
  real ( kind = 8 ), parameter :: split1 = 0.425d+00
  real ( kind = 8 ), parameter :: split2 = 5.0d+00

  integer ( kind = 4 ) ifault
  real ( kind = 8 ) p
  real ( kind = 8 ) ppnd16
  real ( kind = 8 ) q
  real ( kind = 8 ) r

  ifault = 0
  q = p - 0.5D+00

  if ( abs ( q ) <= split1 ) then

    r = const1 - q**2

    ppnd16 = q * ((((((( &
           a7   * r &
         + a6 ) * r &
         + a5 ) * r &
         + a4 ) * r &
         + a3 ) * r &
         + a2 ) * r &
         + a1 ) * r &
         + a0 ) / ((((((( &
           b7   * r &
         + b6 ) * r &
         + b5 ) * r &
         + b4 ) * r &
         + b3 ) * r &
         + b2 ) * r &
         + b1 ) * r &
         + 1.0D+00 )

  else

    if ( q < 0.0D+00 ) then
      r = p
    else
      r = 1.0D+00 - p
    end if

    if ( r <= 0.0D+00 ) then
      ifault = 1
      ppnd16 = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PPND16 - Warning!'
      write ( *, '(a)' ) '  P <= 0 or 1 <= P.'
      write ( *, '(a)' ) '  PPND16 value would be infinite.'
      return
    end if

    r = sqrt ( - log ( r ) )

    if ( r <= split2 ) then

      r = r - const2

      ppnd16 = ((((((( &
             c7   * r &
           + c6 ) * r &
           + c5 ) * r &
           + c4 ) * r &
           + c3 ) * r &
           + c2 ) * r &
           + c1 ) * r &
           + c0 ) / ((((((( &
             d7   * r &
           + d6 ) * r &
           + d5 ) * r &
           + d4 ) * r &
           + d3 ) * r &
           + d2 ) * r &
           + d1 ) * r &
           + 1.0D+00 )

    else

      r = r - split2

      ppnd16 = ((((((( &
             e7   * r &
           + e6 ) * r &
           + e5 ) * r &
           + e4 ) * r &
           + e3 ) * r &
           + e2 ) * r &
           + e1 ) * r &
           + e0 ) / ((((((( &
             f7   * r &
           + f6 ) * r &
           + f5 ) * r &
           + f4 ) * r &
           + f3 ) * r &
           + f2 ) * r &
           + f1 ) * r &
           + 1.0D+00 )

    end if

    if ( q < 0.0D+00 ) then
      ppnd16 = - ppnd16
    end if

  end if

  return
end
function r8_gamma_log ( x )

!*****************************************************************************80
!
!! R8_GAMMA_LOG evaluates the logarithm of the gamma function.
!
!  Discussion:
!
!    This routine calculates the LOG(GAMMA) function for a positive real
!    argument X.  Computation is based on an algorithm outlined in
!    references 1 and 2.  The program uses rational functions that
!    theoretically approximate LOG(GAMMA) to at least 18 significant
!    decimal digits.  The approximation for X > 12 is from reference
!    3, while approximations for X < 12.0 are similar to those in
!    reference 1, but are unpublished.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody, Kenneth Hillstrom,
!    Chebyshev Approximations for the Natural Logarithm of the
!    Gamma Function,
!    Mathematics of Computation,
!    Volume 21, Number 98, April 1967, pages 198-203.
!
!    Kenneth Hillstrom,
!    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
!    May 1969.
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) R8_GAMMA_LOG, the value of the function.
!
  implicit none

  real ( kind = 8 ), dimension ( 7 ) :: c = (/ &
    -1.910444077728D-03, &
     8.4171387781295D-04, &
    -5.952379913043012D-04, &
     7.93650793500350248D-04, &
    -2.777777777777681622553D-03, &
     8.333333333333333331554247D-02, &
     5.7083835261D-03 /)
  real ( kind = 8 ) corr
  real ( kind = 8 ) :: d1 = -5.772156649015328605195174D-01
  real ( kind = 8 ) :: d2 = 4.227843350984671393993777D-01
  real ( kind = 8 ) :: d4 = 1.791759469228055000094023D+00
  real ( kind = 8 ), parameter :: frtbig = 2.25D+76
  integer ( kind = 4 ) i
  real ( kind = 8 ), dimension ( 8 ) :: p1 = (/ &
    4.945235359296727046734888D+00, &
    2.018112620856775083915565D+02, &
    2.290838373831346393026739D+03, &
    1.131967205903380828685045D+04, &
    2.855724635671635335736389D+04, &
    3.848496228443793359990269D+04, &
    2.637748787624195437963534D+04, &
    7.225813979700288197698961D+03 /)
  real ( kind = 8 ), dimension ( 8 ) :: p2 = (/ &
    4.974607845568932035012064D+00, &
    5.424138599891070494101986D+02, &
    1.550693864978364947665077D+04, &
    1.847932904445632425417223D+05, &
    1.088204769468828767498470D+06, &
    3.338152967987029735917223D+06, &
    5.106661678927352456275255D+06, &
    3.074109054850539556250927D+06 /)
  real ( kind = 8 ), dimension ( 8 ) :: p4 = (/ &
    1.474502166059939948905062D+04, &
    2.426813369486704502836312D+06, &
    1.214755574045093227939592D+08, &
    2.663432449630976949898078D+09, &
    2.940378956634553899906876D+10, &
    1.702665737765398868392998D+11, &
    4.926125793377430887588120D+11, &
    5.606251856223951465078242D+11 /)
  real ( kind = 8 ), dimension ( 8 ) :: q1 = (/ &
    6.748212550303777196073036D+01, &
    1.113332393857199323513008D+03, &
    7.738757056935398733233834D+03, &
    2.763987074403340708898585D+04, &
    5.499310206226157329794414D+04, &
    6.161122180066002127833352D+04, &
    3.635127591501940507276287D+04, &
    8.785536302431013170870835D+03 /)
  real ( kind = 8 ), dimension ( 8 ) :: q2 = (/ &
    1.830328399370592604055942D+02, &
    7.765049321445005871323047D+03, &
    1.331903827966074194402448D+05, &
    1.136705821321969608938755D+06, &
    5.267964117437946917577538D+06, &
    1.346701454311101692290052D+07, &
    1.782736530353274213975932D+07, &
    9.533095591844353613395747D+06 /)
  real ( kind = 8 ), dimension ( 8 ) :: q4 = (/ &
    2.690530175870899333379843D+03, &
    6.393885654300092398984238D+05, &
    4.135599930241388052042842D+07, &
    1.120872109616147941376570D+09, &
    1.488613728678813811542398D+10, &
    1.016803586272438228077304D+11, &
    3.417476345507377132798597D+11, &
    4.463158187419713286462081D+11 /)
  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) res
  real ( kind = 8 ), parameter :: sqrtpi = 0.9189385332046727417803297D+00
  real ( kind = 8 ) x
  real ( kind = 8 ), parameter :: xbig = 2.55D+305
  real ( kind = 8 ) xden
  real ( kind = 8 ), parameter :: xinf = 1.79D+308
  real ( kind = 8 ) xm1
  real ( kind = 8 ) xm2
  real ( kind = 8 ) xm4
  real ( kind = 8 ) xnum
  real ( kind = 8 ) y
  real ( kind = 8 ) ysq

  y = x

  if ( 0.0D+00 < y .and. y <= xbig ) then

    if ( y <= epsilon ( y ) ) then

      res = - log ( y )
!
!  EPS < X <= 1.5.
!
    else if ( y <= 1.5D+00 ) then

      if ( y < 0.6796875D+00 ) then
        corr = -log ( y )
        xm1 = y
      else
        corr = 0.0D+00
        xm1 = ( y - 0.5D+00 ) - 0.5D+00
      end if

      if ( y <= 0.5D+00 .or. 0.6796875D+00 <= y ) then

        xden = 1.0D+00
        xnum = 0.0D+00
        do i = 1, 8
          xnum = xnum * xm1 + p1(i)
          xden = xden * xm1 + q1(i)
        end do

        res = corr + ( xm1 * ( d1 + xm1 * ( xnum / xden ) ) )

      else

        xm2 = ( y - 0.5D+00 ) - 0.5D+00
        xden = 1.0D+00
        xnum = 0.0D+00
        do i = 1, 8
          xnum = xnum * xm2 + p2(i)
          xden = xden * xm2 + q2(i)
        end do

        res = corr + xm2 * ( d2 + xm2 * ( xnum / xden ) )

      end if
!
!  1.5 < X <= 4.0.
!
    else if ( y <= 4.0D+00 ) then

      xm2 = y - 2.0D+00
      xden = 1.0D+00
      xnum = 0.0D+00
      do i = 1, 8
        xnum = xnum * xm2 + p2(i)
        xden = xden * xm2 + q2(i)
      end do

      res = xm2 * ( d2 + xm2 * ( xnum / xden ) )
!
!  4.0 < X <= 12.0.
!
    else if ( y <= 12.0D+00 ) then

      xm4 = y - 4.0D+00
      xden = -1.0D+00
      xnum = 0.0D+00
      do i = 1, 8
        xnum = xnum * xm4 + p4(i)
        xden = xden * xm4 + q4(i)
      end do

      res = d4 + xm4 * ( xnum / xden )
!
!  Evaluate for 12 <= argument.
!
    else

      res = 0.0D+00

      if ( y <= frtbig ) then

        res = c(7)
        ysq = y * y

        do i = 1, 6
          res = res / ysq + c(i)
        end do

      end if

      res = res / y
      corr = log ( y )
      res = res + sqrtpi - 0.5D+00 * corr
      res = res + y * ( corr - 1.0D+00 )

    end if
!
!  Return for bad arguments.
!
  else

    res = xinf

  end if
!
!  Final adjustments and return.
!
  r8_gamma_log = res

  return
end
function r8_normal_01 ( seed )

!*****************************************************************************80
!
!! R8_NORMAL_01 returns a unit pseudonormal R8.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    Because this routine uses the Box Muller method, it requires pairs
!    of uniform random values to generate a pair of normal random values.
!    This means that on every other call, essentially, the input value of
!    SEED is ignored, since the code saves the second normal random value.
!
!    If you didn't know this, you might be confused since, usually, the
!    output of a random number generator can be completely controlled by
!    the input value of the SEED.  If I were more careful, I could rewrite
!    this routine so that it would distinguish between cases where the input
!    value of SEED is the output value from the previous call (all is well)
!    and those cases where it is not (the user has decided to do something
!    new.  Restart the uniform random number sequence.)  But I'll leave
!    that for later.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2006
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
!    Output, real ( kind = 8 ) R8_NORMAL_01, a sample of the standard
!    normal PDF.
!
  implicit none

  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_normal_01
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ), save :: seed2 = 0
  integer ( kind = 4 ), save :: used = 0
  real ( kind = 8 ) x
  real ( kind = 8 ), save :: y = 0.0D+00
!
!  On odd numbered calls, generate two uniforms, create two normals,
!  return the first normal and its corresponding seed.
!
  if ( mod ( used, 2 ) == 0 ) then

    r1 = r8_uniform_01 ( seed )
    seed2 = seed
    r2 = r8_uniform_01 ( seed2 )

    x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * pi * r2 )
    y = sqrt ( - 2.0D+00 * log ( r1 ) ) * sin ( 2.0D+00 * pi * r2 )
!
!  On odd calls, return the second normal and its corresponding seed.
!
  else

    seed = seed2
    x = y

  end if

  used = used + 1

  r8_normal_01 = x

  return
end
function r8_psi ( xx )

!*****************************************************************************80
!
!! R8_PSI evaluates the psi function.
!
!  Discussion:
!
!    This routine evaluates the logarithmic derivative of the
!    Gamma function,
!
!      PSI(X) = d/dX ( GAMMA(X) ) / GAMMA(X)
!             = d/dX LN ( GAMMA(X) )
!
!    for real X, where either
!
!      - XMAX1 < X < - XMIN, and X is not a negative integer,
!
!    or
!
!      XMIN < X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2008
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody, Anthony Strecok, Henry Thacher,
!    Chebyshev Approximations for the Psi Function,
!    Mathematics of Computation,
!    Volume 27, Number 121, January 1973, pages 123-127.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XX, the argument of the function.
!
!    Output, real ( kind = 8 ) R8_PSI, the value of the function.
!
  implicit none

  real ( kind = 8 ) aug
  real ( kind = 8 ) den
  real ( kind = 8 ), parameter :: fourth = 0.25D+00
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nq
  real ( kind = 8 ), dimension ( 9 ) :: p1 = (/ &
   4.5104681245762934160D-03, &
   5.4932855833000385356D+00, &
   3.7646693175929276856D+02, &
   7.9525490849151998065D+03, &
   7.1451595818951933210D+04, &
   3.0655976301987365674D+05, &
   6.3606997788964458797D+05, &
   5.8041312783537569993D+05, &
   1.6585695029761022321D+05 /)
  real ( kind = 8 ), dimension ( 7 ) :: p2 = (/ &
  -2.7103228277757834192D+00, &
  -1.5166271776896121383D+01, &
  -1.9784554148719218667D+01, &
  -8.8100958828312219821D+00, &
  -1.4479614616899842986D+00, &
  -7.3689600332394549911D-02, &
  -6.5135387732718171306D-21 /)
  real ( kind = 8 ), parameter :: piov4 = 0.78539816339744830962D+00
  real ( kind = 8 ), dimension ( 8 ) :: q1 = (/ &
   9.6141654774222358525D+01, &
   2.6287715790581193330D+03, &
   2.9862497022250277920D+04, &
   1.6206566091533671639D+05, &
   4.3487880712768329037D+05, &
   5.4256384537269993733D+05, &
   2.4242185002017985252D+05, &
   6.4155223783576225996D-08 /)
  real ( kind = 8 ), dimension ( 6 ) :: q2 = (/ &
   4.4992760373789365846D+01, &
   2.0240955312679931159D+02, &
   2.4736979003315290057D+02, &
   1.0742543875702278326D+02, &
   1.7463965060678569906D+01, &
   8.8427520398873480342D-01 /)
  real ( kind = 8 ) r8_psi
  real ( kind = 8 ) sgn
  real ( kind = 8 ), parameter :: three = 3.0D+00
  real ( kind = 8 ) upper
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ), parameter :: x01 = 187.0D+00
  real ( kind = 8 ), parameter :: x01d = 128.0D+00
  real ( kind = 8 ), parameter :: x02 = 6.9464496836234126266D-04
  real ( kind = 8 ), parameter :: xinf = 1.70D+38
  real ( kind = 8 ), parameter :: xlarge = 2.04D+15
  real ( kind = 8 ), parameter :: xmax1 = 3.60D+16
  real ( kind = 8 ), parameter :: xmin1 = 5.89D-39
  real ( kind = 8 ), parameter :: xsmall = 2.05D-09
  real ( kind = 8 ) xx
  real ( kind = 8 ) z

  x = xx
  w = abs ( x )
  aug = 0.0D+00
!
!  Check for valid arguments, then branch to appropriate algorithm.
!
  if ( xmax1 <= - x .or. w < xmin1 ) then

    if ( 0.0D+00 < x ) then
      r8_psi = - huge ( x )
    else
      r8_psi = huge ( x )
    end if

    return
  end if

  if ( x < 0.5D+00 ) then
!
!  X < 0.5, use reflection formula: psi(1-x) = psi(x) + pi * cot(pi*x)
!  Use 1/X for PI*COTAN(PI*X)  when  XMIN1 < |X| <= XSMALL.
!
    if ( w <= xsmall ) then

      aug = - 1.0D+00 / x
!
!  Argument reduction for cotangent.
!
    else

      if ( x < 0.0D+00 ) then
        sgn = piov4
      else
        sgn = - piov4
      end if

      w = w - real ( int ( w ), kind = 8 )
      nq = int ( w * 4.0D+00 )
      w = 4.0D+00 * ( w - real ( nq, kind = 8 ) * fourth )
!
!  W is now related to the fractional part of 4.0 * X.
!  Adjust argument to correspond to values in the first
!  quadrant and determine the sign.
!
      n = nq / 2

      if ( n + n /= nq ) then
        w = 1.0D+00 - w
      end if

      z = piov4 * w

      if ( mod ( n, 2 ) /= 0 ) then
        sgn = - sgn
      end if
!
!  Determine the final value for  -pi * cotan(pi*x).
!
      n = ( nq + 1 ) / 2
      if ( mod ( n, 2 ) == 0 ) then
!
!  Check for singularity.
!
        if ( z == 0.0D+00 ) then

          if ( 0.0D+00 < x ) then
            r8_psi = - huge ( x )
          else
            r8_psi = huge ( x )
          end if

          return
        end if

        aug = sgn * ( 4.0D+00 / tan ( z ) )

      else

        aug = sgn * ( 4.0D+00 * tan ( z ) )

      end if

    end if

    x = 1.0D+00 - x

  end if
!
!  0.5 <= X <= 3.0.
!
  if ( x <= three ) then

    den = x
    upper = p1(1) * x
    do i = 1, 7
      den = ( den + q1(i) ) * x
      upper = ( upper + p1(i+1) ) * x
    end do
    den = ( upper + p1(9) ) / ( den + q1(8) )
    x = ( x - x01 / x01d ) - x02
    r8_psi = den * x + aug
    return

  end if
!
!  3.0 < X.
!
  if ( x < xlarge ) then
    w = 1.0D+00 / ( x * x )
    den = w
    upper = p2(1) * w
    do i = 1, 5
      den = ( den + q2(i) ) * w
      upper = ( upper + p2(i+1) ) * w
    end do
    aug = ( upper + p2(7) ) / ( den + q2(6) ) - 0.5D+00 / x + aug
  end if

  r8_psi = aug + log ( x )

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
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_01 = real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
function r8_uniform_ab ( a, b, seed )

!*****************************************************************************80
!
!! R8_UNIFORM_AB returns a scaled pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer variable.
!
!    The pseudorandom number should be uniformly distributed
!    between A and B.
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
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_AB, a number strictly between A and B.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_ab = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
subroutine r8col_mean ( m, n, a, mean )

!*****************************************************************************80
!
!! R8COL_MEAN returns the column means of an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!  Example:
!
!    A =
!      1  2  3
!      2  6  7
!
!    MEAN =
!      1.5  4.0  5.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), the array to be examined.
!
!    Output, real ( kind = 8 ) MEAN(N), the means, or averages, of the columns.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean(n)

  do j = 1, n
    mean(j) = sum ( a(1:m,j) )
  end do

  mean(1:n) = mean(1:n) / real ( m, kind = 8  )

  return
end
subroutine r8col_variance ( m, n, a, variance )

!*****************************************************************************80
!
!! R8COL_VARIANCE returns the variances of an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in
!    the array.
!
!    Input, real ( kind = 8 ) A(M,N), the array whose variances are desired.
!
!    Output, real ( kind = 8 ) VARIANCE(N), the variances of the rows.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean
  real ( kind = 8 ) variance(n)

  do j = 1, n

    mean = sum ( a(1:m,j) ) / real ( m, kind = 8  )

    variance(j) = 0.0D+00
    do i = 1, m
      variance(j) = variance(j) + ( a(i,j) - mean )**2
    end do

    if ( 1 < m ) then
      variance(j) = variance(j) / real ( m - 1, kind = 8 )
    else
      variance(j) = 0.0D+00
    end if

  end do

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
function trigamma ( x )

!*****************************************************************************80
!
!! TRIGAMMA calculates trigamma(x) = d^2 log(Gamma(x)) / dx^2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    Original FORTRAN77 version by B Schneider.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    B Schneider,
!    Algorithm AS 121:
!    Trigamma Function,
!    Applied Statistics,
!    Volume 27, Number 1, 1978, page 97-99.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the trigamma function.
!    0 < X.
!
!    Output, real ( kind = 8 ) TRIGAMMA, the value of the trigamma function 
!    at X.
!
  implicit none

  real ( kind = 8 ), parameter :: a = 0.0001D+00
  real ( kind = 8 ), parameter :: b = 5.0D+00
  real ( kind = 8 ), parameter :: b2 =   1.0D+00 / 6.0D+00
  real ( kind = 8 ), parameter :: b4 = - 1.0D+00 / 30.0D+00
  real ( kind = 8 ), parameter :: b6 =   1.0D+00 / 42.0D+00
  real ( kind = 8 ), parameter :: b8 = - 1.0D+00 / 30.0D+00
  real ( kind = 8 ) trigamma
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z
!
!  1): If X is not positive, fail.
!
  if ( x <= 0.0D+00 ) then

    trigamma = 0.0D+00
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRIGAMNA - Fatal error!'
    write ( *, '(a)' ) '  X <= 0.'
    stop
!
!  2): If X is smaller than A, use a small value approximation.
!
  else if ( x <= a ) then

    trigamma = 1.0D+00 / x / x
!
!  3): Otherwise, increase the argument to B <+ ( X + I ).
!
  else

    z = x
    trigamma = 0.0D+00

    do while ( z < b )
      trigamma = trigamma + 1.0D+00 / z**2
      z = z + 1.0D+00
    end do
!
!  ...and then apply an asymptotic formula.
!
    y = 1.0D+00 / z / z

    trigamma = trigamma + 0.5D+00 * &
           y + ( 1.0D+00 &
         + y * ( b2 &
         + y * ( b4 &
         + y * ( b6 &
          + y *  b8 )))) / z

  end if

  return
end
