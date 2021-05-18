function genbet ( aa, bb )

!*****************************************************************************80
!
!! GENBET generates a beta random deviate.
!
!  Discussion:
!
!    This procedure returns a single random deviate from the beta distribution
!    with parameters A and B.  The density is
!
!      x^(a-1) * (1-x)^(b-1) / Beta(a,b) for 0 < x < 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2014
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Russell Cheng,
!    Generating Beta Variates with Nonintegral Shape Parameters,
!    Communications of the ACM,
!    Volume 21, Number 4, April 1978, pages 317-322.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) AA, the first parameter of the beta distribution.
!    0.0 < AA.
!
!    Input, real ( kind = 4 ) BB, the second parameter of the beta distribution.
!    0.0 < BB.
!
!    Output, real ( kind = 4 ) GENBET, a beta random variate.
!
  implicit none

  real ( kind = 4 ) a
  real ( kind = 4 ) aa
  real ( kind = 4 ) alpha
  real ( kind = 4 ) b
  real ( kind = 4 ) bb
  real ( kind = 4 ) beta
  real ( kind = 4 ) delta
  real ( kind = 4 ) gamma
  real ( kind = 4 ) genbet
  real ( kind = 4 ) k1
  real ( kind = 4 ) k2
  real ( kind = 4 ), parameter :: log4 = 1.3862943611198906188E+00
  real ( kind = 4 ), parameter :: log5 = 1.6094379124341003746E+00
  real ( kind = 4 ) r
  real ( kind = 4 ) r4_exp
  real ( kind = 4 ) r4_uni_01
  real ( kind = 4 ) s
  real ( kind = 4 ) t
  real ( kind = 4 ) u1
  real ( kind = 4 ) u2
  real ( kind = 4 ) v
  real ( kind = 4 ) w
  real ( kind = 4 ) y
  real ( kind = 4 ) z

  if ( aa <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENBET - Fatal error!'
    write ( *, '(a)' ) '  AA <= 0.0'
    stop 1
  end if

  if ( bb <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENBET - Fatal error!'
    write ( *, '(a)' ) '  BB <= 0.0'
    stop 1
  end if
!
!  Algorithm BB
!
  if ( 1.0E+00 < aa .and. 1.0E+00 < bb ) then

    a = min ( aa, bb )
    b = max ( aa, bb )
    alpha = a + b
    beta = sqrt ( ( alpha - 2.0E+00 ) / ( 2.0E+00 * a * b - alpha ) )
    gamma = a + 1.0E+00 / beta

    do

      u1 = r4_uni_01 ( )
      u2 = r4_uni_01 ( )
      v = beta * log ( u1 / ( 1.0E+00 - u1 ) )
!
!  exp ( v ) replaced by r4_exp ( v )
!
      w = a * r4_exp ( v )

      z = u1 ** 2 * u2
      r = gamma * v - log4
      s = a + r - w

      if ( 5.0E+00 * z <= s + 1.0E+00 + log5 ) then
        exit
      end if

      t = log ( z )
      if ( t <= s ) then
        exit
      end if

      if ( t <= ( r + alpha * log ( alpha / ( b + w ) ) ) ) then
        exit
      end if

    end do
!
!  Algorithm BC
!
  else

    a = max ( aa, bb )
    b = min ( aa, bb )
    alpha = a + b
    beta = 1.0E+00 / b
    delta = 1.0E+00 + a - b
    k1 = delta * ( 1.0E+00 / 72.0E+00 + b / 24.0E+00 ) &
      / ( a / b - 7.0E+00 / 9.0E+00 )
    k2 = 0.25E+00 + ( 0.5E+00 + 0.25E+00 / delta ) * b

    do

      u1 = r4_uni_01 ( )
      u2 = r4_uni_01 ( )

      if ( u1 < 0.5E+00 ) then

        y = u1 * u2
        z = u1 * y

        if ( k1 <= 0.25E+00 * u2 + z - y ) then
          cycle
        end if

      else

        z = u1 ** 2 * u2

        if ( z <= 0.25E+00 ) then

          v = beta * log ( u1 / ( 1.0E+00 - u1 ) )
          w = a * exp ( v )

          if ( aa == a ) then
            genbet = w / ( b + w )
          else
            genbet = b / ( b + w )
          end if

          return

        end if

        if ( k2 < z ) then
          cycle
        end if

      end if

      v = beta * log ( u1 / ( 1.0E+00 - u1 ) )
      w = a * exp ( v )

      if ( log ( z ) <= alpha * ( log ( alpha / ( b + w ) ) + v ) - log4 ) then
        exit
      end if

    end do

  end if

  if ( aa == a ) then
    genbet = w / ( b + w )
  else
    genbet = b / ( b + w )
  end if

  return
end
function genchi ( df )

!*****************************************************************************80
!
!! GENCHI generates a Chi-Square random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the chi square distribution
!    with DF degrees of freedom random variable.
!
!    The algorithm exploits the relation between chisquare and gamma.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Output, real ( kind = 4 ) GENCHI, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) arg1
  real ( kind = 4 ) arg2
  real ( kind = 4 ) df
  real ( kind = 4 ) genchi
  real ( kind = 4 ) gengam

  if ( df <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENCHI - Fatal error!'
    write ( *, '(a)' ) '  DF <= 0.'
    write ( *, '(a,g14.6)' ) '  Value of DF: ', df
    stop 1
  end if

  arg1 = 1.0E+00
  arg2 = df / 2.0E+00

  genchi = 2.0E+00 * gengam ( arg1, arg2 )

  return
end
function genexp ( av )

!*****************************************************************************80
!
!! GENEXP generates an exponential random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from an exponential
!    distribution with mean AV.
!
!    See also the function R4_EXPONENTIAL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Computer Methods for Sampling From the
!    Exponential and Normal Distributions,
!    Communications of the ACM,
!    Volume 15, Number 10, October 1972, pages 873-882.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) AV, the mean of the exponential distribution 
!    from which a random deviate is to be generated.
!
!    Output, real ( kind = 4 ) GENEXP, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) av
  real ( kind = 4 ) genexp
  real ( kind = 4 ) sexpo

  genexp = sexpo ( ) * av

  return
end
function genf ( dfn, dfd )

!*****************************************************************************80
!
!! GENF generates an F random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the F (variance ratio)
!    distribution with DFN degrees of freedom in the numerator
!    and DFD degrees of freedom in the denominator.
!
!    It directly generates the ratio of chisquare variates
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) DFN, the numerator degrees of freedom.
!    0.0 < DFN.
!
!    Input, real ( kind = 4 ) DFD, the denominator degrees of freedom.
!    0.0 < DFD.
!
!    Output, real ( kind = 4 ) GENF, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) dfd
  real ( kind = 4 ) dfn
  real ( kind = 4 ) genchi
  real ( kind = 4 ) genf
  real ( kind = 4 ) xden
  real ( kind = 4 ) xnum

  if ( dfn <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENF - Fatal error!'
    write ( *, '(a)' ) '  DFN <= 0.0'
    stop 1
  end if

  if ( dfd <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENF - Fatal error!'
    write ( *, '(a)' ) '  DFD <= 0.0'
    stop 1
  end if

  xnum = genchi ( dfn ) / dfn
  xden = genchi ( dfd ) / dfd
  genf = xnum / xden

  return
end
function gengam ( a, r )

!*****************************************************************************80
!
!! GENGAM generates a Gamma random deviate.
!
!  Discussion:
!
!    This procedure generates random deviates from the gamma distribution whose
!    density is (A^R)/Gamma(R) * X^(R-1) * Exp(-A*X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Generating Gamma Variates by a Modified Rejection Technique,
!    Communications of the ACM,
!    Volume 25, Number 1, January 1982, pages 47-54.
!
!    Joachim Ahrens, Ulrich Dieter,
!    Computer Methods for Sampling from Gamma, Beta, Poisson and
!    Binomial Distributions,
!    Computing,
!    Volume 12, Number 3, September 1974, pages 223-246.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) A, the location parameter.
!
!    Input, real ( kind = 4 ) R, the shape parameter.
!
!    Output, real ( kind = 4 ) GENGAM, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) a
  real ( kind = 4 ) gengam
  real ( kind = 4 ) r
  real ( kind = 4 ) sgamma

  gengam = sgamma ( r ) / a

  return
end
subroutine genmn ( parm, x, work )

!*****************************************************************************80
!
!! GENMN generates a multivariate normal deviate.
!
!  Discussion:
!
!    The method is:
!    1) Generate P independent standard normal deviates - Ei ~ N(0,1)
!    2) Using Cholesky decomposition find A so that A'*A = COVM
!    3) A' * E + MEANV ~ N(MEANV,COVM)
!
!    Note that PARM contains information needed to generate the
!    deviates, and is set up by SETGMN.
!
!    PARM(1) contains the size of the deviates, P
!    PARM(2:P+1) contains the mean vector.
!    PARM(P+2:P*(P+3)/2+1) contains the upper half of the Cholesky
!    decomposition of the covariance matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) PARM(P*(P+3)/2+1), parameters set by SETGMN.
!
!    Output, real ( kind = 4 ) X(P), a random deviate from the distribution.
!
!    Workspace, real ( kind = 4 ) WORK(P).
!
  implicit none

  real ( kind = 4 ) ae
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icount
  integer ( kind = 4 ) j
  integer ( kind = 4 ) p
  real ( kind = 4 ) parm(*)
  real ( kind = 4 ) snorm
  real ( kind = 4 ) work(*)
  real ( kind = 4 ) x(*)

  p = int ( parm(1) )
!
!  Generate P independent normal deviates.
!
  do i = 1, p
    work(i) = snorm ( )
  end do
!
!  Compute X = MEANV + A' * WORK
!
  do i = 1, p
    icount = 0
    ae = 0.0E+00
    do j = 1, i
      icount = icount + j - 1
      ae = ae + parm(i+(j-1)*p-icount+p+1) * work(j)
    end do

    x(i) = ae + parm(i+1)

  end do

  return
end
subroutine genmul ( n, p, ncat, ix )

!*****************************************************************************80
!
!! GENMUL generates a multinomial random deviate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer, 1986,
!    ISBN: 0387963057,
!    LC: QA274.D48.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of events, which will be
!    classified into one of the NCAT categories.
!
!    Input, real ( kind = 4 ) P(NCAT-1).  P(I) is the probability that an event
!    will be classified into category I.  Thus, each P(I) must be between 
!    0.0 and 1.0.  Only the first NCAT-1 values of P must be defined since 
!    P(NCAT) would be 1.0 minus the sum of the first NCAT-1 P's.
!
!    Input, integer ( kind = 4 ) NCAT, the number of categories.
!
!    Output, integer ( kind = 4 ) IX(NCAT), a random observation from 
!    the multinomial distribution.  All IX(i) will be nonnegative and their 
!    sum will be N.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncat

  integer ( kind = 4 ) i
  integer ( kind = 4 ) icat
  integer ( kind = 4 ) ignbin
  integer ( kind = 4 ) ix(ncat)
  integer ( kind = 4 ) ntot
  real ( kind = 4 ) p(ncat-1)
  real ( kind = 4 ) prob
  real ( kind = 4 ) ptot

  if ( n < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENMUL - Fatal error!'
    write ( *, '(a)' ) '  N < 0'
    stop 1
  end if

  if ( ncat <= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENMUL - Fatal error!'
    write ( *, '(a)' ) '  NCAT <= 1'
    stop 1
  end if

  do i = 1, ncat - 1

    if ( p(i) < 0.0E+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GENMUL - Fatal error!'
      write ( *, '(a)' ) '  Some P(i) < 0.'
      stop 1
    end if

    if ( 1.0E+00 < p(i) ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GENMUL - Fatal error!'
      write ( *, '(a)' ) '  Some 1 < P(i).'
      stop 1
    end if

  end do

  ptot = 0.0E+00
  do i = 1, ncat - 1
    ptot = ptot + p(i)
  end do

  if ( 0.99999E+00 < ptot ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENMUL - Fatal error!'
    write ( *, '(a)' ) '  1 < Sum of P().'
    stop 1
  end if
!
!  Initialize variables.
!
  ntot = n
  ptot = 1.0E+00
  do i = 1, ncat
    ix(i) = 0
  end do
!
!  Generate the observation.
!
  do icat = 1, ncat - 1
    prob = p(icat) / ptot
    ix(icat) = ignbin ( ntot, prob )
    ntot = ntot - ix(icat)
    if ( ntot <= 0 ) then
      return
    end if
    ptot = ptot - p(icat)
  end do

  ix(ncat) = ntot

  return
end
function gennch ( df, xnonc )

!*****************************************************************************80
!
!! GENNCH generates a noncentral Chi-Square random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the  distribution of a
!    noncentral chisquare with DF degrees of freedom and noncentrality parameter
!    XNONC.
!
!    It uses the fact that the noncentral chisquare is the sum of a chisquare
!    deviate with DF-1 degrees of freedom plus the square of a normal
!    deviate with mean XNONC and standard deviation 1.
!
!    A subtle ambiguity arises in the original formulation:
!
!      gennch = genchi ( arg1 ) + ( gennor ( arg2, arg3 ) ) ^ 2
!
!    because the compiler is free to invoke either genchi or gennor
!    first, both of which alter the random number generator state,
!    resulting in two distinct possible results.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) DF, the degrees of freedom.
!    1.0 < DF.
!
!    Input, real ( kind = 4 ) XNONC, the noncentrality parameter.
!    0.0 <= XNONC.
!
!    Output, real ( kind = 4 ) GENNCH, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) arg1
  real ( kind = 4 ) arg2
  real ( kind = 4 ) arg3
  real ( kind = 4 ) df
  real ( kind = 4 ) genchi
  real ( kind = 4 ) gennch
  real ( kind = 4 ) gennor
  real ( kind = 4 ) t1
  real ( kind = 4 ) t2
  real ( kind = 4 ) xnonc

  if ( df <= 1.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENNCH - Fatal error!'
    write ( *, '(a)' ) '  DF <= 1.'
    stop 1
  end if

  if ( xnonc < 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENNCH - Fatal error!'
    write ( *, '(a)' ) '  XNONC < 0.0.'
    stop 1
  end if

  arg1 = df - 1.0E+00
  arg2 = sqrt ( xnonc )
  arg3 = 1.0E+00

  t1 = genchi ( arg1 )
  t2 = gennor ( arg2, arg3 )

  gennch = t1 + t2 * t2

  return
end
function gennf ( dfn, dfd, xnonc )

!*****************************************************************************80
!
!! GENNF generates a noncentral F random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the noncentral F
!    (variance ratio) distribution with DFN degrees of freedom in the
!    numerator, and DFD degrees of freedom in the denominator, and
!    noncentrality parameter XNONC.
!
!    It directly generates the ratio of noncentral numerator chisquare variate
!    to central denominator chisquare variate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) DFN, the numerator degrees of freedom.
!    1.0 < DFN.
!
!    Input, real ( kind = 4 ) DFD, the denominator degrees of freedom.
!    0.0 < DFD.
!
!    Input, real ( kind = 4 ) XNONC, the noncentrality parameter.
!    0.0 <= XNONC.
!
!    Output, real ( kind = 4 ) GENNF, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) dfd
  real ( kind = 4 ) dfn
  real ( kind = 4 ) genchi
  real ( kind = 4 ) gennch
  real ( kind = 4 ) gennf
  real ( kind = 4 ) xden
  real ( kind = 4 ) xnonc
  real ( kind = 4 ) xnum

  if ( dfn <= 1.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENNF - Fatal error!'
    write ( *, '(a)' ) '  DFN <= 1.0'
    stop 1
  end if

  if ( dfd <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENNF - Fatal error!'
    write ( *, '(a)' ) '  DFD <= 0.0'
    stop 1
  end if

  if ( xnonc < 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENNF - Fatal error!'
    write ( *, '(a)' ) '  XNONC < 0.0'
    stop 1
  end if

  xnum = gennch ( dfn, xnonc ) / dfn
  xden = genchi ( dfd ) / dfd

  gennf = xnum / xden

  return
end
function gennor ( av, sd )

!*****************************************************************************80
!
!! GENNOR generates a normal random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a normal distribution
!    with mean AV, and standard deviation SD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Extensions of Forsythe's Method for Random
!    Sampling from the Normal Distribution,
!    Mathematics of Computation,
!    Volume 27, Number 124, October 1973, page 927-937.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) AV, the mean.
!
!    Input, real ( kind = 4 ) SD, the standard deviation.
!
!    Output, real ( kind = 4 ) GENNOR, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) av
  real ( kind = 4 ) gennor
  real ( kind = 4 ) sd
  real ( kind = 4 ) snorm

  gennor = sd * snorm ( ) + av

  return
end
subroutine genprm ( iarray, n )

!*****************************************************************************80
!
!! GENPRM generates and applies a random permutation to an array.
!
!  Discussion:
!
!    To see the permutation explicitly, let the input array be
!    1, 2, ..., N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) IARRAY(N), an array to be permuted.
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) iarray(n)
  integer ( kind = 4 ) ignuin
  integer ( kind = 4 ) itmp
  integer ( kind = 4 ) iwhich

  do i = 1, n
    iwhich = ignuin ( i, n )
    itmp = iarray(iwhich)
    iarray(iwhich) = iarray(i)
    iarray(i) = itmp
  end do

  return
end
function genunf ( low, high )

!*****************************************************************************80
!
!! GENUNF generates a uniform random deviate.
!
!  Discussion:
!
!    This procedure generates a real deviate uniformly distributed between
!    LOW and HIGH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) LOW, HIGH, the lower and upper bounds.
!
!    Output, real ( kind = 4 ) GENUNF, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  real ( kind = 4 ) low
  real ( kind = 4 ) r4_uni_01

  genunf = low + ( high - low ) * r4_uni_01 ( )

  return
end
function ignbin ( n, pp )

!*****************************************************************************80
!
!! IGNBIN generates a binomial random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a binomial
!    distribution whose number of trials is N and whose
!    probability of an event in each trial is P.
!
!    The previous version of this program relied on the assumption that
!    local memory would be preserved between calls.  It set up data
!    one time to be preserved for use over multiple calls.  In the
!    interests of portability, this assumption has been removed, and
!    the "setup" data is recomputed on every call.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Voratas Kachitvichyanukul, Bruce Schmeiser,
!    Binomial Random Variate Generation,
!    Communications of the ACM,
!    Volume 31, Number 2, February 1988, pages 216-222.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of binomial trials, from which a
!    random deviate will be generated.
!    0 < N.
!
!    Input, real ( kind = 4 ) PP, the probability of an event in each trial of
!    the binomial distribution from which a random deviate is to be generated.
!    0.0 < PP < 1.0.
!
!    Output, integer ( kind = 4 ) IGNBIN, a random deviate from the
!    distribution.
!
  implicit none

  real ( kind = 4 ) al
  real ( kind = 4 ) alv
  real ( kind = 4 ) amaxp
  real ( kind = 4 ) c
  real ( kind = 4 ) f
  real ( kind = 4 ) f1
  real ( kind = 4 ) f2
  real ( kind = 4 ) ffm
  real ( kind = 4 ) fm
  real ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ignbin
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) ix1
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mp
  real ( kind = 4 ) pp
  integer ( kind = 4 ) n
  real ( kind = 4 ) p
  real ( kind = 4 ) p1
  real ( kind = 4 ) p2
  real ( kind = 4 ) p3
  real ( kind = 4 ) p4
  real ( kind = 4 ) q
  real ( kind = 4 ) qn
  real ( kind = 4 ) r
  real ( kind = 4 ) r4_uni_01
  real ( kind = 4 ) t
  real ( kind = 4 ) u
  real ( kind = 4 ) v
  real ( kind = 4 ) w
  real ( kind = 4 ) w2
  real ( kind = 4 ) x
  real ( kind = 4 ) x1
  real ( kind = 4 ) x2
  real ( kind = 4 ) xl
  real ( kind = 4 ) xll
  real ( kind = 4 ) xlr
  real ( kind = 4 ) xm
  real ( kind = 4 ) xnp
  real ( kind = 4 ) xnpq
  real ( kind = 4 ) xr
  real ( kind = 4 ) ynorm
  real ( kind = 4 ) z
  real ( kind = 4 ) z2

  if ( pp <= 0.0E+00 .or. 1.0E+00 <= pp ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNBIN - Fatal error!'
    write ( *, '(a)' ) '  PP is out of range.'
    stop 1
  end if

  p = min ( pp, 1.0E+00 - pp )
  q = 1.0E+00 - p
  xnp = real ( n, kind = 4 ) * p

  if ( xnp < 30.0E+00 ) then

    qn = q ** n
    r = p / q
    g = r * real ( n + 1, kind = 4 )

    do

      ix = 0
      f = qn
      u = r4_uni_01 ( )

      do

        if ( u < f ) then
          if ( 0.5E+00 < pp ) then
            ix = n - ix
          end if
          ignbin = ix
          return
        end if

        if ( 110 < ix ) then
          exit
        end if

        u = u - f
        ix = ix + 1
        f = f * ( g / real ( ix, kind = 4 ) - r )

      end do

    end do

  end if

  ffm = xnp + p
  m = int ( ffm )
  fm = m
  xnpq = xnp * q
  p1 = int ( 2.195E+00 * sqrt ( xnpq ) - 4.6E+00 * q ) + 0.5E+00
  xm = fm + 0.5E+00
  xl = xm - p1
  xr = xm + p1
  c = 0.134E+00 + 20.5E+00 / ( 15.3E+00 + fm )
  al = ( ffm - xl ) / ( ffm - xl * p )
  xll = al * ( 1.0E+00 + 0.5E+00 * al )
  al = ( xr - ffm ) / ( xr * q )
  xlr = al * ( 1.0E+00 + 0.5E+00 * al )
  p2 = p1 * ( 1.0E+00 + c + c )
  p3 = p2 + c / xll
  p4 = p3 + c / xlr
!
!  Generate a variate.
!
  do

    u = r4_uni_01 ( ) * p4
    v = r4_uni_01 ( )
!
!  Triangle
!
    if ( u < p1 ) then
      ix = int ( xm - p1 * v + u )
      if ( 0.5E+00 < pp ) then
        ix = n - ix
      end if
      ignbin = ix
      return
    end if
!
!  Parallelogram
!
    if ( u <= p2 ) then

      x = xl + ( u - p1 ) / c
      v = v * c + 1.0E+00 - abs ( xm - x ) / p1

      if ( v <= 0.0E+00 .or. 1.0E+00 < v ) then
        cycle
      end if

      ix = int ( x )

    else if ( u <= p3 ) then

      ix = int ( xl + log ( v ) / xll )
      if ( ix < 0 ) then
        cycle
      end if
      v = v * ( u - p2 ) * xll

    else

      ix = int ( xr - log ( v ) / xlr )
      if ( n < ix ) then
        cycle
      end if
      v = v * ( u - p3 ) * xlr

    end if

    k = abs ( ix - m )

    if ( k <= 20 .or. xnpq / 2.0 - 1.0 <= k ) then

      f = 1.0E+00
      r = p / q
      g = ( n + 1 ) * r

      if ( m < ix ) then
        mp = m + 1
        do i = m + 1, ix
          f = f * ( g / i - r )
        end do
      else if ( ix < m ) then
        ix1 = ix + 1
        do i = ix + 1, m
          f = f / ( g / real ( i, kind = 4 ) - r )
        end do
      end if

      if ( v <= f ) then
        if ( 0.5E+00 < pp ) then
          ix = n - ix
        end if
        ignbin = ix
        return
      end if

    else

      amaxp = ( k / xnpq ) * ( ( k * ( k / 3.0E+00 &
        + 0.625E+00 ) + 0.1666666666666E+00 ) / xnpq + 0.5E+00 )
      ynorm = - real ( k * k, kind = 4 ) / ( 2.0E+00 * xnpq )
      alv = log ( v )

      if ( alv < ynorm - amaxp ) then
        if ( 0.5E+00 < pp ) then
          ix = n - ix
        end if
        ignbin = ix
        return
      end if

      if ( ynorm + amaxp < alv ) then
        cycle
      end if

      x1 = real ( ix + 1, kind = 4 )
      f1 = fm + 1.0E+00
      z = real ( n + 1, kind = 4 ) - fm
      w = real ( n - ix + 1, kind = 4 )
      z2 = z * z
      x2 = x1 * x1
      f2 = f1 * f1
      w2 = w * w

      t = xm * log ( f1 / x1 ) + ( n - m + 0.5E+00 ) * log ( z / w ) &
        + real ( ix - m, kind = 4 ) * log ( w * p / ( x1 * q ) ) &
        + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 - ( 99.0E+00 - 140.0E+00 &
        / f2 ) / f2 ) / f2 ) / f2 ) / f1 / 166320.0E+00 &
        + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 - ( 99.0E+00 - 140.0E+00 &
        / z2 ) / z2 ) / z2 ) / z2 ) / z / 166320.0E+00 &
        + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 - ( 99.0E+00 - 140.0E+00 &
        / x2 ) / x2 ) / x2 ) / x2 ) / x1 / 166320.0E+00 &
        + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 - ( 99.0E+00 - 140.0E+00 &
        / w2 ) / w2 ) / w2 ) / w2 ) / w / 166320.0E+00

      if ( alv <= t ) then
        if ( 0.5E+00 < pp ) then
          ix = n - ix
        end if
        ignbin = ix
        return
      end if

    end if

  end do

  return
end
function ignnbn ( n, p )

!*****************************************************************************80
!
!! IGNNBN generates a negative binomial random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a negative binomial
!    distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer, 1986,
!    ISBN: 0387963057,
!    LC: QA274.D48.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the required number of events.
!    0 <= N.
!
!    Input, real ( kind = 4 ) P, the probability of an event during a 
!    Bernoulli trial.  0.0 < P < 1.0.
!
!    Output, integer ( kind = 4 ) IGNNBN, a random deviate from 
!    the distribution.
!
  implicit none

  real ( kind = 4 ) a
  real ( kind = 4 ) gengam
  integer ( kind = 4 ) ignnbn
  integer ( kind = 4 ) ignpoi
  integer ( kind = 4 ) n
  real ( kind = 4 ) p
  real ( kind = 4 ) r
  real ( kind = 4 ) y

  if ( n < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNNBN - Fatal error!'
    write ( *, '(a)' ) '  N < 0.'
    stop 1
  end if

  if ( p <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNNBN - Fatal error!'
    write ( *, '(a)' ) '  P <= 0.0'
    stop 1
  end if

  if ( 1.0E+00 <= p ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNNBN - Fatal error!'
    write ( *, '(a)' ) '  1.0 <= P'
    stop 1
  end if
!
!  Generate Y, a random gamma (n,(1-p)/p) variable.
!
  r = real ( n )
  a = p / ( 1.0E+00 - p )
  y = gengam ( a, r )
!
!  Generate a random Poisson ( y ) variable.
!
  ignnbn = ignpoi ( y )

  return
end
function ignpoi ( mu )

!*****************************************************************************80
!
!! IGNPOI generates a Poisson random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a Poisson
!    distribution with given mean.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2018
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Computer Generation of Poisson Deviates
!    From Modified Normal Distributions,
!    ACM Transactions on Mathematical Software,
!    Volume 8, Number 2, June 1982, pages 163-179.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) MU, the mean of the Poisson distribution 
!    from which a random deviate is to be generated.
!
!    Output, integer ( kind = 4 ) IGNPOI, a random deviate from
!    the distribution.
!
  implicit none

  real ( kind = 4 ), parameter :: a0 = -0.5E+00
  real ( kind = 4 ), parameter :: a1 =  0.3333333E+00
  real ( kind = 4 ), parameter :: a2 = -0.2500068E+00
  real ( kind = 4 ), parameter :: a3 =  0.2000118E+00
  real ( kind = 4 ), parameter :: a4 = -0.1661269E+00
  real ( kind = 4 ), parameter :: a5 =  0.1421878E+00
  real ( kind = 4 ), parameter :: a6 = -0.1384794E+00
  real ( kind = 4 ), parameter :: a7 =  0.1250060E+00
  real ( kind = 4 ) b1
  real ( kind = 4 ) b2
  real ( kind = 4 ) c
  real ( kind = 4 ) c0
  real ( kind = 4 ) c1
  real ( kind = 4 ) c2
  real ( kind = 4 ) c3
  real ( kind = 4 ) d
  real ( kind = 4 ) del
  real ( kind = 4 ) difmuk
  real ( kind = 4 ) e
  real ( kind = 4 ) fact(10)
  real ( kind = 4 ) fk
  real ( kind = 4 ) fx
  real ( kind = 4 ) fy
  real ( kind = 4 ) g
  integer ( kind = 4 ) ignpoi
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kflag
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  real ( kind = 4 ) mu
  real ( kind = 4 ) omega
  real ( kind = 4 ) p
  real ( kind = 4 ) p0
  real ( kind = 4 ) px
  real ( kind = 4 ) py
  real ( kind = 4 ) q
  real ( kind = 4 ) r4_uni_01
  real ( kind = 4 ) s
  real ( kind = 4 ) sexpo
  real ( kind = 4 ) snorm
  real ( kind = 4 ) t
  real ( kind = 4 ) u
  real ( kind = 4 ) v
  real ( kind = 4 ) x
  real ( kind = 4 ) xx

  save fact

  data fact / 1.0E+00, 1.0E+00, 2.0E+00, 6.0E+00, 24.0E+00, &
    120.0E+00, 720.0E+00, 5040.0E+00, 40320.0E+00, 362880.0E+00 /
!
!  MU < 10
!
  if ( mu < 10.0E+00 ) then

    m = max ( 1, int ( mu ) )
    l = 0
    p = exp ( - mu )
    q = p
    p0 = p
!
!  Uniform sample for inversion method.
!
    do

      u = r4_uni_01 ( )
      ignpoi = 0

      if ( u <= p0 ) then
        return
      end if
!
!  Creation of new Poisson probabilities.
!
      do k = 1, 35
        p = p * mu / real ( k )
        q = q + p
        if ( u <= q ) then
          ignpoi = k
          return
        end if
      end do

    end do
!
!  10 <= MU
!
  else

    s = sqrt ( mu )
    d = 6.0E+00 * mu * mu
    l = int ( mu - 1.1484E+00 )
!
!  Normal sample.
!
    g = mu + s * snorm ( )

    if ( 0.0E+00 <= g ) then

      ignpoi = int ( g )
!
!  Immediate acceptance if large enough.
!
      if ( l <= ignpoi ) then
        return
      end if
!
!  Squeeze acceptance.
!
      fk = real ( ignpoi )
      difmuk = mu - fk
      u = r4_uni_01 ( )

      if ( difmuk * difmuk * difmuk <= d * u ) then
        return
      end if

    end if
!
!  Preparation for steps P and Q.
!
    omega = 0.3989423E+00 / s
    b1 = 0.04166667E+00 / mu
    b2 = 0.3E+00 * b1 * b1
    c3 = 0.1428571E+00 * b1 * b2
    c2 = b2 - 15.0E+00 * c3
    c1 = b1 - 6.0E+00 * b2 + 45.0E+00 * c3
    c0 = 1.0E+00 - b1 + 3.0E+00 * b2 - 15.0E+00 * c3
    c = 0.1069E+00 / mu

    if ( 0.0E+00 <= g ) then

      kflag = 0

      if ( ignpoi < 10 ) then

        px = - mu
        py = mu ** ignpoi / fact(ignpoi+1)

      else

        del = 0.8333333E-01 / fk
        del = del - 4.8E+00 * del * del * del
        v = difmuk / fk

        if ( 0.25E+00 < abs ( v ) ) then
          px = fk * log ( 1.0E+00 + v ) - difmuk - del
        else
          px = fk * v * v * ((((((( a7 &
            * v + a6 ) &
            * v + a5 ) &
            * v + a4 ) &
            * v + a3 ) &
            * v + a2 ) &
            * v + a1 ) &
            * v + a0 ) - del
        end if

        py = 0.3989423E+00 / sqrt ( fk )

      end if

      x = ( 0.5E+00 - difmuk ) / s
      xx = x * x
      fx = -0.5E+00 * xx
      fy = omega * ((( c3 * xx + c2 ) * xx + c1 ) * xx + c0 )

      if ( fy - u * fy <= py * exp ( px - fx ) ) then
        return
      end if

    end if
!
!  Exponential sample.
!
    do

      e = sexpo ( )
      u = 2.0E+00 * r4_uni_01 ( ) - 1.0E+00
      if ( u < 0.0E+00 ) then
        t = 1.8E+00 - abs ( e )
      else
        t = 1.8E+00 + abs ( e )
      end if

      if ( t <= -0.6744E+00 ) then
        cycle
      end if

      ignpoi = int ( mu + s * t )
      fk = real ( ignpoi )
      difmuk = mu - fk

      kflag = 1
!
!  Calculation of PX, PY, FX, FY.
!
      if ( ignpoi < 10 ) then

        px = -mu
        py = mu ** ignpoi / fact(ignpoi+1)
  
      else

        del = 0.8333333E-01 / fk
        del = del - 4.8E+00 * del * del * del
        v = difmuk / fk

        if ( 0.25E+00 < abs ( v ) ) then
          px = fk * log ( 1.0E+00 + v ) - difmuk - del
        else
          px = fk * v * v * ((((((( a7 &
            * v + a6 ) &
            * v + a5 ) &
            * v + a4 ) &
            * v + a3 ) &
            * v + a2 ) &
            * v + a1 ) &
            * v + a0 ) - del
        end if

        py = 0.3989423E+00 / sqrt ( fk )

      end if

      x = ( 0.5E+00 - difmuk ) / s
      xx = x * x
      fx = -0.5E+00 * xx
      fy = omega * ((( c3 * xx + c2 ) * xx + c1 ) * xx + c0 )

      if ( kflag <= 0 ) then

        if ( fy - u * fy <= py * exp ( px - fx ) ) then
          return
        end if

      else

        if ( c * abs ( u ) <= py * exp ( px + e ) - fy * exp ( fx + e ) ) then
          return
        end if

      end if

    end do

  end if

end
function ignuin ( low, high )

!*****************************************************************************80
!
!! IGNUIN generates a random integer in a given range.
!
!  Discussion:
!
!    Each deviate K satisfies LOW <= K <= HIGH.
!
!    If (HIGH-LOW) > 2,147,483,561, this procedure prints an error message
!    and stops the program.
!
!    IGNLGI generates integer ( kind = 4 )s between 1 and 2147483562.
!
!    MAXNUM is 1 less than the maximum generatable value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) LOW, HIGH, the lower and upper bounds.
!
!    Output, integer ( kind = 4 ) IGNUIN, a random deviate from 
!    the distribution.
!
  implicit none

  integer ( kind = 4 ) high
  integer ( kind = 4 ) i4_uni
  integer ( kind = 4 ) ign
  integer ( kind = 4 ) ignuin
  integer ( kind = 4 ) low
  integer ( kind = 4 ) maxnow
  integer ( kind = 4 ) maxnum
  parameter ( maxnum = 2147483561 )
  integer ( kind = 4 ) ranp1
  integer ( kind = 4 ) width

  if ( high < low ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNUIN - Fatal error!'
    write ( *, '(a)' ) '  HIGH < LOW.'
    stop 1
  end if

  width = high - low

  if ( maxnum < width ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNUIN - Fatal error!'
    write ( *, '(a)' ) '  Range HIGH-LOW is too large.'
    stop 1
  end if

  if ( low == high ) then
    ignuin = low
    return
  end if

  ranp1 = width + 1
  maxnow = ( maxnum / ranp1 ) * ranp1

  do

    ign = i4_uni ( ) - 1

    if ( ign <= maxnow ) then
      exit
    end if

  end do

  ignuin = low + mod ( ign, ranp1 )

  return
end
function lennob ( s )

!*****************************************************************************80
!
!! LENNOB counts the length of a string, ignoring trailing blanks.
!
!  Discussion:
!
!    This procedure returns the length of a string up to and including
!    the last non-blank character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character * ( * ) S, the string.
!
!    Output, integer ( kind = 4 ) LENNOB, the length of the string to the last
!    nonblank.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) lennob
  character * ( * ) s
  integer ( kind = 4 ) s_max

  s_max = len ( s )

  do i = s_max, 1, -1
    if ( s(i:i) /= ' ' ) then
      lennob = i
      return
    end if
  end do

  lennob = 0

  return
end
subroutine phrtsd ( phrase, seed1, seed2 )

!*****************************************************************************80
!
!! PHRTST converts a phrase to a pair of random number generator seeds.
!
!  Discussion:
!
!    This procedure uses a character string to generate two seeds for the RGN
!    random number generator.
!
!    Trailing blanks are eliminated before the seeds are generated.
!
!    Generated seed values will fall in the range 1 to 2^30 = 1,073,741,824.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character * ( * ) PHRASE, a phrase to be used for the
!    random number generation.
!
!    Output, integer ( kind = 4 ) SEED1, SEED2, the two seeds for the
!    random number generator, based on PHRASE.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ichr
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lennob
  integer ( kind = 4 ) lphr
  character * ( * ) phrase
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  integer ( kind = 4 ) shift(0:4)
  character * ( 86 ) table
  parameter ( table = &
    'abcdefghijklmnopqrstuvwxyz'// &
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ'// &
    '0123456789'// &
    '!@#$%^&*()_+[];:''"<>?,./' )
  integer ( kind = 4 ) twop30
  parameter ( twop30 = 1073741824 )
  integer ( kind = 4 ) values(5)

  save shift

  data shift / 1, 64, 4096, 262144, 16777216 /

  seed1 = 1234567890
  seed2 = 123456789

  lphr = lennob ( phrase )

  do i = 1, lphr

    ichr = index ( table, phrase(i:i) )
!
!  If the character does not occur, ICHR is returned as 0.
!
    ichr = mod ( ichr, 64 )

    if ( ichr == 0 ) then
      ichr = 63
    end if

    do j = 1, 5
      values(j) = ichr - j
      if ( values(j) < 1 ) then
        values(j) = values(j) + 63
      end if
    end do

    do j = 1, 5
      seed1 = mod ( seed1 + shift(j-1) * values(j), twop30 )
      seed2 = mod ( seed2 + shift(j-1) * values(6-j), twop30 )
    end do

  end do

  return
end
subroutine prcomp ( maxobs, p, mean, xcovar, answer )

!*****************************************************************************80
!
!! PRCOMP prints covariance information.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 September 2018
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MAXOBS, the number of observations.
!
!    Input, integer ( kind = 4 ) P, the number of variables.
!
!    Input, real ( kind = 4 ) MEAN(P), the mean for each column.
!
!    Input, real ( kind = 4 ) XCOVAR(P,P), the variance/covariance matrix.
!
!    Input, real ( kind = 4 ) ANSWER(MAXOBS,P), the observed values.
!
  implicit none

  integer ( kind = 4 ) p
  integer ( kind = 4 ) maxobs

  real ( kind = 4 ) answer(maxobs,p)
  real ( kind = 4 ) dum1
  real ( kind = 4 ) dum2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 4 ) mean(p)
  real ( kind = 4 ) r4vec_covar
  real ( kind = 4 ) rcovar(p,p)
  real ( kind = 4 ) rmean(p)
  real ( kind = 4 ) rvar(p)
  real ( kind = 4 ) xcovar(p,p)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PRCOMP:'
  write ( *, '(a)' ) '  Print and compare covariance information'
  write ( *, '(a)' ) ' '

  do j = 1, p
    call stats ( answer(1,j), maxobs, rmean(j), rvar(j), &
      dum1, dum2 )
    write ( *, '(a,i4)' ) '  Variable Number ', j
    write ( *, '(a,g14.6,a,g14.6)' ) &
      '  Mean ', mean(j), ' Generated ', rmean(j)
    write ( *, '(a,g14.6,a,g14.6)' ) &
      '  Variance ', xcovar(j,j), ' Generated ', rvar(j)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Covariances:'
  write ( *, '(a)' ) ' '

  do i = 1, p
    do j = 1, i - 1
      write ( *, '(a,i4,a,i4)' ) '  I = ', i, ' J = ', j
      rcovar(i,j) = r4vec_covar ( maxobs, answer(1,i), answer(1,j) )
      write ( *, '(a,g14.6,a,g14.6)' ) &
        '  Covariance ', xcovar(i,j), ' Generated ', rcovar(i,j)
    end do
  end do

  return
end
function r4_exp ( x )

!*****************************************************************************80
!
!! R4_EXP computes the exponential of an R8, avoiding overflow and underflow.
!
!  Discussion:
!
!    For arguments of very large magnitude, the evaluation of the
!    exponential function can cause computational problems.  Some languages
!    and compilers may return an infinite value or a "Not-a-Number".  
!    An alternative, when dealing with a wide range of inputs, is simply
!    to truncate the calculation for arguments whose magnitude is too large.
!    Whether this is the right or convenient approach depends on the problem
!    you are dealing with, and whether or not you really need accurate
!    results for large magnitude inputs, or you just want your code to
!    stop crashing.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) X, the argument of the exponential function.
!
!    Output, real ( kind = 4 ) R4_EXP, the value of exp ( X ).
!
  implicit none

  real ( kind = 4 ) r4_exp
  real ( kind = 4 ), parameter :: r4_huge = 1.0E+30
  real ( kind = 4 ), parameter :: r4_log_max = +69.0776E+00
  real ( kind = 4 ), parameter :: r4_log_min = -69.0776E+00
  real ( kind = 4 ) value
  real ( kind = 4 ) x

  if ( x <= r4_log_min ) then
    value = 0.0E+00
  else if ( x < r4_log_max ) then
    value = exp ( x )
  else
    value = r4_huge
  end if

  r4_exp = value

  return
end
function r4_exponential_sample ( lambda )

!*****************************************************************************80
!
!! R4_EXPONENTIAL_SAMPLE samples the exponential PDF.
!
!  Discussion:
!
!    Note that the parameter LAMBDA is a multiplier.  In some formulations,
!    it is used as a divisor instead.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) LAMBDA, the parameter of the PDF.
!
!    Output, real ( kind = 4 ) R4_EXPONENTIAL_SAMPLE, a sample of the PDF.
!
  implicit none

  real ( kind = 4 ) lambda
  real ( kind = 4 ) r
  real ( kind = 4 ) r4_exponential_sample
  real ( kind = 4 ) r4_uni_01

  r = r4_uni_01 ( )

  r4_exponential_sample = - log ( r ) * lambda

  return
end
function r4vec_covar ( n, x, y )

!*****************************************************************************80
!
!! R4VEC_COVAR computes the covariance of two vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) X(N), Y(N), the two vectors.
!
!    Input, integer ( kind = 4 ) N, the dimension of the two vectors.
!
!    Output, real ( kind = 4 ) R4VEC_COVAR, the covariance of the vectors.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 4 ) r4vec_covar
  real ( kind = 4 ) value
  real ( kind = 4 ) x(n)
  real ( kind = 4 ) x_average
  real ( kind = 4 ) y(n)
  real ( kind = 4 ) y_average

  x_average = sum ( x(1:n) ) / real ( n, kind = 4 )
  y_average = sum ( y(1:n) ) / real ( n, kind = 4 )
 
  value = 0.0E+00
  do i = 1, n
    value = value + ( x(i) - x_average ) * ( y(i) - y_average )
  end do

  r4vec_covar = value / real ( n - 1, kind = 4 )

  return
end
function r8_exponential_sample ( lambda )

!*****************************************************************************80
!
!! R8_EXPONENTIAL_SAMPLE samples the exponential PDF.
!
!  Discussion:
!
!    Note that the parameter LAMBDA is a multiplier.  In some formulations,
!    it is used as a divisor instead.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) LAMBDA, the parameter of the PDF.
!
!    Output, real ( kind = 8 ) R8_EXPONENTIAL_SAMPLE, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) lambda
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_exponential_sample
  real ( kind = 8 ) r8_uni_01

  r = r8_uni_01 ( )

  r8_exponential_sample = - log ( r ) * lambda

  return
end
function r8vec_covar ( n, x, y )

!*****************************************************************************80
!
!! R8VEC_COVAR computes the covariance of two vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), Y(N), the two vectors.
!
!    Input, integer ( kind = 4 ) N, the dimension of the two vectors.
!
!    Output, real ( kind = 8 ) R4VEC_COVAR, the covariance of the vectors.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec_covar
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_average
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) y_average

  x_average = sum ( x(1:n) ) / real ( n, kind = 8 )
  y_average = sum ( y(1:n) ) / real ( n, kind = 8 )
 
  value = 0.0D+00
  do i = 1, n
    value = value + ( x(i) - x_average ) * ( y(i) - y_average )
  end do

  r8vec_covar = value / real ( n - 1, kind = 8 )

  return
end
function sdot ( n, sx, incx, sy, incy )

!*****************************************************************************80
!
!! SDOT forms the dot product of two vectors.
!
!  Discussion:
!
!    This routine uses single precision real ( kind = 4 ) arithmetic.
!
!    This routine uses unrolled loops for increments equal to one.
!
!  Modified:
!
!    07 July 2007
!
!  Author:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for FORTRAN usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, pages 308-323, 1979.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input, real ( kind = 4 ) X(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of X.
!
!    Input, real ( kind = 4 ) Y(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    elements of Y.
!
!    Output, real ( kind = 4 ) SDOT, the dot product of X and Y.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 4 ) sdot
  real ( kind = 4 ) stemp
  real ( kind = 4 ) sx(*)
  real ( kind = 4 ) sy(*)

  sdot = 0.0E+00

  if ( n <= 0 ) then
    return
  end if

  stemp = 0.0E+00
!
!  Code for unequal increments or equal increments not equal to 1.
!
  if ( incx /= 1 .or. incy /= 1 ) then

    if ( incx < 0 ) then
      ix = ( - n + 1 ) * incx + 1
    else
      ix = 1
    end if

    if ( incy < 0 ) then
      iy = ( - n + 1 ) * incy + 1
    else
      iy = 1
    end if

    do i = 1, n
      stemp = stemp + sx(ix) * sy(iy)
      ix = ix + incx
      iy = iy + incy
    end do
!
!  Code for both increments equal to 1.
!
  else

    m = mod ( n, 5 )

    do i = 1, m
      stemp = stemp + sx(i) * sy(i)
    end do

    do i = m + 1, n, 5
      stemp = stemp &
       + sx(i)     * sy(i) &
       + sx(i + 1) * sy(i + 1) &
       + sx(i + 2) * sy(i + 2) &
       + sx(i + 3) * sy(i + 3) &
       + sx(i + 4) * sy(i + 4)
    end do

  end if

  sdot = stemp

  return
end
subroutine setcov ( p, var, corr, covar )

!*****************************************************************************80
!
!! SETCOV sets a covariance matrix from variance and common correlation.
!
!  Discussion:
!
!    This procedure sets the covariance matrix from the variance and
!    common correlation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) P, the number of variables.
!
!    Input, real ( kind = 4 ) VAR(P), the variances.
!
!    Input, real ( kind = 4 ) CORR, the common correlaton.
!
!    Output, real ( kind = 4 ) COVAR(P,P), the covariance matrix.
!
  implicit none

  integer ( kind = 4 ) p

  real ( kind = 4 ) corr
  real ( kind = 4 ) covar(p,p)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 4 ) var(p)

  do i = 1, p
    do  j = 1, p
      if ( i == j ) then
        covar(i,j) = var(i)
      else
        covar(i,j) = corr * sqrt ( var(i) * var(j) )
      end if
    end do
  end do

  return
end
subroutine setgmn ( meanv, covm, p, parm )

!*****************************************************************************80
!
!! SETGMN sets data for the generation of multivariate normal deviates.
!
!  Discussion:
!
!    This procedure places P, MEANV, and the Cholesky factorization of
!    COVM in GENMN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) MEANV(P), the means of the multivariate 
!    normal distribution.
!
!    Input/output, real ( kind = 4 ) COVM(P,P).  On input, the covariance
!    matrix of the multivariate distribution.  On output, the information 
!    in COVM has been overwritten.
!
!    Input, integer ( kind = 4 ) P, the number of dimensions.
!
!    Output, real ( kind = 4 ) PARM(P*(P+3)/2+1), parameters needed to generate
!    multivariate normal deviates.
!
  implicit none

  integer ( kind = 4 ) p

  real ( kind = 4 ) covm(p,p)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icount
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  real ( kind = 4 ) meanv(p)
  real ( kind = 4 ) parm(p*(p+3)/2+1)

  if ( p <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SETGMN - Fatal error!'
    write ( *, '(a)' ) '  P was not positive.'
    stop 1
  end if 
!
!  Store P.
!
  parm(1) = p
!
!  Store MEANV.
!
  do i = 2, p + 1
    parm(i) = meanv(i-1)
  end do
!
!  Compute the Cholesky decomposition.
!
  call spofa ( covm, p, p, info )

  if ( info /= 0) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SETGMN - Fatal error!'
    write ( *, '(a)' ) '  SPOFA finds COVM not positive definite.'
    stop 1
  end if
!
!  Store the upper half of the Cholesky factor.
!
  icount = p + 1

  do i = 1, p
    do j = i, p
      icount = icount + 1
      parm(icount) = covm(i,j)
    end do
  end do

  return
end
function sexpo ( )

!*****************************************************************************80
!
!! SEXPO samples the standard exponential distribution.
!
!  Discussion:
!
!   This procedure corresponds to algorithm SA in the reference.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Computer Methods for Sampling From the
!    Exponential and Normal Distributions,
!    Communications of the ACM,
!    Volume 15, Number 10, October 1972, pages 873-882.
!
!  Parameters:
!
!    Output, real ( kind = 4 ) SEXPO, a random deviate from the standard
!    exponential distribution.
!
  implicit none

  real ( kind = 4 ) a
  integer ( kind = 4 ) i
  real ( kind = 4 ) q(8)
  real ( kind = 4 ) r4_uni_01
  real ( kind = 4 ) sexpo
  real ( kind = 4 ) u
  real ( kind = 4 ) umin
  real ( kind = 4 ) ustar

  save q

  data q / &
       0.6931472E+00, &
       0.9333737E+00, &
       0.9888778E+00, &
       0.9984959E+00, &
       0.9998293E+00, &
       0.9999833E+00, &
       0.9999986E+00, &
       0.9999999E+00 /

  a = 0.0E+00
  u = r4_uni_01 ( )

  do

    u = u + u

    if ( 1.0E+00 < u ) then
      exit
    end if

    a = a + q(1)

  end do

  u = u - 1.0E+00

  if ( u <= q(1) ) then
    sexpo = a + u
    return
  end if

  i = 1
  ustar = r4_uni_01 ( )
  umin = ustar

  do

    ustar = r4_uni_01 ( )
    umin = min ( umin, ustar )
    i = i + 1

    if ( u <= q(i) ) then
      exit
    end if

  end do

  sexpo = a + umin * q(1)

  return
end
function sgamma ( a )

!*****************************************************************************80
!
!! SGAMMA samples the standard Gamma distribution.
!
!  Discussion:
!
!    This procedure corresponds to algorithm GD in the reference.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Generating Gamma Variates by a Modified Rejection Technique,
!    Communications of the ACM,
!    Volume 25, Number 1, January 1982, pages 47-54.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) A, the parameter of the standard gamma
!    distribution.  0.0 < A < 1.0.
!
!    Output, real ( kind = 4 ) SGAMMA, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) a
  real ( kind = 4 ), parameter :: a1 =  0.3333333E+00
  real ( kind = 4 ), parameter :: a2 = -0.2500030E+00
  real ( kind = 4 ), parameter :: a3 =  0.2000062E+00
  real ( kind = 4 ), parameter :: a4 = -0.1662921E+00
  real ( kind = 4 ), parameter :: a5 =  0.1423657E+00
  real ( kind = 4 ), parameter :: a6 = -0.1367177E+00
  real ( kind = 4 ), parameter :: a7 =  0.1233795E+00
  real ( kind = 4 ) b
  real ( kind = 4 ) c
  real ( kind = 4 ) d
  real ( kind = 4 ) e
  real ( kind = 4 ), parameter :: e1 = 1.0E+00
  real ( kind = 4 ), parameter :: e2 = 0.4999897E+00
  real ( kind = 4 ), parameter :: e3 = 0.1668290E+00
  real ( kind = 4 ), parameter :: e4 = 0.0407753E+00
  real ( kind = 4 ), parameter :: e5 = 0.0102930E+00
  real ( kind = 4 ) p
  real ( kind = 4 ) q
  real ( kind = 4 ) q0
  real ( kind = 4 ), parameter :: q1 =  0.04166669E+00
  real ( kind = 4 ), parameter :: q2 =  0.02083148E+00
  real ( kind = 4 ), parameter :: q3 =  0.00801191E+00
  real ( kind = 4 ), parameter :: q4 =  0.00144121E+00
  real ( kind = 4 ), parameter :: q5 = -0.00007388E+00
  real ( kind = 4 ), parameter :: q6 =  0.00024511E+00
  real ( kind = 4 ), parameter :: q7 =  0.00024240E+00
  real ( kind = 4 ) r
  real ( kind = 4 ) r4_uni_01
  real ( kind = 4 ) s
  real ( kind = 4 ) s2
  real ( kind = 4 ) sexpo
  real ( kind = 4 ) si
  real ( kind = 4 ) sgamma
  real ( kind = 4 ) snorm
  real ( kind = 4 ), parameter :: sqrt32 = 5.656854E+00
  real ( kind = 4 ) t
  real ( kind = 4 ) u
  real ( kind = 4 ) v
  real ( kind = 4 ) w
  real ( kind = 4 ) x

  if ( 1.0E+00 <= a ) then

    s2 = a - 0.5E+00
    s = sqrt ( s2 )
    d = sqrt32 - 12.0E+00 * s
!
!  Immediate acceptance.
!
    t = snorm ( )
    x = s + 0.5E+00 * t
    sgamma = x * x

    if ( 0.0E+00 <= t ) then
      return
    end if
!
!  Squeeze acceptance.
!
    u = r4_uni_01 ( )
    if ( d * u <= t * t * t ) then
      return
    end if

    r = 1.0E+00 / a
    q0 = (((((( q7 &
      * r + q6 ) &
      * r + q5 ) &
      * r + q4 ) &
      * r + q3 ) &
      * r + q2 ) &
      * r + q1 ) &
      * r
!
!  Approximation depending on size of parameter A.
!
    if ( 13.022E+00 < a ) then
      b = 1.77E+00
      si = 0.75E+00
      c = 0.1515E+00 / s
    else if ( 3.686E+00 < a ) then
      b = 1.654E+00 + 0.0076E+00 * s2
      si = 1.68E+00 / s + 0.275E+00
      c = 0.062E+00 / s + 0.024E+00
    else
      b = 0.463E+00 + s + 0.178E+00 * s2
      si = 1.235E+00
      c = 0.195E+00 / s - 0.079E+00 + 0.16E+00 * s
    end if
!
!  Quotient test.
!
    if ( 0.0E+00 < x ) then

      v = 0.5E+00 * t / s

      if ( 0.25E+00 < abs ( v ) ) then
        q = q0 - s * t + 0.25E+00 * t * t + 2.0E+00 * s2 * log ( 1.0E+00 + v )
      else
        q = q0 + 0.5E+00 * t * t * (((((( a7 &
          * v + a6 ) &
          * v + a5 ) &
          * v + a4 ) &
          * v + a3 ) &
          * v + a2 ) &
          * v + a1 ) &
          * v
      end if

      if ( log ( 1.0E+00 - u ) <= q ) then
        return
      end if

    end if

    do

      e = sexpo ( )
      u = 2.0E+00 * r4_uni_01 ( ) - 1.0E+00
 
      if ( 0.0E+00 <= u ) then
        t = b + abs ( si * e )
      else
        t = b - abs ( si * e )
      end if
!
!  Possible rejection.
!
      if ( t < -0.7187449E+00 ) then
        cycle
      end if
!
!  Calculate V and quotient Q.
!
      v = 0.5E+00 * t / s

      if ( 0.25E+00 < abs ( v ) ) then
        q = q0 - s * t + 0.25E+00 * t * t + 2.0E+00 * s2 * log ( 1.0E+00 + v )
      else
        q = q0 + 0.5E+00 * t * t * (((((( a7 &
          * v + a6 ) &
          * v + a5 ) &
          * v + a4 ) &
          * v + a3 ) &
          * v + a2 ) &
          * v + a1 ) &
          *  v
      end if
!
!  Hat acceptance.
!
      if ( q <= 0.0E+00 ) then
        cycle
      end if

      if ( 0.5E+00 < q ) then
        w = exp ( q ) - 1.0E+00
      else
        w = (((( e5 * q + e4 ) * q + e3 ) * q + e2 ) * q + e1 ) * q
      end if
!
!  May have to sample again.
!
      if ( c * abs ( u ) <= w * exp ( e - 0.5E+00 * t * t ) ) then
        exit
      end if

    end do

    x = s + 0.5E+00 * t
    sgamma = x * x

    return
!
!  Method for A < 1.
!
  else

    b = 1.0E+00 + 0.3678794E+00 * a

    do

      p = b * r4_uni_01 ( )

      if ( p < 1.0E+00 ) then

        sgamma = exp ( log ( p ) / a )

        if ( sgamma <= sexpo ( ) ) then
          return
        end if

        cycle

      end if

      sgamma = - log ( ( b - p ) / a )

      if ( ( 1.0E+00 - a ) * log ( sgamma ) <= sexpo ( ) ) then
        exit
      end if

    end do

  end if

  return
end
function snorm ( )

!*****************************************************************************80
!
!! SNORM samples the standard normal distribution.
!
!  Discussion:
!
!    This procedure corresponds to algorithm FL, with M = 5, in the reference.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Extensions of Forsythe's Method for Random
!    Sampling from the Normal Distribution,
!    Mathematics of Computation,
!    Volume 27, Number 124, October 1973, page 927-937.
!
!  Parameters:
!
!    Output, real ( kind = 4 ) SNORM, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) a(32)
  real ( kind = 4 ) aa
  real ( kind = 4 ) d(31)
  real ( kind = 4 ) h(31)
  integer ( kind = 4 ) i
  real ( kind = 4 ) r4_uni_01
  real ( kind = 4 ) s
  real ( kind = 4 ) snorm
  real ( kind = 4 ) t(31)
  real ( kind = 4 ) tt
  real ( kind = 4 ) u
  real ( kind = 4 ) ustar
  real ( kind = 4 ) w
  real ( kind = 4 ) y

  save a
  save d
  save h
  save t

  data a / &
        0.0000000E+00, 0.3917609E-01, 0.7841241E-01, 0.1177699E+00, &
        0.1573107E+00, 0.1970991E+00, 0.2372021E+00, 0.2776904E+00, &
        0.3186394E+00, 0.3601299E+00, 0.4022501E+00, 0.4450965E+00, &
        0.4887764E+00, 0.5334097E+00, 0.5791322E+00, 0.6260990E+00, &
        0.6744898E+00, 0.7245144E+00, 0.7764218E+00, 0.8305109E+00, &
        0.8871466E+00, 0.9467818E+00, 1.009990E+00,  1.077516E+00, &
        1.150349E+00,  1.229859E+00,  1.318011E+00,  1.417797E+00, &
        1.534121E+00,  1.675940E+00,  1.862732E+00,  2.153875E+00 /

  data d / &
        0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, &
        0.0000000E+00, 0.2636843E+00, 0.2425085E+00, 0.2255674E+00, &
        0.2116342E+00, 0.1999243E+00, 0.1899108E+00, 0.1812252E+00, &
        0.1736014E+00, 0.1668419E+00, 0.1607967E+00, 0.1553497E+00, &
        0.1504094E+00, 0.1459026E+00, 0.1417700E+00, 0.1379632E+00, &
        0.1344418E+00, 0.1311722E+00, 0.1281260E+00, 0.1252791E+00, &
        0.1226109E+00, 0.1201036E+00, 0.1177417E+00, 0.1155119E+00, &
        0.1134023E+00, 0.1114027E+00, 0.1095039E+00 /

  data h / &
        0.3920617E-01, 0.3932705E-01, 0.3950999E-01, 0.3975703E-01, &
        0.4007093E-01, 0.4045533E-01, 0.4091481E-01, 0.4145507E-01, &
        0.4208311E-01, 0.4280748E-01, 0.4363863E-01, 0.4458932E-01, &
        0.4567523E-01, 0.4691571E-01, 0.4833487E-01, 0.4996298E-01, &
        0.5183859E-01, 0.5401138E-01, 0.5654656E-01, 0.5953130E-01, &
        0.6308489E-01, 0.6737503E-01, 0.7264544E-01, 0.7926471E-01, &
        0.8781922E-01, 0.9930398E-01, 0.1155599E+00, 0.1404344E+00, &
        0.1836142E+00, 0.2790016E+00, 0.7010474E+00 /

  data t / &
        0.7673828E-03, 0.2306870E-02, 0.3860618E-02, 0.5438454E-02, &
        0.7050699E-02, 0.8708396E-02, 0.1042357E-01, 0.1220953E-01, &
        0.1408125E-01, 0.1605579E-01, 0.1815290E-01, 0.2039573E-01, &
        0.2281177E-01, 0.2543407E-01, 0.2830296E-01, 0.3146822E-01, &
        0.3499233E-01, 0.3895483E-01, 0.4345878E-01, 0.4864035E-01, &
        0.5468334E-01, 0.6184222E-01, 0.7047983E-01, 0.8113195E-01, &
        0.9462444E-01, 0.1123001E+00, 0.1364980E+00, 0.1716886E+00, &
        0.2276241E+00, 0.3304980E+00, 0.5847031E+00 /

  u = r4_uni_01 ( )
  if ( u <= 0.5E+00 ) then
    s = 0.0E+00
  else
    s = 1.0E+00
  end if
  u = 2.0E+00 * u - s
  u = 32.0E+00 * u
  i = int ( u )
  if ( i == 32 ) then
    i = 31
  end if
!
!  Center
!
  if ( i /= 0 ) then

    ustar = u - real ( i )
    aa = a(i)

    do

      if ( t(i) < ustar ) then

        w = ( ustar - t(i) ) * h(i)

        y = aa + w

        if ( s /= 1.0E+00 ) then
          snorm = y
        else
          snorm = -y
        end if

        return

      end if

      u = r4_uni_01 ( )
      w = u * ( a(i+1) - aa )
      tt = ( 0.5E+00 * w + aa ) * w

      do

        if ( tt < ustar ) then
          y = aa + w
          if ( s /= 1.0E+00 ) then
            snorm = y
          else
            snorm = -y
          end if
          return
        end if

        u = r4_uni_01 ( )

        if ( ustar < u ) then
          exit
        end if

        tt = u
        ustar = r4_uni_01 ( )

      end do

      ustar = r4_uni_01 ( )

    end do
!
!  Tail
!
  else

    i = 6
    aa = a(32)

    do

      u = u + u

      if ( 1.0E+00 <= u ) then
        exit
      end if

      aa = aa + d(i)
      i = i + 1

    end do

    u = u - 1.0E+00
    w = u * d(i)
    tt = ( 0.5E+00 * w + aa ) * w

    do

      ustar = r4_uni_01 ( )

      if ( tt < ustar ) then
        y = aa + w
        if ( s /= 1.0E+00 ) then
          snorm = y
        else
          snorm = -y
        end if
        return
      end if

      u = r4_uni_01 ( )

      if ( u <= ustar ) then
        tt = u
      else
        u = r4_uni_01 ( )
        w = u * d(i)
        tt = ( 0.5E+00 * w + aa ) * w
      end if

    end do

  end if

end
subroutine spofa ( a, lda, n, info )

!*****************************************************************************80
!
!! SPOFA factors a real symmetric positive definite matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Cleve Moler
!
!  Parameters:
!
!    Input/output, real ( kind = 4 ) A(LDA,N).  On input, the symmetric matrix
!    to be factored.  Only the diagonal and upper triangle are accessed.  
!    On output, the strict lower triangle has not been changed.  The diagonal
!    and upper triangle contain an upper triangular matrix R such that 
!    A = R' * R.  If INFO is nonzero, the factorization was not completed.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of the array A.
!    N <= LDA.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Output, integer ( kind = 4 ) INFO, error flag.
!    0, no error was detected.
!    K, the leading minor of order K is not positive definite.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  real ( kind = 4 ) a(lda,n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jm1
  integer ( kind = 4 ) k
  real ( kind = 4 ) s
  real ( kind = 4 ) sdot
  real ( kind = 4 ) t

  info = 0

  do j = 1, n
    info = j
    s = 0.0E+00
    jm1 = j - 1
    do k = 1, jm1
      t = a(k,j) - sdot ( k-1, a(1,k), 1, a(1,j), 1 )
      t = t / a(k,k)
      a(k,j) = t
      s = s + t * t
    end do
    s = a(j,j) - s
    if ( s <= 0.0E+00 ) then
      info = j
      return
    end if
    a(j,j) = sqrt ( s )
  end do

  return
end
subroutine stats ( x, n, av, var, xmin, xmax )

!*****************************************************************************80
!
!! STATS computes statistics for a given array.
!
!  Discussion:
!
!    This procedure computes the average and variance of an array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) X(N), the array to be analyzed.
!
!    Input, integer ( kind = 4 ) N, the dimension of the array.
!
!    Output, real ( kind = 4 ) AV, the average value.
!
!    Output, real ( kind = 4 ) VAR, the variance.
!
!    Output, real ( kind = 4 ) XMIN, XMAX, the minimum and maximum entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) av
  integer ( kind = 4 ) i
  real ( kind = 4 ) total
  real ( kind = 4 ) var
  real ( kind = 4 ) x(n)
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  xmin = x(1)
  xmax = x(1)
  total = 0.0E+00
  do i = 1, n
    total = total + x(i)
    xmin = min ( xmin, x(i) )
    xmax = max ( xmax, x(i) )
  end do

  av = total / real ( n )

  total = 0.0E+00
  do i = 1, n
    total = total + ( x(i) - av ) ** 2
  end do
  var = total / real ( n - 1 )

  return
end
subroutine trstat ( pdf, parin, av, var )

!*****************************************************************************80
!
!! TRSTAT returns the mean and variance for distributions.
!
!  Discussion:
!
!    This procedure returns the mean and variance for a number of statistical
!    distributions as a function of their parameters.
!
!    The input vector PARIN is used to pass in the parameters necessary
!    to specify the distribution.  The number of these parameters varies
!    per distribution, and it is necessary to specify an ordering for the
!    parameters used to a given distribution.  The ordering chosen here
!    is as follows:
!
!    bet
!      PARIN(1) is A
!      PARIN(2) is B
!    bin
!      PARIN(1) is Number of trials
!      PARIN(2) is Prob Event at Each Trial
!    chi
!      PARIN(1) = df
!    exp
!      PARIN(1) = mu
!    f
!      PARIN(1) is df numerator
!      PARIN(2) is df denominator
!    gam
!      PARIN(1) is A
!      PARIN(2) is R
!    nbn
!      PARIN(1) is N
!      PARIN(2) is P
!    nch
!      PARIN(1) is df
!      PARIN(2) is noncentrality parameter
!    nf
!      PARIN(1) is df numerator
!      PARIN(2) is df denominator
!      PARIN(3) is noncentrality parameter
!    nor
!      PARIN(1) is mean
!      PARIN(2) is standard deviation
!    poi
!      PARIN(1) is Mean
!    unf
!      PARIN(1) is LOW bound
!      PARIN(2) is HIGH bound
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character * ( 4 ) PDF, indicates the distribution:
!    'bet'  beta distribution
!    'bin'  binomial
!    'chi'  chisquare
!    'exp'  exponential
!    'f'    F (variance ratio)
!    'gam'  gamma
!    'nbn'  negative binomial
!    'nch'  noncentral chisquare
!    'nf'   noncentral f
!    'nor'  normal
!    'poi'  Poisson
!    'unf'  uniform
!
!    Input, real ( kind = 4 ) PARIN(*), the parameters of the distribution.
!
!    Output, real ( kind = 4 ) AV, the mean of the specified distribution.
!
!    Output, real ( kind = 4 ) VAR, the variance of the specified distribuion.
!
  implicit none

  real ( kind = 4 ) a
  real ( kind = 4 ) av
  real ( kind = 4 ) b
  integer ( kind = 4 ) n
  real ( kind = 4 ) p
  real ( kind = 4 ) parin(*)
  character * ( 4 ) pdf
  real ( kind = 4 ) r
  real ( kind = 4 ) var
  real ( kind = 4 ) width

  if ( pdf == 'bet' ) then

    av = parin(1) / ( parin(1) + parin(2) )
    var = ( av * parin(2) ) / ( ( parin(1) + parin(2) ) * &
      ( parin(1) + parin(2) + 1.0E+00 ) )

  else if ( pdf == 'bin' ) then

    n = int ( parin(1) )
    p = parin(2)
    av = real ( n ) * p
    var = real ( n ) * p * ( 1.0E+00 - p )

  else if ( pdf == 'chi' ) then

    av = parin(1)
    var = 2.0E+00 * parin(1)

  else if ( pdf == 'exp' ) then

    av = parin(1)
    var = av ** 2

  else if ( pdf == 'f' ) then

    if ( parin(2) <= 2.0001E+00 ) then
      av = -1.0E+00
    else
      av = parin(2) / ( parin(2) - 2.0E+00 )
    end if

    if ( parin(2) <= 4.0001E+00 ) then
      var = -1.0E+00
    else
      var = ( 2.0E+00 * parin(2) ** 2 * ( parin(1) + parin(2) - 2.0E+00 ) ) / &
        ( parin(1) * ( parin(2) - 2.0E+00 ) ** 2 * ( parin(2) - 4.0E+00 ) )
    end if

  else if ( pdf == 'gam' ) then

    a = parin(1)
    r = parin(2)
    av = r / a
    var = r / a ** 2

  else if ( pdf == 'nbn' ) then

    n = int ( parin(1) )
    p = parin(2)
    av = n * ( 1.0E+00 - p ) / p
    var = n * ( 1.0E+00 - p ) / p ** 2

  else if ( pdf == 'nch' ) then

    a = parin(1) + parin(2)
    b = parin(2) / a
    av = a
    var = 2.0E+00 * a * ( 1.0E+00 + b )

  else if ( pdf == 'nf' ) then

    if ( parin(2) <= 2.0001E+00 ) then
      av = -1.0E+00
    else
      av = ( parin(2) * ( parin(1) + parin(3) ) ) &
        / ( ( parin(2) - 2.0E+00 ) * parin(1) )
    end if

    if ( parin(2) <= 4.0001E+00 ) then
      var = -1.0E+00
    else
      a = ( parin(1) + parin(3) ) ** 2 &
        + ( parin(1) + 2.0E+00 * parin(3) ) * ( parin(2) - 2.0E+00 )
      b = ( parin(2) - 2.0E+00 ) ** 2 * ( parin(2) - 4.0E+00 )
      var = 2.0E+00 * ( parin(2) / parin(1) ) ** 2 * ( a / b )
    end if

  else if ( pdf == 'nor' ) then

    av = parin(1)
    var = parin(2) ** 2

  else if ( pdf == 'poi' ) then

    av = parin(1)
    var = parin(1)

  else if ( pdf == 'unf' ) then

    width = parin(2) - parin(1)
    av = parin(1) + width / 2.0E+00
    var = width ** 2 / 12.0E+00

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRSTAT - Fatal error!'
    write ( *, '(a)' ) '  Illegal input value for PDF.'
    stop 1

  end if

  return
end
