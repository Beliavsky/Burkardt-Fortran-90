function besselj ( order, x )

!*****************************************************************************80
!
!! BESSELJ evaluates the Bessel J function at an arbitrary real order.
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
!    Input, real ( kind = 8 ) ORDER, the order.
!    0.0 <= ORDER.
!
!    Input, real ( kind = 8 ), X, the evaluation point. 
!
!    Output, real ( kind = 8 ) BESSELJ, the value.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) besselj
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncalc
  real ( kind = 8 ) order
  real ( kind = 8 ) x

  n = int ( order )
  alpha = order - real ( n, kind = 8 )
  allocate ( b(0:n) )

  call rjbesl ( x, alpha, n + 1, b, ncalc )

  besselj = b(n)

  deallocate ( b )

  return
end
subroutine chebyshev_even1 ( n, f, a2 )

!*****************************************************************************80
!
!! CHEBYSHEV_EVEN1 returns the even Chebyshev coefficients of F.
!
!  Discussion:
!
!    The coefficients are calculated using the extreme points of Tn(x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    D B Hunter, H V Smith,
!    A quadrature formula of Clenshaw-Curtis type for the Gegenbauer 
!    weight function,
!    Journal of Computational and Applied Mathematics,
!    Volume 177, 2005, pages 389-400.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points to use.
!    1 <= N.
!
!    Input, real ( kind = 8 ), external F(x), the function to be 
!    integrated with the Gegenbauer weight.
!
!    Output, real ( kind = 8 ) A2(0:N/2), the even Chebyshev coefficients of F.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a2(0:n/2)
  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) j
  integer ( kind = 4 ) r
  integer ( kind = 4 ) rh
  real ( kind = 8 ) r8_n
  real ( kind = 8 ) r8_mop
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) s
  integer ( kind = 4 ) sigma
  real ( kind = 8 ) total

  s = ( n / 2 )
  sigma = mod ( n, 2 )

  r8_n = real ( n, kind = 8 )

  do r = 0, 2 * s, 2
    total = 0.5D+00 * f ( 1.0D+00 )
    do j = 1, n - 1
      total = total + f ( cos ( j * r8_pi / r8_n ) ) &
        * cos ( r * j * r8_pi / r8_n )
    end do
    total = total + 0.5 * r8_mop ( r ) * f ( -1.0D+00 )
    rh = r / 2
    a2(rh) = ( 2.0D+00 / r8_n ) * total
  end do

  return
end
subroutine chebyshev_even2 ( n, f, b2 )

!*****************************************************************************80
!
!! CHEBYSHEV_EVEN2 returns the even Chebyshev coefficients of F.
!
!  Discussion:
!
!    The coefficients are calculated using the zeros of Tn(x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    D B Hunter, H V Smith,
!    A quadrature formula of Clenshaw-Curtis type for the Gegenbauer 
!    weight function,
!    Journal of Computational and Applied Mathematics,
!    Volume 177, 2005, pages 389-400.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points to use.
!    1 <= N.
!
!    Input, real ( kind = 8 ) F(x), thefunction to be 
!    integrated with the Gegenbauer weight.
!
!    Output, real ( kind = 8 ) B2(0:N/2), the even Chebyshev coefficients of F.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b2(0:n/2)
  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) j
  integer ( kind = 4 ) r
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) rh
  integer ( kind = 4 ) s
  integer ( kind = 4 ) sigma
  real ( kind = 8 ) total
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2

  s = ( n / 2 )
  sigma = mod ( n, 2 )

  do r = 0, 2 * s, 2
    total = 0.0D+00
    do j = 0, n
      x1 = real ( 2 * j + 1, kind = 8 ) * r8_pi &
        / 2.0D+00 / real ( n + 1, kind = 8 )
      x2 = real ( r * ( 2 * j + 1 ), kind = 8 ) * r8_pi &
        / 2.0D+00 / real ( n + 1, kind = 8 )
      total = total + f ( cos ( x1 ) ) * cos ( x2 )
    end do
    rh = r / 2
    b2(rh) = ( 2.0D+00 / real ( n + 1, kind = 8 ) ) * total
  end do

  return
end
subroutine gegenbauer_cc1 ( n, lambda, f, value )

!*****************************************************************************80
!
!! GEGENBAUER_CC1 estimates the Gegenbauer integral of a function.
!
!  Discussion:
!
!     value = integral ( -1 <= x <= + 1 ) ( 1 - x^2 )^(lambda-1/2) * f(x) dx
!
!     The approximation uses the practical abscissas, that is, the extreme
!     points of Tn(x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    D B Hunter, H V Smith,
!    A quadrature formula of Clenshaw-Curtis type for the Gegenbauer 
!    weight function,
!    Journal of Computational and Applied Mathematics,
!    Volume 177, 2005, pages 389-400.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points to use.
!    1 <= N.
!
!    Input, real ( kind = 8 ) LAMBDA, used in the exponent of (1-x^2).
!    -0.5 < LAMBDA.
!
!    Input, real ( kind = 8 ), external, F(x), the function to be integrated 
!    with the Gegenbauer weight.
!
!    Output, real ( kind = 8 ) WEIGHT, the estimate for the Gegenbauer 
!    integral of F.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ), allocatable :: a2(:)
  real ( kind = 8 ), external :: f
  real ( kind = 8 ) lambda
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) rh
  integer ( kind = 4 ) s
  integer ( kind = 4 ) sigma
  real ( kind = 8 ) u
  real ( kind = 8 ) value

  value = 0.0D+00

  s = ( n / 2 )
  sigma = mod ( n, 2 )

  allocate ( a2(0:s) )

  call chebyshev_even1 ( n, f, a2 )

  rh = s
  u = 0.5D+00 * real ( sigma + 1, kind = 8 ) * a2(rh)
  do rh = s - 1, 1, -1
    u = ( real ( rh, kind = 8 ) - lambda ) &
      / ( real ( rh, kind = 8 ) + lambda + 1.0D+00 ) * u + a2(rh)
  end do
  u = - lambda * u / ( lambda + 1.0D+00 ) + 0.5D+00 * a2(0)

  value = gamma ( lambda + 0.5D+00 ) * sqrt ( r8_pi ) * u &
    / gamma ( lambda + 1.0D+00 )

  deallocate ( a2 )

  return
end
subroutine gegenbauer_cc2 ( n, lambda, f, value )

!*****************************************************************************80
!
!! GEGENBAUER_CC2 estimates the Gegenbauer integral of a function.
!
!  Discussion:
!
!     value = integral ( -1 <= x <= + 1 ) ( 1 - x^2 )^(lambda-1/2) * f(x) dx
!
!     The approximation uses the classical abscissas, that is, the zeros
!     of Tn(x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    D B Hunter, H V Smith,
!    A quadrature formula of Clenshaw-Curtis type for the Gegenbauer 
!    weight function,
!    Journal of Computational and Applied Mathematics,
!    Volume 177, 2005, pages 389-400.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points to use.
!    1 <= N.
!
!    Input, real ( kind = 8 ) LAMBDA, used in the exponent of (1-x^2).
!    -0.5 < LAMBDA.
!
!    Input, real ( kind = 8 ), external F(x), the function to be integrated with
!    the Gegenbauer weight.
!
!    Output, real ( kind = 8 ) WEIGHT, the estimate for the Gegenbauer 
!    integral of F.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ), allocatable :: b2(:)
  real ( kind = 8 ), external :: f
  real ( kind = 8 ) lambda
  integer ( kind = 4 ) rh
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) s
  integer ( kind = 4 ) sigma
  real ( kind = 8 ) u
  real ( kind = 8 ) value

  value = 0.0D+00

  s = ( n / 2 )
  sigma = mod ( n, 2 )

  allocate ( b2 ( 0:s) )
  call chebyshev_even2 ( n, f, b2 )

  rh = s
  u = real ( sigma + 1, kind = 8 ) * b2(rh)
  do rh = s - 1, 1, -1
    u = ( real ( rh, kind = 8 ) - lambda ) &
      / ( real ( rh, kind = 8 ) + lambda + 1.0D+00 ) * u + b2(rh)
  end do
  u = - lambda * u / ( lambda + 1.0D+00 ) + 0.5D+00 * b2(0)

  value = gamma ( lambda + 0.5D+00 ) * sqrt ( r8_pi ) * u &
    / gamma ( lambda + 1.0D+00 )

  deallocate ( b2 )

  return
end
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
  real ( kind = 8 ) value

  if ( mod ( i, 2 ) == 0 ) then
    value = + 1.0D+00
  else
    value = - 1.0D+00
  end if

  r8_mop = value

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
subroutine r8vec2_print ( n, a1, a2, title )

!*****************************************************************************80
!
!! R8VEC2_PRINT prints an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), the vectors to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, a1(i), a2(i)
  end do

  return
end
subroutine rjbesl ( x, alpha, nb, b, ncalc )

!*****************************************************************************80
!
!! RJBESL evaluates a sequence of Bessel J functions.
!
!  Discussion:
!
!    This routine calculates Bessel functions J sub(N+ALPHA) (X)
!    for non-negative argument X, and non-negative order N+ALPHA.
!
!    This program is based on a program written by David Sookne
!    that computes values of the Bessel functions J or I of real
!    argument and integer order.  Modifications include the restriction
!    of the computation to the J Bessel function of non-negative real
!    argument, the extension of the computation to arbitrary positive
!    order, and the elimination of most underflow.
!
!  Modified:
!
!    15 January 2016
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Reference: 
!
!    F Olver, David Sookne,
!    A Note on Backward Recurrence Algorithms," 
!    Math. Comp.,
!    Volume 26, 1972, pages 941-947.
!
!    David Sookne,
!    Bessel Functions of Real Argument and Integer Order,
!    NBS Journal of Res. B,
!    Volume 77B, 1973, pages 125-132.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, non-negative real argument for which
!    J's are to be calculated.
!
!    Input, real ( kind = 8 ) ALPHA, fractional part of order for which
!    J's or exponentially scaled J'r (J*exp(X)) are
!    to be calculated.  0 <= ALPHA < 1.0.
!
!    Input, integer ( kind = 4 ) NB, number of functions to be calculated, 
!    NB > 0.  The first function calculated is of order ALPHA, and the
!    last is of order (NB - 1 + ALPHA).
!
!    Output, real ( kind = 8 ) B(NB).  If RJBESL
!    terminates normally (NCALC=NB), the vector B contains the
!    functions J/ALPHA/(X) through J/NB-1+ALPHA/(X), or the
!    corresponding exponentially scaled functions.
!
!    Output, integer ( kind = 4 ) NCALC, indicates possible errors.
!    Before using the vector B, the user should check that
!    NCALC=NB, i.e., all orders have been calculated to
!    the desired accuracy.  See Error Returns below.
!
!  Internal Parameters:
!
!    IT = Number of bits in the mantissa of a working precision variable
!
!    NSIG   = Decimal significance desired.  Should be set to
!    INT(LOG10(2)*it+1).  Setting NSIG lower will result
!    in decreased accuracy while setting NSIG higher will
!    increase CPU time without increasing accuracy.  The
!    truncation error is limited to a relative error of
!    T=.5*10**(-NSIG).
!
!    Then the following machine-dependent constants must be declared
!    in DATA statements.  IEEE values are provided as a default.
!
!    ENTEN  = 10.0 ** K, where K is the largest integer such that
!    ENTEN is machine-representable in working precision.
!
!    ENSIG  = 10.0 ** NSIG
!
!    RTNSIG = 10.0 ** (-K) for the smallest integer K such that K >= NSIG/4
!
!    ENMTEN = Smallest ABS(X) such that X/4 does not underflow
!
!    XLARGE = Upper limit on the magnitude of X.  If ABS(X)=N,
!    then at least N iterations of the backward recursion
!    will be executed.  The value of 10.0 ** 4 is used on
!    every machine.
!
!  Error returns:
!
!    In case of an error,  NCALC /= NB, and not all J's are
!    calculated to the desired accuracy.
!
!    NCALC < 0:  An argument is out of range. For example,
!    NBES <= 0, ALPHA < 0 or > 1, or X is too large.
!    In this case, B(1) is set to zero, the remainder of the
!    B-vector is not calculated, and NCALC is set to
!    MIN(NB,0)-1 so that NCALC /= NB.
!
!    NB > NCALC > 0: Not all requested function values could
!    be calculated accurately.  This usually occurs because NB is
!    much larger than ABS(X).  In this case, B(N) is calculated
!    to the desired accuracy for N <= NCALC, but precision
!    is lost for NCALC < N <= NB.  If B(N) does not vanish
!    for N > NCALC (because it is too small to be represented),
!    and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K
!    significant figures of B(N) can be trusted.
!
  implicit none

  integer ( kind = 4 ) nb

  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpem
  real ( kind = 8 ) alp2em
  real ( kind = 8 ) b(nb)
  real ( kind = 8 ) capp
  real ( kind = 8 ) capq
  real ( kind = 8 ) eighth
  real ( kind = 8 ) em
  real ( kind = 8 ) en
  real ( kind = 8 ) enmten
  real ( kind = 8 ) ensig
  real ( kind = 8 ) enten
  real ( kind = 8 ) fact(25)
  real ( kind = 8 ) four
  real ( kind = 8 ) gnu
  real ( kind = 8 ) half
  real ( kind = 8 ) halfx
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  logical ( kind = 4 ) jump
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) magx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nbmx
  integer ( kind = 4 ) ncalc
  integer ( kind = 4 ) nend
  integer ( kind = 4 ) nstart
  real ( kind = 8 ) one
  real ( kind = 8 ) one30
  real ( kind = 8 ) p
  real ( kind = 8 ) pi2
  real ( kind = 8 ) plast
  real ( kind = 8 ) pold
  real ( kind = 8 ) psave
  real ( kind = 8 ) psavel
  real ( kind = 8 ) rtnsig
  real ( kind = 8 ) s
  real ( kind = 8 ) sum
  real ( kind = 8 ) t
  real ( kind = 8 ) t1
  real ( kind = 8 ) tempa
  real ( kind = 8 ) tempb
  real ( kind = 8 ) tempc
  real ( kind = 8 ) test
  real ( kind = 8 ) three
  real ( kind = 8 ) three5
  real ( kind = 8 ) tover
  real ( kind = 8 ) two
  real ( kind = 8 ) twofiv
  real ( kind = 8 ) twopi1
  real ( kind = 8 ) twopi2
  real ( kind = 8 ) x
  real ( kind = 8 ) xc
  real ( kind = 8 ) xin
  real ( kind = 8 ) xk
  real ( kind = 8 ) xlarge
  real ( kind = 8 ) xm
  real ( kind = 8 ) vcos
  real ( kind = 8 ) vsin
  real ( kind = 8 ) z
  real ( kind = 8 ) zero
!
!  Mathematical constants
!
!  PI2    - 2 / PI
!  TWOPI1 - first few significant digits of 2 * PI
!  TWOPI2 - (2*PI - TWOPI) to working precision, i.e.,
!  TWOPI1 + TWOPI2 = 2 * PI to extra precision.
!
  data pi2 / 0.636619772367581343075535d0 /
  data twopi1 / 6.28125d0 /
  data twopi2 / 1.935307179586476925286767d-3 /
  data zero / 0.0d0 /
  data eighth / 0.125d0 /
  data half / 0.5d0 /
  data one / 1.0d0 /
  data two / 2.0d0 /
  data three / 3.0d0 /
  data four / 4.0d0 /
  data twofiv / 25.0d0 /
  data one30 / 130.0d0 /
  data three5 / 35.0d0 /
!
!  Machine-dependent parameters
!
  data enten / 1.0D+308 /
  data ensig / 1.0D+16 /
  data rtnsig / 1.0D-04 /
  data enmten / 8.90D-308 /
  data xlarge / 1.0D+04 /
!
!  Factorial(N)
!
  data fact / &
    1.0d0, &
    1.0d0, &
    2.0d0, &
    6.0d0, &
    24.0d0, &
    1.2d2, &
    7.2d2, &
    5.04d3, &
    4.032d4, &
    3.6288d5, &
    3.6288d6, &
    3.99168d7, &
    4.790016d8, &
    6.2270208d9, &
    8.71782912d10, &
    1.307674368d12, &
    2.0922789888d13, &
    3.55687428096d14, &
    6.402373705728d15, &
    1.21645100408832d17, &
    2.43290200817664d18, &
    5.109094217170944d19, &
    1.12400072777760768d21, &
    2.585201673888497664d22, &
    6.2044840173323943936d23 /

  jump = .false.
!
!  Check for out of range arguments.
!
  magx = int ( x )

  if ( &
    ( 0 < nb ) .and. &
    ( zero <= x ) .and. &
    ( x <= xlarge ) .and. &
    ( zero <= alpha ) .and. &
    ( alpha < one ) ) then
!
!  Initialize result array to zero.
!
    ncalc = nb
    b(1:nb) = zero
!
!  Branch to use 2-term ascending series for small X and asymptotic
!  form for large X when NB is not too large.
!
    if ( x < rtnsig ) then
!
!  Two-term ascending series for small X.
!
      tempa = one
      alpem = one + alpha

      if ( enmten < x ) then
        halfx = half * x
      else
        halfx = zero
      end if

      if ( alpha /= zero ) then
        tempa = halfx ** alpha / ( alpha * gamma ( alpha ) )
      end if

      if ( one < ( x + one ) ) then
        tempb = - halfx * halfx
      else
        tempb = zero
      end if

      b(1) = tempa + tempa * tempb / alpem

      if ( ( x /= zero ) .and. ( b(1) == zero ) ) then
        ncalc = 0
      end if

      if ( nb /= 1 ) then

        if ( x <= zero ) then

          do n = 2, nb
            b(n) = zero
          end do

        else
!
!  Calculate higher order functions.
!
          tempc = halfx

          if ( tempb /= zero ) then
            tover = enmten / tempb
          else
            tover = ( enmten + enmten ) / x
          end if

          do n = 2, nb

            tempa = tempa / alpem
            alpem = alpem + one

            tempa = tempa * tempc
            if ( tempa <= tover * alpem ) then
              tempa = zero
            end if

            b(n) = tempa + tempa * tempb / alpem

            if ( ( b(n) == zero ) .and. ( n < ncalc ) ) then
              ncalc = n - 1
            end if

          end do

        end if

      end if

    else if ( ( twofiv < x ) .and. ( nb <= magx + 1 ) ) then
!
!  Asymptotic series for 21.0 < X.
!
      xc = sqrt ( pi2 / x )
      xin = ( eighth / x ) ** 2

      if ( x < three5 ) then
        m = 11
      else if ( x < one30 ) then
        m = 8
      else
        m = 4
      end if

      xm = four * real ( m, kind = 8 )
!
!  Argument reduction for SIN and COS routines.
!
      t = aint ( x / ( twopi1 + twopi2 ) + half )
      z = ( ( x - t * twopi1 ) - t * twopi2 ) - ( alpha + half ) / pi2
      vsin = sin ( z )
      vcos = cos ( z )
      gnu = alpha + alpha

      do i = 1, 2

        s = ( ( xm - one ) - gnu ) * ( ( xm - one ) + gnu ) * xin * half
        t = ( gnu - ( xm - three ) ) * ( gnu + ( xm - three ) )
        capp = s * t / fact(2*m+1)
        t1 = ( gnu - ( xm + one ) ) * ( gnu + ( xm + one ) )
        capq = s * t1 / fact(2*m+2)
        xk = xm
        k = m + m
        t1 = t

        do j = 2, m
          xk = xk - four
          s = ( ( xk - one ) - gnu ) * ( ( xk - one ) + gnu )
          t = ( gnu - ( xk - three ) ) * ( gnu + ( xk - three ) )
          capp = ( capp + one / fact(k-1) ) * s * t * xin
          capq = ( capq + one / fact(k) ) * s * t1 * xin
          k = k - 2
          t1 = t
        end do

        capp = capp + one
        capq = ( capq + one ) * ( gnu * gnu - one ) * ( eighth / x )
        b(i) = xc * ( capp * vcos - capq * vsin )

        if ( nb == 1 ) then
          return
        end if

        t = vsin
        vsin = - vcos
        vcos = t
        gnu = gnu + two

      end do
!
!  If 2 < NB, compute J(X,ORDER+I)  I = 2, NB-1
!
      if ( 2 < nb ) then
        gnu = alpha + alpha + two
        do j = 3, nb
          b(j) = gnu * b(j-1) / x - b(j-2)
          gnu = gnu + two
        end do
      end if
!
!  Use recurrence to generate results.  First initialize the
!  calculation of P*S.
!
    else

      nbmx = nb - magx
      n = magx + 1
      en = real ( n + n, kind = 8 ) + ( alpha + alpha )
      plast = one
      p = en / x
!
!  Calculate general significance test.
!
      test = ensig + ensig

      if ( 3 <= nbmx ) then
!
!  Calculate P*S until N = NB-1.  Check for possible overflow.
!
        tover = enten / ensig
        nstart = magx + 2
        nend = nb - 1
        en = real ( nstart + nstart, kind = 8 ) - two + ( alpha + alpha )

        do k = nstart, nend

          n = k
          en = en + two
          pold = plast
          plast = p
          p = en * plast / x - pold

          if ( tover < p ) then
!
!  To avoid overflow, divide P*S by TOVER.  Calculate P*S until 1 < ABS(P).
!
            tover = enten
            p = p / tover
            plast = plast / tover
            psave = p
            psavel = plast
            nstart = n + 1

            do

              n = n + 1
              en = en + two
              pold = plast
              plast = p
              p = en * plast / x - pold
              if ( one < p ) then
                exit
              end if

            end do

            tempb = en / x
!
!  Calculate backward test and find NCALC, the highest N such that
!  the test is passed.
!
            test = pold * plast * ( half - half / ( tempb * tempb ) )
            test = test / ensig
            p = plast * tover
            n = n - 1
            en = en - two
            nend = min ( nb, n )

            do l = nstart, nend
              pold = psavel
              psavel = psave
              psave = en * psavel / x - pold
              if ( test < psave * psavel ) then
                ncalc = l - 1
                jump = .true.
                exit
              end if
            end do

            if ( jump ) then
              exit
            end if

            ncalc = nend
            jump = .true.
            exit

          end if

        end do

        if ( .not. jump ) then

          n = nend
          en = real ( n + n, kind = 8 ) + ( alpha + alpha )
!
!  Calculate special significance test for 2 < NBMX.
!
          test = max ( test, sqrt ( plast * ensig ) * sqrt ( p + p ) )

        end if

      end if
!
!  Calculate P*S until significance test passes.
!
      if ( .not. jump ) then

        do

          n = n + 1
          en = en + two
          pold = plast
          plast = p
          p = en * plast / x - pold

          if ( test <= p ) then
            exit
          end if

        end do

      end if
!
!  Initialize the backward recursion and the normalization sum.
!
      n = n + 1
      en = en + two
      tempb = zero
      tempa = one / p
      m = 2 * n - 4 * ( n / 2 )
      sum = zero
      em = real ( n / 2, kind = 8 )
      alpem = ( em - one ) + alpha
      alp2em = ( em + em ) + alpha
      if ( m /= 0 ) then
        sum = tempa * alpem * alp2em / em
      end if
      nend = n - nb

      if ( 0 < nend ) then
!
!  Recur backward via difference equation, calculating (but not
!  storing) B(N), until N = NB.
!
        do l = 1, nend

          n = n - 1
          en = en - two
          tempc = tempb
          tempb = tempa
          tempa = ( en * tempb ) / x - tempc
          m = 2 - m

          if ( m /= 0 ) then
            em = em - one
            alp2em = ( em + em ) + alpha
            if ( n == 1 ) then
              exit
            end if
            alpem = ( em - one ) + alpha
            if ( alpem == zero ) then
              alpem = one
            end if
            sum = ( sum + tempa * alp2em ) * alpem / em
          end if

        end do

      end if
!
!  Store B(NB).
!
      b(n) = tempa

      if ( 0 <= nend ) then

        if ( nb <= 1 ) then

          alp2em = alpha
          if ( ( alpha + one ) == one ) then
            alp2em = one
          end if
          sum = sum + b(1) * alp2em

          if ( ( alpha + one ) /= one ) then
            sum = sum * gamma ( alpha ) * ( x * half ) ** ( - alpha )
          end if

          tempa = enmten

          if ( one < sum ) then
            tempa = tempa * sum
          end if

          do n = 1, nb
            if ( abs ( b(n) ) < tempa ) then
              b(n) = zero
            end if
            b(n) = b(n) / sum
          end do

          return

        else
!
!  Calculate and store B(NB-1).
!
          n = n - 1
          en = en - two
          b(n) = ( en * tempa ) / x - tempb

          if ( n == 1 ) then

            em = em - one
            alp2em = ( em + em ) + alpha
            if ( alp2em == zero ) then
              alp2em = one
            end if
            sum = sum + b(1) * alp2em
!
!  Normalize.  Divide all B(N) by sum.
!
            if ( ( alpha + one ) /= one ) then
              sum = sum * gamma ( alpha ) * ( x * half ) ** ( - alpha )
            end if

            tempa = enmten

            if ( one < sum ) then
              tempa = tempa * sum
            end if
  
            do n = 1, nb
              if ( abs ( b(n) ) < tempa ) then
                b(n) = zero
              end if
              b(n) = b(n) / sum
            end do

            return

          end if

          m = 2 - m

          if ( m /= 0 ) then
            em = em - one
            alp2em = ( em + em ) + alpha
            alpem = ( em - one ) + alpha
            if ( alpem == zero ) then
              alpem = one
            end if
            sum = ( sum + b(n) * alp2em ) * alpem / em
          end if

        end if

      end if

      nend = n - 2

      if ( nend /= 0 ) then
!
!  Calculate via difference equation and store B(N), until N = 2.
!
        do l = 1, nend

          n = n - 1
          en = en - two
          b(n) = ( en * b(n+1) ) / x - b(n+2)
          m = 2 - m

          if ( m /= 0 ) then
            em = em - one
            alp2em = ( em + em ) + alpha
            alpem = ( em - one ) + alpha
            if ( alpem == zero ) then
              alpem = one
            end if
            sum = ( sum + b(n) * alp2em ) * alpem / em
          end if

        end do

      end if
!
!  Calculate B(1).
!
      b(1) = two * ( alpha + one ) * b(2) / x - b(3)

      em = em - one
      alp2em = ( em + em ) + alpha
      if ( alp2em == zero ) then
        alp2em = one
      end if
      sum = sum + b(1) * alp2em
!
!  Normalize.  Divide all B(N) by sum.
!
      if ( ( alpha + one ) /= one ) then
        sum = sum * gamma ( alpha ) * ( x * half ) ** ( - alpha )
      end if

      tempa = enmten

      if ( one < sum ) then
        tempa = tempa * sum
      end if

      do n = 1, nb
        if ( abs ( b(n) ) < tempa ) then
          b(n) = zero
        end if
        b(n) = b(n) / sum
      end do

    end if
!
!  Error return.
!
  else

    b(1) = zero
    ncalc = min ( nb, 0 ) - 1

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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
