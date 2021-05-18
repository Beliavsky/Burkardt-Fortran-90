function r8_erf ( x )

!*****************************************************************************80
!
!! R8_ERF evaluates the error function.
!
!  Discussion:
!
!    Since some compilers already supply a routine named ERF which evaluates
!    the error function, this routine has been given a distinct, if
!    somewhat unnatural, name.
!    
!    The function is defined by:
!
!      ERF(X) = ( 2 / sqrt ( PI ) ) * Integral ( 0 <= t <= X ) exp ( - t^2 ) dt
!
!    Properties of the function include:
!
!      Limit ( X -> -oo ) ERF(X) =          -1.0;
!                         ERF(0) =           0.0;
!                         ERF(0.476936...) = 0.5;
!      Limit ( X -> +oo ) ERF(X) =          +1.0.
!
!      0.5 * ( ERF(X/sqrt(2)) + 1 ) = Normal_01_CDF(X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 November 2006
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody,
!    Rational Chebyshev Approximations for the Error Function,
!    Mathematics of Computation, 
!    1969, pages 631-638.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the error function.
!
!    Output, real ( kind = 8 ) R8_ERF, the value of the error function.
!
  implicit none

  real ( kind = 8 ), parameter, dimension ( 5 ) :: a = (/ &
    3.16112374387056560D+00, &
    1.13864154151050156D+02, &
    3.77485237685302021D+02, &
    3.20937758913846947D+03, &
    1.85777706184603153D-01 /)
  real ( kind = 8 ), parameter, dimension ( 4 ) :: b = (/ &
    2.36012909523441209D+01, &
    2.44024637934444173D+02, &
    1.28261652607737228D+03, &
    2.84423683343917062D+03 /)
  real ( kind = 8 ), parameter, dimension ( 9 ) :: c = (/ &
    5.64188496988670089D-01, &
    8.88314979438837594D+00, &
    6.61191906371416295D+01, &
    2.98635138197400131D+02, &
    8.81952221241769090D+02, &
    1.71204761263407058D+03, &
    2.05107837782607147D+03, &
    1.23033935479799725D+03, &
    2.15311535474403846D-08 /)
  real ( kind = 8 ), parameter, dimension ( 8 ) :: d = (/ &
    1.57449261107098347D+01, &
    1.17693950891312499D+02, &
    5.37181101862009858D+02, &
    1.62138957456669019D+03, &
    3.29079923573345963D+03, &
    4.36261909014324716D+03, &
    3.43936767414372164D+03, &
    1.23033935480374942D+03 /)
  real ( kind = 8 ) del
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter, dimension ( 6 ) :: p = (/ &
    3.05326634961232344D-01, &
    3.60344899949804439D-01, &
    1.25781726111229246D-01, &
    1.60837851487422766D-02, &
    6.58749161529837803D-04, &
    1.63153871373020978D-02 /)
  real ( kind = 8 ), parameter, dimension ( 5 ) :: q = (/ &
    2.56852019228982242D+00, &
    1.87295284992346047D+00, &
    5.27905102951428412D-01, &
    6.05183413124413191D-02, &
    2.33520497626869185D-03 /)
  real ( kind = 8 ) r8_erf
  real ( kind = 8 ), parameter :: sqrpi = 0.56418958354775628695D+00
  real ( kind = 8 ), parameter :: thresh = 0.46875D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) xabs
  real ( kind = 8 ), parameter :: xbig = 26.543D+00
  real ( kind = 8 ) xden
  real ( kind = 8 ) xnum
  real ( kind = 8 ) xsq

  xabs = abs ( ( x ) )
!
!  Evaluate ERF(X) for |X| <= 0.46875.
!
  if ( xabs <= thresh ) then

    if ( epsilon ( xabs ) < xabs ) then
      xsq = xabs * xabs
    else
      xsq = 0.0D+00
    end if

    xnum = a(5) * xsq
    xden = xsq
    do i = 1, 3
      xnum = ( xnum + a(i) ) * xsq
      xden = ( xden + b(i) ) * xsq
    end do

    r8_erf = x * ( xnum + a(4) ) / ( xden + b(4) )
!
!  Evaluate ERFC(X) for 0.46875 <= |X| <= 4.0.
!
  else if ( xabs <= 4.0D+00 ) then

    xnum = c(9) * xabs
    xden = xabs
    do i = 1, 7
      xnum = ( xnum + c(i) ) * xabs
      xden = ( xden + d(i) ) * xabs
    end do

    r8_erf = ( xnum + c(8) ) / ( xden + d(8) )
    xsq = real ( int ( xabs * 16.0D+00 ), kind = 8 ) / 16.0D+00
    del = ( xabs - xsq ) * ( xabs + xsq )
    r8_erf = exp ( - xsq * xsq ) * exp ( - del ) * r8_erf

    r8_erf = ( 0.5D+00 - r8_erf ) + 0.5D+00

    if ( x < 0.0D+00 ) then
      r8_erf = - r8_erf
    end if
!
!  Evaluate ERFC(X) for 4.0D+00 < |X|.
!
  else

    if ( xbig <= xabs ) then

      if ( 0.0D+00 < x ) then
        r8_erf = 1.0D+00
      else
        r8_erf = - 1.0D+00
      end if

    else

      xsq = 1.0D+00 / ( xabs * xabs )

      xnum = p(6) * xsq
      xden = xsq
      do i = 1, 4
        xnum = ( xnum + p(i) ) * xsq
        xden = ( xden + q(i) ) * xsq
      end do

      r8_erf = xsq * ( xnum + p(5) ) / ( xden + q(5) )
      r8_erf = ( sqrpi - r8_erf ) / xabs
      xsq = real ( int ( xabs * 16.0D+00 ), kind = 8 ) / 16.0D+00
      del = ( xabs - xsq ) * ( xabs + xsq )
      r8_erf = exp ( - xsq * xsq ) * exp ( - del ) * r8_erf

      r8_erf = ( 0.5D+00 - r8_erf ) + 0.5D+00

      if ( x < 0.0D+00 ) then
        r8_erf = - r8_erf
      end if

    end if

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
subroutine resid_burgers ( nu, n, x, y, z, t, ur, vr, wr, pr )

!*****************************************************************************80
!
!! RESID_BURGERS: residual of the Burgers exact Navier Stokes solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Martin Bazant, Henry Moffatt,
!    Exact solutions of the Navier-Stokes equations having steady 
!    vortex structures,
!    Journal of Fluid Mechanics,
!    Volume 541, pages 55-64, 2005.
!
!    Johannes Burgers,
!    A mathematical model illustrating the theory of turbulence,
!    Advances in Applied Mechanics,
!    Volume 1, pages 171-199, 1948.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) NU, the kinematic viscosity.
!
!    Input, integer ( kind = 4 ) N, the number of points at which the solution 
!    is to be evaluated.
!
!    Input, real ( kind = 8 ) X(N), Y(N), Z(N), the coordinates of the points.
!
!    Input, real ( kind = 8 ) T(N), the time coordinate or coordinates.
!
!    Output, real ( kind = 8 ) UR(N), VR(N), WR(N), PR(N), the residuals.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) pr(n)
  real ( kind = 8 ) px(n)
  real ( kind = 8 ) py(n)
  real ( kind = 8 ) pz(n)
  real ( kind = 8 ) r8_erf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) t(n)
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) ut(n)
  real ( kind = 8 ) ux(n)
  real ( kind = 8 ) uxx(n)
  real ( kind = 8 ) uy(n)
  real ( kind = 8 ) uyy(n)
  real ( kind = 8 ) uz(n)
  real ( kind = 8 ) uzz(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) vt(n)
  real ( kind = 8 ) vx(n)
  real ( kind = 8 ) vxx(n)
  real ( kind = 8 ) vy(n)
  real ( kind = 8 ) vyy(n)
  real ( kind = 8 ) vz(n)
  real ( kind = 8 ) vzz(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) wr(n)
  real ( kind = 8 ) wt(n)
  real ( kind = 8 ) wx(n)
  real ( kind = 8 ) wxx(n)
  real ( kind = 8 ) wy(n)
  real ( kind = 8 ) wyy(n)
  real ( kind = 8 ) wz(n)
  real ( kind = 8 ) wzz(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)
!
!  Form the functions and derivatives.
!
  u(1:n) =   2.0D+00 * x(1:n)
  ux(1:n) =  2.0D+00
  uxx(1:n) = 0.0D+00
  uy(1:n) =  0.0D+00
  uyy(1:n) = 0.0D+00
  uz(1:n) =  0.0D+00
  uzz(1:n) = 0.0D+00
  ut(1:n) =  0.0D+00

  v(1:n) =   - 2.0D+00 * y(1:n)
  vx(1:n) =  0.0D+00
  vxx(1:n) = 0.0D+00
  vy(1:n) =  - 2.0D+00
  vyy(1:n) = 0.0D+00
  vz(1:n) =  0.0D+00
  vzz(1:n) = 0.0D+00
  vt(1:n) =  0.0D+00

  do i = 1, n
    w(i) =   r8_erf ( y(i) / sqrt ( nu ) )
  end do
  wx(1:n) =  0.0D+00
  wxx(1:n) = 0.0D+00
  wy(1:n) =    2.0D+00 * sqrt ( 1.0D+00 / nu / r8_pi ) &
             * exp ( - y(1:n) ** 2 / nu )
  wyy(1:n) = - 4.0D+00 * sqrt ( 1.0D+00 / nu / r8_pi ) &
    * y(1:n) * exp ( - y(1:n) ** 2 / nu ) / nu
  wz(1:n) =  0.0D+00
  wzz(1:n) = 0.0D+00
  wt(1:n) =  0.0D+00

  p(1:n) = - 2.0D+00 * ( x(1:n) ** 2 + y(1:n) ** 2 )
  px(1:n) = - 4.0D+00 * x(1:n)
  py(1:n) = - 4.0D+00 * y(1:n)
  pz(1:n) = 0.0D+00
!
!  Evaluate the residuals.
!
  ur(1:n) = ut(1:n) &
    + u(1:n) * ux(1:n) + v(1:n) * uy(1:n) + w(1:n) * uz(1:n) + px(1:n) &
    - nu * ( uxx(1:n) + uyy(1:n) + uzz(1:n) )

  vr(1:n) = vt(1:n) &
    + u(1:n) * vx(1:n) + v(1:n) * vy(1:n) + w(1:n) * vz(1:n) + py(1:n) &
    - nu * ( vxx(1:n) + vyy(1:n) + vzz(1:n) )

  wr(1:n) = wt(1:n) &
    + u(1:n) * wx(1:n) + v(1:n) * wy(1:n) + w(1:n) * wz(1:n) + pz(1:n) &
    - nu * ( wxx(1:n) + wyy(1:n) + wzz(1:n) )

  pr(1:n) = ux(1:n) + vy(1:n) + wz(1:n)

  return
end
subroutine resid_ethier ( a, d, n, x, y, z, t, ur, vr, wr, pr )

!*****************************************************************************80
!
!! RESID_ETHIER: residual of the Ethier exact Navier Stokes solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    C Ross Ethier, David Steinman,
!    Exact fully 3D Navier-Stokes solutions for benchmarking,
!    International Journal for Numerical Methods in Fluids,
!    Volume 19, Number 5, March 1994, pages 369-375.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, D, the parameters.  Sample values are A = PI/4 
!    and D = PI/2.
!
!    Input, integer ( kind = 4 ) N, the number of points at which the solution 
!    is to be evaluated.
!
!    Input, real ( kind = 8 ) X(N), Y(N), Z(N), the coordinates of the points.
!
!    Input, real ( kind = 8 ) T(N), the time coordinates.
!
!    Output, real ( kind = 8 ) UR(N), VR(N), WR(N), PR(N), the residuals.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) cxy(n)
  real ( kind = 8 ) cyz(n)
  real ( kind = 8 ) czx(n)
  real ( kind = 8 ) d
  real ( kind = 8 ) e2t(n)
  real ( kind = 8 ) e2x(n)
  real ( kind = 8 ) e2y(n)
  real ( kind = 8 ) e2z(n)
  real ( kind = 8 ) e4t(n)
  real ( kind = 8 ) ex(n)
  real ( kind = 8 ) exy(n)
  real ( kind = 8 ) ey(n)
  real ( kind = 8 ) eyz(n)
  real ( kind = 8 ) ez(n)
  real ( kind = 8 ) ezx(n)
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) pr(n)
  real ( kind = 8 ) px(n)
  real ( kind = 8 ) py(n)
  real ( kind = 8 ) pz(n)
  real ( kind = 8 ) sxy(n)
  real ( kind = 8 ) syz(n)
  real ( kind = 8 ) szx(n)
  real ( kind = 8 ) t(n)
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) ut(n)
  real ( kind = 8 ) ux(n)
  real ( kind = 8 ) uxx(n)
  real ( kind = 8 ) uy(n)
  real ( kind = 8 ) uyy(n)
  real ( kind = 8 ) uz(n)
  real ( kind = 8 ) uzz(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) vt(n)
  real ( kind = 8 ) vx(n)
  real ( kind = 8 ) vxx(n)
  real ( kind = 8 ) vy(n)
  real ( kind = 8 ) vyy(n)
  real ( kind = 8 ) vz(n)
  real ( kind = 8 ) vzz(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) wr(n)
  real ( kind = 8 ) wt(n)
  real ( kind = 8 ) wx(n)
  real ( kind = 8 ) wxx(n)
  real ( kind = 8 ) wy(n)
  real ( kind = 8 ) wyy(n)
  real ( kind = 8 ) wz(n)
  real ( kind = 8 ) wzz(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)
!
!  Make some temporaries.
!
  ex(1:n) = exp ( a * x(1:n) )
  ey(1:n) = exp ( a * y(1:n) )
  ez(1:n) = exp ( a * z(1:n) )

  e2x(1:n) = exp ( 2.0D+00 * a * x(1:n) )
  e2y(1:n) = exp ( 2.0D+00 * a * y(1:n) )
  e2z(1:n) = exp ( 2.0D+00 * a * z(1:n) )

  e2t(1:n) = exp  ( -       d * d * t(1:n) )
  e4t(1:n) = exp  ( - 2.0 * d * d * t(1:n) )

  exy(1:n) = exp ( a * ( x(1:n) + y(1:n) ) )
  eyz(1:n) = exp ( a * ( y(1:n) + z(1:n) ) )
  ezx(1:n) = exp ( a * ( z(1:n) + x(1:n) ) )

  sxy(1:n) = sin ( a * x(1:n) + d * y(1:n) )
  syz(1:n) = sin ( a * y(1:n) + d * z(1:n) )
  szx(1:n) = sin ( a * z(1:n) + d * x(1:n) )

  cxy(1:n) = cos ( a * x(1:n) + d * y(1:n) )
  cyz(1:n) = cos ( a * y(1:n) + d * z(1:n) )
  czx(1:n) = cos ( a * z(1:n) + d * x(1:n) )
!
!  Form the functions and derivatives.
!
  u(1:n) =   -         a * (           ex(1:n) * syz(1:n) &
                             +         ez(1:n) * cxy(1:n) ) * e2t(1:n)
  ux(1:n) =  -         a * (       a * ex(1:n) * syz(1:n) &
                             -     a * ez(1:n) * sxy(1:n) ) * e2t(1:n)
  uxx(1:n) = -         a * (   a * a * ex(1:n) * syz(1:n) &
                             - a * a * ez(1:n) * cxy(1:n) ) * e2t(1:n)
  uy(1:n) =  -         a * (       a * ex(1:n) * cyz(1:n) &
                             -     d * ez(1:n) * sxy(1:n) ) * e2t(1:n)
  uyy(1:n) = -         a * ( - a * a * ex(1:n) * syz(1:n) &
                             - d * d * ez(1:n) * cxy(1:n) ) * e2t(1:n)
  uz(1:n) =  -         a * (       d * ex(1:n) * cyz(1:n) &
                             +     a * ez(1:n) * cxy(1:n) ) * e2t(1:n)
  uzz(1:n) =  -        a * ( - d * d * ex(1:n) * syz(1:n) &
                             + a * a * ez(1:n) * cxy(1:n) ) * e2t(1:n)
  ut(1:n) =  + d * d * a * (           ex(1:n) * syz(1:n) &
                             +         ez(1:n) * cxy(1:n) ) * e2t(1:n)

  v(1:n) =   -         a * (           ey(1:n) * szx(1:n) &
                             +         ex(1:n) * cyz(1:n) ) * e2t(1:n)
  vx(1:n) =  -         a * (       d * ey(1:n) * czx(1:n) &
                             +     a * ex(1:n) * cyz(1:n) ) * e2t(1:n)
  vxx(1:n) = -         a * ( - d * d * ey(1:n) * szx(1:n) &
                             + a * a * ex(1:n) * cyz(1:n) ) * e2t(1:n)
  vy(1:n) =  -         a * (       a * ey(1:n) * szx(1:n) &
                             -     a * ex(1:n) * syz(1:n) ) * e2t(1:n)
  vyy(1:n) = -         a * (   a * a * ey(1:n) * szx(1:n) &
                             - a * a * ex(1:n) * cyz(1:n) ) * e2t(1:n)
  vz(1:n) =  -         a * (       a * ey(1:n) * czx(1:n) &
                             -     d * ex(1:n) * syz(1:n) ) * e2t(1:n)
  vzz(1:n) =  -        a * ( - a * a * ey(1:n) * szx(1:n) &
                             - d * d * ex(1:n) * cyz(1:n) ) * e2t(1:n)
  vt(1:n) =  + d * d * a * (           ey(1:n) * szx(1:n) &
                             +         ex(1:n) * cyz(1:n) ) * e2t(1:n)

  w(1:n) =   -         a * (           ez(1:n) * sxy(1:n) &
                             +         ey(1:n) * czx(1:n) ) * e2t(1:n)
  wx(1:n) =  -         a * (       a * ez(1:n) * cxy(1:n) &
                             -     d * ey(1:n) * szx(1:n) ) * e2t(1:n)
  wxx(1:n) = -         a * ( - a * a * ez(1:n) * sxy(1:n) &
                             - d * d * ey(1:n) * czx(1:n) ) * e2t(1:n)
  wy(1:n) =  -         a * (       d * ez(1:n) * cxy(1:n) &
                             +     a * ey(1:n) * czx(1:n) ) * e2t(1:n)
  wyy(1:n) = -         a * ( - d * d * ez(1:n) * sxy(1:n) &
                             + a * a * ey(1:n) * czx(1:n) ) * e2t(1:n)
  wz(1:n) =  -         a * (       a * ez(1:n) * sxy(1:n) &
                             -     a * ey(1:n) * szx(1:n) ) * e2t(1:n)
  wzz(1:n) = -         a * (   a * a * ez(1:n) * sxy(1:n) &
                             - a * a * ey(1:n) * czx(1:n) ) * e2t(1:n)
  wt(1:n) =  + d * d * a * (           ez(1:n) * sxy(1:n) &
                             +         ey(1:n) * czx(1:n) ) * e2t(1:n)

  p(1:n) = - 0.5D+00 * a * a * e4t(1:n) * ( &
    + e2x(1:n) + 2.0D+00 * sxy(1:n) * czx(1:n) * eyz(1:n) &
    + e2y(1:n) + 2.0D+00 * syz(1:n) * cxy(1:n) * ezx(1:n) &
    + e2z(1:n) + 2.0D+00 * szx(1:n) * cyz(1:n) * exy(1:n) )

  px(1:n) = - 0.5D+00 * a * a * e4t(1:n) * ( &
    + 2.0D+00 * a * e2x(1:n) &
    + 2.0D+00 * a * cxy(1:n) * czx(1:n) * eyz(1:n) &
    - 2.0D+00 * d * sxy(1:n) * szx(1:n) * eyz(1:n) &
    - 2.0D+00 * a * syz(1:n) * sxy(1:n) * ezx(1:n) &
    + 2.0D+00 * a * syz(1:n) * cxy(1:n) * ezx(1:n) &
    + 2.0D+00 * d * czx(1:n) * cyz(1:n) * exy(1:n) &
    + 2.0D+00 * a * szx(1:n) * cyz(1:n) * exy(1:n) )

  py(1:n) = - 0.5D+00 * a * a * e4t(1:n) * ( &
    + 2.0D+00 * d * cxy(1:n) * czx(1:n) * eyz(1:n) &
    + 2.0D+00 * a * sxy(1:n) * czx(1:n) * eyz(1:n) &
    + 2.0D+00 * a * e2y(1:n) &
    + 2.0D+00 * a * cyz(1:n) * cxy(1:n) * ezx(1:n) &
    - 2.0D+00 * d * syz(1:n) * sxy(1:n) * ezx(1:n) &
    - 2.0D+00 * a * szx(1:n) * syz(1:n) * exy(1:n) &
    + 2.0D+00 * a * szx(1:n) * cyz(1:n) * exy(1:n) )

  pz(1:n) = - 0.5D+00 * a * a * e4t(1:n) * ( &
    - 2.0D+00 * a * sxy(1:n) * szx(1:n) * eyz(1:n) &
    + 2.0D+00 * a * sxy(1:n) * czx(1:n) * eyz(1:n) &
    + 2.0D+00 * d * cyz(1:n) * cxy(1:n) * ezx(1:n) &
    + 2.0D+00 * a * syz(1:n) * cxy(1:n) * ezx(1:n) &
    + 2.0D+00 * a * e2z(1:n) &
    + 2.0D+00 * a * czx(1:n) * cyz(1:n) * exy(1:n) &
    - 2.0D+00 * d * szx(1:n) * syz(1:n) * exy(1:n) )
!
!  Evaluate the residuals.
!
  ur(1:n) = ut(1:n) &
    + u(1:n) * ux(1:n) + v(1:n) * uy(1:n) + w(1:n) * uz(1:n) + px(1:n) &
    - ( uxx(1:n) + uyy(1:n) + uzz(1:n) )

  vr(1:n) = vt(1:n) &
    + u(1:n) * vx(1:n) + v(1:n) * vy(1:n) + w(1:n) * vz(1:n) + py(1:n) &
    - ( vxx(1:n) + vyy(1:n) + vzz(1:n) )

  wr(1:n) = wt(1:n) &
    + u(1:n) * wx(1:n) + v(1:n) * wy(1:n) + w(1:n) * wz(1:n) + pz(1:n) &
    - ( wxx(1:n) + wyy(1:n) + wzz(1:n) )

  pr(1:n) = ux(1:n) + vy(1:n) + wz(1:n)

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
subroutine uvwp_burgers ( nu, n, x, y, z, t, u, v, w, p )

!*****************************************************************************80
!
!! UVWP_BURGERS: the Burgers exact Navier Stokes solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Martin Bazant, Henry Moffatt,
!    Exact solutions of the Navier-Stokes equations having steady 
!    vortex structures,
!    Journal of Fluid Mechanics,
!    Volume 541, pages 55-64, 2005.
!
!    Johannes Burgers,
!    A mathematical model illustrating the theory of turbulence,
!    Advances in Applied Mechanics,
!    Volume 1, pages 171-199, 1948.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) NU, the kinematic viscosity.
!
!    Input, integer ( kind = 4 ) N, the number of points at which the solution 
!    is to be evaluated.
!
!    Input, real ( kind = 8 ) X(N), Y(N), Z(N), the coordinates of the points.
!
!    Input, real ( kind = 8 ) T(N), the time coordinate or coordinates.
!
!    Output, real ( kind = 8 ) U(N), V(N), W(N), P(N), the solution values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) r8_erf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) t(n)
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)

  u(1:n) =   2.0D+00 * x(1:n)
  v(1:n) =   - 2.0D+00 * y(1:n)
  do i = 1, n
    w(i) =   r8_erf ( y(i) / sqrt ( nu ) )
  end do
  p(1:n) = - 2.0D+00 * ( x(1:n) ** 2 + y(1:n) ** 2 )

  return
end
subroutine uvwp_ethier ( a, d, n, x, y, z, t, u, v, w, p )

!*****************************************************************************80
!
!! UVWP_ETHIER evaluates the Ethier exact Navier Stokes solution.
!
!  Discussion:
!
!    The reference asserts that the given velocity and pressure fields
!    are exact solutions for the 3D incompressible time-dependent
!    Navier Stokes equations over any region.
!
!    To define a typical problem, one chooses a bounded spatial region 
!    and a starting time, and then imposes boundary and initial conditions
!    by referencing the exact solution appropriately.
!
!    In the reference paper, a calculation is made for the cube centered
!    at (0,0,0) with a "radius" of 1 unit, and over the time interval
!    from t = 0 to t = 0.1, with parameters a = PI/4 and d = PI/2,
!    and with Dirichlet boundary conditions on all faces of the cube.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    C Ross Ethier, David Steinman,
!    Exact fully 3D Navier-Stokes solutions for benchmarking,
!    International Journal for Numerical Methods in Fluids,
!    Volume 19, Number 5, March 1994, pages 369-375.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, D, the parameters.  Sample values are 
!    A = PI/4 and D = PI/2.
!
!    Input, integer ( kind  N, the number of points at which the solution is to
!    be evaluated.
!
!    Input, real ( kind = 8 ) X(N), Y(N), Z(N), the coordinates of the points.
!
!    Input, real ( kind = 8 ) T, the time coordinate or coordinates.
!
!    Output, real ( kind = 8 ) U(N), V(N), W(N), P(N), the velocity components 
!    and pressure at each of the points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) cxy(n)
  real ( kind = 8 ) cyz(n)
  real ( kind = 8 ) czx(n)
  real ( kind = 8 ) d
  real ( kind = 8 ) e2t
  real ( kind = 8 ) ex(n)
  real ( kind = 8 ) exy(n)
  real ( kind = 8 ) ey(n)
  real ( kind = 8 ) eyz(n)
  real ( kind = 8 ) ez(n)
  real ( kind = 8 ) ezx(n)
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) sxy(n)
  real ( kind = 8 ) syz(n)
  real ( kind = 8 ) szx(n)
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)

  ex(1:n) = exp ( a * x(1:n) )
  ey(1:n) = exp ( a * y(1:n) )
  ez(1:n) = exp ( a * z(1:n) )

  e2t = exp  ( - d * d * t )

  exy(1:n) = exp ( a * ( x(1:n) + y(1:n) ) )
  eyz(1:n) = exp ( a * ( y(1:n) + z(1:n) ) )
  ezx(1:n) = exp ( a * ( z(1:n) + x(1:n) ) )

  sxy(1:n) = sin ( a * x(1:n) + d * y(1:n) )
  syz(1:n) = sin ( a * y(1:n) + d * z(1:n) )
  szx(1:n) = sin ( a * z(1:n) + d * x(1:n) )

  cxy(1:n) = cos ( a * x(1:n) + d * y(1:n) )
  cyz(1:n) = cos ( a * y(1:n) + d * z(1:n) )
  czx(1:n) = cos ( a * z(1:n) + d * x(1:n) )

  u(1:n) = - a * ( ex(1:n) * syz(1:n) + ez(1:n) * cxy(1:n) ) * e2t 
  v(1:n) = - a * ( ey(1:n) * szx(1:n) + ex(1:n) * cyz(1:n) ) * e2t
  w(1:n) = - a * ( ez(1:n) * sxy(1:n) + ey(1:n) * czx(1:n) ) * e2t
  p(1:n) = 0.5D+00 * a * a * e2t * e2t * ( &
    + ex(1:n) * ex(1:n) + 2.0D+00 * sxy(1:n) * czx(1:n) * eyz(1:n) &
    + ey(1:n) * ey(1:n) + 2.0D+00 * syz(1:n) * cxy(1:n) * ezx(1:n) &
    + ez(1:n) * ez(1:n) + 2.0D+00 * szx(1:n) * cyz(1:n) * exy(1:n) )

  return
end

