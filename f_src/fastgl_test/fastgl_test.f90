program main

!*****************************************************************************80
!
!! MAIN is the main program for FASTGL_TEST.
!
!  Discussion:
!
!    FASTGL_TEST tests the FASTGL library.
!
!  Modified:
!
!    05 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ignace Bogaert,
!    Iteration-free computation of Gauss-Legendre quadrature nodes and weights,
!    SIAM Journal on Scientific Computing,
!    Volume 36, Number 3, 2014, pages A1008-1026.
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FASTGL_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the FASTGL library.'

  call besseljzero_test ( )
  call besselj1squared_test ( )
  call glpair_test ( )
  call glpairs_test ( )
  call glpairtabulated_test ( )
  call legendre_theta_test ( )
  call legendre_weight_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FASTGL_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine besseljzero_test ( )

!*****************************************************************************80
!
!! BESSELJZERO_TEST tests BESSELJZERO.
!
!  Discussion:
!
!    GFORTRAN provides the built in BESSEL_J0 function.
!
!  Modified:
!
!    04 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ignace Bogaert,
!    Iteration-free computation of Gauss-Legendre quadrature nodes and weights,
!    SIAM Journal on Scientific Computing,
!    Volume 36, Number 3, 2014, pages A1008-1026.
!
  implicit none

  real ( kind = 8 ) bessel_j0
  real ( kind = 8 ) j0x
  integer ( kind = 4 ) k
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BESSELJZERO_TEST:'
  write ( *, '(a)' ) '  BESSELJZERO returns the K-th zero of J0(X).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   K           X(K)                  J0(X(K))'
  write ( *, '(a)' ) ''

  do k = 1, 30
    call besseljzero ( k, x )
    j0x = bessel_j0 ( x )
    write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) k, x, j0x
  end do

  return
end
subroutine besselj1squared_test ( )

!*****************************************************************************80
!
!! BESSELJ1SQUARED_TEST tests BESSELJ1SQUARED.
!
!  Modified:
!
!    04 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ignace Bogaert,
!    Iteration-free computation of Gauss-Legendre quadrature nodes and weights,
!    SIAM Journal on Scientific Computing,
!    Volume 36, Number 3, 2014, pages A1008-1026.
!
  implicit none

  real ( kind = 8 ) bessel_j1
  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  integer ( kind = 4 ) k
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BESSELJ1SQUARED_TEST:'
  write ( *, '(a)' ) '  BESSELJ1SQUARED returns the square of the Bessel J1(X) function'
  write ( *, '(a)' ) '  at the K-th zero of J0(X).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   K           X(K)                    J1(X(K))^2                 BESSELJ1SQUARED'
  write ( *, '(a)' ) ''

  do k = 1, 30
    call besseljzero ( k, x )
    f1 = bessel_j1 ( x ) ** 2
    call besselj1squared ( k, f2 )
    write ( *, '(2x,i2,2x,g24.16,2x,g24.16,2x,g24.16)' ) k, x, f1, f2
  end do

  return
end
subroutine glpair_test ( )

!*****************************************************************************80
!
!! GLPAIR_TEST tests GLPAIR.
!
!  Discussion:
!
!    Test the numerical integration of ln(x) over the range [0,1]
!    Normally, one would not use Gauss-Legendre quadrature for this,
!    but for the sake of having an example with l > 100, this is included.
!
!  Modified:
!
!    03 January 2016
!
!  Author:
!
!    Ignace Bogaert
!
!  Reference:
!
!    Ignace Bogaert,
!    Iteration-free computation of Gauss-Legendre quadrature nodes and weights,
!    SIAM Journal on Scientific Computing,
!    Volume 36, Number 3, 2014, pages A1008-1026.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) theta
  real ( kind = 8 ) weight
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GLPAIR_TEST'
  write ( *, '(a)' ) '  Estimate integral ( 0 <= x <= 1 ) ln(x) dx.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    Nodes           Estimate'
  write ( *, '(a)' ) ''

  l = 1
  do p = 0, 6
    q = 0.0D+00
    do k = 1, l
      call glpair ( l, k, theta, weight, x )
      q = q + 0.5D+00 * weight * log ( 0.5D+00 * ( x + 1.0D+00 ) )
    end do
    write ( *, '(2x,i7,7x,g24.16)' ) l, q
    l = l * 10
  end do
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    Exact        -1.0'

  return
end
subroutine glpairs_test ( )

!*****************************************************************************80
!
!! GLPAIRS_TEST tests GLPAIRS.
!
!  Discussion:
!
!    Test the numerical integration of cos(1000 x) over the range [-1,1]
!    for varying number of Gauss-Legendre quadrature nodes l.
!    The fact that only twelve digits of accuracy are obtained is due to the 
!    condition number of the summation.
!
!  Modified:
!
!    05 January 2016
!
!  Author:
!
!    Ignace Bogaert
!
!  Reference:
!
!    Ignace Bogaert,
!    Iteration-free computation of Gauss-Legendre quadrature nodes and weights,
!    SIAM Journal on Scientific Computing,
!    Volume 36, Number 3, 2014, pages A1008-1026.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) q
  real ( kind = 8 ) theta
  real ( kind = 8 ) weight
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GLPAIRS_TEST:'
  write ( *, '(a)' ) '  integral ( -1 <= x <= 1 ) cos(1000 x) dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    Nodes           Estimate'
  write ( *, '(a)' ) ''

  do l = 500, 600, 20

    q = 0.0D+00

    do k = 1, l
      call glpairs ( l, k, theta, weight, x )
      q = q + weight * cos ( 1000.0D+00 * x )
    end do

    write ( *, '(2x,i7,7x,g24.16)' ) l, q

  end do

  write ( *, '(a)' ) ''
  write ( *, '(4x,a,7x,g24.16)' ) 'Exact', 0.002D+00 * sin ( 1000.0D+00 )

  return
end
subroutine glpairtabulated_test ( )

!*****************************************************************************80
!
!! GLPAIRTABULATED_TEST tests GLPAIRTABULATED.
!
!  Discussion:
!
!    Test the numerical integration of exp(x) over the range [-1,1]
!    for varying number of Gauss-Legendre quadrature nodes l.
!
!  Modified:
!
!    05 January 2016
!
!  Author:
!
!    Ignace Bogaert
!
!  Reference:
!
!    Ignace Bogaert,
!    Iteration-free computation of Gauss-Legendre quadrature nodes and weights,
!    SIAM Journal on Scientific Computing,
!    Volume 36, Number 3, 2014, pages A1008-1026.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) q
  real ( kind = 8 ) theta
  real ( kind = 8 ) weight
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GLPAIRTABULATED_TEST:'
  write ( *, '(a)' ) '  integral ( -1 <= x <= 1 ) exp(x) dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    Nodes           Estimate'
  write ( *, '(a)' ) ''

  do l = 1, 9
    q = 0.0D+00
    do k = 1, l
      call glpairtabulated ( l, k, theta, weight, x )
      q = q + weight * exp ( x )
    end do
    write ( *, '(2x,i7,2x,g24.16)' ) l, q
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,g24.16)' ) '    Exact  ', exp ( 1.0D+00 ) - exp ( -1.0D+00 )

  return
end
subroutine legendre_theta_test ( )

!*****************************************************************************80
!
!! LEGENDRE_THETA_TEST tests LEGENDRE_THETA.
!
!  Modified:
!
!    05 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ignace Bogaert,
!    Iteration-free computation of Gauss-Legendre quadrature nodes and weights,
!    SIAM Journal on Scientific Computing,
!    Volume 36, Number 3, 2014, pages A1008-1026.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) theta

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEGENDRE_THETA_TEST:'
  write ( *, '(a)' ) '  LEGENDRE_THETA returns the K-th theta value for'
  write ( *, '(a)' ) '  a Gauss Legendre rule of order L.'

  do l = 1, 10
    write ( *, '(a)' ) ''
    write ( *, '(a,i3)' ) '  Gauss Legendre rule of order ', l
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '   K       Theta      Cos(Theta)'
    write ( *, '(a)' ) ''
    do k = 1, l
      call legendre_theta ( l, k, theta )
      write ( *, '(2x,i2,2x,g14.6,2x,g14.6)' ) k, theta, cos ( theta )
    end do
  end do

  return
end
subroutine legendre_weight_test ( )

!*****************************************************************************80
!
!! LEGENDRE_WEIGHT_TEST tests LEGENDRE_WEIGHT.
!
!  Modified:
!
!    05 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ignace Bogaert,
!    Iteration-free computation of Gauss-Legendre quadrature nodes and weights,
!    SIAM Journal on Scientific Computing,
!    Volume 36, Number 3, 2014, pages A1008-1026.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) weight

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEGENDRE_WEIGHT_TEST:'
  write ( *, '(a)' ) '  LEGENDRE_WEIGHT returns the K-th weight for'
  write ( *, '(a)' ) '  a Gauss Legendre rule of order L.'

  do l = 1, 10
    write ( *, '(a)' ) ''
    write ( *, '(a,i3)' ) '  Gauss Legendre rule of order ', l
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '   K      Weight'
    write ( *, '(a)' ) ''
    do k = 1, l
      call legendre_weight ( l, k, weight )
      write ( *, '(2x,i2,2x,g14.6,2x,g14.6)' ) k, weight
    end do
  end do

  return
end
