program main

!*****************************************************************************80
!
!! MAIN is the main program for QUADMOM_TEST.
!
!  Discussion:
!
!    QUADMOM_TEST tests the QUADMOM library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUADMOM_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the QUADMOM library.'

  call quadmom_prb01 ( )
  call quadmom_prb02 ( )
  call quadmom_prb03 ( )
  call quadmom_prb04 ( )
  call quadmom_prb05 ( )
  call quadmom_prb06 ( )
  call quadmom_prb07 ( )
  call quadmom_prb08 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUADMOM_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

 stop 0
end
subroutine quadmom_prb01 ( )

!*****************************************************************************80
!
!! QUADMOM_PRB01 tests the QUADMOM procedure for the Legendre weight.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Gene Golub, John Welsch,
!    Calculation of Gaussian Quadrature Rules,
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 221-230.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha2
  real ( kind = 8 ) b
  real ( kind = 8 ) beta2
  integer ( kind = 4 ) kind
  integer ( kind = 4 ) lu
  integer ( kind = 4 ) m
  real ( kind = 8 ), allocatable :: moment(:)
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w1(:)
  real ( kind = 8 ), allocatable :: w2(:)
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write (  *, '(a)' ) ''
  write (  *, '(a)' ) 'QUADMOM_PRB01:'
  write (  *, '(a)' ) '  Compute the points and weights of a quadrature rule'
  write (  *, '(a)' ) '  for the Legendre weight, rho(x)=1, over [-1,+1],'
  write (  *, '(a)' ) '  using Golub and Welsch''s moment method.'
  write (  *, '(a)' ) '  Compare with a standard calculation.'
!
!  N is the order of the rule we want to compute.
!
  n = 5
!
!  Compute M = 2*N+1 moments for the Legendre weight on [-1,+1].
!
  m = 2 * n + 1
  a = -1.0D+00
  b = 1.0D+00
  allocate ( moment(0:m-1) )

  call moments_legendre ( m, a, b, moment )
!
!  Compute the points and weights by the method of moments.
!
  allocate ( x1(1:n) )
  allocate ( w1(1:n) )

  call moment_method ( n, moment, x1, w1 )
!
!  Compute the points and weights the usual way.
!
  kind = 1
  alpha2 = 0.0D+00
  beta2 = 0.0D+00
  a = -1.0D+00
  b = +1.0D+00
  lu = 0
  allocate ( x2(1:n) )
  allocate ( w2(1:n) )

  call cgqf ( n, kind, alpha2, beta2, a, b, lu, x2, w2 )
!
!  Compare the results.
!
  call r8vec2_print ( n, x1, x2, &
    '  Points from GW moment and orthogonal polynomial methods:' )

  call r8vec2_print ( n, w1, w2, &
    '  Weights from GW moment and orthogonal polynomial methods:' )
!
!  Free memory.
!
  deallocate ( moment )
  deallocate ( w1 )
  deallocate ( w2 )
  deallocate ( x1 )
  deallocate ( x2 )

  return
end
subroutine quadmom_prb02 ( )

!*****************************************************************************80
!
!! QUADMOM_PRB02 tests the QUADMOM procedure for the standard Gaussian weight.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Gene Golub, John Welsch,
!    Calculation of Gaussian Quadrature Rules,
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 221-230.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha2
  real ( kind = 8 ) b
  real ( kind = 8 ) beta2
  integer ( kind = 4 ) kind
  integer ( kind = 4 ) lu
  integer ( kind = 4 ) m
  real ( kind = 8 ), allocatable :: moment(:)
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) sigma
  real ( kind = 8 ), allocatable :: w1(:)
  real ( kind = 8 ), allocatable :: w2(:)
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'QUADMOM_PRB02:'
  write ( *, '(a)' ) '  Compute the points and weights of a quadrature rule for'
  write ( *, '(a)' ) '  the standard Gaussian weight, rho(x)=exp(-x^2/2)/sqrt(2pi),'
  write ( *, '(a)' ) '  over (-oo,+oo), using Golub and Welsch''s moment method.'
  write ( *, '(a)' ) '  Compare with a standard calculation.'
!
!  N is the order of the quadrature rule.
!
  n = 5
!
!  Compute the M = 2 * N + 1 moments for the standard Gaussian weight on (-oo,+oo).
!
  m = 2 * n + 1
  allocate ( moment(0:m-1) )

  call moments_normal_01 ( m, moment )
!
!  Compute the points and weights by the method of moments.
!
  allocate ( x1(1:n) )
  allocate ( w1(1:n) )

  call moment_method ( n, moment, x1, w1 )
!
!  Compute the points and weights the usual way.
!
  kind = 6
  alpha2 = 0.0D+00
  beta2 = 0.0D+00
  a = 0.0D+00
  b = +0.5D+00
  lu = 0
  allocate ( x2(1:n) )
  allocate ( w2(1:n) )

  call cgqf ( n, kind, alpha2, beta2, a, b, lu, x2, w2 )
!
!  The CGQF weights need to be normalized by sigma * sqrt ( 2 * pi )
!  because they don't divide the Gaussian PDF by that factor.
!
  sigma = 1.0D+00
  w2(1:n) = w2(1:n) / sigma / sqrt ( 2.0D+00 * pi )
!
!  Compare the results.
!
  call r8vec2_print ( n, x1, x2, &
    '  Points from GW moment and orthogonal polynomial methods:' )

  call r8vec2_print ( n, w1, w2, &
    '  Weights from GW moment and orthogonal polynomial methods:' )
!
!  Free memory.
!
  deallocate ( moment )
  deallocate ( w1 )
  deallocate ( w2 )
  deallocate ( x1 )
  deallocate ( x2 )

  return
end
subroutine quadmom_prb03 ( )

!*****************************************************************************80
!
!! QUADMOM_PRB03 tests the QUADMOM procedure for the general Gaussian weight.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Gene Golub, John Welsch,
!    Calculation of Gaussian Quadrature Rules,
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 221-230.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha2
  real ( kind = 8 ) b
  real ( kind = 8 ) beta2
  integer ( kind = 4 ) kind
  integer ( kind = 4 ) lu
  integer ( kind = 4 ) m
  real ( kind = 8 ), allocatable :: moment(:)
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) sigma
  real ( kind = 8 ), allocatable :: w1(:)
  real ( kind = 8 ), allocatable :: w2(:)
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUADMOM_PRB03:'
  write ( *, '(a)' ) '  Compute the points and weights of a quadrature rule for'
  write ( *, '(a)' ) '  a general Gaussian weight,'
  write ( *, '(a)' ) '  rho(mu,s;x)=exp(-((x-mu)/sigma)^2/2)/sigma^2/sqrt(2pi),'
  write ( *, '(a)' ) '  over (-oo,+oo), using Golub and Welsch''s moment method.'
  write ( *, '(a)' ) '  Compare with a standard calculation.'
!
!  N is the order of the quadrature rule.
!
  n = 5
!
!  Compute the M = 2 * N + 1 moments for a general Gaussian weight on (-oo,+oo).
!
  m = 2 * n + 1
  mu = 1.0D+00
  sigma = 2.0D+00
  allocate ( moment(0:m-1) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU = ', mu
  write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma

  call moments_normal ( m, mu, sigma, moment )
!
!  Compute the points and weights by the method of moments.
!
  allocate ( x1(1:n) )
  allocate ( w1(1:n) )

  call moment_method ( n, moment, x1, w1 )
!
!  Compute the points and weights the usual way.
!
  kind = 6
  alpha2 = 0.0D+00
  beta2 = 0.0D+00
  a = 1.0D+00
  b = 0.5D+00 / sigma ** 2
  lu = 0
  allocate ( x2(1:n) )
  allocate ( w2(1:n) )

  call cgqf ( n, kind, alpha2, beta2, a, b, lu, x2, w2 )
!
!  The CGQF weights need to be normalized by sigma * sqrt ( 2 * pi )
!  because they don't divide the Gaussian PDF by that factor.
!
  w2(1:n) = w2(1:n) / sigma / sqrt ( 2.0D+00 * pi )
!
!  Compare the results.
!
  call r8vec2_print ( n, x1, x2, &
    '  Points from GW moment and orthogonal polynomial methods:' )

  call r8vec2_print ( n, w1, w2, &
    '  Weights from GW moment and orthogonal polynomial methods:' )
!
!  Free memory.
!
  deallocate ( moment )
  deallocate ( w1 )
  deallocate ( w2 )
  deallocate ( x1 )
  deallocate ( x2 )

  return
end
subroutine quadmom_prb04 ( )

!*****************************************************************************80
!
!! QUADMOM_PRB04 tests the QUADMOM procedure for the Laguerre weight.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Gene Golub, John Welsch,
!    Calculation of Gaussian Quadrature Rules,
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 221-230.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha2
  real ( kind = 8 ) b
  real ( kind = 8 ) beta2
  integer ( kind = 4 ) kind
  integer ( kind = 4 ) lu
  integer ( kind = 4 ) m
  real ( kind = 8 ), allocatable :: moment(:)
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ), allocatable :: w1(:)
  real ( kind = 8 ), allocatable :: w2(:)
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUADMOM_PRB04:'
  write ( *, '(a)' ) '  Compute the points and weights of a quadrature rule for'
  write ( *, '(a)' ) '  the Laguerre weight, rho(x)=exp(-x),'
  write ( *, '(a)' ) '  over [0,+oo), using Golub and Welsch''s moment method.'
  write ( *, '(a)' ) '  Compare with a standard calculation.'
!
!  N is the order of the quadrature rule.
!
  n = 5
!
!  Compute the M = 2 * N + 1 moments for the Laguerre weight on [0,+oo).
!
  m = 2 * n + 1
  allocate ( moment(0:m-1) )

  call moments_laguerre ( m, moment )
!
!  Compute the points and weights by the method of moments.
!
  allocate ( x1(1:n) )
  allocate ( w1(1:n) )

  call moment_method ( n, moment, x1, w1 )
!
!  Compute the points and weights the usual way.
!
  kind = 5
  alpha2 = 0.0D+00
  beta2 = 0.0D+00
  a = 0.0D+00
  b = +1.0D+00
  lu = 0
  allocate ( x2(1:n) )
  allocate ( w2(1:n) )

  call cgqf ( n, kind, alpha2, beta2, a, b, lu, x2, w2 )
!
!  Compare the results.
!
  call r8vec2_print ( n, x1, x2, &
    '  Points from GW moment and orthogonal polynomial methods:' )

  call r8vec2_print ( n, w1, w2, &
    '  Weights from GW moment and orthogonal polynomial methods:' )
!
!  Free memory.
!
  deallocate ( moment )
  deallocate ( w1 )
  deallocate ( w2 )
  deallocate ( x1 )
  deallocate ( x2 )

  return
end
subroutine quadmom_prb05 ( )

!*****************************************************************************80
!
!! QUADMOM_PRB05 tests the QUADMOM procedure for the truncated normal weight.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Gene Golub, John Welsch,
!    Calculation of Gaussian Quadrature Rules,
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 221-230.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) m
  real ( kind = 8 ), allocatable :: moment(:)
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n
  real ( kind = 8 ) sigma
  real ( kind = 8 ), allocatable :: w1(:)
  real ( kind = 8 ), allocatable :: x1(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUADMOM_PRB05:'
  write ( *, '(a)' ) '  Compute the points and weights of a quadrature rule for'
  write ( *, '(a)' ) '  a truncated normal weight,'
  write ( *, '(a)' ) '  rho(mu,s;x)=exp(-((x-mu)/sigma)^2/2)/sigma^2/sqrt(2pi),'
  write ( *, '(a)' ) '  over [a,b], using Golub and Welsch''s moment method.'
!
!  N is the order of the quadrature rule.
!
  n = 5
!
!  Compute the M = 2 * N + 1 moments.
!
  m = 2 * n + 1
  mu = 100.0D+00
  sigma = 25.0D+00
  a = 50.0D+00
  b = 150.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU = ', mu
  write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
  write ( *, '(a,g14.6)' ) '  A = ', a
  write ( *, '(a,g14.6)' ) '  B = ', b

  allocate ( moment(0:m-1) )

  call moments_truncated_normal_ab ( m, mu, sigma, a, b, moment )
!
!  Compute the points and weights by the method of moments.
!
  allocate ( x1(1:n) )
  allocate ( w1(1:n) )

  call moment_method ( n, moment, x1, w1 )
!
!  Print the results.
!
  call r8vec_print ( n, x1, '  Points from GW moment method:' )

  call r8vec_print ( n, w1, '  Weights from GW moment method:' )
!
!  Free memory.
!
  deallocate ( moment )
  deallocate ( w1 )
  deallocate ( x1 )

  return
end
subroutine quadmom_prb06 ( )

!*****************************************************************************80
!
!! QUADMOM_PRB06 tests QUADMOM for the lower truncated normal weight.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Gene Golub, John Welsch,
!    Calculation of Gaussian Quadrature Rules,
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 221-230.
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) m
  real ( kind = 8 ), allocatable :: moment(:)
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n
  real ( kind = 8 ) sigma
  real ( kind = 8 ), allocatable :: w1(:)
  real ( kind = 8 ), allocatable :: x1(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUADMOM_PRB06:'
  write ( *, '(a)' ) '  Compute the points and weights of a quadrature rule for'
  write ( *, '(a)' ) '  a lower truncated normal weight,'
  write ( *, '(a)' ) '  rho(mu,s;x)=exp(-((x-mu)/sigma)^2/2)/sigma^2/sqrt(2pi),'
  write ( *, '(a)' ) '  over [a,+oo), using Golub and Welsch''s moment method.'
!
!  N is the order of the quadrature rule.
!
  n = 9
!
!  Compute the M = 2 * N + 1 moments.
!
  m = 2 * n + 1
  mu = 2.0D+00
  sigma = 0.5D+00
  a = 0.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU = ', mu
  write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
  write ( *, '(a,g14.6)' ) '  A = ', a

  allocate ( moment(0:m-1) )

  call moments_truncated_normal_a ( m, mu, sigma, a, moment )
!
!  Compute the points and weights by the method of moments.
!
  allocate ( x1(1:n) )
  allocate ( w1(1:n) )

  call moment_method ( n, moment, x1, w1 )
!
!  Print the results.
!
  call r8vec_print ( n, x1, '  Points from GW moment method:' )

  call r8vec_print ( n, w1, '  Weights from GW moment method:' )
!
!  Free memory.
!
  deallocate ( moment )
  deallocate ( w1 )
  deallocate ( x1 )

  return
end
subroutine quadmom_prb07 ( )

!*****************************************************************************80
!
!! QUADMOM_PRB07 tests QUADMOM for the upper truncated normal weight.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Gene Golub, John Welsch,
!    Calculation of Gaussian Quadrature Rules,
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 221-230.
!
  implicit none

  real ( kind = 8 ) b
  integer ( kind = 4 ) m
  real ( kind = 8 ), allocatable :: moment(:)
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n
  real ( kind = 8 ) sigma
  real ( kind = 8 ), allocatable :: w1(:)
  real ( kind = 8 ), allocatable :: x1(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUADMOM_PRB07:'
  write ( *, '(a)' ) '  Compute the points and weights of a quadrature rule for'
  write ( *, '(a)' ) '  an upper truncated normal weight,'
  write ( *, '(a)' ) '  rho(mu,s;x)=exp(-((x-mu)/sigma)^2/2)/sigma^2/sqrt(2pi),'
  write ( *, '(a)' ) '  over (-oo,b], using Golub and Welsch''s moment method.'
!
!  N is the order of the quadrature rule.
!
  n = 9
!
!  Compute the M = 2 * N + 1 moments.
!
  m = 2 * n + 1
  mu = 2.0D+00
  sigma = 0.5D+00
  b = 3.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU = ', mu
  write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
  write ( *, '(a,g14.6)' ) '  B = ', b

  allocate ( moment(0:m-1) )

  call moments_truncated_normal_b ( m, mu, sigma, b, moment )
!
!  Compute the points and weights by the method of moments.
!
  allocate ( x1(1:n) )
  allocate ( w1(1:n) )

  call moment_method ( n, moment, x1, w1 )
!
!  Print the results.
!
  call r8vec_print ( n, x1, '  Points from GW moment method:' )

  call r8vec_print ( n, w1, '  Weights from GW moment method:' )
!
!  Free memory.
!
  deallocate ( moment )
  deallocate ( w1 )
  deallocate ( x1 )

  return
end
subroutine quadmom_prb08 ( )

!*****************************************************************************80
!
!! QUADMOM_PRB08 integrates sin(x) against a lower truncated normal weight.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Gene Golub, John Welsch,
!    Calculation of Gaussian Quadrature Rules,
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 221-230.
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  real ( kind = 8 ), allocatable :: moment(:)
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ) sigma
  real ( kind = 8 ), allocatable :: w1(:)
  real ( kind = 8 ), allocatable :: x1(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUADMOM_PRB08:'
  write ( *, '(a)' ) '  Integrate sin(x) against a lower truncated normal weight.'

  mu = 0.0D+00
  sigma = 1.0D+00
  a = -3.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MU = ', mu
  write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
  write ( *, '(a,g14.6)' ) '  A = ', a
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   N    Estimate'
  write ( *, '(a)' ) ' '
!
!  N is the order of the quadrature rule.
!
  do n = 1, 9
!
!  Compute the M = 2 * N + 1 moments.
!
    m = 2 * n + 1

    allocate ( moment(0:m-1) )

    call moments_truncated_normal_a ( m, mu, sigma, a, moment )
!
!  Compute the points and weights by the method of moments.
!
    allocate ( x1(1:n) )
    allocate ( w1(1:n) )

    call moment_method ( n, moment, x1, w1 )

    q = 0.0D+00
    do i = 1, n
      q = q + w1(i) * sin ( x1(i) )
    end do

    write ( *, '(2x,i2,2x,g14.6)' ) n, q
!
!  Free memory.
!
    deallocate ( moment )
    deallocate ( w1 )
    deallocate ( x1 )

  end do

  return
end
