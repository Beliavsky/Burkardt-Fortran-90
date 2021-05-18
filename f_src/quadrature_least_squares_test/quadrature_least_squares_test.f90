program main

!*****************************************************************************80
!
!! MAIN is the main program for QLS_TEST.
!
!  Discussion:
!
!    QLS_TEST tests the QUADRATURE_LEAST_SQUARES library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QLS_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the QUADRATURE_LEAST_SQUARES library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QLS_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! QLS_TEST01 shows that we can compute the Newton-Cotes rules.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w1(:)
  real ( kind = 8 ), allocatable :: w2(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  WEIGHTS_LS computes the weights for a'
  write ( *, '(a)' ) '  least squares quadrature rule.'
!
!  Demonstrate the 5 point Newton-Cotes closed rule.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  W1 = classical Newton Cotes weights, N = 5'
  write ( *, '(a)' ) '  W2 = least squares weights, D = 4, N = 5'

  n = 5
  allocate ( x(1:n) )
  allocate ( w1(1:n) )

  call ncc_set ( n, x, w1 )
!
!  Using the same points, compute the least squares weights
!  for polynomial approximation up to degree 4.
!
  d = n - 1
  a = -1.0D+00
  b = +1.0D+00
  allocate ( w2(1:n) )

  call weights_ls ( d, a, b, n, x, w2 )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I        X(i)          W1(i)           W2(i)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i2,2x,f10.4,2x,g14.6,2x,g14.6)' ) i, x(i), w1(i), w2(i)
  end do

  deallocate ( w1 )
  deallocate ( w2 )
  deallocate ( x )
!
!  Look at a 9 point rule.
!  Note that Newton Cotes rules soon have many negative weights.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  W1 = classical Newton Cotes weights, N = 9'
  write ( *, '(a)' ) '  W2 = least squares weights, D = 4, N = 9'

  n = 9
  allocate ( x(1:n) )
  allocate ( w1(1:n) )

  call ncc_set ( n, x, w1 )
!
!  Using the same points, compute the least squares weights
!  for polynomial approximation up to degree 4.
!
  d = 4
  a = -1.0D+00
  b = +1.0D+00
  allocate ( w2(1:n) )

  call weights_ls ( d, a, b, n, x, w2 )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I        X(i)          W1(i)           W2(i)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i2,2x,f10.4,2x,g14.6,2x,g14.6)' ) i, x(i), w1(i), w2(i)
  end do

  deallocate ( w1 )
  deallocate ( w2 )
  deallocate ( x )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 uses random points as abscissas.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 50

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) exact
  real ( kind = 8 ) f(n)
  real ( kind = 8 ) q
  integer ( kind = 4 ) seed
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  WEIGHTS_LS computes the weights for a'
  write ( *, '(a)' ) '  least squares quadrature rule.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Pick 50 random values in [-1,+1].'
  write ( *, '(a)' ) '  Compare Monte Carlo (equal weight) integral estimate'
  write ( *, '(a)' ) '  to least squares estimates of degree D = 0, 1, 2, 3, 4.'
  write ( *, '(a)' ) '  For low values of D, the least squares estimate improves.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  As D increases, the estimate can deteriorate.'
!
!  Define the integration interval.
!
  a = -5.0D+00
  b = +5.0D+00
!
!  Get random values.
!
  seed = 123456789
  call r8vec_uniform_ab ( n, a, b, seed, x )
!
!  Evaluate the function.
!
  f(1:n) = 1.0D+00 / ( 1.0 + x(1:n) ** 2 )
  exact = atan ( b ) - atan ( a )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rule         Estimate          Error'
!
!  Get the MC estimate.
!
  q = ( b - a ) * sum ( f ) / real ( n, kind = 8 )
  e = abs ( q - exact );

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,2x,g14.6)' ) '  MC     ', q, e
  write ( *, '(a)' ) ''
!
!  Using the same points, compute the least squares weights
!  for polynomial approximation of degree D.
!
  do d = 0, 15

    call weights_ls ( d, a, b, n, x, w )
    q = dot_product ( w(1:n), f(1:n) )
    e = abs ( q - exact )
    write ( *, '(a,i2,2x,g14.6,2x,g14.6)' ) '  LS', d, q, e

  end do

  q = exact
  e = abs ( q - exact )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,2x,g14.6)' ) '  EXACT ', q, e

  return
end
