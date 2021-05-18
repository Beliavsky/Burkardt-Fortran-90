program main

!*****************************************************************************80
!
!! MAIN is the main program for LINE_FEKETE_RULE_TEST.
!
!  Discussion:
!
!    LINE_FEKETE_RULE_TEST tests the LINE_FEKETE_RULE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 3

  integer ( kind = 4 ) m
  integer ( kind = 4 ), dimension ( test_num ) :: m_test = (/ 5, 11, 21 /)
  integer ( kind = 4 ) test

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LINE_FEKETE_RULE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LINE_FEKETE_RULE library.'

  do test = 1, test_num
    m = m_test(test)
    call test01 ( m )
  end do

  do test = 1, test_num
    m = m_test(test)
    call test02 ( m )
  end do

  do test = 1, test_num
    m = m_test(test)
    call test03 ( m )
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LINE_FEKETE_RULE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( m )

!*****************************************************************************80
!
!! TEST01 seeks Fekete points in [-1,+1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Alvise Sommariva, Marco Vianello,
!    Computing approximate Fekete points by QR factorizations of Vandermonde 
!    matrices,
!    Computers and Mathematics with Applications,
!    Volume 57, 2009, pages 1324-1336.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the dimension of the polynomial space.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ), parameter :: n = 5001

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) nf
  real ( kind = 8 ) wf(m)
  real ( kind = 8 ) wf_sum
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xf(m)

  a = -1.0D+00
  b = +1.0D+00
  call r8vec_linspace ( n, a, b, x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  Seek Fekete points in [', a, ',', b, ']'
  write ( *, '(a,i6,a)' ) '  using ', n, ' equally spaced sample points'
  write ( *, '(a,i6)' ) '  for polynomials of degree M = ', m
  write ( *, '(a)' ) '  using the monomial basis and uniform weight.'

  call line_fekete_monomial ( m, a, b, n, x, nf, xf, wf )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  NF = ', nf
  call r8vec_print ( nf, xf, '  Estimated Fekete points XF:' )
  wf_sum = sum ( wf(1:nf) )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Sum(WF) = ', wf_sum

  return
end
subroutine test02 ( m )

!*****************************************************************************80
!
!! TEST02 seeks Fekete points in [-1,+1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    L Bos, N Levenberg,
!    On the calculation of approximate Fekete points: the univariate case,
!    Electronic Transactions on Numerical Analysis,
!    Volume 30, pages 377-397, 2008.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the dimension of the polynomial space.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ), parameter :: n = 5001

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) nf
  real ( kind = 8 ) wf(m)
  real ( kind = 8 ) wf_sum
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xf(m)

  a = -1.0D+00
  b = +1.0D+00
  call r8vec_linspace ( n, a, b, x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  Seek Fekete points in [', a, ',', b, ']'
  write ( *, '(a,i6,a)' ) '  using ', n, ' equally spaced sample points'
  write ( *, '(a,i6)' ) '  for polynomials of degree M = ', m
  write ( *, '(a)' ) '  with the Chebyshev basis.'

  call line_fekete_chebyshev ( m, a, b, n, x, nf, xf, wf )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  NF = ', nf
  call r8vec_print ( nf, xf, '  Estimated Fekete points XF:' )
  wf_sum = sum ( wf(1:nf) )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Sum(WF) = ', wf_sum

  return
end
subroutine test03 ( m )

!*****************************************************************************80
!
!! TEST03 seeks Fekete points in [-1,+1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the dimension of the polynomial space.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ), parameter :: n = 5001

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) nf
  real ( kind = 8 ) wf(m)
  real ( kind = 8 ) wf_sum
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xf(m)

  a = -1.0D+00
  b = +1.0D+00
  call r8vec_linspace ( n, a, b, x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03:'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  Seek Fekete points in [', a, ',', b, ']'
  write ( *, '(a,i6,a)' ) '  using ', n, ' equally spaced sample points'
  write ( *, '(a,i6)' ) '  for polynomials of degree M = ', m
  write ( *, '(a)' ) '  with the Legendre basis and uniform weight.'

  call line_fekete_legendre ( m, a, b, n, x, nf, xf, wf )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  NF = ', nf
  call r8vec_print ( nf, xf, '  Estimated Fekete points XF:' )
  wf_sum = sum ( wf(1:nf) )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Sum(WF) = ', wf_sum

  return
end

