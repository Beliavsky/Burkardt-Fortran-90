program main

!*****************************************************************************80
!
!! MAIN is the main program for BLAS3_C_TEST.
!
!  Discussion:
!
!    BLAS3_C_TEST tests the BLAS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS3_C_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BLAS library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS3_C_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 demonstrates the use of CGEMM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3
  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), parameter :: lda = m
  integer ( kind = 4 ), parameter :: ldb = k
  integer ( kind = 4 ), parameter :: ldc = m

  complex ( kind = 4 ) a(lda,k)
  complex ( kind = 4 ) alpha
  complex ( kind = 4 ) b(ldb,n)
  complex ( kind = 4 ) beta
  complex ( kind = 4 ) c(ldc,n)
  character :: transa = 'N'
  character :: transb = 'N'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  CGEMM can combine scale, multiply and add matrices'
  write ( *, '(a)' ) '  using single precision complex arithmetic.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Here, we simply compute C = A * B.'
  write ( *, '(a)' ) '  Because B is inverse ( A ), C should be the identity.'
 
  call c4mat_test ( m, a )
  call c4mat_print ( m, m, a, '  Matrix A:' )

  call c4mat_test_inverse ( m, b )
  call c4mat_print ( m, m, b, '  Matrix B:' )

  alpha = ( 1.0E+00, 0.0E+00 )
  beta = ( 0.0E+00, 0.0E+00 )

  call cgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call c4mat_print ( m, n, c, '  Product C = A * B:' )

  return
end
