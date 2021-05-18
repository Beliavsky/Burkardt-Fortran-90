program main

!*****************************************************************************80
!
!! MAIN is the main program for BLAS3_S_TEST.
!
!  Discussion:
!
!    BLAS3_S_TEST tests the BLAS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS3_S_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BLAS library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS3_S_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests SGEMM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 4 ), allocatable :: a(:,:)
  real ( kind = 4 ) alpha
  real ( kind = 4 ), allocatable :: b(:,:)
  real ( kind = 4 ) beta
  real ( kind = 4 ), allocatable :: c(:,:)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) ldc
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character * ( 1 ) transa
  character * ( 1 ) transb
  character * ( 1 ) transc

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  SGEMM carries out matrix multiplications'
  write ( *, '(a)' ) '  for double precision real matrices.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  1: C = alpha * A  * B  + beta * C;'
  write ( *, '(a)' ) '  2: C = alpha * A'' * B  + beta * C;'
  write ( *, '(a)' ) '  3: C = alpha * A  * B'' + beta * C;'
  write ( *, '(a)' ) '  4: C = alpha * A'' * B'' + beta * C;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  We carry out all four calculations, but in each case,'
  write ( *, '(a)' ) '  we choose our input matrices so that we get the same result.'
!
!  C = alpha * A * B + beta * C.
!
  transa = 'N'
  transb = 'N'
  transc = 'N'
  m = 4
  n = 5
  k = 3
  alpha = 2.0E+00
  lda = m
  allocate ( a(1:lda,1:k) )
  call r4mat_test ( transa, lda, m, k, a )
  ldb = k
  allocate ( b(1:ldb,1:n) )
  call r4mat_test ( transb, ldb, k, n, b )
  beta = 3.0E+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r4mat_test ( transc, ldc, m, n, c )

  call sgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r4mat_print ( m, n, c, '  C = alpha * A * B + beta * C:' );

  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  C = alpha * A' * B + beta * C.
!
  transa = 'T'
  transb = 'N'
  transc = 'N'
  m = 4
  n = 5
  k = 3
  alpha = 2.0E+00
  lda = k
  allocate ( a(1:lda,1:m) )
  call r4mat_test ( transa, lda, m, k, a )
  ldb = k
  allocate ( b(1:ldb,1:n) )
  call r4mat_test ( transb, ldb, k, n, b )
  beta = 3.0E+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r4mat_test ( transc, ldc, m, n, c )

  call sgemm ( transa, transb,  m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r4mat_print ( m, n, c, '  C = alpha * A'' * B + beta * C:' )

  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  C = alpha * A * B' + beta * C.
!
  transa = 'N'
  transb = 'T'
  transc = 'N'
  m = 4
  n = 5
  k = 3
  alpha = 2.0E+00
  lda = m
  allocate ( a(1:lda,1:k) )
  call r4mat_test ( transa, lda, m, k, a )
  ldb = n
  allocate ( b(1:ldb,1:k) )
  call r4mat_test ( transb, ldb, k, n, b )
  beta = 3.0E+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r4mat_test ( transc, ldc, m, n, c )

  call sgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r4mat_print ( m, n, c, '  C = alpha * A * B'' + beta * C:' )

  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  C = alpha * A' * B' + beta * C.
!
  transa = 'T'
  transb = 'T'
  transc = 'N'
  m = 4
  n = 5
  k = 3
  alpha = 2.0E+00
  lda = k
  allocate ( a(1:lda,1:m) )
  call r4mat_test ( transa, lda, m, k, a )
  ldb = n
  allocate ( b(1:ldb,1:k) )
  call r4mat_test ( transb, ldb, k, n, b )
  beta = 3.0E+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r4mat_test ( transc, ldc, m, n, c )

  call sgemm ( transa, transb,  m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r4mat_print ( m, n, c, '  C = alpha * A'' * B'' + beta * C:' )

  deallocate ( a )
  deallocate ( b )
  deallocate ( c )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests DTRMM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 4 ), allocatable :: a(:,:)
  real ( kind = 4 ) alpha
  real ( kind = 4 ), allocatable :: b(:,:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character side
  character transa
  character transb
  character uplo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  STRMM multiplies a triangular matrix A and a'
  write ( *, '(a)' ) '  rectangular matrix B'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  1: B = alpha * A  * B;'
  write ( *, '(a)' ) '  2: B = alpha * A'' * B;'
!
!  B = alpha * A * B.
!
  side = 'L'
  uplo = 'U'
  transa = 'N'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0E+00
  lda = m
  ldb = m

  allocate ( a(1:m,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 4 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0E+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r4mat_test ( transb, ldb, m, n, b )

  call strmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r4mat_print ( m, n, b, '  B = alpha * A * B:' );

  deallocate ( a )
  deallocate ( b )
!
!  B = alpha * A' * B.
!
  side = 'L'
  uplo = 'U'
  transa = 'T'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0E+00
  lda = m
  ldb = m

  allocate ( a(1:m,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 4 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0E+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r4mat_test ( transb, ldb, m, n, b )

  call strmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r4mat_print ( m, n, b, '  B = alpha * A * B:' );

  deallocate ( a )
  deallocate ( b )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests STRSM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 4 ), allocatable :: a(:,:)
  real ( kind = 4 ) alpha
  real ( kind = 4 ), allocatable :: b(:,:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character side
  character transa
  character transb
  character uplo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  STRSM solves a linear system involving a triangular'
  write ( *, '(a)' ) '  matrix A and a rectangular matrix B.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  1: Solve A  * X  = alpha * B;'
  write ( *, '(a)' ) '  2: Solve A'' * X  = alpha * B;'
  write ( *, '(a)' ) '  3: Solve X  * A  = alpha * B;'
  write ( *, '(a)' ) '  4: Solve X  * A'' = alpha * B;'
!
!  Solve A * X = alpha * B.
!
  side = 'L'
  uplo = 'U'
  transa = 'N'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0E+00
  lda = m
  ldb = m

  allocate ( a(1:lda,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 4 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0E+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r4mat_test ( transb, ldb, m, n, b )

  call strsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r4mat_print ( m, n, b, '  X = inv ( A ) * alpha * B:' );

  deallocate ( a )
  deallocate ( b )
!
!  Solve A' * X = alpha * B.
!
  side = 'L'
  uplo = 'U'
  transa = 'T'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0E+00
  lda = m
  ldb = m

  allocate ( a(1:lda,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 4 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0E+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r4mat_test ( transb, ldb, m, n, b )

  call strsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r4mat_print ( m, n, b, '  X = inv ( A'' ) * alpha * B:' );

  deallocate ( a )
  deallocate ( b )
!
!  Solve X * A = alpha * B.
!
  side = 'R'
  uplo = 'U'
  transa = 'N'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0E+00
  lda = n
  ldb = m

  allocate ( a(1:lda,1:n) )
  do j = 1, n
    do i = 1, j
      a(i,j) = real ( i + j, kind = 4 )
    end do
    do i = j + 1, n
      a(i,j) = 0.0E+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r4mat_test ( transb, ldb, m, n, b )

  call strsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r4mat_print ( m, n, b, '  X = alpha * B * inv ( A ):' );

  deallocate ( a )
  deallocate ( b )
!
!  Solve X * A'' = alpha * B.
!
  side = 'R'
  uplo = 'U'
  transa = 'T'
  diag = 'N'
  m = 4
  n = 5
  alpha = 2.0E+00
  lda = n
  ldb = m

  allocate ( a(1:lda,1:n) )
  do j = 1, n
    do i = 1, j
      a(i,j) = real ( i + j, kind = 4 )
    end do
    do i = j + 1, n
      a(i,j) = 0.0E+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r4mat_test ( transb, ldb, m, n, b )

  call strsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r4mat_print ( m, n, b, '  X = alpha * B * inv ( A'' ):' );

  deallocate ( a )
  deallocate ( b )

  return
end
