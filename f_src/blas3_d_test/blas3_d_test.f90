program main

!*****************************************************************************80
!
!! MAIN is the main program for BLAS3_D_TEST.
!
!  Discussion:
!
!    BLAS3_D_TEST tests the BLAS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 February 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS3_D_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BLAS library.'

  call dgemm_test ( )
  call dtrmm_test ( )
  call dtrsm_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS3_D_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine dgemm_test ( )

!*****************************************************************************80
!
!! DGEMM_TEST tests DGEMM.
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

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ), allocatable :: b(:,:)
  real ( kind = 8 ) beta
  real ( kind = 8 ), allocatable :: c(:,:)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) ldc
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character transa
  character transb
  character transc

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DGEMM_TEST'
  write ( *, '(a)' ) '  DGEMM multiplies two matrices A and B.'
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
  alpha = 2.0D+00
  lda = m
  allocate ( a(1:lda,1:k) )
  call r8mat_test ( transa, lda, m, k, a )
  ldb = k
  allocate ( b(1:ldb,1:n) )
  call r8mat_test ( transb, ldb, k, n, b )
  beta = 3.0D+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r8mat_test ( transc, ldc, m, n, c )

  call dgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r8mat_print ( m, n, c, '  C = alpha * A * B + beta * C:' );

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
  alpha = 2.0D+00
  lda = k
  allocate ( a(1:lda,1:m) )
  call r8mat_test ( transa, lda, m, k, a )
  ldb = k
  allocate ( b(1:ldb,1:n) )
  call r8mat_test ( transb, ldb, k, n, b )
  beta = 3.0D+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r8mat_test ( transc, ldc, m, n, c )

  call dgemm ( transa, transb,  m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r8mat_print ( m, n, c, '  C = alpha * A'' * B + beta * C:' )

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
  alpha = 2.0D+00
  lda = m
  allocate ( a(1:lda,1:k) )
  call r8mat_test ( transa, lda, m, k, a )
  ldb = n
  allocate ( b(1:ldb,1:k) )
  call r8mat_test ( transb, ldb, k, n, b )
  beta = 3.0D+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r8mat_test ( transc, ldc, m, n, c )

  call dgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r8mat_print ( m, n, c, '  C = alpha * A * B'' + beta * C:' )

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
  alpha = 2.0D+00
  lda = k
  allocate ( a(1:lda,1:m) )
  call r8mat_test ( transa, lda, m, k, a )
  ldb = n
  allocate ( b(1:ldb,1:k) )
  call r8mat_test ( transb, ldb, k, n, b )
  beta = 3.0D+00
  ldc = m
  allocate ( c(1:ldc,1:n) )
  call r8mat_test ( transc, ldc, m, n, c )

  call dgemm ( transa, transb,  m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

  call r8mat_print ( m, n, c, '  C = alpha * A'' * B'' + beta * C:' )

  deallocate ( a )
  deallocate ( b )
  deallocate ( c )

  return
end
subroutine dtrmm_test ( )

!*****************************************************************************80
!
!! DTRMM_TEST tests DTRMM.
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

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ), allocatable :: b(:,:)
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
  write ( *, '(a)' ) 'DTRMM_TEST'
  write ( *, '(a)' ) '  DTRMM multiplies a triangular matrix A and a'
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
  alpha = 2.0D+00
  lda = m
  ldb = m

  allocate ( a(1:lda,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  B = alpha * A * B:' );

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
  alpha = 2.0D+00
  lda = m
  ldb = m

  allocate ( a(1:lda,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  B = alpha * A * B:' );

  deallocate ( a )
  deallocate ( b )

  return
end
subroutine dtrsm_test ( )

!*****************************************************************************80
!
!! DTRSM_TEST tests DTRSM.
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

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ), allocatable :: b(:,:)
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
  write ( *, '(a)' ) 'DTRSM_TEST'
  write ( *, '(a)' ) '  DTRSM solves a linear system involving a triangular'
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
  alpha = 2.0D+00
  lda = m
  ldb = m

  allocate ( a(1:lda,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  X = inv ( A ) * alpha * B:' );

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
  alpha = 2.0D+00
  lda = m
  ldb = m

  allocate ( a(1:lda,1:m) )
  do j = 1, m
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, m
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  X = inv ( A'' ) * alpha * B:' );

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
  alpha = 2.0D+00
  lda = n
  ldb = m

  allocate ( a(1:lda,1:n) )
  do j = 1, n
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, n
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  X = alpha * B * inv ( A ):' );

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
  alpha = 2.0D+00
  lda = n
  ldb = m

  allocate ( a(1:lda,1:n) )
  do j = 1, n
    do i = 1, j
      a(i,j) = real ( i + j, kind = 8 )
    end do
    do i = j + 1, n
      a(i,j) = 0.0D+00
    end do
  end do

  allocate ( b(1:ldb,1:n) )
  transb = 'N'
  call r8mat_test ( transb, ldb, m, n, b )

  call dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

  call r8mat_print ( m, n, b, '  X = alpha * B * inv ( A'' ):' );

  deallocate ( a )
  deallocate ( b )

  return
end

