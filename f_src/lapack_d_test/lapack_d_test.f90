program main

!*****************************************************************************80
!
!! MAIN is the main program for LAPACK_D_TEST.
!
!  Discussion:
!
!    LAPACK_D_TEST tests the LAPACK_D library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 January 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAPACK_D_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LAPACK_D library.'

  call dgbtrf_test ( )
  call dgecon_test ( )
  call dgeqrf_test ( )
  call dgesvd_test ( )
  call dgetrf_test ( )
  call dgetri_test ( )
  call dgtsv_test ( )
  call dormgqr_test ( )
  call dpbtrf_test ( )
  call dpbtrs_test ( )
  call dpotrf_test ( )
  call dpotri_test ( )
  call dsbgvx_test ( )
  call dsyev_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LAPACK_D_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'

  stop 0
end
subroutine dgbtrf_test ( )

!*****************************************************************************80
!
!! DGBTRF_TEST tests DGBTRF.
!
!  Discussion:
!
!    The problem is just an enlarged version of the
!    problem for n = 5, which is:
!
!    Matrix A is ( 2 -1  0  0  0)    right hand side b is  (1)
!                (-1  2 -1  0  0)                          (0)
!                ( 0 -1  2 -1  0)                          (0)
!                ( 0  0 -1  2 -1)                          (0)
!                ( 0  0  0 -1  2)                          (1)
!
!
!    Solution is   (1)
!                  (1)
!                  (1)
!                  (1)
!                  (1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 25
  integer ( kind = 4 ), parameter :: ml = 1
  integer ( kind = 4 ), parameter :: mu = 1

  integer ( kind = 4 ), parameter :: lda = 2 * ml + mu + 1

  real ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipiv(n)
  integer ( kind = 4 ) m

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DGBTRF_TEST'
  write ( *, '(a)' ) '  DGBTRF factors a general band matrix.'
  write ( *, '(a)' ) '  DGBTRS solves a factored system.'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general band storage mode (GB):'
!
!  Assign values to matrix A and right hand side b.
!
  b(1) = 1.0D+00
  b(2:n-1) = 0.0D+00
  b(n) = 1.0D+00
!
!  Zero out the matrix.
!
  a(1:lda,1:n) = 0.0D+00

  m = ml + mu + 1
!
!  Superdiagonal,
!  Diagonal,
!  Subdiagonal.
!
  a(m-1,2:n) = -1.0D+00
  a(m,1:n) = 2.0D+00
  a(m+1,1:n-1) = -1.0D+00
!
!  Factor the matrix.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Bandwidth is ', m
  write ( *, '(a)' ) ' '

  call dgbtrf ( n, n, ml, mu, a, lda, ipiv, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST01'
    write ( *, '(a,i8)' ) '  Factorization failed, INFO = ', info
    return
  end if
!
!  Solve the linear system.
!
  call dgbtrs ( 'n', n, ml, mu, 1, a, lda, ipiv, b, n, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST01'
    write ( *, '(a,i8)' ) '  Solution failed, INFO = ', info
    return
  end if

  call r8vec_print_some ( n, b, 1, 5, '  Partial solution (all should be 1)' )

  return
end
subroutine dgecon_test ( )

!*****************************************************************************80
!
!! DGECON_TEST tests DGECON and DGETRF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), parameter :: lda = n
  integer ( kind = 4 ), parameter :: lwork = 4 * n

  real ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) anorm
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipiv(n)
  integer ( kind = 4 ) iwork(n)
  real ( kind = 8 ) rcond
  real ( kind = 8 ) r8mat_norm_li
  real ( kind = 8 ) work(lwork)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DGECON_TEST'
  write ( *, '(a)' ) '  DGECON computes the condition number '
  write ( *, '(a)' ) '  of a factored matrix'
  write ( *, '(a)' ) '  DGETRF computes the LU factorization;'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE):'
!
!  Set the matrix.
!
  a(1,1) = 1.0D+00
  a(1,2) = 2.0D+00
  a(1,3) = 3.0D+00

  a(2,1) = 4.0D+00
  a(2,2) = 5.0D+00
  a(2,3) = 6.0D+00

  a(3,1) = 7.0D+00
  a(3,2) = 8.0D+00
  a(3,3) = 0.0D+00

  call r8mat_print ( n, n, a, '  The matrix A:' )
!
!  Get the infinity norm of the matrix.
!
  anorm = r8mat_norm_li ( n, n, a )
!
!  Factor the matrix.
!
  call dgetrf ( n, n, a, lda, ipiv, info )
!
!  Get the condition number.
!
  call dgecon ( 'I', n, a, lda, anorm, rcond, work, iwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Condition number calculation failed!'
    write ( *, '(a,i8)' ) '  INFO = ', info
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Matrix reciprocal condition number = ', rcond

  return
end
subroutine dgeqrf_test ( )

!*****************************************************************************80
!
!! DGEQRF_TEST tests DGEQRF.
!
!  Discussion:
!
!    DGEQRF computes the QR factorization of an M by N matrix A:
!
!      A(MxN) = Q(MxK) * R(KxN)
!
!    where K = min ( M, N ).
!
!    DORGQR computes the explicit form of the Q factor.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 8
  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ), parameter :: k = min ( m, n )
  integer ( kind = 4 ), parameter :: lwork = n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) lda
  real ( kind = 8 ) q(m,k)
  real ( kind = 8 ) r(k,n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) tau(k)
  real ( kind = 8 ) work(lwork)

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DGEQRF_TEST'
  write ( *, '(a)' ) '  DGEQRF computes the QR factorization:'
  write ( *, '(a)' ) '    A = Q * R'
  write ( *, '(a)' ) '  DORGQR computes the explicit form of the Q factor.'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE):'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In this case, our M x N matrix A has more rows'
  write ( *, '(a)' ) '  than columns:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  M = ', m
  write ( *, '(a,i8)' ) '  N = ', n
!
!  Set A.
!
  call r8mat_uniform_01 ( m, n, seed, a )

  call r8mat_print ( m, n, a, '  The matrix A:' )
!
!  Compute the QR factorization.
!
  lda = m

  call dgeqrf ( m, n, a, lda, tau, work, lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  DGEQRF returned nonzero INFO = ', info
    return
  end if

  r(1:k,1:n) = 0.0D+00
  do i = 1, k
    r(i,i:n) = a(i,i:n)
  end do
!
!  Construct Q explicitly.
!
  q(1:m,1:k) = a(1:m,1:k)

  call dorgqr ( m, k, k, q, lda, tau, work, lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  DORGQR returned nonzero INFO = ', info
    return
  end if

  call r8mat_print ( m, k, q, '  The Q factor:' )

  call r8mat_print ( k, n, r, '  The R factor:' )

  a(1:m,1:n) = matmul ( q(1:m,1:k), r(1:k,1:n) )

  call r8mat_print ( m, n, a, '  The product Q * R:' )

  return
end
subroutine dormgqr_test ( )

!*****************************************************************************80
!
!! DORMGQR_TEST tests DORMGQR.
!
!  Discussion:
!
!    We want to solve the MxN linear system A*x=b using the QR approach:
!
!    Factor A:
!
!      A = Q * R                        (step 1)
!
!    Transform:
!
!               A * x =               b
!      ==>  Q * R * x =               b
!      ==>      R * x =          Q' * b  (step 2)
!      ==>          x = inv(R) * Q' * b. (step 3)
!
!    Step 1) DGEQRF computes the QR factorization of an M by N matrix A:
!    A(MxN) = Q(MxK) * R(KxN) where K = min ( M, N ).
!
!    Step 2) DORMQR can multiply Q' * b, putting the result back into b.
!
!    Step 3) We could call a LAPACK routine to solve the upper triangular
!    system R * x = Q' * b.  Instead, we will try this part ourselves.
!
!
!    LAPACK makes this process tricky because of two things it does
!    for efficiency:
!
!    *) LAPACK computes the Q and R factors in a
!       compressed and encoded form, overwriting the matrix A and
!       storing some extra information in a vector called TAU.
!
!    *) LAPACK defines K = min ( M, N ), and
!       does NOT compute the QR factorization as an MxM Q
!       times an MxN R.  Instead, it computes an MxK Q times
!       a KxN R.  This saves it worrying about zeroes, but it
!       means the programmer has to worry about proper storage
!       and correct dimensioning.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 January 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 8
  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ), parameter :: k = min ( m, n )
  integer ( kind = 4 ), parameter :: lwork = n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lda
  real ( kind = 8 ) r(k,n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) tau(k)
  real ( kind = 8 ) work(lwork)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DORMGQR_TEST'
  write ( *, '(a)' ) '  DORMQR can compute Q'' * b.'
  write ( *, '(a)' ) '  after DGEQRF computes the QR factorization:'
  write ( *, '(a)' ) '    A = Q * R'
  write ( *, '(a)' ) '  storing a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  We use these routines to carry out a QR'
  write ( *, '(a)' ) '  solve of an M by N linear system A * x = b.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In this case, our M x N matrix A has more rows'
  write ( *, '(a)' ) '  than columns:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  M = ', m
  write ( *, '(a,i8)' ) '  N = ', n
!
!  STEP 0: Set random A, simple x, and compute b.
!
  seed = 123456789

  call r8mat_uniform_01 ( m, n, seed, a )

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  b(1:m) = matmul ( a(1:m,1:n), x(1:n) )
!
!  Wipe out X so we believe it when we get it back...
!
  x(1:n) =  0.0D+00

  call r8mat_print ( m, n, a, '  The matrix A:' )
!
!  STEP 1: Compute the QR factorization.
!
  lda = m

  call dgeqrf ( m, n, a, lda, tau, work, lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  DGEQRF returned nonzero INFO = ', info
    return
  end if
!
!  Extract and save the K by N matrix R.
!
  r(1:k,1:n) = 0.0D+00
  do i = 1, k
    r(i,i:n) = a(i,i:n)
  end do
!
!  STEP 2: Multiply Q' * b to get the N by 1 result in b.
!
  call dormqr ( 'L', 'T', m, 1, k, a, m, tau, b, m, work, lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  DORMQR returned nonzero INFO = ', info
    return
  end if
!
!  STEP 3: Compute inv(R) * Q' * b, or, equivalently,
!  solve R * x = Q' * b.
!
  do j = n, 1, -1
    x(j) = b(j) / r(j,j)
    do i = 1, n - 1
      b(i) = b(i) - r(i,j) * x(j)
    end do
  end do

  call r8vec_print ( n, x, '  The solution X:' )

  return
end
subroutine dgesvd_test ( )

!*****************************************************************************80
!
!! DGESVD_TEST tests DGESVD.
!
!  Discussion:
!
!    DGESVD computes the singular value decomposition:
!
!      A = U * S * V'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), parameter :: lwork = 3*min(m,n) + max ( max(m,n), 2*min(m,n) )

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldu
  integer ( kind = 4 ) ldvt
  character jobu
  character jobvt
  real ( kind = 8 ) s(min(m,n))
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma(m,n)
  real ( kind = 8 ) u(m,m)
  real ( kind = 8 ) vt(n,n)
  real ( kind = 8 ) work(lwork)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DGESVD_TEST'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE):'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  DGESVD computes the singular value decomposition:'
  write ( *, '(a)' ) '    A = U * S * V'''
!
!  Set A.
!
  seed = 123456789

  call r8mat_uniform_01 ( m, n, seed, a )

  call r8mat_print ( m, n, a, '  The matrix A:' )
!
!  Compute the singular values and singular vectors.
!
  jobu = 'A'
  jobvt = 'A'
  lda = m
  ldu = m
  ldvt = n

  call dgesvd ( jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, &
    lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  DGESVD returned nonzero INFO = ', info
    return
  end if

  call r8vec_print ( min ( m, n ), s, '  Singular values' )

  call r8mat_print ( m, m, u, '  Left singular vectors U:' )
  call r8mat_print ( n, n, vt, '  Right singular vectors V'':' )

  sigma(1:m,1:n) = 0.0D+00
  do i = 1, min ( m, n )
    sigma(i,i) = s(i)
  end do

  a(1:m,1:n) = matmul ( u(1:m,1:m), matmul ( sigma(1:m,1:n), vt(1:n,1:n) ) )

  call r8mat_print ( m, n, a, '  The product U * S * V'':' )

  return
end
subroutine dgetri_test ( )

!*****************************************************************************80
!
!! DGETRI_TEST tests DGETRI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), parameter :: lda = n
  integer ( kind = 4 ), parameter :: lwork = n

  real ( kind = 8 ) a(lda,n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipiv(n)
  real ( kind = 8 ) work(lwork)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DGETRI_TEST'
  write ( *, '(a)' ) '  DGETRI computes the inverse'
  write ( *, '(a)' ) '  of a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE):'
!
!  Set the matrix.
!
  a(1,1) = 1.0D+00
  a(1,2) = 2.0D+00
  a(1,3) = 3.0D+00

  a(2,1) = 4.0D+00
  a(2,2) = 5.0D+00
  a(2,3) = 6.0D+00

  a(3,1) = 7.0D+00
  a(3,2) = 8.0D+00
  a(3,3) = 0.0D+00

  call r8mat_print ( n, n, a, '  The matrix A:' )
!
!  Factor the matrix.
!
  call dgetrf ( n, n, a, lda, ipiv, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  DGETRF returned INFO = ', info
    write ( *, '(a)' ) '  The matrix is numerically singular.'
    return
  end if
!
!  Compute the inverse matrix.
!
  call dgetri ( n, a, lda, ipiv, work, lwork, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The inversion procedure failed!'
    write ( *, '(a,i8)' ) '  INFO = ', info
    return
  end if

  call r8mat_print ( n, n, a, '  The inverse matrix:' )

  return
end
subroutine dgetrf_test ( )

!*****************************************************************************80
!
!! DGETRF_TEST tests DGETRF.
!
!  Discussion:
!
!    The problem is just an enlarged version of the
!    problem for n = 5, which is:
!
!    Matrix A is ( N -1 -1 -1 -1)    right hand side b is  (1)
!                (-1  N -1 -1 -1)                          (1)
!                (-1 -1  N -1 -1)                          (1)
!                (-1 -1 -1  N -1)                          (1)
!                (-1 -1 -1 -1  N)                          (1)
!
!    Solution is   (1)
!                  (1)
!                  (1)
!                  (1)
!                  (1)
!
!    For this problem, no pivoting is required.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 25
  integer ( kind = 4 ), parameter :: lda = n

  real ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipiv(n)
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DGETRF_TEST'
  write ( *, '(a)' ) '  DGETRF factors a general matrix;'
  write ( *, '(a)' ) '  DGETRS solves a linear system;'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general storage mode (GE):'
!
!  Assign values to matrix A and right hand side b.
!
  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = dble ( n )
      else
        a(i,j) = -1.0D+00
      end if
    end do
  end do

  b(1:n) = 1.0D+00
!
!  Factor the matrix.
!
  call dgetrf ( n, n, a, lda, ipiv, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Matrix is singular, INFO = ', info
    return
  end if
!
!  Solve the linear system.
!
  call dgetrs ( 'n', n, 1, a, lda, ipiv, b, n, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Solution procedure failed, INFO = ', info
    return
  end if

  call r8vec_print_some ( n, b, 1, 5, '  Partial solution (all should be 1)' )

  return
end
subroutine dgtsv_test ( )

!*****************************************************************************80
!
!! DGTSV_TEST tests DGTSV.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 100

  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c(n-1)
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) e(n-1)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) nrhs

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DGTSV_TEST'
  write ( *, '(a)' ) '  DGTSV factors and solves a linear system'
  write ( *, '(a)' ) '  with a general tridiagonal matrix'
  write ( *, '(a)' ) '  for a double precision real matrix (D)'
  write ( *, '(a)' ) '  in general tridiagonal storage mode (GT).'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The system is of order N = ', n
  write ( *, '(a)' ) ' '
!
!  Right hand side.
!
  b(1:n-1) = 0.0D+00
  b(n) = n + 1
!
!  Subdiagonal.
!  Diagonal.
!  Superdiagonal.
!
  c(1:n-1) = -1.0D+00
  d(1:n) = 2.0D+00
  e(1:n-1) = -1.0D+00

  nrhs = 1
  ldb = n
!
!  Factor and solve the linear system.
!
  call dgtsv ( n, nrhs, c, d, e, b, ldb, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Solution procedure failed.'
    write ( *, '(a,i8)' ) '  INFO = ', info
    return
  end if

  call r8vec_print_some ( n, b, 1, 5, '  Partial solution (Should be 1,2,3...)' )

  return
end
subroutine dpbtrf_test ( )

!*****************************************************************************80
!
!! DPBTRF_TEST tests DPBTRF.
!
!  Discussion:
!
!    We want to compute the lower triangular Cholesky factor L
!    of a positive definite symmetric band matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: nband = 1

  real ( kind = 8 ) a(nband+1,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  real ( kind = 8 ) l(nband+1,n)
  real ( kind = 8 ) l_row(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DPBTRF_TEST'
  write ( *, '(a)' ) '  DPBTRF computes'
  write ( *, '(a)' ) '    the lower Cholesky factor A = L*L'' or'
  write ( *, '(a)' ) '    the upper Cholesky factor A = U''*U;'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in positive definite band storage mode (PB):'
!
!  Zero out the matrix.
!
  a(1:nband+1,1:n) = 0.0D+00
!
!  Store the diagonal of a symmetric band matrix.
!
  a(1,1:n) = 2.0D+00
!
!  Store the subdiagonal of a symmetric band matrix.
!
  a(2,1:n-1) = -1.0D+00
!
!  Get the lower triangular Cholesky factor L:
!
  l(1:nband+1,1:n) = a(1:nband+1,1:n)

  call dpbtrf ( 'L', n, nband, l, nband+1, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Factorization failed, INFO = ', info
    return
  end if
!
!  Print the relevant entries of L:
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The lower Cholesky factor L:'
  write ( *, '(a)' ) ' '

  do i = 1, n
    do j = 1, n

      if ( 0 <= i - j .and. i-j <= nband ) then
        l_row(j) = l(i-j+1,j)
      else
        l_row(j) = 0.0D+00
      end if

    end do

    write ( *, '(5f10.6)' ) l_row(1:n)

  end do

  return
end
subroutine dpbtrs_test ( )

!*****************************************************************************80
!
!! DPBTRS_TEST tests DPBTRS.
!
!  Discussion:
!
!    The problem is just an enlarged version of the
!    problem for n = 5, which is:
!
!    Matrix A is ( 2 -1  0  0  0)    right hand side b is  (1)
!                (-1  2 -1  0  0)                          (0)
!                ( 0 -1  2 -1  0)                          (0)
!                ( 0  0 -1  2 -1)                          (0)
!                ( 0  0  0 -1  2)                          (1)
!
!
!    Solution is   (1)
!                  (1)
!                  (1)
!                  (1)
!                  (1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 25
  integer ( kind = 4 ), parameter :: nband = 1

  integer ( kind = 4 ), parameter :: lda = nband + 1

  real ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) nrhs

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DPBTRS_TEST'
  write ( *, '(a)' ) '  DPBTRS solves linear systems'
  write ( *, '(a)' ) '  for a positive definite symmetric band matrix,'
  write ( *, '(a)' ) '  stored as a double precision real matrix (D)'
  write ( *, '(a)' ) '  in positive definite band storage mode (PB):'
!
!  Zero out the matrix.
!
  a(1:lda,1:n) = 0.0D+00
!
!  Super (and sub) diagonal.
!
  a(1,2:n) = -1.0D+00
!
!  Diagonal.
!
  a(2,1:n) = 2.0D+00
!
!  Set the right hand side.
!
  b(1) = 1.0D+00
  b(2:n-1) = 0.0D+00
  b(n) = 1.0D+00
!
!  Factor the matrix.
!
  call dpbtrf ( 'u', n, nband, a, lda, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Factorization failed, INFO = ', info
    return
  end if
!
!  Solve the linear system.
!
  nrhs = 1
  call dpbtrs ( 'u', n, nband, nrhs, a, lda, b, n, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Solution failed, INFO = ', info
  end if

  call r8vec_print_some ( n, b, 1, 5, '  Partial solution (all should be 1)' )

  return
end
subroutine dpotrf_test ( )

!*****************************************************************************80
!
!! DPOTRF_TEST tests DPOTRF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ), parameter :: lda = n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  real ( kind = 8 ) r(n,n)
  real ( kind = 8 ) temp(n,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DPOTRF_TEST'
  write ( *, '(a)' ) '  DPOTRF computes the Cholesky factorization R''*R'
  write ( *, '(a)' ) '  for a double precision real matrix (D)'
  write ( *, '(a)' ) '  in positive definite storage mode (PO).'
  write ( *, '(a)' ) ' '
!
!  Zero out the matrix.
!
  a(1:n,1:n) = 0.0D+00
!
!  Subdiagonal.
!
  do i = 2, n
    a(i,i-1) = -1.0D+00
  end do
!
!  Diagonal.
!
  do i = 1, n
    a(i,i) = 2.0D+00
  end do
!
!  Superdiagonal.
!
  do i = 1, n - 1
    a(i,i+1) = -1.0D+00
  end do

  call r8mat_print ( n, n, a, '  The matrix A:' )
!
!  Factor the matrix.
!
  r(1:n,1:n) = a(1:n,1:n)

  call dpotrf ( 'u', n, r, lda, info )

  if ( info /= 0 ) then
    write ( *, '(a,i8)' ) '  DPOTRF returns INFO = ', info
    return
  end if

  do i = 1, n
    r(i,1:i-1) = 0.0D+00
  end do

  call r8mat_print ( n, n, r, '  The Cholesky factor R:' )

  temp(1:n,1:n) = matmul ( transpose ( r(1:n,1:n) ) , r(1:n,1:n) )

  call r8mat_print ( n, n, temp, '  The product R'' * R' )

  return
end
subroutine dpotri_test ( )

!*****************************************************************************80
!
!! DPOTRI_TEST tests DPOTRI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ), parameter :: lda = n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a_inv(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  real ( kind = 8 ) r(n,n)
  real ( kind = 8 ) temp(n,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DPOTRI_TEST'
  write ( *, '(a)' ) '  DPOTRI computes the inverse'
  write ( *, '(a)' ) '  for a double precision real matrix (D)'
  write ( *, '(a)' ) '  in positive definite storage mode (PO).'
  write ( *, '(a)' ) ' '
!
!  Zero out the matrix.
!
  a(1:n,1:n) = 0.0D+00
!
!  Subdiagonal.
!
  do i = 2, n
    a(i,i-1) = -1.0D+00
  end do
!
!  Diagonal.
!
  do i = 1, n
    a(i,i) = 2.0D+00
  end do
!
!  Superdiagonal.
!
  do i = 1, n - 1
    a(i,i+1) = -1.0D+00
  end do

  call r8mat_print ( n, n, a, '  The matrix A:' )
!
!  Factor the matrix.
!
  r(1:n,1:n) = a(1:n,1:n)

  call dpotrf ( 'u', n, r, lda, info )

  if ( info /= 0 ) then
    write ( *, '(a,i8)' ) '  DPOTRF returns INFO = ', info
    return
  end if

  do i = 1, n
    r(i,1:i-1) = 0.0D+00
  end do

  call r8mat_print ( n, n, r, '  The Cholesky factor R:' )

  temp(1:n,1:n) = matmul ( transpose ( r(1:n,1:n) ) , r(1:n,1:n) )

  call r8mat_print ( n, n, temp, '  The product R'' * R' )
!
!  Compute the inverse matrix.
!
  a_inv(1:n,1:n) = r(1:n,1:n)

  call dpotri ( 'u', n, a_inv, lda, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  The inversion procedure failed, INFO = ', info
    return
  end if

  do i = 1, n
    a_inv(i,1:i-1) = a_inv(1:i-1,i)
  end do

  call r8mat_print ( n, n, a_inv, '  The inverse matrix B:' )

  temp(1:n,1:n) = matmul ( transpose ( a_inv(1:n,1:n) ) , a(1:n,1:n) )

  call r8mat_print ( n, n, temp, '  The product B * A' )

  return
end
subroutine dsbgvx_test ( )

!*****************************************************************************80
!
!! DSBGVX_TEST tests DSBGVX.
!
!  Discussion:
!
!    DSBGVX deals with the generalized eigenvalue problem:
!
!      A * x = lambda * B * x
!
!    where A and B are symmetric and banded (and stored in LAPACK symmetric
!    band storage mode).  B is additionally assumed to be positive definite.
!
!    This is an "expert" interface, and the user is requesting
!    only some of the eigenvalues and eigenvectors.  In this example,
!    only the largest and smallest (in magnitude) eigenvalues will
!    be requested.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    real ( kind = 8 ) AB(LDAB,N), contains, on input, the upper or lower
!    triangle of the symmetric band matrix A, stored in the first KA+1 rows
!    of the array AB.
!    If UPLO = 'U', then
!      AB(KA+1+I-J,J) = A(I,J) for max(1,J-KA) <= I <= J;
!    If UPLO = 'L', then
!      AB(1+I-J,J) = A(I,J) for J <= I <= min(N,J+KA).
!
!    real ( kind = 8 ) ABSTOL, the absolute error tolerance for the eigenvalues.
!    If the input value of ABSTOL is not positive, then an appropriate
!    value will be determined internally and used instead.
!
!    real ( kind = 8 ) BB(LDBB,N), contains, on input, the upper or lower
!    triangle of the positive definite symmetric band matrix B, stored in
!    the first KB+1 rows of the array BB.
!    If UPLO = 'U', then
!      BB(KB+1+I-J,J) = B(I,J) for max(1,J-KB) <= I <= J;
!    If UPLO = 'L', then
!      BB(1+I-J,J) = B(I,J) for J <= I <= min(N,J+KB).
!
!    integer ( kind = 4 ) IFAIL(N), if JOBZ = 'V', then if INFO is 0, the first
!    M elements of IFAIL have been set to zero by DSBGVX, but if INFO
!    is nonzero, IFAIL contains the indices of the eigenvalues
!    for which the eigenvectors failed to converge.  If JOBZ = 'N',
!    then IFAIL is not referenced.
!
!    integer ( kind = 4 ) IL, IU, the indices of the first (smallest) and last
!    (largest) eigenvalues to be returned.  These values are only
!    used if RANGE = 'I'.  It must be the case that 1 <= IL <= IU <= N.
!
!    Integer INFO, is 0 for a successful computation,
!    negative if an input argument was illegal (the index of that
!    argument is the value of -INFO), or positive, in which case,
!    if 0 < INFO <= N, then INFO off diagonal elements of an
!    intermediate tridiagonal form did not converge to zero, or
!    if N < INFO, B is not positive definite and the computation
!    could not be completed.
!
!    integer ( kind = 4 ) IWORK(5*N), workspace.
!
!    character JOBZ, is 'N' if only eigenvalues are desired, or 'V'
!    if eigenvectors will also be required.
!
!    Integer KA, the number of superdiagonals (if UPLO = 'U') or
!    subdiagonals (if UPLO = 'L') of A that are nonzero.
!
!    integer ( kind = 4 ) KB, the number of superdiagonals (if UPLO = 'U') or
!    subdiagonals (if UPLO = 'L') of B that are nonzero.
!
!    integer ( kind = 4 ) LDAB, the leading dimension of the array AB, which
!    must be at least KA+1.
!
!    integer ( kind = 4 ) LDBB, the leading dimension of the array BB, which
!    must be at least KB+1.
!
!    integer ( kind = 4 ) LDQ, the leading dimension of the array Q.
!    If JOBZ = 'N', then Q is not used, and LDQ should be any
!    positive value such as 1.  If JOBZ = 'V', then LDQ must be
!    at least N.
!
!    integer ( kind = 4 ) LDZ, the leading dimension of the array Z.
!    If JOBZ = 'N', then Z is not used, and LDZ should be any
!    positive value such as 1.  If JOBZ = 'V', then LDZ must be
!    at least N.
!
!    integer ( kind = 4 ) M, the number of eigenvalues found by DSBGVX.
!
!    integer ( kind = 4 ) N, the order of the matrices A and B.
!
!    real ( kind = 8 ) Q(LDQ,N), if JOBZ = 'V', the N by N matrix used to
!    reduce the problem to standard form: "C * x = lambda * x"
!    and then to reduce the matrix C to tridiagonal form.  But
!    if JOBZ is not 'V', Q is not referenced.
!
!    character RANGE, specifies which eigenvalues are desired.
!    'A' means all, 'V' means a real interval will be specified in which
!    eigenvalues are to be sought, 'I' means a range of indices will
!    be specified.
!
!    character UPLO, is 'U' if the upper triangles of A and B are stored,
!    'L' if the lower triangles are stored.
!
!    real ( kind = 8 ) VL, VU, the lower and upper bounds of an interval to be
!    searched for eigenvalues.  In this case, VL must be less than VU.
!    These values are used only if RANGE = 'V'.
!
!    real ( kind = 8 ) W(N), the requested eigenvalues, in ascending order.
!
!    real ( kind = 8 ) WORK(7*N), workspace.
!
!    real ( kind = 8 ) Z(LDZ,N), if JOBZ = 'V', the I-th column of Z contains
!    the eigenvector associated with the I-th eigenvalue W(I).
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11
  integer ( kind = 4 ), parameter :: ka = 2
  integer ( kind = 4 ), parameter :: kb = 1

  integer ( kind = 4 ), parameter :: ldab = ka+1
  integer ( kind = 4 ), parameter :: ldbb = kb+1
  integer ( kind = 4 ), parameter :: ldq = 1
  integer ( kind = 4 ), parameter :: ldz = 1

  real ( kind = 8 ) ab(ldab,n)
  real ( kind = 8 ), parameter :: abstol = 0.0D+00
  real ( kind = 8 ) bb(ldbb,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifail(n)
  integer ( kind = 4 ) il
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) info
  integer ( kind = 4 ) iu
  integer ( kind = 4 ) iwork(5*n)
  integer ( kind = 4 ) j
  character :: jobz = 'N'
  integer ( kind = 4 ) m
  real ( kind = 8 ) q(ldq,n)
  character :: range = 'I'
  integer ( kind = 4 ) test
  character :: uplo = 'U'
  real ( kind = 8 ) value
  real ( kind = 8 ), parameter :: vl = 0.0D+00
  real ( kind = 8 ), parameter :: vu = 1.0D+00
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) work(7*n)
  real ( kind = 8 ) z(ldz,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DSBGVX_TEST'
  write ( *, '(a)' ) '  DSBGVX solves the generalized eigenvalue problem'
  write ( *, '(a)' ) '    A * X = LAMBDA * B * X'
  write ( *, '(a)' ) '  for a symmetric banded NxN matrix A, and a symmetric'
  write ( *, '(a)' ) '  banded positive definite NxN matrix B,'
  write ( *, '(a)' ) ' '

  do test = 1, 2
!
!  Set A.
!
    do j = 1, n
      ilo = max ( j - ka, 1 )
      do i = ilo, j

        if ( j == i-2 ) then
          value = -1.0D+00
        else if ( j == i-1 ) then
          value = -1.0D+00
        else if ( j == i ) then
          value = +4.0D+00
        else if ( j == i+1 ) then
          value = -1.0D+00
        else if ( j == i+2 ) then
          value = -1.0D+00
        else
          value = 0.0D+00
        end if

        ab(ka+1+i-j,j) = value

      end do
    end do
!
!  Set B.
!
    do j = 1, n
      ilo = max ( j - kb, 1 )
      do i = ilo, j

        if ( j == i-1 ) then
          value = -1.0D+00
        else if ( j == i ) then
          value = +2.0D+00
        else if ( j == i+1 ) then
          value = -1.0D+00
        else
          value = 0.0D+00
        end if

        bb(kb+1+i-j,j) = value

      end do
    end do
!
!  Request the value of the SMALLEST or LARGEST eigenvalue:
!
    if ( test == 1 ) then
      il = 1
      iu = 1
    else if ( test == 2 ) then
      il = n
      iu = n
    end if

    call dsbgvx ( jobz, range, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, &
      ldq, vl, vu, il, iu, abstol, m, w, z, ldz, work, iwork, ifail, info )

    if ( info < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Illegal value for input argument ', -info
      return
    else if ( 0 < info ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The eigenvalue or eigenvector iterations'
      write ( *, '(a)' ) '  did not converge.'
      cycle
    end if

    call r8vec_print ( m, w, '  Computed eigenvalues' )

  end do

  return
end
subroutine dsyev_test ( )

!*****************************************************************************80
!
!! DSYEV_TEST tests DSYEV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ), parameter :: lwork = 3 * n - 1

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) info
  character jobz
  real ( kind = 8 ) lambda(n)
  character uplo
  real ( kind = 8 ) work(lwork)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DSYEV_TEST'
  write ( *, '(a)' ) '  DSYEV computes eigenvalues and eigenvectors'
  write ( *, '(a)' ) '  For a double precision real matrix (D)'
  write ( *, '(a)' ) '  in symmetric storage mode (SY).'
  write ( *, '(a)' ) ' '
!
!  Set A.
!
  call clement2 ( n, a )

  call r8mat_print ( n, n, a, '  The matrix A:' )
!
!  Compute the eigenvalues and eigenvectors.
!
  jobz = 'V'
  uplo = 'U'

  call dsyev ( jobz, uplo, n, a, n, lambda, work, lwork, info )

  if ( info /= 0 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  DSYEV returned nonzero INFO = ', info

  else

    call r8vec_print ( n, lambda, '  The eigenvalues:' )

    if ( jobz == 'V' ) then
      call r8mat_print ( n, n, a, '  The eigenvector matrix:' )
    end if

  end if

  return
end
subroutine clement2 ( n, a )

!*****************************************************************************80
!
!! CLEMENT2 returns the Clement2 matrix.
!
!  Formula:
!
!    if ( J = I+1 )
!      A(I,J) = sqrt(I*(N-I))
!    else if ( I = J+1 )
!      A(I,J) = sqrt(J*(N-J))
!    else
!      A(I,J) = 0
!
!  Example:
!
!    N = 5
!
!       .    sqrt(4)    .       .       .
!    sqrt(4)    .    sqrt(6)    .       .
!       .    sqrt(6)    .    sqrt(6)    .
!       .       .    sqrt(6)    .    sqrt(4)
!       .       .       .    sqrt(4)    .
!
!  Properties:
!
!    A is tridiagonal.
!
!    Because A is tridiagonal, it has property A (bipartite).
!
!    A is symmetric: A' = A.
!
!    Because A is symmetric, it is normal.
!
!    Because A is normal, it is diagonalizable.
!
!    A is persymmetric: A(I,J) = A(N+1-J,N+1-I).
!
!    The diagonal of A is zero.
!
!    A is singular if N is odd.
!
!    About 64 percent of the entries of the inverse of A are zero.
!
!    The eigenvalues are plus and minus the numbers
!
!      N-1, N-3, N-5, ..., (1 or 0).
!
!    If N is even,
!
!      det ( A ) = (-1)**(N/2) * (N-1) * (N+1)**(N/2)
!
!    and if N is odd,
!
!      det ( A ) = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    P A Clement,
!    A class of triple-diagonal matrices for test purposes,
!    SIAM Review,
!    Volume 1, 1959, pages 50-52.
!
!  Parameters:
!
!    Input, integer N, the order of A.
!
!    Output, real ( kind = 8 ) A(N,N), the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do i = 1, n
    do j = 1, n

      if ( j == i + 1 ) then
        a(i,j) = sqrt ( real ( i * ( n - i ), kind = 8 ) )
      else if ( i == j + 1 ) then
        a(i,j) = sqrt ( real ( j * ( n - j ), kind = 8 ) )
      else
        a(i,j) = 0.0D+00
      end if

    end do
  end do

  return
end
function r8mat_norm_li ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NORM_LI returns the matrix L-oo norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The matrix L-oo norm is defined as:
!
!      R8MAT_NORM_LI =  max ( 1 <= I <= M ) sum ( 1 <= J <= N ) abs ( A(I,J) ).
!
!    The matrix L-oo norm is derived from the vector L-oo norm,
!    and satisifies:
!
!      r8vec_norm_li ( A * x ) <= r8mat_norm_li ( A ) * r8vec_norm_li ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix whose L-oo
!    norm is desired.
!
!    Output, real ( kind = 8 ) R8MAT_NORM_LI, the L-oo norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8mat_norm_li
  real ( kind = 8 ) row_sum

  r8mat_norm_li = 0.0D+00

  do i = 1, m
    row_sum = sum ( abs ( a(i,1:n) ) )
    r8mat_norm_li = max ( r8mat_norm_li, row_sum )
  end do

  return
end
subroutine r8mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_PRINT prints an R8MAT.
!
!  Discussion:
!
!    An R8MAT is a two dimensional matrix of double precision real values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the number of rows in A.
!
!    Input, integer N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME prints some of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is a two dimensional matrix of double precision real values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ILO, JLO, the first row and column to print.
!
!    Input, integer IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8,6x)' ) j
    end do

    write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( a(i,j) == real ( int ( a(i,j) ), kind = 8 ) ) then
          write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
        else
          write ( ctemp(j2), '(g14.6)' ) a(i,j)
        end if

      end do

      write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j), j = 1, inc )

    end do

  end do

  write ( *, '(a)' ) ' '

  return
end
subroutine r8mat_uniform_01 ( m, n, seed, r )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_01 fills an R8MAT with unit pseudorandom numbers.
!
!  Discussion:
!
!    An R8MAT is a two dimensional matrix of double precision real values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns in the array.
!
!    Input/output, integer SEED, the "seed" value, which should NOT be 0.
!    On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + huge ( seed )
      end if

      r(i,j) = real ( seed, kind = 8 ) * 4.656612875D-10

    end do
  end do

  return
end
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is an array of double precision real values.
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
!    Input, integer N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,2x,g16.8)' ) i, a(i)
  end do

  return
end
subroutine r8vec_print_some ( n, a, i_lo, i_hi, title )

!*****************************************************************************80
!
!! R8VEC_PRINT_SOME prints "some" of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, integer I_LO, I_HI, the first and last indices to print.
!    The routine expects 1 <= I_LO <= I_HI <= N.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_hi
  integer ( kind = 4 ) i_lo
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  do i = max ( i_lo, 1 ), min ( i_hi, n )
    write ( *, '(2x,i8,2x,g16.8)' ) i, a(i)
  end do

  return
end
