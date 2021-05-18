program main

!*****************************************************************************80
!
!! MAIN is the main program for TEST_EIGEN_TEST.
!
!  Discussion:
!
!    TEST_EIGEN_TEST tests the TEST_EIGEN library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_EIGEN_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the TEST_EIGEN library.'

  call r8symm_gen_test ( )
  call r8nsymm_gen_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST_EIGEN_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine r8symm_gen_test ( )

!*****************************************************************************80
!
!! R8SYMM_GEN_TEST tests R8SYMM_GEN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 100
  integer ( kind = 4 ), parameter :: bin_num = 10

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) aq(n,n)
  integer ( kind = 4 ) bin(0:bin_num+1)
  real ( kind = 8 ) bin_limit(0:bin_num)
  integer ( kind = 4 ) j
  real ( kind = 8 ) lambda(n)
  real ( kind = 8 ) lambda2(n)
  real ( kind = 8 ), parameter :: lambda_dev = 1.0D+00
  real ( kind = 8 ) lambda_max
  real ( kind = 8 ), parameter :: lambda_mean = 1.0D+00
  real ( kind = 8 ) lambda_min
  real ( kind = 8 ) q(n,n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8SYMM_GEN_TEST'
  write ( *, '(a)' ) '  R8SYMM_GEN generates an arbitrary size symmetric matrix'
  write ( *, '(a)' ) '  with known eigenvalues and eigenvectors.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Real data is declared as "REAL ( KIND = 8 )".'

  call r8symm_gen ( n, lambda_mean, lambda_dev, seed, a, q, lambda )
!
!  Get the eigenvalue range.
!
  lambda_min = minval ( lambda(1:n) )
  lambda_max = maxval ( lambda(1:n) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  LAMBDA_MIN = ', lambda_min
  write ( *, '(a,g14.6)' ) '  LAMBDA_MAX = ', lambda_max
!
!  Bin the eigenvalues.
!
  call r8vec_bin ( n, lambda, bin_num, lambda_min, lambda_max, bin, bin_limit )

  call r8bin_print ( bin_num, bin, bin_limit, '  Lambda bins:' )

  if ( .false. ) then
    call r8mat_print ( n, n, a, '  The matrix A:' )
  end if

  if ( .false. ) then
    call r8mat_print ( n, n, q, '  The eigenvector matrix Q:' )
  end if

  aq(1:n,1:n) = matmul ( a(1:n,1:n), q(1:n,1:n) )

  do j = 1, n
    lambda2(j) = sqrt ( sum ( aq(1:n,j)**2 ) )
  end do

  call r8vec2_print ( n, lambda, lambda2, &
    '  LAMBDA versus the column norms of A*Q:' )

  return
end
subroutine r8nsymm_gen_test ( )

!*****************************************************************************80
!
!! R8NSYMM_GEN_TEST tests R8NSYMM_GEN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 100
  integer ( kind = 4 ), parameter :: bin_num = 10

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) aqr(n,n)
  integer ( kind = 4 ) bin(0:bin_num+1)
  real ( kind = 8 ) bin_limit(0:bin_num)
  integer ( kind = 4 ) j
  real ( kind = 8 ) lambda(n)
  real ( kind = 8 ) lambda2(n)
  real ( kind = 8 ), parameter :: lambda_dev = 1.0D+00
  real ( kind = 8 ) lambda_max
  real ( kind = 8 ), parameter :: lambda_mean = 1.0D+00
  real ( kind = 8 ) lambda_min
  real ( kind = 8 ) ql(n,n)
  real ( kind = 8 ) qr(n,n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8NSYMM_GEN_TEST'
  write ( *, '(a)' ) '  R8NSYMM_GEN generates an arbitrary size nonsymmetric'
  write ( *, '(a)' ) '  matrix with known eigenvalues and eigenvectors.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Real data is declared as "REAL ( KIND = 8 )".'

  call r8nsymm_gen ( n, lambda_mean, lambda_dev, seed, a, ql, qr, lambda )
!
!  Get the eigenvalue range.
!
  lambda_min = minval ( lambda(1:n) )
  lambda_max = maxval ( lambda(1:n) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  LAMBDA_MIN = ', lambda_min
  write ( *, '(a,g14.6)' ) '  LAMBDA_MAX = ', lambda_max
!
!  Bin the eigenvalues.
!
  call r8vec_bin ( n, lambda, bin_num, lambda_min, lambda_max, bin, bin_limit )

  call r8bin_print ( bin_num, bin, bin_limit, '  Lambda bins:' )

  if ( .false. ) then
    call r8mat_print ( n, n, a, '  The matrix A:' )
  end if

  if ( .false. ) then
    call r8mat_print ( n, n, qr, '  The right eigenvector matrix QR:' )
  end if

  aqr(1:n,1:n) = matmul ( a(1:n,1:n), qr(1:n,1:n) )

  do j = 1, n
    lambda2(j) = sqrt ( sum ( aqr(1:n,j)**2 ) )
  end do

  call r8vec2_print ( n, lambda, lambda2, &
    '  LAMBDA versus the column norms of A*Q:' )

  return
end
