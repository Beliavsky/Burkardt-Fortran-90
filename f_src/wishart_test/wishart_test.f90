program main

!*****************************************************************************80
!
!! MAIN is the main program for WISHART_TEST.
!
!  Discussion:
!
!    WISHART_TEST tests the WISHART library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WISHART_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the WISHART library.'

  call wishart_unit_sample_test ( )
  call bartlett_unit_sample_test ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
  call test06 ( )
  call test07 ( )
  call test08 ( )
  call test09 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WISHART_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine wishart_unit_sample_test ( )

!*****************************************************************************80
!
!! WISHART_UNIT_SAMPLE_TEST demonstrates the unit Wishart sampling function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) df
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  real ( kind = 8 ), allocatable :: lambda(:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rot_num
  real ( kind = 8 ), allocatable :: v(:,:)
  real ( kind = 8 ), allocatable :: w(:,:)
!
!  Initialize the RNGLIB library.
!
  call initialize ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WISHART_UNIT_SAMPLE_TEST:'
  write ( *, '(a)' ) '  WISHART_UNIT_SAMPLE samples unit Wishart matrices by:'
  write ( *, '(a)' ) '  W = wishart_unit_sample ( n, df )'
!
!  Set the parameters and call.
!
  n = 5
  df = 8
  allocate ( w(1:n,1:n) )
  call wishart_unit_sample ( n, df, w )
  call r8mat_print ( n, n, w, '  wishart_unit_sample ( 5, 8 ):' )
  deallocate ( w )
!
!  Calling again yields a new matrix.
!
  allocate ( w(1:n,1:n) )
  call wishart_unit_sample ( n, df, w )
  call r8mat_print ( n, n, w, '  wishart_unit_sample ( 5, 8 ):' )
  deallocate ( w )
!
!  Reduce DF
!
  n = 5
  df = 5
  allocate ( w(1:n,1:n) )
  call wishart_unit_sample ( n, df, w )
  call r8mat_print ( n, n, w, '  wishart_unit_sample ( 5, 5 ):' )
  deallocate ( w )
!
!  Try a smaller matrix.
!
  n = 3
  df = 5
  allocate ( w(1:n,1:n) )
  call wishart_unit_sample ( n, df, w )
  call r8mat_print ( n, n, w, '  wishart_unit_sample ( 3, 5 ):' )
!
!  What is the eigendecomposition of the matrix?
!
  it_max = 50
  allocate ( v(1:n,1:n) )
  allocate ( lambda(1:n) )

  call jacobi_eigenvalue ( n, w, it_max, v, lambda, it_num, rot_num )
  call r8mat_print ( n, n, v, '  Eigenvectors of previous matrix:' )
  call r8vec_print ( n, lambda, '  Eigenvalues of previous matrix:' )
!
!  Free memory.
!
  deallocate ( lambda )
  deallocate ( v )
  deallocate ( w )

  return
end
subroutine bartlett_unit_sample_test ( )

!*****************************************************************************80
!
!! BARTLETT_UNIT_SAMPLE_TEST demonstrates the unit Bartlett sampling function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) df
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  real ( kind = 8 ), allocatable :: lambda(:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rot_num
  real ( kind = 8 ), allocatable :: t(:,:)
  real ( kind = 8 ), allocatable :: v(:,:)
  real ( kind = 8 ), allocatable :: w(:,:)
!
!   Initialize the RNGLIB library.
!
  call initialize ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BARTLETT_UNIT_SAMPLE_TEST:'
  write ( *, '(a)' ) '  BARTLETT_UNIT_SAMPLE samples unit Bartlett matrices by:'
  write ( *, '(a)' ) '  call bartlett_unit_sample ( n, df, t )'
!
!   Set the parameters and call.
!
  n = 5
  df = 8
  allocate ( t(1:n,1:n) )
  call bartlett_unit_sample ( n, df, t )
  call r8mat_print ( n, n, t, '  bartlett_unit_sample ( 5, 8, t ):' )
  deallocate ( t )
!
!   Calling again yields a new matrix.
!
  allocate ( t(1:n,1:n) )
  call bartlett_unit_sample ( n, df, t )
  call r8mat_print ( n, n, t, '  bartlett_unit_sample ( 5, 8, t ):' )
  deallocate ( t )
!
!   Reduce DF.
!
  n = 5
  df = 5
  allocate ( t(1:n,1:n) )
  call bartlett_unit_sample ( n, df, t )
  call r8mat_print ( n, n, t, '  bartlett_unit_sample ( 5, 5, t ):' )
  deallocate ( t )
!
!   Try a smaller matrix.
!
  n = 3
  df = 5
  allocate ( t(1:n,1:n) )
  call bartlett_unit_sample ( n, df, t )
  call r8mat_print ( n, n, t, '  bartlett_unit_sample ( 3, 5, t ):' )
!
!   What is the eigendecomposition of the matrix T' * T?
!
  allocate ( w(1:n,1:n) )

  w = matmul ( transpose ( t(1:n,1:n) ), t(1:n,1:n) )

  it_max = 50
  allocate ( v(1:n,1:n) )
  allocate ( lambda(1:n) )

  call jacobi_eigenvalue ( n, w, it_max, v, lambda, it_num, rot_num )
  call r8mat_print ( n, n, v, '  Eigenvectors of previous matrix:' )
  call r8vec_print ( n, lambda, '  Eigenvalues of previous matrix:' )
!
!  Free memory.
!
  deallocate ( lambda )
  deallocate ( t )
  deallocate ( v )
  deallocate ( w )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 compares the unit Wishart and Bartlett sample matrices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) df
  real ( kind = 8 ) diff
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8mat_norm_fro_affine
  real ( kind = 8 ), allocatable :: t(:,:)
  real ( kind = 8 ), allocatable :: tt(:,:)
  real ( kind = 8 ), allocatable :: w(:,:)
!
!   Initialize the RNGLIB library.
!   Normally, we would do this just once, here at the beginning.
!   In this example, however, we really want to do it just before
!   we call each of the sampling routines, so that they both access
!   the same set of random numbers...
!
  call initialize ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03:'
  write ( *, '(a)' ) '  Verify that, if using the same set of random numbers,'
  write ( *, '(a)' ) '    W = T'' * T,'
  write ( *, '(a)' ) '  where'
  write ( *, '(a)' ) '    W = wishart_unit_sample ( n, df )'
  write ( *, '(a)' ) '    T = bartlett_unit_sample ( n, df )'
!
!   Set the parameters.
!
  n = 5
  df = 8
!
!   Initialize the random number package and compute W.
!
  call initialize ( )
  allocate ( w(1:n,1:n) )
  call wishart_unit_sample ( n, df, w )
!
!   Initialize the random number package again, and compute T.
!
  call initialize ( )
  allocate ( t(1:n,1:n) )
  call bartlett_unit_sample ( n, df, t )
!
!   Compute T' * T.
!
  allocate ( tt(1:n,1:n) )
  tt = matmul ( transpose ( t(1:n,1:n) ), t(1:n,1:n) )
!
!   Compare T'T to W.
!
  diff = r8mat_norm_fro_affine ( n, n, w, tt )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Frobenius norm of error is ', diff
!
!  Free memory.
!
  deallocate ( t )
  deallocate ( tt )
  deallocate ( w )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 demonstrates the Wishart sampling function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) df
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  real ( kind = 8 ), allocatable :: lambda(:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rot_num
!
!  Note that R is an upper triangular matrix,
!  whose entries here are listed in column major order.
!
  real ( kind = 8 ), dimension ( 3, 3 ) :: r = reshape ( (/ &
    5.0D+00, 0.0D+00, 0.0D+00, &
    1.0D+00, 4.0D+00, 0.0D+00, &
    3.0D+00, 2.0D+00, 6.0D+00 /), (/ 3, 3 /) )
  real ( kind = 8 ), allocatable :: sigma(:,:)
  real ( kind = 8 ), dimension ( 5 ) :: sigma_diag = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /)
  real ( kind = 8 ), allocatable :: v(:,:)
  real ( kind = 8 ), allocatable :: w(:,:)
!
!  Initialize the RNGLIB library.
!
  call initialize ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST04:'
  write ( *, '(a)' ) '  We can compute sample Wishart matrices by:'
  write ( *, '(a)' ) '    W = wishart_sample ( n, df, sigma )'
!
!   Set the parameters and call.
!
  n = 5
  df = 8
  allocate ( sigma(1:n,1:n) )
  call r8mat_identity ( n, sigma )

  allocate ( w(1:n,1:n) )
  call wishart_sample ( n, df, sigma, w )
  call r8mat_print ( n, n, w, '  wishart_sample ( 5, 8, Identity ):' )
  deallocate ( w )
!
!   Calling again yields a new matrix.
!
  allocate ( w(1:n,1:n) )
  call wishart_sample ( n, df, sigma, w )
  call r8mat_print ( n, n, w, '  wishart_sample ( 5, 8, Identity ):' )
  deallocate ( sigma )
  deallocate ( w )
!
!   Try a diagonal matrix.
!
  allocate ( sigma(1:n,1:n) )
  call r8mat_diagonal ( n, sigma_diag, sigma )

  allocate ( w(1:n,1:n) )
  call wishart_sample ( n, df, sigma, w )
  call r8mat_print ( n, n, w, '  wishart_sample ( 5, 8, diag(1,2,3,4,5) ):' )
  deallocate ( sigma )
  deallocate ( w )
!
!   Try a smaller matrix.  Sigma must be positive definite symmetric.
!
  n = 3
  df = 3
  allocate ( sigma(1:n,1:n) )
  sigma = matmul ( transpose ( r(1:n,1:n) ), r(1:n,1:n) )
  call r8mat_print ( n, n, sigma, '  Set covariance SIGMA:' )

  allocate ( w(1:n,1:n) )
  call wishart_sample ( n, df, sigma, w )
  call r8mat_print ( n, n, w, '  wishart_sample ( 3, 3, sigma ):' )
!
!   What is the eigendecomposition of this matrix?
!
  it_max = 50
  allocate ( v(1:n,1:n) )
  allocate ( lambda(1:n) )

  call jacobi_eigenvalue ( n, w, it_max, v, lambda, it_num, rot_num )
  call r8mat_print ( n, n, v, '  Eigenvectors of previous matrix:' )
  call r8vec_print ( n, lambda, '  Eigenvalues of previous matrix:' )
!
!  Free memory.
!
  deallocate ( lambda )
  deallocate ( sigma )
  deallocate ( v )
  deallocate ( w )

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 demonstrates the Bartlett sampling function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) df
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  real ( kind = 8 ), allocatable :: lambda(:)
  integer ( kind = 4 ) n
!
!  Note that R is an upper triangular matrix,
!  whose entries here are listed in column major order.
!
  real ( kind = 8 ), dimension ( 3, 3 ) :: r = reshape ( (/ &
    5.0D+00, 0.0D+00, 0.0D+00, &
    1.0D+00, 4.0D+00, 0.0D+00, &
    3.0D+00, 2.0D+00, 6.0D+00 /), (/ 3, 3 /) )
  integer ( kind = 4 ) rot_num
  real ( kind = 8 ), allocatable :: sigma(:,:)
  real ( kind = 8 ), dimension ( 5 ) :: sigma_diag = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /)
  real ( kind = 8 ), allocatable :: t(:,:)
  real ( kind = 8 ), allocatable :: v(:,:)
  real ( kind = 8 ), allocatable :: w(:,:)
!
!   Initialize the RNGLIB library.
!
  call initialize ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST05:'
  write ( *, '(a)' ) '  We can compute sample Bartlett matrices by:'
  write ( *, '(a)' ) '    T = bartlett_sample ( n, df, sigma )'
!
!   Set the parameters and call.
!
  n = 5
  df = 8
  allocate ( sigma(1:n,1:n) )
  call r8mat_identity ( n, sigma )

  allocate ( t(1:n,1:n) )
  call bartlett_sample ( n, df, sigma, t )
  call r8mat_print ( n, n, t, '  bartlett_sample ( 5, 8, Identity ):' )
  deallocate ( t )
!
!   Calling again yields a new matrix.
!
  allocate ( t(1:n,1:n) )
  call bartlett_sample ( n, df, sigma, t )
  call r8mat_print ( n, n, t, '  bartlett_sample ( 5, 8, Identity ):' )
  deallocate ( sigma )
  deallocate ( t )
!
!   Try a diagonal matrix.
!
  allocate ( sigma(1:n,1:n) )
  call r8mat_diagonal ( n, sigma_diag, sigma )

  allocate ( t(1:n,1:n) )
  call bartlett_sample ( n, df, sigma, t )
  call r8mat_print ( n, n, t, '  bartlett_sample ( 5, 8, diag(1,2,3,4,5) ):' )
  deallocate ( sigma )
  deallocate ( t )
!
!   Try a smaller matrix.
!
  n = 3
  df = 3
  allocate ( sigma(1:n,1:n) )
  sigma = matmul ( transpose ( r(1:n,1:n) ), r(1:n,1:n) )
  call r8mat_print ( n, n, sigma, '  Set covariance SIGMA:' )

  allocate ( t(1:n,1:n) )
  call bartlett_sample ( n, df, sigma, t )
  call r8mat_print ( n, n, t, '  bartlett_sample ( 3, 3, sigma ):' )
!
!   What is the eigendecomposition of T' * T?
!
  w = matmul ( transpose ( t(1:n,1:n) ), t(1:n,1:n) )
  it_max = 50
  allocate ( v(1:n,1:n) )
  allocate ( lambda(1:n) )

  call jacobi_eigenvalue ( n, w, it_max, v, lambda, it_num, rot_num )
  call r8mat_print ( n, n, v, '  Eigenvectors of previous matrix:' )
  call r8vec_print ( n, lambda, '  Eigenvalues of previous matrix:' )
!
!  Free memory.
!
  deallocate ( lambda )
  deallocate ( sigma )
  deallocate ( t )
  deallocate ( v )
  deallocate ( w )

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 compares the Wishart and Bartlett sample matrices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) df
  real ( kind = 8 ) diff
  integer ( kind = 4 ) n
!
!  Note that R is an upper triangular matrix,
!  whose entries here are listed in column major order.
!
  real ( kind = 8 ), dimension ( 3, 3 ) :: r = reshape ( (/ &
    5.0D+00, 0.0D+00, 0.0D+00, &
    1.0D+00, 4.0D+00, 0.0D+00, &
    3.0D+00, 2.0D+00, 6.0D+00 /), (/ 3, 3 /) )
  real ( kind = 8 ) r8mat_norm_fro_affine
  real ( kind = 8 ), allocatable :: sigma(:,:)
  real ( kind = 8 ), allocatable :: t(:,:)
  real ( kind = 8 ), allocatable :: tt(:,:)
  real ( kind = 8 ), allocatable :: w(:,:)
!
!   Initialize the RNGLIB library.
!   Normally, we would do this just once, here at the beginning.
!   In this example, however, we really want to do it just before
!   we call each of the sampling routines, so that they both access
!   the same set of random numbers...
!
  call initialize ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST06:'
  write ( *, '(a)' ) '  Verify that, if using the same set of random numbers,'
  write ( *, '(a)' ) '    W = T'' * T,'
  write ( *, '(a)' ) '  where'
  write ( *, '(a)' ) '    W = wishart_sample ( n, df, sigma )'
  write ( *, '(a)' ) '    T = bartlett_sample ( n, df, sigma )'
!
!   Set the parameters.
!
  n = 3
  df = 5
  allocate ( sigma(1:n,1:n) )
  sigma = matmul ( transpose ( r(1:n,1:n) ), r(1:n,1:n) )
  call r8mat_print ( n, n, sigma, "  Covariance SIGMA:" )
!
!   Initialize the random number package and compute W.
!
  call initialize ( )
  allocate ( w(1:n,1:n) )
  call wishart_sample ( n, df, sigma, w )
!
!   Initialize the random number package again, and compute T.
!
  call initialize ( )
  allocate ( t(1:n,1:n) )
  call bartlett_sample ( n, df, sigma, t )
!
!   Compute T' * T.
!
  allocate ( tt(1:n,1:n) )
  tt = matmul ( transpose ( t(1:n,1:n) ), t(1:n,1:n) )
!
!   Compare T'T to W.
!
  diff = r8mat_norm_fro_affine ( n, n, w, tt )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Frobenius norm of error is ', diff
!
!  Free memory.
!
  deallocate ( sigma )
  deallocate ( t )
  deallocate ( tt )
  deallocate ( w )

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! TEST07 demonstrates a property of the Wishart distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) df
  real ( kind = 8 ) diff
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
!
!  Note that R is an upper triangular matrix,
!  whose entries here are listed in column major order.
!
  real ( kind = 8 ), dimension ( 3, 3 ) :: r = reshape ( (/ &
    5.0D+00, 0.0D+00, 0.0D+00, &
    1.0D+00, 4.0D+00, 0.0D+00, &
    3.0D+00, 2.0D+00, 6.0D+00 /), (/ 3, 3 /) )
  real ( kind = 8 ) r8mat_norm_fro_affine
  integer ( kind = 4 ) sample_num
  real ( kind = 8 ), allocatable :: sigma(:,:)
  real ( kind = 8 ), allocatable ::  w(:,:)
  real ( kind = 8 ), allocatable ::  w_average(:,:)
!
!   Initialize the RNGLIB library.
!
  call initialize ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST07:'
  write ( *, '(a)' ) '  For given values of N, DF, SIGMA, the random\'
  write ( *, '(a)' ) '  matrices from the Wishart distribution:'
  write ( *, '(a)' ) '    W = wishart_sample ( n, df, sigma )'
  write ( *, '(a)' ) '  should have mean DF * SIGMA.'
!
!   Set the parameters.
!
  n = 3
  write ( *, '(a,i4)' ) '  Fix N = ', n
  df = 5
  write ( *, '(a,i4)' ) '  Fix DF = ', df
  allocate ( sigma(1:n,1:n) )
  sigma = matmul ( transpose ( r(1:n,1:n) ), r(1:n,1:n) )
  call r8mat_print ( n, n, sigma, '  Fix covariance SIGMA:' )
!
!   Sample many times and average.
!
  sample_num = 1000
  allocate ( w_average(1:n,1:n) )
  w_average(1:n,1:n) = 0.0D+00

  allocate ( w(1:n,1:n) )

  do i = 1, sample_num
    call wishart_sample ( n, df, sigma, w )
    w_average(1:n,1:n) = w_average(1:n,1:n) + w(1:n,1:n)
  end do

  deallocate ( w )

  w_average(1:n,1:n) = w_average(1:n,1:n) / real ( sample_num, kind = 8 )
!
!   Compare SIGMA and W_SAMPLE / DF.
!
  w_average(1:n,1:n) = w_average(1:n,1:n) / real ( df, kind = 8 )

  call r8mat_print ( n, n, w_average, '  W_Average / DF: ' )

  diff = r8mat_norm_fro_affine ( n, n, sigma, w_average )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Frobenius norm of SIGMA-W_average/DF = ', diff
!
!  Free memory.
!
  deallocate ( sigma )
  deallocate ( w_average )

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 samples the unit Wishart and unit Wishart inverse matrices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) df
  real ( kind = 8 ) diff
  integer ( kind = 4 ) i
  real ( kind = 8 ), allocatable :: ident(:,:)
  integer ( kind = 4 ) j 
  real ( kind = 8 ), allocatable :: m(:,:)
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8mat_norm_fro_affine
  real ( kind = 8 ), allocatable :: w(:,:)
  real ( kind = 8 ), allocatable :: wm(:,:)
!
!   Initialize the RNGLIB library.
!   Normally, we would do this just once, here at the beginning.
!   In this example, however, we really want to do it just before
!   we call each of the sampling routines, so that they both access
!   the same set of random numbers...
!
  call initialize ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST08:'
  write ( *, '(a)' ) '  Verify that, if using the same set of random numbers,'
  write ( *, '(a)' ) '    inverse(W) = M,'
  write ( *, '(a)' ) '  where'
  write ( *, '(a)' ) '    W = wishart_unit_sample ( n, df )'
  write ( *, '(a)' ) '    M = wishart_unit_sample_inverse ( n, df )'
!
!  Set the parameters.
!
  n = 5
  df = 8
!
!  Initialize the random number package and compute W.
!
  call initialize ( )
  allocate ( w(1:n,1:n) )
  call wishart_unit_sample ( n, df, w )
!
!  Initialize the random number package again, and compute M.
!
  call initialize ( )
  allocate ( m(1:n,1:n) )
  call wishart_unit_sample_inverse ( n, df, m )
!
!  Compute W * M.
!
  allocate ( wm(1:n,1:n) )
  wm = matmul ( w, m )
!
!  Compare W*M to I.
!
  allocate ( ident(1:n,1:n) )

  do j = 1, n
    do i = 1, n
      ident(i,j) = 0.0D+00
    end do
    ident(j,j) = 1.0D+00
  end do

  diff = r8mat_norm_fro_affine ( n, n, wm, ident )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Frobenius norm of error is ', diff
!
!  Free memory.
!
  deallocate ( ident )
  deallocate ( m )
  deallocate ( w )
  deallocate ( wm )

  return
end
subroutine test09 ( )

!*****************************************************************************80
!
!! TEST09 samples the Wishart and Wishart inverse matrices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) df
  real ( kind = 8 ) diff
  integer ( kind = 4 ) i
  real ( kind = 8 ), allocatable :: ident(:,:)
  integer ( kind = 4 ) j 
  real ( kind = 8 ), allocatable :: m(:,:)
  integer ( kind = 4 ) n
!
!  Note that R is an upper triangular matrix,
!  whose entries here are listed in column major order.
!
  real ( kind = 8 ), dimension ( 5, 5 ) :: r = reshape ( (/ &
    3.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, &
    1.0D+00, 7.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, &
    1.0D+00, 1.0D+00, 5.0D+00, 0.0D+00, 0.0D+00, &
    1.0D+00, 2.0D+00, 1.0D+00, 4.0D+00, 0.0D+00, &
    1.0D+00, 3.0D+00, 3.0D+00, 2.0D+00, 6.0D+00 /), (/ 5, 5 /) )
  real ( kind = 8 ) r8mat_norm_fro_affine
  real ( kind = 8 ), allocatable :: sigma(:,:)
  real ( kind = 8 ), allocatable :: w(:,:)
  real ( kind = 8 ), allocatable :: wm(:,:)
!
!   Initialize the RNGLIB library.
!   Normally, we would do this just once, here at the beginning.
!   In this example, however, we really want to do it just before
!   we call each of the sampling routines, so that they both access
!   the same set of random numbers...
!
  call initialize ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST09:'
  write ( *, '(a)' ) '  Verify that, if using the same set of random numbers,'
  write ( *, '(a)' ) '    inverse(W) = M,'
  write ( *, '(a)' ) '  where'
  write ( *, '(a)' ) '    W = wishart_sample ( n, df, sigma )'
  write ( *, '(a)' ) '    M = wishart_sample_inverse ( n, df, sigma )'
!
!   Set the parameters.
!
  n = 5
  df = 8
  allocate ( sigma(1:n,1:n) )
  sigma = matmul ( transpose ( r ), r )
!
!   Initialize the random number package and compute W.
!
  call initialize ( )
  allocate ( w(1:n,1:n) )
  call wishart_sample ( n, df, sigma, w )
!
!   Initialize the random number package again, and compute M.
!
  call initialize ( )
  allocate ( m(1:n,1:n) )
  call wishart_sample_inverse ( n, df, sigma, m )
!
!   Compute W * M.
!
  allocate ( wm(1:n,1:n) )
  wm = matmul ( w, m )
!
!   Compare W*M to I.
!
  allocate ( ident(1:n,1:n) )

  do j = 1, n
    do i = 1, n
      ident(i,j) = 0.0D+00
    end do
    ident(j,j) = 1.0D+00
  end do

  diff = r8mat_norm_fro_affine ( n, n, wm, ident )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Frobenius norm of error is ', diff
!
!  Free memory.
!
  deallocate ( ident )
  deallocate ( m )
  deallocate ( sigma )
  deallocate ( w )
  deallocate ( wm )

  return
end
