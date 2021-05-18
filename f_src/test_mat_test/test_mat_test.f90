program main

!*****************************************************************************80
!
!! test_mat_test() tests test_mat().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 May 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_mat_test():'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test test_mat().'
!
!  Utilities.
!
  call bvec_next_grlex_test ( ) 
  call legendre_zeros_test ( )
  call mertens_test ( )
  call moebius_test ( )
  call r8mat_is_eigen_left_test ( )
  call r8mat_is_eigen_right_test ( )
  call r8mat_is_llt_test ( )
  call r8mat_is_null_left_test ( )
  call r8mat_is_null_right_test ( )
  call r8mat_is_solution_test ( )
  call r8mat_norm_fro_test ( )
!
!  Important things.
!
  call test_analyze ( )
  call test_condition ( )
  call test_determinant ( )
  call test_eigen_left ( )
  call test_eigen_right ( )
  call test_inverse ( )
  call test_llt ( )
  call test_null_left ( )
  call test_null_right ( )
  call test_plu ( )
  call test_solution ( )
  call test_type ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_mat_test()'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine bvec_next_grlex_test ( )

!*****************************************************************************80
!
!! bvec_next_grlex_test() tests bvec_next_grlex().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
 
  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'bvec_next_grlex_test():'
  write ( *, '(a)' ) '  bvec_next_grlex() computes binary vectors in GRLEX order.'
  write ( *, '(a)' ) ''

  b(1:n) = 0

  do i = 0, 16
    write ( *, '(2x,i2,a)', advance = 'no' ) i, ':  '
    do j = 1, n
      write ( *, '(i1)', advance = 'no' ) b(j)
    end do
    write ( *, '(a)' ) ''
    call bvec_next_grlex ( n, b )
  end do

  return
end
subroutine legendre_zeros_test ( )

!*****************************************************************************80
!
!! legendre_zeros_test() tests LEGENDRE_zeroS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: l(:)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'legendre_zeros_test():'
  write ( *, '(a)' ) '  LEGENDRE_zeroS computes the zeros of the N-th Legendre'
  write ( *, '(a)' ) '  polynomial.'

  do n = 1, 7
    allocate ( l(1:n) )
    call legendre_zeros ( n, l )
    call r8vec_print ( n, l, '  Legendre zeros' )
    deallocate ( l )
  end do

  return
end
subroutine mertens_test ( )

!*****************************************************************************80
!
!! mertens_test() tests MERTENS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 October 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) c
  integer ( kind = 4 ) c2
  integer ( kind = 4 ) mertens
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'mertens_test():'
  write ( *, '(a)' ) '  MERTENS computes the Mertens function.'
  write ( *, '(a)' ) ' ' 
  write ( *, '(a)' ) '         N     Exact   MERTENS(N)'
  write ( *, '(a)' ) ' '
 
  n_data = 0

  do

    call mertens_values ( n_data, n, c )

    if ( n_data == 0 ) then
      exit
    end if

    c2 = mertens ( n )

    write ( *, '(2x,i8,2x,i10,2x,i10)' ) n, c, c2

  end do
 
  return
end
subroutine moebius_test ( )

!*****************************************************************************80
!
!! moebius_test() tests MOEBIUS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) c
  integer ( kind = 4 ) c2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'moebius_test():'
  write ( *, '(a)' ) '  MOEBIUS computes the Moebius function.'
  write ( *, '(a)' ) ' ' 
  write ( *, '(a)' ) '         N     Exact   MOEBIUS(N)'
  write ( *, '(a)' ) ' '
 
  n_data = 0

  do

    call moebius_values ( n_data, n, c )

    if ( n_data == 0 ) then
      exit
    end if

    call moebius ( n, c2 )

    write ( *, '(2x,i8,2x,i10,2x,i10)' ) n, c, c2

  end do
 
  return
end
subroutine r8mat_is_eigen_left_test ( )

!*****************************************************************************80
!
!! r8mat_is_eigen_left_test() tests R8MAT_IS_EIGEN_LEFT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
  integer ( kind = 4 ), parameter :: k = 4
!
!  This is the carry_matrix ( 4.0, 4 ) matrix.
!
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
   0.13671875D+00,   0.05859375D+00,   0.01953125D+00,   0.00390625D+00, &
   0.60546875D+00,   0.52734375D+00,   0.39453125D+00,   0.25390625D+00, &
   0.25390625D+00,   0.39453125D+00,   0.52734375D+00,   0.60546875D+00, &
   0.00390625D+00,   0.01953125D+00,   0.05859375D+00,   0.13671875D+00 /), &
   (/ n, n /) )
  real ( kind = 8 ), dimension ( n ) :: lam = (/ &
     1.000000000000000D+00, &
     0.250000000000000D+00, &
     0.062500000000000D+00, &
     0.015625000000000D+00 /)
  real ( kind = 8 ) value
  real ( kind = 8 ), dimension ( n, k ) :: x = reshape ( (/ &
       1.0D+00, 11.0D+00, 11.0D+00,  1.0D+00, &
       1.0D+00,  3.0D+00, -3.0D+00, -1.0D+00, &
       1.0D+00, -1.0D+00, -1.0D+00,  1.0D+00, &
       1.0D+00, -3.0D+00,  3.0D+00, -1.0D+00 /), &
    (/ n, k /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8mat_is_eigen_left_test():'
  write ( *, '(a)' ) '  R8MAT_IS_EIGEN_LEFT tests the error in the left eigensystem'
  write ( *, '(a)' ) '    A'' * X - X * LAMBDA = 0'

  call r8mat_print ( n, n, a, '  Matrix A:' )
  call r8mat_print ( n, k, x, '  Eigenmatrix X:' )
  call r8vec_print ( n, lam, '  Eigenvalues LAM:' )

  call r8mat_is_eigen_left ( n, k, a, x, lam, value )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Frobenius norm of A''*X-X*LAMBDA is ', value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_IS_EIGEN_LEFT_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'

  return
end
subroutine r8mat_is_eigen_right_test ( )

!*****************************************************************************80
!
!! r8mat_is_eigen_right_test() tests R8MAT_IS_EIGEN_RIGHT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
  integer ( kind = 4 ), parameter :: k = 4
!
!  This is the carry_matrix ( 4.0, 4 ) matrix.
!
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
   0.13671875D+00,   0.05859375D+00,   0.01953125D+00,   0.00390625D+00, &
   0.60546875D+00,   0.52734375D+00,   0.39453125D+00,   0.25390625D+00, &
   0.25390625D+00,   0.39453125D+00,   0.52734375D+00,   0.60546875D+00, &
   0.00390625D+00,   0.01953125D+00,   0.05859375D+00,   0.13671875D+00 /), &
   (/ n, n /) )
  real ( kind = 8 ), dimension ( n ) :: lam = (/ &
     1.000000000000000D+00, &
     0.250000000000000D+00, &
     0.062500000000000D+00, &
     0.015625000000000D+00 /)
  real ( kind = 8 ) value
  real ( kind = 8 ), dimension ( n, k ) :: x = reshape ( (/ &
       1.0D+00,  1.0D+00,  1.0D+00,  1.0D+00, &
       6.0D+00,  2.0D+00, -2.0D+00, -6.0D+00, &
      11.0D+00, -1.0D+00, -1.0D+00, 11.0D+00, &
       6.0D+00, -2.0D+00,  2.0D+00, -6.0D+00 /), &
    (/ n, k /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8mat_is_eigen_right_test():'
  write ( *, '(a)' ) '  R8MAT_IS_EIGEN_RIGHT tests the error in the right eigensystem'
  write ( *, '(a)' ) '    A * X - X * LAMBDA = 0'

  call r8mat_print ( n, n, a, '  Matrix A:' )
  call r8mat_print ( n, k, x, '  Eigenmatrix X:' )
  call r8vec_print ( n, lam, '  Eigenvalues LAM:' )

  call r8mat_is_eigen_right ( n, k, a, x, lam, value )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Frobenius norm of A*X-X*LAMBDA is ', value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_IS_EIGEN_RIGHT_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'

  return
end
subroutine r8mat_is_llt_test ( )

!*****************************************************************************80
!
!! r8mat_is_llt_test() tests R8MAT_IS_LLT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ), dimension ( 4, 4 ) :: a = reshape ( (/ &
    2.0D+00, 1.0D+00, 0.0D+00, 0.0D+00, &
    1.0D+00, 2.0D+00, 1.0D+00, 0.0D+00, &
    0.0D+00, 1.0D+00, 2.0D+00, 1.0D+00, &
    0.0D+00, 0.0D+00, 1.0D+00, 2.0D+00 /), (/ 4, 4 /) )
  real ( kind = 8 ) enorm
  real ( kind = 8 ), dimension ( 4, 4 ) :: l = reshape ( (/ &
    1.414213562373095D+00, 0.707106781186547D+00, &
    0.0D+00,               0.0D+00,               &
    0.0D+00,               1.224744871391589D+00, &
    0.816496580927726D+00, 0.0D+00,               &
    0.0D+00,               0.0D+00,               &
    1.154700538379251D+00, 0.866025403784439D+00, &
    0.0D+00,               0.0D+00,               &
    0.0D+00,               1.118033988749895D+00 /), (/ 4, 4 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8mat_is_llt_test():'
  write ( *, '(a)' ) '  R8MAT_IS_LLT tests the error in a lower triangular'
  write ( *, '(a)' ) '  Cholesky factorization A = L * L'' by looking at'
  write ( *, '(a)' ) '    A - L * L'''

  call r8mat_print ( m, m, a, '  Matrix A:' )
  call r8mat_print ( m, n, l, '  Factor L:' )

  call r8mat_is_llt ( m, n, a, l, enorm )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Frobenius norm of A-L*L'' is ', enorm

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_IS_LLT_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'

  return
end
subroutine r8mat_is_null_left_test ( )

!*****************************************************************************80
!
!! r8mat_is_null_left_test() tests R8MAT_IS_NULL_LEFT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ), dimension ( m, n ) :: a = reshape ( (/ &
    1.0D+00, 4.0D+00, 7.0D+00, &
    2.0D+00, 5.0D+00, 8.0D+00, &
    3.0D+00, 6.0D+00, 9.0D+00 /), (/ m, n /) )
  real ( kind = 8 ) enorm
  real ( kind = 8 ), dimension ( m ) :: x = (/ 1.0D+00, -2.0D+00, 1.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8mat_is_null_left_test():'
  write ( *, '(a)' ) '  R8MAT_IS_NULL_LEFT tests whether the M vector X'
  write ( *, '(a)' ) '  is a left null vector of A, that is, x''*A=0.'

  call r8mat_print ( m, n, a, '  Matrix A:' )
  call r8vec_print ( m, x, '  Vector X:' )

  call r8mat_is_null_left ( m, n, a, x, enorm )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Frobenius norm of X''*A is ', enorm

  return
end
subroutine r8mat_is_null_right_test ( )

!*****************************************************************************80
!
!! r8mat_is_null_right_test() tests R8MAT_IS_NULL_RIGHT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ), dimension ( m, n ) :: a = reshape ( (/ &
    1.0D+00, 4.0D+00, 7.0D+00, &
    2.0D+00, 5.0D+00, 8.0D+00, &
    3.0D+00, 6.0D+00, 9.0D+00 /), (/ m, n /) )
  real ( kind = 8 ) enorm
  real ( kind = 8 ), dimension ( n ) :: x = (/ 1.0D+00, -2.0D+00, 1.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8mat_is_null_right_test():'
  write ( *, '(a)' ) '  R8MAT_IS_NULL_RIGHT tests whether the N vector X'
  write ( *, '(a)' ) '  is a right null vector of A, that is, A*X=0.'

  call r8mat_print ( m, n, a, '  Matrix A:' )
  call r8vec_print ( n, x, '  Vector X:' )

  call r8mat_is_null_right ( m, n, a, x, enorm )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Frobenius norm of A*X is ', enorm

  return
end
subroutine r8mat_is_solution_test ( )

!*****************************************************************************80
!
!! r8mat_is_solution_test() tests R8MAT_IS_SOLUTION.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:,:)
  real ( kind = 8 ) enorm
  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8mat_is_solution_test():'
  write ( *, '(a)' ) '  R8MAT_IS_SOLUTION tests whether X is the solution of'
  write ( *, '(a)' ) '  A*X=B by computing the Frobenius norm of the residual.'
!
!  Get random shapes.
!
  i4_lo = 1
  i4_hi = 10
  seed = 123456789
  m = i4_uniform_ab ( i4_lo, i4_hi, seed )
  n = i4_uniform_ab ( i4_lo, i4_hi, seed )
  k = i4_uniform_ab ( i4_lo, i4_hi, seed )
!
!  Get a random A.
!
  allocate ( a(1:m,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  call r8mat_uniform_ab ( m, n, r8_lo, r8_hi, seed, a )
!
!  Get a random X.
!
  allocate ( x(1:n,1:k) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  call r8mat_uniform_ab ( n, k, r8_lo, r8_hi, seed, x )
!
!  Compute B = A * X.
!
  allocate ( b(1:m,1:k) )
  b = matmul ( a, x )
!
!  Compute || A*X-B||
!
  call r8mat_is_solution ( m, n, k, a, x, b, enorm )
  
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2)' ) '  A is ', m, ' by ', n
  write ( *, '(a,i2,a,i2)' ) '  X is ', n, ' by ', k
  write ( *, '(a,i2,a,i2)' ) '  B is ', m, ' by ', k
  write ( *, '(a,g14.6)' ) '  Frobenius error in A*X-B is ', enorm
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( b )
  deallocate ( x )

  return
end
subroutine r8mat_norm_fro_test ( )

!*****************************************************************************80
!
!! r8mat_norm_fro_test() tests R8MAT_NORM_FRO.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8mat_norm_fro
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8mat_norm_fro_test():'
  write ( *, '(a)' ) '  R8MAT_NORM_FRO computes a Frobenius norm of an R8MAT.'

  t1 = 0.0D+00
  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
      t1 = t1 + real ( k * k, kind = 8 )
    end do
  end do

  t1 = sqrt ( t1 )

  call r8mat_print ( m, n, a, '  A:' )

  t2 = r8mat_norm_fro ( m, n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Expected norm = ', t1
  write ( *, '(a,g14.6)' ) '  Computed norm = ', t2

  return
end
subroutine test_analyze ( )

!*****************************************************************************80
!
!! test_analyze() tests the matrix analysis functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character ( len = 20 ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_analyze():'
  write ( *, '(a)' ) '  Analyze a matrix.'
  write ( *, '(a)' ) ' '
!
!  a123
!
  title = 'a123'
  m = 3
  n = 3
  allocate ( a(1:m,1:n) )
  call a123_matrix ( a )
  write ( *, '(a)' ) ''
  write ( *, '(2x,a)' ) trim ( title )
  write ( *, '(a)' ) ''
  call r8mat_analyze ( m, n, a )
  deallocate ( a )

  return
end
subroutine test_condition ( )

!*****************************************************************************80
!
!! test_condition(): tests the condition number computations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) a_norm
  real ( kind = 8 ) alpha
  real ( kind = 8 ), allocatable :: b(:,:)
  real ( kind = 8 ) b_norm
  real ( kind = 8 ) beta
  real ( kind = 8 ) cond1
  real ( kind = 8 ) cond2
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) r8mat_norm_l1
  integer ( kind = 4 ) seed
  character ( len = 20 ) title
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_condition():'
  write ( *, '(a)' ) '  Compute the L1 condition number of an example of each'
  write ( *, '(a)' ) '  test matrix'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Title                    N      COND            COND'
  write ( *, '(a)' ) ' '
!
!  aegerter
!
  title = 'aegerter'
  n = 5
  call aegerter_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call aegerter_matrix ( n, a )
  call aegerter_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  bab
!
  title = 'bab'
  n = 5
  seed = 123456789
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call bab_condition ( n, alpha, beta, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call bab_matrix ( n, alpha, beta, a )
  call bab_inverse ( n, alpha, beta, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  bauer
!
  title = 'bauer'
  n = 6
  call bauer_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call bauer_matrix ( a )
  call bauer_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  BIS
!
  title = 'BIS'
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call bis_condition ( alpha, beta, n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call bis_matrix ( alpha, beta, n, n, a )
  call bis_inverse ( alpha, beta, n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  BIW
!
  title = 'BIW'
  n = 5
  call biw_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call biw_matrix ( n, a )
  call biw_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  bodewig
!
  title = 'bodewig'
  n = 4
  call bodewig_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call bodewig_matrix ( a )
  call bodewig_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  boothroyd
!
  title = 'boothroyd'
  n = 5
  call boothroyd_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call boothroyd_matrix ( n, a )
  call boothroyd_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  combin
!
  title = 'combin'
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call combin_condition ( alpha, beta, n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call combin_matrix ( alpha, beta, n, a )
  call combin_inverse ( alpha, beta, n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  companion
!
  title = 'companion'
  n = 5
  seed = 123456789
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call companion_condition ( n, x, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call companion_matrix ( n, x, a )
  call companion_inverse ( n, x, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
  deallocate ( x )
!
!  conex1
!
  title = 'conex1'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call conex1_condition ( alpha, cond1 )
  call conex1_matrix ( alpha, a )
  call conex1_inverse ( alpha, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  conex2
!
  title = 'conex2'
  n = 3
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call conex2_condition ( alpha, cond1 )

  call conex2_matrix ( alpha, a )
  call conex2_inverse ( alpha, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  conex3
!
  title = 'conex3'
  n = 5
  call conex3_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call conex3_matrix ( n, a )
  call conex3_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  conex4
!
  title = 'conex4'
  n = 4
  call conex4_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call conex4_matrix ( a )
  call conex4_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  daub2
!
  title = 'daub2'
  n = 4
  call daub2_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call daub2_matrix ( n, a )
  call daub2_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  daub4
!
  title = 'daub4'
  n = 8
  call daub4_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call daub4_matrix ( n, a )
  call daub4_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  daub6
!
  title = 'daub6'
  n = 12
  call daub6_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call daub6_matrix ( n, a )
  call daub6_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  daub8
!
  title = 'daub8'
  n = 16
  call daub8_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call daub8_matrix ( n, a )
  call daub8_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  daub10
!
  title = 'daub10'
  n = 20
  call daub10_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call daub10_matrix ( n, a )
  call daub10_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  daub12
!
  title = 'daub12'
  n = 24
  call daub12_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call daub12_matrix ( n, a )
  call daub12_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  DIAGONAL
!
  title = 'DIAGONAL'
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  allocate ( x(1:n) )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call diagonal_condition ( n, x, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call diagonal_matrix ( n, n, x, a )
  call diagonal_inverse ( n, x, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
  deallocate ( x )
!
!  dif2
!
  title = 'dif2'
  n = 5
  call dif2_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call dif2_matrix ( n, n, a )
  call dif2_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  DOWNSHIFT
!
  title = 'DOWNSHIFT'
  n = 5
  call downshift_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call downshift_matrix ( n, a )
  call downshift_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  exchange
!
  title = 'exchange'
  n = 5
  call exchange_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call exchange_matrix ( n, n, a )
  call exchange_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  FIBONACCI2
!
  title = 'FIBONACCI2'
  n = 5
  call fibonacci2_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call fibonacci2_matrix ( n, a )
  call fibonacci2_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  GFPP
!
  title = 'GFPP'
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call gfpp_condition ( n, alpha, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call gfpp_matrix ( n, alpha, a )
  call gfpp_inverse ( n, alpha, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  givens
!
  title = 'givens'
  n = 5
  call givens_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call givens_matrix ( n, n, a )
  call givens_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  HANKEL_N
!
  title = 'HANKEL_N'
  n = 5
  call hankel_n_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call hankel_n_matrix ( n, a )
  call hankel_n_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  HARMAN
!
  title = 'HARMAN'
  n = 8
  call harman_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call harman_matrix ( a )
  call harman_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  HARTLEY
!
  title = 'HARTLEY'
  n = 5
  call hartley_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call hartley_matrix ( n, a )
  call hartley_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  identity
!
  title = 'identity'
  n = 5
  call identity_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call identity_matrix ( n, n, a )
  call identity_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  ill3
!
  title = 'ill3'
  n = 3
  call ill3_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call ill3_matrix ( a )
  call ill3_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  JORDAN
!
  title = 'JORDAN'
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call jordan_condition ( n, alpha, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call jordan_matrix ( n, n, alpha, a )
  call jordan_inverse ( n, alpha, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  KERSHAW
!
  title = 'KERSHAW'
  n = 4
  call kershaw_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call kershaw_matrix ( a )
  call kershaw_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  LIETZKE
!
  title = 'LIETZKE'
  n = 5
  call lietzke_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call lietzke_matrix ( n, a )
  call lietzke_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  MAXIJ
!
  title = 'MAXIJ'
  n = 5
  call maxij_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call maxij_matrix ( n, n, a )
  call maxij_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  MINIJ
!
  title = 'MINIJ'
  n = 5
  call minij_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call minij_matrix ( n, n, a )
  call minij_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  orthogonal_symmetric
!
  title = 'orthogonal_symmetric'
  n = 5
  call orthogonal_symmetric_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call orthogonal_symmetric_matrix ( n, a )
  call orthogonal_symmetric_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  OTO
!
  title = 'OTO'
  n = 5
  call oto_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call oto_matrix ( n, n, a )
  call oto_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  PASCAL1
!
  title = 'PASCAL1'
  n = 5
  call pascal1_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call pascal1_matrix ( n, a )
  call pascal1_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  PASCAL3
!
  title = 'PASCAL3'
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call pascal3_condition ( n, alpha, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call pascal3_matrix ( n, alpha, a )
  call pascal3_inverse ( n, alpha, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  PEI
!
  title = 'PEI'
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call pei_condition ( alpha, n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call pei_matrix ( alpha, n, a )
  call pei_inverse ( alpha, n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  rodman
!
  title = 'rodman'
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call rodman_condition ( n, alpha, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call rodman_matrix ( n, n, alpha, a )
  call rodman_inverse ( n, alpha, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  rutis1
!
  title = 'rutis1'
  n = 4
  call rutis1_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call rutis1_matrix ( a )
  call rutis1_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  rutis2
!
  title = 'rutis2'
  n = 4
  call rutis2_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call rutis2_matrix ( a )
  call rutis2_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  rutis3
!
  title = 'rutis3'
  n = 4
  call rutis3_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call rutis3_matrix ( a )
  call rutis3_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  rutis5
!
  title = 'rutis5'
  n = 4
  call rutis5_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call rutis5_matrix ( a )
  call rutis5_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  summation
!
  title = 'summation'
  n = 5
  call summation_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call summation_matrix ( n, n, a )
  call summation_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  sweet1
!
  title = 'sweet1'
  n = 6
  call sweet1_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call sweet1_matrix ( a )
  call sweet1_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  sweet2
!
  title = 'sweet2'
  n = 6
  call sweet2_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call sweet2_matrix ( a )
  call sweet2_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  sweet3
!
  title = 'sweet3'
  n = 6
  call sweet3_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call sweet3_matrix ( a )
  call sweet3_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  sweet4
!
  title = 'sweet4'
  n = 13
  call sweet4_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call sweet4_matrix ( a )
  call sweet4_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  tri_upper
!
  title = 'tri_upper'
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call tri_upper_condition ( alpha, n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call tri_upper_matrix ( alpha, n, a )
  call tri_upper_inverse ( alpha, n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  upshift
!
  title = 'upshift'
  n = 5
  call upshift_condition ( n, cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call upshift_matrix ( n, a )
  call upshift_inverse ( n, b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  wilk03
!
  title = 'wilk03'
  n = 3
  call wilk03_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call wilk03_matrix ( a )
  call wilk03_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  wilk04
!
  title = 'wilk04'
  n = 4
  call wilk04_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call wilk04_matrix ( a )
  call wilk04_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  wilk05
!
  title = 'wilk05'
  n = 5
  call wilk05_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call wilk05_matrix ( a )
  call wilk05_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )
!
!  wilson
!
  title = 'wilson'
  n = 4
  call wilson_condition ( cond1 )

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  call wilson_matrix ( a )
  call wilson_inverse ( b )
  a_norm = r8mat_norm_l1 ( n, n, a )
  b_norm = r8mat_norm_l1 ( n, n, b )
  cond2 = a_norm * b_norm

  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) title, n, cond1, cond2
  deallocate ( a )
  deallocate ( b )

  return
end
subroutine test_determinant ( )

!*****************************************************************************80
!
!! test_determinant() tests the determinant computations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  integer ( kind = 4 ) col_num
  real ( kind = 8 ), allocatable, dimension ( : ) :: d
  real ( kind = 8 ) d1
  real ( kind = 8 ) d2
  real ( kind = 8 ) d3
  real ( kind = 8 ) d4
  real ( kind = 8 ) d5
  real ( kind = 8 ) da
  real ( kind = 8 ) determ1
  real ( kind = 8 ) determ2
  real ( kind = 8 ) di
  real ( kind = 8 ) gamma
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  integer ( kind = 4 ) key
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) norm_frobenius
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: p
  integer ( kind = 4 ), allocatable, dimension ( : ) :: pivot
  real ( kind = 8 ) prob
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) r8mat_norm_fro
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) row_num
  integer ( kind = 4 ) seed
  character ( len = 20 ) title
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: u
  real ( kind = 8 ), allocatable, dimension ( : ) :: v1
  real ( kind = 8 ), allocatable, dimension ( : ) :: v2
  real ( kind = 8 ), allocatable, dimension ( : ) :: v3
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo
  integer ( kind = 4 ) x_n
  real ( kind = 8 ), allocatable, dimension ( : ) :: y
  integer ( kind = 4 ) y_n
  real ( kind = 8 ) y_sum
  real ( kind = 8 ), allocatable, dimension ( : ) :: z

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_determinant():'
  write ( *, '(a)' ) '  Compute the determinants of an example of each'
  write ( *, '(a)' ) '  test matrix.  Compare with the determinant routine,'
  write ( *, '(a)' ) '  if available.  Print the matrix Frobenius norm'
  write ( *, '(a)' ) '  for an estimate of magnitude.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Title                    N      ' // &
                     'Determ          Determ         ||A||'
  write ( *, '(a)' ) ' '
!
!  a123
!
  title = 'a123'
  n = 3
  allocate ( a(1:n,1:n) )
  call a123_matrix ( a )
  call a123_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  aegerter
!
  title = 'aegerter'
  n = 5
  allocate ( a(1:n,1:n) )
  call aegerter_matrix ( n, a )
  call aegerter_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  ANTIcirculant
!
  title = 'ANTIcirculant'
  n = 3
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call anticirculant_matrix ( n, n, x, a )
  call anticirculant_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  ANTIcirculant
!
  title = 'ANTIcirculant'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call anticirculant_matrix ( n, n, x, a )
  call anticirculant_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  ANTIcirculant
!
  title = 'ANTIcirculant'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call anticirculant_matrix ( n, n, x, a )
  call anticirculant_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  ANTIHADAMARD
!
  title = 'ANTIHADAMARD'
  n = 5
  allocate ( a(1:n,1:n) )
  call antihadamard_matrix ( n, a )
  call antihadamard_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  ANTIsymmetric_random
!
  title = 'ANTIsymmetric_random'
  n = 5
  allocate ( a(1:n,1:n) )
  key = 123456789
  call antisymmetric_random_matrix ( n, key, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  ANTIsymmetric_random
!
  title = 'ANTIsymmetric_random'
  n = 6
  allocate ( a(1:n,1:n) )
  key = 123456789
  call antisymmetric_random_matrix ( n, key, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  bab
!
  title = 'bab'
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  allocate ( a(1:n,1:n) )
  call bab_matrix ( n, alpha, beta, a )
  call bab_determinant ( n, alpha, beta, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  bauer
!
  title = 'bauer'
  n = 6
  allocate ( a(1:n,1:n) )
  call bauer_matrix ( a )
  call bauer_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  bernstein
!
  title = 'bernstein'
  n = 5
  allocate ( a(1:n,1:n) )
  call bernstein_matrix ( n, a )
  call bernstein_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  BIMARKOV_random
!
  title = 'BIMARKOV_random'
  n = 5
  allocate ( a(1:n,1:n) )
  key = 123456789
  call bimarkov_random_matrix ( n, key, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  BIS
!
  title = 'BIS'
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  allocate ( a(1:n,1:n) )
  call bis_matrix ( alpha, beta, n, n, a )
  call bis_determinant ( alpha, beta, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  BIW
!
  title = 'BIW'
  n = 5
  allocate ( a(1:n,1:n) )
  call biw_matrix ( n, a )
  call biw_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  bodewig
!
  title = 'bodewig'
  n = 4
  allocate ( a(1:n,1:n) )
  call bodewig_matrix ( a )
  call bodewig_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  boothroyd
!
  title = 'boothroyd'
  n = 5
  allocate ( a(1:n,1:n) )
  call boothroyd_matrix ( n, a )
  call boothroyd_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  borderband
!
  title = 'borderband'
  n = 5
  allocate ( a(1:n,1:n) )
  call borderband_matrix ( n, a )
  call borderband_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  carry
!
  title = 'carry'
  n = 5
  allocate ( a(1:n,1:n) )
  seed = 123456789
  i4_lo = 2
  i4_hi = 20
  k = i4_uniform_ab ( i4_lo, i4_hi, seed )
  call carry_matrix ( n, k, a )
  call carry_determinant ( n, k, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  CAUCHY
!
  title = 'CAUCHY'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  allocate ( y(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
  call cauchy_matrix ( n, x, y, a )
  call cauchy_determinant ( n, x, y, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )
!
!  cheby_diff1
!
  title = 'cheby_diff1'
  n = 5
  allocate ( a(1:n,1:n) )
  call cheby_diff1_matrix ( n, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  cheby_diff1
!
  title = 'cheby_diff1'
  n = 6
  allocate ( a(1:n,1:n) )
  call cheby_diff1_matrix ( n, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  cheby_T
!
  title = 'cheby_T'
  n = 5
  allocate ( a(1:n,1:n) )
  call cheby_t_matrix ( n, a )
  call cheby_t_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  cheby_U
!
  title = 'cheby_U'
  n = 5
  allocate ( a(1:n,1:n) )
  call cheby_u_matrix ( n, a )
  call cheby_u_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  cheby_van1
!
  title = 'cheby_van1'
  n = 5
  x_lo = -1.0D+00
  x_hi = +1.0D+00
  allocate ( x(1:n) )
  call r8vec_linspace ( n, x_lo, x_hi, x )
  allocate ( a(1:n,1:n) )
  call cheby_van1_matrix ( n, x_lo, x_hi, n, x, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  cheby_van2
!
  do n = 2, 10
    title = 'cheby_van2'
    allocate ( a(1:n,1:n) )
    call cheby_van2_matrix ( n, a )
    call cheby_van2_determinant ( n, determ1 )
    call r8mat_determinant ( n, a, determ2 )
    norm_frobenius = r8mat_norm_fro ( n, n, a )
    write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      title, n, determ1, determ2, norm_frobenius
    deallocate ( a )
  end do
!
!  cheby_van3
!
  title = 'cheby_van3'
  n = 5
  allocate ( a(1:n,1:n) )
  call cheby_van3_matrix ( n, a )
  call cheby_van3_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  chow
!
  title = 'chow'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call chow_matrix ( alpha, beta, n, n, a )
  call chow_determinant ( alpha, beta, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  circulant
!
  title = 'circulant'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call circulant_matrix ( n, n, x, a )
  call circulant_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  circulant2
!
  title = 'circulant2'
  n = 3
  allocate ( a(1:n,1:n) )
  call circulant2_matrix ( n, a )
  call circulant2_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  circulant2
!
  title = 'circulant2'
  n = 4
  allocate ( a(1:n,1:n) )
  call circulant2_matrix ( n, a )
  call circulant2_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  circulant2
!
  title = 'circulant2'
  n = 5
  allocate ( a(1:n,1:n) )
  call circulant2_matrix ( n, a )
  call circulant2_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  CLEMENT1
!
  title = 'CLEMENT1'
  n = 5
  allocate ( a(1:n,1:n) )
  call clement1_matrix ( n, a )
  call clement1_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  CLEMENT1
!
  title = 'CLEMENT1'
  n = 6
  allocate ( a(1:n,1:n) )
  call clement1_matrix ( n, a )
  call clement1_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  CLEMENT2
!
  title = 'CLEMENT2'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n-1) )
  allocate ( y(1:n-1) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, y )
  call clement2_matrix ( n, x, y, a )
  call clement2_determinant ( n, x, y, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )
!
!  CLEMENT2
!
  title = 'CLEMENT2'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n-1) )
  allocate ( y(1:n-1) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, y )
  call clement2_matrix ( n, x, y, a )
  call clement2_determinant ( n, x, y, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )
!
!  combin
!
  title = 'combin'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call combin_matrix ( alpha, beta, n, a )
  call combin_determinant ( alpha, beta, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  companion
!
  title = 'companion'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call companion_matrix ( n, x, a )
  call companion_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  COMPLEX_I
!
  title = 'COMPLEX_I'
  n = 2
  allocate ( a(1:n,1:n) )
  call complex_i_matrix ( a )
  call complex_i_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  conex1
!
  title = 'conex1'
  n = 4
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call conex1_matrix ( alpha, a )
  call conex1_determinant ( alpha, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  conex2
!
  title = 'conex2'
  n = 3
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call conex2_matrix ( alpha, a )
  call conex2_determinant ( alpha, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  conex3
!
  title = 'conex3'
  n = 5
  allocate ( a(1:n,1:n) )
  call conex3_matrix ( n, a )
  call conex3_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  conex4
!
  title = 'conex4'
  n = 4
  allocate ( a(1:n,1:n) )
  call conex4_matrix ( a )
  call conex4_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  CONFERENCE
!
  title = 'CONFERENCE'
  n = 6
  allocate ( a(1:n,1:n) )
  call conference_matrix ( n, a )
  call conference_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  CREATION
!
  title = 'CREATION'
  n = 5
  allocate ( a(1:n,1:n) )
  call creation_matrix ( n, n, a )
  call creation_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  daub2
!
  title = 'daub2'
  n = 4
  allocate ( a(1:n,1:n) )
  call daub2_matrix ( n, a )
  call daub2_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  daub4
!
  title = 'daub4'
  n = 8
  allocate ( a(1:n,1:n) )
  call daub4_matrix ( n, a )
  call daub4_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  daub6
!
  title = 'daub6'
  n = 12
  allocate ( a(1:n,1:n) )
  call daub6_matrix ( n, a )
  call daub6_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  daub8
!
  title = 'daub8'
  n = 16
  allocate ( a(1:n,1:n) )
  call daub8_matrix ( n, a )
  call daub8_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  daub10
!
  title = 'daub10'
  n = 20
  allocate ( a(1:n,1:n) )
  call daub10_matrix ( n, a )
  call daub10_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  daub12
!
  title = 'daub12'
  n = 24
  allocate ( a(1:n,1:n) )
  call daub12_matrix ( n, a )
  call daub12_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  DIAGONAL
!
  title = 'DIAGONAL'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call diagonal_matrix ( n, n, x, a )
  call diagonal_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  dif1
!
  title = 'dif1'
  n = 5
  allocate ( a(1:n,1:n) )
  call dif1_matrix ( n, n, a )
  call dif1_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  dif1
!
  title = 'dif1'
  n = 6
  allocate ( a(1:n,1:n) )
  call dif1_matrix ( n, n, a )
  call dif1_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  dif1CYCLIC
!
  title = 'dif1CYCLIC'
  n = 5
  allocate ( a(1:n,1:n) )
  call dif1cyclic_matrix ( n, a )
  call dif1cyclic_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  dif2
!
  title = 'dif2'
  n = 5
  allocate ( a(1:n,1:n) )
  call dif2_matrix ( n, n, a )
  call dif2_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  dif2CYCLIC
!
  title = 'dif2CYCLIC'
  n = 5
  allocate ( a(1:n,1:n) )
  call dif2cyclic_matrix ( n, a )
  call dif2cyclic_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  DORR
!
  title = 'DORR'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call dorr_matrix ( alpha, n, a )
  call dorr_determinant ( alpha, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  DOWNSHIFT
!
  title = 'DOWNSHIFT'
  n = 5
  allocate ( a(1:n,1:n) )
  call downshift_matrix ( n, a )
  call downshift_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  EBERLEIN
!
  title = 'EBERLEIN'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call eberlein_matrix ( alpha, n, a )
  call eberlein_determinant ( alpha, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  EULERIAN
!
  title = 'EULERIAN'
  n = 5
  allocate ( a(1:n,1:n) )
  call eulerian_matrix ( n, n, a )
  call eulerian_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  exchange
!
  title = 'exchange'
  n = 5
  allocate ( a(1:n,1:n) )
  call exchange_matrix ( n, n, a )
  call exchange_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  FIBONACCI1
!
  title = 'FIBONACCI1'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call fibonacci1_matrix ( n, alpha, beta, a )
  call fibonacci1_determinant ( n, alpha, beta, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  fibonacci2
!
  title = 'fibonacci2'
  n = 5
  allocate ( a(1:n,1:n) )
  call fibonacci2_matrix ( n, a )
  call fibonacci2_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  FIBONACCI3
!
  title = 'FIBONACCI3'
  n = 5
  allocate ( a(1:n,1:n) )
  call fibonacci3_matrix ( n, a )
  call fibonacci3_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  FIEDLER.
!
  title = 'FIEDLER'
  n = 7
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call fiedler_matrix ( n, n, x, a )
  call fiedler_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  FORSYTHE
!
  title = 'FORSYTHE'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call forsythe_matrix ( alpha, beta, n, a )
  call forsythe_determinant ( alpha, beta, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  FORSYTHE
!
  title = 'FORSYTHE'
  n = 6
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call forsythe_matrix ( alpha, beta, n, a )
  call forsythe_determinant ( alpha, beta, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  FOURIER_COSINE
!
  title = 'FOURIER_COSINE'
  n = 5
  allocate ( a(1:n,1:n) )
  call fourier_cosine_matrix ( n, a )
  call fourier_cosine_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  FOURIER_SINE
!
  title = 'FOURIER_SINE'
  n = 5
  allocate ( a(1:n,1:n) )
  call fourier_sine_matrix ( n, a )
  call fourier_sine_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  frank
!
  title = 'frank'
  n = 5
  allocate ( a(1:n,1:n) )
  call frank_matrix ( n, a )
  call frank_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  GFPP
!
  title = 'GFPP'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call gfpp_matrix ( n, alpha, a )
  call gfpp_determinant ( n, alpha, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  givens.
!
  title = 'givens'
  n = 5
  allocate ( a(1:n,1:n) )
  call givens_matrix ( n, n, a )
  call givens_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  GK316
!
  title = 'GK316'
  n = 5
  allocate ( a(1:n,1:n) )
  call gk316_matrix ( n, a )
  call gk316_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  GK323
!
  title = 'GK323'
  n = 5
  allocate ( a(1:n,1:n) )
  call gk323_matrix ( n, n, a )
  call gk323_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  GK324
!
  title = 'GK324'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
  call gk324_matrix ( n, n, x, a )
  call gk324_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  GRCAR
!
  title = 'GRCAR'
  n = 5
  seed = 123456789
  allocate ( a(1:n,1:n) )
  i4_lo = 1
  i4_hi = n - 1
  k = i4_uniform_ab ( i4_lo, i4_hi, seed )
  call grcar_matrix ( n, n, k, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  HADAMARD
!
  title = 'HADAMARD'
  n = 5
  allocate ( a(1:n,1:n) )
  call hadamard_matrix ( n, n, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  HANKEL
!
  title = 'HANKEL'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:2*n-1) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( 2 * n - 1, r8_lo, r8_hi, seed, x )
  call hankel_matrix ( n, x, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  HANKEL_N
!
  title = 'HANKEL_N'
  n = 5
  allocate ( a(1:n,1:n) )
  call hankel_n_matrix ( n, a )
  call hankel_n_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  HANOWA
!
  title = 'HANOWA'
  n = 6
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call hanowa_matrix ( alpha, n, a )
  call hanowa_determinant ( alpha, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  HARMAN
!
  title = 'HARMAN'
  n = 8
  allocate ( a(1:n,1:n) )
  call harman_matrix ( a )
  call harman_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  HARTLEY
!
  title = 'HARTLEY'
  do n = 5, 8
    allocate ( a(1:n,1:n) )
    call hartley_matrix ( n, a )
    call hartley_determinant ( n, determ1 )
    call r8mat_determinant ( n, a, determ2 )
    norm_frobenius = r8mat_norm_fro ( n, n, a )
    write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      title, n, determ1, determ2, norm_frobenius
    deallocate ( a )
  end do
!
!  HELMERT
!
  title = 'HELMERT'
  n = 5
  allocate ( a(1:n,1:n) )
  call helmert_matrix ( n, a )
  call helmert_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  HELMERT2
!
  title = 'HELMERT2'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  x(1:n) = anint ( 50.0D+00 * x(1:n) - 25.0D+00 ) / 5.0D+00
  call helmert2_matrix ( n, x, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  HERMITE
!
  title = 'HERMITE'
  n = 5
  allocate ( a(1:n,1:n) )
  call hermite_matrix ( n, a )
  call hermite_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  HERNDON
!
  title = 'HERNDON'
  n = 5
  allocate ( a(1:n,1:n) )
  call herndon_matrix ( n, a )
  call herndon_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  HILBERT
!
  title = 'HILBERT'
  n = 5
  allocate ( a(1:n,1:n) )
  call hilbert_matrix ( n, n, a )
  call hilbert_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  HOUSEHOLDER
!
  title = 'HOUSEHOLDER'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call householder_matrix ( n, x, a )
  call householder_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  idempotent_random
!
  title = 'idempotent_random'
  n = 5
  allocate ( a(1:n,1:n) )
  seed = 123456789
  i4_lo = 0
  i4_hi = n
  rank = i4_uniform_ab ( i4_lo, i4_hi, seed )
  key = 123456789
  call idempotent_random_matrix ( n, rank, key, a )
  call idempotent_random_determinant ( n, rank, key, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  identity
!
  title = 'identity'
  n = 5
  allocate ( a(1:n,1:n) )
  call identity_matrix ( n, n, a )
  call identity_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  IJFACT1
!
  title = 'IJFACT1'
  n = 5
  allocate ( a(1:n,1:n) )
  call ijfact1_matrix ( n, a )
  call ijfact1_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  IJFACT2
!
  title = 'IJFACT2'
  n = 5
  allocate ( a(1:n,1:n) )
  call ijfact2_matrix ( n, a )
  call ijfact2_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  ill3
!
  title = 'ill3'
  n = 3
  allocate ( a(1:n,1:n) )
  call ill3_matrix ( a )
  call ill3_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  INTEGRATION
!
  title = 'INTEGRATION'
  n = 6
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call integration_matrix ( alpha, n, a )
  call integration_determinant ( alpha, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  involutory 
!
  title = 'involutory '
  n = 5
  allocate ( a(1:n,1:n) )
  call involutory_matrix ( n, a )
  call involutory_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  involutory_random
!
  title = 'involutory_random'
  n = 5
  allocate ( a(1:n,1:n) )
  i4_lo = 0
  i4_hi = n
  rank = i4_uniform_ab ( i4_lo, i4_hi, seed )
  key = 123456789
  call involutory_random_matrix ( n, rank, key, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  JACOBI
!
  title = 'JACOBI'
  n = 5
  allocate ( a(1:n,1:n) )
  call jacobi_matrix ( n, n, a )
  call jacobi_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  JACOBI
!
  title = 'JACOBI'
  n = 6
  allocate ( a(1:n,1:n) )
  call jacobi_matrix ( n, n, a )
  call jacobi_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  JORDAN
!
  title = 'JORDAN'
  n = 6
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call jordan_matrix ( n, n, alpha, a )
  call jordan_determinant ( n, alpha, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  kahan
!
  title = 'kahan'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call kahan_matrix ( alpha, n, n, a )
  call kahan_determinant ( alpha, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  KERSHAW
!
  title = 'KERSHAW'
  n = 4
  allocate ( a(1:n,1:n) )
  call kershaw_matrix ( a )
  call kershaw_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  KERSHAWTRI
!
  title = 'KERSHAWTRI'
  n = 5
  x_n = ( n + 1 ) / 2
  allocate ( a(1:n,1:n) )
  allocate ( x(1:x_n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( x_n, r8_lo, r8_hi, seed, x )
  call kershawtri_matrix ( n, x, a )
  call kershawtri_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  kms
!
  title = 'kms'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call kms_matrix ( alpha, n, n, a )
  call kms_determinant ( alpha, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  LAGUERRE
!
  title = 'LAGUERRE'
  n = 5
  allocate ( a(1:n,1:n) )
  call laguerre_matrix ( n, a )
  call laguerre_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  LEGENDRE
!
  title = 'LEGENDRE'
  n = 5
  allocate ( a(1:n,1:n) )
  call legendre_matrix ( n, a )
  call legendre_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  LEHMER
!
  title = 'LEHMER'
  n = 5
  allocate ( a(1:n,1:n) )
  call lehmer_matrix ( n, n, a )
  call lehmer_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  LESLIE
!
  title = 'LESLIE'
  n = 4
  allocate ( a(1:n,1:n) )
  b =  0.025D+00
  di = 0.010D+00
  da = 0.100D+00
  call leslie_matrix ( b, di, da, a )
  call leslie_determinant ( b, di, da, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  LESP
!
  title = 'LESP'
  n = 5
  allocate ( a(1:n,1:n) )
  call lesp_matrix ( n, n, a )
  call lesp_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  LIETZKE
!
  title = 'LIETZKE'
  n = 5
  allocate ( a(1:n,1:n) )
  call lietzke_matrix ( n, a )
  call lietzke_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  LIGHTS_OUT
!
  title = 'LIGHTS_OUT'
  row_num = 5
  col_num = 5
  n = row_num * col_num
  allocate ( a(1:row_num*col_num,1:row_num*col_num) )
  call lights_out_matrix ( row_num, col_num, n, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  line_adj
!
  title = 'line_adj'
  n = 5
  allocate ( a(1:n,1:n) )
  call line_adj_matrix ( n, a )
  call line_adj_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  line_adj
!
  title = 'line_adj'
  n = 6
  allocate ( a(1:n,1:n) )
  call line_adj_matrix ( n, a )
  call line_adj_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  LINE_LOOP_ADJ
!
  title = 'LINE_LOOP_ADJ'
  n = 5
  allocate ( a(1:n,1:n) )
  call line_loop_adj_matrix ( n, a )
  call line_loop_adj_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  LOEWNER
!
  title = 'LOEWNER'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( w(1:n) )
  allocate ( x(1:n) )
  allocate ( y(1:n) )
  allocate ( z(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, w )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, z )
  call loewner_matrix ( w, x, y, z, n, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
  deallocate ( w )
  deallocate ( x )
  deallocate ( y )
  deallocate ( z )
!
!  LOTKIN
!
  title = 'LOTKIN'
  n = 5
  allocate ( a(1:n,1:n) )
  call lotkin_matrix ( n, n, a )
  call lotkin_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  MARKOV_random
!
  title = 'MARKOV_random'
  n = 5
  allocate ( a(1:n,1:n) )
  key = 123456789
  call markov_random_matrix ( n, key, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  MAXIJ
!
  title = 'MAXIJ'
  n = 5
  allocate ( a(1:n,1:n) )
  call maxij_matrix ( n, n, a )
  call maxij_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  MILNES
!
  title = 'MILNES'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call milnes_matrix ( n, n, x, a )
  call milnes_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  MINIJ
!
  title = 'MINIJ'
  n = 5
  allocate ( a(1:n,1:n) )
  call minij_matrix ( n, n, a )
  call minij_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  MOLER1
!
  title = 'MOLER1'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call moler1_matrix ( alpha, n, n, a )
  call moler1_determinant ( alpha, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  MOLER2
!
  title = 'MOLER2'
  n = 5
  allocate ( a(1:n,1:n) )
  call moler2_matrix ( a )
  call moler2_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  MOLER3
!
  title = 'MOLER3'
  n = 5
  allocate ( a(1:n,1:n) )
  call moler3_matrix ( n, n, a )
  call moler3_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  MOLER4
!
  title = 'MOLER4'
  n = 4
  allocate ( a(1:n,1:n) )
  call moler4_matrix ( a )
  call moler4_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  NEUMANN
!
  title = 'NEUMANN'
  row_num = 5
  col_num = 5
  n = row_num * col_num
  allocate ( a(1:n,1:n) )
  call neumann_matrix ( row_num, col_num, a )
  call neumann_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  ONE
!
  title = 'ONE'
  n = 5
  allocate ( a(1:n,1:n) )
  call one_matrix ( n, n, a )
  call one_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  ORTEGA
!
  title = 'ORTEGA'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( v1(1:n) )
  allocate ( v2(1:n) )
  allocate ( v3(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v1 )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v2 )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v3 )
  call ortega_matrix ( n, v1, v2, v3, a )
  call ortega_determinant ( n, v1, v2, v3, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( v1 )
  deallocate ( v2 )
  deallocate ( v3 )
!
!  orthogonal_random
!
  title = 'orthogonal_random'
  n = 5
  allocate ( a(1:n,1:n) )
  key = 123456789
  call orthogonal_random_matrix ( n, key, a )
  call orthogonal_random_determinant ( n, key, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  orthogonal_symmetric
!
  title = 'orthogonal_symmetric'
  n = 5
  allocate ( a(1:n,1:n) )
  call orthogonal_symmetric_matrix ( n, a )
  call orthogonal_symmetric_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  OTO
!
  title = 'OTO'
  n = 5
  allocate ( a(1:n,1:n) )
  call oto_matrix ( n, n, a )
  call oto_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  PARTER
!
  title = 'PARTER'
  n = 5
  allocate ( a(1:n,1:n) )
  call parter_matrix ( n, n, a )
  call parter_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  PASCAL1
!
  title = 'PASCAL1'
  n = 5
  allocate ( a(1:n,1:n) )
  call pascal1_matrix ( n, a )
  call pascal1_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  PASCAL2
!
  title = 'PASCAL2'
  n = 5
  allocate ( a(1:n,1:n) )
  call pascal2_matrix ( n, a )
  call pascal2_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  PASCAL3
!
  title = 'PASCAL3'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call pascal3_matrix ( n, alpha, a )
  call pascal3_determinant ( n, alpha, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  PEI
!
  title = 'PEI'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call pei_matrix ( alpha, n, a )
  call pei_determinant ( alpha, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  PERMUTATION_random
!
  title = 'PERMUTATION_random'
  n = 5
  allocate ( a(1:n,1:n) )
  key = 123456789
  call permutation_random_matrix ( n, key, a )
  call permutation_random_determinant ( n, key, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  PLU
!
  title = 'PLU'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( l(1:n,1:n) )
  allocate ( p(1:n,1:n) )
  allocate ( pivot(n) )
  allocate ( u(1:n,1:n) )
  seed = 123456789
  do i = 1, n
    i4_lo = i
    i4_hi = n
    pivot(i) = i4_uniform_ab ( i4_lo, i4_hi, seed )
  end do
  call plu_matrix ( n, pivot, a )
  call plu_determinant ( n, pivot, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( pivot )
  deallocate ( u )
!
!  poisson
!
  title = 'poisson'
  row_num = 5
  col_num = 5
  n = row_num * col_num
  allocate ( a(1:n,1:n) )
  call poisson_matrix ( row_num, col_num, a )
  call poisson_determinant ( row_num, col_num, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  PROLATE
!
  title = 'PROLATE'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call prolate_matrix ( alpha, n, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  RECTANGLE_ADJ
!
  title = 'RECTANGLE_ADJ'
  row_num = 5
  col_num = 5
  n = row_num * col_num
  allocate ( a(1:n,1:n) )
  call rectangle_adj_matrix ( row_num, col_num, n, a )
  call rectangle_adj_determinant ( row_num, col_num, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  REDHEFFER
!
  title = 'REDHEFFER'
  n = 5
  allocate ( a(1:n,1:n) )
  call redheffer_matrix ( n, a )
  call redheffer_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  REF_random
!
  title = 'REF_random'
  n = 5
  allocate ( a(1:n,1:n) )
  prob = 0.65D+00
  key = 123456789
  call ref_random_matrix ( n, n, prob, key, a )
  call ref_random_determinant ( n, prob, key, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  REF_random
!
  title = 'REF_random'
  n = 5
  allocate ( a(1:n,1:n) )
  prob = 0.85D+00
  key = 123456789
  call ref_random_matrix ( n, n, prob, key, a )
  call ref_random_determinant ( n, prob, key, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  RIEMANN
!
  title = 'RIEMANN'
  n = 5
  allocate ( a(1:n,1:n) )
  call riemann_matrix ( n, n, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  RING_ADJ
!
  do n = 1, 8
    title = 'RING_ADJ'
    allocate ( a(1:n,1:n) )
    call ring_adj_matrix ( n, a )
    call ring_adj_determinant ( n, determ1 )
    call r8mat_determinant ( n, a, determ2 )
    norm_frobenius = r8mat_norm_fro ( n, n, a )
    write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      title, n, determ1, determ2, norm_frobenius
    deallocate ( a )
  end do
!
!  ris
!
  title = 'ris'
  n = 5
  allocate ( a(1:n,1:n) )
  call ris_matrix ( n, a )
  call ris_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  rodman
!
  title = 'rodman'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call rodman_matrix ( n, n, alpha, a )
  call rodman_determinant ( n, alpha, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  rosser1
!
!  Note that while the correct determinant of this matrix is 0,
!  that value is very difficult to calculate correctly.  MATLAB
!  returns det ( A ) = -10611, for instance.
!
  title = 'rosser1'
  n = 8
  allocate ( a(1:n,1:n) )
  call rosser1_matrix ( a )
  call rosser1_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  routh
!
  title = 'routh'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n ) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call routh_matrix ( n, x, a )
  call routh_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  rutis1
!
  title = 'rutis1'
  n = 4
  allocate ( a(1:n,1:n) )
  call rutis1_matrix ( a )
  call rutis1_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  rutis2
!
  title = 'rutis2'
  n = 4
  allocate ( a(1:n,1:n) )
  call rutis2_matrix ( a )
  call rutis2_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  rutis3
!
  title = 'rutis3'
  n = 4
  allocate ( a(1:n,1:n) )
  call rutis3_matrix ( a )
  call rutis3_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  rutis4
!
  title = 'rutis4'
  n = 5
  allocate ( a(1:n,1:n) )
  call rutis4_matrix ( n, a )
  call rutis4_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  rutis5
!
  title = 'rutis5'
  n = 4
  allocate ( a(1:n,1:n) )
  call rutis5_matrix ( a )
  call rutis5_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  schur_block
!
  title = 'schur_block'
  n = 5
  x_n = ( n + 1 ) / 2
  y_n = n / 2
  allocate ( a(1:n,1:n) )
  allocate ( x(1:x_n) )
  allocate ( y(1:y_n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( x_n, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( y_n, r8_lo, r8_hi, seed, y )
  call schur_block_matrix ( n, x, y, a )
  call schur_block_determinant ( n, x, y, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )
!
!  skew_circulant
!
  title = 'skew_circulant'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call skew_circulant_matrix ( n, n, x, a )
  call skew_circulant_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  spd_random
!
  title = 'spd_random'
  n = 5
  allocate ( a(1:n,1:n) )
  key = 123456789
  call spd_random_matrix ( n, key, a )
  call spd_random_determinant ( n, key, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  spline
!
  title = 'spline'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call spline_matrix ( n, x, a )
  call spline_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  stirling
!
  title = 'stirling'
  n = 5
  allocate ( a(1:n,1:n) )
  call stirling_matrix ( n, n, a )
  call stirling_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  stripe
!
  title = 'stripe'
  n = 5
  allocate ( a(1:n,1:n) )
  call stripe_matrix ( n, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  summation
!
  title = 'summation'
  n = 5
  allocate ( a(1:n,1:n) )
  call summation_matrix ( n, n, a )
  call summation_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  sweet1
!
  title = 'sweet1'
  n = 6
  allocate ( a(1:n,1:n) )
  call sweet1_matrix ( a )
  call sweet1_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  sweet2
!
  title = 'sweet2'
  n = 6
  allocate ( a(1:n,1:n) )
  call sweet2_matrix ( a )
  call sweet2_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  sweet3
!
  title = 'sweet3'
  n = 6
  allocate ( a(1:n,1:n) )
  call sweet3_matrix ( a )
  call sweet3_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  sweet4
!
  title = 'sweet4'
  n = 13
  allocate ( a(1:n,1:n) )
  call sweet4_matrix ( a )
  call sweet4_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  sylvester
!
  title = 'sylvester'
  n = 5
  x_n = ( n / 2 )
  y_n = n - ( n / 2 )
  allocate ( a(1:n,1:n) )
  allocate ( x(0:x_n) )
  allocate ( y(0:y_n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( x_n + 1, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( y_n + 1, r8_lo, r8_hi, seed, y )
  call sylvester_matrix ( n, x_n, x, y_n, y, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )
!
!  sylvester_kac
!
  title = 'sylvester_kac'
  n = 5
  allocate ( a(1:n,1:n) )
  call sylvester_kac_matrix ( n, a )
  call sylvester_kac_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  sylvester_kac
!
  title = 'sylvester_KAC'
  n = 6
  allocate ( a(1:n,1:n) )
  call sylvester_kac_matrix ( n, a )
  call sylvester_kac_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  symmetric_random
!
  title = 'symmetric_random'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( d(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, d )
  key = 123456789
  call symmetric_random_matrix ( n, d, key, a )
  call symmetric_random_determinant ( n, d, key, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( d )
!
!  toeplitz
!
  title = 'toeplitz'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:2*n-1) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( 2 * n - 1, r8_lo, r8_hi, seed, x )
  call toeplitz_matrix ( n, x, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  toeplitz_5diag
!
  title = 'toeplitz_5diag'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  d1 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  d2 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  d3 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  d4 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  d5 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call toeplitz_5diag_matrix ( n, d1, d2, d3, d4, d5, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  toeplitz_5s
!
  title = 'toeplitz_5s'
  row_num = 5
  col_num = 5
  n = row_num * col_num
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  gamma = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call toeplitz_5s_matrix ( row_num, col_num, alpha, beta, gamma, n, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  toeplitz_spd
!
  title = 'toeplitz_spd'
  m = 3
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:m) )
  allocate ( y(1:m) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( m, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( m, r8_lo, r8_hi, seed, y )
  y_sum = sum ( y(1:m) )
  y(1:m) = y(1:m) / y_sum
  call toeplitz_spd_matrix ( m, n, x, y, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )
!
!  tournament_random
!
  title = 'tournament_random'
  n = 5
  allocate ( a(1:n,1:n) )
  key = 123456789
  call tournament_random_matrix ( n, key, a )
  call tournament_random_determinant ( n, key, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  transition_random
!
  title = 'transition_random'
  n = 5
  allocate ( a(1:n,1:n) )
  seed = key
  call transition_random_matrix ( n, key, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  trench
!
  title = 'trench'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call trench_matrix ( alpha, n, n, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  tri_upper
!
  title = 'tri_upper'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call tri_upper_matrix ( alpha, n, a )
  call tri_upper_determinant ( alpha, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  tribonacci2
!
  title = 'tribonacci2'
  n = 5
  allocate ( a(1:n,1:n) )
  call tribonacci2_matrix ( n, a )
  call tribonacci2_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  tris
!
  title = 'tris'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  gamma = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call tris_matrix ( n, n, alpha, beta, gamma, a )
  call tris_determinant ( n, alpha, beta, gamma, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  triv
!
  title = 'triv'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n-1) )
  allocate ( y(1:n) )
  allocate ( z(1:n-1) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, z )
  call triv_matrix ( n, x, y, z, a )
  call triv_determinant ( n, x, y, z, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )
  deallocate ( z )
!
!  triw
!
  title = 'triw'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  i4_lo = 0
  i4_hi = n - 1
  k = i4_uniform_ab ( i4_lo, i4_hi, seed )
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call triw_matrix ( alpha, k, n, a )
  call triw_determinant ( alpha, k, n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  upshift
!
  title = 'upshift'
  n = 5
  allocate ( a(1:n,1:n) )
  call upshift_matrix ( n, a )
  call upshift_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  vand1
!
  title = 'vand1'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call vand1_matrix ( n, x, a )
  call vand1_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  vand2
!
  title = 'vand2'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call vand2_matrix ( n, x, a )
  call vand2_determinant ( n, x, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
  deallocate ( x )
!
!  wathen
!
  title = 'wathen'
  row_num = 5
  col_num = 5
  call wathen_order ( row_num, col_num, n )
  allocate ( a(1:n,1:n) )
  call wathen_matrix ( row_num, col_num, n, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  wilk03
!
  title = 'wilk03'
  n = 3
  allocate ( a(1:n,1:n) )
  call wilk03_matrix ( a )
  call wilk03_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  wilk04
!
  title = 'wilk04'
  n = 4
  allocate ( a(1:n,1:n) )
  call wilk04_matrix ( a )
  call wilk04_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  wilk05
!
  title = 'wilk05'
  n = 5
  allocate ( a(1:n,1:n) )
  call wilk05_matrix ( a )
  call wilk05_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  wilk12
!
  title = 'wilk12'
  n = 12
  allocate ( a(1:n,1:n) )
  call wilk12_matrix ( a )
  call wilk12_determinant ( determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  wilk20
!
  title = 'wilk20'
  n = 20
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call wilk20_matrix ( alpha, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )
!
!  wilk21
!
  title = 'wilk21'
  n = 21
  allocate ( a(1:n,1:n) )
  call wilk21_matrix ( n, a )
  call wilk21_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  wilson
!
  title = 'wilson'
  n = 4
  allocate ( a(1:n,1:n) )
  call wilson_matrix ( a )
  call wilson_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  zero
!
  title = 'zero'
  n = 5
  allocate ( a(1:n,1:n) )
  call zero_matrix ( n, n, a )
  call zero_determinant ( n, determ1 )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, n, determ1, determ2, norm_frobenius
  deallocate ( a )
!
!  zielke
!
  title = 'zielke'
  n = 5
  allocate ( a(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  d1 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  d2 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  d3 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call zielke_matrix ( n, d1, d2, d3, a )
  call r8mat_determinant ( n, a, determ2 )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) &
    title, n,          determ2, norm_frobenius
  deallocate ( a )

  return
end
subroutine test_eigen_left ( )

!*****************************************************************************80
!
!! test_eigen_left() tests left eigensystems.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  real ( kind = 8 ), allocatable, dimension ( : ) :: d
  real ( kind = 8 ) error_frobenius
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  integer ( kind = 4 ) key
  real ( kind = 8 ), allocatable, dimension ( : ) :: lambda
  integer ( kind = 4 ) n
  real ( kind = 8 ) norm_frobenius
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) r8mat_norm_fro
  integer ( kind = 4 ) seed
  character ( len = 20 ) title
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_eigen_left():'
  write ( *, '(a)' ) '  Compute the Frobenius norm of the eigenvalue error:'
  write ( *, '(a)' ) '    X * A - LAMBDA * X'
  write ( *, '(a)' ) '  given K left eigenvectors X and eigenvalues LAMBDA.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Title                    N     K      ' // &
    '||A||          ||X*A-LAMBDA*X||'
  write ( *, '(a)' ) ''
!
!  a123
!
  title = 'a123'
  n = 3
  k = 3
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call a123_matrix ( a )
  call a123_eigenvalues ( lambda )
  call a123_eigen_left ( x )
  call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  carry
!
  title = 'carry'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  i4_lo = 2
  i4_hi = 20
  seed = 123456789
  i1 = i4_uniform_ab ( i4_lo, i4_hi, seed )
  call carry_matrix ( n, i1, a )
  call carry_eigenvalues ( n, i1, lambda )
  call carry_eigen_left ( n, i1, x )
  call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  chow
!
  title = 'chow'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call chow_matrix ( alpha, beta, n, n, a )
  call chow_eigenvalues ( alpha, beta, n, lambda )
  call chow_eigen_left ( alpha, beta, n, x )
  call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  DIAGONAL
!
  title = 'DIAGONAL'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( d(1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, d )
  call diagonal_matrix ( n, n, d, a )
  call diagonal_eigenvalues ( n, d, lambda )
  call diagonal_eigen_left ( n, d, x )
  call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( d )
  deallocate ( lambda )
  deallocate ( x )
!
!  rosser1
!
  title = 'rosser1'
  n = 8
  k = 8
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call rosser1_matrix ( a )
  call rosser1_eigenvalues ( lambda )
  call rosser1_eigen_left ( x )
  call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  symmetric_random
!
  title = 'symmetric_random'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( d(1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, d )
  key = 123456789
  call symmetric_random_matrix ( n, d, key, a )
  call symmetric_random_eigenvalues ( n, d, key, lambda )
  call symmetric_random_eigen_left ( n, d, key, x )
  call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( d )
  deallocate ( lambda )
  deallocate ( x )

  return
end
subroutine test_eigen_right ( )

!*****************************************************************************80
!
!! test_eigen_right() tests right eigensystems.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  complex ( kind = 8 ), allocatable, dimension ( :, : ) :: a_complex
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  real ( kind = 8 ), allocatable, dimension ( : ) :: d
  real ( kind = 8 ) error_frobenius
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  integer ( kind = 4 ) key
  real ( kind = 8 ), allocatable, dimension ( : ) :: lambda
  complex ( kind = 8 ), allocatable, dimension ( : ) :: lambda_complex
  integer ( kind = 4 ) n
  real ( kind = 8 ) norm_frobenius
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) r8mat_norm_fro
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) seed
  character ( len = 20 ) title
  real ( kind = 8 ), allocatable, dimension ( : ) :: v1
  real ( kind = 8 ), allocatable, dimension ( : ) :: v2
  real ( kind = 8 ), allocatable, dimension ( : ) :: v3
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: x
  complex ( kind = 8 ), allocatable, dimension ( :, : ) :: x_complex

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_eigen_right():'
  write ( *, '(a)' ) '  Compute the Frobenius norm of the eigenvalue error:'
  write ( *, '(a)' ) '    A * X - X * LAMBDA'
  write ( *, '(a)' ) '  given K right eigenvectors X and eigenvalues LAMBDA.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Title                    N     K      ||A||          ||A*X-X*Lambda||'
  write ( *, '(a)' ) ''
!
!  a123
!
  title = 'a123'
  n = 3
  k = 3
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )

  call a123_matrix ( a )
  call a123_eigenvalues ( lambda )
  call a123_eigen_right ( x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  bab
!
  title = 'bab'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )

  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call bab_matrix ( n, alpha, beta, a )
  call bab_eigenvalues ( n, alpha, beta, lambda )
  call bab_eigen_right ( n, alpha, beta, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  bodewig
!
  title = 'bodewig'
  n = 4
  k = 4
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )

  call bodewig_matrix ( a )
  call bodewig_eigenvalues ( lambda )
  call bodewig_eigen_right ( x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  carry
!
  title = 'carry'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  i4_lo = 2
  i4_hi = 20
  seed = 123456789
  i1 = i4_uniform_ab ( i4_lo, i4_hi, seed )
  call carry_matrix ( n, i1, a )
  call carry_eigenvalues ( n, i1, lambda )
  call carry_eigen_right ( n, i1, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  chow
!
  title = 'chow'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call chow_matrix ( alpha, beta, n, n, a )
  call chow_eigenvalues ( alpha, beta, n, lambda )
  call chow_eigen_right ( alpha, beta, n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  combin
!
  title = 'combin'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call combin_matrix ( alpha, beta, n, a )
  call combin_eigenvalues ( alpha, beta, n, lambda )
  call combin_eigen_right ( alpha, beta, n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  dif2
!
  title = 'dif2'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call dif2_matrix ( n, n, a )
  call dif2_eigenvalues ( n, lambda )
  call dif2_eigen_right ( n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  exchange
!
  title = 'exchange'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call exchange_matrix ( n, n, a )
  call exchange_eigenvalues ( n, lambda )
  call exchange_eigen_right ( n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  fibonacci2
!
  title = 'fibonacci2'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call fibonacci2_matrix ( n, a )
  call fibonacci2_eigenvalues ( n, lambda )
  call fibonacci2_eigen_right ( n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  idempotent_random
!
  title = 'idempotent_random'
  n = 5
  k = 5
  rank = 3
  key = 123456789
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call idempotent_random_matrix ( n, rank, key, a )
  call idempotent_random_eigenvalues ( n, rank, key, lambda )
  call idempotent_random_eigen_right ( n, rank, key, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  identity
!
  title = 'identity'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call identity_matrix ( n, n, a )
  call identity_eigenvalues ( n, lambda )
  call identity_eigen_right ( n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  ill3
!
  title = 'ill3'
  n = 3
  k = 3
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call ill3_matrix ( a )
  call ill3_eigenvalues ( lambda )
  call ill3_eigen_right ( x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  kershaw
!
  title = 'kershaw'
  n = 4
  k = 4
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call kershaw_matrix ( a )
  call kershaw_eigenvalues ( lambda )
  call kershaw_eigen_right ( x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  kms
!  Eigenvalue information requires 0 <= ALPHA <= 1.0.
!
  title = 'kms'
  n = 5
  k = 5
  r8_lo = 0.0D+00
  r8_hi = +1.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call kms_matrix ( alpha, n, n, a )
  call kms_eigenvalues ( alpha, n, lambda )
  call kms_eigen_right ( alpha, n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  line_adj
!
  title = 'line_adj'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )

  call line_adj_matrix ( n, a )
  call line_adj_eigenvalues ( n, lambda )
  call line_adj_eigen_right ( n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  LINE_LOOP_ADJ
!
  title = 'LINE_LOOP_ADJ'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )

  call line_loop_adj_matrix ( n, a )
  call line_loop_adj_eigenvalues ( n, lambda )
  call line_loop_adj_eigen_right ( n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  ONE
!
  title = 'ONE'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call one_matrix ( n, n, a )
  call one_eigenvalues ( n, lambda )
  call one_eigen_right ( n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  ORTEGA
!
  title = 'ORTEGA'
  n = 5
  k = n
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:n) )
  allocate ( v1(1:n) )
  allocate ( v2(1:n) )
  allocate ( v3(1:n) )
  allocate ( x(1:n,1:k) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v1 )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v2 )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v3 )
  call ortega_matrix ( n, v1, v2, v3, a )
  call ortega_eigenvalues ( n, v1, v2, v3, lambda )
  call ortega_eigen_right ( n, v1, v2, v3, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( v1 )
  deallocate ( v2 )
  deallocate ( v3 )
  deallocate ( x )
!
!  OTO
!
  title = 'OTO'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call oto_matrix ( n, n, a )
  call oto_eigenvalues ( n, lambda )
  call oto_eigen_right ( n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  PEI
!
  title = 'PEI'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call pei_matrix ( alpha, n, a )
  call pei_eigenvalues ( alpha, n, lambda )
  call pei_eigen_right ( alpha, n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  rodman
!
  title = 'rodman'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call rodman_matrix ( n, n, alpha, a )
  call rodman_eigenvalues ( n, alpha, lambda )
  call rodman_eigen_right ( n, alpha, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  rosser1
!
  title = 'rosser1'
  n = 8
  k = 8
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call rosser1_matrix ( a )
  call rosser1_eigenvalues ( lambda )
  call rosser1_eigen_right ( x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  rutis1
!
  title = 'rutis1'
  n = 4
  k = 4
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call rutis1_matrix ( a )
  call rutis1_eigenvalues ( lambda )
  call rutis1_eigen_right ( x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  rutis2
!
  title = 'rutis2'
  n = 4
  k = 4
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call rutis2_matrix ( a )
  call rutis2_eigenvalues ( lambda )
  call rutis2_eigen_right ( x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  rutis3
!
  title = 'rutis3'
  n = 4
  k = 4
  allocate ( a(1:n,1:n) )
  allocate ( a_complex(1:n,1:n) )
  allocate ( lambda_complex(1:k) )
  allocate ( x_complex(1:n,1:k) )
  call rutis3_matrix ( a )
  call r8mat_to_c8mat ( n, n, a, a_complex )
  call rutis3_eigenvalues ( lambda_complex )
  call rutis3_eigen_right ( x_complex )
  call c8mat_is_eigen_right ( n, k, a_complex, x_complex, &
    lambda_complex, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( a_complex )
  deallocate ( lambda_complex)
  deallocate ( x_complex )
!
!  rutis5
!
  title = 'rutis5'
  n = 4
  k = 4
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call rutis5_matrix ( a )
  call rutis5_eigenvalues ( lambda )
  call rutis5_eigen_right ( x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  spd_random
!
  title = 'spd_random'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  key = 123456789
  call spd_random_matrix ( n, key, a )
  call spd_random_eigenvalues ( n, key, lambda )
  call spd_random_eigen_right ( n, key, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  sylvester_kac
!
  title = 'sylvester_kac'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call sylvester_kac_matrix ( n, a )
  call sylvester_kac_eigenvalues ( n, lambda )
  call sylvester_kac_eigen_right ( n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  symmetric_random
!
  title = 'symmetric_random'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  allocate ( d(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, d )
  key = 123456789
  call symmetric_random_matrix ( n, d, key, a )
  call symmetric_random_eigenvalues ( n, d, key, lambda )
  call symmetric_random_eigen_right ( n, d, key, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( d )
  deallocate ( lambda )
  deallocate ( x )
!
!  tribonacci2
!
  title = 'tribonacci2'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( a_complex(1:n,1:n) )
  allocate ( lambda_complex(1:k) )
  allocate ( x_complex(1:n,1:k) )
  call tribonacci2_matrix ( n, a )
  call tribonacci2_eigenvalues ( n, lambda_complex )
  call tribonacci2_eigen_right ( n, x_complex )
  call r8mat_to_c8mat ( n, n, a, a_complex )
  call c8mat_is_eigen_right ( n, k, a_complex, x_complex, &
    lambda_complex, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( a_complex )
  deallocate ( lambda_complex )
  deallocate ( x_complex )
!
!  wilk12
!
  title = 'wilk12'
  n = 12
  k = 12
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call wilk12_matrix ( a )
  call wilk12_eigenvalues ( lambda )
  call wilk12_eigen_right ( x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  wilson
!
  title = 'wilson'
  n = 4
  k = 4
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call wilson_matrix ( a )
  call wilson_eigenvalues ( lambda )
  call wilson_eigen_right ( x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )
!
!  zero
!
  title = 'zero'
  n = 5
  k = 5
  allocate ( a(1:n,1:n) )
  allocate ( lambda(1:k) )
  allocate ( x(1:n,1:k) )
  call zero_matrix ( n, n, a )
  call zero_eigenvalues ( n, lambda )
  call zero_eigen_right ( n, x )
  call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( lambda )
  deallocate ( x )

  return
end
subroutine test_inverse ( )

!*****************************************************************************80
!
!! test_inverse() tests the inverse computations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  real ( kind = 8 ) alpha
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: b
  real ( kind = 8 ) beta
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: c
  real ( kind = 8 ), allocatable, dimension ( : ) :: d
  real ( kind = 8 ) error_ab
  real ( kind = 8 ) error_ac
  real ( kind = 8 ) gamma
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  integer ( kind = 4 ) key
  integer ( kind = 4 ) n
  real ( kind = 8 ) norma_frobenius
  real ( kind = 8 ) normc_frobenius
  integer ( kind = 4 ), allocatable, dimension ( : ) :: pivot
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) r8mat_norm_fro
  integer ( kind = 4 ) seed
  character ( len = 20 ) title
  real ( kind = 8 ), allocatable, dimension ( : ) :: v1
  real ( kind = 8 ), allocatable, dimension ( : ) :: v2
  real ( kind = 8 ), allocatable, dimension ( : ) :: v3
  real ( kind = 8 ), allocatable, dimension ( : ) :: x
  integer ( kind = 4 ) x_n
  real ( kind = 8 ), allocatable, dimension ( : ) :: y
  integer ( kind = 4 ) y_n
  real ( kind = 8 ), allocatable, dimension ( : ) :: z

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_inverse():'
  write ( *, '(a)' ) '  A = a test matrix of order N.'
  write ( *, '(a)' ) '  B = inverse as computed by a routine.'
  write ( *, '(a)' ) '  C = inverse as computed by R8MAT_INVERSE.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  ||A||    = Frobenius norm of A.'
  write ( *, '(a)' ) '  ||C||    = Frobenius norm of C.'
  write ( *, '(a)' ) '  ||I-AC|| = Frobenius norm of I-A*C.'
  write ( *, '(a)' ) '  ||I-AB|| = Frobenius norm of I-A*B.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Title                    N    ' // &
                     '||A||      ||C||        ||I-AC||      ||I-AB||'
  write ( *, '(a)' ) ' '
!
!  aegerter
!
  title = 'aegerter'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call aegerter_matrix ( n, a )
  call aegerter_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  bab
!
  title = 'bab'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call bab_matrix ( n, alpha, beta, a )
  call bab_inverse ( n, alpha, beta, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  bauer
!
  title = 'bauer'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call bauer_matrix ( a )
  call bauer_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  bernstein
!
  title = 'bernstein'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call bernstein_matrix ( n, a )
  call bernstein_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  bis
!
  title = 'bis'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call bis_matrix ( alpha, beta, n, n, a )
  call bis_inverse ( alpha, beta, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  BIW
!
  title = 'BIW'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call biw_matrix ( n, a )
  call biw_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  bodewig
!
  title = 'bodewig'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call bodewig_matrix ( a )
  call bodewig_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  boothroyd
!
  title = 'boothroyd'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call boothroyd_matrix ( n, a )
  call boothroyd_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  borderband
!
  title = 'borderband'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call borderband_matrix ( n, a )
  call borderband_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  carry
!
  title = 'carry'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  i4_lo = 2
  i4_hi = 20
  k = i4_uniform_ab ( i4_lo, i4_hi, seed )
  call carry_matrix ( n, k, a )
  call carry_inverse ( n, k, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  CAUCHY
!
  title = 'CAUCHY'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n) )
  allocate ( y(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
  call cauchy_matrix ( n, x, y, a )
  call cauchy_inverse ( n, x, y, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
  deallocate ( y )
!
!  cheby_T
!
  title = 'cheby_T'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call cheby_t_matrix ( n, a )
  call cheby_t_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  cheby_U
!
  title = 'cheby_U'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call cheby_u_matrix ( n, a )
  call cheby_u_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  cheby_van2
!
  title = 'cheby_van2'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call cheby_van2_matrix ( n, a )
  call cheby_van2_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  cheby_van3
!
  title = 'cheby_van3'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call cheby_van3_matrix ( n, a )
  call cheby_van3_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  chow
!
  title = 'chow'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call chow_matrix ( alpha, beta, n, n, a )
  call chow_inverse ( alpha, beta, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  circulant
!
  title = 'circulant'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call circulant_matrix ( n, n, x, a )
  call circulant_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  circulant2
!
  title = 'circulant2'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call circulant2_matrix ( n, a )
  call circulant2_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  CLEMENT1
!  N must be even.
!
  title = 'CLEMENT1'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call clement1_matrix ( n, a )
  call clement1_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  CLEMENT2
!  N must be even.
!
  title = 'CLEMENT2'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n-1) )
  allocate ( y(1:n-1) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, y )
  call clement2_matrix ( n, x, y, a )
  call clement2_inverse ( n, x, y, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
  deallocate ( y )
!
!  combin
!
  title = 'combin'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call combin_matrix ( alpha, beta, n, a )
  call combin_inverse ( alpha, beta, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  companion
!
  title = 'companion'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call companion_matrix ( n, x, a )
  call companion_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  COMPLEX_I
!
  title = 'COMPLEX_I'
  n = 2
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call complex_i_matrix ( a )
  call complex_i_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  conex1
!
  title = 'conex1'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call conex1_matrix ( alpha, a )
  call conex1_inverse ( alpha, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  conex2
!
  title = 'conex2'
  n = 3
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call conex2_matrix ( alpha, a )
  call conex2_inverse ( alpha, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  conex3
!
  title = 'conex3'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call conex3_matrix ( n, a )
  call conex3_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  conex4
!
  title = 'conex4'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call conex4_matrix ( a )
  call conex4_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  CONFERENCE
!  N-1 must be an odd prime or a power of an odd prime.
!
  title = 'CONFERENCE'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call conference_matrix ( n, a )
  call conference_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  daub2
!
  title = 'daub2'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call daub2_matrix ( n, a )
  call daub2_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  daub4
!
  title = 'daub4'
  n = 8
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call daub4_matrix ( n, a )
  call daub4_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  daub6
!
  title = 'daub6'
  n = 12
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call daub6_matrix ( n, a )
  call daub6_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  daub8
!
  title = 'daub8'
  n = 16
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call daub8_matrix ( n, a )
  call daub8_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  daub10
!
  title = 'daub10'
  n = 20
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call daub10_matrix ( n, a )
  call daub10_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  daub12
!
  title = 'daub12'
  n = 24
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call daub12_matrix ( n, a )
  call daub12_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  DIAGONAL
!
  title = 'DIAGONAL'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call diagonal_matrix ( n, n, x, a )
  call diagonal_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  dif1
!  N must be even.
!
  title = 'dif1'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call dif1_matrix ( n, n, a )
  call dif1_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  dif2
!
  title = 'dif2'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call dif2_matrix ( n, n, a )
  call dif2_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  DORR
!
  title = 'DORR'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call dorr_matrix ( alpha, n, a )
  call dorr_inverse ( alpha, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  DOWNSHIFT
!
  title = 'DOWNSHIFT'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call downshift_matrix ( n, a )
  call downshift_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  EULERIAN
!
  title = 'EULERIAN'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call eulerian_matrix ( n, n, a )
  call eulerian_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  exchange
!
  title = 'exchange'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call exchange_matrix ( n, n, a )
  call exchange_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  FIBONACCI2
!
  title = 'FIBONACCI2'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call fibonacci2_matrix ( n, a )
  call fibonacci2_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  FIBONACCI3
!
  title = 'FIBONACCI3'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call fibonacci3_matrix ( n, a )
  call fibonacci3_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  FIEDLER.
!  The FIEDLER_INVERSE routine assumes the X vector is sorted.
!
  title = 'FIEDLER'
  n = 7
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call r8vec_sort_bubble_a ( n, x )
  call fiedler_matrix ( n, n, x, a )
  call fiedler_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  FORSYTHE
!
  title = 'FORSYTHE'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call forsythe_matrix ( alpha, beta, n, a )
  call forsythe_inverse ( alpha, beta, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  FOURIER_COSINE
!
  title = 'FOURIER_COSINE'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call fourier_cosine_matrix ( n, a )
  call fourier_cosine_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  FOURIER_SINE
!
  title = 'FOURIER_SINE'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call fourier_sine_matrix ( n, a )
  call fourier_sine_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  frank
!
  title = 'frank'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call frank_matrix ( n, a )
  call frank_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  GFPP
!
  title = 'GFPP'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call gfpp_matrix ( n, alpha, a )
  call gfpp_inverse ( n, alpha, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  givens
!
  title = 'givens'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call givens_matrix ( n, n, a )
  call givens_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  GK316
!
  title = 'GK316'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call gk316_matrix ( n, a )
  call gk316_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  GK323
!
  title = 'GK323'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call gk323_matrix ( n, n, a )
  call gk323_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  GK324
!
  title = 'GK324'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
  call gk324_matrix ( n, n, x, a )
  call gk324_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  HANKEL_N
!
  title = 'HANKEL_N'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call hankel_n_matrix ( n, a )
  call hankel_n_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  HANOWA
!  N must be even.
!
  title = 'HANOWA'
  n = 6
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call hanowa_matrix ( alpha, n, a )
  call hanowa_inverse ( alpha, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  HARMAN
!
  title = 'HARMAN'
  n = 8
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call harman_matrix ( a )
  call harman_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  HARTLEY
!
  title = 'HARTLEY'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call hartley_matrix ( n, a )
  call hartley_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  HELMERT
!
  title = 'HELMERT'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call helmert_matrix ( n, a )
  call helmert_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  HELMERT2
!
  title = 'HELMERT2'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call helmert2_matrix ( n, x, a )
  call helmert2_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  HERMITE
!
  title = 'HERMITE'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call hermite_matrix ( n, a )
  call hermite_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  HERNDON
!
  title = 'HERNDON'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call herndon_matrix ( n, a )
  call herndon_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  HILBERT
!
  title = 'HILBERT'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call hilbert_matrix ( n, n, a )
  call hilbert_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  HOUSEHOLDER
!
  title = 'HOUSEHOLDER'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call householder_matrix ( n, x, a )
  call householder_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  identity
!
  title = 'identity'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call identity_matrix ( n, n, a )
  call identity_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  ill3
!
  title = 'ill3'
  n = 3
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call ill3_matrix ( a )
  call ill3_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  INTEGRATION
!
  title = 'INTEGRATION'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call integration_matrix ( alpha, n, a )
  call integration_inverse ( alpha, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  involutory 
!
  title = 'involutory '
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call involutory_matrix ( n, a )
  call involutory_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  JACOBI
!  N must be even.
!
  title = 'JACOBI'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call jacobi_matrix ( n, n, a )
  call jacobi_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  JORDAN
!
  title = 'JORDAN'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call jordan_matrix ( n, n, alpha, a )
  call jordan_inverse ( n, alpha, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  kahan
!
  title = 'kahan'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call kahan_matrix ( alpha, n, n, a )
  call kahan_inverse ( alpha, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
   write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  KERSHAW
!
  title = 'KERSHAW'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call kershaw_matrix ( a )
  call kershaw_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  KERSHAWTRI
!
  title = 'KERSHAWTRI'
  n = 5
  x_n = ( n + 1 ) / 2
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:x_n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( x_n, r8_lo, r8_hi, seed, x )
  call kershawtri_matrix ( n, x, a )
  call kershawtri_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  kms
!
  title = 'kms'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call kms_matrix ( alpha, n, n, a )
  call kms_inverse ( alpha, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  LAGUERRE
!
  title = 'LAGUERRE'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call laguerre_matrix ( n, a )
  call laguerre_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  LEGENDRE
!
  title = 'LEGENDRE'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call legendre_matrix ( n, a )
  call legendre_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  LEHMER
!
  title = 'LEHMER'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call lehmer_matrix ( n, n, a )
  call lehmer_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  LESP
!
  title = 'LESP'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call lesp_matrix ( n, n, a )
  call lesp_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  LIETZKE
!
  title = 'LIETZKE'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call lietzke_matrix ( n, a )
  call lietzke_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  line_adj
!  N must be even.
!
  title = 'line_adj'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call line_adj_matrix ( n, a )
  call line_adj_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  LOTKIN
!
  title = 'LOTKIN'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call lotkin_matrix ( n, n, a )
  call lotkin_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  MAXIJ
!
  title = 'MAXIJ'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call maxij_matrix ( n, n, a )
  call maxij_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  MILNES
!
  title = 'MILNES'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call milnes_matrix ( n, n, x, a )
  call milnes_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  MINIJ
!
  title = 'MINIJ'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call minij_matrix ( n, n, a )
  call minij_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  MOLER1
!
  title = 'MOLER1'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call moler1_matrix ( alpha, n, n, a )
  call moler1_inverse ( alpha, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  MOLER3
!
  title = 'MOLER3'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call moler3_matrix ( n, n, a )
  call moler3_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  ORTEGA
!
  title = 'ORTEGA'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( v1(1:n) )
  allocate ( v2(1:n) )
  allocate ( v3(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v1 )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v2 )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v3 )
  call ortega_matrix ( n, v1, v2, v3, a )
  call ortega_inverse ( n, v1, v2, v3, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( v1 )
  deallocate ( v2 )
  deallocate ( v3 )
!
!  orthogonal_symmetric
!
  title = 'orthogonal_symmetric'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call orthogonal_symmetric_matrix ( n, a )
  call orthogonal_symmetric_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  OTO
!
  title = 'OTO'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call oto_matrix ( n, n, a )
  call oto_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  PARTER
!
  title = 'PARTER'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call parter_matrix ( n, n, a )
  call parter_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  PASCAL1
!
  title = 'PASCAL1'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call pascal1_matrix ( n, a )
  call pascal1_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  PASCAL2
!
  title = 'PASCAL2'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call pascal2_matrix ( n, a )
  call pascal2_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  PASCAL3
!
  title = 'PASCAL3'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call pascal3_matrix ( n, alpha, a )
  call pascal3_inverse ( n, alpha, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  PEI
!
  title = 'PEI'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call pei_matrix ( alpha, n, a )
  call pei_inverse ( alpha, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  PERMUTATION_random
!
  title = 'PERMUTATION_random'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  key = 123456789
  call permutation_random_matrix ( n, key, a )
  call permutation_random_inverse ( n, key, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  PLU
!
  title = 'PLU'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( pivot(1:n) )
  seed = 123456789
  do i = 1, n
    i4_lo = i
    i4_hi = n
    pivot(i) = i4_uniform_ab ( i4_lo, i4_hi, seed )
  end do
  call plu_matrix ( n, pivot, a )
  call plu_inverse ( n, pivot, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( pivot )
!
!  RIS
!
  title = 'RIS'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call ris_matrix ( n, a )
  call ris_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  rodman
!
  title = 'rodman'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call rodman_matrix ( n, n, alpha, a )
  call rodman_inverse ( n, alpha, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  rutis1
!
  title = 'rutis1'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call rutis1_matrix ( a )
  call rutis1_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  rutis2
!
  title = 'rutis2'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call rutis2_matrix ( a )
  call rutis2_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  rutis3
!
  title = 'rutis3'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call rutis3_matrix ( a )
  call rutis3_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  rutis4
!
  title = 'rutis4'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call rutis4_matrix ( n, a )
  call rutis4_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  rutis5
!
  title = 'rutis5'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call rutis5_matrix ( a )
  call rutis5_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  schur_block
!
  title = 'schur_block'
  n = 5
  x_n = ( n + 1 ) / 2
  y_n = n / 2
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:x_n) )
  allocate ( y(1:y_n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( x_n, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( y_n, r8_lo, r8_hi, seed, y )
  call schur_block_matrix ( n, x, y, a )
  call schur_block_inverse ( n, x, y, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
  deallocate ( y )
!
!  spd_random
!
  title = 'spd_random'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  key = 123456789
  call spd_random_matrix ( n, key, a )
  call spd_random_inverse ( n, key, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  spline
!
  title = 'spline'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n-1) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
  call spline_matrix ( n, x, a )
  call spline_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  stirling
!
  title = 'stirling'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call stirling_matrix ( n, n, a )
  call stirling_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  summation
!
  title = 'summation'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call summation_matrix ( n, n, a )
  call summation_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  sweet1
!
  title = 'sweet1'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call sweet1_matrix ( a )
  call sweet1_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  sweet2
!
  title = 'sweet2'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call sweet2_matrix ( a )
  call sweet2_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  sweet3
!
  title = 'sweet3'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call sweet3_matrix ( a )
  call sweet3_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  sweet4
!
  title = 'sweet4'
  n = 13
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call sweet4_matrix ( a )
  call sweet4_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  sylvester_kac
!  N must be even.
!
  title = 'sylvester_kac'
  n = 6
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call sylvester_kac_matrix ( n, a )
  call sylvester_kac_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  symmetric_random
!
  title = 'symmetric_random'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( d(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, d )
  key = 123456789
  call symmetric_random_matrix ( n, d, key, a )
  call symmetric_random_inverse ( n, d, key, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( d )
!
!  tri_upper
!
  title = 'tri_upper'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call tri_upper_matrix ( alpha, n, a )
  call tri_upper_inverse ( alpha, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  tris
!
  title = 'tris'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
  gamma = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call tris_matrix ( n, n, alpha, beta, gamma, a )
  call tris_inverse ( n, alpha, beta, gamma, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  triv
!
  title = 'triv'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n-1) )
  allocate ( y(1:n) )
  allocate ( z(1:n-1) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
  call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, z )
  call triv_matrix ( n, x, y, z, a )
  call triv_inverse ( n, x, y, z, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
  deallocate ( y )
  deallocate ( z )
!
!  triw
!
  title = 'triw'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  i4_lo = 0
  i4_hi = n - 1
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  k = i4_uniform_ab ( i4_lo, i4_hi, seed )
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call triw_matrix ( alpha, k, n, a )
  call triw_inverse ( alpha, k, n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  UNITARY_random
!  Need to add C8_INVERSE(), or, more likely, push complex matrices out
!  to another package.
!
  title = 'UNITARY_random'
  n = 5
! allocate ( c8_a(1:n,1:n) )
! allocate ( c8_b(1:n,1:n) )
! allocate ( c8_c(1:n,1:n) )
! key = 123456789
! call unitary_random_matrix ( n, key, c8_a )
! call unitary_random_inverse ( n, key, c8_b )
! call c8mat_inverse ( n, c8_a, c8_c )
! call c8mat_is_inverse ( n, c8_a, c8_b, error_ab )
! call c8mat_is_inverse ( n, c8_a, c8_c, error_ac )
! norma_frobenius = c8mat_norm_fro ( n, n, a )
! normc_frobenius = c8mat_norm_fro ( n, n, c )
! write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
!   title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
! deallocate ( c8_a )
! deallocate ( c8_b )
! deallocate ( c8_c )
!
!  upshift
!
  title = 'upshift'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call upshift_matrix ( n, a )
  call upshift_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  vand1
!
  title = 'vand1'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call vand1_matrix ( n, x, a )
  call vand1_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  vand2
!
  title = 'vand2'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call vand2_matrix ( n, x, a )
  call vand2_inverse ( n, x, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
  deallocate ( x )
!
!  wilk03
!
  title = 'wilk03'
  n = 3
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call wilk03_matrix ( a )
  call wilk03_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  wilk04
!
  title = 'wilk04'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call wilk04_matrix ( a )
  call wilk04_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  wilk05
!
  title = 'wilk05'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call wilk05_matrix ( a )
  call wilk05_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  wilk21
!
  title = 'wilk21'
  n = 21
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call wilk21_matrix ( n, a )
  call wilk21_inverse ( n, b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  wilson
!
  title = 'wilson'
  n = 4
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
  call wilson_matrix ( a )
  call wilson_inverse ( b )
  call r8mat_inverse ( n, a, c )
  call r8mat_is_inverse ( n, a, b, error_ab )
  call r8mat_is_inverse ( n, a, c, error_ac )
  norma_frobenius = r8mat_norm_fro ( n, n, a )
  normc_frobenius = r8mat_norm_fro ( n, n, c )
  write ( *, '(2x,a20,2x,i4,2x,g10.2,2x,g10.2,2x,g10.2,2x,g10.2)' ) &
    title, n, norma_frobenius, normc_frobenius, error_ac, error_ab
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )

  return
end
subroutine test_llt ( )

!*****************************************************************************80
!
!! test_llt() tests LLT factors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) error_frobenius
  real ( kind = 8 ), allocatable :: l(:,:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) norm_a_frobenius
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) r8mat_norm_fro
  integer ( kind = 4 ) seed
  character ( len = 20 ) title

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'test_llt():'
  write ( *, '(a)' ) '  A = a test matrix of order M by M'
  write ( *, '(a)' ) '  L is an M by N lower triangular Cholesky factor.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  ||A|| = Frobenius norm of A.'
  write ( *, '(a)' ) '  ||A-LLT|| = Frobenius norm of A-L*L''.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Title                    M     N      ' // &
    '||A||            ||A-LLT||'
  write ( *, '(a)' ) ''
!
!  dif2
!
  title = 'dif2'
  m = 5
  n = 5
  allocate ( a(1:m,1:m) )
  allocate ( l(1:m,1:n) )
  call dif2_matrix ( m, n, a )
  call dif2_llt ( n, l )
  call r8mat_is_llt ( m, n, a, l, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
!
!  givens
!
  title = 'givens'
  m = 5
  n = 5
  allocate ( a(1:m,1:m) )
  allocate ( l(1:m,1:n) )
  call givens_matrix ( m, n, a )
  call givens_llt ( n, l )
  call r8mat_is_llt ( m, n, a, l, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
!
!  KERSHAW
!
  title = 'KERSHAW'
  m = 4
  n = 4
  allocate ( a(1:m,1:m) )
  allocate ( l(1:m,1:n) )
  call kershaw_matrix ( a )
  call kershaw_llt ( l )
  call r8mat_is_llt ( m, n, a, l, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
!
!  LEHMER
!
  title = 'LEHMER'
  m = 5
  n = 5
  allocate ( a(1:m,1:m) )
  allocate ( l(1:m,1:n) )
  call lehmer_matrix ( n, n, a )
  call lehmer_llt ( n, l )
  call r8mat_is_llt ( m, n, a, l, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
!
!  MINIJ
!
  title = 'MINIJ'
  m = 5
  n = 5
  allocate ( a(1:m,1:m) )
  allocate ( l(1:m,1:n) )
  call minij_matrix ( n, n, a )
  call minij_llt ( n, l )
  call r8mat_is_llt ( m, n, a, l, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
!
!  MOLER1
!
  title = 'MOLER1'
  m = 5
  n = 5
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  allocate ( a(1:m,1:m) )
  allocate ( l(1:m,1:n) )
  call moler1_matrix ( alpha, m, n, a )
  call moler1_llt ( alpha, n, l )
  call r8mat_is_llt ( m, n, a, l, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
!
!  MOLER3
!
  title = 'MOLER3'
  m = 5
  n = 5
  allocate ( a(1:m,1:m) )
  allocate ( l(1:m,1:n) )
  call moler3_matrix ( m, n, a )
  call moler3_llt ( n, l )
  call r8mat_is_llt ( m, n, a, l, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
!
!  OTO
!
  title = 'OTO'
  m = 5
  n = 5
  allocate ( a(1:m,1:m) )
  allocate ( l(1:m,1:n) )
  call oto_matrix ( m, n, a )
  call oto_llt ( n, l )
  call r8mat_is_llt ( m, n, a, l, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
!
!  PASCAL2
!
  title = 'PASCAL2'
  m = 5
  n = 5
  allocate ( a(1:m,1:m) )
  allocate ( l(1:m,1:n) )
  call pascal2_matrix ( n, a )
  call pascal2_llt ( n, l )
  call r8mat_is_llt ( m, n, a, l, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
!
!  wilson
!
  title = 'wilson'
  m = 4
  n = 4
  allocate ( a(1:m,1:m) )
  allocate ( l(1:m,1:n) )
  call wilson_matrix ( a )
  call wilson_llt ( l )
  call r8mat_is_llt ( m, n, a, l, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )

  return
end
subroutine test_null_left ( )

!*****************************************************************************80
!
!! test_null_left() tests left null vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) error_l2
  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) norm_a_frobenius
  real ( kind = 8 ) norm_x_l2
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) r8mat_norm_fro
  real ( kind = 8 ) r8vec_norm_l2
  integer ( kind = 4 ) seed
  character ( len = 20 ) title
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_null_left():'
  write ( *, '(a)' ) '  A = a test matrix of order M by N'
  write ( *, '(a)' ) '  x = an M vector, candidate for a left null vector.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  ||A|| = Frobenius norm of A.'
  write ( *, '(a)' ) '  ||x|| = L2 norm of x.'
  write ( *, '(a)' ) '  ||x''*A||/||x|| = L2 norm of x''A over L2 norm of x.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Title                    M     N      ' // &
               '||A||            ||x||        ||x''*A||/||x||'
  write ( *, '(a)' ) ' '
!
!  a123
!
  title = 'a123'
  m = 3
  n = 3
  allocate ( a(1:m,1:n) )
  allocate ( x(1:m) )
  call a123_matrix ( a )
  call a123_null_left ( x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  cheby_diff1
!
  title = 'cheby_diff1'
  m = 5
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call cheby_diff1_matrix ( n, a )
  call cheby_diff1_null_left ( m, n, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  CREATION
!
  title = 'CREATION'
  m = 5
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call creation_matrix ( m, n, a )
  call creation_null_left ( m, n, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  dif1
!  Only has null vectors for M odd
!
  title = 'dif1'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( x(1:m) )
  call dif1_matrix ( m, n, a )
  call dif1_null_left ( m, n, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  dif1CYCLIC
!
  title = 'dif1CYCLIC'
  m = 5
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call dif1cyclic_matrix ( n, a )
  call dif1cyclic_null_left ( m, n, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  dif2CYCLIC
!
  title = 'dif2CYCLIC'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( x(1:m) )
  call dif2cyclic_matrix ( n, a )
  call dif2cyclic_null_left ( m, n, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  EBERLEIN
!
  title = 'EBERLEIN'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( x(1:m) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call eberlein_matrix ( alpha, n, a )
  call eberlein_null_left ( m, n, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  FIBONACCI1
!
  title = 'FIBONACCI1'
  m = 5
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  f1 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  f2 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call fibonacci1_matrix ( n, f1, f2, a )
  call fibonacci1_null_left ( m, n, f1, f2, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  LAUCHLI
!
  title = 'LAUCHLI'
  m = 6
  n = m - 1
  allocate ( a(1:m,1:n) )
  allocate ( x(1:m) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call lauchli_matrix ( alpha, m, n, a )
  call lauchli_null_left ( alpha, m, n, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  line_adj
!
  title = 'line_adj'
  m = 7
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call line_adj_matrix ( n, a )
  call line_adj_null_left ( m, n, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  MOLER2
!
  title = 'MOLER2'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call moler2_matrix ( a )
  call moler2_null_left ( x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  ONE
!
  title = 'ONE'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( x(1:m) )
  call one_matrix ( m, n, a )
  call one_null_left ( m, n, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  RING_ADJ
!  M must be a multiple of 4 for there to be a null vector.
!
  title = 'RING_ADJ'
  m = 12
  n = 12
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call ring_adj_matrix ( n, a )
  call ring_adj_null_left ( m, n, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  rosser1
!
  title = 'rosser1'
  m = 8
  n = 8
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call rosser1_matrix ( a )
  call rosser1_null_left ( x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  zero
!
  title = 'zero'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( x(1:m) )
  call zero_matrix ( m, n, a )
  call zero_null_left ( m, n, x )
  call r8mat_is_null_left ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( m, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )

  return
end
subroutine test_null_right ( )

!*****************************************************************************80
!
!! test_null_right() tests right null vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  integer ( kind = 4 ) col_num
  real ( kind = 8 ) error_l2
  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) norm_a_frobenius
  real ( kind = 8 ) norm_x_l2
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) r8mat_norm_fro
  real ( kind = 8 ) r8vec_norm_l2
  integer ( kind = 4 ) row_num
  integer ( kind = 4 ) seed
  character ( len = 20 ) title
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_null_right()'
  write ( *, '(a)' ) '  A = a test matrix of order M by N'
  write ( *, '(a)' ) '  x = an N vector, candidate for a right null vector.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  ||A|| = Frobenius norm of A.'
  write ( *, '(a)' ) '  ||x|| = L2 norm of x.'
  write ( *, '(a)' ) '  ||A*x||/||x|| = L2 norm of A*x over L2 norm of x.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Title                    M     N      ' // &
               '||A||            ||x||        ||A*x||/||x||'
  write ( *, '(a)' ) ' '
!
!  a123
!
  title = 'a123'
  m = 3
  n = 3
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call a123_matrix ( a )
  call a123_null_right ( x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  archimedes
!
  title = 'archimedes'
  m = 7
  n = 8
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call archimedes_matrix ( a )
  call archimedes_null_right ( x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  cheby_diff1
!
  title = 'cheby_diff1'
  m = 5
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call cheby_diff1_matrix ( n, a )
  call cheby_diff1_null_right ( m, n, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  CREATION
!
  title = 'CREATION'
  m = 5
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call creation_matrix ( m, n, a )
  call creation_null_right ( m, n, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  dif1
!  Only has null vectors for N odd.
!
  title = 'dif1'
  m = 5
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call dif1_matrix ( m, n, a )
  call dif1_null_right ( m, n, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  dif1CYCLIC
!
  title = 'dif1CYCLIC'
  m = 5
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call dif1cyclic_matrix ( n, a )
  call dif1cyclic_null_right ( m, n, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  dif2CYCLIC
!
  title = 'dif2CYCLIC'
  m = 5
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call dif2cyclic_matrix ( n, a )
  call dif2cyclic_null_right ( m, n, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  FIBONACCI1
!
  title = 'FIBONACCI1'
  m = 5
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  f1 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  f2 = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call fibonacci1_matrix ( n, f1, f2, a )
  call fibonacci1_null_right ( m, n, f1, f2, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  HAMMING
!
  title = 'HAMMING'
  m = 5
  n = 2 ** m - 1
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call hamming_matrix ( m, n, a )
  call hamming_null_right ( m, n, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  line_adj
!
  title = 'line_adj'
  m = 7
  n = m
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call line_adj_matrix ( n, a )
  call line_adj_null_right ( m, n, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  MOLER2
!
  title = 'MOLER2'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call moler2_matrix ( a )
  call moler2_null_right ( x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  NEUMANN
!
  title = 'NEUMANN'
  row_num = 5
  col_num = 5
  m = row_num * col_num
  n = row_num * col_num
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call neumann_matrix ( row_num, col_num, a )
  call neumann_null_right ( row_num, col_num, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  ONE
!
  title = 'ONE'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call one_matrix ( m, n, a )
  call one_null_right ( m, n, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  RING_ADJ
!  N must be a multiple of 4 for there to be a null vector.
!
  title = 'RING_ADJ'
  m = 12
  n = 12
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call ring_adj_matrix ( n, a )
  call ring_adj_null_right ( m, n, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  rosser1
!
  title = 'rosser1'
  m = 8
  n = 8
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call rosser1_matrix ( a )
  call rosser1_null_right ( x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )
!
!  zero
!
  title = 'zero'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( x(1:n) )
  call zero_matrix ( m, n, a )
  call zero_null_right ( m, n, x )
  call r8mat_is_null_right ( m, n, a, x, error_l2 )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  norm_x_l2 = r8vec_norm_l2 ( n, x )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
  deallocate ( a )
  deallocate ( x )

  return
end
subroutine test_plu ( )

!*****************************************************************************80
!
!! test_plu() tests the PLU factors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) error_frobenius
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: l
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) norm_a_frobenius
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: p
  integer ( kind = 4 ), allocatable, dimension ( : ) :: pivot
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) r8mat_norm_fro
  integer ( kind = 4 ) seed
  character ( len = 20 ) title
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: u
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_plu():'
  write ( *, '(a)' ) '  A = a test matrix of order M by N'
  write ( *, '(a)' ) '  P, L, U are the PLU factors.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  ||A|| = Frobenius norm of A.'
  write ( *, '(a)' ) '  ||A-PLU|| = Frobenius norm of A-P*L*U.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Title                    M     N      ' // &
               '||A||            ||A-PLU||'
  write ( *, '(a)' ) ' '
!
!  a123
!
  title = 'a123'
  m = 3
  n = 3
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call a123_matrix ( a )
  call a123_plu ( p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  bodewig
!
  title = 'bodewig'
  m = 4
  n = 4
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call bodewig_matrix ( a )
  call bodewig_plu ( p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  borderband
!
  title = 'borderband'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call borderband_matrix ( n, a )
  call borderband_plu ( n, p, l, u )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  dif2
!
  title = 'dif2'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call dif2_matrix ( m, n, a )
  call dif2_plu ( n, p, l, u )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  GFPP
!
  title = 'GFPP'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call gfpp_matrix ( n, alpha, a )
  call gfpp_plu ( n, alpha, p, l, u )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  givens
!
  title = 'givens'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call givens_matrix ( n, n, a )
  call givens_plu ( n, p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  kms
!
  title = 'kms'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call kms_matrix ( alpha, m, n, a )
  call kms_plu ( alpha, n, p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  LEHMER
!
  title = 'LEHMER'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call lehmer_matrix ( n, n, a )
  call lehmer_plu ( n, p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  MAXIJ
!
  title = 'MAXIJ'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call maxij_matrix ( n, n, a )
  call maxij_plu ( n, p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  MINIJ
!
  title = 'MINIJ'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call minij_matrix ( n, n, a )
  call minij_plu ( n, p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  MOLER1
!
  title = 'MOLER1'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
  call moler1_matrix ( alpha, n, n, a )
  call moler1_plu ( alpha, n, p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  MOLER3
!
  title = 'MOLER3'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call moler3_matrix ( m, n, a )
  call moler3_plu ( n, p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  OTO
!
  title = 'OTO'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call oto_matrix ( m, n, a )
  call oto_plu ( n, p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  PASCAL2
!
  title = 'PASCAL2'
  m = 5
  n = 5
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call pascal2_matrix ( n, a )
  call pascal2_plu ( n, p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  PLU
!
  title = 'PLU'
  n = 5
  allocate ( a(1:n,1:n) )
  allocate ( l(1:n,1:n) )
  allocate ( p(1:n,1:n) )
  allocate ( pivot(n) )
  allocate ( u(1:n,1:n) )
  seed = 123456789
  do i = 1, n
    i4_lo = i
    i4_hi = n
    pivot(i) = i4_uniform_ab ( i4_lo, i4_hi, seed )
  end do
  call plu_matrix ( n, pivot, a )
  call plu_plu ( n, pivot, p, l, u )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
!
!  vand2
!
  title = 'vand2'
  m = 4
  n = 4
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  allocate ( x(1:m) )
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( m, r8_lo, r8_hi, seed, x )
  call vand2_matrix ( m, x, a )
  call vand2_plu ( m, x, p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )
  deallocate ( x )
!
!  wilson
!
  title = 'wilson'
  m = 4
  n = 4
  allocate ( a(1:m,1:n) )
  allocate ( p(1:m,1:m) )
  allocate ( l(1:m,1:m) )
  allocate ( u(1:m,1:n) )
  call wilson_matrix ( a )
  call wilson_plu ( p, l, u  )
  call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
  norm_a_frobenius = r8mat_norm_fro ( m, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_a_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( l )
  deallocate ( p )
  deallocate ( u )

  return
end
subroutine test_solution ( )

!*****************************************************************************80
!
!! test_solution() tests the linear solution computations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: b
  real ( kind = 8 ) error_frobenius
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncol
  real ( kind = 8 ) norm_frobenius
  integer ( kind = 4 ) nrow
  real ( kind = 8 ) r8mat_norm_fro
  character ( len = 20 ) title
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'test_solution():'
  write ( *, '(a)' ) '  Compute the Frobenius norm of the solution error:'
  write ( *, '(a)' ) '    A * X - B'
  write ( *, '(a)' ) '  given MxN matrix A, NxK solution X, MxK right hand side B.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Title                    M     N     K      ||A||         ||A*X-B||'
  write ( *, '(a)' ) ' '
!
!  a123
!
  title = 'a123'
  m = 3
  n = 3
  k = 1
  allocate ( a(1:m,1:n) )
  allocate ( b(1:m,1:k) )
  allocate ( x(1:n,1:k) )
  call a123_matrix ( a )
  call a123_rhs ( b )
  call a123_solution ( x )
  call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( b )
  deallocate ( x )
!
!  bodewig
!
  title = 'bodewig'
  m = 4
  n = 4
  k = 1
  allocate ( a(1:m,1:n) )
  allocate ( b(1:m,1:k) )
  allocate ( x(1:n,1:k) )
  call bodewig_matrix ( a )
  call bodewig_rhs ( b )
  call bodewig_solution ( x )
  call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( b )
  deallocate ( x )
!
!  dif2
!
  title = 'dif2'
  m = 10
  n = 10
  k = 2
  allocate ( a(1:m,1:n) )
  allocate ( b(1:m,1:k) )
  allocate ( x(1:n,1:k) )
  call dif2_matrix ( m, n, a )
  call dif2_rhs ( m, k, b )
  call dif2_solution ( n, k, x )
  call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( b )
  deallocate ( x )
!
!  frank
!
  title = 'frank'
  m = 10
  n = 10
  k = 2
  allocate ( a(1:m,1:n) )
  allocate ( b(1:m,1:k) )
  allocate ( x(1:n,1:k) )
  call frank_matrix ( n, a )
  call frank_rhs ( m, k, b )
  call frank_solution ( n, k, x )
  call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( b )
  deallocate ( x )
!
!  poisson
!
  title = 'poisson'
  nrow = 4
  ncol = 5
  m = nrow * ncol
  n = nrow * ncol
  k = 1
  allocate ( a(1:m,1:n) )
  allocate ( b(1:m,1:k) )
  allocate ( x(1:n,1:k) )
  call poisson_matrix ( nrow, ncol, a )
  call poisson_rhs ( nrow, ncol, b )
  call poisson_solution ( nrow, ncol, x )
  call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( b )
  deallocate ( x )
!
!  wilk03
!
  title = 'wilk03'
  m = 3
  n = 3
  k = 1
  allocate ( a(1:m,1:n) )
  allocate ( b(1:m,1:k) )
  allocate ( x(1:n,1:k) )
  call wilk03_matrix ( a )
  call wilk03_rhs ( b )
  call wilk03_solution ( x )
  call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( b )
  deallocate ( x )
!
!  wilk04
!
  title = 'wilk04'
  m = 4
  n = 4
  k = 1
  allocate ( a(1:m,1:n) )
  allocate ( b(1:m,1:k) )
  allocate ( x(1:n,1:k) )
  call wilk04_matrix ( a )
  call wilk04_rhs ( b )
  call wilk04_solution ( x )
  call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( b )
  deallocate ( x )
!
!  wilson
!
  title = 'wilson'
  m = 4
  n = 4
  k = 1
  allocate ( a(1:m,1:n) )
  allocate ( b(1:m,1:k) )
  allocate ( x(1:n,1:k) )
  call wilson_matrix ( a )
  call wilson_rhs ( b )
  call wilson_solution ( x )
  call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, k, norm_frobenius, error_frobenius
  deallocate ( a )
  deallocate ( b )
  deallocate ( x )

  return
end
subroutine test_type ( )

!*****************************************************************************80
!
!! test_type() tests functions which test the type of a matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 July 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) error_frobenius
  integer ( kind = 4 ) key
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) norm_frobenius
  real ( kind = 8 ) r8mat_norm_fro
  character ( len = 20 ) title

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'test_type():'
  write ( *, '(a)' ) '  Test functions that query the type of a matrix.'
!
!  R8MAT_IS_transition.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Title                    M     N     ||A||' // &
    '            ||transition Error||'
  write ( *, '(a)' ) ''

  title = 'bodewig'
  m = 4
  n = 4
  allocate ( a(1:m,1:n) )
  call bodewig_matrix ( a )
  call r8mat_is_transition ( m, n, a, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_frobenius, error_frobenius
  deallocate ( a )

  title = 'snakes'
  m = 101
  n = 101
  allocate ( a(1:m,1:n) )
  call snakes_matrix ( a )
  call r8mat_is_transition ( m, n, a, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_frobenius, error_frobenius
  deallocate ( a )

  title = 'transition_random'
  m = 5
  n = 5
  key = 123456789
  allocate ( a(1:m,1:n) )
  call transition_random_matrix ( n, key, a )
  call r8mat_is_transition ( m, n, a, error_frobenius )
  norm_frobenius = r8mat_norm_fro ( n, n, a )
  write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) &
    title, m, n, norm_frobenius, error_frobenius
  deallocate ( a )

  return
end
