program main

!*****************************************************************************80
!
!! MAIN is the main program for R8GE_TEST.
!
!  Discussion:
!
!    R8GE_TEST tests the R8GE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Test the R8GE library.'

  call i4_log_10_test ( )
  call i4vec_print_test ( )

  call r8_sign_test ( )
  call r8_uniform_01_test ( )

  call r8col_swap_test ( )

  call r8ge_cg_test ( )
  call r8ge_co_test ( )
  call r8ge_det_test ( )
  call r8ge_dif2_test ( )
  call r8ge_dilu_test ( )
  call r8ge_fa_test ( )
  call r8ge_fs_test ( )
  call r8ge_fss_test ( )
  call r8ge_hilbert_test ( )
  call r8ge_hilbert_inverse_test ( )
  call r8ge_identity_test ( )
  call r8ge_ilu_test ( )
  call r8ge_indicator_test ( )
  call r8ge_inverse_test ( )
  call r8ge_ml_test ( )
  call r8ge_mm_test ( )
  call r8ge_mtm_test ( )
  call r8ge_mtv_test ( )
  call r8ge_mu_test ( )
  call r8ge_mv_test ( )
  call r8ge_plu_test ( )
  call r8ge_poly_test ( )
  call r8ge_print_test ( )
  call r8ge_print_some_test ( )
  call r8ge_random_test ( )
  call r8ge_res_test ( )
  call r8ge_sl_test ( )
  call r8ge_sl_it_test ( )
  call r8ge_to_r8vec_test ( )
  call r8ge_transpose_print_test ( ) 
  call r8ge_transpose_print_some_test ( )
  call r8ge_trf_test ( )
  call r8ge_trs_test ( )
  call r8ge_zeros_test ( )

  call r8row_swap_test ( )

  call r8vec_indicator1_test ( )
  call r8vec_norm_test ( )
  call r8vec_norm_affine_test ( )
  call r8vec_print_test ( )
  call r8vec_print_some_test ( )
  call r8vec_to_r8ge_test ( )
  call r8vec_uniform_01_test ( )

  call r8vec2_print_some_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine i4_log_10_test ( )

!*****************************************************************************80
!
!! I4_LOG_10_TEST tests I4_LOG_10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 13

  integer ( kind = 4 ) i4_log_10
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x
  integer ( kind = 4 ), dimension ( test_num ) :: x_test = (/ &
    0, 1, 2, 3, 9, 10, 11, 99, 101, -1, -2, -3, -9 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_LOG_10_TEST'
  write ( *, '(a)' ) '  I4_LOG_10: whole part of log base 10,'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  X, I4_LOG_10'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    x = x_test(test)
    write ( *, '( 2x, i8, i12 )' ) x, i4_log_10 ( x )
  end do

  return
end
subroutine i4vec_print_test ( )

!*****************************************************************************80
!
!! I4VEC_PRINT_TEST tests I4VEC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), dimension ( n ) :: a = (/ &
    91, 92, 93, 94 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_PRINT_TEST'
  write ( *, '(a)' ) '  I4VEC_PRINT prints an I4VEC'

  call i4vec_print ( n, a, '  The I4VEC:' )

  return
end
subroutine r8_sign_test ( )

!*****************************************************************************80
!
!! R8_SIGN_TEST tests R8_SIGN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 5

  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_sign
  real ( kind = 8 ), parameter, dimension ( test_num ) :: r8_test = (/ &
    -1.25D+00, -0.25D+00, 0.0D+00, +0.5D+00, +9.0D+00 /)
  real ( kind = 8 ) s
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_SIGN_TEST'
  write ( *, '(a)' ) '  R8_SIGN returns the sign of an R8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    R8    R8_SIGN(R8)'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    r8 = r8_test(test)
    s = r8_sign ( r8 )
    write ( *, '(2x,f8.4,2x,f8.0)' ) r8, s
  end do

  return
end
subroutine r8_uniform_01_test ( )

!*****************************************************************************80
!
!! R8_UNIFORM_01_TEST tests R8_UNIFORM_01
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ), parameter :: n = 1000
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R8_UNIFORM_01 samples a uniform random'
  write ( *, '(a)' ) '  distribution in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Starting with seed = ', seed

  do i = 1, n
    x(i) = r8_uniform_01 ( seed )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  First few values:'
  write ( *, '(a)' ) ' '
  do i = 1, 5
    write ( *, '(2x,i8,2x,g14.6)' ) i, x(i)
  end do

  mean = 0.0D+00
  do i = 1, n
    mean = mean + x(i)
  end do
  mean = mean / real ( n, kind = 8 )
 
  variance = 0.0D+00
  do i = 1, n
    variance = variance + ( x(i) - mean ) ** 2
  end do
  variance = variance / real ( n, kind = 8 )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of values computed was N = ', n
  write ( *, '(a,g14.6)' ) '  Average value was ', mean
  write ( *, '(a,g14.6)' ) '  Minimum value was ', minval ( x(1:n) )
  write ( *, '(a,g14.6)' ) '  Maximum value was ', maxval ( x(1:n) )
  write ( *, '(a,g14.6)' ) '  Variance was ', variance

  return
end
subroutine r8col_swap_test ( )

!*****************************************************************************80
!
!! R8COL_SWAP_TEST tests R8COL_SWAP;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) icol1
  integer ( kind = 4 ) icol2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_SWAP_TEST'
  write ( *, '(a)' ) '  R8COL_SWAP swaps two columns of an R8COL;'

  call r8ge_indicator ( m, n, a )

  call r8ge_print ( m, n, a, '  The array:' )

  icol1 = 1
  icol2 = 3

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,a,i8)' ) '  Swap columns ', icol1, ' and ', icol2

  call r8col_swap ( m, n, a, icol1, icol2 )

  call r8ge_print ( m, n, a, '  The updated matrix:' )

  return
end
subroutine r8ge_cg_test ( )

!*****************************************************************************80
!
!! R8GE_CG_TEST tests R8GE_CG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) e_norm
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ) r_norm
  real ( kind = 8 ) r8vec_norm_affine
  real ( kind = 8 ) r8vec_norm
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8GE_CG_TEST'
  write ( *, '(a)' ) '  R8GE_CG applies CG to an R8GE matrix.'

  seed = 123456789
  n = 10
!
!  Let A be the -1 2 -1 matrix.
!
  allocate ( a(n,1:n) )
  call r8ge_dif2 ( n, n, a )
!
!  Choose a random solution.
!
  allocate ( x1(1:n) )
  call r8vec_uniform_01 ( n, seed, x1 )
!
!  Compute the corresponding right hand side.
!
  allocate ( b(1:n) )
  call r8ge_mv ( n, n, a, x1, b )
!
!  Call the CG routine.
!
  allocate ( x2(1:n) )
  x2(1:n) = 1.0D+00
  call r8ge_cg ( n, a, b, x2 )
!
!  Compute the residual.
!
  allocate ( r(1:n) )
  call r8ge_res ( n, n, a, x2, b, r )
  r_norm = r8vec_norm ( n, r )
!
!  Compute the error.
!
  e_norm = r8vec_norm_affine ( n, x1, x2 )
!
!  Report.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  Number of variables N = ', n
  write ( *, '(a,g14.6)' ) '  Norm of residual ||Ax-b|| = ', r_norm
  write ( *, '(a,g14.6)' ) '  Norm of error ||x1-x2|| = ', e_norm
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( b )
  deallocate ( r )
  deallocate ( x1 )
  deallocate ( x2 )

  return
end
subroutine r8ge_co_test ( )

!*****************************************************************************80
!
!! R8GE_CO_TEST tests R8GE_CO.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a_inverse(n,n)
  real ( kind = 8 ) a_inverse_norm_l1
  real ( kind = 8 ) a_norm_l1
  real ( kind = 8 ) cond_l1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) pivot(n)
  real ( kind = 8 ) rcond
  real ( kind = 8 ) :: x = 2.0D+00
  real ( kind = 8 ) :: y = 3.0D+00
  real ( kind = 8 ) z(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_CO_TEST'
  write ( *, '(a)' ) '  R8GE_CO estimates the condition number.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = x + y
      else
        a(i,j) = y
      end if
    end do
  end do

  a_norm_l1 = 0.0D+00
  do j = 1, n
    a_norm_l1 = max ( a_norm_l1, sum ( abs ( a(1:n,j) ) ) )
  end do

  a_inverse(1:n,1:n) = a(1:n,1:n)
  call r8ge_fa ( n, a_inverse, pivot, info )
  call r8ge_inverse ( n, a_inverse, pivot )

  a_inverse_norm_l1 = 0.0D+00
  do j = 1, n
    a_inverse_norm_l1 = max ( a_inverse_norm_l1, &
      sum ( abs ( a_inverse(1:n,j) ) ) )
  end do

  cond_l1 = a_norm_l1 * a_inverse_norm_l1

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The L1 condition number is ', cond_l1
!
!  Factor the matrix.
!
  call r8ge_co ( n, a, pivot, rcond, z )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The R8GE_CO estimate is     ', 1.0D+00 / rcond

  return
end
subroutine r8ge_det_test ( )

!*****************************************************************************80
!
!! R8GE_DET_TEST tests R8GE_DET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) det
  real ( kind = 8 ) exact
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) pivot(n)
  real ( kind = 8 ) :: x = 2.0D+00
  real ( kind = 8 ) :: y = 3.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_DET_TEST'
  write ( *, '(a)' ) '  R8GE_DET computes the determinant of an R8GE matrix.'
!
!  Set the matrix.
!
  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = x + y
      else
        a(i,j) = y
      end if
    end do
  end do
!
!  Factor the matrix.
!
  call r8ge_fa ( n, a, pivot, info )
!
!  Compute the determinant.
!
  call r8ge_det ( n, a, pivot, det )

  exact = x ** ( n - 1 ) * ( x + real ( n, kind = 8 ) * y )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  R8GE_DET computes the determinant = ', det
  write ( *, '(a,g14.6)' ) '  Exact determinant =                ', exact

  return
end
subroutine r8ge_dif2_test ( )

!*****************************************************************************80
!
!! R8GE_DIF2_TEST tests R8GE_DIF2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 7
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_DIF2_TEST'
  write ( *, '(a)' ) '  R8GE_DIF2 sets up the second difference matrix.'

  call r8ge_dif2 ( m, n, a )

  call r8ge_print ( m, n, a, '  The second difference matrix:' )

  return
end
subroutine r8ge_dilu_test ( )

!*****************************************************************************80
!
!! R8GE_DILU_TEST tests R8GE_DILU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: ncol = 3
  integer ( kind = 4 ), parameter :: nrow = 3
  integer ( kind = 4 ), parameter :: n = nrow * ncol
  integer ( kind = 4 ), parameter :: m = n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) d(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_DILU_TEST'
  write ( *, '(a)' ) '  R8GE_DILU returns the DILU factors of an R8GE matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n

  do i = 1, nrow * ncol
    do j = 1, nrow * ncol

      if ( i == j ) then
        a(i,j) = 4.0D+00
      else if ( i == j + 1 .or. i == j - 1 .or. &
                i == j + nrow .or. i == j - nrow ) then
        a(i,j) = -1.0D+00
      else
        a(i,j) = 0.0D+00
      end if

    end do
  end do

  call r8ge_print ( m, n, a, '  Matrix A:' )
!
!  Compute the incomplete LU factorization.
!
  call r8ge_dilu ( m, n, a, d )

  call r8vec_print ( m, d, '  DILU factor:' )

  return
end
subroutine r8ge_fa_test ( )

!*****************************************************************************80
!
!! R8GE_FA_TEST tests R8GE_FA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) job
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_FA_TEST'
  write ( *, '(a)' ) '  R8GE_FA computes the LU factors of a matrix, so'
  write ( *, '(a)' ) '  that R8GE_SL can solve the factored system.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8ge_random ( n, n, seed, a )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  call r8ge_mv ( n, n, a, x, b )
!
!  Factor the matrix.
!
  call r8ge_fa ( n, a, pivot, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8GE_FA_TEST - Fatal error!'
    write ( *, '(a)' ) '  R8GE_FA declares the matrix is singular!'
    write ( *, '(a,i8)' ) '  The value of INFO is ', info
    return
  end if
!
!  Solve the linear system.
!
  job = 0
  call r8ge_sl ( n, a, pivot, b, job )
 
  call r8vec_print ( n, b, '  Solution:' )
!
!  Set the desired solution.
!
  x(1:n) = 1.0D+00
!
!  Compute the corresponding right hand side.
!
  job = 0
  call r8ge_ml ( n, a, pivot, x, b, job )
!
!  Solve the system
!
  job = 0
  call r8ge_sl ( n, a, pivot, b, job )

  call r8vec_print ( n, b, '  Solution:' )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  job = 1
  call r8ge_ml ( n, a, pivot, x, b, job )
!
!  Solve the system
!
  job = 1
  call r8ge_sl ( n, a, pivot, b, job )

  call r8vec_print ( n, b, '  Solution of transposed system:' )

  return
end
subroutine r8ge_fs_test ( )

!*****************************************************************************80
!
!! R8GE_FS_TEST tests R8GE_FS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_FS_TEST'
  write ( *, '(a)' ) '  R8GE_FS factors and solves a linear system'
  write ( *, '(a)' ) '  for a general matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8ge_random ( n, n, seed, a )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  call r8ge_mv ( n, n, a, x, b )
!
!  Factor and solve the system.
!
  call r8ge_fs ( n, a, b, info )
  
  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST34 - Fatal error!'
    write ( *, '(a)' ) '  R8GE_FS reports the matrix is singular.'
    return
  end if

  call r8vec_print ( n, b, '  Solution:' )

  return
end
subroutine r8ge_fss_test ( )

!*****************************************************************************80
!
!! R8GE_FSS_TEST tests R8GE_FSS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 June 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10
  integer ( kind = 4 ), parameter :: nb = 3

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,nb)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_FSS_TEST'
  write ( *, '(a)' ) '  For a matrix in general storage,'
  write ( *, '(a)' ) '  R8GE_FSS factors and solves multiple linear systems.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8ge_random ( n, n, seed, a )
!
!  Set the desired solutions.
!
  x(1:n) = 1.0D+00
  call r8ge_mv ( n, n, a, x, b(1:n,1) )

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do
  call r8ge_mv ( n, n, a, x, b(1:n,2) )

  do i = 1, n
    x(i) = real ( 1 + mod ( i - 1, 3 ), kind = 8 )
  end do
  call r8ge_mv ( n, n, a, x, b(1:n,3) )
!
!  Factor and solve the system.
!
  call r8ge_fss ( n, a, nb, b, info )
  
  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8GE_FSS_TEST - Warning!'
    write ( *, '(a)' ) '  R8GE_FSS reports the matrix is singular.'
    return
  end if

  call r8ge_print ( n, nb, b, '  Solutions:' )

  return
end
subroutine r8ge_hilbert_test ( )

!*****************************************************************************80
!
!! R8GE_HILBERT_TEST tests R8GE_HILBERT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 7
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_HILBERT_TEST'
  write ( *, '(a)' ) '  R8GE_HILBERT sets up the Hilbert matrix.'

  call r8ge_hilbert ( m, n, a )

  call r8ge_print ( m, n, a, '  The Hilbert matrix:' )

  return
end
subroutine r8ge_hilbert_inverse_test ( )

!*****************************************************************************80
!
!! R8GE_HILBERT_INVERSE_TEST tests R8GE_HILBERT_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_HILBERT_INVERSE_TEST'
  write ( *, '(a)' ) '  R8GE_HILBERT_INVERSE sets up the Hilbert matrix inverse.'

  call r8ge_hilbert ( n, n, a )
  call r8ge_print ( n, n, a, '  The Hilbert matrix A:' )

  call r8ge_hilbert_inverse ( n, b )
  call r8ge_print ( n, n, b, '  The inverse Hilbert matrix B:' )

  call r8ge_mm ( n, n, n, a, b, c )
  call r8ge_print ( n, n, c, '  C = A * B:' )

  return
end
subroutine r8ge_identity_test ( )

!*****************************************************************************80
!
!! R8GE_IDENTITY_TEST tests R8GE_IDENTITY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 7
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_IDENTITY_TEST'
  write ( *, '(a)' ) '  R8GE_IDENTITY sets up the identity matrix.'

  call r8ge_identity ( m, n, a )

  call r8ge_print ( m, n, a, '  The R8GE identity matrix:' )

  return
end
subroutine r8ge_ilu_test ( )

!*****************************************************************************80
!
!! R8GE_ILU_TEST tests R8GE_ILU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: ncol = 3
  integer ( kind = 4 ), parameter :: nrow = 3
  integer ( kind = 4 ), parameter :: n = nrow * ncol
  integer ( kind = 4 ), parameter :: m = n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) l(m,m)
  real ( kind = 8 ) lu(m,n)
  real ( kind = 8 ) u(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_ILU_TEST'
  write ( *, '(a)' ) '  For a matrix in general storage,'
  write ( *, '(a)' ) '  R8GE_ILU returns the ILU factors.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n

  do i = 1, nrow * ncol
    do j = 1, nrow * ncol

      if ( i == j ) then
        a(i,j) = 4.0D+00
      else if ( i == j + 1 .or. i == j - 1 .or. &
                i == j + nrow .or. i == j - nrow ) then
        a(i,j) = -1.0D+00
      else
        a(i,j) = 0.0D+00
      end if

    end do
  end do

  call r8ge_print ( m, n, a, '  Matrix A:' )
!
!  Compute the incomplete LU factorization.
!
  call r8ge_ilu ( m, n, a, l, u )

  call r8ge_print ( m, m, l, '  Factor L:' )

  call r8ge_print ( m, n, u, '  Factor U:' )

  lu(1:m,1:n) = matmul ( l(1:m,1:m), u(1:m,1:n) )

  call r8ge_print ( m, n, lu, '  Product L*U:' )

  return
end
subroutine r8ge_indicator_test ( )

!*****************************************************************************80
!
!! R8GE_INDICATOR_TEST tests R8GE_INDICATOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 7
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_INDICATOR_TEST'
  write ( *, '(a)' ) '  R8GE_INDICATOR sets up the indicator matrix.'

  call r8ge_indicator ( m, n, a )

  call r8ge_print ( m, n, a, '  The R8GE indicator matrix:' )

  return
end
subroutine r8ge_inverse_test ( )

!*****************************************************************************80
!
!! R8GE_INVERSE_TEST tests R8GE_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) pivot(n)
  real ( kind = 8 ), parameter :: x = 2.0D+00
  real ( kind = 8 ), parameter :: y = 3.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_INVERSE_TEST'
  write ( *, '(a)' ) '  For a matrix in general storage,'
  write ( *, '(a)' ) '  R8GE_INVERSE computes the inverse matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  do j = 1, n
    do i = 1, n
      if ( i == j ) then
        a(i,i) = x + y
      else
        a(i,j) = y
      end if
    end do
  end do

  call r8ge_print ( n, n, a, '  Matrix A:' )
!
!  Factor and invert the matrix.
!
  b(1:n,1:n) = a(1:n,1:n)

  call r8ge_fa ( n, b, pivot, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8GE_INVERSE_TEST - Warning!'
    write ( *, '(a)' ) '  R8GE_FA reports the matrix is singular.'
    return
  end if

  call r8ge_inverse ( n, b, pivot )

  call r8ge_print ( n, n, b, '  Inverse matrix B:' )
!
!  Check.
!
  call r8ge_mm ( n, n, n, a, b, c )

  call r8ge_print ( n, n, c, '  Product matrix:' )

  return
end
subroutine r8ge_ml_test ( )

!*****************************************************************************80
!
!! R8GE_ML_TEST tests R8GE_ML.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) b2(n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) job
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_ML_TEST'
  write ( *, '(a)' ) '  For a matrix in general storage,'
  write ( *, '(a)' ) '  R8GE_ML computes A*x or A''*X'
  write ( *, '(a)' ) '  where A has been factored by R8GE_FA.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n

  do job = 0, 1
!
!  Set the matrix.
!
    call r8ge_random ( n, n, seed, a )
!
!  Set the desired solution.
!
    call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
    if ( job == 0 ) then
      call r8ge_mv ( n, n, a, x, b )
    else
      call r8ge_mtv ( n, n, a, x, b )
    end if
!
!  Factor the matrix.
!
    call r8ge_fa ( n, a, pivot, info )

    if ( info /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8GE_ML_TEST - Warning!'
      write ( *, '(a)' ) '  R8GE_FA declares the matrix is singular!'
      write ( *, '(a,i8)' ) '  The value of INFO is ', info
      return
    end if
!
!  Now multiply factored matrix times solution to get right hand side again.
!
    call r8ge_ml ( n, a, pivot, x, b2, job )

    if ( job == 0 ) then
      call r8vec2_print_some ( n, b, b2, 10, '  A*x and PLU*x' )
    else
      call r8vec2_print_some ( n, b, b2, 10, '  A''*x and (PLU)''*x' )
    end if

  end do

  return
end
subroutine r8ge_mm_test ( )

!*****************************************************************************80
!
!! R8GE_MM_TEST tests R8GE_MM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3
  integer ( kind = 4 ), parameter :: n3 = 4

  real ( kind = 8 ) a(n1,n2)
  real ( kind = 8 ) b(n2,n3)
  real ( kind = 8 ) c(n1,n3)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8GE_MM_TEST'
  write ( *, '(a)' ) '  R8GE_MM computes a matrix-matrix product C = A * B.'

  do i = 1, n1
    do j = 1, n2

      if ( j == 1 ) then
        a(i,j) = 1.0D+00
      else if ( i == 1 ) then
        a(i,j) = 0.0D+00
      else
        a(i,j) = a(i-1,j-1) + a(i-1,j)
      end if

    end do
  end do

  b = transpose ( a )

  call r8ge_mm ( n1, n2, n3, a, b, c )

  call r8ge_print ( n1, n2, a, '  A:' )
  call r8ge_print ( n2, n3, b, '  B:' )
  call r8ge_print ( n1, n3, c, '  C = A*B:' )

  return
end
subroutine r8ge_mtm_test ( )

!*****************************************************************************80
!
!! R8GE_MTM_TEST tests R8GE_MTM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3

  real ( kind = 8 ) a(n1,n2)
  real ( kind = 8 ) b(n1,n2)
  real ( kind = 8 ) c(n2,n2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8GE_MTM_TEST'
  write ( *, '(a)' ) '  R8GE_MTM computes a matrix-transpose-matrix product C = A'' * B.'

  do i = 1, n1
    do j = 1, n2

      if ( j == 1 ) then
        a(i,j) = 1.0D+00
      else if ( i == 1 ) then
        a(i,j) = 0.0D+00
      else
        a(i,j) = a(i-1,j-1) + a(i-1,j)
      end if

    end do
  end do

  b = a

  call r8ge_mtm ( n2, n1, n2, a, b, c )

  call r8ge_print ( n1, n2, a, '  A:' )
  call r8ge_print ( n1, n2, b, '  B:' )
  call r8ge_print ( n2, n2, c, '  C = A''*B:' )

  return
end
subroutine r8ge_mtv_test ( )

!*****************************************************************************80
!
!! R8GE_MTV_TEST tests R8GE_MTV
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8GE_MTV_TEST'
  write ( *, '(a)' ) '  R8GE_MTV computes a product b=A''*x for an R8GE matrix.'

  call r8ge_indicator ( m, n, a )
  call r8ge_print ( m, n, a, '  The R8GE matrix A:' )

  call r8vec_indicator1 ( m, x )
  call r8vec_print ( m, x, '  Vector x:' )

  call r8ge_mtv ( m, n, a, x, b )
  call r8vec_print ( n, b, '  Vector b = A''*x:' )

  return
end
subroutine r8ge_mu_test ( )

!*****************************************************************************80
!
!! R8GE_MU_TEST tests R8GE_MU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) amn(m,n)
  real ( kind = 8 ) anm(n,m)
  real ( kind = 8 ) bm(m)
  real ( kind = 8 ) bn(n)
  real ( kind = 8 ) cm(m)
  real ( kind = 8 ) cn(n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) job
  integer ( kind = 4 ) pivot(m+n)
  integer ( kind = 4 ) :: seed = 123456789
  character trans
  real ( kind = 8 ) xm(m)
  real ( kind = 8 ) xn(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_MU_TEST'
  write ( *, '(a)' ) '  For a matrix in general storage,'
  write ( *, '(a)' ) '  R8GE_MU computes A*x or A''*X'
  write ( *, '(a)' ) '  where A has been factored by R8GE_TRF.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n

  do job = 0, 1

    if ( job == 0 ) then 
      trans = 'N'
    else
      trans = 'T'
    end if
!
!  Set the matrix.
!
    call r8ge_random ( m, n, seed, amn )

    if ( job == 0 ) then

      call r8vec_indicator1 ( n, xn )

      call r8ge_mv ( m, n, amn, xn, cm )

    else

      call r8vec_indicator1 ( m, xm )

      call r8ge_mtv ( m, n, amn, xm, cn )

    end if
!
!  Factor the matrix.
!
    call r8ge_trf ( m, n, amn, pivot, info )

    if ( info /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8GE_MU_TEST - Warning!'
      write ( *, '(a)' ) '  R8GE_TRF declares the matrix is singular!'
      write ( *, '(a,i8)' ) '  The value of INFO is ', info
      cycle
    end if
!
!  Now multiply factored matrix times solution to get right hand side again.
!
    if ( job == 0 ) then

      call r8ge_mu ( m, n, amn, trans, pivot, xn, bm )

      call r8vec2_print_some ( m, cm, bm, 10, '  A*x and PLU*x' )

    else

      call r8ge_mu ( m, n, amn, trans, pivot, xm, bn )

      call r8vec2_print_some ( n, cn, bn, 10, '  A''*x and (PLU)''*x' )

    end if

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,a,i8)' ) '  Matrix is ', n, ' by ', m

  do job = 0, 1

    if ( job == 0 ) then 
      trans = 'N'
    else
      trans = 'T'
    end if
!
!  Set the matrix.
!
    call r8ge_random ( n, m, seed, anm )

    if ( job == 0 ) then

      call r8vec_indicator1 ( m, xm )

      call r8ge_mv ( n, m, anm, xm, cn )

    else

      call r8vec_indicator1 ( n, xn )

      call r8ge_mtv ( n, m, anm, xn, cm )

    end if
!
!  Factor the matrix.
!
    call r8ge_trf ( n, m, anm, pivot, info )

    if ( info /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8GE_MU_TEST - Warning!'
      write ( *, '(a)' ) '  R8GE_TRF declares the matrix is singular!'
      write ( *, '(a)' ) '  The value of INFO is ', info
      cycle
    end if
!
!  Now multiply factored matrix times solution to get right hand side again.
!
    if ( job == 0 ) then

      call r8ge_mu ( n, m, anm, trans, pivot, xm, bn )

      call r8vec2_print_some ( n, cn, bn, 10, '  A*x and PLU*x' )

    else

      call r8ge_mu ( n, m, anm, trans, pivot, xn, bm )

      call r8vec2_print_some ( m, cm, bm, 10, '  A''*x and (PLU)''*x' )

    end if

  end do

  return
end
subroutine r8ge_mv_test ( )

!*****************************************************************************80
!
!! R8GE_MV_TEST tests R8GE_MV
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(m)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8GE_MV_TEST'
  write ( *, '(a)' ) '  R8GE_MV computes a product b=A*x for an R8GE matrix.'

  call r8ge_indicator ( m, n, a )
  call r8ge_print ( m, n, a, '  The R8GE matrix A:' )

  call r8vec_indicator1 ( n, x )
  call r8vec_print ( n, x, '  Vector x:' )

  call r8ge_mv ( m, n, a, x, b )
  call r8vec_print ( m, b, '  Vector b = A*x:' )

  return
end
subroutine r8ge_plu_test ( )

!*****************************************************************************80
!
!! R8GE_PLU_TEST tests R8GE_PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) l(m,m)
  real ( kind = 8 ) p(m,m)
  real ( kind = 8 ) plu(m,n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u(m,n)

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_PLU_TEST'
  write ( *, '(a)' ) '  R8GE_PLU returns the PLU factors of a matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n

  call r8ge_random ( m, n, seed, a )

  call r8ge_print ( m, n, a, '  Matrix A:' )
!
!  Compute the PLU factors.
!
  call r8ge_plu ( m, n, a, p, l, u )

  call r8ge_print ( m, m, p, '  Factor P:' )

  call r8ge_print ( m, m, l, '  Factor L:' )

  call r8ge_print ( m, n, u, '  Factor U:' )

  plu(1:m,1:n) = matmul ( p(1:m,1:m), &
                 matmul ( l(1:m,1:m), u(1:m,1:n) ) )
        
  call r8ge_print ( m, n, plu, '  Product P*L*U:')

  return
end
subroutine r8ge_poly_test ( )

!*****************************************************************************80
!
!! R8GE_POLY_TEST tests R8GE_POLY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 12

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) p(0:n)
  real ( kind = 8 ) p_true(0:n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_POLY_TEST'
  write ( *, '(a)' ) '  R8GE_POLY computes the characteristic polynomial.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n

  p_true(0:12) = &   
       (/ 1.0D+00,    - 23.0D+00,     231.0D+00,  - 1330.0D+00,    4845.0D+00, &
    - 11628.0D+00,   18564.0D+00, - 19448.0D+00,   12870.0D+00,  - 5005.0D+00, &
       1001.0D+00,    - 78.0D+00,       1.0D+00 /)
!
!  Set the matrix.
!
  do i = 1, n
    do j = 1, n
      a(i,j) = real ( min ( i, j ), kind = 8 )
    end do
  end do
!
!  Get the characteristic polynomial.
!
  call r8ge_poly ( n, a, p )
!
!  Compare.
!
  call r8vec2_print_some ( n + 1, p, p_true, 10, 'I, P(I), True P(I)' )

  return
end
subroutine r8ge_print_test ( )

!*****************************************************************************80
!
!! R8GE_PRINT_TEST tests R8GE_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_PRINT_TEST'
  write ( *, '(a)' ) '  R8GET_PRINT prints an R8GE matrix.'

  do j = 1, n
    do i = 1, m
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8ge_print ( m, n, a, '  The matrix:' )

  return
end
subroutine r8ge_print_some_test ( )

!*****************************************************************************80
!
!! R8GE_PRINT_SOME_TEST tests R8GE_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8GE_PRINT_SOME prints some of an R8GE matrix.'

  do j = 1, n
    do i = 1, m
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8ge_print_some ( m, n, a, 2, 1, 4, 2, &
    '  The matrix, rows 2:4, cols 1:2:' )

  return
end
subroutine r8ge_random_test ( )

!*****************************************************************************80
!
!! R8GE_RANDOM_TEST tests R8GE_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8GE_RANDOM_TEST'
  write ( *, '(a)' ) '  R8GE_RANDOM returns a random R8GE matrix.'

  seed = 123456789

  call r8ge_random ( m, n, seed, a )

  call r8ge_print ( m, n, a, '  Random matrix:' )

  return
end
subroutine r8ge_res_test ( )

!*****************************************************************************80
!
!! R8GE_RES_TEST tests R8GE_RES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8GE_RES_TEST'
  write ( *, '(a)' ) '  R8GE_RES computes b-A*x, where A is an R8GE matrix.'
  write ( *, '(a)' ) '  We check three cases, M<N, M=N, M>N.'

  do i = 1, 3

    if ( i == 1 ) then
      m = 3
      n = 5
    else if ( i == 2 ) then
      m = 5
      n = 5
    else if ( i == 3 ) then
      m = 5
      n = 3
    end if

    allocate ( a(1:m,1:n) )
    allocate ( b(1:m) )
    allocate ( r(1:m) )
    allocate ( x(1:n) )

    seed = 123456789
    call r8ge_random ( m, n, seed, a )
    call r8vec_indicator1 ( n, x )
    call r8ge_mv ( m, n, a, x, b )
    call r8ge_res ( m, n, a, x, b, r )
    call r8vec_print ( m, r, '  Residual A*x-b:' )

    deallocate ( a )
    deallocate ( b )
    deallocate ( r )
    deallocate ( x )

  end do

  return
end
subroutine r8ge_sl_test ( )

!*****************************************************************************80
!
!! R8GE_SL_TEST tests R8GE_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) job
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_SL_TEST'
  write ( *, '(a)' ) '  R8GE_SL solves a system after it has been'
  write ( *, '(a)' ) '  factored by R8GE_FA.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8ge_random ( n, n, seed, a )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  call r8ge_mv ( n, n, a, x, b )
!
!  Factor the matrix.
!
  call r8ge_fa ( n, a, pivot, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST30 - Fatal error!'
    write ( *, '(a)' ) '  R8GE_FA declares the matrix is singular!'
    write ( *, '(a,i8)' ) '  The value of INFO is ', info
    return
  end if
!
!  Solve the linear system.
!
  job = 0
  call r8ge_sl ( n, a, pivot, b, job )
 
  call r8vec_print ( n, b, '  Solution:' )
!
!  Set the desired solution.
!
  x(1:n) = 1.0D+00
!
!  Compute the corresponding right hand side.
!
  job = 0
  call r8ge_ml ( n, a, pivot, x, b, job )
!
!  Solve the system
!
  job = 0
  call r8ge_sl ( n, a, pivot, b, job )

  call r8vec_print ( n, b, '  Solution:' )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  job = 1
  call r8ge_ml ( n, a, pivot, x, b, job )
!
!  Solve the system
!
  job = 1
  call r8ge_sl ( n, a, pivot, b, job )

  call r8vec_print ( n, b, '  Solution of transposed system:' )

  return
end
subroutine r8ge_sl_it_test ( )

!*****************************************************************************80
!
!! R8GE_SL_IT_TEST tests R8GE_SL_IT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) alu(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) job
  integer ( kind = 4 ) pivot(n)
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_SL_IT_TEST'
  write ( *, '(a)' ) '  R8GE_SL_IT applies one step of iterative '
  write ( *, '(a)' ) '  refinement to an R8GE_SL solution.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the coefficient matrix.
!
  call r8ge_hilbert_inverse ( n, a )
!
!  Set the right hand side b.
!
  b(1:n-1) = 0.0D+00
  b(n) = 1.0D+00
!
!  It is necessary to keep both an unfactored and factored copy
!  of the coefficient matrix.
!
  alu(1:n,1:n) = a(1:n,1:n)
!
!  Compute the factored coefficient matrix.
!
  call r8ge_fa ( n, alu, pivot, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8GE_SL_IT_TEST - Warning!'
    write ( *, '(a)' ) '  R8GE_FA declares the matrix is singular!'
    write ( *, '(a)' ) '  The value of INFO is ', info
    return
  end if
!
!  Solve the system.
!  (Careful!  R8GE_SL overwrites the right hand side with the solution!)
!
  x(1:n) = b(1:n)

  call r8ge_sl ( n, alu, pivot, x, job )
!
!  Compute and print the residual.
!
  call r8ge_res ( n, n, a, x, b, r )

  call r8vec2_print_some ( n, x, r, 10, '  i, x, b-A*x' )
!
!  Take a few steps of iterative refinement.
!
  do j = 1, 5

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) 'Iterative refinement step ', j
    write ( *, '(a)' ) ' '
!
!  Improve the solution.
!
    call r8ge_sl_it ( n, a, alu, pivot, b, job, x, r )

    call r8vec_print_some ( n, r, 10, '  I, DX:' )
!
!  Compute and print the residual.
!
    call r8ge_res ( n, n, a, x, b, r )

    call r8vec2_print_some ( n, x, r, 10, '  i, x, b-A*x' )

  end do

  return
end
subroutine r8ge_to_r8vec_test ( )

!*****************************************************************************80
!
!! R8GE_TO_R8VEC_TEST tests R8GE_TO_R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(m*n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_TO_R8VEC_TEST'
  write ( *, '(a)' ) '  R8GE_TO_R8VEC converts an R8GE matrix to an R8VEC.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n

  call r8ge_indicator ( m, n, a )

  call r8ge_print ( m, n, a, '  The R8GE indicator matrix:' )

  call r8ge_to_r8vec ( m, n, a, x )

  k = 0
  do j = 1, n
    do i = 1, m
      k = k + 1
      write ( *, '(3i8,g14.6)' ) i, j, k, x(k)
    end do
  end do

  return
end
subroutine r8ge_transpose_print_test ( )

!*****************************************************************************80
!
!! R8GE_TRANSPOSE_PRINT_TEST tests R8GE_TRANSPOSE_PRINT;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 7
  integer ( kind = 4 ), parameter :: n = 12

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_TRANSPOSE_PRINT_TEST'
  write ( *, '(a)' ) '  R8GE_TRANSPOSE_PRINT prints an R8GE matrix,'
  write ( *, '(a)' ) '  transposed.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix row order M =    ', m
  write ( *, '(a,i8)' ) '  Matrix column order N = ', n
!
!  Set the matrix.
!
  do i = 1, m
    do j = 1, n
      a(i,j) = real ( i * 100 + j, kind = 8 )
    end do
  end do

  call r8ge_transpose_print ( m, n, a, '  The transposed matrix A:' )

  return
end
subroutine r8ge_transpose_print_some_test ( )

!*****************************************************************************80
!
!! R8GE_TRANSPOSE_PRINT_SOME_TEST tests R8GE_TRANSPOSE_PRINT_SOME;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 7
  integer ( kind = 4 ), parameter :: n = 12

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_TRANSPOSE_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8GE_TRANSPOSE_PRINT_SOME prints some of an R8GE'
  write ( *, '(a)' ) '  matrix, transposed.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix row order M =    ', m
  write ( *, '(a,i8)' ) '  Matrix column order N = ', n
!
!  Set the matrix.
!
  do i = 1, m
    do j = 1, n
      a(i,j) = real ( i * 100 + j, kind = 8 )
    end do
  end do

  call r8ge_transpose_print_some ( m, n, a, 3, 4, 5, 8, '  Rows 3:5, Cols 4:8' )

  return
end
subroutine r8ge_trf_test ( )

!*****************************************************************************80
!
!! R8GE_TRF_TEST tests R8GE_TRF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: m = n
  integer ( kind = 4 ), parameter :: nrhs = 1

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,nrhs)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) pivot(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_TRF_TEST'
  write ( *, '(a)' ) '  R8GE_TRF computes the LU factors of an R8GE matrix'
  write ( *, '(a)' ) '  so that R8GE_TRS can solve the factored system.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Number of matrix columns N = ', n

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0D+00
      else if ( i == j - 1 ) then
        a(i,j) = - 1.0D+00
      else if ( i == j + 1 ) then
        a(i,j) = - 1.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8ge_trf ( m, n, a, pivot, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8GE_TRF_TEST - Warning!'
    write ( *, '(a)' ) '  R8GE_TRF declares the matrix is singular!'
    write ( *, '(a)' ) '  The value of INFO is ', info
    return
  end if

  b(1:n-1,1) = 0.0D+00
  b(n,1) = n + 1

  call r8ge_trs ( n, nrhs, 'N', a, pivot, b, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8GE_TRF_TEST - Warning!'
    write ( *, '(a)' ) '  R8GE_TRS returned an error condition!'
    write ( *, '(a)' ) '  The value of INFO is ', info
    return
  end if

  call r8vec_print ( n, b, '  Solution:' )

  b(1:n-1,1) = 0.0D+00
  b(n,1) = n + 1

  call r8ge_trs ( n, nrhs, 'T', a, pivot, b, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8GE_TRF_TEST - Warning!'
    write ( *, '(a)' ) '  R8GE_TRS returned an error condition!'
    write ( *, '(a)' ) '  The value of INFO is ', info
    return
  end if

  call r8vec_print ( n, b, '  Solution to transposed system:' )

  return
end
subroutine r8ge_trs_test ( )

!*****************************************************************************80
!
!! R8GE_TRS_TEST tests R8GE_TRS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: m = n
  integer ( kind = 4 ), parameter :: nrhs = 1

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,nrhs)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) pivot(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_TRS_TEST'
  write ( *, '(a)' ) '  R8GE_TRS solves a linear system that has been'
  write ( *, '(a)' ) '  factored by R8GE_TRF.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Number of matrix columns N = ', n

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0D+00
      else if ( i == j - 1 ) then
        a(i,j) = - 1.0D+00
      else if ( i == j + 1 ) then
        a(i,j) = - 1.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8ge_trf ( m, n, a, pivot, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8GE_TRS_TEST - Warning!'
    write ( *, '(a)' ) '  R8GE_TRF declares the matrix is singular!'
    write ( *, '(a)' ) '  The value of INFO is ', info
    return
  end if

  b(1:n-1,1) = 0.0D+00
  b(n,1) = n + 1

  call r8ge_trs ( n, nrhs, 'N', a, pivot, b, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8GE_TRS_TEST - Warning!'
    write ( *, '(a)' ) '  R8GE_TRS returned an error condition!'
    write ( *, '(a)' ) '  The value of INFO is ', info
    return
  end if

  call r8vec_print ( n, b, '  Solution:' )

  b(1:n-1,1) = 0.0D+00
  b(n,1) = n + 1

  call r8ge_trs ( n, nrhs, 'T', a, pivot, b, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8GE_TRS_TEST - Warning!'
    write ( *, '(a)' ) '  R8GE_TRS returned an error condition!'
    write ( *, '(a)' ) '  The value of INFO is ', info
    return
  end if

  call r8vec_print ( n, b, '  Solution to transposed system:' )

  return
end
subroutine r8ge_zeros_test ( )

!*****************************************************************************80
!
!! R8GE_ZEROS_TEST tests R8GE_ZEROS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_ZEROS_TEST'
  write ( *, '(a)' ) '  R8GE_ZEROS zeros out a matrix in R8GE format.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,i8)' ) '  Matrix order M, N = ', m, n

  call r8ge_zeros ( m, n, a )

  call r8ge_print ( m, n, a, '  The zero R8GE matrix:' )
 
  return
end
subroutine r8row_swap_test ( )

!*****************************************************************************80
!
!! R8ROW_SWAP_TEST tests R8ROW_SWAP;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) row1
  integer ( kind = 4 ) row2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_SWAP_TEST'
  write ( *, '(a)' ) '  For a R8ROW (a matrix regarded as rows):'
  write ( *, '(a)' ) '  R8ROW_SWAP swaps two rows;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8ge_print ( m, n, a, '  The original matrix:' )

  row1 = 1
  row2 = 3

  write ( *, '(a)' ) ' '
  write ( *, '(a,i3,a,i3)' ) '  Swap rows ', row1, ' and ', row2

  call r8row_swap ( m, n, a, row1, row2 )

  call r8ge_print ( m, n, a, '  The modified matrix:' )

  return
end
subroutine r8vec_indicator1_test ( )

!*****************************************************************************80
!
!! R8VEC_INDICATOR1_TEST tests R8VEC_INDICATOR1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_INDICATOR1_TEST'
  write ( *, '(a)' ) '  R8VEC_INDICATOR returns the indicator vector.'

  call r8vec_indicator1 ( n, x )

  call r8vec_print ( n, x, '  Vector X:' )

  return
end
subroutine r8vec_norm_test ( )

!*****************************************************************************80
!
!! R8VEC_NORM_TEST tests R8VEC_NORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) r8vec_norm
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_NORM_TEST'
  write ( *, '(a)' ) '  R8VEC_NORM computes the L2 norm of an R8VEC.'

  seed = 123456789

  call r8vec_uniform_01 ( n, seed, x )

  call r8vec_print ( n, x, '  Vector X:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  R8VEC_NORM ( X ) = ', r8vec_norm ( n, x )

  return
end
subroutine r8vec_norm_affine_test ( )

!*****************************************************************************80
!
!! R8VEC_NORM_AFFINE_TEST tests R8VEC_NORM_AFFINE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) r8vec_norm
  real ( kind = 8 ) r8vec_norm_affine
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_NORM_AFFINE_TEST'
  write ( *, '(a)' ) '  R8VEC_NORM_AFFINE computes the L2 norm of '
  write ( *, '(a)' ) '  the difference of two R8VECs.'

  seed = 123456789

  call r8vec_uniform_01 ( n, seed, x )
  call r8vec_uniform_01 ( n, seed, y )
  z(1:n) = x(1:n) - y(1:n)

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  R8VEC_NORM_AFFINE(X,Y) = ', r8vec_norm_affine ( n, x, y )
  write ( *, '(a,g14.6)' ) '  R8VEC_NORM (X-Y):        ', r8vec_norm ( n, z )

  return
end
subroutine r8vec_print_test ( )

!*****************************************************************************80
!
!! R8VEC_PRINT_TEST tests R8VEC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ), dimension ( n ) :: a = (/ &
    123.456D+00, 0.000005D+00, -1.0D+06, 3.14159265D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC_PRINT prints an R8VEC.'

  call r8vec_print ( n, a, '  The R8VEC:' )

  return
end
subroutine r8vec_print_some_test ( )

!*****************************************************************************80
!
!! R8VEC_PRINT_SOME_TEST tests R8VEC_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 100

  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, 100
    x(i) = ( real ( i, kind = 8 ) ) ** 2
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8VEC_PRINT_SOME prints some of a pair of R8VEC''s.'

  call r8vec_print_some ( n, x, 10, '  No more than 10 lines from an R8VEC:' );

  return
end
subroutine r8vec_to_r8ge_test ( )

!*****************************************************************************80
!
!! R8VEC_TO_R8GE_TEST tests R8VEC_TO_R8GE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a_r8ge(m,n)
  real ( kind = 8 ) a_r8vec(m*n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8GE_TO_R8VEC_TEST'
  write ( *, '(a)' ) '  R8GE_TO_R8VEC converts an R8GE matrix to an R8VEC vector.'

  k = 0
  do j = 1, n
    do i = 1, m
      k = k + 1
      a_r8vec(k) = real ( k, kind = 8 )
    end do
  end do

  call r8vec_print ( m * n, a_r8vec, '  Corresponding R8VEC vector:' );

  call r8vec_to_r8ge ( m, n, a_r8vec, a_r8ge )

  call r8ge_print ( m, n, a_r8ge, '  R8GE matrix:' )

  return
end
subroutine r8vec_uniform_01_test ( )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01_TEST tests R8VEC_UNIFORM_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) r(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_01 returns a random R8VEC '
  write ( *, '(a)' ) '  with entries in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Input SEED = ', seed

  call r8vec_uniform_01 ( n, seed, r )

  call r8vec_print ( n, r, '  Random R8VEC:' )

  return
end
subroutine r8vec2_print_some_test ( )

!*****************************************************************************80
!
!! R8VEC2_PRINT_SOME_TEST tests R8VEC2_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 100

  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1, 100
    x(i) = ( real ( i, kind = 8 ) ) ** 2
    y(i) = sqrt ( real ( i, kind = 8 ) )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC2_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8VEC2_PRINT_SOME prints some of a pair of R8VEC''s.'

  call r8vec2_print_some ( n, x, y, 10, &
    '  No more than 10 lines from a pair of R8VEC''s:' );

  return
end

