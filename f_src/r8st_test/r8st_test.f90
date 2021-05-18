program main

!*****************************************************************************80
!
!! MAIN is the main program for r8st_TEST.
!
!  Discussion:
!
!    r8st_TEST tests r8st.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Test r8st.'

  call r8st_cg_test ( )
  call r8st_diagonal_test ( )
  call r8st_dif2_test ( )
  call r8st_ij_to_k_test ( )
  call r8st_indicator_test ( )
  call r8st_jac_sl_test ( )
  call r8st_mtv_test ( )
  call r8st_mv_test ( )
  call r8st_print_test ( )
  call r8st_random_test ( )
  call r8st_read_test ( )
  call r8st_res_test ( )
  call r8st_to_r8ge_test ( )
  call r8st_to_r8ncf_test ( )
  call r8st_write_test ( )
  call r8st_zeros_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine r8st_cg_test ( )

!*****************************************************************************80
!
!! r8st_CG_TEST tests r8st_CG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:)
  real ( kind = 8 ), allocatable :: b(:)
  integer ( kind = 4 ), allocatable :: col(:)
  real ( kind = 8 ) err
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nz_num
  real ( kind = 8 ), allocatable :: r(:)
  integer ( kind = 4 ), allocatable :: row(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_CG_TEST'
  write ( *, '(a)' ) '  r8st_CG applies the conjugate gradient method'
  write ( *, '(a)' ) '  to a linear system with r8st matrix.'

  m = 50
  n = m
  nz_num = 3 * n - 2

  allocate ( a(nz_num) )
  allocate ( row(nz_num ) )
  allocate ( col(nz_num ) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros =  ', nz_num

  call r8st_dif2 ( m, n, nz_num, row, col, a )

  call r8st_print_some ( m, n, nz_num, row, col, a, 1, 1, 10, 10, &
    '  Rows 1:10, Cols 1:10:' )
!
!  Set the desired solution.
!
  allocate ( x(1:n) )
  call r8vec_indicator1 ( n, x )
!
!  Compute the right hand side.
!
  allocate ( b(1:n) )
  call r8st_mv ( m, n, nz_num, row, col, a, x, b )
!
!  Set the approximate solution.
!
  x(1:n) = 1.0D+00
!
!  Call the conjugate gradient method.
!
  call r8st_cg ( n, nz_num, row, col, a, b, x )
!
!  Compute the residual, A*x-b
!
  allocate ( r(1:n) )
  call r8st_res ( m, n, nz_num, row, col, a, x, b, r )
 
  err = maxval ( abs ( r(1:n) ) )
 
  call r8vec_print_some ( n, x, 10, '  Solution:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Maximum residual = ', err
 
  deallocate ( a )
  deallocate ( b )
  deallocate ( col )
  deallocate ( r )
  deallocate ( row )
  deallocate ( x )

  return
end
subroutine r8st_diagonal_test ( )

!*****************************************************************************80
!
!! r8st_DIAGONAL_TEST tests r8st_DIAGONAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nz_num = 20

  real ( kind = 8 ) a(nz_num)
  integer ( kind = 4 ), dimension ( nz_num ) :: col = (/ &
    5, 6, 2, 2, 3, 4, 4, 5, 1, 6, &
    4, 6, 5, 1, 6, 3, 1, 2, 1, 3 /)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( nz_num ) :: row = (/ &
    1, 3, 4, 6, 5, 2, 6, 3, 1, 2, &
    4, 6, 5, 4, 4, 3, 6, 2, 3, 4 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_DIAGONAL_TEST'
  write ( *, '(a)' ) '  r8st_DIAGONAL rearranges an r8st matrix'
  write ( *, '(a)' ) '  so that the diagonal is listed first.'

  m = 6
  n = 6

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order M =         ', m
  write ( *, '(a,i8)' ) '  Matrix order N =         ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros NZ_NUM = ', nz_num

  call r8st_indicator ( m, n, nz_num, row, col, a )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Before rearrangement:'
  write ( *, '(a)' ) '       K  ROW(K)  COL(K)      A(K)'
  write ( *, '(a)' ) ' '
  do k = 1, nz_num
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) k, row(k), col(k), a(k)
  end do

  call r8st_diagonal ( m, n, nz_num, row, col, a )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  After rearrangement:'
  write ( *, '(a)' ) '       K  ROW(K)  COL(K)      A(K)'
  write ( *, '(a)' ) ' '
  do k = 1, nz_num
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) k, row(k), col(k), a(k)
  end do

  return
end
subroutine r8st_dif2_test ( )

!*****************************************************************************80
!
!! r8st_DIF2_TEST tests r8st_DIF2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:)
  integer ( kind = 4 ), allocatable :: col(:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nz_num
  integer ( kind = 4 ), allocatable :: row(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_DIF2_TEST'
  write ( *, '(a)' ) '  r8st_DIF2 sets up the second difference as'
  write ( *, '(a)' ) '  an r8st matrix;'

  m = 7
  n = 5
  if ( m == n ) then
    nz_num = 3 * min ( m, n ) - 2
  else
    nz_num = 3 * min ( m, n ) - 1
  end if

  allocate ( a(nz_num) )
  allocate ( row(nz_num ) )
  allocate ( col(nz_num ) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros =  ', nz_num

  call r8st_dif2 ( m, n, nz_num, row, col, a )

  call r8st_print ( m, n, nz_num, row, col, a, '  The r8st matrix:' )

  deallocate ( a )
  deallocate ( col )
  deallocate ( row )

  return
end
subroutine r8st_ij_to_k_test ( )

!*****************************************************************************80
!
!! r8st_IJ_TO_K_TEST tests r8st_IJ_TO_K.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 7
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: nz_num = 10

  logical check
  integer ( kind = 4 ), dimension ( nz_num ) :: col = (/ &
    2, 5, 1, 5, 1, 2, 3, 4, 4, 1 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ), dimension ( nz_num ) :: row = (/ &
    1, 1, 2, 2, 4, 4, 4, 5, 6, 7 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_IJ_TO_K_TEST'
  write ( *, '(a)' ) '  r8st_IJ_TO_K returns the r8st index of (I,J).'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros =  ', nz_num

  call r8st_check ( m, n, nz_num, row, col, check )

  if ( .not. check ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'r8st_CHECK - Error!'
    write ( *, '(a)' ) '  The matrix is not in the proper sorted format.'
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I         J         K'
  write ( *, '(a)' ) ' '
  do i = 1, m
    do j = 1, n
      call r8st_ij_to_k ( nz_num, row, col, i, j, k )
      write ( *, '(2x,i8,2x,i8,2x,i8)' ) i, j, k
    end do
  end do

  return
end
subroutine r8st_indicator_test ( )

!*****************************************************************************80
!
!! r8st_INDICATOR_TEST tests r8st_INDICATOR.
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
  integer ( kind = 4 ), parameter :: nz_num = 10

  real ( kind = 8 ), dimension ( nz_num ) :: a
  integer ( kind = 4 ), dimension ( nz_num ) :: col = (/ &
    2, 5, 1, 5, 1, 2, 3, 4, 4, 1 /)
  integer ( kind = 4 ), dimension ( nz_num ) :: row = (/ &
    1, 1, 2, 2, 4, 4, 4, 5, 6, 7 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_INDICATOR_TEST'
  write ( *, '(a)' ) '  r8st_INDICATOR sets up a r8st indicator matrix;'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros =  ', nz_num

  call r8st_indicator ( m, n, nz_num, row, col, a )

  call r8st_print ( m, n, nz_num, row, col, a, '  The r8st indicator matrix:' )

  return
end
subroutine r8st_jac_sl_test ( )

!*****************************************************************************80
!
!! r8st_JAC_SL_TEST tests r8st_JAC_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nz_max = 30

  real ( kind = 8 ) a(nz_max)
  real ( kind = 8 ), allocatable :: b(:)
  integer ( kind = 4 ) col(nz_max)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nz_num
  integer ( kind = 4 ) row(nz_max)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_JAC_SL_TEST'
  write ( *, '(a)' ) '  r8st_JAC_SL uses Jacobi iteration to solve a linear system'
  write ( *, '(a)' ) '  with an r8st matrix.'

  m = 10
  n = 10
  nz_num = 3 * n - 2
  it_max = 25

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order M =         ', m
  write ( *, '(a,i8)' ) '  Matrix order N =         ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros NZ_NUM = ', nz_num
  write ( *, '(a,i8)' ) '  Iterations per call    = ', it_max
!
!  Set the matrix values.
!
  call r8st_dif2 ( m, n, nz_num, row, col, a )
!
!  Set the desired solution.
!
  allocate ( x(1:n) )

  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  allocate ( b(1:n) )

  call r8st_mv ( n, n, nz_num, row, col, a, x, b )
!
!  Set the starting solution.
!
  x(1:n) = 0.0D+00
!
!  Solve the linear system.
!
  do i = 1, 3

    call r8st_jac_sl ( n, nz_num, row, col, a, b, x, it_max )

    call r8vec_print ( n, x, '  Current solution estimate:' )

  end do

  deallocate ( b )
  deallocate ( x )

  return
end
subroutine r8st_mtv_test ( )

!*****************************************************************************80
!
!! r8st_MTV_TEST tests r8st_MTV.
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
  integer ( kind = 4 ), parameter :: nz_num = 10

  real ( kind = 8 ), dimension ( nz_num ) :: a
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c(m,n)
  integer ( kind = 4 ), dimension ( nz_num ) :: col = (/ &
    2, 5, 1, 5, 1, 2, 3, 4, 4, 1 /)
  integer ( kind = 4 ), dimension ( nz_num ) :: row = (/ &
    1, 1, 2, 2, 4, 4, 4, 5, 6, 7 /)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(m)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_MTV_TEST'
  write ( *, '(a)' ) '  r8st_MTV computes b=A''*x, where A is an r8st matrix;'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros =  ', nz_num
!
!  Set the matrix.
!
  call r8st_random ( m, n, nz_num, row, col, seed, a )
!
!  Make an R8GE copy.
!
  call r8st_to_r8ge ( m, n, nz_num, row, col, a, c )
!
!  Print the R8GE copy.
!
  call r8ge_print ( m, n, c, '  The r8st matrix, in R8GE form:' )

  x(1) = 1.0D+00
  x(2:m-1) = 0.0D+00
  x(m) = -1.0D+00

  call r8vec_print ( m, x, '  The vector x:' )

  call r8st_mtv ( m, n, nz_num, row, col, a, x, b )

  call r8vec_print ( n, b, '  The product A'' * x:' )

  return
end
subroutine r8st_mv_test ( )

!*****************************************************************************80
!
!! r8st_MV_TEST tests r8st_MV.
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
  integer ( kind = 4 ), parameter :: nz_num = 10

  real ( kind = 8 ), dimension ( nz_num ) :: a
  real ( kind = 8 ) b(m)
  real ( kind = 8 ) c(m,n)
  integer ( kind = 4 ), dimension ( nz_num ) :: col = (/ &
    2, 5, 1, 5, 1, 2, 3, 4, 4, 1 /)
  integer ( kind = 4 ), dimension ( nz_num ) :: row = (/ &
    1, 1, 2, 2, 4, 4, 4, 5, 6, 7 /)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_MV_TEST'
  write ( *, '(a)' ) '  r8st_MV multiplies an r8st matrix by a vector;'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros =  ', nz_num
!
!  Set the matrix.
!
  call r8st_random ( m, n, nz_num, row, col, seed, a )
!
!  Make an R8GE copy.
!
  call r8st_to_r8ge ( m, n, nz_num, row, col, a, c )
!
!  Print the R8GE copy.
!
  call r8ge_print ( m, n, c, '  The r8st matrix, in R8GE form:' )

  x(1) = 1.0D+00
  x(2:n-1) = 0.0D+00
  x(n) = -1.0D+00

  call r8vec_print ( n, x, '  The vector x:' )

  call r8st_mv ( m, n, nz_num, row, col, a, x, b )

  call r8vec_print ( m, b, '  The product A * x:' )

  return
end
subroutine r8st_print_test ( )

!*****************************************************************************80
!
!! r8st_PRINT_TEST tests r8st_PRINT.
!
!  Discussion:
!
!    Because MATLAB seems to allow a r8st matrix to store the same index
!    several times, presumably with the matrix entry being the SUM of
!    these occurrences, I modified r8st_PRINT to handle this situation
!    (I hope).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 7
  integer ( kind = 4 ), parameter :: nz_num = 12

  real ( kind = 8 ), dimension ( nz_num ) :: a = (/ &
    21.0D+00,  51.0D+00, 12.0D+00, 52.0D+00, 14.0D+00, &
    24.0D+00,  34.0D+00, 45.0D+00, 46.0D+00, 17.0D+00, &
   100.0D+00, 200.0D+00 /)
  integer ( kind = 4 ), dimension ( nz_num ) :: col = (/ &
    1, 1, 2, 2, 4, 4, 4, 5, 6, 7, 2, 4 /)
  integer ( kind = 4 ), dimension ( nz_num ) :: row = (/ &
    2, 5, 1, 5, 1, 2, 3, 4, 4, 1, 1, 3 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_PRINT_TEST'
  write ( *, '(a)' ) '  r8st_PRINT prints a r8st matrix;'
  write ( *, '(a)' ) '  In this example, we have listed several matrix'
  write ( *, '(a)' ) '  locations TWICE.  r8st_PRINT should compute the'
  write ( *, '(a)' ) '  sum of these values.' 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In particular, we want A(1,2) = 112 and A(3,4) = 234.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros =  ', nz_num

  call r8st_print ( m, n, nz_num, row, col, a, '  The r8st matrix:' )

  return
end
subroutine r8st_print_some_test ( )

!*****************************************************************************80
!
!! r8st_PRINT_SOME_TEST tests r8st_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nz_max = 20

  real ( kind = 8 ) a(nz_max)
  integer ( kind = 4 ) col(nz_max)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nz_num
  integer ( kind = 4 ) row(nz_max)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  r8st_PRINT_SOME prints some of an r8st matrix.'

  m = 5
  n = 5
  nz_num = 3 * n - 2

  call r8st_dif2 ( m, n, nz_num, row, col, a )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order M =         ', m
  write ( *, '(a,i8)' ) '  Matrix order N =         ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros NZ_NUM = ', nz_num

  call r8st_print_some ( m, n, nz_num, row, col, a, 2, 3, 4, 5, &
    '  Rows 2:4, Cols 3:5:' )

  return
end
subroutine r8st_random_test ( )

!*****************************************************************************80
!
!! r8st_RANDOM_TEST tests r8st_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 7
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: nz_num = 10

  real ( kind = 8 ), dimension ( nz_num ) :: a
  integer ( kind = 4 ), dimension ( nz_num ) :: col = (/ &
    2, 5, 1, 5, 1, 2, 3, 4, 4, 1 /)
  integer ( kind = 4 ), dimension ( nz_num ) :: row = (/ &
    1, 1, 2, 2, 4, 4, 4, 5, 6, 7 /)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_RANDOM_TEST'
  write ( *, '(a)' ) '  r8st_RANDOM randomizes a r8st indicator matrix;'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros =  ', nz_num

  seed = 123456789

  call r8st_random ( m, n, nz_num, row, col, seed, a )

  call r8st_print ( m, n, nz_num, row, col, a, '  The r8st matrix:' )

  return
end
subroutine r8st_read_test ( )

!*****************************************************************************80
!
!! r8st_READ_TEST tests r8st_READ.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( : ) :: a
  integer ( kind = 4 ), allocatable, dimension ( : ) :: col
  character ( len = 80 ) input_file
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nz_num
  integer ( kind = 4 ), allocatable, dimension ( : ) :: row

  input_file = 'r8st_matrix.txt'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_READ_TEST'
  write ( *, '(a)' ) '  r8st_READ_SIZE reads the size of an r8st matrix.'
  write ( *, '(a)' ) '  r8st_READ reads an r8st matrix from a file.'

  call r8st_read_size ( input_file, m, n, nz_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  r8st_READ_SIZE reports matrix size data:'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =          ', m
  write ( *, '(a,i8)' ) '  Matrix columns N =       ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros NZ_NUM = ', nz_num

  allocate ( row(1:nz_num) )
  allocate ( col(1:nz_num) )
  allocate ( a(1:nz_num) )

  call r8st_read ( input_file, m, n, nz_num, row, col, a )

  call r8st_print_some ( m, n, nz_num, row, col, a, 1, 1, &
    10, 10, '  Initial 10x10 block of recovered r8st matrix:' )

  deallocate ( row )
  deallocate ( col )
  deallocate ( a )

  return
end
subroutine r8st_res_test ( )

!*****************************************************************************80
!
!! r8st_RES_TEST tests r8st_RES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:)
  real ( kind = 8 ), allocatable :: b(:)
  integer ( kind = 4 ), allocatable :: col(:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nz_num
  real ( kind = 8 ), allocatable :: r(:)
  integer ( kind = 4 ), allocatable :: row(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_RES_TEST'
  write ( *, '(a)' ) '  r8st_RES computes r=b-A*x, where A is an R8S3 matrix.'

  m = 5
  n = 4
  if ( m == n ) then
    nz_num = 3 * n - 2
  else
    nz_num = 3 * n - 1
  end if

  allocate ( row(1:nz_num) )
  allocate ( col(1:nz_num) )
  allocate ( a(1:nz_num) )

  call r8st_dif2 ( m, n, nz_num, row, col, a )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order M =         ', m
  write ( *, '(a,i8)' ) '  Matrix order N =         ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros NZ_NUM = ', nz_num

  allocate ( x(1:n) )
  call r8vec_indicator1 ( n, x )
  call r8vec_print ( n, x, '  x:' )

  allocate ( b(1:m) )
  call r8st_mv ( m, n, nz_num, row, col, a, x, b )

  allocate ( r(1:m) )
  call r8st_res ( m, n, nz_num, row, col, a, x, b, r )

  call r8vec_print ( m, r, '  r=b-A*x:' )

  deallocate ( a )
  deallocate ( b )
  deallocate ( col )
  deallocate ( r )
  deallocate ( row )
  deallocate ( x )

  return
end
subroutine r8st_to_r8ge_test ( )

!*****************************************************************************80
!
!! r8st_TO_R8GE_TEST tests r8st_TO_R8GE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a_r8ge(:,:)
  real ( kind = 8 ), allocatable :: a_r8st(:)
  integer ( kind = 4 ), allocatable :: col(:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nz_num
  integer ( kind = 4 ), allocatable :: row(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_TO_R8GE_TEST'
  write ( *, '(a)' ) '  r8st_TO_R8GE converts an r8st matrix to R8GE format.'

  m = 7
  n = 5
  nz_num = min ( m, n )

  if ( m == n ) then
    nz_num = 3 * min ( m, n ) - 2
  else
    nz_num = 3 * min ( m, n ) - 1
  end if

  allocate ( a_r8st(nz_num) )
  allocate ( row(nz_num ) )
  allocate ( col(nz_num ) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros =  ', nz_num

  call r8st_dif2 ( m, n, nz_num, row, col, a_r8st )

  call r8st_print ( m, n, nz_num, row, col, a_r8st, '  The r8st matrix:' )

  allocate ( a_r8ge(m,n) )

  call r8st_to_r8ge ( m, n, nz_num, row, col, a_r8st, a_r8ge ) 

  call r8ge_print ( m, n, a_r8ge, '  The R8GE matrix:' )

  deallocate ( a_r8ge )
  deallocate ( a_r8st )
  deallocate ( col )
  deallocate ( row )

  return
end
subroutine r8st_to_r8ncf_test ( )

!*****************************************************************************80
!
!! r8st_TO_R8NCF_TEST tests r8st_TO_R8NCF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:)
  integer ( kind = 4 ), allocatable :: col(:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nz_num
  integer ( kind = 4 ), allocatable :: row(:)
  integer ( kind = 4 ), allocatable :: rowcol(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_TO_R8NCF_TEST'
  write ( *, '(a)' ) '  r8st_TO_R8NCF converts an r8st matrix to R8NCF format.'

  m = 7
  n = 5
  nz_num = min ( m, n )

  if ( m == n ) then
    nz_num = 3 * min ( m, n ) - 2
  else
    nz_num = 3 * min ( m, n ) - 1
  end if

  allocate ( a(nz_num) )
  allocate ( row(nz_num ) )
  allocate ( col(nz_num ) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros =  ', nz_num

  call r8st_dif2 ( m, n, nz_num, row, col, a )

  call r8st_print ( m, n, nz_num, row, col, a, '  The r8st matrix:' )

  allocate ( rowcol(2,nz_num) )

  call r8st_to_r8ncf ( m, n, nz_num, row, col, a, rowcol ) 

  call r8ncf_print ( m, n, nz_num, rowcol, a, '  The R8NCF matrix:' )

  deallocate ( a )
  deallocate ( col )
  deallocate ( row )
  deallocate ( rowcol )

  return
end
subroutine r8st_write_test ( )

!*****************************************************************************80
!
!! r8st_WRITE_TEST tests r8st_WRITE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 100
  integer ( kind = 4 ), parameter :: n = 100
  integer ( kind = 4 ), parameter :: nz_num = 3 * n - 2

  real ( kind = 8 ) a(nz_num)
  integer ( kind = 4 ) col(nz_num)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character ( len = 80 ) output_file
  integer ( kind = 4 ) row(nz_num)

  output_file = 'r8st_matrix.txt'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_WRITE_TEST'
  write ( *, '(a)' ) '  r8st_WRITE writes an r8st matrix to a file.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =          ', m
  write ( *, '(a,i8)' ) '  Matrix columns N =       ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros NZ_NUM = ', nz_num
!
!  Set the matrix values.
!
  k = 0
  do i = 1, n
    k = k + 1
    j = i
    row(k) = i
    col(k) = j
    a(k) = real ( 100 * i + j, kind = 8 )
  end do

  do i = 2, n
    j = i - 1
    k = k + 1
    row(k) = i
    col(k) = j
    a(k) = real ( 100 * i + j, kind = 8 )
  end do

  do i = 1, n-1
    j = i + 1
    k = k + 1
    row(k) = i
    col(k) = j
    a(k) = real ( 100 * i + j, kind = 8 )
  end do

  call r8st_print_some ( m, n, nz_num, row, col, a, 1, 1, &
    10, 10, '  Initial 10x10 block of R8S3 matrix:' )

  call r8st_write ( m, n, nz_num, row, col, a, output_file )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  r8st_WRITE wrote the matrix data to "' &
    // trim ( output_file ) // '".'

  return
end
subroutine r8st_zeros_test ( )

!*****************************************************************************80
!
!! r8st_ZEROS_TEST tests r8st_ZEROS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 7
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: nz_num = 10

  real ( kind = 8 ), dimension ( nz_num ) :: a
  integer ( kind = 4 ), dimension ( nz_num ) :: col = (/ &
    2, 5, 1, 5, 1, 2, 3, 4, 4, 1 /)
  integer ( kind = 4 ), dimension ( nz_num ) :: row = (/ &
    1, 1, 2, 2, 4, 4, 4, 5, 6, 7 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'r8st_ZEROS_TEST'
  write ( *, '(a)' ) '  r8st_ZEROS zeros up a r8st indicator matrix;'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n
  write ( *, '(a,i8)' ) '  Matrix nonzeros =  ', nz_num

  call r8st_zeros ( m, n, nz_num, row, col, a )

  call r8st_print ( m, n, nz_num, row, col, a, '  The r8st zero matrix:' )

  return
end
