program main

!*****************************************************************************80
!
!! MAIN is the main program for R8UT_TEST.
!
!  Discussion:
!
!    R8UT_TEST tests the R8UT library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Test the R8UT library.'

  call r8ge_to_r8ut_test ( )

  call r8ut_det_test ( )
  call r8ut_indicator_test ( )
  call r8ut_inverse_test ( )
  call r8ut_mm_test ( )
  call r8ut_mtm_test ( )
  call r8ut_mtv_test ( )
  call r8ut_mv_test ( )
  call r8ut_print_test ( )
  call r8ut_print_some_test ( )
  call r8ut_random_test ( )
  call r8ut_sl_test ( )
  call r8ut_slt_test ( )
  call r8ut_to_r8ge_test ( )
  call r8ut_zeros_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine r8ge_to_r8ut_test ( )

!*****************************************************************************80
!
!! R8GE_TO_R8UT_TEST tests R8GE_TO_R8UT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a_ge(m,n)
  real ( kind = 8 ) a_ut(m,n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_TO_R8UT_TEST'
  write ( *, '(a)' ) '  R8GE_TO_R8UT converts an R8GE matrix to R8UT format.'

  call r8ge_random ( m, n, seed, a_ge )

  call r8ge_print ( m, n, a_ge, '  The random R8GE matrix:' )

  call r8ge_to_r8ut ( m, n, a_ge, a_ut )

  call r8ut_print ( m, n, a_ut, '  The R8UT matrix' )

  return
end
subroutine r8ut_det_test ( )

!*****************************************************************************80
!
!! R8UT_DET_TEST tests R8UT_DET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_DET_TEST'
  write ( *, '(a)' ) '  R8UT_DET computes the determinant of an R8UT matrix.'

  call r8ut_random ( n, n, seed, a )

  call r8ut_print ( n, n, a, '  The matrix A:' )
!
!  Compute the determinant.
!
  call r8ut_det ( n, a, det )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Determinant is ', det

  return
end
subroutine r8ut_indicator_test ( )

!*****************************************************************************80
!
!! R8UT_INDICATOR_TEST tests R8UT_INDICATOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 August 2015
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
  write ( *, '(a)' ) 'R8UT_INDICATOR_TEST'
  write ( *, '(a)' ) '  R8UT_INDICATOR sets up an indicator matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n

  call r8ut_indicator ( m, n, a )

  call r8ut_print ( m, n, a, '  The R8UT indicator matrix:' )

  return
end
subroutine r8ut_inverse_test ( )

!*****************************************************************************80
!
!! R8UT_INVERSE_TEST tests R8UT_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_INVERSE_TEST'
  write ( *, '(a)' ) '  R8UT_INVERSE computes the inverse of an R8UT matrix.'

  call r8ut_random ( n, n, seed, a )

  call r8ut_print ( n, n, a, '  The matrix A:' )
!
!  Compute the inverse matrix B.
!
  b(1:n,1:n) = a(1:n,1:n)

  call r8ut_inverse ( n, b )

  call r8ut_print ( n, n, b, '  The inverse matrix B:' )
!
!  C = A * B.
!
  call r8ut_mm ( n, a, b, c )

  call r8ut_print ( n, n, c, '  The product C = A * B:' )

  return
end
subroutine r8ut_mm_test ( )

!*****************************************************************************80
!
!! R8UT_MM_TEST tests R8UT_MM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_MM_TEST'
  write ( *, '(a)' ) '  R8UT_MM computes C = A * B for R8UT matrices.'
 
  call r8ut_zeros ( n, n, a )
  do i = 1, n 
    do j = i, n
      a(i,j) = 1.0D+00
    end do
  end do
  call r8ut_print ( n, n, a, '  The matrix A:' )

  call r8ut_mm ( n, a, a, c )
  call r8ut_print ( n, n, c, '  The product C = A * A' )

  return
end
subroutine r8ut_mtm_test ( )

!*****************************************************************************80
!
!! R8UT_MTM_TEST tests R8UT_MTM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_MTM_TEST'
  write ( *, '(a)' ) '  R8UT_MTM computes C = A'' * B for R8UT matrices.'
 
  call r8ut_zeros ( n, n, a )
  do i = 1, n 
    do j = i, n
      a(i,j) = 1.0D+00
    end do
  end do
  call r8ut_print ( n, n, a, '  The matrix A:' )

  call r8ut_mtm ( n, a, a, c )
  call r8ge_print ( n, n, c, '  The product C = A'' * A' )

  return
end
subroutine r8ut_mtv_test ( )

!*****************************************************************************80
!
!! R8UT_MTV_TEST tests R8UT_MTV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 August 2015
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_MTV_TEST'
  write ( *, '(a)' ) '  R8UT_MTV computes A''*x for an R8UT matrix.'

  call r8ut_indicator ( m, n, a )
  call r8ut_print ( m, n, a, '  The matrix A:' )

  call r8vec_indicator1 ( m, x )
  call r8vec_print ( m, x, '  The vector x' )

  call r8ut_mtv ( m, n, a, x, b )
  call r8vec_print ( n, b, '  b = A''*x:' )

  return
end
subroutine r8ut_mv_test ( )

!*****************************************************************************80
!
!! R8UT_MV_TEST tests R8UT_MV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 August 2015
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_MV_TEST'
  write ( *, '(a)' ) '  R8UT_MV computes A*x for an R8UT matrix.'

  call r8ut_indicator ( m, n, a )
  call r8ut_print ( m, n, a, '  The matrix A:' )

  call r8vec_indicator1 ( n, x )
  call r8vec_print ( n, x, '  The vector x' )

  call r8ut_mv ( m, n, a, x, b )
  call r8vec_print ( m, b, '  b = A*x:' )

  return
end
subroutine r8ut_print_test ( )

!*****************************************************************************80
!
!! R8UT_PRINT_TEST tests R8UT_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_PRINT_TEST'
  write ( *, '(a)' ) '  R8UT_PRINT prints an R8UT matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n

  call r8ut_indicator ( m, n, a )

  call r8ut_print ( m, n, a, '  The R8UT matrix:' )

  return
end
subroutine r8ut_print_some_test ( )

!*****************************************************************************80
!
!! R8UT_PRINT_SOME_TEST tests R8UT_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8UT_PRINT_SOME prints some of an R8UT matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n

  call r8ut_indicator ( m, n, a )

  call r8ut_print_some ( m, n, a, 1, 4, 3, 6, '  Some of the matrix:' )

  return
end
subroutine r8ut_random_test ( )

!*****************************************************************************80
!
!! R8UT_RANDOM_TEST tests R8UT_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 August 2015
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_RANDOM_TEST'
  write ( *, '(a)' ) '  R8UT_RANDOM randomizes an R8UT matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,i8)' ) '  Matrix order M, N = ', m, n

  seed = 123456789
  call r8ut_random ( m, n, seed, a )

  call r8ut_print ( m, n, a, '  The matrix:' )
 
  return
end
subroutine r8ut_sl_test ( )

!*****************************************************************************80
!
!! R8UT_SL_TEST tests R8UT_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_SL_TEST'
  write ( *, '(a)' ) '  R8UT_SL solves A*x=b for an upper triangular A.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n

  do i = 1, n
    do j = 1, n
      if ( i <= j ) then
        a(i,j) = real ( j, kind = 8 )
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8ut_print ( n, n, a, '  The upper triangular matrix:' )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  call r8ut_mv ( n, n, a, x, b )
!
!  Solve the linear system.
!
  call r8ut_sl ( n, a, b, x )

  call r8vec_print ( n, x, '  Solution:' )

  return
end
subroutine r8ut_slt_test ( )

!*****************************************************************************80
!
!! R8UT_SLT_TEST tests R8UT_SLT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_SLT_TEST'
  write ( *, '(a)' ) '  R8UT_SLT solves A''*x=b for an upper triangular A.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n

  do i = 1, n
    do j = 1, n
      if ( i <= j ) then
        a(i,j) = real ( j, kind = 8 )
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8ut_print ( n, n, a, '  The upper triangular matrix:' )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  call r8ut_mtv ( n, n, a, x, b )
!
!  Solve the linear system.
!
  call r8ut_slt ( n, a, b, x )

  call r8vec_print ( n, x, '  Solution to transposed system:' )

  return
end
subroutine r8ut_to_r8ge_test ( )

!*****************************************************************************80
!
!! R8UT_TO_R8GE_TEST tests R8UT_TO_R8GE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a_ge(m,n)
  real ( kind = 8 ) a_ut(m,n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8UT_TO_R8GE_TEST'
  write ( *, '(a)' ) '  R8UT_TO_R8GE converts an R8UT matrix to R8GE format.'

  call r8ut_random ( m, n, seed, a_ut )

  call r8ut_print ( m, n, a_ut, '  The random R8UT matrix:' )

  call r8ut_to_r8ge ( m, n, a_ut, a_ge )

  call r8ge_print ( m, n, a_ge, '  The R8GE matrix' )

  return
end
subroutine r8ut_zeros_test ( )

!*****************************************************************************80
!
!! R8UT_ZEROS_TEST tests R8UT_ZEROS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 August 2015
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
  write ( *, '(a)' ) 'R8UT_ZEROS_TEST'
  write ( *, '(a)' ) '  R8UT_ZEROS zeros out a matrix in R8UT format.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,i8)' ) '  Matrix order M, N = ', m, n

  call r8ut_zeros ( m, n, a )

  call r8ut_print ( m, n, a, '  The matrix:' )
 
  return
end

