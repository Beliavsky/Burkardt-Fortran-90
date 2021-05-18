program main

!*****************************************************************************80
!
!! MAIN is the main program for R8LT_TEST.
!
!  Discussion:
!
!    R8LT_TEST tests the R8LT library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8LT_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Test the R8LT library.'

  call r8ge_to_r8lt_test ( )

  call r8lt_det_test ( )
  call r8lt_indicator_test ( )
  call r8lt_inverse_test ( )
  call r8lt_mm_test ( )
  call r8lt_mtm_test ( )
  call r8lt_mtv_test ( )
  call r8lt_mv_test ( )
  call r8lt_print_test ( )
  call r8lt_print_some_test ( )
  call r8lt_random_test ( )
  call r8lt_sl_test ( )
  call r8lt_slt_test ( )
  call r8lt_to_r8ge_test ( )
  call r8lt_zeros_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8LT_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine r8ge_to_r8lt_test ( )

!*****************************************************************************80
!
!! R8GE_TO_R8LT_TEST tests R8GE_TO_R8LT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a_ge(m,n)
  real ( kind = 8 ) a_lt(m,n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8GE_TO_R8LT_TEST'
  write ( *, '(a)' ) '  R8GE_TO_R8LT converts an R8GE matrix to R8LT format.'

  call r8ge_random ( m, n, seed, a_ge )

  call r8ge_print ( m, n, a_ge, '  The random R8GE matrix:' )

  call r8ge_to_r8lt ( m, n, a_ge, a_lt )

  call r8lt_print ( m, n, a_lt, '  The R8LT matrix' )

  return
end
subroutine r8lt_det_test ( )

!*****************************************************************************80
!
!! R8LT_DET_TEST tests R8LT_DET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
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
  write ( *, '(a)' ) 'R8LT_DET_TEST'
  write ( *, '(a)' ) '  R8LT_DET computes the determinant of an R8LT matrix.'

  call r8lt_random ( n, n, seed, a )

  call r8lt_print ( n, n, a, '  The matrix A:' )
!
!  Compute the determinant.
!
  call r8lt_det ( n, a, det )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Determinant is ', det

  return
end
subroutine r8lt_indicator_test ( )

!*****************************************************************************80
!
!! R8LT_INDICATOR_TEST tests R8LT_INDICATOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8LT_INDICATOR_TEST'
  write ( *, '(a)' ) '  R8LT_INDICATOR sets up an indicator matrix in R8LT format;'

  call r8lt_indicator ( m, n, a )

  call r8lt_print ( m, n, a, '  The R8LT indicator matrix:' )

  return
end
subroutine r8lt_inverse_test ( )

!*****************************************************************************80
!
!! R8LT_INVERSE_TEST tests R8LT_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
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
  write ( *, '(a)' ) 'R8LT_INVERSE_TEST'
  write ( *, '(a)' ) '  R8LT_INVERSE computes the inverse of an R8LT matrix.'

  call r8lt_random ( n, n, seed, a )

  call r8lt_print ( n, n, a, '  The matrix A:' )
!
!  Compute the inverse matrix B.
!
  b(1:n,1:n) = a(1:n,1:n)

  call r8lt_inverse ( n, b )

  call r8lt_print ( n, n, b, '  The inverse matrix B:' )
!
!  C = A * B.
!
  call r8lt_mm ( n, a, b, c )

  call r8lt_print ( n, n, c, '  The product C = A * B:' )

  return
end
subroutine r8lt_mm_test ( )

!*****************************************************************************80
!
!! R8LT_MM_TEST tests R8LT_MM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
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
  write ( *, '(a)' ) 'R8LT_MM_TEST'
  write ( *, '(a)' ) '  R8LT_MM computes C = A * B for R8LT matrices.'
 
  call r8lt_zeros ( n, n, a )
  do i = 1, n 
    do j = 1, i
      a(i,j) = 1.0D+00
    end do
  end do
  call r8lt_print ( n, n, a, '  The matrix A:' )

  call r8lt_mm ( n, a, a, c )
  call r8lt_print ( n, n, c, '  The product C = A * A' )

  return
end
subroutine r8lt_mtm_test ( )

!*****************************************************************************80
!
!! R8LT_MTM_TEST tests R8LT_MTM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
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
  write ( *, '(a)' ) 'R8LT_MTM_TEST'
  write ( *, '(a)' ) '  R8LT_MTM computes C = A'' * B for R8LT matrices.'
 
  call r8lt_zeros ( n, n, a )
  do i = 1, n 
    do j = 1, i
      a(i,j) = 1.0D+00
    end do
  end do
  call r8lt_print ( n, n, a, '  The matrix A:' )

  call r8lt_mtm ( n, a, a, c )
  call r8ge_print ( n, n, c, '  The product C = A'' * A' )

  return
end
subroutine r8lt_mtv_test ( )

!*****************************************************************************80
!
!! R8LT_MTV_TEST tests R8LT_MTV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
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
  write ( *, '(a)' ) 'R8LT_MTV_TEST'
  write ( *, '(a)' ) '  R8LT_MTV computes A''*x for an R8LT matrix.'

  call r8lt_indicator ( m, n, a )
  call r8lt_print ( m, n, a, '  The matrix A:' )

  call r8vec_indicator1 ( m, x )
  call r8vec_print ( m, x, '  The vector x' )

  call r8lt_mtv ( m, n, a, x, b )
  call r8vec_print ( n, b, '  b = A''*x:' )

  return
end
subroutine r8lt_mv_test ( )

!*****************************************************************************80
!
!! R8LT_MV_TEST tests R8LT_MV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
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
  write ( *, '(a)' ) 'R8LT_MV_TEST'
  write ( *, '(a)' ) '  R8LT_MV computes A*x for an R8LT matrix.'

  call r8lt_indicator ( m, n, a )
  call r8lt_print ( m, n, a, '  The matrix A:' )

  call r8vec_indicator1 ( n, x )
  call r8vec_print ( n, x, '  The vector x' )

  call r8lt_mv ( m, n, a, x, b )
  call r8vec_print ( m, b, '  b = A*x:' )

  return
end
subroutine r8lt_print_test ( )

!*****************************************************************************80
!
!! R8LT_PRINT_TEST tests R8LT_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
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
  write ( *, '(a)' ) 'R8LT_PRINT_TEST'
  write ( *, '(a)' ) '  R8UT_PRINT prints an R8LT matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n

  call r8lt_indicator ( m, n, a )

  call r8lt_print ( m, n, a, '  The R8UT matrix:' )

  return
end
subroutine r8lt_print_some_test ( )

!*****************************************************************************80
!
!! R8LT_PRINT_SOME_TEST tests R8LT_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
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
  write ( *, '(a)' ) 'R8LT_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8LT_PRINT_SOME prints some of an R8LT matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix rows M =    ', m
  write ( *, '(a,i8)' ) '  Matrix columns N = ', n

  call r8lt_indicator ( m, n, a )

  call r8lt_print_some ( m, n, a, 2, 2, 3, 4, '  Rows 2-3, Cols 2-4:' )

  return
end
subroutine r8lt_random_test ( )

!*****************************************************************************80
!
!! R8LT_RANDOM_TEST tests R8LT_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8LT_RANDOM_TEST'
  write ( *, '(a)' ) '  R8LT_RANDOM randomizes an R8LT matrix.'
  
  seed = 123456789

  call r8lt_random ( m, n, seed, a )

  call r8lt_print ( m, n, a, '  The random RLT matrix:' )

  return
end
subroutine r8lt_sl_test ( )

!*****************************************************************************80
!
!! R8LT_SL_TEST tests R8LT_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
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
  write ( *, '(a)' ) 'R8LT_SL_TEST'
  write ( *, '(a)' ) '  R8LT_SL solves A*x=b for an R8LT matrix A.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n

  do i = 1, n
    do j = 1, n
      if ( j <= i ) then
        a(i,j) = real ( j, kind = 8 )
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8lt_print ( n, n, a, '  The R8LT matrix:' )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  call r8lt_mv ( n, n, a, x, b )
!
!  Solve the linear system.
!
  call r8lt_sl ( n, a, b, x )

  call r8vec_print ( n, x, '  Solution:' )

  return
end
subroutine r8lt_slt_test ( )

!*****************************************************************************80
!
!! R8LT_SLT_TEST tests R8LT_SLT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
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
  write ( *, '(a)' ) 'R8LT_SLT_TEST'
  write ( *, '(a)' ) '  R8LT_SLT solves A''*x=b for an R8LT matrix A'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n

  do i = 1, n
    do j = 1, n
      if ( j <= i ) then
        a(i,j) = real ( j, kind = 8 )
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8lt_print ( n, n, a, '  The R8LT matrix:' )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  call r8lt_mtv ( n, n, a, x, b )
!
!  Solve the linear system.
!
  call r8lt_slt ( n, a, b, x )

  call r8vec_print ( n, x, '  Solution to transposed system:' )

  return
end
subroutine r8lt_to_r8ge_test ( )

!*****************************************************************************80
!
!! R8LT_TO_R8GE_TEST tests R8LT_TO_R8GE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a_ge(m,n)
  real ( kind = 8 ) a_lt(m,n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8LT_TO_R8GE_TEST'
  write ( *, '(a)' ) '  R8LT_TO_R8GE converts an R8LT matrix to R8GE format.'

  call r8lt_random ( m, n, seed, a_lt )

  call r8lt_print ( m, n, a_lt, '  The random R8LT matrix:' )

  call r8lt_to_r8ge ( m, n, a_lt, a_ge )

  call r8ge_print ( m, n, a_ge, '  The R8GE matrix' )

  return
end
subroutine r8lt_zeros_test ( )

!*****************************************************************************80
!
!! R8LT_ZEROS_TEST tests R8LT_ZEROS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8LT_ZEROS_TEST'
  write ( *, '(a)' ) '  R8LT_ZEROS zeros an R8LT matrix;'

  call r8lt_zeros ( m, n, a )

  call r8lt_print ( m, n, a, '  The R8LT matrix:' )

  return
end
