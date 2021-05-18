program main

!*****************************************************************************80
!
!! MAIN is the main program for R83_TEST.
!
!  Discussion:
!
!    R83_TEST tests the R83 library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Test the LINPLUS library.'

  call r83_cg_test ( )
  call r83_cr_fa_test ( )
  call r83_cr_sl_test ( )
  call r83_cr_sls_test ( )
  call r83_dif2_test ( )
  call r83_gs_sl_test ( )
  call r83_indicator_test ( )
  call r83_jac_sl_test ( )
  call r83_mtv_test ( )
  call r83_mv_test ( )
  call r83_print_test ( )
  call r83_print_some_test ( )
  call r83_random_test ( )
  call r83_res_test ( )
  call r83_to_r8ge_test ( )
  call r83_zeros_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine r83_cg_test ( )

!*****************************************************************************80
!
!! R83_CG_TEST tests R83_CG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2014
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
  write ( *, '(a)' ) 'R83_CG_TEST'
  write ( *, '(a)' ) '  R83_CG applies CG to an R83 matrix.'

  seed = 123456789
  n = 10
!
!  Let A be the -1 2 -1 matrix.
!
  allocate ( a(1:3,1:n) )
  call r83_dif2 ( n, n, a )
!
!  Choose a random solution.
!
  allocate ( x1(1:n) )
  call r8vec_uniform_01 ( n, seed, x1 )
!
!  Compute the corresponding right hand side.
!
  allocate ( b(1:n) )
  call r83_mv ( n, n, a, x1, b )
!
!  Call the CG routine.
!
  allocate ( x2(1:n) )
  x2(1:n) = 1.0D+00
  call r83_cg ( n, a, b, x2 )
!
!  Compute the residual.
!
  allocate ( r(1:n) )
  call r83_res ( n, n, a, x2, b, r )
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
subroutine r83_cr_fa_test ( )

!*****************************************************************************80
!
!! R83_CR_FA_TEST tests R83_CR_FA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(3,n)
  real ( kind = 8 ) a_cr(3,0:2*n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_CR_FA_TEST'
  write ( *, '(a)' ) '  R83_CR_FA factors an R83 matrix for cyclic reduction.'
  write ( *, '(a)' ) '  Linear systems can then be solved by R83_CR_SL.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
  write ( *, '(a)' ) '  The matrix is NOT symmetric.'
!
!  Set the matrix values.
!
  a(1:3,1:n) = 0.0D+00

  do j = 1, n
    do i = max ( 1, j - 1 ), min ( n, j + 1 )
      if ( j == i - 1 ) then
        value = real ( j, kind = 8 )
      else if ( j == i ) then
        value = 4.0D+00 * real ( j, kind = 8 )
      else if ( j == i + 1 ) then
        value = real ( j, kind = 8 )
      end if
      a(i-j+2,j) = value
    end do
  end do

  call r83_print ( n, n, a, '  The matrix:' )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  call r83_mv ( n, n, a, x, b )
  x(1:n) = 0.0D+00
!
!  Factor the matrix.
!
  call r83_cr_fa ( n, a, a_cr )
!
!  Solve the linear system.
!
  call r83_cr_sl ( n, a_cr, b, x )

  call r8vec_print ( n, x, '  Solution:' )

  return
end
subroutine r83_cr_sl_test ( )

!*****************************************************************************80
!
!! R83_CR_SL_TEST tests R83_CR_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(3,n)
  real ( kind = 8 ) a_cr(3,0:2*n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_CR_SL_TEST'
  write ( *, '(a)' ) '  R83_CR_SL solves a linear system by cyclic reduction'
  write ( *, '(a)' ) '  after the R83 matrix has been factored by R83_CR_FA.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
  write ( *, '(a)' ) '  The matrix is NOT symmetric.'
!
!  Set the matrix values.
!
  a(1:3,1:n) = 0.0D+00

  do j = 1, n
    do i = max ( 1, j - 1 ), min ( n, j + 1 )
      if ( j == i - 1 ) then
        value = real ( j, kind = 8 )
      else if ( j == i ) then
        value = 4.0D+00 * real ( j, kind = 8 )
      else if ( j == i + 1 ) then
        value = real ( j, kind = 8 )
      end if
      a(i-j+2,j) = value
    end do
  end do

  call r83_print ( n, n, a, '  The matrix:' )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  call r83_mv ( n, n, a, x, b )
  x(1:n) = 0.0D+00
!
!  Factor the matrix.
!
  call r83_cr_fa ( n, a, a_cr )
!
!  Solve the linear system.
!
  call r83_cr_sl ( n, a_cr, b, x )

  call r8vec_print ( n, x, '  Solution:' )

  return
end
subroutine r83_cr_sls_test ( )

!*****************************************************************************80
!
!! R83_CR_SLS_TEST tests R83_CR_SLS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: nb = 2

  real ( kind = 8 ) a(3,n)
  real ( kind = 8 ) a_cr(3,0:2*n)
  real ( kind = 8 ) b(n,nb)
  real ( kind = 8 ) x(n,nb)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_CR_SLS_TEST'
  write ( *, '(a)' ) '  R83_CR_SLS solves linear systems by cyclic reduction'
  write ( *, '(a)' ) '  after the R83 matrix has been factored by R83_CR_FA.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
  write ( *, '(a)' ) '  Demonstrate multiple system solution method.'
!
!  Set the matrix values.
!
  call r83_dif2 ( n, n, a )
!
!  Print the matrix.
!
  call r83_print ( n, n, a, '  Input matrix:' )
!
!  Factor the matrix once.
!
  call r83_cr_fa ( n, a, a_cr )
!
!  Solve 2 systems simultaneously.
!
  b(1:n-1,1) = 0.0D+00
  b(n,1) = real ( n + 1, kind = 8 )

  b(1,2) = 1.0D+00
  b(2:n-1,2) = 0.0D+00
  b(n,2) = 1.0D+00
!
!  Solve the linear systems.
!
  call r83_cr_sls ( n, a_cr, nb, b, x )

  call r8ge_print ( n, nb, x, '  Solutions:' )

  return
end
subroutine r83_dif2_test ( )

!*****************************************************************************80
!
!! R83_DIF2_TEST tests R83_DIF2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R83_DIF2_TEST'
  write ( *, '(a)' ) '  R83_DIF2 sets an R83 matrix to the second difference.'
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

    allocate ( a(3,n) )

    call r83_dif2 ( m, n, a )

    call r83_print ( m, n, a, '  Second difference in R83 format:' )

    deallocate ( a )

  end do

  return
end
subroutine r83_gs_sl_test ( )

!*****************************************************************************80
!
!! R83_GS_SL_TEST tests R83_GS_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(3,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: it_max = 25
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_GS_SL_TEST'
  write ( *, '(a)' ) '  R83_GS_SL solves a linear system using'
  write ( *, '(a)' ) '  Gauss-Seidel iteration, with R83 matrix storage.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N =      ', n
  write ( *, '(a,i8)' ) '  Iterations per call = ', it_max
!
!  Set the matrix values.
!
  call r83_dif2 ( n, n, a )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  call r83_mv ( n, n, a, x, b )
!
!  Set the starting solution.
!
  x(1:n) = 0.0D+00
!
!  Solve the linear system.
!
  do i = 1, 3

    call r83_gs_sl ( n, a, b, x, it_max )

    call r8vec_print ( n, x, '  Current solution estimate:' )

  end do

  return
end
subroutine r83_indicator_test ( )

!*****************************************************************************80
!
!! R83_INDICATOR_TEST tests R83_INDICATOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_INDICATOR_TEST'
  write ( *, '(a)' ) '  R83_INDICATOR sets up an R83 indicator matrix.'
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

    allocate ( a(1:3,1:n) )

    call r83_indicator ( m, n, a )

    call r83_print ( m, n, a, '  The R83 indicator matrix:' )

    deallocate ( a )

  end do

  return
end
subroutine r83_jac_sl_test ( )

!*****************************************************************************80
!
!! R83_JAC_SL_TEST tests R83_JAC_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(3,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_max
  real ( kind = 8 ) x(n)

  it_max = 25

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_JAC_SL_TEST'
  write ( *, '(a)' ) '  R83_JAC_SL solves a linear system using'
  write ( *, '(a)' ) '  Jacobi iteration using R83 matrix storage.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N =      ', n
  write ( *, '(a,i8)' ) '  Iterations per call = ', it_max
!
!  Set the matrix values.
!
  call r83_dif2 ( n, n, a )
!
!  Set the desired solution.
!
  call r8vec_indicator1 ( n, x )
!
!  Compute the corresponding right hand side.
!
  call r83_mv ( n, n, a, x, b )

! call r8vec_print ( n, b, '  The right hand side:' )
!
!  Set the starting solution.
!
  x(1:n) = 0.0D+00
!
!  Solve the linear system.
!
  do i = 1, 3

    call r83_jac_sl ( n, a, b, x, it_max )

    call r8vec_print ( n, x, '  Current solution estimate:' )

  end do

  return
end
subroutine r83_mtv_test ( )

!*****************************************************************************80
!
!! R83_MTV_TEST tests R83_MTV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a_83(:,:)
  real ( kind = 8 ), allocatable :: a_ge(:,:)
  real ( kind = 8 ), allocatable :: ax_83(:)
  real ( kind = 8 ), allocatable :: ax_ge(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R83_MTV_TEST'
  write ( *, '(a)' ) '  R83_MTV computes b=A''*x, where A is an R83 matrix.'
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

    allocate ( a_83(3,n) )
    allocate ( a_ge(m,n) )
    allocate ( ax_83(n) )
    allocate ( ax_ge(n) )
    allocate ( x(m) )

    seed = 123456789
    call r83_random ( m, n, seed, a_83 )
    call r8vec_indicator1 ( m, x )
    call r83_mtv ( m, n, a_83, x, ax_83 )
    call r83_to_r8ge ( m, n, a_83, a_ge )
    call r8ge_mtv ( m, n, a_ge, x, ax_ge )
    call r8vec2_print ( n, ax_83, ax_ge, '  Product comparison:' )

    deallocate ( a_83 )
    deallocate ( a_ge )
    deallocate ( ax_83 )
    deallocate ( ax_ge )
    deallocate ( x )

  end do

  return
end
subroutine r83_mv_test ( )

!*****************************************************************************80
!
!! R83_MV_TEST tests R83_MV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a_83(:,:)
  real ( kind = 8 ), allocatable :: a_ge(:,:)
  real ( kind = 8 ), allocatable :: ax_83(:)
  real ( kind = 8 ), allocatable :: ax_ge(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R83_MV_TEST'
  write ( *, '(a)' ) '  R83_MV computes b=A*x, where A is an R83 matrix.'
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

    allocate ( a_83(3,n) )
    allocate ( a_ge(m,n) )
    allocate ( ax_83(m) )
    allocate ( ax_ge(m) )
    allocate ( x(n) )

    seed = 123456789
    call r83_random ( m, n, seed, a_83 )
    call r8vec_indicator1 ( n, x )
    call r83_mv ( m, n, a_83, x, ax_83 )
    call r83_to_r8ge ( m, n, a_83, a_ge )
    call r8ge_mv ( m, n, a_ge, x, ax_ge )
    call r8vec2_print ( m, ax_83, ax_ge, '  Product comparison:' )

    deallocate ( a_83 )
    deallocate ( a_ge )
    deallocate ( ax_83 )
    deallocate ( ax_ge )
    deallocate ( x )

  end do

  return
end
subroutine r83_print_test ( )

!*****************************************************************************80
!
!! R83_PRINT_TEST tests R83_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(3,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_PRINT_TEST'
  write ( *, '(a)' ) '  R83_PRINT prints an R83 matrix.'
!
!  Set the matrix.
!
  call r83_indicator ( m, n, a )

  call r83_print ( m, n, a, '  The R83 matrix:' )

  return
end
subroutine r83_print_some_test ( )

!*****************************************************************************80
!
!! R83_PRINT_SOME_TEST tests R83_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(3,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R83_PRINT_SOME prints some of an R83 matrix.'
!
!  Set the matrix.
!
  call r83_indicator ( m, n, a )

  call r83_print_some ( m, n, a, 2, 2, 5, 4, '  Rows 2-5, Cols 2-4:' )

  return
end
subroutine r83_random_test ( )

!*****************************************************************************80
!
!! R83_RANDOM_TEST tests R83_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_RANDOM_TEST'
  write ( *, '(a)' ) '  R83_RANDOM randomizes an R83 matrix.'
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

    seed = 123456789
    call r83_random ( m, n, seed, a )

    call r83_print ( m, n, a, '  The random R83 matrix:' )

    deallocate ( a )

  end do

  return
end
subroutine r83_res_test ( )

!*****************************************************************************80
!
!! R83_RES_TEST tests R83_RES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2015
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
  write ( *, '(a)' ) 'R83_RES_TEST'
  write ( *, '(a)' ) '  R83_RES computes b-A*x, where A is an R83 matrix.'
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
    call r83_random ( m, n, seed, a )
    call r8vec_indicator1 ( n, x )
    call r83_mv ( m, n, a, x, b )
    call r83_res ( m, n, a, x, b, r )
    call r8vec_print ( m, r, '  Residual A*x-b:' )

    deallocate ( a )
    deallocate ( b )
    deallocate ( r )
    deallocate ( x )

  end do

  return
end
subroutine r83_to_r8ge_test ( )

!*****************************************************************************80
!
!! R83_TO_R8GE_TEST tests R83_TO_R8GE.
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

  real ( kind = 8 ), allocatable :: a_83(:,:)
  real ( kind = 8 ), allocatable :: a_ge(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R83_TO_R8GE_TEST'
  write ( *, '(a)' ) '  R83_TO_R8GE converse an R83 matrix to R8GE format.'
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

    allocate ( a_83(1:3,1:n) )
    allocate ( a_ge(1:m,1:n) )

    seed = 123456789
    call r83_random ( m, n, seed, a_83 )
    call r83_print ( m, n, a_83, '  R83 matrix:' )

    call r83_to_r8ge ( m, n, a_83, a_ge )
    call r8ge_print ( m, n, a_ge, '  R8GE matrix:' )

    deallocate ( a_83 )
    deallocate ( a_ge )

  end do

  return
end
subroutine r83_zeros_test ( )

!*****************************************************************************80
!
!! R83_ZEROS_TEST tests R83_ZEROS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R83_ZEROS_TEST'
  write ( *, '(a)' ) '  R83_ZEROS zeros an R83 matrix.'
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

    call r83_zeros ( m, n, a )

    call r83_print ( m, n, a, '  The R83 zero matrix:' )

    deallocate ( a )

  end do

  return
end
