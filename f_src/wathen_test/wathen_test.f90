program main

!*****************************************************************************80
!
!! MAIN is the main program for WATHEN_TEST.
!
!  Discussion:
!
!    WATHEN_TEST tests wathen.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WATHEN_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the WATHEN library.'

  call test01 ( )
  call test02 ( )
  call test05 ( )
  call test06 ( )
  call test07 ( )
  call test08 ( )
  call test10 ( )
  call test11 ( )
  call test115 ( )
  call wathen_xy_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WATHEN_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 assembles, factor and solve using WATHEN_GE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) e
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ), allocatable :: ipvt(:)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Assemble, factor and solve a Wathen system'
  write ( *, '(a)' ) '  defined by WATHEN_GE.'
  write ( *, '(a)' ) ''

  nx = 4
  ny = 4
  write ( *, '(a,i6)' ) '  Elements in X direction NX = ', nx
  write ( *, '(a,i6)' ) '  Elements in Y direction NY = ', ny
  write ( *, '(a,i6)' ) '  Number of elements = ', nx * ny
!
!  Compute the number of unknowns.
!
  call wathen_order ( nx, ny, n )
  write ( *, '(a,i6)' ) '  Number of nodes N = ', n
!
!  Set up a random solution X.
!
  seed = 123456789
  allocate ( x1(1:n) )
  call r8vec_uniform_01 ( n, seed, x1 )
!
!  Compute the matrix.
!
  seed = 123456789
  allocate ( a(1:n,1:n) )
  call wathen_ge ( nx, ny, n, seed, a )
!
!  Compute the corresponding right hand side B.
!
  allocate ( b(1:n) )
  b = matmul ( a, x1 )
!
!  Solve the linear system.
!
  allocate ( ipvt(1:n) )
  call dgefa ( a, n, n, ipvt, info )

  allocate ( x2(1:n) )
  do i = 1, n
    x2(i) = b(i)
  end do
  job = 0

  call dgesl ( a, n, n, ipvt, x2, job )
!
!  Compute the maximum solution error.
!
  e = maxval ( abs ( x1 - x2 ) )
  write ( *, '(a,g14.6)' ) '  Maximum solution error is ', e 
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( b )
  deallocate ( ipvt )
  deallocate ( x1 )
  deallocate ( x2 )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 assembles, factors and solves using WATHEN_GB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) e
  integer ( kind = 4 ) info
  integer ( kind = 4 ), allocatable :: ipvt(:)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) md
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Assemble, factor and solve a Wathen system'
  write ( *, '(a)' ) '  using WATHEN_GB.'
  write ( *, '(a)' ) ''

  nx = 4
  ny = 4
  write ( *, '(a,i6)' ) '  Elements in X direction NX = ', nx
  write ( *, '(a,i6)' ) '  Elements in Y direction NY = ', ny
  write ( *, '(a,i6)' ) '  Number of elements = ', nx * ny
!
!  Compute the number of unknowns.
!
  call wathen_order ( nx, ny, n )
  write ( *, '(a,i6)' ) '  Number of nodes N = ', n
!
!  Compute the bandwidth.
!
  call wathen_bandwidth ( nx, ny, ml, md, mu )
  write ( *, '(a,i6)' ) '  Lower bandwidth ML = ', ml
  write ( *, '(a,i6)' ) '  Upper bandwidth MU = ', mu
!
!  Set up a random solution X1.
!
  seed = 123456789
  allocate ( x1(1:n) )
  call r8vec_uniform_01 ( n, seed, x1 )
!
!  Compute the matrix.
!
  seed = 123456789
  allocate ( a(1:2*ml+mu+1,1:n) )
  call wathen_gb ( nx, ny, n, seed, a )
!
!  Compute the corresponding right hand side B.
!
  allocate ( b(1:n) )
  call mv_gb ( n, n, ml, mu, a, x1, b )
!
!  Solve the linear system.
!
  lda = 2 * ml + mu + 1
  allocate ( ipvt(1:n) )
  call dgbfa ( a, lda, n, ml, mu, ipvt, info )

  allocate ( x2(1:n) )
  x2(1:n) = b(1:n)
  job = 0
  call dgbsl ( a, lda, n, ml, mu, ipvt, x2, job )
!
!  Compute the maximum solution error.
!
  e = maxval ( abs ( x1 - x2 ) );
  write ( *, '(a,g14.6)' ) '  Maximum solution error is ', e
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( b )
  deallocate ( ipvt )
  deallocate ( x1 )
  deallocate ( x2 )

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 measures the storage needed for the Wathen system.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) bd1
  integer ( kind = 4 ) bd2
  integer ( kind = 4 ) bl1
  integer ( kind = 4 ) bl2
  integer ( kind = 4 ) bu1
  integer ( kind = 4 ) bu2
  integer ( kind = 4 ) bw1
  integer ( kind = 4 ) bw2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) storage_gb
  integer ( kind = 4 ) storage_ge
  integer ( kind = 4 ) storage_sparse
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  For various problem sizes and storage schemes,'
  write ( *, '(a)' ) '  measure the storage used for the Wathen system.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '                                   Predicted  Observed'
  write ( *, '(a)' ) '                              GE        Band      Band      Band    Sparse'
  write ( *, '(a)' ) '    NX  Elements   Nodes   storage     width     width   storage   storage'
  write ( *, '(a)' ) ''

  nx = 1
  ny = 1

  do test = 1, 6
!
!  Compute the number of unknowns.
!
    call wathen_order ( nx, ny, n )
!
!  Predict the bandwidth.
!
    call wathen_bandwidth ( nx, ny, bl1, bd1, bu1 )
    bw1 = bl1 + bd1 + bu1
!
!  Compute the matrix.
!
    seed = 123456789
    allocate ( a(1:n,1:n) )
    call wathen_ge ( nx, ny, n, seed, a )

    storage_ge = n * n

    call bandwidth ( n, n, a, bw2, bl2, bd2, bu2 )
    storage_gb = ( 2 * bl2 + 1 + bu2 ) * n

    call nonzeros ( n, n, a, storage_sparse )
!
!  Report.
!
    write ( *, '(2x,i4,6x,i4,2x,i6,2x,i8,2x,i8,2x,i8,2x,i8,2x,i8)' )  &
      nx, nx * ny, n, storage_ge, bw1, bw2, storage_gb, storage_sparse
!
!  Ready for next iteration.
!
    nx = nx * 2
    ny = ny * 2

    deallocate ( a )

  end do

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 times WATHEN_GE assembly and solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) e
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ), allocatable :: ipvt(:)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) storage_ge
  real ( kind = 8 ) t0
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST06'
  write ( *, '(a)' ) '  For various problem sizes,'
  write ( *, '(a)' ) '  time the assembly and factorization of a Wathen system'
  write ( *, '(a)' ) '  using the WATHEN_GE function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    NX  Elements   Nodes   Storage    Assembly      Factor      Error'
  write ( *, '(a)' ) ''

  nx = 1
  ny = 1

  do test = 1, 6
!
!  Compute the number of unknowns.
!
    call wathen_order ( nx, ny, n )
    storage_ge = n * n
!
!  Set up a random solution X1.
!
    seed = 123456789
    allocate ( x1(1:n) )
    call r8vec_uniform_01 ( n, seed, x1 )
!
!  Compute the matrix, and measure the storage required.
!
    seed = 123456789
    allocate ( a(1:n,1:n) )

    call cpu_time ( t0 )
    call wathen_ge ( nx, ny, n, seed, a )
    call cpu_time ( t1 )
    t1 = t1 - t0
!
!  Compute the corresponding right hand side B.
!
    allocate ( b(1:n) )
    b = matmul ( a, x1 )
!
!  Solve the system.
!
    allocate ( ipvt(1:n) )
    allocate ( x2(1:n) )
    do i = 1, n
      x2(i) = b(i)
    end do
    job = 0

    call cpu_time ( t0 )
    call dgefa ( a, n, n, ipvt, info )
    call dgesl ( a, n, n, ipvt, x2, job )
    call cpu_time ( t2 )
    t2 = t2 - t0
!
!  Compute the maximum solution error.
!
    e = maxval ( abs ( x1 - x2 ) )
!
!  Report.
!
    write ( *, '(2x,i4,6x,i4,2x,i6,2x,i8,2x,e10.2,2x,e10.2,2x,e10.2)' ) &
      nx, nx * ny, n, storage_ge, t1, t2, e
!
!  Ready for next iteration.
!
    nx = nx * 2
    ny = ny * 2
!
!  Free memory.
!
    deallocate ( a )
    deallocate ( b )
    deallocate ( ipvt )
    deallocate ( x1 )
    deallocate ( x2 )

  end do

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!!  TEST07 times WATHEN_GB assembly and solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) e
  integer ( kind = 4 ) info
  integer ( kind = 4 ), allocatable :: ipvt(:)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) md
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) storage_gb
  real ( kind = 8 ) t0
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST07'
  write ( *, '(a)' ) '  For various problem sizes,'
  write ( *, '(a)' ) '  time the assembly and factorization of a Wathen system'
  write ( *, '(a)' ) '  using the WATHEN_GB function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    NX  Elements   Nodes   Storage    Assembly      Factor      Error'
  write ( *, '(a)' ) ''

  nx = 1
  ny = 1

  do test = 1, 6
!
!  Compute the number of unknowns.
!
    call wathen_order ( nx, ny, n )
!
!  Compute the bandwidth.
!
    call wathen_bandwidth ( nx, ny, ml, md, mu )
    storage_gb = ( 2 * ml + mu + 1 ) * n
!
!  Set up a random solution X1.
!
    seed = 123456789
    allocate ( x1(1:n) )
    call r8vec_uniform_01 ( n, seed, x1 )
!
!  Compute the matrix.
!
    seed = 123456789
    allocate ( a(1:2*ml+mu+1,1:n) )
    call cpu_time ( t0 )
    call wathen_gb ( nx, ny, n, seed, a )
    call cpu_time ( t1 )
    t1 = t1 - t0
!
!  Compute the corresponding right hand side B.
!
    allocate ( b(1:n) )
    call mv_gb ( n, n, ml, mu, a, x1, b )
!
!  Solve the system.
!
    lda = 2 * ml + mu + 1
    allocate ( ipvt(1:n) )
    allocate ( x2(1:n) )
    x2(1:n) = b(1:n)
    job = 0

    call cpu_time ( t0 )
    call dgbfa ( a, lda, n, ml, mu, ipvt, info )
    call dgbsl ( a, lda, n, ml, mu, ipvt, x2, job )
    call cpu_time ( t2 )
    t2 = t2 - t0
!
!  Compute the maximum solution error.
!
    e = maxval ( abs ( x1 - x2 ) )
!
!  Report.
!
    write ( *, '(2x,i4,6x,i4,2x,i6,2x,i8,2x,e10.2,2x,e10.2,2x,e10.2)' ) &
      nx, nx * ny, n, storage_gb, t1, t2, e
!
!  Ready for next iteration.
!
    nx = nx * 2
    ny = ny * 2
!
!  Free memory.
!
    deallocate ( a )
    deallocate ( b )
    deallocate ( ipvt )
    deallocate ( x1 )
    deallocate ( x2 )

  end do

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 times WATHEN_GE/WATHEN_GB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) e
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ), allocatable :: ipvt(:)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) md
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) storage_gb
  integer ( kind = 4 ) storage_ge
  real ( kind = 8 ) t0
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST08'
  write ( *, '(a)' ) '  For various problem sizes,'
  write ( *, '(a)' ) '  time the assembly and factorization of a Wathen system'
  write ( *, '(a)' ) '  WATHEN_GE/WATHEN_GB'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '                   NX  Elements   Nodes   Storage    Assembly      Factor      Error'

  nx = 1
  ny = 1

  do test = 1, 6
!
!  Compute the number of unknowns.
!
    call wathen_order ( nx, ny, n )
    storage_ge = n * n
!
!  Set up a random solution X1.
!
    seed = 123456789
    allocate ( x1(1:n) )
    call r8vec_uniform_01 ( n, seed, x1 )
!
!  Compute the matrix.
!
    seed = 123456789
    allocate ( a(1:n,1:n) )
    call cpu_time ( t0 )
    call wathen_ge ( nx, ny, n, seed, a )
    call cpu_time ( t1 )
    t1 = t1 - t0
!
!  Compute the corresponding right hand side B.
!
    allocate ( b(1:n) )
    b = matmul ( a, x1 )
!
!  Solve the system.
!
    allocate ( ipvt(1:n) )
    allocate ( x2(1:n) )
    do i = 1, n
      x2(i) = b(i)
    end do
    job = 0

    call cpu_time ( t0 )
    call dgefa ( a, n, n, ipvt, info )
    call dgesl ( a, n, n, ipvt, x2, job )
    call cpu_time ( t2 )
    t2 = t2 - t0
!
!  Compute the maximum solution error.
!
    e = maxval ( abs ( x1 - x2 ) )
!
!  Report.
!
    write ( *, '(a)' ) ''
    write ( *, '(2x,a,2x,i4,6x,i4,2x,i6,2x,i8,2x,e10.2,2x,e10.2,2x,e10.2)' ) &
      'WATHEN_GE    ', nx, nx * ny, n, storage_ge, t1, t2, e
!
!  Free memory.
!
    deallocate ( a )
    deallocate ( b )
    deallocate ( ipvt )
    deallocate ( x1 )
    deallocate ( x2 )
!
!  Compute the bandwidth.
!
    call wathen_bandwidth ( nx, ny, ml, md, mu )
    storage_gb = ( 2 * ml + mu + 1 ) * n
!
!  Set up a random solution X1.
!
    seed = 123456789
    allocate ( x1(1:n) )
    call r8vec_uniform_01 ( n, seed, x1 )
!
!  Compute the matrix.
!
    seed = 123456789
    allocate ( a(1:2*ml+mu+1,1:n) )
    call cpu_time ( t0 )
    call wathen_gb ( nx, ny, n, seed, a )
    call cpu_time ( t1 )
    t1 = t1 - t0
!
!  Compute the corresponding right hand side B.
!
    allocate ( b(1:n) )
    call mv_gb ( n, n, ml, mu, a, x1, b )
!
!  Solve the system.
!
    lda = 2 * ml + mu + 1
    allocate ( ipvt(1:n) )
    allocate ( x2(1:n) )
    x2(1:n) = b(1:n)
    job = 0

    call cpu_time ( t0 )
    call dgbfa ( a, lda, n, ml, mu, ipvt, info )
    call dgbsl ( a, lda, n, ml, mu, ipvt, x2, job )
    call cpu_time ( t2 )
    t2 = t2 - t0
!
!  Compute the maximum solution error.
!
    e = maxval ( abs ( x1 - x2 ) )
!
!  Report.
!
    write ( *, '(2x,a,2x,i4,6x,i4,2x,i6,2x,i8,2x,e10.2,2x,e10.2,2x,e10.2)' ) &
      'WATHEN_GB    ', nx, nx * ny, n, storage_gb, t1, t2, e
!
!  Free memory.
!
    deallocate ( a )
    deallocate ( b )
    deallocate ( ipvt )
    deallocate ( x1 )
    deallocate ( x2 )
!
!  Ready for next iteration.
!
    nx = nx * 2
    ny = ny * 2

  end do

  return
end
subroutine test10 ( )

!*****************************************************************************80
!
!! TEST10 assembles, factor and solve using WATHEN_GE and CG_GE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) e
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST10'
  write ( *, '(a)' ) '  Assemble, factor and solve a Wathen system'
  write ( *, '(a)' ) '  defined by WATHEN_GE and CG_GE.'
  write ( *, '(a)' ) ''

  nx = 1
  ny = 1
  write ( *, '(a,i6)' ) '  Elements in X direction NX = ', nx
  write ( *, '(a,i6)' ) '  Elements in Y direction NY = ', ny
  write ( *, '(a,i6)' ) '  Number of elements = ', nx * ny
!
!  Compute the number of unknowns.
!
  call wathen_order ( nx, ny, n )
  write ( *, '(a,i6)' ) '  Number of nodes N = ', n
!
!  Set up a random solution X.
!
  seed = 123456789
  allocate ( x1(1:n) )
  call r8vec_uniform_01 ( n, seed, x1 )
!
!  Compute the matrix.
!
  seed = 123456789
  allocate ( a(1:n,1:n) )
  call wathen_ge ( nx, ny, n, seed, a )
!
!  Compute the corresponding right hand side B.
!
  allocate ( b(1:n) )
  b = matmul ( a, x1 )
!
!  Solve the linear system.
!
  allocate ( x2(1:n) )
  x2(1:n) = 1.0D+00
  call cg_ge ( n, a, b, x2 )
!
!  Compute the maximum solution error.
!
  e = maxval ( abs ( x1 - x2 ) )
  write ( *, '(a,g14.6)' ) '  Maximum solution error is ', e 
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( b )
  deallocate ( x1 )
  deallocate ( x2 )

  return
end
subroutine test11 ( )

!*****************************************************************************80
!
!! TEST11 assemble, factor and solve using WATHEN_ST + CG_ST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:)
  real ( kind = 8 ), allocatable :: b(:)
  integer ( kind = 4 ), allocatable :: col(:)
  real ( kind = 8 ) e
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) nz_num
  integer ( kind = 4 ), allocatable :: row(:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST11'
  write ( *, '(a)' ) '  Assemble, factor and solve a Wathen system'
  write ( *, '(a)' ) '  defined by WATHEN_ST and CG_ST.'
  write ( *, '(a)' ) ''

  nx = 1
  ny = 1
  write ( *, '(a,i6)' ) '  Elements in X direction NX = ', nx
  write ( *, '(a,i6)' ) '  Elements in Y direction NY = ', ny
  write ( *, '(a,i6)' ) '  Number of elements = ', nx * ny
!
!  Compute the number of unknowns.
!
  call wathen_order ( nx, ny, n )
  write ( *, '(a,i6)' ) '  Number of nodes N = ', n
!
!  Set up a random solution X1.
!
  seed = 123456789
  allocate ( x1(1:n) )
  call r8vec_uniform_01 ( n, seed, x1 )
!
!  Compute the matrix size.
!
  call wathen_st_size ( nx, ny, nz_num )
  write ( *, '(a,i6)' ) '  Number of nonzeros NZ_NUM = ', nz_num
!
!  Compute the matrix.
!
  seed = 123456789
  allocate ( row(1:nz_num) )
  allocate ( col(1:nz_num) )
  allocate ( a(1:nz_num) )
  call wathen_st ( nx, ny, nz_num, seed, row, col, a )
!
!  Compute the corresponding right hand side B.
!
  allocate ( b(1:n) )
  call mv_st ( n, n, nz_num, row, col, a, x1, b )
!
!  Solve the linear system.
!
  allocate ( x2(1:n) )
  x2(1:n) = 1.0D+00
  call cg_st ( n, nz_num, row, col, a, b, x2 )
!
!  Compute the maximum solution error.
!
  e = maxval ( abs ( x1 - x2 ) )
  write ( *, '(a,g14.6)' ) '  Maximum solution error is ', e 
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( b )
  deallocate ( col )
  deallocate ( row )
  deallocate ( x1 )
  deallocate ( x2 )

  return
end
subroutine test115 ( )

!*****************************************************************************80
!
!! TEST115 assembles, factors and solves using WATHEN_GB and CG_GB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) e
  integer ( kind = 4 ) md
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x1(:)
  real ( kind = 8 ), allocatable :: x2(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST115'
  write ( *, '(a)' ) '  Assemble, factor and solve a Wathen system'
  write ( *, '(a)' ) '  using WATHEN_GB and CG_GB.'
  write ( *, '(a)' ) ''

  nx = 4
  ny = 4
  write ( *, '(a,i6)' ) '  Elements in X direction NX = ', nx
  write ( *, '(a,i6)' ) '  Elements in Y direction NY = ', ny
  write ( *, '(a,i6)' ) '  Number of elements = ', nx * ny
!
!  Compute the number of unknowns.
!
  call wathen_order ( nx, ny, n )
  write ( *, '(a,i6)' ) '  Number of nodes N = ', n
!
!  Compute the bandwidth.
!
  call wathen_bandwidth ( nx, ny, ml, md, mu )
  write ( *, '(a,i6)' ) '  Lower bandwidth ML = ', ml
  write ( *, '(a,i6)' ) '  Upper bandwidth MU = ', mu
!
!  Set up a random solution X1.
!
  seed = 123456789
  allocate ( x1(1:n) )
  call r8vec_uniform_01 ( n, seed, x1 )
!
!  Compute the matrix.
!
  seed = 123456789
  allocate ( a(1:2*ml+mu+1,1:n) )
  call wathen_gb ( nx, ny, n, seed, a )
!
!  Compute the corresponding right hand side B.
!
  allocate ( b(1:n) )
  call mv_gb ( n, n, ml, mu, a, x1, b )
!
!  Solve the linear system.
!
  allocate ( x2(1:n) )
  x2(1:n) = 1.0D+00
  call cg_gb ( n, ml, mu, a, b, x2 )
!
!  Compute the maximum solution error.
!
  e = maxval ( abs ( x1 - x2 ) );
  write ( *, '(a,g14.6)' ) '  Maximum solution error is ', e
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( b )
  deallocate ( x1 )
  deallocate ( x2 )

  return
end
subroutine wathen_xy_test ( )

!*****************************************************************************80
!
!! wathen_xy_test tests wathen_xy.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'wathen_xy_test'
  write ( *, '(a)' ) '  wathen_xy returns the (X,Y) coordinates of nodes in the'
  write ( *, '(a)' ) '  Wathen finite element system.'

  nx = 3
  ny = 3
  call wathen_order ( nx, ny, n )

  allocate ( x(1:n) )
  allocate ( y(1:n) )
  call wathen_xy ( nx, ny, n, x, y )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   k   i   j         x          y'
  write ( *, '(a)' ) ''

  k = 0
  do j = 1, 2 * ny + 1

    if ( mod ( j, 2 ) == 1 ) then

      do i = 1, 2 * nx + 1
        k = k + 1
        write ( *, '(2x,i2,2x,i2,2x,i2,2x,f8.4,2x,f8.4)' ) k, i, j, x(k), y(k)
      end do

    else

      do i = 1, nx + 1
        k = k + 1
        write ( *, '(2x,i2,2x,i2,2x,i2,2x,f8.4,2x,f8.4)' ) k, i, j, x(k), y(k)
      end do

    end if

  end do
 
  deallocate ( x )
  deallocate ( y )

  return
end

