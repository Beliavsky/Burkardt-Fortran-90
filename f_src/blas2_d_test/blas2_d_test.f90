program main

!*****************************************************************************80
!
!! MAIN is the main program for BLAS2_D_TEST.
!
!  Discussion:
!
!    BLAS2_D_TEST tests the BLAS2_D library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS2_D_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BLAS2_D library.'

  call dgbmv_test ( )
  call dgemv_test ( )
  call dger_test ( )
  call dsbmv_test ( )
  call dspmv_test ( )
  call dspr_test ( )
  call dspr2_test ( )
  call dsymv_test ( )
  call dsyr_test ( )
  call dsyr2_test ( )
  call dtbmv_test ( )
  call dtbsv_test ( )
  call dtpmv_test ( )
  call dtpsv_test ( )
  call dtrmv_test ( )
  call dtrsv_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS2_D_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine dgbmv_test ( )

!*****************************************************************************80
!
!! DGBMV_TEST tests DGBMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: kl = 1
  integer ( kind = 4 ), parameter :: ku = 1
  integer ( kind = 4 ), parameter :: lda = kl + 1 + ku

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character trans
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DGBMV_TEST'
  write ( *, '(a)' ) '  DGBMV computes y := alpha * A * x + beta * y'
  write ( *, '(a)' ) '  for a general band matrix A.'

  trans = 'N'
  alpha = 2.0D+00
  incx = 1
  beta = 3.0D+00
  incy = 1

  allocate ( a(1:lda,1:n) )

  do i = 1, m

    jlo = max ( 1, i - kl )
    jhi = min ( n, i + ku )

    do j = jlo, jhi

      if ( i == j ) then
        a(ku+1+i-j,j) = 2.0D+00
      else if ( i == j - 1 .or. i == j + 1 ) then
        a(ku+1+i-j,j) = -1.0D+00
      else
        a(ku+1+i-j,j) = 0.0D+00
      end if

    end do
  end do

  allocate ( x(1:n) )

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  allocate ( y(1:m) )

  do i = 1, m
    y(i) = real ( 10 * i, kind = 8 )
  end do

  call dgbmv ( trans, m, n, kl, ku, alpha, a, lda, x, incx, beta, y, incy )

  call r8vec_print ( m, y, '  Result vector y:' )
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine dgemv_test ( )

!*****************************************************************************80
!
!! DGEMV_TEST tests DGEMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character trans
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DGEMV_TEST'
  write ( *, '(a)' ) '  DGEMV computes y := alpha * A * x + beta * y'
  write ( *, '(a)' ) '  or             y := alpha * A'' * x + beta * y,'
  write ( *, '(a)' ) '  for a general matrix A.'
!
!  y = alpha * A * x + beta * y
!
  trans = 'N'
  m = 5
  n = 4
  alpha = 2.0D+00
  lda = m
  allocate ( a(1:lda,1:n) )
  call r8mat_test ( trans, lda, m, n, a )
  allocate ( x(1:n) )
  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do
  incx = 1
  beta = 3.0D+00
  allocate ( y(1:m) )
  do i = 1, m
    y(i) = real ( 10 * i, kind = 8 )
  end do
  incy = 1

  call r8mat_print ( m, n, a, '  Matrix A:' )
  call r8vec_print ( n, x, '  Vector X:' )
  call r8vec_print ( m, y, '  Vector Y:' )

  call dgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )

  call r8vec_print ( m, y, '  Result Y = alpha * A  * x + beta * y' )
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )
!
!  y = alpha * A' * x + beta * y
!
  trans = 'T'
  m = 5
  n = 4
  alpha = 2.0D+00
  lda = m
  allocate ( a(1:lda,1:n) )
  call r8mat_test ( trans, lda, n, m, a )
  allocate ( x(1:m) )
  do i = 1, m
    x(i) = real ( i, kind = 8 )
  end do
  incx = 1
  beta = 3.0D+00
  allocate ( y(1:n) )
  do i = 1, n
    y(i) = real ( 10 * i, kind = 8 )
  end do
  incy = 1

  call r8mat_print ( m, n, a, '  Matrix A:' )
  call r8vec_print ( m, x, '  Vector X:' )
  call r8vec_print ( n, y, '  Vector Y:' )

  call dgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )

  call r8vec_print ( n, y, '  Result Y = alpha * A'' * x + beta * y' )
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine dger_test ( )

!*****************************************************************************80
!
!! DGER_TEST tests DGER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character trans
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DGER_TEST'
  write ( *, '(a)' ) '  DGER computes A := A + alpha * x * y'''
  write ( *, '(a)' ) '  for a general matrix A.'

  m = 5
  n = 4
  alpha = 2.0D+00
  trans = 'N'
  lda = m
  allocate ( a(1:lda,1:n) )
  call r8mat_test ( trans, lda, m, n, a )

  allocate ( x(1:m) )
  do i = 1, m
    x(i) = real ( i, kind = 8 )
  end do
  incx = 1

  allocate ( y(1:n) )
  do i = 1, n
    y(i) = real ( 10 * i, kind = 8 )
  end do
  incy = 1

  call r8mat_print ( m, n, a, '  Matrix A:' )
  call r8vec_print ( m, x, '  Vector X:' )
  call r8vec_print ( n, y, '  Vector Y:' )

  call dger ( m, n, alpha, x, incx, y, incy, a, lda )

  call r8mat_print ( m, n, a, '  Result A = A + alpha * x * y''' )
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine dsbmv_test ( )

!*****************************************************************************80
!
!! DSBMV_TEST tests DSBMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: k = 1
  integer ( kind = 4 ), parameter :: lda = k + 1

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  character uplo
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DSBMV_TEST'
  write ( *, '(a)' ) '  DSBMV computes y := alpha * A * x + beta * y'
  write ( *, '(a)' ) '  for a symmetric band matrix A.'

  uplo = 'U'
  alpha = 2.0D+00
  incx = 1
  beta = 3.0D+00
  incy = 1

  allocate ( a(1:lda,1:n) )

  do i = 1, m

    jhi = min ( n, i + k )

    do j = i, jhi

      if ( i == j ) then
        a(k+1+i-j,j) = 2.0D+00
      else if ( i == j - 1 ) then
        a(k+1+i-j,j) = -1.0D+00
      else
        a(k+1+i-j,j) = 0.0D+00
      end if

    end do
  end do

  allocate ( x(1:n) )

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  allocate ( y(1:m) )

  do i = 1, m
    y(i) = real ( 10 * i, kind = 8 )
  end do

  call dsbmv ( uplo, n, k, alpha, a, lda, x, incx, beta, y, incy )

  call r8vec_print ( m, y, '  Result Y:' )
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine dspmv_test ( )

!*****************************************************************************80
!
!! DSPMV_TEST tests DSPMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ), allocatable :: ap(:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character uplo
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DSPMV_TEST'
  write ( *, '(a)' ) '  DSPMV computes y := alpha * A * x + beta * y'
  write ( *, '(a)' ) '  for a symmetric packed band matrix A.'

  uplo = 'U'
  alpha = 2.0D+00
  incx = 1
  beta = 3.0D+00
  incy = 1

  allocate ( ap((n*(n+1))/2) )

  k = 0

  do j = 1, n

    do i = 1, j

      k = k + 1
      if ( i == j ) then
        ap(k) = 2.0D+00
      else if ( i == j - 1 ) then
        ap(k) = -1.0D+00
      else
        ap(k) = 0.0D+00
      end if

    end do
  end do

  allocate ( x(1:n) )

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  allocate ( y(1:n) )

  do i = 1, n
    y(i) = real ( 10 * i, kind = 8 )
  end do

  call dspmv ( uplo, n, alpha, ap, x, incx, beta, y, incy )

  call r8vec_print ( n, y, '  Result Y:' )
!
!  Free memory.
!
  deallocate ( ap )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine dspr_test ( )

!*****************************************************************************80
!
!! DSPR_TEST tests DSPR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ), allocatable :: ap(:)
  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character uplo
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DSPR_TEST'
  write ( *, '(a)' ) '  DSPR computes A := A + alpha * x * x'''
  write ( *, '(a)' ) '  for a symmetric packed band matrix A.'

  uplo = 'U'
  alpha = 2.0D+00
  incx = 1

  allocate ( ap((n*(n+1))/2) )

  k = 0

  do j = 1, n

    do i = 1, j

      k = k + 1
      if ( i == j ) then
        ap(k) = 2.0D+00
      else if ( i == j - 1 ) then
        ap(k) = -1.0D+00
      else
        ap(k) = 0.0D+00
      end if

    end do
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A:'
  write ( *, '(a)' ) ''
  do i = 1, n
    do j = 1, i - 1
      call triangle_upper_to_i4 ( j, i, k )
      write ( *, '(f8.2)', advance = 'no' ) ap(k)
    end do
    do j = i, n
      call triangle_upper_to_i4 ( i, j, k )
      write ( *, '(f8.2)', advance = 'no' ) ap(k)
    end do
    write ( *, '(a)', advance = 'yes' )
  end do

  allocate ( x(1:n) )

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  call dspr ( uplo, n, alpha, x, incx, ap )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A = A + alpha*x*x'':'
  write ( *, '(a)' ) ''
  do i = 1, n
    do j = 1, i - 1
      call triangle_upper_to_i4 ( j, i, k )
      write ( *, '(f8.2)', advance = 'no' ) ap(k)
    end do
    do j = i, n
      call triangle_upper_to_i4 ( i, j, k )
      write ( *, '(f8.2)', advance = 'no' ) ap(k)
    end do
    write ( *, '(a)', advance = 'yes' )
  end do
!
!  Free memory.
!
  deallocate ( ap )
  deallocate ( x )

  return
end
subroutine dspr2_test ( )

!*****************************************************************************80
!
!! DSPR2_TEST tests DSPR2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ), allocatable :: ap(:)
  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character uplo
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DSPR2_TEST'
  write ( *, '(a)' ) '  DSPR2 computes A := A + alpha*x*y''+alpha*y*x'''
  write ( *, '(a)' ) '  for a symmetric packed band matrix A.'

  uplo = 'U'
  alpha = 2.0D+00
  incx = 1
  incy = 1

  allocate ( ap((n*(n+1))/2) )

  k = 0

  do j = 1, n

    do i = 1, j

      k = k + 1
      if ( i == j ) then
        ap(k) = 2.0D+00
      else if ( i == j - 1 ) then
        ap(k) = -1.0D+00
      else
        ap(k) = 0.0D+00
      end if

    end do
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A:'
  write ( *, '(a)' ) ''
  do i = 1, n
    do j = 1, i - 1
      call triangle_upper_to_i4 ( j, i, k )
      write ( *, '(f8.2)', advance = 'no' ) ap(k)
    end do
    do j = i, n
      call triangle_upper_to_i4 ( i, j, k )
      write ( *, '(f8.2)', advance = 'no' ) ap(k)
    end do
    write ( *, '(a)', advance = 'yes' )
  end do

  allocate ( x(1:n) )

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  allocate ( y(1:n) )

  do i = 1, n
    y(i) = real ( 10 * i, kind = 8 )
  end do

  call dspr2 ( uplo, n, alpha, x, incx, y, incy, ap )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A = A + alpha*x*y''+alpha*y*x'':'
  write ( *, '(a)' ) ''
  do i = 1, n
    do j = 1, i - 1
      call triangle_upper_to_i4 ( j, i, k )
      write ( *, '(f8.2)', advance = 'no' ) ap(k)
    end do
    do j = i, n
      call triangle_upper_to_i4 ( i, j, k )
      write ( *, '(f8.2)', advance = 'no' ) ap(k)
    end do
    write ( *, '(a)', advance = 'yes' )
  end do
!
!  Free memory.
!
  deallocate ( ap )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine dsymv_test ( )

!*****************************************************************************80
!
!! DSYMV_TEST tests DSYMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: lda = n

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  character uplo
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DSYMV_TEST'
  write ( *, '(a)' ) '  DSYMV computes y := alpha * A * x + beta * y'
  write ( *, '(a)' ) '  for a general symmetric matrix A.'

  uplo = 'U'
  alpha = 2.0D+00
  incx = 1
  beta = 3.0D+00
  incy = 1

  allocate ( a(1:lda,1:n) )

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0D+00
      else if ( i == j - 1 ) then
        a(i,j) = -1.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  allocate ( x(1:n) )

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  allocate ( y(1:n) )

  do i = 1, n
    y(i) = real ( 10 * i, kind = 8 )
  end do

  call dsymv ( uplo, n, alpha, a, lda, x, incx, beta, y, incy )

  call r8vec_print ( n, y, '  Result vector y := alpha * A * x + beta * y' );
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine dsyr_test ( )

!*****************************************************************************80
!
!! DSYR_TEST tests DSYR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n
  character uplo
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DSYR_TEST'
  write ( *, '(a)' ) '  DSYR computes A := A + alpha * x * x'''
  write ( *, '(a)' ) '  for a symmetric matrix A.'

  uplo = 'U'
  n = 5
  alpha = 2.0D+00
  lda = n

  allocate ( a(1:lda,1:n) )

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0D+00
      else if ( i == j - 1 ) then
        a(i,j) = -1.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  allocate ( x(1:n) )
  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do
  incx = 1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A:'
  write ( *, '(a)' ) ''
  do i = 1, n
    do j = 1, i - 1
      write ( *, '(f8.2)', advance = 'no' ) a(j,i)
    end do
    do j = i, n
      write ( *, '(f8.2)', advance = 'no' ) a(i,j)
    end do
    write ( *, '(a)', advance = 'yes' )
  end do

  call r8vec_print ( n, x, '  Vector X:' )

  call dsyr ( uplo, n, alpha, x, incx, a, lda )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A = A + alpha * x * x'':'
  write ( *, '(a)' ) ''
  do i = 1, n
    do j = 1, i - 1
      write ( *, '(f8.2)', advance = 'no' ) a(j,i)
    end do
    do j = i, n
      write ( *, '(f8.2)', advance = 'no' ) a(i,j)
    end do
    write ( *, '(a)', advance = 'yes' )
  end do
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )

  return
end
subroutine dsyr2_test ( )

!*****************************************************************************80
!
!! DSYR2_TEST tests DSYR2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n
  character uplo
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DSYR2_TEST'
  write ( *, '(a)' ) '  DSYR2 computes A := A + alpha*x*y'' + alpha*y*x'''
  write ( *, '(a)' ) '  for a symmetric matrix A.'

  uplo = 'U'
  n = 5
  alpha = 2.0D+00
  lda = n

  allocate ( a(1:lda,1:n) )

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0D+00
      else if ( i == j - 1 ) then
        a(i,j) = -1.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  allocate ( x(1:n) )
  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do
  incx = 1

  allocate ( y(1:n) )
  do i = 1, n
    y(i) = real ( 10 * i, kind = 8 )
  end do
  incy = 1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A:'
  write ( *, '(a)' ) ''
  do i = 1, n
    do j = 1, i - 1
      write ( *, '(f8.2)', advance = 'no' ) a(j,i)
    end do
    do j = i, n
      write ( *, '(f8.2)', advance = 'no' ) a(i,j)
    end do
    write ( *, '(a)', advance = 'yes' )
  end do

  call r8vec_print ( n, x, '  Vector X:' )
  call r8vec_print ( n, y, '  Vector y:' )

  call dsyr2 ( uplo, n, alpha, x, incx, y, incy, a, lda )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A = A + alpha*x*y'' + alpha*y*x'':'
  write ( *, '(a)' ) ''
  do i = 1, n
    do j = 1, i - 1
      write ( *, '(f8.2)', advance = 'no' ) a(j,i)
    end do
    do j = i, n
      write ( *, '(f8.2)', advance = 'no' ) a(i,j)
    end do
    write ( *, '(a)', advance = 'yes' )
  end do
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine dtbmv_test ( )

!*****************************************************************************80
!
!! DTBMV_TEST tests DTBMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 1
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: lda = k + 1

  real ( kind = 8 ), allocatable :: a(:,:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ij
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  character trans
  character uplo
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DTBMV_TEST'
  write ( *, '(a)' ) '  DTBMV computes x=A*x or x=A''*x'
  write ( *, '(a)' ) '  for a triangular band matrix A.'

  allocate ( a(1:lda,1:n) )
  a(1:lda,1:n) = 0.0D+00

  do ij = 0, k
    do j = k + 1 - ij, n
      if ( ij == 0 ) then
        a(ij+1,j) = -1.0D+00
      else if ( ij == 1 ) then
        a(ij+1,j) = 2.0D+00
      end if
    end do
  end do

  allocate ( x(1:n) )

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  uplo = 'U'
  trans = 'N'
  diag = 'N'
  incx = 1

  call dtbmv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r8vec_print ( n, x, '  x := A*x' )

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  uplo = 'U'
  trans = 'T'
  diag = 'N'
  incx = 1

  call dtbmv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r8vec_print ( n, x, '  x := A''*x' )
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )

  return
end
subroutine dtbsv_test ( )

!*****************************************************************************80
!
!! DTBSV_TEST tests DTBSV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 1
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: lda = k + 1

  real ( kind = 8 ), allocatable :: a(:,:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ij
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  character trans
  character uplo
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DTBSV_TEST'
  write ( *, '(a)' ) '  DTBSV solves A*x=b or A''*x=b'
  write ( *, '(a)' ) '  for a triangular band matrix A.'

  allocate ( a(1:lda,1:n) )
  a(1:lda,1:n) = 0.0D+00

  do ij = 0, k
    do j = k + 1 - ij, n
      if ( ij == 0 ) then
        a(ij+1,j) = -1.0D+00
      else if ( ij == 1 ) then
        a(ij+1,j) = 2.0D+00
      end if
    end do
  end do

  allocate ( x(1:n) )

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  uplo = 'U'
  trans = 'N'
  diag = 'N'
  incx = 1

  call dtbmv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r8vec_print ( n, x, '  b:' )

  call dtbsv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r8vec_print ( n, x, '  solution x so that A*x=b' );

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  uplo = 'U'
  trans = 'T'
  diag = 'N'
  incx = 1

  call dtbmv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r8vec_print ( n, x, '  b:' )

  call dtbsv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r8vec_print ( n, x, '  solution x so that A''*x=b' );
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )

  return
end
subroutine dtpmv_test ( )

!*****************************************************************************80
!
!! DTPMV_TEST tests DTPMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5

  integer ( kind = 4 ), parameter :: lda = m
  integer ( kind = 4 ), parameter :: n = m

  real ( kind = 8 ), allocatable :: ap(:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) test
  character trans
  character uplo
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DTPMV_TEST'
  write ( *, '(a)' ) '  DTPMV computes x := A * x or x := A'' * x'
  write ( *, '(a)' ) '  for a packed triangular matrix A.'

  do test = 1, 2

    uplo = 'U'

    if ( test == 1 ) then
      trans = 'N'
    else
      trans = 'T'
    end if

    diag = 'N'

    allocate ( ap((n*(n+1))/2) )

    k = 0
    do j = 1, n
      do i = 1, j
        k = k + 1
        ap(k) = real ( i + j, kind = 8 )
      end do
    end do

    allocate ( x(1:n) )

    incx = 1
    do i = 1, n
      x(i) = real ( i, kind = 8 )
    end do

    call dtpmv ( uplo, trans, diag, n, ap, x, incx )

    if ( trans == 'N' ) then
      call r8vec_print ( n, x, '  Result y = A * x' );
    else
      call r8vec_print ( n, x, '  Result y = A'' * x' );
    end if
!
!  Free memory.
!
    deallocate ( ap )
    deallocate ( x )

  end do

  return
end
subroutine dtpsv_test ( )

!*****************************************************************************80
!
!! DTPSV_TEST tests DTPSV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5

  integer ( kind = 4 ), parameter :: lda = m
  integer ( kind = 4 ), parameter :: n = m

  real ( kind = 8 ), allocatable :: ap(:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) test
  character trans
  character uplo
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DTPSV_TEST'
  write ( *, '(a)' ) '  DTPSV computes solves A*x=b or A''*x=b'
  write ( *, '(a)' ) '  for a packed triangular matrix A.'

  do test = 1, 2

    uplo = 'U'

    if ( test == 1 ) then
      trans = 'N'
    else
      trans = 'T'
    end if

    diag = 'N'

    allocate ( ap((n*(n+1))/2) )

    k = 0
    do j = 1, n
      do i = 1, j
        k = k + 1
        ap(k) = real ( i + j, kind = 8 )
      end do
    end do

    allocate ( x(1:n) )

    incx = 1
    do i = 1, n
      x(i) = real ( i, kind = 8 )
    end do

    call dtpmv ( uplo, trans, diag, n, ap, x, incx )

    call r8vec_print ( n, x, '  b:' )

    call dtpsv ( uplo, trans, diag, n, ap, x, incx )

    if ( trans == 'N' ) then
      call r8vec_print ( n, x, '  Solution x so that A*x=b' );
    else
      call r8vec_print ( n, x, '  Solution x so that A''*x=b' );
    end if
!
!  Free memory.
!
    deallocate ( ap )
    deallocate ( x )

  end do

  return
end
subroutine dtrmv_test ( )

!*****************************************************************************80
!
!! DTRMV_TEST tests DTRMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5

  integer ( kind = 4 ), parameter :: lda = m
  integer ( kind = 4 ), parameter :: n = m

  real ( kind = 8 ), allocatable :: a(:,:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) test
  character trans
  character uplo
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DTRMV_TEST'
  write ( *, '(a)' ) '  DTRMV computes x := A * x or x := A'' * x'
  write ( *, '(a)' ) '  for a triangular matrix A.'

  do test = 1, 2

    uplo = 'U'

    if ( test == 1 ) then
      trans = 'N'
    else
      trans = 'T'
    end if

    diag = 'N'

    allocate ( a(1:lda,1:n) )

    do j = 1, n
      do i = 1, j
        a(i,j) = real ( i + j, kind = 8 )
      end do
      do i = j + 1, m
        a(i,j) = 0.0D+00
      end do
    end do

    allocate ( x(1:n) )

    incx = 1
    do i = 1, n
      x(i) = real ( i, kind = 8 )
    end do

    call dtrmv ( uplo, trans, diag, n, a, lda, x, incx )

    if ( trans == 'N' ) then
      call r8vec_print ( n, x, '  Result y = A * x' );
    else
      call r8vec_print ( n, x, '  Result y = A'' * x' );
    end if
!
!  Free memory.
!
    deallocate ( a )
    deallocate ( x )

  end do

  return
end
subroutine dtrsv_test ( )

!*****************************************************************************80
!
!! DTRSV_TEST tests DTRSV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5

  integer ( kind = 4 ), parameter :: lda = m
  integer ( kind = 4 ), parameter :: n = m

  real ( kind = 8 ), allocatable :: a(:,:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) test
  character trans
  character uplo
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DTRSV_TEST'
  write ( *, '(a)' ) '  DTRSV solves A * x = b or A'' * x = b'
  write ( *, '(a)' ) '  for a triangular matrix A.'

  do test = 1, 2

    uplo = 'U'

    if ( test == 1 ) then
      trans = 'N'
    else
      trans = 'T'
    end if

    diag = 'N'

    allocate ( a(1:lda,1:n) )

    do j = 1, n
      do i = 1, j
        a(i,j) = real ( i + j, kind = 8 )
      end do
      do i = j + 1, m
        a(i,j) = 0.0D+00
      end do
    end do

    allocate ( x(1:n) )

    incx = 1
    do i = 1, n
      x(i) = real ( i, kind = 8 )
    end do

    call dtrmv ( uplo, trans, diag, n, a, lda, x, incx )
    call r8vec_print ( n, x, '  Right hand side b:' )

    call dtrsv ( uplo, trans, diag, n, a, lda, x, incx )

    if ( trans == 'N' ) then
      call r8vec_print ( n, x, '  Solution x for A * x = b' );
    else
      call r8vec_print ( n, x, '  Solution x for A'' * x = b' );
    end if
!
!  Free memory.
!
    deallocate ( a )
    deallocate ( x )

  end do

  return
end
