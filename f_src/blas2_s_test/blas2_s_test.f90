program main

!*****************************************************************************80
!
!! MAIN is the main program for BLAS2_S_TEST.
!
!  Discussion:
!
!    BLAS2_S_TEST tests the BLAS2_S library.
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
  write ( *, '(a)' ) 'BLAS2_S_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BLAS2_S library.'

  call sgbmv_test ( )
  call sgemv_test ( )
  call sger_test ( )
  call ssbmv_test ( )
  call sspmv_test ( )
  call sspr_test ( )
  call sspr2_test ( )
  call ssymv_test ( )
  call ssyr_test ( )
  call ssyr2_test ( )
  call stbmv_test ( )
  call strsv_test ( )
  call stpmv_test ( )
  call stpsv_test ( )
  call strmv_test ( )
  call strsv_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS2_S_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine sgbmv_test ( )

!*****************************************************************************80
!
!! SGBMV_TEST tests SGBMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 2014
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

  real ( kind = 4 ) a(lda,n)
  real ( kind = 4 ) alpha
  real ( kind = 4 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character trans
  real ( kind = 4 ) x(n)
  real ( kind = 4 ) y(m)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SGBMV_TEST'
  write ( *, '(a)' ) '  SGBMV computes y := alpha * A * x + beta * y'
  write ( *, '(a)' ) '  For a general band matrix A.'

  trans = 'N'
  alpha = 2.0E+00
  incx = 1
  beta = 3.0E+00
  incy = 1

  do i = 1, m

    jlo = max ( 1, i - kl )
    jhi = min ( n, i + ku )

    do j = jlo, jhi

      if ( i == j ) then
        a(ku+1+i-j,j) = 2.0E+00
      else if ( i == j - 1 .or. i == j + 1 ) then
        a(ku+1+i-j,j) = -1.0E+00
      else
        a(ku+1+i-j,j) = 0.0E+00
      end if

    end do
  end do

  do i = 1, n
    x(i) = real ( i, kind = 4 )
  end do

  do i = 1, m
    y(i) = real ( 10 * i, kind = 4 )
  end do

  call sgbmv ( trans, m, n, kl, ku, alpha, a, lda, x, incx, beta, y, incy )

  call r4vec_print ( m, y, '  Result vector y' )

  return
end
subroutine sgemv_test ( )

!*****************************************************************************80
!
!! SGEMV_TEST tests SGEMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 4 ), allocatable :: a(:,:)
  real ( kind = 4 ) alpha
  real ( kind = 4 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character trans
  real ( kind = 4 ), allocatable :: x(:)
  real ( kind = 4 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SGEMV_TEST'
  write ( *, '(a)' ) '  SGEMV computes y := alpha * A * x + beta * y'
  write ( *, '(a)' ) '  or             y := alpha * A'' * x + beta * y,'
  write ( *, '(a)' ) '  for a general matrix A.'
!
!  y = alpha * A * x + beta * y
!
  trans = 'N'
  m = 5
  n = 4
  alpha = 2.0E+00
  lda = m
  allocate ( a(1:m,1:n) )
  call r4mat_test ( trans, lda, m, n, a )
  allocate ( x(1:n) )
  do i = 1, n
    x(i) = real ( i, kind = 4 )
  end do
  incx = 1
  beta = 3.0E+00
  allocate ( y(1:m) )
  do i = 1, m
    y(i) = real ( 10 * i, kind = 4 )
  end do
  incy = 1

  call r4mat_print ( m, n, a, '  Matrix A:' )
  call r4vec_print ( n, x, '  Vector X:' )
  call r4vec_print ( m, y, '  Vector Y:' )

  call sgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )

  call r4vec_print ( m, y, '  Result Y = alpha * A  * x + beta * y' )

  deallocate ( a )
  deallocate ( x )
  deallocate ( y )
!
!  y = alpha * A' * x + beta * y
!
  trans = 'T'
  m = 5
  n = 4
  alpha = 2.0E+00
  lda = m
  allocate ( a(1:m,1:n) )
  call r4mat_test ( trans, lda, n, m, a )
  allocate ( x(1:m) )
  do i = 1, m
    x(i) = real ( i, kind = 4 )
  end do
  incx = 1
  beta = 3.0E+00
  allocate ( y(1:n) )
  do i = 1, n
    y(i) = real ( 10 * i, kind = 4 )
  end do
  incy = 1

  call r4mat_print ( m, n, a, '  Matrix A:' )
  call r4vec_print ( m, x, '  Vector X:' )
  call r4vec_print ( n, y, '  Vector Y:' )

  call sgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )

  call r4vec_print ( n, y, '  Result Y = alpha * A'' * x + beta * y' )

  deallocate ( a )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine sger_test ( )

!*****************************************************************************80
!
!! SGER_TEST tests SGER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 4 ), allocatable :: a(:,:)
  real ( kind = 4 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character trans
  real ( kind = 4 ), allocatable :: x(:)
  real ( kind = 4 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SGER_TEST'
  write ( *, '(a)' ) '  SGER computes A := A + alpha * x * y'''
  write ( *, '(a)' ) '  for a general matrix A.'

  m = 5
  n = 4
  alpha = 2.0E+00
  trans = 'N'
  lda = m
  allocate ( a(1:m,1:n) )
  call r4mat_test ( trans, lda, m, n, a )

  allocate ( x(1:m) )
  do i = 1, m
    x(i) = real ( i, kind = 4 )
  end do
  incx = 1

  allocate ( y(1:n) )
  do i = 1, n
    y(i) = real ( 10 * i, kind = 4 )
  end do
  incy = 1

  call r4mat_print ( m, n, a, '  Matrix A:' )
  call r4vec_print ( m, x, '  Vector X:' )
  call r4vec_print ( n, y, '  Vector Y:' )

  call sger ( m, n, alpha, x, incx, y, incy, a, lda )

  call r4mat_print ( m, n, a, '  Result A = A + alpha * x * y' )

  deallocate ( a )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine ssbmv_test ( )

!*****************************************************************************80
!
!! SSBMV_TEST tests SSBMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 2014
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

  real ( kind = 4 ) a(lda,n)
  real ( kind = 4 ) alpha
  real ( kind = 4 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  character uplo
  real ( kind = 4 ) x(n)
  real ( kind = 4 ) y(m)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SSBMV_TEST'
  write ( *, '(a)' ) '  SSBMV computes y := alpha * A * x + beta * y'
  write ( *, '(a)' ) '  for a symmetric band matrix A.'

  uplo = 'U'
  alpha = 2.0E+00
  incx = 1
  beta = 3.0E+00
  incy = 1

  do i = 1, m

    jhi = min ( n, i + k )

    do j = i, jhi

      if ( i == j ) then
        a(k+1+i-j,j) = 2.0E+00
      else if ( i == j - 1 ) then
        a(k+1+i-j,j) = -1.0E+00
      else
        a(k+1+i-j,j) = 0.0E+00
      end if

    end do
  end do

  do i = 1, n
    x(i) = real ( i, kind = 4 )
  end do

  do i = 1, m
    y(i) = real ( 10 * i, kind = 4 )
  end do

  call ssbmv ( uplo, n, k, alpha, a, lda, x, incx, beta, y, incy )

  call r4vec_print ( m, y, '  Result vector y:' )

  return
end
subroutine sspmv_test ( )

!*****************************************************************************80
!
!! SSPMV_TEST tests SSPMV.
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

  real ( kind = 4 ), allocatable :: ap(:)
  real ( kind = 4 ) alpha
  real ( kind = 4 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character uplo
  real ( kind = 4 ), allocatable :: x(:)
  real ( kind = 4 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SSPMV_TEST'
  write ( *, '(a)' ) '  SSPMV computes y := alpha * A * x + beta * y'
  write ( *, '(a)' ) '  for a symmetric packed band matrix A.'

  uplo = 'U'
  alpha = 2.0E+00
  incx = 1
  beta = 3.0E+00
  incy = 1

  allocate ( ap((n*(n+1))/2) )

  k = 0

  do j = 1, n

    do i = 1, j

      k = k + 1
      if ( i == j ) then
        ap(k) = 2.0E+00
      else if ( i == j - 1 ) then
        ap(k) = -1.0E+00
      else
        ap(k) = 0.0E+00
      end if

    end do
  end do

  allocate ( x(1:n) )

  do i = 1, n
    x(i) = real ( i, kind = 4 )
  end do

  allocate ( y(1:n) )

  do i = 1, n
    y(i) = real ( 10 * i, kind = 4 )
  end do

  call sspmv ( uplo, n, alpha, ap, x, incx, beta, y, incy )

  call r4vec_print ( n, y, '  Result Y:' )
!
!  Free memory.
!
  deallocate ( ap )
  deallocate ( x )
  deallocate ( y )

  return
end
subroutine sspr_test ( )

!*****************************************************************************80
!
!! SSPR_TEST tests SSPR.
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

  real ( kind = 4 ), allocatable :: ap(:)
  real ( kind = 4 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character uplo
  real ( kind = 4 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SSPR_TEST'
  write ( *, '(a)' ) '  SSPR computes A := A + alpha * x * x'''
  write ( *, '(a)' ) '  for a symmetric packed band matrix A.'

  uplo = 'U'
  alpha = 2.0E+00
  incx = 1

  allocate ( ap((n*(n+1))/2) )

  k = 0

  do j = 1, n

    do i = 1, j

      k = k + 1
      if ( i == j ) then
        ap(k) = 2.0E+00
      else if ( i == j - 1 ) then
        ap(k) = -1.0E+00
      else
        ap(k) = 0.0E+00
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
    x(i) = real ( i, kind = 4 )
  end do

  call sspr ( uplo, n, alpha, x, incx, ap )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A = A + x*x'':'
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
subroutine sspr2_test ( )

!*****************************************************************************80
!
!! SSPR2_TEST tests SSPR2.
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

  real ( kind = 4 ), allocatable :: ap(:)
  real ( kind = 4 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character uplo
  real ( kind = 4 ), allocatable :: x(:)
  real ( kind = 4 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SSPR2_TEST'
  write ( *, '(a)' ) '  SSPR2 computes A := A + alpha*x*y''+alpha*y*x'''
  write ( *, '(a)' ) '  for a symmetric packed band matrix A.'

  uplo = 'U'
  alpha = 2.0E+00
  incx = 1
  incy = 1

  allocate ( ap((n*(n+1))/2) )

  k = 0

  do j = 1, n

    do i = 1, j

      k = k + 1
      if ( i == j ) then
        ap(k) = 2.0E+00
      else if ( i == j - 1 ) then
        ap(k) = -1.0E+00
      else
        ap(k) = 0.0E+00
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
    x(i) = real ( i, kind = 4 )
  end do

  allocate ( y(1:n) )

  do i = 1, n
    y(i) = real ( 10 * i, kind = 4 )
  end do

  call sspr2 ( uplo, n, alpha, x, incx, y, incy, ap )

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
subroutine ssymv_test ( )

!*****************************************************************************80
!
!! SSYMV_TEST tests SSYMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: lda = n

  real ( kind = 4 ) a(lda,n)
  real ( kind = 4 ) alpha
  real ( kind = 4 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  character uplo
  real ( kind = 4 ) x(n)
  real ( kind = 4 ) y(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SSYMV_TEST'
  write ( *, '(a)' ) '  SSYMV computes y := alpha * A * x + beta * y'
  write ( *, '(a)' ) '  for a general symmetric matrix A.'

  uplo = 'U'
  alpha = 2.0E+00
  incx = 1
  beta = 3.0E+00
  incy = 1

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0E+00
      else if ( i == j - 1 ) then
        a(i,j) = -1.0E+00
      else
        a(i,j) = 0.0E+00
      end if
    end do
  end do

  do i = 1, n
    x(i) = real ( i, kind = 4 )
  end do

  do i = 1, n
    y(i) = real ( 10 * i, kind = 4 )
  end do

  call ssymv ( uplo, n, alpha, a, lda, x, incx, beta, y, incy )

  call r4vec_print ( n, y, '  Result vector y:' )

  return
end
subroutine ssyr_test ( )

!*****************************************************************************80
!
!! SSYR_TEST tests SSYR.
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

  real ( kind = 4 ), allocatable :: a(:,:)
  real ( kind = 4 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n
  character uplo
  real ( kind = 4 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SSYR_TEST'
  write ( *, '(a)' ) '  SSYR computes A := A + alpha * x * x'''
  write ( *, '(a)' ) '  for a symmetric matrix A.'

  uplo = 'U'
  n = 5
  alpha = 2.0E+00
  lda = n

  allocate ( a(1:lda,1:n) )

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0E+00
      else if ( i == j - 1 ) then
        a(i,j) = -1.0E+00
      else
        a(i,j) = 0.0E+00
      end if
    end do
  end do

  allocate ( x(1:n) )
  do i = 1, n
    x(i) = real ( i, kind = 4 )
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

  call r4vec_print ( n, x, '  Vector X:' )

  call ssyr ( uplo, n, alpha, x, incx, a, lda )

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
subroutine ssyr2_test ( )

!*****************************************************************************80
!
!! SSYR2_TEST tests SSYR2.
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

  real ( kind = 4 ), allocatable :: a(:,:)
  real ( kind = 4 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n
  character uplo
  real ( kind = 4 ), allocatable :: x(:)
  real ( kind = 4 ), allocatable :: y(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SSYR2_TEST'
  write ( *, '(a)' ) '  SSYR2 computes A := A + alpha*x*y'' + alpha*y*x'''
  write ( *, '(a)' ) '  for a symmetric matrix A.'

  uplo = 'U'
  n = 5
  alpha = 2.0E+00
  lda = n

  allocate ( a(1:lda,1:n) )

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0E+00
      else if ( i == j - 1 ) then
        a(i,j) = -1.0E+00
      else
        a(i,j) = 0.0E+00
      end if
    end do
  end do

  allocate ( x(1:n) )
  do i = 1, n
    x(i) = real ( i, kind = 4 )
  end do
  incx = 1

  allocate ( y(1:n) )
  do i = 1, n
    y(i) = real ( 10 * i, kind = 4 )
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

  call r4vec_print ( n, x, '  Vector X:' )
  call r4vec_print ( n, y, '  Vector y:' )

  call ssyr2 ( uplo, n, alpha, x, incx, y, incy, a, lda )

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
subroutine stbmv_test ( )

!*****************************************************************************80
!
!! STBMV_TEST tests STBMV.
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

  real ( kind = 4 ), allocatable :: a(:,:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ij
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  character trans
  character uplo
  real ( kind = 4 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'STBMV_TEST'
  write ( *, '(a)' ) '  STBMV computes x=A*x or x=A''*x'
  write ( *, '(a)' ) '  for a triangular band matrix A.'

  allocate ( a(1:lda,1:n) )
  a(1:lda,1:n) = 0.0E+00

  do ij = 0, k
    do j = k + 1 - ij, n
      if ( ij == 0 ) then
        a(ij+1,j) = -1.0E+00
      else if ( ij == 1 ) then
        a(ij+1,j) = 2.0E+00
      end if
    end do
  end do

  allocate ( x(1:n) )

  do i = 1, n
    x(i) = real ( i, kind = 4 )
  end do

  uplo = 'U'
  trans = 'N'
  diag = 'N'
  incx = 1

  call stbmv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r4vec_print ( n, x, '  x := A*x' );

  do i = 1, n
    x(i) = real ( i, kind = 4 )
  end do

  uplo = 'U'
  trans = 'T'
  diag = 'N'
  incx = 1

  call stbmv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r4vec_print ( n, x, '  x := A''*x' );
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )

  return
end
subroutine stbsv_test ( )

!*****************************************************************************80
!
!! STBSV_TEST tests STBSV.
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

  real ( kind = 4 ), allocatable :: a(:,:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ij
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  character trans
  character uplo
  real ( kind = 4 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'STBSV_TEST'
  write ( *, '(a)' ) '  STBSV solves A*x=b or A''*x=b'
  write ( *, '(a)' ) '  for a triangular band matrix A.'

  allocate ( a(1:lda,1:n) )
  a(1:lda,1:n) = 0.0E+00

  do ij = 0, k
    do j = k + 1 - ij, n
      if ( ij == 0 ) then
        a(ij+1,j) = -1.0E+00
      else if ( ij == 1 ) then
        a(ij+1,j) = 2.0E+00
      end if
    end do
  end do

  allocate ( x(1:n) )

  do i = 1, n
    x(i) = real ( i, kind = 4 )
  end do

  uplo = 'U'
  trans = 'N'
  diag = 'N'
  incx = 1

  call stbmv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r4vec_print ( n, x, '  b:' )

  call stbsv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r4vec_print ( n, x, '  solution x so that A*x=b' );

  do i = 1, n
    x(i) = real ( i, kind = 4 )
  end do

  uplo = 'U'
  trans = 'T'
  diag = 'N'
  incx = 1

  call stbmv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r4vec_print ( n, x, '  b:' )

  call stbsv ( uplo, trans, diag, n, k, a, lda, x, incx )

  call r4vec_print ( n, x, '  solution x so that A''*x=b' );
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )

  return
end
subroutine stpmv_test ( )

!*****************************************************************************80
!
!! STPMV_TEST tests STPMV.
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

  real ( kind = 4 ), allocatable :: ap(:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) test
  character trans
  character uplo
  real ( kind = 4 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'STPMV_TEST'
  write ( *, '(a)' ) '  STPMV computes x := A * x or x := A'' * x'
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
        ap(k) = real ( i + j, kind = 4 )
      end do
    end do

    allocate ( x(1:n) )

    incx = 1
    do i = 1, n
      x(i) = real ( i, kind = 4 )
    end do

    call stpmv ( uplo, trans, diag, n, ap, x, incx )

    if ( trans == 'N' ) then
      call r4vec_print ( n, x, '  Result y = A * x' );
    else
      call r4vec_print ( n, x, '  Result y = A'' * x' );
    end if
!
!  Free memory.
!
    deallocate ( ap )
    deallocate ( x )

  end do

  return
end
subroutine stpsv_test ( )

!*****************************************************************************80
!
!! STPSV_TEST tests STPSV.
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

  real ( kind = 4 ), allocatable :: ap(:)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) test
  character trans
  character uplo
  real ( kind = 4 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'STPSV_TEST'
  write ( *, '(a)' ) '  STPSV computes solves A*x=b or A''*x=b'
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
        ap(k) = real ( i + j, kind = 4 )
      end do
    end do

    allocate ( x(1:n) )

    incx = 1
    do i = 1, n
      x(i) = real ( i, kind = 4 )
    end do

    call stpmv ( uplo, trans, diag, n, ap, x, incx )

    call r4vec_print ( n, x, '  b:' )

    call stpsv ( uplo, trans, diag, n, ap, x, incx )

    if ( trans == 'N' ) then
      call r4vec_print ( n, x, '  Solution x so that A*x=b' );
    else
      call r4vec_print ( n, x, '  Solution x so that A''*x=b' );
    end if
!
!  Free memory.
!
    deallocate ( ap )
    deallocate ( x )

  end do

  return
end
subroutine strmv_test ( )

!*****************************************************************************80
!
!! STRMV_TEST tests STRMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5

  integer ( kind = 4 ), parameter :: lda = m
  integer ( kind = 4 ), parameter :: n = m

  real ( kind = 4 ) a(lda,n)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) test
  character trans
  character uplo
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'STRMV_TEST'
  write ( *, '(a)' ) '  STRMV computes y := A * x or y := A'' * x'
  write ( *, '(a)' ) '  for a triangular matrix A.'

  do test = 1, 2

    uplo = 'U'

    if ( test == 1 ) then
      trans = 'N'
    else
      trans = 'T'
    end if

    diag = 'N'

    do j = 1, n
      do i = 1, j
        a(i,j) = real ( i + j, kind = 4 )
      end do
      do i = j + 1, m
        a(i,j) = 0.0E+00
      end do
    end do

    incx = 1
    do i = 1, n
      x(i) = real ( i, kind = 4 )
    end do

    call strmv ( uplo, trans, diag, n, a, lda, x, incx )

    if ( trans == 'N' ) then
      call r4vec_print ( n, x, '  Result y = A * x' );
    else
      call r4vec_print ( n, x, '  Result y = A'' * x' );
    end if

  end do

  return
end
subroutine strsv_test ( )

!*****************************************************************************80
!
!! STRSV_TEST tests STRSV.
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

  real ( kind = 4 ) a(lda,n)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  integer ( kind = 4 ) test
  character trans
  character uplo
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'STRSV_TEST'
  write ( *, '(a)' ) '  STRSV solves A * x = b or A'' * x = b'
  write ( *, '(a)' ) '  for a triangular matrix A.'

  do test = 1, 2

    uplo = 'U'

    if ( test == 1 ) then
      trans = 'N'
    else
      trans = 'T'
    end if

    diag = 'N'

    do j = 1, n
      do i = 1, j
        a(i,j) = real ( i + j, kind = 4 )
      end do
      do i = j + 1, m
        a(i,j) = 0.0E+00
      end do
    end do

    incx = 1
    do i = 1, n
      x(i) = real ( i, kind = 4 )
    end do
    call strmv ( uplo, trans, diag, n, a, lda, x, incx )
    call r4vec_print ( n, x, '  Right hand side b:' );

    call strsv ( uplo, trans, diag, n, a, lda, x, incx )

    if ( trans == 'N' ) then
      call r4vec_print ( n, x, '  Solution x of A * x = b' );
    else
      call r4vec_print ( n, x, '  Solution x of A'' * x = b' );
    end if

  end do

  return
end
