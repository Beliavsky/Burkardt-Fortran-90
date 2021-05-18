program main

!*****************************************************************************80
!
!! MAIN is the main program for BLAS1_D_TEST.
!
!  Discussion:
!
!    BLAS1_D_TEST tests the BLAS1_D library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS1_D_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BLAS1_D library.'

  call dasum_test ( )
  call daxpy_test ( )
  call dcopy_test ( )
  call ddot_test ( )
  call dnrm2_test ( )
  call drot_test ( )
  call drotg_test ( )
  call dscal_test ( )
  call dswap_test ( )
  call idamax_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS1_D_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'

  stop 0
end
subroutine dasum_test ( )

!*****************************************************************************80
!
!! DASUM_TEST tests DASUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: lda = 6
  integer ( kind = 4 ), parameter :: ma = 5
  integer ( kind = 4 ), parameter :: na = 4
  integer ( kind = 4 ), parameter :: nx = 10

  real ( kind = 8 ) a(lda,na)
  real ( kind = 8 ) dasum
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(nx)

  do i = 1, nx
    x(i) = ( -1.0D+00 )**i * real ( 2 * i, kind = 8 )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DASUM_TEST'
  write ( *, '(a)' ) '  DASUM adds the absolute values of elements'
  write ( *, '(a)' ) '  of a double precision real vector.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  X = '
  write ( *, '(a)' ) ' '
  do i = 1, nx
    write ( *, '(2x,i6,g14.6)' ) i, x(i)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  DASUM ( NX, X, 1 ) =   ', dasum ( nx, x, 1 )
  write ( *, '(a,g14.6)' ) '  DASUM ( NX/2, X, 2 ) = ', dasum ( nx/2, x, 2 )
  write ( *, '(a,g14.6)' ) '  DASUM ( 2, X, NX/2 ) = ', dasum ( 2, x, nx/2 )

  a(1:lda,1:na) = 0.0D+00

  do i = 1, ma
    do j = 1, na
      a(i,j) = ( -1.0D+00 )**( i + j ) * real ( 10 * i + j, kind = 8 )
    end do
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Demonstrate with a matrix A:'
  write ( *, '(a)' ) ' '
  do i = 1, ma
    write ( *, '(2x,5g14.6)' ) a(i,1:na)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  DASUM(MA,A(1,2),1) =   ', dasum ( ma, a(1,2), 1 )
  write ( *, '(a,g14.6)' ) &
    '  DASUM(NA,A(2,1),LDA) = ', dasum ( na, a(2,1), lda )

  return
end
subroutine daxpy_test ( )

!*****************************************************************************80
!
!! DAXPY_TEST tests DAXPY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ) da
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  do i = 1, n
    y(i) = real ( 100 * i, kind = 8 )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DAXPY_TEST'
  write ( *, '(a)' ) '  DAXPY adds a multiple of a double precision real'
  write ( *, '(a)' ) '  vector X to vector Y.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  X = '
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6)' ) i, x(i)
  end do
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Y = '
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6)' ) i, y(i)
  end do

  da = 1.0D+00
  call daxpy ( n, da, x, 1, y, 1 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a)' ) '  DAXPY ( N, ', da, ', X, 1, Y, 1 )'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6)' ) i, y(i)
  end do

  do i = 1, n
    y(i) = real ( 100 * i, kind = 8 )
  end do

  da = -2.0D+00
  call daxpy ( n, da, x, 1, y, 1 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a)' ) '  DAXPY ( N, ', da, ', X, 1, Y, 1 )'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6)' ) i, y(i)
  end do

  do i = 1, n
    y(i) = real ( 100 * i, kind = 8 )
  end do

  da = +3.0D+00
  call daxpy ( 3, da, x, 2, y, 1 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a)' ) '  DAXPY ( 3, ', da, ', X, 2, Y, 1 )'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6)' ) i, y(i)
  end do

  do i = 1, n
    y(i) = real ( 100 * i, kind = 8 )
  end do

  da = -4.0D+00
  call daxpy ( 3, da, x, 1, y, 2 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a)' ) '  DAXPY ( 3, ', da, ', X, 1, Y, 2 )'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6)' ) i, y(i)
  end do

  return
end
subroutine dcopy_test ( )

!*****************************************************************************80
!
!! DCOPY_TEST tests DCOPY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(5,5)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(10)
  real ( kind = 8 ) y(10)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DCOPY_TEST'
  write ( *, '(a)' ) '  DCOPY copies one double precision real vector'
  write ( *, '(a)' ) '  into another.'

  do i = 1, 10
    x(i) = real ( i, kind = 8 )
  end do

  do i = 1, 10
    y(i) = real ( 10 * i, kind = 8 )
  end do

  do i = 1, 5
    do j = 1, 5
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  X = '
  write ( *, '(a)' ) ' '
  do i = 1, 10
    write ( *, '(2x,i6,g14.6)' ) i, x(i)
  end do
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Y = '
  write ( *, '(a)' ) ' '
  do i = 1, 10
    write ( *, '(2x,i6,g14.6)' ) i, y(i)
  end do
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  A = '
  write ( *, '(a)' ) ' '
  do i = 1, 5
    write ( *, '(2x,5f8.2)' ) a(i,1:5)
  end do

  call dcopy ( 5, x, 1, y, 1 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a)' ) '  DCOPY ( 5, X, 1, Y, 1 )'
  write ( *, '(a)' ) ' '
  do i = 1, 10
    write ( *, '(2x,i6,g14.6)' ) i, y(i)
  end do

  do i = 1, 10
    y(i) = real ( 10 * i, kind = 8 )
  end do

  call dcopy ( 3, x, 2, y, 3 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a)' ) '  DCOPY ( 3, X, 2, Y, 3 )'
  write ( *, '(a)' ) ' '
  do i = 1, 10
    write ( *, '(2x,i6,g14.6)' ) i, y(i)
  end do

  call dcopy ( 5, x, 1, a, 1 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a)' ) '  DCOPY ( 5, X, 1, A, 1 )'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  A = '
  write ( *, '(a)' ) ' '
  do i = 1, 5
    write ( *, '(2x,5f8.2)' ) a(i,1:5)
  end do

  do i = 1, 5
    do j = 1, 5
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call dcopy ( 5, x, 2, a, 5 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a)' ) '  DCOPY ( 5, X, 2, A, 5 )'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  A = '
  write ( *, '(a)' ) ' '
  do i = 1, 5
    write ( *, '(2x,5f8.2)' ) a(i,1:5)
  end do

  return
end
subroutine ddot_test ( )

!*****************************************************************************80
!
!! DDOT_TEST tests DDOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: lda = 10
  integer ( kind = 4 ), parameter :: ldb = 7
  integer ( kind = 4 ), parameter :: ldc = 6

  real ( kind = 8 ) a(lda,lda)
  real ( kind = 8 ) b(ldb,ldb)
  real ( kind = 8 ) c(ldc,ldc)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) ddot
  real ( kind = 8 ) sum1
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DDOT_TEST'
  write ( *, '(a)' ) '  DDOT computes the dot product of two'
  write ( *, '(a)' ) '  double precision real vectors.'

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  do i = 1, n
    y(i) = - real ( i, kind = 8 )
  end do

  do i = 1, n
    do j = 1, n
      a(i,j) = real ( i + j, kind = 8 )
    end do
  end do

  do i = 1, n
    do j = 1, n
      b(i,j) = real ( i - j, kind = 8 )
    end do
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  X = '
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6)' ) i, x(i)
  end do
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Y = '
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6)' ) i, y(i)
  end do
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  A = '
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,10f8.2)' ) a(i,1:n)
  end do
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  B = '
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,10f8.2)' ) b(i,1:n)
  end do
!
!  To compute a simple dot product of two vectors, use a
!  call like this:
!
  sum1 = ddot ( n, x, 1, y, 1 )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Dot product of X and Y is ', sum1
!
!  To multiply a ROW of a matrix A times a vector X, we need to
!  specify the increment between successive entries of the row of A:
!
  sum1 = ddot ( n, a(2,1), lda, x, 1 )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Product of row 2 of A and X is ', sum1
!
!  Product of a column of A and a vector is simpler:
!
  sum1 = ddot ( n, a(1,2), 1, x, 1 )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Product of column 2 of A and X is ', sum1
!
!  Here's how matrix multiplication, c = a*b, could be done
!  with DDOT:
!
  do i = 1, n
    do j = 1, n
      c(i,j) = ddot ( n, a(i,1), lda, b(1,j), 1 )
    end do
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Matrix product A*B computed with DDOT:'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,5g14.6)' ) c(i,1:n)
  end do

  return
end
subroutine dnrm2_test ( )

!*****************************************************************************80
!
!! DNRM2_TEST tests DNRM2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: lda = n + 5
!
!  These parameters illustrate the fact that matrices are typically
!  dimensioned with more space than the user requires.
!
  real ( kind = 8 ) a(lda,lda)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) j
  real ( kind = 8 ) dnrm2
  real ( kind = 8 ) sum1
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DNRM2_TEST'
  write ( *, '(a)' ) '  DNRM2 computes the Euclidean norm of a '
  write ( *, '(a)' ) '  double precision real vector.'
!
!  Compute the euclidean norm of a vector:
!
  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The vector X:'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,f8.4)' ) i, x(i)
  end do
  incx = 1

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The 2-norm of X is ', dnrm2 ( n, x, incx )
!
!  Compute the euclidean norm of a row or column of a matrix:
!
  do i = 1, n
    do j = 1, n
      a(i,j) = real ( i + j, kind = 8 )
    end do
  end do

  incx = lda
  sum1 = dnrm2 ( n, a(2,1), incx )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The 2-norm of row 2 of A is ', sum1

  incx = 1
  sum1 = dnrm2 ( n, a(1,2), incx )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The 2-norm of column 2 of A is ', sum1

  return
end
subroutine drot_test ( )

!*****************************************************************************80
!
!! DROT_TEST tests DROT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) s
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  do i = 1, n
    y(i) = real ( i * i - 12, kind = 8 )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DROT_TEST'
  write ( *, '(a)' ) &
    '  DROT carries out a double precision real Givens rotation.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  X and Y'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6,g14.6)' ) i, x(i), y(i)
  end do

  c = 0.5D+00
  s = sqrt ( 1.0D+00 - c * c )
  call drot ( n, x, 1, y, 1, c, s )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a,f8.4,a)' ) '  DROT ( N, X, 1, Y, 1, ', c, ',', s, ' )'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6,g14.6)' ) i, x(i), y(i)
  end do

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  do i = 1, n
    y(i) = real ( i * i - 12, kind = 8 )
  end do

  c = x(1) / sqrt ( x(1) * x(1) + y(1) * y(1) )
  s = y(1) / sqrt ( x(1) * x(1) + y(1) * y(1) )
  call drot ( n, x, 1, y, 1, c, s )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a,f8.4,a)' ) '  DROT ( N, X, 1, Y, 1, ', c, ',', s, ' )'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6,g14.6)' ) i, x(i), y(i)
  end do

  return
end
subroutine drotg_test ( )

!*****************************************************************************80
!
!! DROTG_TEST tests DROTG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) sa
  real ( kind = 8 ) sb
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 5
  real ( kind = 8 ) z

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DROTG_TEST'
  write ( *, '(a)' ) '  DROTG generates a double precision real Givens rotation'
  write ( *, '(a)' ) '    (  C  S ) * ( A ) = ( R )'
  write ( *, '(a)' ) '    ( -S  C )   ( B )   ( 0 )'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    call random_number ( a )
    call random_number ( b )

    sa = a
    sb = b

    call drotg ( sa, sb, c, s )

    r = sa
    z = sb

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6,a,g14.6)' ) '  A =  ', a,  '  B =  ', b
    write ( *, '(a,g14.6,a,g14.6)' ) '  C =  ', c,  '  S =  ', s
    write ( *, '(a,g14.6,a,g14.6)' ) '  R =  ', r,  '  Z =  ', z
    write ( *, '(a,g14.6)' ) '   C*A+S*B = ',  c * a + s * b
    write ( *, '(a,g14.6)' ) '  -S*A+C*B = ', -s * a + c * b

  end do

  return
end
subroutine dscal_test ( )

!*****************************************************************************80
!
!! DSCAL_TEST tests DSCAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ) da
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DSCAL_TEST'
  write ( *, '(a)' ) '  DSCAL multiplies a double precision real scalar '
  write ( *, '(a)' ) '  times a double precision real vector.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  X = '
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6)' ) i, x(i)
  end do

  da = 5.0D+00
  call dscal ( n, da, x, 1 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a)' ) '  DSCAL ( N, ', da, ', X, 1 )'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6)' ) i, x(i)
  end do

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  da = -2.0D+00
  call dscal ( 3, da, x, 2 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a)' ) '  DSCAL ( 3, ', da, ', X, 2 )'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6)' ) i, x(i)
  end do

  return
end
subroutine dswap_test ( )

!*****************************************************************************80
!
!! DSWAP_TEST tests DSWAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2006
!
!  Author:
!
!    John Burkardt
!  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  do i = 1, n
    y(i) = real ( 100 * i, kind = 8 )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DSWAP_TEST'
  write ( *, '(a)' ) '  DSWAP swaps two double precision real vectors.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  X and Y'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6,g14.6)' ) i, x(i), y(i)
  end do

  call dswap ( n, x, 1, y, 1 )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  DSWAP ( N, X, 1, Y, 1 )'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  X and Y'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6,g14.6)' ) i, x(i), y(i)
  end do

  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  do i = 1, n
    y(i) = real ( 100 * i, kind = 8 )
  end do

  call dswap ( 3, x, 2, y, 1 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.4,a)' ) '  DSWAP ( 3, X, 2, Y, 1 )'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  X and Y'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,g14.6,g14.6)' ) i, x(i), y(i)
  end do

  return
end
subroutine idamax_test ( )

!*****************************************************************************80
!
!! IDAMAX_TEST tests IDAMAX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) idamax
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'IDAMAX_TEST'
  write ( *, '(a)' ) '  IDAMAX returns the index of the entry of'
  write ( *, '(a)' ) '  maximum magnitude in a double precision real vector.'

  do i = 1, n
    x(i) = real ( mod ( 7 * i, 11 ) - 5, kind = 8 )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The vector X:'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i6,f8.4)' ) i, x(i)
  end do

  incx = 1

  i1 = idamax ( n, x, incx )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  The index of maximum magnitude = ', i1

  return
end

