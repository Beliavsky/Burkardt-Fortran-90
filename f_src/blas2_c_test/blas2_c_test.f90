program main

!*****************************************************************************80
!
!! MAIN is the main program for BLAS2_C_TEST.
!
!  Discussion:
!
!    BLAS2_C_TEST tests the BLAS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS2_C_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BLAS library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BLAS2_C_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests CGEMV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5

  complex ( kind = 4 ) a(m,n)
  complex ( kind = 4 ) alpha
  complex ( kind = 4 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lda
  character trans
  complex ( kind = 4 ) x(n)
  real ( kind = 4 ) x1
  real ( kind = 4 ) x2
  complex ( kind = 4 ) y(m)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  For a general matrix A,'
  write ( *, '(a)' ) '  CGEMV computes y := alpha * A * x + beta * y'

  trans = 'N'
  alpha = ( 10.0E+00, 1.0E+00 ) 
  lda = m
  incx = 1
  beta = 3.0E+00
  incy = 1

  do i = 1, m
    do j = 1, n
      if ( i == j ) then
        a(i,j) = ( 2.0E+00, 0.0E+00 )
      else if ( i == j - 1 .or. i == j + 1 ) then
        a(i,j) = ( -1.0E+00, 0.0E+00 )
      else
        a(i,j) = ( 0.0E+00, 0.0E+00 )
      end if
    end do
  end do

  x1 = 0.0E+00
  x2 = real ( n, kind = 4 )
  do i = 1, n
    x(i) = cmplx ( x1, x2, kind = 4 )
    x1 = x1 + 1.0E+00
    x2 = x2 - 2.0E+00
  end do

  do i = 1, m
    y(i) = ( 100.0E+00, 1.0E+00 )
  end do

  call cgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Result vector Y = '
  write ( *, '(a)' ) ' '

  do i = 1, m
    write ( *, '(2x,2g14.6)' ) y(i)
  end do

  return
end

