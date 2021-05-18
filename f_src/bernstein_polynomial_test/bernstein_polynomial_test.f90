program main

!*****************************************************************************80
!
!! MAIN is the main program for BERNSTEIN_POLYNOMIAL_TEST.
!
!  Discussion:
!
!    BERNSTEIN_POLYNOMIAL_TEST tests the BERNSTEIN_POLYNOMIAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BERNSTEIN_POLYNOMIAL_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BERNSTEIN_POLYNOMIAL library.'

  call bernstein_matrix_test ( )
  call bernstein_matrix_test2 ( )
  call bernstein_matrix_determinant_test ( )
  call bernstein_matrix_inverse_test ( )
  call bernstein_poly_01_test ( )
  call bernstein_poly_01_test2 ( )
  call bernstein_poly_01_matrix_test ( )
  call bernstein_poly_ab_test ( )
  call bernstein_poly_ab_approx_test ( )
  call bernstein_to_legendre_test ( )
  call bernstein_to_power_test ( )
  call bernstein_vandermonde_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BERNSTEIN_POLYNOMIAL_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine bernstein_matrix_test ( )

!*****************************************************************************80
!
!! BERNSTEIN_MATRIX_TEST tests BERNSTEIN_MATRIX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BERNSTEIN_MATRIX_TEST'
  write ( *, '(a)' ) '  BERNSTEIN_MATRIX returns a matrix A which transforms a'
  write ( *, '(a)' ) '  polynomial coefficient vector from the power basis to'
  write ( *, '(a)' ) '  the Bernstein basis.'

  n = 5
  allocate ( a(1:n,1:n) )
  call bernstein_matrix ( n, a )
  call r8mat_print ( n, n, a, '  The Bernstein matrix A of order 5x5:' )
  deallocate ( a )

  return
end
subroutine bernstein_matrix_test2 ( )

!*****************************************************************************80
!
!! BERNSTEIN_MATRIX_TEST2 tests BERNSTEIN_MATRIX.
!
!  Discussion:
!
!    We use the Bernstein matrix to rewrite a Bernstein polynomial
!    in terms of the standard monomial basis.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  real ( kind = 8 ), allocatable, dimension ( : ) :: ax
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BERNSTEIN_MATRIX_TEST2'
  write ( *, '(a)' ) '  BERNSTEIN_MATRIX returns a matrix A which'
  write ( *, '(a)' ) '  transforms a polynomial coefficient vector'
  write ( *, '(a)' ) '  from the the Bernstein basis to the power basis.'
  write ( *, '(a)' ) '  We can use this to get explicit values of the'
  write ( *, '(a)' ) '  4-th degree Bernstein polynomial coefficients as'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    b(4,K)(X) = C4 * x^4'
  write ( *, '(a)' ) '              + C3 * x^3'
  write ( *, '(a)' ) '              + C2 * x^2'
  write ( *, '(a)' ) '              + C1 * x'
  write ( *, '(a)' ) '              + C0 * 1'

  n = 5
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     K       C4           C3            C2' // &
    '            C1             C0'
  write ( *, '(a)' ) ' '

  allocate ( a(1:n,1:n) )
  allocate ( ax(1:n) )
  allocate ( x(1:n) )

  call bernstein_matrix ( n, a )

  do k = 1, n

    x(1:n) = 0.0D+00
    x(k) = 1.0D+00

    ax = matmul ( a, x )

    write ( *, '(2x,i4,2x,5g14.6)' ) k, ax(1:n)

  end do

  deallocate ( a )
  deallocate ( ax )
  deallocate ( x )

  return
end
subroutine bernstein_matrix_determinant_test ( )

!*****************************************************************************80
!
!! BERNSTEIN_MATRIX_DETERMINANT_TEST tests BERNSTEIN_MATRIX_DETERMINANT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) a_norm_frobenius
  real ( kind = 8 ) d1
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8mat_norm_fro

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BERNSTEIN_MATRIX_DETERMINANT_TEST'
  write ( *, '(a)' ) '  BERNSTEIN_MATRIX_DETERMINANT computes the determinant of'
  write ( *, '(a)' ) '  the Bernstein matrix A.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     N         ||A||          det(A)'
  write ( *, '(a)' ) '                              computed'
  write ( *, '(a)' ) ''

  do n = 5, 15

    allocate ( a(1:n,1:n) )
    call bernstein_matrix ( n, a )
    a_norm_frobenius = r8mat_norm_fro ( n, n, a )

    call bernstein_matrix_determinant ( n, d1 )

    write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) n, a_norm_frobenius, d1

    deallocate ( a )

  end do

  return
end
subroutine bernstein_matrix_inverse_test ( )

!*****************************************************************************80
!
!! BERNSTEIN_MATRIX_INVERSE_TEST tests BERNSTEIN_MATRIX_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  real ( kind = 8 ) a_norm_frobenius
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: b
  real ( kind = 8 ) b_norm_frobenius
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: c
  real ( kind = 8 ) error_norm_frobenius
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8mat_norm_fro

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BERNSTEIN_MATRIX_INVERSE_TEST'
  write ( *, '(a)' ) '  BERNSTEIN_MATRIX_INVERSE computes the inverse of the'
  write ( *, '(a)' ) '  Bernstein matrix A.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     N     ||A||       ||inv(A)|| ||I-A*inv(A)||'
  write ( *, '(a)' ) ' '

  do n = 5, 15

    allocate ( a(1:n,1:n) )
    allocate ( b(1:n,1:n) )
    allocate ( c(1:n,1:n) )

    call bernstein_matrix ( n, a )
    a_norm_frobenius = r8mat_norm_fro ( n, n, a )

    call bernstein_matrix_inverse ( n, b )
    b_norm_frobenius = r8mat_norm_fro ( n, n, b )

    c = matmul ( a, b )
    call r8mat_is_identity ( n, c, error_norm_frobenius )

    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      n, a_norm_frobenius, b_norm_frobenius, error_norm_frobenius

    deallocate ( a )
    deallocate ( b )
    deallocate ( c )

  end do

  return
end
subroutine bernstein_poly_01_test ( )

!*****************************************************************************80
!
!! BERNSTEIN_POLY_01_TEST tests BERNSTEIN_POLY_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ), allocatable, dimension ( : ) :: bvec
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BERNSTEIN_POLY_01_TEST:'
  write ( *, '(a)' ) '  BERNSTEIN_POLY_01 evaluates the Bernstein polynomials'
  write ( *, '(a)' ) '  based on the interval [0,1].'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     N     K     X       Exact         BP01(N,K)(X)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bernstein_poly_01_values ( n_data, n, k, x, b )

    if ( n_data == 0 ) then
      exit
    end if

    allocate ( bvec(0:n) )

    call bernstein_poly_01 ( n, x, bvec )

    write ( *, '(2x,i4,2x,i4,2x,f7.4,g14.6,2x,g14.6)' ) n, k, x, b, bvec(k)

    deallocate ( bvec )

  end do

  return
end
subroutine bernstein_poly_01_test2 ( )

!*****************************************************************************80
!
!! BERNSTEIN_POLY_01_TEST2 tests BERNSTEIN_POLY_01.
!
!  Discussion:
!
!    Here we test the Partition-of-Unity property.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 July 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( : ) :: bvec
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BERNSTEIN_POLY_01_TEST2:'
  write ( *, '(a)' ) '  BERNSTEIN_POLY_01 evaluates the Bernstein polynomials'
  write ( *, '(a)' ) '  based on the interval [0,1].'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Here we test the partition of unity property.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     N     X          Sum ( 0 <= K <= N ) BP01(N,K)(X)'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do n = 0, 10

    allocate ( bvec(0:n) )

    x = r8_uniform_01 ( seed )

    call bernstein_poly_01 ( n, x, bvec )

    write ( *, '(2x,i4,2x,f7.4,2x,g14.6)' ) n, x, sum ( bvec(0:n) )

    deallocate ( bvec )

  end do

  return
end
subroutine bernstein_poly_01_matrix_test ( )

!*****************************************************************************80
!
!! BERNSTEIN_POLY_01_MATRIX_TEST tests BERNSTEIN_POLY_01_MATRIX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ), allocatable :: b(:,:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BERNSTEIN_POLY_01_MATRIX_TEST'
  write ( *, '(a)' ) '  BERNSTEIN_POLY_01_MATRIX is given M data values X,'
  write ( *, '(a)' ) '  and a degree N, and returns an Mx(N+1) matrix B such that'
  write ( *, '(a)' ) '  B(i,j) is the j-th Bernstein polynomial evaluated at the.'
  write ( *, '(a)' ) '  i-th data value.'

  m = 5
  allocate ( x(1:m) )
  call r8vec_linspace ( m, 0.0D+00, 1.0D+00, x )
  n = 1
  allocate ( b(1:m,1:n+1) )
  call bernstein_poly_01_matrix ( m, n, x, b )
  call r8mat_print ( m, n + 1, b, '  B(5,1+1):' )
  deallocate ( b )
  deallocate ( x )

  m = 5
  allocate ( x(1:m) )
  call r8vec_linspace ( m, 0.0D+00, 1.0D+00, x )
  n = 4
  allocate ( b(1:m,1:n+1) )
  call bernstein_poly_01_matrix ( m, n, x, b )
  call r8mat_print ( m, n + 1, b, '  B(5,4+1):' )
  deallocate ( b )
  deallocate ( x )

  m = 10
  allocate ( x(1:m) )
  call r8vec_linspace ( m, 0.0D+00, 1.0D+00, x )
  n = 4
  allocate ( b(1:m,1:n+1) )
  call bernstein_poly_01_matrix ( m, n, x, b )
  call r8mat_print ( m, n + 1, b, '  B(10,4+1):' )
  deallocate ( b )
  deallocate ( x )

  m = 3
  allocate ( x(1:m) )
  call r8vec_linspace ( m, 0.0D+00, 1.0D+00, x )
  n = 5
  allocate ( b(1:m,1:n+1) )
  call bernstein_poly_01_matrix ( m, n, x, b )
  call r8mat_print ( m, n + 1, b, '  B(3,5+1):' )
  deallocate ( b )
  deallocate ( x )

  return
end
subroutine bernstein_poly_ab_test ( )

!*****************************************************************************80
!
!! BERNSTEIN_POLY_AB_TEST tests BERNSTEIN_POLY_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 July 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) bern(0:n)
  integer ( kind = 4 ) k
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BERNSTEIN_POLY_AB_TEST'
  write ( *, '(a)' ) '  BERNSTEIN_POLY_AB evaluates Bernstein polynomials over'
  write ( *, '(a)' ) '  an arbitrary interval [A,B].'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Here, we demonstrate that '
  write ( *, '(a)' ) '    BPAB(N,K,A1,B1)(X1) = BEpAB(N,K,A2,B2)(X2)'
  write ( *, '(a)' ) '  provided only that'
  write ( *, '(a)' ) '    (X1-A1)/(B1-A1) = (X2-A2)/(B2-A2).'

  x = 0.3D+00
  a = 0.0D+00
  b = 1.0D+00
  call bernstein_poly_ab ( n, a, b, x, bern )
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '     N     K     A        B        X       BEPAB(N,K,A,B)(X)'
  write ( *, '(a)' ) ' ' 
  do k = 0, n
    write ( *, '(2x,i4,2x,i4,2x,f7.4,2x,f7.4,2x,f7.4,2x,g14.6)' ) &
      n, k, a, b, x, bern(k)
  end do
 
  x = 1.3D+00
  a = 1.0D+00
  b = 2.0D+00
  call bernstein_poly_ab ( n, a, b, x, bern )
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '     N     K     A        B        X       BPAB(N,K,A,B)(X)'
  write ( *, '(a)' ) ' ' 
  do k = 0, n
    write ( *, '(2x,i4,2x,i4,2x,f7.4,2x,f7.4,2x,f7.4,2x,g14.6)' ) &
      n, k, a, b, x, bern(k)
  end do

  x = 2.6D+00
  a = 2.0D+00
  b = 4.0D+00
  call bernstein_poly_ab ( n, a, b, x, bern )
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '     N     K     A        B        X       BPAB(N,K,A,B)(X)'
  write ( *, '(a)' ) ' '
 
  do k = 0, n
    write ( *, '(2x,i4,2x,i4,2x,f7.4,2x,f7.4,2x,f7.4,2x,g14.6)' ) &
      n, k, a, b, x, bern(k)
  end do

  return
end
subroutine bernstein_poly_ab_approx_test ( )

!*****************************************************************************80
!
!! BERNSTEIN_POLY_AB_APPROX_TEST tests BERNSTEIN_POLY_AB_APPROX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 June 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxdata = 20
  integer ( kind = 4 ), parameter :: nval = 501

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) error_max
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ndata
  real ( kind = 8 ) xdata(0:maxdata)
  real ( kind = 8 ) xval(nval)
  real ( kind = 8 ) ydata(0:maxdata)
  real ( kind = 8 ) yval(nval)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BERNSTEIN_POLY_AB_APPROX_TEST'
  write ( *, '(a)' ) '  BERNSTEIN_POLY_AB_APPROX evaluates the Bernstein'
  write ( *, '(a)' ) '  polynomial approximant to a function F(X) over [A,B].'

  a = 1.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     N      Max Error'
  write ( *, '(a)' ) ' '

  do ndata = 0, maxdata
!
!  Generate data values.
!
    do i = 0, ndata

      if ( ndata == 0 ) then
        xdata(i) = 0.5D+00 * ( a + b )
      else
        xdata(i) = ( real ( ndata - i, kind = 8 ) * a   &
                   + real (         i, kind = 8 ) * b ) &
                   / real ( ndata,     kind = 8 )
      end if

      ydata(i) = sin ( xdata(i) )

    end do
!
!  Compare the true function and the approximant.
!
    call r8vec_linspace ( nval, a, b, xval )

    error_max = 0.0D+00

    call bernstein_poly_ab_approx ( ndata, a, b, ydata, nval, xval, yval )

    error_max = maxval ( abs ( yval(1:nval) - sin ( xval(1:nval) ) ) )

    write ( *, '(2x,i4,2x,g14.6)' ) ndata, error_max

  end do

  return
end
subroutine bernstein_to_legendre_test ( )

!*****************************************************************************80
!
!! BERNSTEIN_TO_LEGENDRE_TEST tests BERNSTEIN_TO_LEGENDRE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(0:n,0:n)
  real ( kind = 8 ) b(0:n,0:n)
  real ( kind = 8 ) c(0:n,0:n)
  real ( kind = 8 ) e

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BERNSTEIN_TO_LEGENDRE_TEST:'
  write ( *, '(a)' ) '  BERNSTEIN_TO_LEGENDRE returns the matrix A which maps'
  write ( *, '(a)' ) '  polynomial coefficients from Bernstein to Legendre form.'

  call bernstein_to_legendre ( n, a )
  call r8mat_print ( n + 1, n + 1, a, '  A = bernstein_to_legendre(5):' )

  call legendre_to_bernstein ( n, b )
  call r8mat_print ( n + 1, n + 1, b, '  B = legendre_to_bernstein(5):' )

  c = matmul ( a, b )
  call r8mat_is_identity ( n + 1, c, e )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  ||A*B-I|| = ', e

  return
end
subroutine bernstein_to_power_test ( )

!*****************************************************************************80
!
!! BERNSTEIN_TO_POWER_TEST tests BERNSTEIN_TO_POWER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(0:n,0:n)
  real ( kind = 8 ) b(0:n,0:n)
  real ( kind = 8 ) c(0:n,0:n)
  real ( kind = 8 ) e

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BERNSTEIN_TO_POWER_TEST:'
  write ( *, '(a)' ) '  BERNSTEIN_TO_POWER returns the matrix A which maps'
  write ( *, '(a)' ) '  polynomial coefficients from Bernstein to Power form.'

  call bernstein_to_power ( n, a )
  call r8mat_print ( n + 1, n + 1, a, '  A = bernstein_to_power(5):' )

  call power_to_bernstein ( n, b )
  call r8mat_print ( n + 1, n + 1, b, '  B = power_to_bernstein(5):' )

  c = matmul ( a, b )
  call r8mat_is_identity ( n + 1, c, e )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  ||A*B-I|| = ', e

  return
end
subroutine bernstein_vandermonde_test ( )

!*****************************************************************************80
!
!! BERNSTEIN_VANDERMONDE_TEST tests BERNSTEIN_VANDERMONDE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8

  real ( kind = 8 ) a(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BERNSTEIN_VANDERMONDE_TEST'
  write ( *, '(a)' ) '  BERNSTEIN_VANDERMONDE returns an NxN matrix whose (I,J) entry'
  write ( *, '(a)' ) '  is the value of the J-th Bernstein polynomial of degree N-1'
  write ( *, '(a)' ) '  evaluated at the I-th equally spaced point in [0,1].'

  call bernstein_vandermonde ( n, a )
  call r8mat_print ( n, n, a, '  Bernstein Vandermonde ( 8 ):' )

  return
end

