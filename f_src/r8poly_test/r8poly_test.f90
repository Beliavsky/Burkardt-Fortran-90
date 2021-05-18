program main

!*****************************************************************************80
!
!! MAIN is the main program for r8poly_test.
!
!  Discussion:
!
!    r8poly_test tests r8poly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8poly_test'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test R8POLY.'

  call r8_sign_test ( )

  call r82poly2_print_test ( )
  call r82poly2_type_test ( )

  call r8mat_inverse_3d_test ( )
  call r8mat_print_test ( )
  call r8mat_print_some_test ( )

  call r8poly_add_test ( )
  call r8poly_ant_coef_test ( )
  call r8poly_ant_value_test ( )
  call r8poly_degree_test ( )
  call r8poly_deriv_test ( )
  call r8poly_division_test ( )
  call r8poly_lagrange_coef_test ( )
  call r8poly_lagrange_0_test ( )
  call r8poly_lagrange_1_test ( )
  call r8poly_lagrange_2_test ( )
  call r8poly_lagrange_factor_test ( )
  call r8poly_lagrange_value_test ( )
  call r8poly_multiply_test ( )
  call r8poly_power_test ( )
  call r8poly_print_test ( )
  call r8poly_value_test ( )
  call r8poly_value_horner_test ( )
  call r8poly_values_horner_test ( )

  call r8poly2_ex_test ( )
  call r8poly2_ex2_test ( )
  call r8poly2_root_test ( )
  call r8poly2_rroot_test ( )
  call r8poly2_val_test ( )
  call r8poly2_val2_test ( )

  call r8poly3_root_test ( )

  call r8poly4_root_test ( )

  call r8vec_even_test ( )
  call r8vec_even_select_test ( )
  call r8vec_indicator1_test ( )
  call r8vec_is_distinct_test ( )
  call r8vec_linspace_test ( )
  call r8vec_print_test ( )
  call r8vec_transpose_print_test ( )
  call r8vec_uniform_01_test ( )

  call r8vec2_print_test ( )

  call roots_to_r8poly_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8poly_test'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop
end
subroutine r8_sign_test ( )

!*****************************************************************************80
!
!! R8_SIGN_TEST tests R8_SIGN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 5

  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_sign
  real ( kind = 8 ), parameter, dimension ( test_num ) :: r8_test = (/ &
    -1.25D+00, -0.25D+00, 0.0D+00, +0.5D+00, +9.0D+00 /)
  real ( kind = 8 ) s
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SIGN_TEST'
  write ( *, '(a)' ) '  R8_SIGN returns the sign of an R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    R8    R8_SIGN(R8)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    r8 = r8_test(test)
    s = r8_sign ( r8 )
    write ( *, '(2x,f8.4,2x,f8.0)' ) r8, s
  end do

  return
end
subroutine r82poly2_print_test ( )

!*****************************************************************************80
!
!! R82POLY2_PRINT_TEST tests R82POLY2_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) f

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R82POLY2_PRINT_TEST'
  write ( *, '(a)' ) '  R82POLY2_PRINT prints an R82POLY2,'
  write ( *, '(a)' ) '  a quadratic polynomial in x and y.'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00
  d = 4.0D+00
  e = 5.0D+00
  f = 6.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Coefficients a, b, c, d, e, f'
  write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    a, b, c, d, e, f
  write ( *, '(a)' ) ''

  call r82poly2_print ( a, b, c, d, e, f  )

  return
end
subroutine r82poly2_type_test ( )

!*****************************************************************************80
!
!! R82POLY2_TYPE_TEST tests R82POLY2_TYPE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 12

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( test_num ) :: a_test = (/  &
    9.0D+00, 4.0D+00, 9.0D+00,  1.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, 0.0D+00, &
    0.0D+00, 0.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), save, dimension ( test_num ) :: b_test = (/ &
    -4.0D+00,   1.0D+00,  16.0D+00,   1.0D+00, 0.0D+00,  &
     2.0D+00, 1.0D+00,   1.0D+00,  1.0D+00,  0.0D+00, &
     0.0D+00, 0.0D+00 /)
  real ( kind = 8 ) c
  real ( kind = 8 ), save, dimension ( test_num ) :: c_test = (/  &
     0.0D+00,  -4.0D+00,   0.0D+00,   0.0D+00, 1.0D+00,  &
     0.0D+00, 0.0D+00,   0.0D+00,  0.0D+00,  0.0D+00, &
     0.0D+00, 0.0D+00 /)
  real ( kind = 8 ) d
  real ( kind = 8 ), save, dimension ( test_num ) :: d_test = (/ &
    -36.0D+00,  3.0D+00,  36.0D+00,  -6.0D+00, 3.0D+00, &
    -2.0D+00, 0.0D+00,   0.0D+00,  0.0D+00,  2.0D+00, &
     0.0D+00, 0.0D+00 /)
  real ( kind = 8 ) e
  real ( kind = 8 ), save, dimension ( test_num ) :: e_test = (/ &
    -24.0D+00, -4.0D+00, -32.0D+00, -10.0D+00, -1.0D+00, &
     16.0D+00, -6.0D+00, -6.0D+00, -2.0D+00, -1.0D+00, &
     0.0D+00, 0.0D+00 /)
  real ( kind = 8 ) f
  real ( kind = 8 ), save, dimension ( test_num ) :: f_test = (/ &
    -36.0D+00,  1.0D+00, -92.0D+00, 115.0D+00, -3.0D+00, &
     33.0D+00, +8.0D+00, 10.0D+00,  +1.0D+00,  1.0D+00, &
      0.0D+00, 1.0D+00 /)
  integer ( kind = 4 ) test
  integer ( kind = 4 ) type

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R82POLY2_TYPE_TEST'
  write ( *, '(a)' ) '  R82POLY2_TYPE determines the type of a second order'
  write ( *, '(a)' ) '  equation in two variables.'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    a = a_test(test)
    b = b_test(test)
    c = c_test(test)
    d = d_test(test)
    e = e_test(test)
    f = f_test(test)

    write ( *, '(a)' ) ''

    call r82poly2_print ( a, b, c, d, e, f )

    call r82poly2_type ( a, b, c, d, e, f, type )

    write ( *, '(a,i8)' ) '  Type = ', type

    call r82poly2_type_print ( type )

  end do

  return
end
subroutine r8mat_det_3d_test ( )

!*****************************************************************************80
!
!! R8MAT_DET_3D_TESt tests R8MAT_DET_3D;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_det_3d
  real ( kind = 8 ), dimension ( n ) :: x = (/ &
    1.0D+00, 10.0D+00, 4.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_DET_3D_TEST'
  write ( *, '(a)' ) '  R8MAT_DET_3D: determinant of a 3 by 3 matrix;'

  call r8mat_vand2 ( n, x, a )
  det = r8mat_det_3d ( a )

  call r8mat_print ( n, n, a, '  Matrix:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  R8MAT_DET_3D computes determinant:', det
!
!  Special formula for the determinant of a Vandermonde matrix:
!
  det = 1.0D+00
  do i = 1, n
    do j = 1, i - 1
      det = det * ( x(i) - x(j) )
    end do
  end do
  write ( *, '(a,g14.6)' ) '  Exact determinant is ', det

  return
end
subroutine r8mat_inverse_3d_test ( )

!*****************************************************************************80
!
!! R8MAT_INVERSE_3D_TEST tests R8MAT_INVERSE_3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3
!
!  Each ROW of this definion is a COLUMN of the matrix.
!
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
    1.0D+00, 4.0D+00, 7.0D+00, &
    2.0D+00, 5.0D+00, 8.0D+00, &
    3.0D+00, 6.0D+00, 0.0D+00 /), (/ n, n /) )
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)
  real ( kind = 8 ) det

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_INVERSE_3D_TEST'
  write ( *, '(a)' ) '  R8MAT_INVERSE_3D inverts a 3 by 3 matrix.'

  call r8mat_print ( n, n, a, '  Matrix A to be inverted:' )
!
!  Compute the inverse matrix.
!
  call r8mat_inverse_3d ( a, b, det )

  if ( det == 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  The input matrix was singular, no inverse'
    write ( *, '(a)' ) '  could be computed.'
    return
  end if

  call r8mat_print ( n, n, b, '  Inverse matrix B:' )

  c(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )

  call r8mat_print ( n, n, c, '  Product C = A * B:' )

  return
end
subroutine r8mat_print_test ( )

!*****************************************************************************80
!
!! R8MAT_PRINT_TEST tests R8MAT_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_PRINT_TEST'
  write ( *, '(a)' ) '  R8MAT_PRINT prints an R8MAT.'

  do j = 1, n
    do i = 1, m
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8mat_print ( m, n, a, '  The R8MAT:' )

  return
end
subroutine r8mat_print_some_test ( )

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME_TEST tests R8MAT_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8MAT_PRINT_SOME prints some of an R8MAT.'

  do j = 1, n
    do i = 1, m
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8mat_print_some ( m, n, a, 2, 1, 4, 2, &
    '  The R8MAT, rows 2:4, cols 1:2:' )

  return
end
subroutine r8poly_add_test ( )

!*****************************************************************************80
!
!! R8POLY_ADD_TEST tests R8POLY_ADD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(0:5)
  real ( kind = 8 ) b(0:5)
  real ( kind = 8 ) c(0:5)
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nb
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) nc2
  integer ( kind = 4 ) r8poly_degree

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_ADD_TEST'
  write ( *, '(a)' ) '  R8POLY_ADD adds two R8POLY''s.'

  na = 5
  a(0:na) = (/ 0.0, 1.1, 2.2, 3.3, 4.4, 5.5 /)
  nb = 5
  b(0:nb) = (/ 1.0, -2.1, 7.2, 8.3, 0.0, -5.5 /)

  call r8poly_add ( na, a, nb, b, c )

  call r8poly_print ( na, a, '  Polynomial A:' )

  call r8poly_print ( nb, b, '  Polynomial B:' )

  nc = max ( na, nb )

  nc2 = r8poly_degree ( nc, c )

  call r8poly_print ( nc2, c, '  Polynomial C = A+B:' )

  return
end
subroutine r8poly_ant_coef_test ( )

!*****************************************************************************80
!
!! r8poly_ant_coef_test tests r8poly_ant_coef().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 October 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  real ( kind = 8 ) poly_cof(n+1)
  real ( kind = 8 ) poly_cof2(n+2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8poly_ant_coef_test'
  write ( *, '(a)' ) '  r8poly_ant_coef() computes the coefficients of the'
  write ( *, '(a)' ) '  antiderivative of a polynomial'

  do i = 1, n + 1
    poly_cof(i) = real ( n + 2 - i, kind = 8 )
  end do

  call r8poly_print ( n, poly_cof, '  Polynomial p(x):' )

  call r8poly_ant_coef ( n, poly_cof, poly_cof2 )

  call r8poly_print ( n+1, poly_cof2, '  Antideriv(p(x)):' )

  return
end
subroutine r8poly_ant_value_test ( )

!*****************************************************************************80
!
!! r8poly_ant_value_test tests r8poly_ant_value().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 16

  real ( kind = 8 ), dimension (0:m) :: c = (/ &
    24.0D+00, -50.0D+00, +35.0D+00, -10.0D+00, 1.0D+00 /)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p
  real ( kind = 8 ) r8poly_ant_value
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8poly_ant_value_test'
  write ( *, '(a)' ) '  r8poly_ant_value() evaluates the antiderivative of a polynomial at'
  write ( *, '(a)' ) '  one point, using a naive method.'

  call r8poly_print ( m, c, '  The polynomial coefficients:' )

  x_lo = 0.0D+00
  x_hi = 5.0D+00
  call r8vec_linspace ( n, x_lo, x_hi, x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I    X    antiP(X)'
  write ( *, '(a)' ) ''

  do i = 1, n
    p = r8poly_ant_value ( m, c, x(i) )
    write ( *, '(2x,i2,2x,f8.4,2x,g14.6)' ) i, x(i), p
  end do

  return
end
subroutine r8poly_degree_test ( )

!*****************************************************************************80
!
!! R8POLY_DEGREE_TEST tests R8POLY_DEGREE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) :: c1(0:3) = (/ 1.0, 2.0, 3.0, 4.0 /) 
  real ( kind = 8 ) :: c2(0:3) = (/ 1.0, 2.0, 3.0, 0.0 /) 
  real ( kind = 8 ) :: c3(0:3) = (/ 1.0, 2.0, 0.0, 4.0 /)
  real ( kind = 8 ) :: c4(0:3) = (/ 1.0, 0.0, 0.0, 0.0 /)
  real ( kind = 8 ) :: c5(0:3) = (/ 0.0, 0.0, 0.0, 0.0 /)
  integer ( kind = 4 ) d
  integer ( kind = 4 ) m
  integer ( kind = 4 ) r8poly_degree

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_DEGREE_TEST'
  write ( *, '(a)' ) '  R8POLY_DEGREE determines the degree of an R8POLY.'

  m = 3

  call r8poly_print ( m, c1, '  The R8POLY:' )
  d = r8poly_degree ( m, c1 )
  write ( *, '(a,i2,a,i2)' ) '  Dimensioned degree = ', m, '  Actual degree = ', d

  call r8poly_print ( m, c2, '  The R8POLY:' )
  d = r8poly_degree ( m, c2 )
  write ( *, '(a,i2,a,i2)' ) '  Dimensioned degree = ', m, '  Actual degree = ', d

  call r8poly_print ( m, c3, '  The R8POLY:' )
  d = r8poly_degree ( m, c3 )
  write ( *, '(a,i2,a,i2)' ) '  Dimensioned degree = ', m, '  Actual degree = ', d

  call r8poly_print ( m, c4, '  The R8POLY:' )
  d = r8poly_degree ( m, c4 )
  write ( *, '(a,i2,a,i2)' ) '  Dimensioned degree = ', m, '  Actual degree = ', d

  call r8poly_print ( m, c5, '  The R8POLY:' )
  d = r8poly_degree ( m, c5 )
  write ( *, '(a,i2,a,i2)' ) '  Dimensioned degree = ', m, '  Actual degree = ', d

  return
end
subroutine r8poly_deriv_test ( )

!*****************************************************************************80
!
!! R8POLY_DERIV_TEST tests R8POLY_DERIV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) c(0:n)
  real ( kind = 8 ) cp(0:n)
  integer ( kind = 4 ) d
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_DERIV_TEST'
  write ( *, '(a)' ) '  R8POLY_DERIV computes the coefficients of'
  write ( *, '(a)' ) '  the derivative of a polynomial.'

  call r8vec_indicator1 ( n, x )

  call roots_to_r8poly ( n, x, c )

  call r8poly_print ( n, c, '  The initial polynomial' )

  do d = 0, n
    call r8poly_deriv ( n, c, d, cp )
    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  The derivative of order ', d
    write ( *, '(a)' ) ''
    call r8poly_print ( n - d, cp, ' ' )
  end do

  return
end
subroutine r8poly_division_test ( )

!*****************************************************************************80
!
!! r8poly_division_test tests r8poly_division.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 October 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(0:10)
  real ( kind = 8 ) b(0:10)
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nb
  integer ( kind = 4 ) nq
  integer ( kind = 4 ) nr
  real ( kind = 8 ) q(0:10)
  real ( kind = 8 ) r(0:10)
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 3

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8poly_division_test'
  write ( *, '(a)' ) '  r8poly_division computes the quotient and'
  write ( *, '(a)' ) '  remainder for polynomial division.'
  write ( *, '(a)' ) ''
!
!  1: Divide X^3 + 2*X^2 - 5*X - 6  by X-2.  
!     Quotient is 3+4*X+X^2, remainder is 0.
!
!  2: Divide X^4 + 3*X^3 + 2*X^2 - 2  by  X^2 + X - 3.
!     Quotient is X**2 + 2*X + 3, remainder 8*X + 7.
!
!  3: Divide X^3 - 2*X^2 + 0*X - 4  by  X - 3.
!     Quotient is X^2 + X + 3, remainder 5.
!
  do test = 1, test_num

    if ( test == 1 ) then
      na = 3
      a(0:na) = (/ -6.0D+00, -5.0D+00, 2.0D+00, 1.0D+00 /)
      nb = 1
      b(0:nb) = (/ -2.0D+00, 1.0D+00 /)
    else if ( test == 2 ) then
      na = 4
      a(0:na) = (/ -2.0D+00, 5.0D+00, 2.0D+00, 3.0D+00, 1.0D+00 /)
      nb = 2
      b(0:nb) = (/ -3.0D+00, 1.0D+00, 1.0D+00 /)
    else if ( test == 3 ) then
      na = 3
      a(0:na) = (/ -4.0D+00, 0.0D+00, -2.0D+00, 1.0D+00 /)
      nb = 1
      b(0:nb) = (/ -3.0D+00, 1.0D+00 /)
    end if

    call r8poly_print ( na, a, '  The polynomial to be divided, A:' )
    call r8poly_print ( nb, b, '  The divisor polynomial, B:' )

    call r8poly_division ( na, a, nb, b, nq, q, nr, r )
 
    call r8poly_print ( nq, q, '  The quotient polynomial, Q:' )
    call r8poly_print ( nr, r, '  The remainder polynomial, R:' )

  end do

  return
end
subroutine r8poly_lagrange_coef_test ( )

!*****************************************************************************80
!
!! R8POLY_LAGRANGE_COEF_TEST tests R8POLY_LAGRANGE_COEF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npol = 5

  integer ( kind = 4 ) ipol
  real ( kind = 8 ) pcof(0:npol-1)
  real ( kind = 8 ) xpol(npol)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_LAGRANGE_COEF_TEST'
  write ( *, '(a)' ) '  R8POLY_LAGRANGE_COEF returns the coefficients'
  write ( *, '(a)' ) '  for a Lagrange basis polynomial.'

  call r8vec_indicator1 ( npol, xpol )

  call r8vec_print ( npol, xpol, '  Abscissas:' )

  do ipol = 1, npol

    call r8poly_lagrange_coef ( npol, ipol, xpol, pcof )

    call r8poly_print ( npol - 1, pcof, '  The Lagrange basis polynomial:' )

  end do

  return
end
subroutine r8poly_lagrange_0_test ( )

!*****************************************************************************80
!
!! R8POLY_LAGRANGE_0_TEST tests R8POLY_LAGRANGE_0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npol = 5

  integer ( kind = 4 ) ival
  integer ( kind = 4 ) nx
  real ( kind = 8 ) wval
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xpol(npol)
  real ( kind = 8 ) xval

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_LAGRANGE_0_TEST'
  write ( *, '(a)' ) '  R8POLY_LAGRANGE_0 evaluates the Lagrange'
  write ( *, '(a)' ) '  factor W(X) at a point.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number of data points is ', npol
!
!  Set the abscissas of the polynomials.
!
  xlo = 0.0D+00
  xhi = real ( npol - 1, kind = 8 )

  call r8vec_even ( npol, xlo, xhi, xpol )

  call r8vec_print ( npol, xpol, '  Abscissas:' )
!
!  Evaluate W(X).
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X          W(X)'
  write ( *, '(a)' ) ''

  nx = 4 * npol - 1

  do ival = 1, nx

    call r8vec_even_select ( nx, xlo, xhi, ival, xval )

    call r8poly_lagrange_0 ( npol, xpol, xval, wval )

    write ( *, '(6g12.4)' ) xval, wval

  end do

  return
end
subroutine r8poly_lagrange_1_test ( )

!*****************************************************************************80
!
!! R8POLY_LAGRANGE_1_TEST tests R8POLY_LAGRANGE_1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npol = 5

  real ( kind = 8 ) dwdx
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) nx
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xpol(npol)
  real ( kind = 8 ) xval

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_LAGRANGE_1_TEST'
  write ( *, '(a)' ) '  R8POLY_LAGRANGE_1 evaluates the Lagrange'
  write ( *, '(a)' ) '  factor W''(X) at a point.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number of data points is ', npol
!
!  Set the abscissas of the polynomials.
!
  xlo = 0.0D+00
  xhi = real ( npol - 1, kind = 8 )

  call r8vec_even ( npol, xlo, xhi, xpol )

  call r8vec_print ( npol, xpol, '  Abscissas:' )
!
!  Evaluate W'(X)
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X          W''(X)'
  write ( *, '(a)' ) ''

  nx = 4 * npol - 1

  do ival = 1, nx

    call r8vec_even_select ( nx, xlo, xhi, ival, xval )

    call r8poly_lagrange_1 ( npol, xpol, xval, dwdx )

    write ( *, '(2g12.4)' ) xval, dwdx

  end do

  return
end
subroutine r8poly_lagrange_2_test ( )

!*****************************************************************************80
!
!! R8POLY_LAGRANGE_2_TEST tests R8POLY_LAGRANGE_2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npol = 5

  real ( kind = 8 ) dw2dx2
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) nx
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xpol(npol)
  real ( kind = 8 ) xval

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_LAGRANGE_2_TEST'
  write ( *, '(a)' ) '  R8POLY_LAGRANGE_2 evaluates the Lagrange'
  write ( *, '(a)' ) '  factor W"(X) at a point.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number of data points is ', npol
!
!  Set the abscissas of the polynomials.
!
  xlo = 0.0D+00
  xhi = real ( npol - 1, kind = 8 )

  call r8vec_even ( npol, xlo, xhi, xpol )

  call r8vec_print ( npol, xpol, '  Abscissas:' )
!
!  Evaluate W(X), W'(X), W''.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X          W"(X)'
  write ( *, '(a)' ) ''

  nx = 4 * npol - 1

  do ival = 1, nx

    call r8vec_even_select ( nx, xlo, xhi, ival, xval )

    call r8poly_lagrange_2 ( npol, xpol, xval, dw2dx2 )

    write ( *, '(2g12.4)' ) xval, dw2dx2

  end do

  return
end
subroutine r8poly_lagrange_factor_test ( )

!*****************************************************************************80
!
!! R8POLY_LAGRANGE_FACTOR_TEST tests R8POLY_LAGRANGE_FACTOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npol = 5

  real ( kind = 8 ) dwdx
  integer ( kind = 4 ) i
  real ( kind = 8 ) wval
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xpol(npol)
  real ( kind = 8 ) xval

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_LAGRANGE_FACTOR_TEST'
  write ( *, '(a)' ) '  R8POLY_LAGRANGE_FACTOR evaluates the Lagrange'
  write ( *, '(a)' ) '  factor W(X) at a point.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8,a)' ) '  For this test, we use ', npol, ' functions.'
!
!  Set the abscissas of the polynomials.
!
  xlo = 0.0D+00
  xhi = real ( npol - 1, kind = 8 )

  do i = 1, npol
    xpol(i) = ( real ( npol - ( i - 1 ), kind = 8 ) * xlo &
              + real (        ( i - 1 ), kind = 8 ) * xhi ) &
              / real ( npol,             kind = 8 )
  end do

  call r8vec_print ( npol, xpol, '  Abscissas:' )
!
!  Evaluate W(X) and W'(X).
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X          W(X)          W''(X)'
  write ( *, '(a)' ) ''

  do i = 0, 2 * npol - 1

    call r8vec_even_select ( 2 * npol - 1, xhi, xlo, i, xval )

    call r8poly_lagrange_factor ( npol, xpol, xval, wval, dwdx )

    write ( *, '(2x,f10.4,2x,f10.4,2x,f10.4)' ) xval, wval, dwdx

  end do

  return
end
subroutine r8poly_lagrange_value_test ( )

!*****************************************************************************80
!
!! r8poly_lagrange_value_test tests r8poly_lagrange_value().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npol = 5

  real ( kind = 8 ) dpdx(npol)
  integer ( kind = 4 ) ipol
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) nx
  real ( kind = 8 ) pval(npol)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xpol(npol)
  real ( kind = 8 ) xval

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8poly_lagrange_value_test'
  write ( *, '(a)' ) '  r8poly_lagrange_value() evaluates a Lagrange'
  write ( *, '(a)' ) '  interpolating polynomial at a point.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of data points = ', npol
!
!  Set the abscissas of the polynomials.
!
  xlo = 0.0D+00
  xhi = real ( npol - 1, kind = 8 )
  call r8vec_even ( npol, xlo, xhi, xpol )

  call r8vec_print ( npol, xpol, '  Abscissas:' )
!
!  Evaluate the polynomials.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here are the values of the functions at '
  write ( *, '(a)' ) '  several points:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X          L1          L2          L3      L4' // &
    '          L5'
  write ( *, '(a)' ) ''

  nx = 2 * npol - 1

  do ival = 1, nx

    call r8vec_even_select ( nx, xlo, xhi, ival, xval )

    do ipol = 1, npol
      call r8poly_lagrange_value ( npol, ipol, xpol, xval, pval(ipol), dpdx(ipol) )
    end do

    write ( *, '(6g12.4)' ) xval, ( pval(ipol), ipol = 1, npol )

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  And the derivatives:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X          L''1         L''2         L''3' // &
    '     L''4         L''5'
  write ( *, '(a)' ) ''

  nx = 2 * npol - 1

  do ival = 1, nx

    call r8vec_even_select ( nx, xlo, xhi, ival, xval )

    do ipol = 1, npol
      call r8poly_lagrange_value ( npol, ipol, xpol, xval, pval(ipol), dpdx(ipol) )
    end do

    write ( *, '(6g12.4)' ) xval, ( dpdx(ipol), ipol = 1, npol )

  end do

  return
end
subroutine r8poly_multiply_test ( )

!*****************************************************************************80
!
!! r8poly_multiply_test tests r8poly_multiply.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxn = 5

  real ( kind = 8 ) a(0:maxn)
  real ( kind = 8 ) b(0:maxn)
  real ( kind = 8 ) c(0:maxn)
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nb
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8poly_multiply_test'
  write ( *, '(a)' ) '  r8poly_multiply multiplies two polynomials.'
  write ( *, '(a)' ) ''
!
!  1: Multiply (1+X) times (1-X).  Answer is 1-X**2.
!  2: Multiply (1+2*X+3*X**2) by (1-2*X). Answer is 1 + 0*X - X**2 - 6*X**3
!
  do test = 1, test_num

    if ( test == 1 ) then
      na = 1
      a(0:na) = (/ 1.0D+00, 1.0D+00 /)
      nb = 1
      b(0:nb) = (/ 1.0D+00, -1.0D+00 /)
    else if ( test == 2 ) then
      na = 2
      a(0:na) = (/ 1.0D+00, 2.0D+00, 3.0D+00 /)
      nb = 1
      b(0:nb) = (/ 1.0D+00, -2.0D+00 /)
    end if

    call r8poly_multiply ( na, a, nb, b, c )

    call r8poly_print ( na, a, '  The factor A:' )

    call r8poly_print ( nb, b, '  The factor B:' )

    call r8poly_print ( na+nb, c, '  The product C = A*B:' )

  end do

  return
end
subroutine r8poly_power_test ( )

!*****************************************************************************80
!
!! R8POLY_POWER_TEST tests R8POLY_POWER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: lmax = 10

  real ( kind = 8 ) a(0:lmax)
  real ( kind = 8 ) b(0:10)
  integer ( kind = 4 ) na
  integer ( kind = 4 ) p

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_POWER_TEST'
  write ( *, '(a)' ) '  R8POLY_POWER takes a polynomial to a power.'
!
!  Cube (2-X).  Answer is 8-12*X+6*X^2-X^3.
!
  na = 1
  a(0:na) = (/ 2.0D+00, -1.0D+00 /)
  p = 3

  call r8poly_print ( na, a, '  The polynomial A:' )
 
  call r8poly_power ( na, a, p, b )
 
  call r8poly_print ( p*na, b, '  Raised to the power 3:' )
!
!  Square X+X^2
!
  na = 2
  a(0:na) = (/ 0.0D+00, 1.0D+00, 1.0D+00 /)
  p = 2

  call r8poly_print ( na, a, '  The polynomial A:' )
 
  call r8poly_power ( na, a, p, b )
 
  call r8poly_print ( p*na, b, '  Raised to the power 2:' )
 
  return
end
subroutine r8poly_print_test ( )

!*****************************************************************************80
!
!! R8POLY_PRINT_TEST tests R8POLY_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5

  real ( kind = 8 ), dimension ( 0 : m ) :: c = (/ &
    12.0D+00, -3.4D+00, 56.0D+00, 0.0D+00, 0.78D+00, 9.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_PRINT_TEST'
  write ( *, '(a)' ) '  R8POLY_PRINT prints an R8POLY.'

  call r8poly_print ( m, c, '  The R8POLY:' )

  return
end
subroutine r8poly_value_test ( )

!*****************************************************************************80
!
!! R8POLY_VALUE_TEST tests R8POLY_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 16

  real ( kind = 8 ), dimension (0:m) :: c = (/ &
    24.0D+00, -50.0D+00, +35.0D+00, -10.0D+00, 1.0D+00 /)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p
  real ( kind = 8 ) r8poly_value
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_VALUE_TEST'
  write ( *, '(a)' ) '  R8POLY_VALUE evaluates a polynomial at'
  write ( *, '(a)' ) '  one point, using a naive method.'

  call r8poly_print ( m, c, '  The polynomial coefficients:' )

  x_lo = 0.0D+00
  x_hi = 5.0D+00
  call r8vec_linspace ( n, x_lo, x_hi, x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I    X    P(X)'
  write ( *, '(a)' ) ''

  do i = 1, n
    p = r8poly_value ( m, c, x(i) )
    write ( *, '(2x,i2,2x,f8.4,2x,g14.6)' ) i, x(i), p
  end do

  return
end
subroutine r8poly_value_horner_test ( )

!*****************************************************************************80
!
!! R8POLY_VALUE_HORNER_TEST tests R8POLY_VALUE_HORNER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 16

  real ( kind = 8 ), dimension (0:m) :: c = (/ &
    24.0D+00, -50.0D+00, +35.0D+00, -10.0D+00, 1.0D+00 /)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p
  real ( kind = 8 ) r8poly_value_horner
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_VALUE_HORNER_TEST'
  write ( *, '(a)' ) '  R8POLY_VALUE_HORNER evaluates a polynomial at'
  write ( *, '(a)' ) '  one point, using Horner''s method.'

  call r8poly_print ( m, c, '  The polynomial coefficients:' )

  x_lo = 0.0D+00
  x_hi = 5.0D+00
  call r8vec_linspace ( n, x_lo, x_hi, x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I    X    P(X)'
  write ( *, '(a)' ) ''

  do i = 1, n
    p = r8poly_value_horner ( m, c, x(i) )
    write ( *, '(2x,i2,2x,f8.4,2x,g14.6)' ) i, x(i), p
  end do

  return
end
subroutine r8poly_values_horner_test ( )

!*****************************************************************************80
!
!! R8POLY_VALUES_HORNER_TEST tests R8POLY_VALUES_HORNER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 16

  real ( kind = 8 ), dimension (0:m) :: c = (/ &
    24.0D+00, -50.0D+00, +35.0D+00, -10.0D+00, 1.0D+00 /)
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_VALUES_HORNER_TEST'
  write ( *, '(a)' ) '  R8POLY_VALUES_HORNER evaluates a polynomial at'
  write ( *, '(a)' ) '  many points, using Horner''s method.'

  call r8poly_print ( m, c, '  The polynomial coefficients:' )

  x_lo = 0.0D+00
  x_hi = 5.0D+00
  call r8vec_linspace ( n, x_lo, x_hi, x )

  call r8poly_values_horner ( m, c, n, x, p )

  call r8vec2_print ( n, x, p, '  X, P(X)' )

  return
end
subroutine r8poly2_ex_test ( )

!*****************************************************************************80
!
!! R8POLY2_EX_TEST tests R8POLY2_EX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) ierror
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) xmin
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) ymin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY2_EX_TEST'
  write ( *, '(a)' ) '  R8POLY2_EX finds the extreme value'
  write ( *, '(a)' ) '  of a parabola determined by three points.'

  a =  2.0D+00
  b = -4.0D+00
  c = 10.0D+00

  x1 = 1.0D+00
  y1 = a * x1 ** 2 + b * x1 + c
  x2 = 2.0D+00
  y2 = a * x2 ** 2 + b * x2 + c
  x3 = 3.0D+00
  y3 = a * x3 ** 2 + b * x3 + c

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Parabolic coefficients A, B, C ='
  write ( *, '(2x,3g14.6)' ) a, b, c
  write ( *, '(a)' ) ''

  call r8r8_print ( x1, y1, '  Point 1' )
  call r8r8_print ( x2, y2, '  Point 2' )
  call r8r8_print ( x3, y3, '  Point 3' )

  a = 0.0D+00
  b = 0.0D+00
  c = 0.0D+00

  call r8poly2_ex ( x1, y1, x2, y2, x3, y3, xmin, ymin, ierror )

  write ( *, '(a)' ) ''
  write ( *, '(a,2g14.6)' ) '  R8POLY2_EX returns XMIN, YMIN = ', xmin, ymin

  return
end
subroutine r8poly2_ex2_test ( )

!*****************************************************************************80
!
!! R8POLY2_EX2_TEST tests R8POLY2_EX2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) ierror
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) xmin
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) ymin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY2_EX2_TEST'
  write ( *, '(a)' ) '  R8POLY2_EX2 finds the extreme value'
  write ( *, '(a)' ) '  of a parabola determined by three points.'
  a =  2.0D+00
  b = -4.0D+00
  c = 10.0D+00

  x1 = 1.0D+00
  y1 = a * x1 ** 2 + b * x1 + c
  x2 = 2.0D+00
  y2 = a * x2 ** 2 + b * x2 + c
  x3 = 3.0D+00
  y3 = a * x3 ** 2 + b * x3 + c

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Parabolic coefficients A, B, C ='
  write ( *, '(2x,3g14.6)' ) a, b, c
  write ( *, '(a)' ) ''

  call r8r8_print ( x1, y1, '  Point 1' )
  call r8r8_print ( x2, y2, '  Point 2' )
  call r8r8_print ( x3, y3, '  Point 3' )

  a = 0.0D+00
  b = 0.0D+00
  c = 0.0D+00

  call r8poly2_ex2 ( x1, y1, x2, y2, x3, y3, xmin, ymin, a, b, c, ierror )

  write ( *, '(a)' ) ''
  write ( *, '(a,3g14.6)' ) '  R8POLY2_EX2 returns XMIN, YMIN = ', xmin, ymin
  write ( *, '(a,3g14.6)' ) '  and A, B, C = ', a, b, c

  return
end
subroutine r8poly2_root_test ( )

!*****************************************************************************80
!
!! R8POLY2_ROOT_TEST tests R8POLY2_ROOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 3

  real ( kind = 8 ) a
  real ( kind = 8 ), dimension(test_num) :: a_test = (/ &
    2.0D+00, 1.0D+00, 1.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), dimension(test_num) :: b_test = (/ &
    -2.0D+00, -20.0D+00, -2.0D+00 /)
  real ( kind = 8 ) c
  real ( kind = 8 ), dimension(test_num) :: c_test = (/ &
    -24.0D+00, 100.0D+00, 10.0D+00 /)
  complex ( kind = 8 ) r1
  complex ( kind = 8 ) r2
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY2_ROOT_TEST'
  write ( *, '(a)' ) '  R8POLY2_ROOT finds quadratic equation roots.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         A         B         C     R1         R2'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    a = a_test(test)
    b = b_test(test)
    c = c_test(test)

    call r8poly2_root ( a, b, c, r1, r2 )

    write ( *, '(2x,3f8.1,4g14.6)' ) a, b, c, r1, r2

  end do

  return
end
subroutine r8poly2_rroot_test ( )

!*****************************************************************************80
!
!! R8POLY2_RROOT_TEST tests R8POLY2_RROOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 5

  real ( kind = 8 ) a
  real ( kind = 8 ), dimension(test_num) :: a_test = (/ &
    2.0D+00,    1.0D+00,  1.0D+00, 1.0D+00,  1.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), dimension(test_num) :: b_test = (/ &
    -2.0D+00,  -20.0D+00, -2.0D+00, 0.0D+00, -6.0D+00 /)
  real ( kind = 8 ) c
  real ( kind = 8 ), dimension(test_num) :: c_test = (/ &
    -24.0D+00, 100.0D+00, 10.0D+00, 1.0D+00, 10.0D+00 /)
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY2_RROOT_TEST'
  write ( *, '(a)' ) '  R8POLY2_RROOT finds the real parts of quadratic equation roots.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         A         B         C     R1         R2'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    a = a_test(test)
    b = b_test(test)
    c = c_test(test)

    call r8poly2_rroot ( a, b, c, r1, r2 )
 
    write ( *, '(2x,g16.4,2x,g16.4,2x,g16.4,2x,g16.4,2x,g16.4)' ) a, b, c, r1, r2
 
  end do
 
  return
end
subroutine r8poly2_val_test ( )

!*****************************************************************************80
!
!! R8POLY2_VAL_TEST tests R8POLY2_VAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) y
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) yp
  real ( kind = 8 ) ypp

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY2_VAL_TEST'
  write ( *, '(a)' ) '  R8POLY2_VAL evaluates a parabola given'
  write ( *, '(a)' ) '  3 data points.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Our parabola will be 2*x*x + 3 * x + 1.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Case 1: 3 distinct data points:'
  write ( *, '(a)' ) ''

  x1 = -1.0D+00
  x2 = 1.0D+00
  x3 = 3.0D+00

  call r8poly2_val_f ( x1, y1, yp, ypp )
  call r8poly2_val_f ( x2, y2, yp, ypp )
  call r8poly2_val_f ( x3, y3, yp, ypp )

  write ( *, '(2x,2g14.6)' ) x1, y1
  write ( *, '(2x,2g14.6)' ) x2, y2
  write ( *, '(2x,2g14.6)' ) x3, y3

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Sampled data:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  X, Y, Y'', Y"'
  write ( *, '(a)' ) ''
  do i = 0, 3
    x = real ( i, kind = 8 )
    call r8poly2_val ( x1, y1, x2, y2, x3, y3, x, y, yp, ypp )
    write ( *, '(2x,4g14.6)' ) x, y, yp, ypp
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Case 2: X1=X2, X3 distinct:'
  write ( *, '(a)' ) ''

  x1 = - 1.0D+00
  x2 = - 1.0D+00
  x3 = 3.0D+00

  call r8poly2_val_f ( x1, y1, y2, ypp )
  call r8poly2_val_f ( x3, y3, yp, ypp )
  write ( *, '(2x,2g14.6)' ) x1, y1
  write ( *, '(2x,2g14.6)' ) x2, y2
  write ( *, '(2x,2g14.6)' ) x3, y3

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Sampled data:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  X, Y, Y'', Y"'
  write ( *, '(a)' ) ''
  do i = 0, 3
    x = real ( i, kind = 8 )
    call r8poly2_val ( x1, y1, x2, y2, x3, y3, x, y, yp, ypp )
    write ( *, '(2x,4g14.6)' ) x, y, yp, ypp
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Case 3: X1=X2=X3:'
  write ( *, '(a)' ) ''

  x1 = - 1.0D+00
  x2 = - 1.0D+00
  x3 = - 1.0D+00

  call r8poly2_val_f ( x1, y1, y2, y3 )

  write ( *, '(2x,2g14.6)' ) x1, y1
  write ( *, '(2x,2g14.6)' ) x2, y2
  write ( *, '(2x,2g14.6)' ) x3, y3

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Sampled data:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  X, Y, Y'', Y"'
  write ( *, '(a)' ) ''
  do i = 0, 3
    x = real ( i, kind = 8 )
    call r8poly2_val ( x1, y1, x2, y2, x3, y3, x, y, yp, ypp )
    write ( *, '(2x,4g14.6)' ) x, y, yp, ypp
  end do

  return
end
subroutine r8poly2_val_f ( x, y, yp, ypp )

!*****************************************************************************80
!
!! R8POLY2_VAL_F evaluates a parabola for us.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) yp
  real ( kind = 8 ) ypp

  y = 2.0D+00 * x ** 2 + 3.0D+00 * x + 1.0D+00
  yp = 4.0D+00 * x + 3.0D+00
  ypp = 4.0D+00

  return
end
subroutine r8poly2_val2_test ( )

!*****************************************************************************80
!
!! R8POLY2_VAL2_TEST tests R8POLY2_VAL2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: ndata = 5
  integer ( kind = 4 ), parameter :: dim_num = 2

  integer ( kind = 4 ) i
  integer ( kind = 4 ) left
  real ( kind = 8 ) xdata(ndata)
  real ( kind = 8 ) xval
  real ( kind = 8 ) ydata(dim_num,ndata)
  real ( kind = 8 ) yval(dim_num)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY2_VAL2_TEST'
  write ( *, '(a)' ) '  R8POLY2_VAL2 evaluates parabolas through'
  write ( *, '(a)' ) '  3 points in a table'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Our data tables will actually be parabolas:'
  write ( *, '(a)' ) '    A: 2*x*x + 3 * x + 1.'
  write ( *, '(a)' ) '    B: 4*x*x - 2 * x + 5.'
  write ( *, '(a)' ) ''

  do i = 1, ndata
    xval = 2.0D+00 * real ( i, kind = 8 )
    xdata(i) = xval
    ydata(1,i) = 2.0D+00 * xval ** 2 + 3.0D+00 * xval + 1.0D+00
    ydata(2,i) = 4.0D+00 * xval ** 2 - 2.0D+00 * xval + 5.0D+00
    write ( *, '(2x,i8,3g14.6)' ) i, xdata(i), ydata(1,i), ydata(2,i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Interpolated data:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  LEFT, X, Y1, Y2'
  write ( *, '(a)' ) ''

  do i = 0, 4
    xval = real ( 2 * i + 1, kind = 8 )
    left = max ( min ( i + 1, ndata - 2 ), 1 )
    call r8poly2_val2 ( dim_num, ndata, xdata, ydata, left, xval, yval )
    write ( *, '(2x,i8,3g14.6)' ) left, xval, yval(1), yval(2)
  end do

  return
end
subroutine r8poly3_root_test ( )

!*****************************************************************************80
!
!! R8POLY3_ROOT_TEST tests R8POLY3_ROOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  real ( kind = 8 ) a
  real ( kind = 8 ), dimension(test_num) :: a_test = (/ &
    1.0D+00, 9.0D+00, 1.0D+00, 1.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), dimension(test_num) :: b_test = (/ &
    -6.0D+00, -36.0D+00, -5.0D+00, -8.0D+00  /)
  real ( kind = 8 ) c
  real ( kind = 8 ), dimension(test_num) :: c_test = (/ &
    11.0D+00, 54.0D+00, 8.0D+00, 25.0D+00  /)
  real ( kind = 8 ) d
  real ( kind = 8 ), dimension(test_num) :: d_test = (/ &
    -6.0D+00, -27.0D+00, -4.0D+00, -26.0D+00  /)
  complex ( kind = 8 ) r1
  complex ( kind = 8 ) r2
  complex ( kind = 8 ) r3
  integer ( kind = 4 ) test
!
!  1: Three distinct real roots, 1, 2, 3.
!  2: One repeated real root, 1.5, 1.5, 1.5.
!  3: Two real roots, one repeated, 1, 2, 2.
!  4: One real root, a complex conjugate pair, 2, 3+2I, 3-2I.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY3_ROOT_TEST'
  write ( *, '(a)' ) '  R8POLY3_ROOT finds roots of cubic equations.'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    a = a_test(test)
    b = b_test(test)
    c = c_test(test)
    d = d_test(test)

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Polynomial coefficients A, B, C, D:'
    write ( *, '(a)' ) ''
    write ( *, '(2x,4g14.6)' ) a, b, c, d

    call r8poly3_root ( a, b, c, d, r1, r2, r3 )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Roots:'
    write ( *, '(a)' ) ''
    write ( *, '(2x,2g14.6)' ) r1
    write ( *, '(2x,2g14.6)' ) r2
    write ( *, '(2x,2g14.6)' ) r3

  end do

  return
end
subroutine r8poly4_root_test ( )

!*****************************************************************************80
!
!! R8POLY4_ROOT_TEST tests R8POLY4_ROOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 7

  real ( kind = 8 ) a
  real ( kind = 8 ), dimension(test_num) :: a_test = (/ &
    1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, &
    1.0D+00, 1.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), dimension(test_num) :: b_test = (/ &
    -10.0D+00, -5.0D+00, -22.0D+00, -16.0D+00, -20.0D+00, &
    2.0D+00, 0.0D+00 /)
  real ( kind = 8 ) c
  real ( kind = 8 ), dimension(test_num) :: c_test = (/ &
    35.0D+00, 1.0D+00, 141.0D+00, 72.0D+00, 150.0D+00, &
    1.0D+00, 13.0D+00 /)
  real ( kind = 8 ) d
  real ( kind = 8 ), dimension(test_num) :: d_test = (/ &
    -50.0D+00, 21.0D+00, -220.0D+00, -128.0D+00, -500.0D+00, &
    8.0D+00, 0.0D+00 /)
  real ( kind = 8 ) e
  real ( kind = 8 ), dimension(test_num) :: e_test = (/ &
    24.0D+00, -18.0D+00, +100.0D+00, 80.0D+00, 625.0D+00, &
    -12.0D+00, 36.0D+00 /)
  complex ( kind = 8 ) r1
  complex ( kind = 8 ) r2
  complex ( kind = 8 ) r3
  complex ( kind = 8 ) r4
  integer ( kind = 4 ) test
!
!  1: Four distinct real roots, 1, 2, 3, 4.
!  2: Three distinct real roots, 1, -2, 3, 3
!  3: Two distinct real roots, 1, 1, 10, 10.
!  4: Two distinct real roots, 2, 2, 2, 10
!  5: One real root, 5, 5, 5, 5
!  6: Two distinct real roots, one complex conjugate pair.
!  7: Two distinct complex conjugate pairs.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY4_ROOT_TEST'
  write ( *, '(a)' ) '  R8POLY4_ROOT finds roots of quartic equations.'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    a = a_test(test)
    b = b_test(test)
    c = c_test(test)
    d = d_test(test)
    e = e_test(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  A =', a
    write ( *, '(a,g14.6)' ) '  B =', b
    write ( *, '(a,g14.6)' ) '  C =', c
    write ( *, '(a,g14.6)' ) '  D =', d
    write ( *, '(a,g14.6)' ) '  E =', e

    call r8poly4_root ( a, b, c, d, e, r1, r2, r3, r4 )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Roots:'
    write ( *, '(a)' ) ''
    write ( *, '(2x,2g14.6)' ) r1
    write ( *, '(2x,2g14.6)' ) r2
    write ( *, '(2x,2g14.6)' ) r3
    write ( *, '(2x,2g14.6)' ) r4

  end do

  return
end
subroutine r8vec_even_test ( )

!*****************************************************************************80
!
!! R8VEC_EVEN_TEST tests R8VEC_EVEN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo

  xlo = 0.0D+00
  xhi = 99.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_EVEN_TEST'
  write ( *, '(a)' ) '  R8VEC_EVEN computes an R8VEC containing N evenly spaced'
  write ( *, '(a)' ) '  values between XLO and XHI.'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  XLO = ', xlo
  write ( *, '(a,g14.6)' ) '  XHI = ', xhi
  write ( *, '(a,i8)' ) '  while N = ', n

  call r8vec_even ( n, xlo, xhi, x )

  call r8vec_print ( n, x, '  Resulting array:' )

  return
end
subroutine r8vec_even_select_test ( )

!*****************************************************************************80
!
!! R8VEC_EVEN_SELECT_TEST tests R8VEC_EVEN_SELECT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xi
  real ( kind = 8 ) xlo

  xlo = 0.0D+00
  xhi = 99.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_EVEN_SELECT_TEST'
  write ( *, '(a)' ) '  R8VEC_EVEN_SELECT returns the I-th of N evenly spaced values'
  write ( *, '(a)' ) '  between XLO and XHI.'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  XLO = ', xlo
  write ( *, '(a,g14.6)' ) '  XHI = ', xhi
  write ( *, '(a,i8)' ) '  while N = ', n

  do i = 2, n, 3
    call r8vec_even_select ( n, xlo, xhi, i, xi )
    write ( *, '(2x,i2,2x,g14.6)' ) i, xi
  end do

  return
end
subroutine r8vec_indicator1_test ( )

!*****************************************************************************80
!
!! R8VEC_INDICATOR1_TEST tests R8VEC_INDICATOR1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDICATOR1_TEST'
  write ( *, '(a)' ) '  R8VEC_INDICATOR1 returns an indicator1 vector.'

  call r8vec_indicator1 ( n, a )

  call r8vec_print ( n, a, '  The indicator1 vector:' )

  return
end
subroutine r8vec_is_distinct_test ( )

!*****************************************************************************80
!
!! R8VEC_IS_DISTINCT_TEST tests R8VEC_IS_DISTINCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  logical r8vec_is_distinct
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_IS_DISTINCT_TEST'
  write ( *, '(a)' ) '  R8VEC_IS_DISTINCT is TRUE if an R8VEC only contains'
  write ( *, '(a)' ) '  distinct entries.'

  n = 3
  allocate ( x(1:n) )
  x = (/ 0.0D+00, 1.0D+00, 3.0D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_distinct ( n, x ) ) then
    write ( *, '(a)' ) '  X is distinct.'
  else
    write ( *, '(a)' ) '  X is NOT distinct.'
  end if
  deallocate ( x )

  n = 3
  allocate ( x(1:n) )
  x = (/ 1.5D+00, 1.6D+00, 1.5D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_distinct ( n, x ) ) then
    write ( *, '(a)' ) '  X is distinct.'
  else
    write ( *, '(a)' ) '  X is NOT distinct.'
  end if
  deallocate ( x )

  n = 3
  allocate ( x(1:n) )
  x = (/ -1.0D+00, 1.0D+00, 10.0D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_distinct ( n, x ) ) then
    write ( *, '(a)' ) '  X is distinct.'
  else
    write ( *, '(a)' ) '  X is NOT distinct.'
  end if
  deallocate ( x )

  return
end
subroutine r8vec_linspace_test ( )

!*****************************************************************************80
!
!! R8VEC_LINSPACE_TEST tests R8VEC_LINSPACE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_LINSPACE_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_LINSPACE: evenly spaced points between A and B;'

  a = 10.0D+00
  b = 20.0D+00

  call r8vec_linspace ( n, a, b, x )
  call r8vec_print ( n, x, '  r8vec_linspace ( 5, 10, 20 )' )

  return
end
subroutine r8vec_print_test ( )

!*****************************************************************************80
!
!! R8VEC_PRINT_TEST tests R8VEC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ), dimension ( n ) :: a = (/ &
    123.456D+00, 0.000005D+00, -1.0D+06, 3.14159265D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC_PRINT prints an R8VEC.'

  call r8vec_print ( n, a, '  The R8VEC:' )

  return
end

subroutine r8vec_transpose_print_test ( )

!*****************************************************************************80
!
!! R8VEC_TRANSPOSE_PRINT_TEST tests R8VEC_TRANSPOSE_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 12

  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_TRANSPOSE_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC_TRANSPOSE_PRINT prints an R8VEC "tranposed",'
  write ( *, '(a)' ) '  that is, placing multiple entries on a line.'

  call r8vec_uniform_01 ( n, seed, x )

  call r8vec_transpose_print ( n, x, '  The vector X:' )

  return
end
subroutine r8vec_uniform_01_test ( )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01_TEST tests R8VEC_UNIFORM_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) r(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_01 returns a random R8VEC '
  write ( *, '(a)' ) '  with entries in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  Input SEED = ', seed

  call r8vec_uniform_01 ( n, seed, r )

  call r8vec_print ( n, r, '  Random R8VEC:' )

  return
end
subroutine r8vec2_print_test ( )

!*****************************************************************************80
!
!! R8VEC2_PRINT_TEST tests R8VEC2_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) :: a(n) = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c(n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC2_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC2_PRINT prints a pair of R8VEC''s.'

  do i = 1, n
    b(i) = a(i) ** 2
    c(i) = sqrt ( a(i) )
  end do

  call r8vec2_print ( n, b, c, '  Squares and square roots:' )

  return
end
subroutine roots_to_r8poly_test ( )

!*****************************************************************************80
!
!! ROOTS_TO_R8POLY_TEST tests ROOTS_TO_R8POLY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) c(0:n)
  real ( kind = 8 ), dimension ( n ) :: x = (/ &
    1.0D+00, -4.0D+00, 3.0D+00, 0.0D+00, 3.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROOTS_TO_R8POLY_TEST:'
  write ( *, '(a)' ) '  ROOTS_TO_R8POLY is given N real roots,'
  write ( *, '(a)' ) '  and constructs the coefficient vector'
  write ( *, '(a)' ) '  of the corresponding polynomial.'

  call r8vec_print ( n, x, '  N real roots:' )

  call roots_to_r8poly ( n, x, c )

  call r8poly_print ( n, c, '  Corresponding polynomial:' )

  return
end

