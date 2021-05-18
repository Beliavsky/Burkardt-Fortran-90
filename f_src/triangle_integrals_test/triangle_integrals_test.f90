program main

!*****************************************************************************80
!
!! MAIN is the main program for TRIANGLE_INTEGRALS_TEST.
!
!  Discussion:
!
!    TRIANGLE_INTEGRALS_TEST tests the TRIANGLE_INTEGRALS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_INTEGRALS_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the TRIANGLE_INTEGRALS library.'

  call i4_to_pascal_test ( )
  call i4_to_pascal_degree_test ( )
  call pascal_to_i4_test ( )
  call r8mat_print_test ( )
  call r8mat_print_some_test ( )
  call trinomial_test ( )

  call rs_to_xy_map_test ( )
  call xy_to_rs_map_test ( )

  call poly_print_test ( )
  call poly_power_linear_test ( )
  call poly_power_test ( )
  call poly_product_test ( )

  call triangle01_monomial_integral_test ( )
  call triangle01_poly_integral_test ( )
  call triangle_area_test ( )
  call triangle_xy_integral_test ( )
  call triangle_monomial_integral_test ( )
  call triangle_poly_integral_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_INTEGRALS_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine i4_to_pascal_test ( )

!*****************************************************************************80
!
!! I4_TO_PASCAL_TEST tests I4_TO_PASCAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_TO_PASCAL_TEST'
  write ( *, '(a)' ) '  I4_TO_PASCAL converts a linear index to'
  write ( *, '(a)' ) '  Pascal triangle indices.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     K  =>   I     J'
  write ( *, '(a)' ) ''

  do k = 1, 20

    call i4_to_pascal ( k, i, j )

    write ( *, '(2x,i4,4x,i4,2x,i4)' ) k, i, j

  end do

  return
end
subroutine i4_to_pascal_degree_test ( )

!*****************************************************************************80
!
!! I4_TO_PASCAL_DEGREE_TEST tests I4_TO_PASCAL_DEGREE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_TO_PASCAL_DEGREE_TEST'
  write ( *, '(a)' ) '  I4_TO_PASCAL_DEGREE converts a linear index to'
  write ( *, '(a)' ) '  the degree of the corresponding Pascal triangle indices.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     K  =>   D'
  write ( *, '(a)' ) ''

  do k = 1, 20

    call i4_to_pascal_degree ( k, d )

    write ( *, '(2x,i4,4x,i4)' ) k, d

  end do

  return
end
subroutine pascal_to_i4_test ( )

!*****************************************************************************80
!
!! PASCAL_TO_I4_TEST tests PASCAL_TO_I4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PASCAL_TO_I4_TEST'
  write ( *, '(a)' ) '  PASCAL_TO_I4 converts Pascal triangle indices to a'
  write ( *, '(a)' ) '  linear index.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I     J =>    K'
  write ( *, '(a)' ) ''

  do d = 0, 4
    do i = d, 0, -1
      j = d - i
      call pascal_to_i4 ( i, j, k )
      write ( *, '(2x,i4,2x,i4,4x,i4)' ) i, j, k
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine poly_power_test ( )

!*****************************************************************************80
!
!! POLY_POWER_TEST tests POLY_POWER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 2
  integer ( kind = 4 ), parameter :: n4 = 3

  integer ( kind = 4 ), parameter :: d1 = 1
  integer ( kind = 4 ), parameter :: d2 = d1 * n1
  integer ( kind = 4 ), parameter :: d3 = 2
  integer ( kind = 4 ), parameter :: d4 = 2
  integer ( kind = 4 ), parameter :: d5 = n4 * d4
  integer ( kind = 4 ), parameter :: d6 = 6

  integer ( kind = 4 ), parameter :: m1 = ( ( d1 + 1 ) * ( d1 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m4 = ( ( d4 + 1 ) * ( d4 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m5 = ( ( d5 + 1 ) * ( d5 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m6 = ( ( d6 + 1 ) * ( d6 + 2 ) ) / 2

  real ( kind = 8 ) p1(m1)
  real ( kind = 8 ) p2(m2)
  real ( kind = 8 ) p3(m3)
  real ( kind = 8 ) p4(m4)
  real ( kind = 8 ) p5(m5)
  real ( kind = 8 ) p6(m6)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLY_POWER_TEST:'
  write ( *, '(a)' ) '  POLY_POWER computes the N-th power of an X,Y polynomial.'
!
!  P1 = ( 1 + 2 x + 3 y )
!  P2 = P1^2 = 1 + 4x + 6y + 4x^2 + 12xy + 9y^2 
!  P3 = correct value
!
  p1 = (/ 1.0, 2.0, 3.0 /)

  write ( *, '(a)' ) ''
  call poly_print ( d1, p1, '  p1(x,y)' )

  call poly_power ( d1, p1, n1, d2, p2 )
  write ( *, '(a)' ) ''
  call poly_print ( d2, p2, '  p2(x,y) = p1(x,y)^2' )

  p3 = (/ 1.0, 4.0, 6.0, 4.0, 12.0, 9.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d3, p3, '  p3(x,y)=correct answer' )
!
!  P4 = ( 1 - 2 x + 3 y - 4 x^2 + 5 xy - 6 y^2 )
!  P5 = P4^3 =
!    1
!    -6x +9y
!    +0x^2 - 21xy + 9y^2
!    +40x^3 - 96x^2y  + 108x^y2 - 81y^3
!    +0x^4 + 84x^3y - 141 x^2y^2 +171xy^3 - 54y^4
!    -96x^5 + 384x^4y -798x^3y^2 + 1017 x^2y^3 - 756 xy^4 + 324 y^5
!    -64x^6 + 240x^5y - 588x^4y^2 + 845 x^3y^3 - 882 x^2y^4 +540 xy^5 - 216y^6
!
  p4 = (/ 1.0, -2.0, 3.0, -4.0, +5.0, -6.0 /)

  write ( *, '(a)' ) ''
  call poly_print ( d4, p4, '  p4(x,y)' )

  call poly_power ( d4, p4, n4, d5, p5 )
  write ( *, '(a)' ) ''
  call poly_print ( d5, p5, '  p5(x,y) = p1(x,y)^3' )

  p6 = (/ &
      1.0, &
     -6.0,  9.0, &   
      0.0, -21.0,    9.0, &
     40.0, -96.0,  108.0,  -81.0, &
      0.0,  84.0, -141.0,  171.0,  -54.0, &
    -96.0, 384.0, -798.0, 1017.0, -756.0, 324.0, &
    -64.0, 240.0, -588.0,  845.0, -882.0, 540.0, -216.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d6, p6, '  p6(x,y)=correct answer' )

  return
end
subroutine poly_power_linear_test ( )

!*****************************************************************************80
!
!! POLY_POWER_LINEAR_TEST tests POLY_POWER_LINEAR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: d1 = 1
  integer ( kind = 4 ), parameter :: n1 = 2
  integer ( kind = 4 ), parameter :: d4 = 1
  integer ( kind = 4 ), parameter :: n4 = 3

  integer ( kind = 4 ), parameter :: d2 = d1 * n1
  integer ( kind = 4 ), parameter :: d3 = 2
  integer ( kind = 4 ), parameter :: d5 = d4 * n4
  integer ( kind = 4 ), parameter :: d6 = 3

  integer ( kind = 4 ), parameter :: m1 = ( ( d1 + 1 ) * ( d1 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m4 = ( ( d4 + 1 ) * ( d4 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m5 = ( ( d5 + 1 ) * ( d5 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m6 = ( ( d6 + 1 ) * ( d6 + 2 ) ) / 2

  real ( kind = 8 ) p1(m1)
  real ( kind = 8 ) p2(m2)
  real ( kind = 8 ) p3(m3)
  real ( kind = 8 ) p4(m4)
  real ( kind = 8 ) p5(m5)
  real ( kind = 8 ) p6(m6)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLY_POWER_LINEAR_TEST:'
  write ( *, '(a)' ) '  POLY_POWER_LINEAR computes the N-th power of '
  write ( *, '(a)' ) '  a linear polynomial in X and Y.'
!
!  P1 = ( 1 + 2 x + 3 y )
!  P2 = P1^2
!  P3 = correct value
!
  p1 = (/ 1.0, 2.0, 3.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d1, p1, '  p1(x,y)' )

  call poly_power_linear ( d1, p1, n1, d2, p2 )
  write ( *, '(a)' ) ' '
  call poly_print ( d2, p2, '  p2(x,y) = p1(x,y)^n' )

  p3 = (/ 1.0, 4.0, 6.0, 4.0, 12.0, 9.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d3, p3, '  Correct answer' )
!
!  P4 = ( 2 - x + 3 y )
!  P5 = P4^3
!  P6 = correct value
!
  p4 = (/ 2.0, -1.0, 3.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d4, p4, '  p4(x,y)' )

  call poly_power_linear ( d4, p4, n4, d5, p5 )
  write ( *, '(a)' ) ''
  call poly_print ( d5, p5, '  p5(x,y) = p4(x,y)^3' )

  p6 = (/ 8.0, -12.0, 36.0, 6.0, -36.0, 54.0, -1.0, 9.0, -27.0, 27.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d6, p6, '  Correct answer' )

  return
end
subroutine poly_print_test ( )

!*****************************************************************************80
!
!! POLY_PRINT_TEST tests POLY_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: d1 = 0
  integer ( kind = 4 ), parameter :: d2 = 1
  integer ( kind = 4 ), parameter :: d3 = 2
  integer ( kind = 4 ), parameter :: d4 = 3

  integer ( kind = 4 ), parameter :: m1 = ( ( d1 + 1 ) * ( d1 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m4 = ( ( d4 + 1 ) * ( d4 + 2 ) ) / 2

  real ( kind = 8 ), dimension ( m1 ) :: p1 = (/ 12.34D+00 /)
  real ( kind = 8 ), dimension ( m2 ) :: p2 = (/ 1.0, 2.0, 3.0 /)
  real ( kind = 8 ), dimension ( m3 ) :: p3 = (/ 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 /)
  real ( kind = 8 ), dimension ( m4 ) :: p4 = (/ 1.0, -2.1, +3.2, -4.3, +5.4, &
    -6.5, +7.6, -8.7, +9.8, -10.9 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLY_PRINT_TEST:'
  write ( *, '(a)' ) '  POLY_PRINT can print a D-degree polynomial in X and Y.'
!
!  P1 = 12.34
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  P1(x,y) = 12.34'
  call poly_print ( d1, p1, '  p1(x,y)' )
!
!  P2 = 1.0 + 2.0 * x + 3.0 * Y
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  P2(x,y) = 1 + 2 * x + 3 * Y'
  call poly_print ( d2, p2, '  p2(x,y)' )
!
!  P3 = XY
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  P3(x,y) = xy'
  call poly_print ( d3, p3, '  p3(x,y) = xy' )
!
!  P4 = 1 - 2.1 * x + 3.2 * y - 4.3 * x^2 + 5.4 * xy - 6.5 * y^2
!    + 7.6 * x^3 - 8.7 * x^2y + 9.8 * xy^2 - 10.9 * y^3.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  P4(x,y) = 1.0 - 2.1 * x + 3.2 * y - 4.3 * x^2 '
  write ( *, '(a)' ) '          + 5.4 * xy - 6.5 * y^2 + 7.6 * x^3 '
  write ( *, '(a)' ) '          - 8.7 * x^2y + 9.8 * xy^2 - 10.9 * y^3.'
  call poly_print ( d4, p4, '  p4(x,y)' )

  return
end
subroutine poly_product_test ( )

!*****************************************************************************80
!
!! POLY_PRODUCT_TEST tests POLY_PRODUCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: d1 = 1
  integer ( kind = 4 ), parameter :: d2 = 1
  integer ( kind = 4 ), parameter :: d3 = d1 + d2
  integer ( kind = 4 ), parameter :: d4 = d3

  integer ( kind = 4 ), parameter :: d5 = 2
  integer ( kind = 4 ), parameter :: d6 = 2
  integer ( kind = 4 ), parameter :: d7 = d5 + d6
  integer ( kind = 4 ), parameter :: d8 = d7

  integer ( kind = 4 ), parameter :: m1 = ( ( d1 + 1 ) * ( d1 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m4 = ( ( d4 + 1 ) * ( d4 + 2 ) ) / 2

  integer ( kind = 4 ), parameter :: m5 = ( ( d5 + 1 ) * ( d5 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m6 = ( ( d6 + 1 ) * ( d6 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m7 = ( ( d7 + 1 ) * ( d7 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m8 = ( ( d8 + 1 ) * ( d8 + 2 ) ) / 2

  real ( kind = 8 ) p1(m1)
  real ( kind = 8 ) p2(m2)
  real ( kind = 8 ) p3(m3)
  real ( kind = 8 ) p4(m4)
  real ( kind = 8 ) p5(m5)
  real ( kind = 8 ) p6(m6)
  real ( kind = 8 ) p7(m7)
  real ( kind = 8 ) p8(m8)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLY_PRODUCT_TEST:'
  write ( *, '(a)' ) '  POLY_PRODUCT computes the product of two X,Y polynomials.'
!
!  P1 = ( 1 + 2 x + 3 y )
!  P2 = ( 4 + 5 x )
!  P3 = P1 * P2
!  P4 = 4 + 13x + 12y + 10x^2 + 15xy + 0y^2 
!
  p1 = (/ 1.0, 2.0, 3.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d1, p1, '  p1(x,y)' )

  p2 = (/ 4.0, 5.0, 0.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d2, p2, '  p2(x,y)' )

  call poly_product ( d1, p1, d2, p2, d3, p3 )
  write ( *, '(a)' ) ''
  call poly_print ( d3, p3, '  p3(x,y) = p1(x,y) * p2(x,y)' )

  p4 = (/ 4.0, 13.0, 12.0, 10.0, 15.0, 0.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d4, p4, '  p4(x,y) = correct answer' )
!
!  P5 = ( 1 - 2 x + 3 y - 4x^2 + 5xy - 6y^2)
!  P6 = ( 7 + 3x^2 )
!  P7 = P5 * P6
!  P8 =    7 
!       - 14x   + 21   y 
!       - 25x^2 + 35x  y - 42   y^2 
!       -  6x^3 +  9x^2y +  0x  y^2 + 0  y^3
!       - 12x^4 + 15x^3y - 18x^2y^2 + 0 xy^3 + 0y^4
!
  p5 = (/ 1.0, -2.0, 3.0, -4.0, +5.0, -6.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d5, p5, '  p5(x,y)' )

  p6 = (/ 7.0, 0.0, 0.0, 3.0, 0.0, 0.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d6, p6, '  p6(x,y)' )

  call poly_product ( d5, p5, d6, p6, d7, p7 )
  write ( *, '(a)' ) ''
  call poly_print ( d7, p7, '  p7(x,y) = p5(x,y) * p6(x,y)' )

  p8 = (/ &
    7.0, &
  -14.0,  21.0, &
  -25.0, +35.0, -42.0, &
   -6.0,   9.0,   0.0, 0.0, &
  -12.0, +15.0, -18.0, 0.0, 0.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d8, p8, '  p8(x,y) = Correct answer' )

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

  write ( *, '(a)' ) ' '
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

  write ( *, '(a)' ) ' '
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
subroutine rs_to_xy_map_test ( )

!*****************************************************************************80
!
!! RS_TO_XY_MAP_TEST tests RS_TO_XY_MAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
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
  integer ( kind = 4 ) j
  real ( kind = 8 ), dimension ( 2, 3 ) :: t = reshape ( (/ &
    2.0, 0.0, &
    3.0, 4.0, &
    0.0, 3.0 /), (/ 2, 3 /) )
  real ( kind = 8 ), dimension ( 2, 3 ) :: tr = reshape ( (/ &
    0.0, 0.0, &
    1.0, 0.0, &
    0.0, 1.0 /), (/ 2, 3 /) )
  real ( kind = 8 ) x
  real ( kind = 8 ) y 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RS_TO_XY_MAP_TEST:'
  write ( *, '(a)' ) '  RS_TO_XY_MAP determines the coefficients of '
  write ( *, '(a)' ) '  the linear map from a the reference in RS coordinates'
  write ( *, '(a)' ) '  to the physical triangle in XY coordinates:'
  write ( *, '(a)' ) '    X = a + b * R + c * S'
  write ( *, '(a)' ) '    Y = d + e * R + f * S'

  call r8mat_print ( 2, 3, t, '  XY triangle vertices:' )

  call rs_to_xy_map ( t, a, b, c, d, e, f )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Mapping coefficients are:'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6,a,g14.6,a)' ) &
    '    X = ', a, ' + ', b, ' * R + ', c, ' * S'
  write ( *, '(a,g14.6,a,g14.6,a,g14.6,a)' ) &
    '    Y = ', d, ' + ', e, ' * R + ', f, ' * S'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Apply map to RS triangle vertices.'
  write ( *, '(a)' ) '  Recover XY vertices (2,0), (3,4) and (0,3).'
  write ( *, '(a)' ) ''
  do j = 1, 3
    x = a + b * tr(1,j) + c * tr(2,j)
    y = d + e * tr(1,j) + f * tr(2,j)
    write ( *, '(a,i1,a,g14.6,a,g14.6,a)' ) &
      '  V(', j, ') = ( ', x, ',', y, ')'
  end do

  return
end
subroutine triangle_area_test ( )

!*****************************************************************************80
!
!! TRIANGLE_AREA_TEST tests TRIANGLE_AREA_MAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angled
  real ( kind = 8 ) angler
  real ( kind = 8 ) area
  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ), dimension ( 2, 3 ) :: t = reshape ( (/ & 
    0.0, 0.0, &
    2.0, 0.0, &
    0.0, 1.0 /), (/ 2, 3 /) )
  real ( kind = 8 ) triangle_area

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_AREA_TEST:'
  write ( *, '(a)' ) &
    '  TRIANGLE_AREA determines the (signed) area of a triangle.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Triangle vertices are:'
  write ( *, '(a)' ) '    (X1,Y1) = (0,0)'
  write ( *, '(a)' ) '    (X2,Y2) = 2*(cos(angle),sin(angle))'
  write ( *, '(a)' ) '    (X3,Y3) = (0,1)'
  write ( *, '(a)' ) '  where angle will sweep from 0 to 360 degrees.'

  r = 2.0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I      Angle         X2          Y2          Area'
  write ( *, '(a)' ) '        (degrees)'
  write ( *, '(a)' ) ''
  do i = 0, 24
    angled = real ( i, kind = 8 ) * 180.0D+00 / 12.0D+00
    angler = real ( i, kind = 8 ) * r8_pi / 12.0D+00
    t(1,2) = r * cos ( angler )
    t(2,2) = r * sin ( angler )
    area = triangle_area ( t )
    write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4,2x,g14.6)' ) &
      i, angled, t(1,2), t(2,2), area
  end do

  return
end
subroutine triangle_monomial_integral_test ( )

!*****************************************************************************80
!
!! TRIANGLE_MONOMIAL_INTEGRAL_TEST estimates integrals over a triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) q
  real ( kind = 8 ) q2
  real ( kind = 8 ), dimension ( 2, 3 ) :: t1 = reshape ( (/ &
     0.0, 0.0, &
     1.0, 0.0, &
     0.0, 1.0 /), (/ 2, 3 /) )
  real ( kind = 8 ), dimension ( 2, 3 ) :: t2 = reshape ( (/ &
     0.0, 0.0, &
     1.0, 0.0, &
     1.0, 2.0 /), (/ 2, 3 /) )
  real ( kind = 8 ), dimension ( 2, 3 ) :: t3 = reshape ( (/ &
    -3.0, 0.0, &
     6.0, 0.0, &
     0.0, 3.0 /), (/ 2, 3 /) )
  real ( kind = 8 ), dimension ( 2, 3 ) :: t4 = reshape ( (/ &
     0.0, 0.0, &
     4.0, 0.0, &
     0.0, 1.0  /), (/ 2, 3 /) )
  real ( kind = 8 ) triangle_monomial_integral

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_MONOMIAL_INTEGRAL_TEST'
  write ( *, '(a)' ) '  TRIANGLE_MONOMIAL_INTEGRAL returns the integral Q of'
  write ( *, '(a)' ) '  a monomial X^I Y^J over the interior of a triangle.'
!
!  Test 1:
!
  i = 1
  j = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Triangle vertices:'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t1(1,1), ',', t1(2,1), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t1(1,2), ',', t1(2,2), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t1(1,3), ',', t1(2,3), ')'
  write ( *, '(a,i1,a,i1)' ) '  Integrand = x^', i, ' * y^', j

  q = triangle_monomial_integral ( i, j, t1 )
  q2 = 1.0D+00 / 6.0D+00

  write ( *, '(a,g14.6)' ) '  Computed Q = ', q
  write ( *, '(a,g14.6)' ) '  Exact Q    = ', q2
!
!  Test 2:
!
  i = 1
  j = 1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Triangle vertices:'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t2(1,1), ',', t2(2,1), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t2(1,2), ',', t2(2,2), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t2(1,3), ',', t2(2,3), ')'
  write ( *, '(a,i1,a,i1)' ) '  Integrand = x^', i, ' * y^', j

  q = triangle_monomial_integral ( i, j, t2 )
  q2 = 0.5D+00

  write ( *, '(a,g14.6)' ) '  Computed Q = ', q
  write ( *, '(a,g14.6)' ) '  Exact Q    = ', q2
!
!  Test 3:
!
  i = 1
  j = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Triangle vertices:'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t3(1,1), ',', t3(2,1), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t3(1,2), ',', t3(2,2), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t3(1,3), ',', t3(2,3), ')'
  write ( *, '(a,i1,a,i1)' ) '  Integrand = x^', i, ' * y^', j

  q = triangle_monomial_integral ( i, j, t3 )
  q2 = 13.5D+00

  write ( *, '(a,g14.6)' ) '  Computed Q = ', q
  write ( *, '(a,g14.6)' ) '  Exact Q    = ', q2
!
!  Test 4:
!
  i = 1
  j = 1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Triangle vertices:'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t4(1,1), ',', t4(2,1), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t4(1,2), ',', t4(2,2), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t4(1,3), ',', t4(2,3), ')'
  write ( *, '(a,i1,a,i1)' ) '  Integrand = x^', i, ' * y^', j

  q = triangle_monomial_integral ( i, j, t4 )
  q2 = 2.0D+00 / 3.0D+00

  write ( *, '(a,g14.6)' ) '  Computed Q = ', q
  write ( *, '(a,g14.6)' ) '  Exact Q    = ', q2

  return
end
subroutine triangle_poly_integral_test ( )

!*****************************************************************************80
!
!! TRIANGLE_POLY_INTEGRAL_TEST estimates integrals over a triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: d1 = 1
  integer ( kind = 4 ), parameter :: d2 = 2
  integer ( kind = 4 ), parameter :: d3 = 2
  integer ( kind = 4 ), parameter :: d4 = 2

  integer ( kind = 4 ), parameter :: m1 = ( ( d1 + 1 ) * ( d1 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m4 = ( ( d4 + 1 ) * ( d4 + 2 ) ) / 2

  real ( kind = 8 ) p1(m1)
  real ( kind = 8 ) p2(m2)
  real ( kind = 8 ) p3(m3)
  real ( kind = 8 ) p4(m4)
  real ( kind = 8 ) q
  real ( kind = 8 ) q2
  real ( kind = 8 ), dimension ( 2, 3 ) :: t1 = reshape ( (/ &
     0.0, 0.0, &
     1.0, 0.0, &
     0.0, 1.0  /), (/ 2, 3 /) )
  real ( kind = 8 ), dimension ( 2, 3 ) :: t2 = reshape ( (/ &
     0.0, 0.0, &
     1.0, 0.0, &
     1.0, 2.0  /), (/ 2, 3 /) )
  real ( kind = 8 ), dimension ( 2, 3 ) :: t3 = reshape ( (/ &
     0.0, 0.0, &
     1.0, 0.0, &
     1.0, 3.0  /), (/ 2, 3 /) )
  real ( kind = 8 ), dimension ( 2, 3 ) :: t4 = reshape ( (/ &
     0.0, 3.0, &
     1.0, 1.0, &
     5.0, 3.0  /), (/ 2, 3 /) )
  real ( kind = 8 ) triangle_poly_integral

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_POLY_INTEGRAL_TEST'
  write ( *, '(a)' ) '  TRIANGLE_POLY_INTEGRAL returns the integral Q of'
  write ( *, '(a)' ) '  a polynomial over the interior of a triangle.'
!
!  Test 1:
!  Integrate x over reference triangle.
!
  p1 = (/ 0.0, 1.0, 0.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Triangle vertices:'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t1(1,1), ',', t1(2,1), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t1(1,2), ',', t1(2,2), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t1(1,3), ',', t1(2,3), ')'

  call poly_print ( d1, p1, '  Integrand p1(x,y)' )

  q = triangle_poly_integral ( d1, p1, t1 )
  q2 = 1.0D+00 / 6.0D+00

  write ( *, '(a,g14.6)' ) '  Computed Q = ', q
  write ( *, '(a,g14.6)' ) '  Exact Q    = ', q2
!
!  Test 2:
!  Integrate xy over a general triangle.
!
  p2 = (/ 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Triangle vertices:'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t2(1,1), ',', t2(2,1), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t2(1,2), ',', t2(2,2), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t2(1,3), ',', t2(2,3), ')'

  call poly_print ( d2, p2, '  Integrand p2(x,y)' );

  q = triangle_poly_integral ( d2, p2, t2 )
  q2 = 0.5D+00

  write ( *, '(a,g14.6)' ) '  Computed Q = ', q
  write ( *, '(a,g14.6)' ) '  Exact Q    = ', q2
!
!  Test 3:
!  Integrate 2-3x+xy over a general triangle.
!
  p3 = (/ 2.0, -3.0, 0.0, 0.0, 1.0, 0.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Triangle vertices:'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t3(1,1), ',', t3(2,1), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t3(1,2), ',', t3(2,2), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t3(1,3), ',', t3(2,3), ')'

  call poly_print ( d3, p3, '  Integrand p3(x,y)' );

  q = triangle_poly_integral ( d3, p3, t3 )
  q2 = 9.0D+00 / 8.0D+00

  write ( *, '(a,g14.6)' ) '  Computed Q = ', q
  write ( *, '(a,g14.6)' ) '  Exact Q    = ', q2
!
!  Test 4:
!  Integrate -40y + 6x^2 over a general triangle.
!
  p4 = (/ 0.0, 0.0,-40.0, 6.0, 0.0, 0.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Triangle vertices:'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t4(1,1), ',', t4(2,1), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t4(1,2), ',', t4(2,2), ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '    (', t4(1,3), ',', t4(2,3), ')'

  call poly_print ( d4, p4, '  Integrand p4(x,y)' );

  q = triangle_poly_integral ( d4, p4, t4 )
  q2 = - 935.0D+00 / 3.0D+00

  write ( *, '(a,g14.6)' ) '  Computed Q = ', q
  write ( *, '(a,g14.6)' ) '  Exact Q    = ', q2

  return
end
subroutine triangle_xy_integral_test ( )

!*****************************************************************************80
!
!! TRIANGLE_XY_INTEGRAL_TEST tests TRIANGLE_XY_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) q
  real ( kind = 8 ) triangle_xy_integral
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_XY_INTEGRAL_TEST'
  write ( *, '(a)' ) '  TRIANGLE_XY_INTEGRAL determines Q, the integral of the'
  write ( *, '(a)' ) '  monomial X*Y over a triangle (X1,Y1), (X2,Y2), (X3,Y3).'

  x1 = 0.0D+00
  y1 = 0.0D+00

  x2 = 1.0D+00
  y2 = 0.0D+00

  x3 = 1.0D+00
  y3 = 2.0D+00

  q = triangle_xy_integral ( x1, y1, x2, y2, x3, y3 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  (X1,Y1) = ( ', x1, ',', y1, ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  (X2,Y2) = ( ', x2, ',', y2, ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  (X3,Y3) = ( ', x3, ',', y3, ')'
  write ( *, '(a,g14.6)' ) '  Q = ', q
  write ( *, '(a)' ) '  (Expecting answer 1/2.'

  x1 = 0.0D+00
  y1 = 0.0D+00

  x2 = 4.0D+00
  y2 = 0.0D+00

  x3 = 0.0D+00
  y3 = 1.0D+00

  q = triangle_xy_integral ( x1, y1, x2, y2, x3, y3 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  (X1,Y1) = ( ', x1, ',', y1, ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  (X2,Y2) = ( ', x2, ',', y2, ')'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  (X3,Y3) = ( ', x3, ',', y3, ')'
  write ( *, '(a,g14.6)' ) '  Q = ', q
  write ( *, '(a)' ) '  (Expecting answer 2/3.'

  return
end
subroutine triangle01_monomial_integral_test ( )

!*****************************************************************************80
!
!! TRIANGLE01_MONOMIAL_INTEGRAL_TEST estimates integrals over the unit triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) q
  real ( kind = 8 ) triangle01_monomial_integral
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE01_MONOMIAL_INTEGRAL_TEST'
  write ( *, '(a)' ) '  TRIANGLE01_MONOMIAL_INTEGRAL returns the integral Q of'
  write ( *, '(a)' ) &
    '  a monomial X^I Y^J over the interior of the unit triangle.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I   J         Q(I,J)'

  do d = 0, 5
    write ( *, '(a)' ) ''
    do i = 0, d
      j = d - i
      q = triangle01_monomial_integral ( i, j )
      write ( *, '(2x,i2,2x,i2,2x,g14.6)' ) i, j, q
    end do
  end do

  return
end
subroutine triangle01_poly_integral_test ( )

!*****************************************************************************80
!
!! TRIANGLE01_POLY_INTEGRAL_TEST: polynomial integrals over the unit triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: d_max = 6
  integer ( kind = 4 ), parameter :: d1 = 1
  integer ( kind = 4 ), parameter :: d2 = 2
  integer ( kind = 4 ), parameter :: d3 = 2

  integer ( kind = 4 ), parameter :: m_max = ( ( d_max + 1 ) * ( d_max + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m1 = ( ( d1 + 1 ) * ( d1 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) p1(m1)
  real ( kind = 8 ) p2(m2)
  real ( kind = 8 ) p3(m3)
  real ( kind = 8 ) q
  real ( kind = 8 ) q2
  real ( kind = 8 ) qm(m_max)
  real ( kind = 8 ) triangle01_monomial_integral
  real ( kind = 8 ) triangle01_poly_integral

  do k = 1, m_max
    call i4_to_pascal ( k, i, j )
    qm(k) = triangle01_monomial_integral ( i, j )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE01_POLY_INTEGRAL_TEST'
  write ( *, '(a)' ) '  TRIANGLE01_POLY_INTEGRAL returns the integral Q of'
  write ( *, '(a)' ) '  a polynomial P(X,Y) over the interior of the unit triangle.'

  p1 = (/ 1.0, 2.0, 3.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d1, p1, '  p(x,y)' )
  q = triangle01_poly_integral ( d1, p1 )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Q =         ', q
  q2 = dot_product ( p1(1:m1), qm(1:m1) )
  write ( *, '(a,g14.6)' ) '  Q (exact) = ', q2

  p2 = (/ 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d2, p2, '  p(x,y)' )
  q = triangle01_poly_integral ( d2, p2 )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Q =         ', q
  q2 = dot_product ( p2(1:m2), qm(1:m2) )
  write ( *, '(a,g14.6)' ) '  Q (exact) = ', q2

  p3 = (/ 1.0, -2.0, 3.0, -4.0, 5.0, -6.0 /)
  write ( *, '(a)' ) ''
  call poly_print ( d3, p3, '  p(x,y)' )
  q = triangle01_poly_integral ( d3, p3 )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Q =         ', q
  q2 = dot_product ( p3(1:m3), qm(1:m3) )
  write ( *, '(a,g14.6)' ) '  Q (exact) = ', q2

  return
end
subroutine trinomial_test ( )

!*****************************************************************************80
!
!! TRINOMIAL_TEST tests TRINOMIAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) t
  integer ( kind = 4 ) trinomial

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRINOMIAL_TEST'
  write ( *, '(a)' ) '  TRINOMIAL evaluates the trinomial coefficient:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  T(I,J,K) = (I+J+K)! / I! / J! / K!'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I     J     K    T(I,J,K)'
  write ( *, '(a)' ) ' '
 
  do k = 0, 4
    do j = 0, 4
      do i = 0, 4
        t = trinomial ( i, j, k )
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,i8)' ) i, j, k, t
      end do
    end do
  end do
 
  return
end
subroutine xy_to_rs_map_test ( )

!*****************************************************************************80
!
!! XY_TO_RS_MAP_TEST tests XY_TO_RS_MAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
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
  integer ( kind = 4 ) j
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ), dimension ( 2, 3 ) :: t = reshape ( (/ &
    2.0, 0.0, &
    3.0, 4.0, &
    0.0, 3.0 /), (/ 2, 3 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'XY_TO_RS_MAP_TEST:'
  write ( *, '(a)' ) '  XY_TO_RS_MAP determines the coefficients of the linear'
  write ( *, '(a)' ) '  map from a general triangle in XY coordinates '
  write ( *, '(a)' ) '  to the reference triangle in RS coordinates:'
  write ( *, '(a)' ) '    R = a + b * X + c * Y'
  write ( *, '(a)' ) '    S = d + e * X + f * Y'

  call r8mat_print ( 2, 3, t, '  XY triangle vertices:' )

  call xy_to_rs_map ( t, a, b, c, d, e, f )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Mapping coefficients are:'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6,a,g14.6,a)' ) &
    '    R = ', a, ' + ', b, ' * X + ', c, ' * Y'
  write ( *, '(a,g14.6,a,g14.6,a,g14.6,a)' ) &
    '    S = ', d, ' + ', e, ' * X + ', f, ' * Y'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Apply map to XY triangle vertices.'
  write ( *, '(a)' ) '  Recover RS vertices (0,0), (1,0) and (0,1).'
  write ( *, '(a)' ) ''
  do j = 1, 3
    r = a + b * t(1,j) + c * t(2,j)
    s = d + e * t(1,j) + f * t(2,j)
    write ( *, '(a,i1,a,g14.6,a,g14.6,a)' ) '  V(', j, ') = ( ', r, ',', s, ' )'
  end do

  return
end
