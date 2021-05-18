program main

!*****************************************************************************80
!
!! MAIN is the main program for CHEBYSHEV_POLYNOMIAL_TEST.
!
!  Discussion:
!
!    CHEBYSHEV_POLYNOMIAL_TEST tests the CHEBYSHEV_POLYNOMIAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CHEBYSHEV_POLYNOMIAL_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the CHEBYSHEV_POLYNOMIAL library.'

  call test01 ( )
  call t_mass_matrix_test ( )
  call t_moment_test ( )
  call t_polynomial_test ( )
  call t_polynomial_ab_test ( )
  call t_polynomial_ab_value_test ( )
  call t_polynomial_coefficients_test ( )
  call t_polynomial_plot_test ( )
  call t_polynomial_value_test ( )
  call t_polynomial_zeros_test ( )
  call t_quadrature_rule_test ( )
  call test07 ( )
  call test08 ( )
  call test09 ( )
  call test10 ( )
  call tt_product_test ( )
  call tt_product_integral_test ( )
  call ttt_product_integral_test ( )
  call tu_product_test ( )

  call u_mass_matrix_test ( )
  call u_moment_test ( )
  call u_polynomial_test ( )
  call u_polynomial_ab_test ( )
  call u_polynomial_ab_value_test ( )
  call u_polynomial_coefficients_test ( )
  call u_polynomial_plot_test ( )
  call u_polynomial_value_test ( )
  call u_polynomial_zeros_test ( )
  call u_quadrature_rule_test ( )
  call uu_product_test ( )
  call uu_product_integral_test ( )

  call v_mass_matrix_test ( )
  call v_moment_test ( )
  call v_polynomial_test ( )
  call v_polynomial_ab_test ( )
  call v_polynomial_ab_value_test ( )
  call v_polynomial_coefficients_test ( )
  call v_polynomial_plot_test ( )
  call v_polynomial_value_test ( )
  call v_polynomial_zeros_test ( )
  call v_quadrature_rule_test ( ) 
  call vv_product_integral_test ( )

  call w_mass_matrix_test ( )
  call w_moment_test ( )
  call w_polynomial_test ( )
  call w_polynomial_ab_test ( )
  call w_polynomial_ab_value_test ( )
  call w_polynomial_coefficients_test ( )
  call w_polynomial_plot_test ( )
  call w_polynomial_value_test ( )
  call w_polynomial_zeros_test ( )
  call w_quadrature_rule_test ( ) 
  call ww_product_integral_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CHEBYSHEV_POLYNOMIAL_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests T_PROJECT_COEFFICIENTS_DATA.
!
!  Discussion:
!
!    Thanks to Dick Chaffer for pointing out that the coefficients were
!    computed for Chebyshev polynomials in [0,1], but then evaluated
!    using Chebyshev polynomials based in [-1,+1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), allocatable :: c(:)
  real ( kind = 8 ), allocatable :: d(:)
  real ( kind = 8 ), allocatable :: d2(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CHEBYSHEV_POLYNOMIAL_TEST01:'
  write ( *, '(a)' ) '  T_PROJECT_COEFFICIENTS_DATA estimates the Chebyshev polynomial'
  write ( *, '(a)' ) '  coefficients for a function given as data (x,fx).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Here, we use fx = f(x) = x^2 for the data.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Since T(0,x) = 1 and T(2,x) = 2*x^2 - 1, the correct expansion is'
  write ( *, '(a)' ) '  f(x) = 1/2 T(0,x) + 0 T(1,x) + 1/2 T(2,x) + 0 * all other polys'
  write ( *, '(a)' ) '  if we are using the interval [A,B] = [-1,+1].'
!
!  Data in [0,1];
!
  a = 0.0D+00
  b = 1.0D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  Using interval [A,B] = [', a, ', ', b, ']'
!
!  Get sample values of X^2 in [A,B].
!
  m = 20
  seed = 123456789
  allocate ( x(1:m) )
  call r8vec_uniform_ab ( m, a, b, seed, x )
  allocate ( d(1:m) )
  d(1:m) = x(1:m)**2

  call r8vec2_print ( m, x, d, '  Data ( X, D(X) ):' )
!
!  Seek a Chebyshev expansion with the Chebyshev polynomials based in [A,B]=[0,1].
!
  n = 4
  allocate ( c(0:n) )
  call t_project_coefficients_data ( a, b, m, n, x, d, c )
  
  call r8vec_print ( n, c, '  Coefficients of Chebyshev expansion of degree 4 based in [A,B].' )
!
!  Compare the Chebyshev expansion and the original function.
!
  allocate ( d2(1:m) )

  call t_project_value_ab ( m, n, x, c, a, b, d2 )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I      X(I)     D(X(I))      Chebyshev(X(I))'
  write ( *, '(a)' ) ' '
  do i = 1, m
    write ( *, '(2x,i2,2x,g12.4,2x,g12.4,2x,g12.4)' ) i, x(i), d(i), d2(i)
  end do
!
!  Free memory.
!
  deallocate ( c )
  deallocate ( d )
  deallocate ( d2 )
  deallocate ( x )

  return
end
subroutine t_mass_matrix_test ( )

!*****************************************************************************80
!
!! T_MASS_MATRIX_TEST tests T_MASS_MATRIX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'T_MASS_MATRIX_TEST:'
  write ( *, '(a)' ) '  T_MASS_MATRIX computes the mass matrix for the'
  write ( *, '(a)' ) '  Chebyshev polynomials T(i,x).'
  write ( *, '(a)' ) &
    '  A(I,J) = integral ( -1 <=x <= +1 ) T(i,x) T(j,x) / sqrt ( 1 - x^2 ) dx'
  write ( *, '(a)' ) '  0    if i is not equal to j;'
  write ( *, '(a)' ) '  pi   if i = j = 0;'
  write ( *, '(a)' ) '  pi/2 if i = j =/= 0.'

  n = 3
  allocate ( a(0:n,0:n) )

  call t_mass_matrix ( n, a )

  call r8mat_print ( n + 1, n + 1, a, '  T mass matrix:' )

  return
end
subroutine t_moment_test ( )

!*****************************************************************************80
!
!! T_MOMENT_TEST tests T_MOMENT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e
  real ( kind = 8 ) t_moment
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'T_MOMENT_TEST:'
  write ( *, '(a)' ) '  T_MOMENT returns the value of'
  write ( *, '(a)' ) '  integral ( -1 <=x <= +1 ) x^e / sqrt ( 1 - x^2 ) dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   E       Integral'
  write ( *, '(a)' ) ''
  do e = 0, 10
    value = t_moment ( e )
    write ( *, '(2x,i2,2x,g14.6)' ) e, value
  end do

  return
end
subroutine t_polynomial_test ( )

!*****************************************************************************80
!
!! T_POLYNOMIAL_TEST tests T_POLYNOMIAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ), allocatable :: fx2(:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'T_POLYNOMIAL_TEST:'
  write ( *, '(a)' ) '  T_POLYNOMIAL evaluates Chebyshev polynomials T(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        T(n,x)        T(n,x)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call t_polynomial_values ( n_data, n, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    if ( n < 0 ) then
      cycle
    end if

    allocate ( fx2(0:n) )

    call t_polynomial ( 1, n, x, fx2 )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

    deallocate ( fx2 )

  end do

  return
end
subroutine t_polynomial_ab_test ( )

!*****************************************************************************80
!
!! T_POLYNOMIAL_AB_TEST tests T_POLYNOMIAL_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 11
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) v(m,0:n)
  real ( kind = 8 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'T_POLYNOMIAL_AB_TEST:'
  write ( *, '(a)' ) '  T_POLYNOMIAL_AB evaluates Chebyshev polynomials TAB(n,x)'
  write ( *, '(a)' ) '  shifted from [-1,+1] to the domain [A,B].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here, we will use the new domain [0,1]'
  write ( *, '(a)' ) '  and the desired maximum polynomial degree will be N = 5.'

  a = 0.0D+00
  b = 1.0D+00
  call r8vec_linspace ( m, a, b, x )
  
  call t_polynomial_ab ( a, b, m, n, x, v )

  call r8mat_print ( m, n + 1, v, '  Tables of T values:' )

  return
end
subroutine t_polynomial_ab_value_test ( )

!*****************************************************************************80
!
!! T_POLYNOMIAL_AB_VALUE_TEST tests T_POLYNOMIAL_AB_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) t_polynomial_ab_value
  real ( kind = 8 ) x01

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'T_POLYNOMIAL_AB_VALUE_TEST:'
  write ( *, '(a)' ) '  T_POLYNOMIAL_AB_VALUE evaluates Chebyshev polynomials TAB(n,x)'
  write ( *, '(a)' ) '  shifted from [-1,+1] to the domain [A,B].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here, we will use the new domain [0,1].'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        T01(n,x)      T01(n,x)'
  write ( *, '(a)' ) ' '

  a = 0.0D+00
  b = 1.0D+00

  n_data = 0

  do

    call t_polynomial_01_values ( n_data, n, x01, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = t_polynomial_ab_value ( a, b, n, x01 )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x01, fx, fx2

  end do

  return
end
subroutine t_polynomial_coefficients_test ( )

!*****************************************************************************80
!
!! T_POLYNOMIAL_COEFFICIENTS_TEST tests T_POLYNOMIAL_COEFFICIENTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) c(0:n,0:n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'T_POLYNOMIAL_COEFFICIENTS_TEST'
  write ( *, '(a)' ) '  T_POLYNOMIAL_COEFFICIENTS determines coefficients for'
  write ( *, '(a)' ) '  Chebyshev polynomials T(n,x).'

  call t_polynomial_coefficients ( n, c )
 
  do i = 0, n
    call r8poly_print ( i, c(i,0:i), '' );
  end do

  return
end
subroutine t_polynomial_plot_test ( )

!*****************************************************************************80
!
!! T_POLYNOMIAL_PLOT_TEST tests T_POLYNOMIAL_PLOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_num = 6

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n_val(n_num)
  character ( len = 255 ) output_filename

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'T_POLYNOMIAL_PLOT_TEST'
  write ( *, '(a)' ) '  T_POLYNOMIAL_PLOT plots selected '
  write ( *, '(a)' ) '  Chebyshev polynomials T(n,x).'

  do i = 0, 5
    n_val(i+1) = i
  end do

  output_filename = 't_polynomial_plot.png'

  call t_polynomial_plot( n_num, n_val, output_filename )

  return
end
subroutine t_polynomial_value_test ( )

!*****************************************************************************80
!
!! T_POLYNOMIAL_VALUE_TEST tests T_POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) t_polynomial_value
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'T_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  T_POLYNOMIAL_VALUE evaluates Chebyshev polynomials T(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        T(n,x)        T(n,x)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call t_polynomial_values ( n_data, n, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = t_polynomial_value ( n, x )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2

  end do

  return
end
subroutine t_polynomial_zeros_test ( )

!*****************************************************************************80
!
!! T_POLYNOMIAL_ZEROS_TEST tests T_POLYNOMIAL_ZEROS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 April 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: fx(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: n_max = 5
  real ( kind = 8 ), allocatable :: z(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'T_POLYNOMIAL_ZEROS_TEST:'
  write ( *, '(a)' ) '  T_POLYNOMIAL_ZEROS returns zeroes of T(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       N      X        T(n,x)'

  do n = 1, n_max

    allocate ( z(1:n) )

    call t_polynomial_zeros ( n, z )

    allocate ( fx(1:n,0:n) )

    call t_polynomial ( n, n, z, fx )

    write ( *, '(a)' ) ' '
    do i = 1, n
      write ( *, '(2x,i8,2x,f8.4,2x,g14.6)' ) n, z(i), fx(i,n)
    end do

    deallocate ( fx )
    deallocate ( z )

  end do

  return
end
subroutine t_quadrature_rule_test ( )

!*****************************************************************************80
!
!! T_QUADRATURE_RULE_TEST tests T_QUADRATURE_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e
  real ( kind = 8 ), allocatable :: f(:)
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ) q_exact
  real ( kind = 8 ) t_moment
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'T_QUADRATURE_RULE_TEST:'
  write ( *, '(a)' ) '  T_QUADRATURE_RULE computes the quadrature rule'
  write ( *, '(a)' ) '  associated with T(n,x);'

  n = 7
  allocate ( x(1:n) )
  allocate ( w(1:n) )

  call t_quadrature_rule ( n, x, w )

  call r8vec2_print ( n, x, w, '      X            W' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Q = Integral ( -1 <= X <= +1 ) X^E / sqrt ( 1-x^2) dx'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   E       Q_Estimate      Q_Exact'
  write ( *, '(a)' ) ' '

  allocate ( f(1:n) )

  do e = 0, 2 * n - 1
    if ( e == 0 ) then
      f(1:n) = 1.0D+00
    else
      f(1:n) = x(1:n) ** e
    end if
    q = dot_product ( w, f )
    q_exact = t_moment ( e )
    write ( *, '(2x,i2,g14.6,2x,g14.6)' ) e, q, q_exact
  end do

  deallocate ( f )
  deallocate ( w )
  deallocate ( x )

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! TEST07 tests T_PROJECT_COEFFICIENTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), allocatable :: c(:)
  intrinsic dexp
  intrinsic dsin
  intrinsic dsqrt
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST07:'
  write ( *, '(a)' ) '  T_PROJECT_COEFFICIENTS computes the Chebyshev coefficients'
  write ( *, '(a)' ) '  of a function defined over [-1,+1].'
  write ( *, '(a)' ) '  T_PROJECT_COEFFICIENTS_AB works in [A,B].'

  n = 3
  allocate ( c(0:n) )
  call t_project_coefficients ( n, dexp, c )
  call r8vec_print ( n + 1, c, '  Chebyshev coefficients for exp(x) in [-1,+1]' )
  deallocate ( c )

  n = 5
  allocate ( c(0:n) )
  call t_project_coefficients ( n, dexp, c )
  call r8vec_print ( n + 1, c, '  Chebyshev coefficients for exp(x) in [-1,+1]' )
  deallocate ( c )

  n = 5
  allocate ( c(0:n) )
  call t_project_coefficients ( n, dsin, c )
  call r8vec_print ( n + 1, c, '  Chebyshev coefficients for sin(x) in [-1,+1]' )
  deallocate ( c )
!
!  Repeat calculation with T_PROJECT_COEFFICIENTS_AB.
!
  n = 5
  allocate ( c(0:n) )
  a = -1.0D+00
  b = +1.0D+00
  call t_project_coefficients_ab ( n, dsin, a, b, c )
  call r8vec_print ( n + 1, c, '  Chebyshev coefficients for sin(x) in [-1,+1]' )
  deallocate ( c )
!
!  Now try a different interval.
!
  n = 5
  allocate ( c(0:n) )
  a = 0.0D+00
  b = 1.0D+00
  call t_project_coefficients_ab ( n, dsqrt, a, b, c )
  call r8vec_print ( n + 1, c, '  Chebyshev coefficients for sqrt(x) in [0,+1]' )
  deallocate ( c )

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 tests T_PROJECT_COEFFICIENTS_DATA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), allocatable :: c(:)
  real ( kind = 8 ), allocatable :: d(:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST08:'
  write ( *, '(a)' ) '  T_PROJECT_COEFFICIENTS_DATA computes the Chebyshev'
  write ( *, '(a)' ) '  coefficients of a function defined by data.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  We are looking for an approximation that is good in [-1,+1].'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Begin by using equally spaced points in [-1,+1].'

  a = -1.0D+00
  b = +1.0D+00
  m = 10
  allocate ( x(1:m) )
  allocate ( d(1:m) )
  call r8vec_linspace ( m, a, b, x )
  d(1:m) = exp ( x(1:m) )
  n = 3
  allocate ( c(0:n) )
  call t_project_coefficients_data ( a, b, m, n, x, d, c )
  call r8vec_print ( n + 1, c, '  Chebyshev coefficients for exp(x) on [-1,+1]' )
  deallocate ( c )
  deallocate ( d )
  deallocate ( x )

  a = -1.0D+00
  b = +1.0D+00
  m = 10
  allocate ( x(1:m) )
  allocate ( d(1:m) )
  call r8vec_linspace ( m, a, b, x )
  d(1:m) = exp ( x(1:m) )
  n = 5
  allocate ( c(0:n) )
  call t_project_coefficients_data ( a, b, m, n, x, d, c )
  call r8vec_print ( n + 1, c, '  Chebyshev coefficients for exp(x) on [-1,+1]' )
  deallocate ( c )
  deallocate ( d )
  deallocate ( x )

  a = -1.0D+00
  b = +1.0D+00
  m = 10
  allocate ( x(1:m) )
  allocate ( d(1:m) )
  call r8vec_linspace ( m, a, b, x )
  d(1:m) = sin ( x(1:m) )
  n = 5
  allocate ( c(0:n) )
  call t_project_coefficients_data ( a, b, m, n, x, d, c )
  call r8vec_print ( n + 1, c, '  Chebyshev coefficients for sin(x) on [-1,+1]' )
  deallocate ( c )
  deallocate ( d )
  deallocate ( x )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Now sample equally spaced points in [0,+1].'
  write ( *, '(a)' ) '  The approximation still applies to the interval [-1,+1].'

  a = 0.0D+00
  b = +1.0D+00
  m = 10
  allocate ( x(1:m) )
  allocate ( d(1:m) )
  call r8vec_linspace ( m, a, b, x )
  d(1:m) = sin ( x(1:m) )
  n = 5
  allocate ( c(0:n) )
  call t_project_coefficients_data ( a, b, m, n, x, d, c )
  call r8vec_print ( n + 1, c, '  Chebyshev coefficients for sin(x) on [0,+1]' )
  deallocate ( c )
  deallocate ( d )
  deallocate ( x )

  a = 0.0D+00
  b = +1.0D+00
  m = 10
  allocate ( x(1:m) )
  allocate ( d(1:m) )
  call r8vec_linspace ( m, a, b, x )
  d(1:m) = sqrt ( x(1:m) )
  n = 5
  allocate ( c(0:n) )
  call t_project_coefficients_data ( a, b, m, n, x, d, c )
  call r8vec_print ( n + 1, c, '  Chebyshev coefficients for sqrt(x) on [0,+1]' )
  deallocate ( c )
  deallocate ( d )
  deallocate ( x )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Now random points in [-1,+1].'

  a = -1.0D+00
  b = +1.0D+00
  m = 10
  allocate ( x(1:m) )
  allocate ( d(1:m) )
  seed = 123456789
  call r8vec_uniform_01 ( m, seed, x )
  x = x * b + ( 1.0D+00 - x ) * a
  d(1:m) = sin ( x(1:m) )
  n = 5
  allocate ( c(0:n) )
  call t_project_coefficients_data ( a, b, m, n, x, d, c )
  call r8vec_print ( n + 1, c, '  Chebyshev coefficients for sin(x) on [-1,+1]' )
  deallocate ( c )
  deallocate ( d )
  deallocate ( x )

  return
end
subroutine test09 ( )

!*****************************************************************************80
!
!! TEST09 compares a function and projection over [-1,+1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), allocatable :: c(:)
  intrinsic dexp
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ), allocatable :: v(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST09:'
  write ( *, '(a)' ) '  T_PROJECT_COEFFICIENTS computes the Chebyshev interpolant C(F)(N,X)'
  write ( *, '(a)' ) '  of a function F(X) defined over [-1,+1].'
  write ( *, '(a)' ) '  T_PROJECT_VALUE evaluates that projection.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Compute projections of order N to exp(x) over [-1,+1],'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   N   Max||F(X)-C(F)(N,X)||'
  write ( *, '(a)' ) ' '

  a = -1.0D+00
  b = +1.0D+00

  do n = 0, 10
    allocate ( c(0:n) )
    call t_project_coefficients ( n, dexp, c )
    m = 101
    allocate ( x(1:m) )
    call r8vec_linspace ( m, a, b, x )
    allocate ( v(1:m) )
    call t_project_value ( m, n, x, c, v )
    allocate ( r(1:m) )
    r(1:m) = v(1:m) - exp ( x(1:m) )
    write ( *, '(2x,i2,2x,g12.4)' ) n, maxval ( abs ( r ) )
    deallocate ( c )
    deallocate ( r )
    deallocate ( v )
    deallocate ( x )
  end do

  return
end
subroutine test10 ( )

!*****************************************************************************80
!
!! TEST10 compares a function and projection over [A,B].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), allocatable :: c(:)
  intrinsic dexp
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ), allocatable :: v(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST10:'
  write ( *, '(a)' ) '  T_PROJECT_COEFFICIENTS_AB computes the Chebyshev interpolant C(F)(N,X)'
  write ( *, '(a)' ) '  of a function F(X) defined over [A,B].'
  write ( *, '(a)' ) '  T_PROJECT_VALUE_AB evaluates that projection.'

  a = 0.0D+00
  b = 1.5D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,f4.1,a,f4.1,a)' ) &
    '  Compute projections of order N to exp(x) over [', a, ',', b, ']'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   N   Max||F(X)-C(F)(N,X)||'
  write ( *, '(a)' ) ' '

  do n = 0, 10
    allocate ( c(0:n) )
    call t_project_coefficients_ab ( n, dexp, a, b, c )
    m = 101
    allocate ( x(1:m) )
    call r8vec_linspace ( m, a, b, x )
    allocate ( v(1:m) )
    call t_project_value_ab ( m, n, x, c, a, b, v )
    allocate ( r(1:m) )
    r(1:m) = v(1:m) - exp ( x(1:m) )
    write ( *, '(2x,i2,2x,g12.4)' ) n, maxval ( abs ( r ) )
    deallocate ( c )
    deallocate ( r )
    deallocate ( v )
    deallocate ( x )
  end do

  return
end
subroutine tt_product_test ( )

!*****************************************************************************80
!
!! TT_PRODUCT_TEST tests TT_PRODUCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t_polynomial_value
  real ( kind = 8 ) tt_product
  integer ( kind = 4 ) test
  real ( kind = 8 ) ti
  real ( kind = 8 ) titj
  real ( kind = 8 ) tj
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TT_PRODUCT_TEST:'
  write ( *, '(a)' ) '  TT_PRODUCT(I,J;X) = T(I,X) * T(J,X)'

  r8_lo = -1.0D+00
  r8_hi = +1.0D+00
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I   J      X               TI              TJ              TI*TJ       TT_PRODUCT' 
  write ( *, '(a)' ) '' 
  do test = 1, 10
    x = r8_uniform_ab ( r8_lo, r8_hi, seed )
    i = i4_uniform_ab ( 0, 6, seed )
    ti = t_polynomial_value ( i, x )
    j = i4_uniform_ab ( -1, 4, seed )
    tj = t_polynomial_value ( j, x )
    titj = tt_product ( i, j, x )
    write ( *, '(2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, j, x, ti, tj, ti * tj, titj
  end do

  return
end
subroutine tt_product_integral_test ( )

!*****************************************************************************80
!
!! TT_PRODUCT_INTEGRAL_TEST tests TT_PRODUCT_INTEGRAL.
!
!  Discussion:
!
!    This process should match the T_MASS_MATRIX computation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) tt_product_integral

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TT_PRODUCT_INTEGRAL_TEST:'
  write ( *, '(a)' ) '  TT_PRODUCT_INTEGRAL computes the product integral'
  write ( *, '(a)' ) '  of a pair of Chebyshev T polynomials T(i,x) and T(j,x).'
  write ( *, '(a)' ) &
    '  A(I,J) = integral ( -1 <=x <= +1 ) T(i,x) T(j,x) / sqrt ( 1 - x^2 ) dx'
  write ( *, '(a)' ) '  0    if i is not equal to j;'
  write ( *, '(a)' ) '  pi   if i = j = 0;'
  write ( *, '(a)' ) '  pi/2 if i = j =/= 0.'

  n = 4
  allocate ( a(0:n,0:n) )
  do i = 0, n
    do j = 0, n
      a(i,j) = tt_product_integral ( i, j )
    end do
  end do

  call r8mat_print ( n + 1, n + 1, a, '  T(i,x)*T(j,x) integral matrix:' )

  return
end
subroutine ttt_product_integral_test ( )

!*****************************************************************************80
!
!! TTT_PRODUCT_INTEGRAL_TEST tests TTT_PRODUCT_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t_polynomial_value
  real ( kind = 8 ) ttt_product_integral
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 20
  real ( kind = 8 ) ti
  real ( kind = 8 ) tj
  real ( kind = 8 ) tk
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TTT_PRODUCT_INTEGRAL_TEST:'
  write ( *, '(a)' ) '  TTT_PRODUCT_INTEGRAL computes the triple integral'
  write ( *, '(a)' ) '  Tijk = integral ( -1 <= x <= 1 ) T(i,x) T(j,x) T(k,x) / sqrt ( 1-x^2) dx'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I   J   K     Tijk           Tijk'
  write ( *, '(a)' ) '                 computed       exact'
  write ( *, '(a)' ) ' '

  n = 15
  allocate ( x(1:n) )
  allocate ( w(1:n) )

  call t_quadrature_rule ( n, x, w )

  seed = 123456789

  do test = 1, test_num
    i = i4_uniform_ab ( 2, 6, seed )
    j = i4_uniform_ab ( 1, 3, seed )
    k = i4_uniform_ab ( 0, 4, seed )
    fx1 = ttt_product_integral ( i, j, k )
    fx2 = 0.0D+00
    do l = 1, n
      ti = t_polynomial_value ( i, x(l) )
      tj = t_polynomial_value ( j, x(l) )
      tk = t_polynomial_value ( k, x(l) )
      fx2 = fx2 + w(l) * ti * tj * tk
    end do
    write ( *, '(2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6)' ) i, j, k, fx1, fx2
  end do

  deallocate ( x )
  deallocate ( w )

  return
end
subroutine tu_product_test ( )

!*****************************************************************************80
!
!! TU_PRODUCT_TEST tests TU_PRODUCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t_polynomial_value
  real ( kind = 8 ) tu_product
  integer ( kind = 4 ) test
  real ( kind = 8 ) ti
  real ( kind = 8 ) tiuj
  real ( kind = 8 ) u_polynomial_value
  real ( kind = 8 ) uj
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TU_PRODUCT_TEST:'
  write ( *, '(a)' ) '  TU_PRODUCT(I,J;X) = T(I,X) * U(J,X)'

  r8_lo = -1.0D+00
  r8_hi = +1.0D+00
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I   J      X               TI              UJ              TI*UJ       TU_PRODUCT' 
  write ( *, '(a)' ) '' 
  do test = 1, 10
    x = r8_uniform_ab ( r8_lo, r8_hi, seed )
    i = i4_uniform_ab ( 0, 6, seed )
    ti = t_polynomial_value ( i, x )
    j = i4_uniform_ab ( -1, 4, seed )
    uj = u_polynomial_value ( j, x )
    tiuj = tu_product ( i, j, x )
    write ( *, '(2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, j, x, ti, uj, ti * uj, tiuj
  end do

  return
end
subroutine u_mass_matrix_test ( )

!*****************************************************************************80
!
!! U_MASS_MATRIX_TEST tests U_MASS_MATRIX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'U_MASS_MATRIX_TEST:'
  write ( *, '(a)' ) '  U_MASS_MATRIX computes the mass matrix for the'
  write ( *, '(a)' ) '  Chebyshev polynomials U(i,x).'
  write ( *, '(a)' ) &
    '  A(I,J) = integral ( -1 <=x <= +1 ) U(i,x) U(j,x) * sqrt ( 1 - x^2 ) dx'
  write ( *, '(a)' ) '  0    if i is not equal to j;'
  write ( *, '(a)' ) '  pi/2 if i = j =/= 0.'

  n = 3
  allocate ( a(0:n,0:n) )

  call u_mass_matrix ( n, a )

  call r8mat_print ( n + 1, n + 1, a, '  U mass matrix:' )

  return
end
subroutine u_moment_test ( )

!*****************************************************************************80
!
!! U_MOMENT_TEST tests U_MOMENT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e
  real ( kind = 8 ) u_moment
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'U_MOMENT_TEST:'
  write ( *, '(a)' ) '  U_MOMENT returns the value of'
  write ( *, '(a)' ) '  integral ( -1 <=x <= +1 ) x^e * sqrt ( 1 - x^2 ) dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   E       Integral'
  write ( *, '(a)' ) ''
  do e = 0, 10
    value = u_moment ( e )
    write ( *, '(2x,i2,2x,g14.6)' ) e, value
  end do

  return
end
subroutine u_polynomial_test ( )

!*****************************************************************************80
!
!! U_POLYNOMIAL_TEST tests U_POLYNOMIAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ), allocatable :: fx2(:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'U_POLYNOMIAL_TEST:'
  write ( *, '(a)' ) '  U_POLYNOMIAL evaluates the Chebyshev polynomials U(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        U(n,x)        U(n,x)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call u_polynomial_values ( n_data, n, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    if ( n < 0 ) then
      cycle
    end if

    allocate ( fx2(0:n) )

    call u_polynomial ( 1, n, x, fx2 )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

    deallocate ( fx2 )

  end do

  return
end
subroutine u_polynomial_ab_test ( )

!*****************************************************************************80
!
!! U_POLYNOMIAL_AB_TEST tests U_POLYNOMIAL_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 11
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) v(m,0:n)
  real ( kind = 8 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'U_POLYNOMIAL_AB_TEST:'
  write ( *, '(a)' ) '  U_POLYNOMIAL_AB evaluates Chebyshev polynomials UAB(n,x)'
  write ( *, '(a)' ) '  shifted from [-1,+1] to the domain [A,B].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here, we will use the new domain [0,1]'
  write ( *, '(a)' ) '  and the desired maximum polynomial degree will be N = 5.'

  a = 0.0D+00
  b = 1.0D+00
  call r8vec_linspace ( m, a, b, x )
  
  call u_polynomial_ab ( a, b, m, n, x, v )

  call r8mat_print ( m, n + 1, v, '  Tables of U values:' )

  return
end
subroutine u_polynomial_ab_value_test ( )

!*****************************************************************************80
!
!! U_POLYNOMIAL_AB_VALUE_TEST tests U_POLYNOMIAL_AB_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) U_polynomial_ab_value
  real ( kind = 8 ) x01

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'U_POLYNOMIAL_AB_VALUE_TEST:'
  write ( *, '(a)' ) '  U_POLYNOMIAL_AB_VALUE evaluates Chebyshev polynomials UAB(n,x)'
  write ( *, '(a)' ) '  shifted from [-1,+1] to the domain [A,B].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here, we will use the new domain [0,1].'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        U01(n,x)      U01(n,x)'
  write ( *, '(a)' ) ' '

  a = 0.0D+00
  b = 1.0D+00

  n_data = 0

  do

    call u_polynomial_01_values ( n_data, n, x01, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = u_polynomial_ab_value ( a, b, n, x01 )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x01, fx, fx2

  end do

  return
end
subroutine u_polynomial_coefficients_test ( )

!*****************************************************************************80
!
!! U_POLYNOMIAL_COEFFICIENTS_TEST tests U_POLYNOMIAL_COEFFICIENTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) c(0:n,0:n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'U_POLYNOMIAL_COEFFICIENTS_TEST'
  write ( *, '(a)' ) '  U_POLYNOMIAL_COEFFICIENTS determines coefficients'
  write ( *, '(a)' ) '  for Chebyshev polynomials U(n,x).'

  call u_polynomial_coefficients ( n, c )
 
  do i = 0, n
    call r8poly_print ( i, c(i,0:i), '' );
  end do

  return
end
subroutine u_polynomial_plot_test ( )

!*****************************************************************************80
!
!! U_POLYNOMIAL_PLOT_TEST tests U_POLYNOMIAL_PLOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_num = 6

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n_val(n_num)
  character ( len = 255 ) output_filename

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'U_POLYNOMIAL_PLOT_TEST'
  write ( *, '(a)' ) '  U_POLYNOMIAL_PLOT plots selected '
  write ( *, '(a)' ) '  Chebyshev polynomials U(n,x).'

  do i = 0, 5
    n_val(i+1) = i
  end do

  output_filename = 'u_polynomial_plot.png'

  call u_polynomial_plot( n_num, n_val, output_filename )

  return
end
subroutine u_polynomial_value_test ( )

!*****************************************************************************80
!
!! U_POLYNOMIAL_VALUE_TEST tests U_POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) u_polynomial_value
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'U_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  U_POLYNOMIAL_VALUE evaluates the Chebyshev polynomials U(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        U(n,x)        U(n,x)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call u_polynomial_values ( n_data, n, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = u_polynomial_value ( n, x )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2

  end do

  return
end
subroutine u_polynomial_zeros_test ( )

!*****************************************************************************80
!
!! U_POLYNOMIAL_ZEROS_TEST tests U_POLYNOMIAL_ZEROS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 April 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: fx(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: n_max = 5
  real ( kind = 8 ), allocatable :: z(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'U_POLYNOMIAL_ZEROS_TEST:'
  write ( *, '(a)' ) '  U_POLYNOMIAL_ZEROS returns zeroes of U(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       N      X        U(n,x)'

  do n = 1, n_max

    allocate ( z(1:n) )

    call u_polynomial_zeros ( n, z )

    allocate ( fx(1:n,0:n) )

    call u_polynomial ( n, n, z, fx )

    write ( *, '(a)' ) ' '
    do i = 1, n
      write ( *, '(2x,i8,2x,f8.4,2x,g14.6)' ) n, z(i), fx(i,n)
    end do

    deallocate ( fx )
    deallocate ( z )

  end do

  return
end
subroutine u_quadrature_rule_test ( )

!*****************************************************************************80
!
!! U_QUADRATURE_RULE_TEST tests U_QUADRATURE_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e
  real ( kind = 8 ), allocatable :: f(:)
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ) q_exact
  real ( kind = 8 ) u_moment
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'U_QUADRATURE_RULE_TEST:'
  write ( *, '(a)' ) '  U_QUADRATURE_RULE computes the quadrature rule'
  write ( *, '(a)' ) '  associated with U(n,x);'

  n = 7
  allocate ( x(1:n) )
  allocate ( w(1:n) )

  call u_quadrature_rule ( n, x, w )

  call r8vec2_print ( n, x, w, '      X            W' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Q = Integral ( -1 <= X <= +1 ) X^E * sqrt ( 1-x^2) dx'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   E       Q_Estimate      Q_Exact'
  write ( *, '(a)' ) ' '

  allocate ( f(1:n) )

  do e = 0, 2 * n - 1
    if ( e == 0 ) then
      f(1:n) = 1.0D+00
    else
      f(1:n) = x(1:n) ** e
    end if
    q = dot_product ( w, f )
    q_exact = u_moment ( e )
    write ( *, '(2x,i2,g14.6,2x,g14.6)' ) e, q, q_exact
  end do

  deallocate ( f )
  deallocate ( w )
  deallocate ( x )

  return
end
subroutine uu_product_test ( )

!*****************************************************************************80
!
!! UU_PRODUCT_TEST tests UU_PRODUCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ) u_polynomial_value
  real ( kind = 8 ) ui
  real ( kind = 8 ) uiuj
  real ( kind = 8 ) uj
  real ( kind = 8 ) uu_product
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UU_PRODUCT_TEST:'
  write ( *, '(a)' ) '  UU_PRODUCT(I,J;X) = U(I,X) * U(J,X)'

  r8_lo = -1.0D+00
  r8_hi = +1.0D+00
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I   J      X               UI              UJ              UI*UJ       UU_PRODUCT' 
  write ( *, '(a)' ) '' 
  do test = 1, 10
    x = r8_uniform_ab ( r8_lo, r8_hi, seed )
    i = i4_uniform_ab ( 0, 6, seed )
    ui = u_polynomial_value ( i, x )
    j = i4_uniform_ab ( -1, 4, seed )
    uj = u_polynomial_value ( j, x )
    uiuj = uu_product ( i, j, x )
    write ( *, '(2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, j, x, ui, uj, ui * uj, uiuj
  end do

  return
end
subroutine uu_product_integral_test ( )

!*****************************************************************************80
!
!! UU_PRODUCT_INTEGRAL_TEST tests UU_PRODUCT_INTEGRAL.
!
!  Discussion:
!
!    This process should match the UU_MASS_MATRIX computation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) uu_product_integral

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UU_PRODUCT_INTEGRAL_TEST:'
  write ( *, '(a)' ) '  UU_PRODUCT_INTEGRAL computes the product integral'
  write ( *, '(a)' ) '  of a pair of Chebyshev U polynomials U(i,x) and U(j,x).'
  write ( *, '(a)' ) &
    '  A(I,J) = integral ( -1 <=x <= +1 ) U(i,x) U(j,x) sqrt ( 1 - x^2 ) dx'
  write ( *, '(a)' ) '  0    if i is not equal to j;'
  write ( *, '(a)' ) '  pi/2 if i = j'

  n = 4
  allocate ( a(0:n,0:n) )
  do i = 0, n
    do j = 0, n
      a(i,j) = uu_product_integral ( i, j )
    end do
  end do

  call r8mat_print ( n + 1, n + 1, a, '  U(i,x)*U(j,x) integral matrix:' )

  return
end
subroutine v_mass_matrix_test ( )

!*****************************************************************************80
!
!! V_MASS_MATRIX_TEST tests V_MASS_MATRIX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'V_MASS_MATRIX_TEST:'
  write ( *, '(a)' ) '  V_MASS_MATRIX computes the mass matrix for the'
  write ( *, '(a)' ) '  Chebyshev polynomials V(i,x).'
  write ( *, '(a)' ) &
    '  A(I,J) = integral ( -1 <=x <= +1 ) V(i,x) V(j,x) sqrt(1+x)/sqrt (1-x) dx'
  write ( *, '(a)' ) '  0  if i is not equal to j;'
  write ( *, '(a)' ) '  pi if i = j.'

  n = 3
  allocate ( a(0:n,0:n) )

  call v_mass_matrix ( n, a )

  call r8mat_print ( n + 1, n + 1, a, '  V mass matrix:' )

  return
end
subroutine v_moment_test ( )

!*****************************************************************************80
!
!! V_MOMENT_TEST tests V_MOMENT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e
  real ( kind = 8 ) v_moment
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'V_MOMENT_TEST:'
  write ( *, '(a)' ) '  V_MOMENT returns the value of'
  write ( *, '(a)' ) '  integral ( -1 <=x <= +1 ) x^e * sqrt(1+x) / sqrt(1-x) dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   E       Integral'
  write ( *, '(a)' ) ''
  do e = 0, 10
    value = v_moment ( e )
    write ( *, '(2x,i2,2x,g14.6)' ) e, value
  end do

  return
end
subroutine v_polynomial_test ( )

!*****************************************************************************80
!
!! V_POLYNOMIAL_TEST tests V_POLYNOMIAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 April 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ), allocatable :: fx2(:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'V_POLYNOMIAL_TEST:'
  write ( *, '(a)' ) '  V_POLYNOMIAL evaluates Chebyshev polynomials V(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        V(n,x)        V(n,x)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call v_polynomial_values ( n_data, n, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    if ( n < 0 ) then
      cycle
    end if

    allocate ( fx2(0:n) )

    call v_polynomial ( 1, n, x, fx2 )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

    deallocate ( fx2 )

  end do

  return
end
subroutine v_polynomial_ab_test ( )

!*****************************************************************************80
!
!! V_POLYNOMIAL_AB_TEST tests V_POLYNOMIAL_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 11
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) v(m,0:n)
  real ( kind = 8 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'V_POLYNOMIAL_AB_TEST:'
  write ( *, '(a)' ) '  V_POLYNOMIAL_AB evaluates Chebyshev polynomials VAB(n,x)'
  write ( *, '(a)' ) '  shifted from [-1,+1] to the domain [A,B].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here, we will use the new domain [0,1]'
  write ( *, '(a)' ) '  and the desired maximum polynomial degree will be N = 5.'

  a = 0.0D+00
  b = 1.0D+00
  call r8vec_linspace ( m, a, b, x )
  
  call v_polynomial_ab ( a, b, m, n, x, v )

  call r8mat_print ( m, n + 1, v, '  Tables of T values:' )

  return
end
subroutine v_polynomial_ab_value_test ( )

!*****************************************************************************80
!
!! V_POLYNOMIAL_AB_VALUE_TEST tests V_POLYNOMIAL_AB_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) v_polynomial_ab_value
  real ( kind = 8 ) x01

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'V_POLYNOMIAL_AB_VALUE_TEST:'
  write ( *, '(a)' ) '  V_POLYNOMIAL_AB_VALUE evaluates Chebyshev polynomials VAB(n,x)'
  write ( *, '(a)' ) '  shifted from [-1,+1] to the domain [A,B].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here, we will use the new domain [0,1].'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        V01(n,x)      V01(n,x)'
  write ( *, '(a)' ) ' '

  a = 0.0D+00
  b = 1.0D+00

  n_data = 0

  do

    call v_polynomial_01_values ( n_data, n, x01, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = v_polynomial_ab_value ( a, b, n, x01 )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x01, fx, fx2

  end do

  return
end
subroutine v_polynomial_coefficients_test ( )

!*****************************************************************************80
!
!! V_POLYNOMIAL_COEFFICIENTS_TEST tests V_POLYNOMIAL_COEFFICIENTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) c(0:n,0:n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'V_POLYNOMIAL_COEFFICIENTS_TEST'
  write ( *, '(a)' ) '  V_POLYNOMIAL_COEFFICIENTS determines coefficients for'
  write ( *, '(a)' ) '  Chebyshev polynomials V(n,x).'

  call v_polynomial_coefficients ( n, c )
 
  do i = 0, n
    call r8poly_print ( i, c(i,0:i), '' );
  end do

  return
end
subroutine v_polynomial_plot_test ( )

!*****************************************************************************80
!
!! V_POLYNOMIAL_PLOT_TEST tests V_POLYNOMIAL_PLOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_num = 6

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n_val(n_num)
  character ( len = 255 ) output_filename

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'V_POLYNOMIAL_PLOT_TEST'
  write ( *, '(a)' ) '  V_POLYNOMIAL_PLOT plots selected '
  write ( *, '(a)' ) '  Chebyshev polynomials V(n,x).'

  do i = 0, 5
    n_val(i+1) = i
  end do

  output_filename = 'v_polynomial_plot.png'

  call v_polynomial_plot( n_num, n_val, output_filename )

  return
end
subroutine v_polynomial_value_test ( )

!*****************************************************************************80
!
!! V_POLYNOMIAL_VALUE_TEST tests V_POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) v_polynomial_value
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'V_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  V_POLYNOMIAL_VALUE evaluates Chebyshev polynomials V(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        V(n,x)        V(n,x)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call v_polynomial_values ( n_data, n, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = v_polynomial_value ( n, x )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2

  end do

  return
end
subroutine v_polynomial_zeros_test ( )

!*****************************************************************************80
!
!! V_POLYNOMIAL_ZEROS_TEST tests V_POLYNOMIAL_ZEROS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: fx(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: n_max = 5
  real ( kind = 8 ), allocatable :: z(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'V_POLYNOMIAL_ZEROS_TEST:'
  write ( *, '(a)' ) '  V_POLYNOMIAL_ZEROS returns zeroes of V(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       N      X        V(n,x)'

  do n = 1, n_max

    allocate ( z(1:n) )

    call v_polynomial_zeros ( n, z )

    allocate ( fx(1:n,0:n) )

    call v_polynomial ( n, n, z, fx )

    write ( *, '(a)' ) ' '
    do i = 1, n
      write ( *, '(2x,i8,2x,f8.4,2x,g14.6)' ) n, z(i), fx(i,n)
    end do

    deallocate ( fx )
    deallocate ( z )

  end do

  return
end
subroutine v_quadrature_rule_test ( )

!*****************************************************************************80
!
!! V_QUADRATURE_RULE_TEST tests V_QUADRATURE_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e
  real ( kind = 8 ), allocatable :: f(:)
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ) q_exact
  real ( kind = 8 ) v_moment
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'V_QUADRATURE_RULE_TEST:'
  write ( *, '(a)' ) '  V_QUADRATURE_RULE computes the quadrature rule'
  write ( *, '(a)' ) '  associated with V(n,x);'

  n = 7
  allocate ( x(1:n) )
  allocate ( w(1:n) )

  call v_quadrature_rule ( n, x, w )

  call r8vec2_print ( n, x, w, '      X            W' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Q = Integral ( -1 <= X <= +1 ) X^E * sqrt (1+x)/sqrt(1-x) dx'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   E       Q_Estimate      Q_Exact'
  write ( *, '(a)' ) ' '

  allocate ( f(1:n) )

  do e = 0, 2 * n - 1
    if ( e == 0 ) then
      f(1:n) = 1.0D+00
    else
      f(1:n) = x(1:n) ** e
    end if
    q = dot_product ( w, f )
    q_exact = v_moment ( e )
    write ( *, '(2x,i2,g14.6,2x,g14.6)' ) e, q, q_exact
  end do

  deallocate ( f )
  deallocate ( w )
  deallocate ( x )

  return
end
subroutine vv_product_integral_test ( )

!*****************************************************************************80
!
!! VV_PRODUCT_INTEGRAL_TEST tests VV_PRODUCT_INTEGRAL.
!
!  Discussion:
!
!    This process should match the VV_MASS_MATRIX computation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) vv_product_integral

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VV_PRODUCT_INTEGRAL_TEST:'
  write ( *, '(a)' ) '  VV_PRODUCT_INTEGRAL computes the product integral'
  write ( *, '(a)' ) '  of a pair of Chebyshev V polynomials V(i,x) and V(j,x).'
  write ( *, '(a)' ) &
    '  A(I,J) = integral ( -1 <=x <= +1 ) V(i,x) V(j,x) sqrt ( 1 + x ) / sqrt ( 1 - x ) dx'
  write ( *, '(a)' ) '  0  if i is not equal to j;'
  write ( *, '(a)' ) '  pi if i = j'

  n = 4
  allocate ( a(0:n,0:n) )
  do i = 0, n
    do j = 0, n
      a(i,j) = vv_product_integral ( i, j )
    end do
  end do

  call r8mat_print ( n + 1, n + 1, a, '  V(i,x)*V(j,x) integral matrix:' )

  return
end
subroutine w_mass_matrix_test ( )

!*****************************************************************************80
!
!! W_MASS_MATRIX_TEST tests W_MASS_MATRIX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'W_MASS_MATRIX_TEST:'
  write ( *, '(a)' ) '  W_MASS_MATRIX computes the mass matrix for the'
  write ( *, '(a)' ) '  Chebyshev polynomials W(i,x).'
  write ( *, '(a)' ) &
    '  A(I,J) = integral ( -1 <=x <= +1 ) W(i,x) W(j,x) sqrt(1-x)/sqrt(1+x) dx'
  write ( *, '(a)' ) '  0  if i is not equal to j;'
  write ( *, '(a)' ) '  pi if i = j.'

  n = 3
  allocate ( a(0:n,0:n) )

  call w_mass_matrix ( n, a )

  call r8mat_print ( n + 1, n + 1, a, '  W mass matrix:' )

  return
end
subroutine w_moment_test ( )

!*****************************************************************************80
!
!! W_MOMENT_TEST tests W_MOMENT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e
  real ( kind = 8 ) value
  real ( kind = 8 ) w_moment

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'W_MOMENT_TEST:'
  write ( *, '(a)' ) '  W_MOMENT returns the value of'
  write ( *, '(a)' ) '  integral ( -1 <=x <= +1 ) x^e * sqrt(1-x) / sqrt(1+x) dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   E       Integral'
  write ( *, '(a)' ) ''
  do e = 0, 10
    value = w_moment ( e )
    write ( *, '(2x,i2,2x,g14.6)' ) e, value
  end do

  return
end
subroutine w_polynomial_test ( )

!*****************************************************************************80
!
!! W_POLYNOMIAL_TEST tests W_POLYNOMIAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 April 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ), allocatable :: fx2(:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'W_POLYNOMIAL_TEST:'
  write ( *, '(a)' ) '  W_POLYNOMIAL evaluates Chebyshev polynomials W(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        W(n,x)        W(n,x)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call w_polynomial_values ( n_data, n, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    if ( n < 0 ) then
      cycle
    end if

    allocate ( fx2(0:n) )

    call w_polynomial ( 1, n, x, fx2 )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

    deallocate ( fx2 )

  end do

  return
end
subroutine w_polynomial_ab_test ( )

!*****************************************************************************80
!
!! W_POLYNOMIAL_AB_TEST tests W_POLYNOMIAL_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 11
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) v(m,0:n)
  real ( kind = 8 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'W_POLYNOMIAL_AB_TEST:'
  write ( *, '(a)' ) '  W_POLYNOMIAL_AB evaluates Chebyshev polynomials WAB(n,x)'
  write ( *, '(a)' ) '  shifted from [-1,+1] to the domain [A,B].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here, we will use the new domain [0,1]'
  write ( *, '(a)' ) '  and the desired maximum polynomial degree will be N = 5.'

  a = 0.0D+00
  b = 1.0D+00
  call r8vec_linspace ( m, a, b, x )
  
  call w_polynomial_ab ( a, b, m, n, x, v )

  call r8mat_print ( m, n + 1, v, '  Tables of T values:' )

  return
end
subroutine w_polynomial_ab_value_test ( )

!*****************************************************************************80
!
!! W_POLYNOMIAL_AB_VALUE_TEST tests W_POLYNOMIAL_AB_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) w_polynomial_ab_value
  real ( kind = 8 ) x01

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'W_POLYNOMIAL_AB_VALUE_TEST:'
  write ( *, '(a)' ) '  W_POLYNOMIAL_AB_VALUE evaluates Chebyshev polynomials WAB(n,x)'
  write ( *, '(a)' ) '  shifted from [-1,+1] to the domain [A,B].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here, we will use the new domain [0,1].'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        W01(n,x)      W01(n,x)'
  write ( *, '(a)' ) ' '

  a = 0.0D+00
  b = 1.0D+00

  n_data = 0

  do

    call w_polynomial_01_values ( n_data, n, x01, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = w_polynomial_ab_value ( a, b, n, x01 )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x01, fx, fx2

  end do

  return
end
subroutine w_polynomial_coefficients_test ( )

!*****************************************************************************80
!
!! W_POLYNOMIAL_COEFFICIENTS_TEST tests W_POLYNOMIAL_COEFFICIENTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) c(0:n,0:n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'W_POLYNOMIAL_COEFFICIENTS_TEST'
  write ( *, '(a)' ) '  W_POLYNOMIAL_COEFFICIENTS determines coefficients for'
  write ( *, '(a)' ) '  Chebyshev polynomials W(n,x).'

  call w_polynomial_coefficients ( n, c )
 
  do i = 0, n
    call r8poly_print ( i, c(i,0:i), '' );
  end do

  return
end
subroutine w_polynomial_plot_test ( )

!*****************************************************************************80
!
!! W_POLYNOMIAL_PLOT_TEST tests W_POLYNOMIAL_PLOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_num = 6

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n_val(n_num)
  character ( len = 255 ) output_filename

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'W_POLYNOMIAL_PLOT_TEST'
  write ( *, '(a)' ) '  W_POLYNOMIAL_PLOT plots selected '
  write ( *, '(a)' ) '  Chebyshev polynomials W(n,x).'

  do i = 0, 5
    n_val(i+1) = i
  end do

  output_filename = 'w_polynomial_plot.png'

  call w_polynomial_plot( n_num, n_val, output_filename )

  return
end
subroutine w_polynomial_value_test ( )

!*****************************************************************************80
!
!! W_POLYNOMIAL_VALUE_TEST tests W_POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) w_polynomial_value
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'W_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  W_POLYNOMIAL_VALUE evaluates Chebyshev polynomials W(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                    Tabulated     Computed'
  write ( *, '(a)' ) '     N      X        W(n,x)        W(n,x)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call w_polynomial_values ( n_data, n, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = w_polynomial_value ( n, x )

    write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2

  end do

  return
end
subroutine w_polynomial_zeros_test ( )

!*****************************************************************************80
!
!! W_POLYNOMIAL_ZEROS_TEST tests W_POLYNOMIAL_ZEROS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: fx(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: n_max = 5
  real ( kind = 8 ), allocatable :: z(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'W_POLYNOMIAL_ZEROS_TEST:'
  write ( *, '(a)' ) '  W_POLYNOMIAL_ZEROS returns zeroes of W(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       N      X        W(n,x)'

  do n = 1, n_max

    allocate ( z(1:n) )

    call w_polynomial_zeros ( n, z )

    allocate ( fx(1:n,0:n) )

    call w_polynomial ( n, n, z, fx )

    write ( *, '(a)' ) ' '
    do i = 1, n
      write ( *, '(2x,i8,2x,f8.4,2x,g14.6)' ) n, z(i), fx(i,n)
    end do

    deallocate ( fx )
    deallocate ( z )

  end do

  return
end
subroutine w_quadrature_rule_test ( )

!*****************************************************************************80
!
!! W_QUADRATURE_RULE_TEST tests W_QUADRATURE_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e
  real ( kind = 8 ), allocatable :: f(:)
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ) q_exact
  real ( kind = 8 ) w_moment
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'W_QUADRATURE_RULE_TEST:'
  write ( *, '(a)' ) '  W_QUADRATURE_RULE computes the quadrature rule'
  write ( *, '(a)' ) '  associated with W(n,x);'

  n = 7
  allocate ( x(1:n) )
  allocate ( w(1:n) )

  call w_quadrature_rule ( n, x, w )

  call r8vec2_print ( n, x, w, '      X            W' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Q = Integral ( -1 <= X <= +1 ) X^E * sqrt (1-x)/sqrt(1+x) dx'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   E       Q_Estimate      Q_Exact'
  write ( *, '(a)' ) ' '

  allocate ( f(1:n) )

  do e = 0, 2 * n - 1
    if ( e == 0 ) then
      f(1:n) = 1.0D+00
    else
      f(1:n) = x(1:n) ** e
    end if
    q = dot_product ( w, f )
    q_exact = w_moment ( e )
    write ( *, '(2x,i2,g14.6,2x,g14.6)' ) e, q, q_exact
  end do

  deallocate ( f )
  deallocate ( w )
  deallocate ( x )

  return
end
subroutine ww_product_integral_test ( )

!*****************************************************************************80
!
!! WW_PRODUCT_INTEGRAL_TEST tests WW_PRODUCT_INTEGRAL.
!
!  Discussion:
!
!    This process should match the WW_MASS_MATRIX computation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) ww_product_integral

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WW_PRODUCT_INTEGRAL_TEST:'
  write ( *, '(a)' ) '  WW_PRODUCT_INTEGRAL computes the product integral'
  write ( *, '(a)' ) '  of a pair of Chebyshev W polynomials W(i,x) and W(j,x).'
  write ( *, '(a)' ) &
    '  A(I,J) = integral ( -1 <=x <= +1 ) W(i,x) W(j,x) sqrt ( 1 - x ) / sqrt ( 1 + x ) dx'
  write ( *, '(a)' ) '  0  if i is not equal to j;'
  write ( *, '(a)' ) '  pi if i = j'

  n = 4
  allocate ( a(0:n,0:n) )
  do i = 0, n
    do j = 0, n
      a(i,j) = ww_product_integral ( i, j )
    end do
  end do

  call r8mat_print ( n + 1, n + 1, a, '  W(i,x)*W(j,x) integral matrix:' )

  return
end
