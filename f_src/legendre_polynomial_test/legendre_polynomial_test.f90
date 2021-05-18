program main

!*****************************************************************************80
!
!! MAIN is the main program for LEGENDRE_POLYNOMIAL_TEST.
!
!  Discussion:
!
!    LEGENDRE_POLYNOMIAL_TEST tests the LEGENDRE_POLYNOMIAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) b
  integer ( kind = 4 ) e
  integer ( kind = 4 ) p

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the LEGENDRE_POLYNOMIAL library.'

  p = 5
  b = 0.0D+00
  call p_exponential_product_test ( p, b )

  p = 5
  b = 1.0D+00
  call p_exponential_product_test ( p, b )

  call p_integral_test ( )

  call p_polynomial_coefficients_test ( )
  call p_polynomial_prime_test ( )
  call p_polynomial_prime2_test ( )
  call p_polynomial_value_test ( )
  call p_polynomial_zeros_test ( )

  p = 5
  e = 0
  call p_power_product_test ( p, e )

  p = 5
  e = 1
  call p_power_product_test ( p, e )

  call p_quadrature_rule_test ( )

  call pm_polynomial_value_test ( )

  call pmn_polynomial_value_test ( )

  call pmns_polynomial_value_test ( )

  p = 5
  call pn_pair_product_test ( p )
  call pn_polynomial_coefficients_test ( )
  call pn_polynomial_value_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine p_exponential_product_test ( p, b )

!*****************************************************************************80
!
!! P_EXPONENTIAL_PRODUCT_TEST tests P_EXPONENTIAL_PRODUCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) P, the maximum degree of the polynomial 
!    factors.
!
!    Input, real ( kind = 8 ) B, the coefficient of X in the exponential factor.
!
  implicit none

  real ( kind = 8 ) b
  integer ( kind = 4 ) p
  real ( kind = 8 ), allocatable :: table(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'P_EXPONENTIAL_PRODUCT_TEST'
  write ( *, '(a)' ) '  P_EXPONENTIAL_PRODUCT_TEST computes an exponential product table for P(n,x):'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Tij = integral ( -1 <= x <= +1 ) exp(b*x) P(i,x) P(j,x) dx'

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Maximum degree P = ', p
  write ( *, '(a,g14.6)' ) '  Exponential argument coefficient B = ', b

  allocate ( table(0:p,0:p) )

  call p_exponential_product ( p, b, table )

  call r8mat_print ( p + 1, p + 1, table, '  Exponential product table:' )

  deallocate ( table )

  return
end
subroutine p_integral_test ( )

!*****************************************************************************80
!
!! P_INTEGRAL_TEST tests P_INTEGRAL.
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

  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'P_INTEGRAL_TEST:'
  write ( *, '(a)' ) '  P_INTEGRAL returns the integral of P(n,x) over [-1,+1].'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     N        Integral'
  write ( *, '(a)' ) ' '

  do n = 0, 10

    call p_integral ( n, value )

    write ( *, '(2x,i4,2x,g14.6)' ) n, value

  end do

  return
end
subroutine p_polynomial_coefficients_test ( )

!*****************************************************************************80
!
!! P_POLYNOMIAL_COEFFICIENTS_TEST tests P_POLYNOMIAL_COEFFICIENTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 March 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) c(0:n,0:n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'P_POLYNOMIAL_COEFFICIENTS_TEST'
  write ( *, '(a)' ) &
    '  P_POLYNOMIAL_COEFFICIENTS determines polynomial coefficients of P(n,x).'

  call p_polynomial_coefficients ( n, c )
 
  do i = 0, n
    write ( *, '(a)' ) ' '
    write ( *, '(a,i2,a)' ) '  P(', i, ',x) = '
    write ( *, '(a)' ) ' '
    do j = i, 0, -1
      if ( c(i,j) == 0.0D+00 ) then

      else if ( j == 0 ) then
        write ( *, '(2x,g14.6)' ) c(i,j)
      else if ( j == 1 ) then
        write ( *, '(2x,g14.6,a)' ) c(i,j), ' * x'
      else
        write ( *, '(2x,g14.6,a,i2)' ) c(i,j), ' * x^', j
      end if
    end do
  end do
 
  return
end
subroutine p_polynomial_prime_test ( )

!*****************************************************************************80
!
!! P_POLYNOMIAL_PRIME_TEST tests P_POLYNOMIAL_PRIME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: vp(:,:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'P_POLYNOMIAL_PRIME_TEST:'
  write ( *, '(a)' ) '  P_POLYNOMIAL_PRIME evaluates the derivative of'
  write ( *, '(a)' ) '  the Legendre polynomial.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                        Computed'
  write ( *, '(a)' ) '     N        X           P''(N,X)'
  write ( *, '(a)' ) ' '

  m = 11
  n = 5

  allocate ( x(1:m) )

  a = -1.0D+00
  b = +1.0D+00
  call r8vec_linspace ( m, a, b, x )

  allocate ( vp(1:m,0:n) )
  call p_polynomial_prime ( m, n, x, vp )

  do i = 1, m
    write ( *, '(a)' ) ' '
    do j = 0, n
      write ( *, '(2x,i4,2x,f12.6,2x,g24.16)' ) j, x(i), vp(i,j)
    end do

  end do

  deallocate ( vp )
  deallocate ( x )

  return
end
subroutine p_polynomial_prime2_test ( )

!*****************************************************************************80
!
!! P_POLYNOMIAL_PRIME2_TEST tests P_POLYNOMIAL_PRIME2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: vpp(:,:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'P_POLYNOMIAL_PRIME2_TEST:'
  write ( *, '(a)' ) '  P_POLYNOMIAL_PRIME2 evaluates the second derivative of'
  write ( *, '(a)' ) '  the Legendre polynomial.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                        Computed'
  write ( *, '(a)' ) '     N        X           P"(N,X)'
  write ( *, '(a)' ) ' '

  m = 11
  n = 5

  allocate ( x(1:m) )

  a = -1.0D+00
  b = +1.0D+00
  call r8vec_linspace ( m, a, b, x )

  allocate ( vpp(1:m,0:n) )
  call p_polynomial_prime2 ( m, n, x, vpp )

  do i = 1, m
    write ( *, '(a)' ) ' '
    do j = 0, n
      write ( *, '(2x,i4,2x,f12.6,2x,g24.16)' ) j, x(i), vpp(i,j)
    end do

  end do

  deallocate ( vpp )
  deallocate ( x )

  return
end
subroutine p_polynomial_value_test ( )

!*****************************************************************************80
!
!! P_POLYNOMIAL_VALUE_TEST tests P_POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 March 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  real ( kind = 8 ) e
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ), parameter :: m = 1
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: v(:,:)
  real ( kind = 8 ) x
  real ( kind = 8 ) x_vec(1)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'P_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  P_POLYNOMIAL_VALUE evaluates the Legendre polynomial P(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                        Tabulated                 Computed'
  write ( *, '(a)' ) '     N        X           P(N,X)                    P(N,X)                     Error'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call p_polynomial_values ( n_data, n, x, fx1 )

    x_vec(1) = x

    if ( n_data == 0 ) then
      exit
    end if

    allocate ( v(m,n+1) )
    call p_polynomial_value ( m, n, x_vec, v )
    fx2 = v(1,n+1)
    deallocate ( v )

    e = fx1 - fx2

    write ( *, '(2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) &
      n, x, fx1, fx2, e

  end do

  return
end
subroutine p_polynomial_zeros_test ( )

!*****************************************************************************80
!
!! P_POLYNOMIAL_ZEROS_TEST tests P_POLYNOMIAL_ZEROS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) degree
  real ( kind = 8 ), allocatable :: lz(:,:)
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable :: z(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'P_POLYNOMIAL_ZEROS_TEST:'
  write ( *, '(a)' ) '  P_POLYNOMIAL_ZEROS computes the zeros of P(n,x)'
  write ( *, '(a)' ) '  Check by calling P_POLYNOMIAL_VALUE.'

  do degree = 1, 5

    allocate ( z(1:degree) )
    call p_polynomial_zeros ( degree, z )
    write ( title, '(a,i1,a)' ) '  Computed zeros for P(', degree, ',z):'
    call r8vec_print ( degree, z, title )

    allocate ( lz(degree,0:degree) )
    call p_polynomial_value ( degree, degree, z, lz )
    write ( title, '(a,i1,a)' ) '  Evaluate P(', degree, ',z):'
    call r8vec_print ( degree, lz(1:degree,degree), title )

    deallocate ( lz )
    deallocate ( z )

  end do

  return
end
subroutine p_quadrature_rule_test ( )

!*****************************************************************************80
!
!! P_QUADRATURE_RULE_TEST tests P_QUADRATURE_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2012
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
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'P_QUADRATURE_RULE_TEST:'
  write ( *, '(a)' ) '  P_QUADRATURE_RULE computes the quadrature rule'
  write ( *, '(a)' ) '  associated with P(n,x)'

  n = 5
  allocate ( x(1:n) )
  allocate ( w(1:n) )

  call p_quadrature_rule ( n, x, w )

  call r8vec2_print ( n, x, w,  '      X            W' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Q = Integral ( -1 <= X < +1 ) X^E dx'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   E       Q_Estimate      Q_Exact'
  write ( *, '(a)' ) ' '

  allocate ( f(1:n) )

  do e = 0, 2 * n - 1
    if ( e == 0 ) then
      f(1:n) = 1.0D+00
    else
      f(1:n) = x(1:n)**e
    end if
    q = dot_product ( w(1:n), f(1:n) )
    call p_integral ( e, q_exact )
    write ( *, '(2x,i2,2x,g14.6,2x,g14.6)' ) e, q, q_exact
  end do

  deallocate ( f )
  deallocate ( w )
  deallocate ( x )

  return
end
subroutine p_power_product_test ( p, e )

!*****************************************************************************80
!
!! P_POWER_PRODUCT_TEST tests P_POWER_PRODUCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) P, the maximum degree of the polynomial 
!    factors.
!
!    Input, integer ( kind = 4 ) E, the exponent of X.
!
  implicit none

  integer ( kind = 4 ) e
  integer ( kind = 4 ) p
  real ( kind = 8 ), allocatable :: table(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'P_POWER_PRODUCT_TEST'
  write ( *, '(a)' ) '  P_POWER_PRODUCT_TEST computes a power product table for P(n,x):'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Tij = integral ( -1 <= x <= +1 ) x^e P(i,x) P(j,x) dx'

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Maximum degree P = ', p
  write ( *, '(a,g14.6)' ) '  Exponent of X, E = ', e

  allocate ( table(0:p,0:p) )

  call p_power_product ( p, e, table )

  call r8mat_print ( p + 1, p + 1, table, '  Power product table:' )

  deallocate ( table )

  return
end
subroutine pm_polynomial_value_test ( )

!*****************************************************************************80
!
!! PM_POLYNOMIAL_VALUE_TEST tests PM_POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 March 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  real ( kind = 8 ) e
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) m
  integer ( kind = 4 ), parameter :: mm = 1
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: v(:,:)
  real ( kind = 8 ) x
  real ( kind = 8 ) x_vec(1)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PM_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  PM_POLYNOMIAL_VALUE evaluates the Legendre polynomial Pm(n,m,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                                Tabulated                 Computed'
  write ( *, '(a)' ) '     N     M        X           Pm(N,M,X)                 Pm(N,M,X)             Error'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call pm_polynomial_values ( n_data, n, m, x, fx1 )

    x_vec(1) = x

    if ( n_data == 0 ) then
      exit
    end if

    allocate ( v(mm,n+1) )
    call pm_polynomial_value ( mm, n, m, x_vec, v )
    fx2 = v(1,n+1)
    deallocate ( v )

    e = fx1 - fx2

    write ( *, '(2x,i4,2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) &
      n, m, x, fx1, fx2, e

  end do

  return
end
subroutine pmn_polynomial_value_test ( )

!*****************************************************************************80
!
!! PMN_POLYNOMIAL_VALUE_TEST tests PMN_POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 March 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  real ( kind = 8 ) e
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) m
  integer ( kind = 4 ), parameter :: mm = 1
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: v(:,:)
  real ( kind = 8 ) x
  real ( kind = 8 ) x_vec(1)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PMN_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  PMN_POLYNOMIAL_VALUE evaluates the Legendre polynomial Pmn(n,m,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                                Tabulated                 Computed'
  write ( *, '(a)' ) '     N     M        X           Pmn(N,M,X)                Pmn(N,M,X)             Error'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call pmn_polynomial_values ( n_data, n, m, x, fx1 )

    x_vec(1) = x

    if ( n_data == 0 ) then
      exit
    end if

    allocate ( v(mm,n+1) )
    call pmn_polynomial_value ( mm, n, m, x_vec, v )
    fx2 = v(1,n+1)
    deallocate ( v )

    e = fx1 - fx2

    write ( *, '(2x,i4,2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) &
      n, m, x, fx1, fx2, e

  end do

  return
end
subroutine pmns_polynomial_value_test ( )

!*****************************************************************************80
!
!! PMNS_POLYNOMIAL_VALUE_TEST tests PMNS_POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 March 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  real ( kind = 8 ) e
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) m
  integer ( kind = 4 ), parameter :: mm = 1
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: v(:,:)
  real ( kind = 8 ) x
  real ( kind = 8 ) x_vec(1)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PMNS_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  PMNS_POLYNOMIAL_VALUE evaluates the Legendre polynomial Pmns(n,m,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                                Tabulated                 Computed'
  write ( *, '(a)' ) '     N     M        X           Pmns(N,M,X)                Pmns(N,M,X)             Error'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call pmns_polynomial_values ( n_data, n, m, x, fx1 )

    x_vec(1) = x

    if ( n_data == 0 ) then
      exit
    end if

    allocate ( v(mm,n+1) )
    call pmns_polynomial_value ( mm, n, m, x_vec, v )
    fx2 = v(1,n+1)
    deallocate ( v )

    e = fx1 - fx2

    write ( *, '(2x,i4,2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) &
      n, m, x, fx1, fx2, e

  end do

  return
end
subroutine pn_pair_product_test ( p )

!*****************************************************************************80
!
!! PN_PAIR_PRODUCT_TEST tests PN_PAIR_PRODUCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) P, the maximum degree of the polynomial 
!    factors.
!
  implicit none

  integer ( kind = 4 ) p
  real ( kind = 8 ), allocatable :: table(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PN_PAIR_PRODUCT_TEST'
  write ( *, '(a)' ) '  PN_PAIR_PRODUCT_TEST computes a pair product table for Pn(n,x):'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Tij = integral ( -1 <= x <= +1 ) Pn(i,x) Pn(j,x) dx'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The Pn(n,x) polynomials are orthonormal,'
  write ( *, '(a)' ) '  so T should be the identity matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Maximum degree P = ', p

  allocate ( table(0:p,0:p) )

  call pn_pair_product ( p, table )

  call r8mat_print ( p + 1, p + 1, table, '  Pair product table:' )

  deallocate ( table )

  return
end
subroutine pn_polynomial_coefficients_test ( )

!*****************************************************************************80
!
!! PN_POLYNOMIAL_COEFFICIENTS_TEST tests PN_POLYNOMIAL_COEFFICIENTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) c(0:n,0:n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PN_POLYNOMIAL_COEFFICIENTS_TEST'
  write ( *, '(a)' ) &
    '  PN_POLYNOMIAL_COEFFICIENTS determines polynomial coefficients of Pn(n,x).'

  call pn_polynomial_coefficients ( n, c )
 
  do i = 0, n
    write ( *, '(a)' ) ' '
    write ( *, '(a,i2,a)' ) '  P(', i, ',x) = '
    write ( *, '(a)' ) ' '
    do j = i, 0, -1
      if ( c(i,j) == 0.0D+00 ) then

      else if ( j == 0 ) then
        write ( *, '(2x,g14.6)' ) c(i,j)
      else if ( j == 1 ) then
        write ( *, '(2x,g14.6,a)' ) c(i,j), ' * x'
      else
        write ( *, '(2x,g14.6,a,i2)' ) c(i,j), ' * x^', j
      end if
    end do
  end do
 
  return
end
subroutine pn_polynomial_value_test ( )

!*****************************************************************************80
!
!! PN_POLYNOMIAL_VALUE_TEST tests PN_POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  real ( kind = 8 ) e
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ), parameter :: m = 1
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: v(:,:)
  real ( kind = 8 ) x
  real ( kind = 8 ) x_vec(1)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PN_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  PN_POLYNOMIAL_VALUE evaluates the normalized Legendre polynomial Pn(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                        Tabulated                 Computed'
  write ( *, '(a)' ) '     N        X          Pn(N,X)                   Pn(N,X)                     Error'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call pn_polynomial_values ( n_data, n, x, fx1 )

    x_vec(1) = x

    if ( n_data == 0 ) then
      exit
    end if

    allocate ( v(m,n+1) )
    call pn_polynomial_value ( m, n, x_vec, v )
    fx2 = v(1,n+1)
    deallocate ( v )

    e = fx1 - fx2

    write ( *, '(2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) &
      n, x, fx1, fx2, e

  end do

  return
end

