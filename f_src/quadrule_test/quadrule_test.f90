program main

!*****************************************************************************80
!
!! MAIN is the main program for QUADRULE_TEST.
!
!  Discussion:
!
!    QUADRULE_TEST tests the QUADRULE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUADRULE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the QUADRULE library.'

  call chebyshev_set_test ( )
  call chebyshev1_compute_test ( )
  call chebyshev1_integral_test ( )
  call chebyshev1_set_test ( )
  call chebyshev2_compute_test ( )
  call chebyshev2_compute_test2 ( )
  call chebyshev2_integral_test ( )
  call chebyshev2_set_test ( )
  call chebyshev3_compute_test ( )
  call chebyshev3_integral_test ( )
  call chebyshev3_set_test ( )
  call clenshaw_curtis_compute_test ( )
  call clenshaw_curtis_set_test ( )
  call fejer1_compute_test ( )
  call fejer1_set_test ( )
  call fejer2_compute_test ( )
  call fejer2_set_test ( )
  call gegenbauer_integral_test ( )
  call gegenbauer_ek_compute_test ( )
  call gegenbauer_ss_compute_test ( )
  call gen_hermite_ek_compute_test ( )
  call gen_hermite_integral_test ( )
  call gen_laguerre_ek_compute_test ( )
  call gen_laguerre_integral_test ( )
  call gen_laguerre_ss_compute_test ( )
  call hermite_ek_compute_test ( )
  call hermite_integral_test ( )
  call hermite_set_test ( )
  call hermite_ss_compute_test ( )
  call hermite_gk16_set_test ( )
  call hermite_gk18_set_test ( )
  call hermite_gk22_set_test ( )
  call hermite_gk24_set_test ( )
  call hermite_1_set_test ( )
  call hermite_probabilist_set_test ( )
  call imtqlx_test ( )
  call jacobi_ek_compute_test ( )
  call jacobi_integral_test ( )
  call jacobi_ss_compute_test ( )
  call kronrod_set_test ( )
  call laguerre_ek_compute_test ( )
  call laguerre_integral_test ( )
  call laguerre_set_test ( )
  call laguerre_ss_compute_test ( )
  call laguerre_1_set_test ( )
  call legendre_dr_compute_test ( )
  call legendre_ek_compute_test ( )
  call legendre_gw_compute_test ( )
  call legendre_integral_test ( )
  call legendre_set_test ( )
  call legendre_ss_compute_test ( )
  call lobatto_compute_test ( )
  call lobatto_set_test ( )
  call nc_compute_weights_test ( )
  call ncc_compute_test ( )
  call ncc_set_test ( )
  call nco_compute_test ( )
  call nco_set_test ( )
  call ncoh_compute_test ( )
  call ncoh_set_test ( )
  call patterson_set_test ( )
  call r8_psi_test ( )
  call radau_compute_test ( )
  call radau_set_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUADRULE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )
 
  stop 0
end
subroutine chebyshev_set_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV_SET_TEST tests CHEBYSHEV_SET
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV_SET_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV_SET sets a Chebyshev rule over [-1,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 9

    if ( n == 8 ) then
      cycle
    end if

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call chebyshev_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine chebyshev1_compute_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV1_COMPUTE_TEST tests CHEBYSHEV1_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV1_COMPUTE_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV1_COMPUTE computes'
  write ( *, '(a)' ) '  a Chebyshev Type 1 quadrature rule over [-1,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call chebyshev1_compute ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine chebyshev1_integral_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV1_INTEGRAL_TEST tests CHEBYSHEV1_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV1_INTEGRAL_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV1_INTEGRAL evaluates'
  write ( *, '(a)' ) '  Integral ( -1 < x < +1 ) x^n / sqrt(1-x^2) dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         Value'
  write ( *, '(a)' ) ''

  do n = 0, 10
 
    call chebyshev1_integral ( n, value )

    write ( *, '(2x,i8,2x,g24.16)' ) n, value

  end do
 
  return
end
subroutine chebyshev1_set_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV1_SET_TEST tests CHEBYSHEV1_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV1_SET_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV1_SET sets'
  write ( *, '(a)' ) '  a Chebyshev Type 1 quadrature rule over [-1,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call chebyshev1_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine chebyshev2_compute_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV2_COMPUTE_TEST tests CHEBYSHEV2_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 March 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV2_COMPUTE_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV2_COMPUTE computes a Gauss-Chebyshev type 2 rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call chebyshev2_compute ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine chebyshev2_compute_test2 ( )

!*****************************************************************************80
!
!! CHEBYSHEV2_COMPUTE_TEST2 uses CHEBSHEV2_COMPUTE over the semicircle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  real ( kind = 8 ) f(n)
  real ( kind = 8 ) q
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV2_COMPUTE_TEST2'
  write ( *, '(a)' ) '  Approximate the integral of f(x,y) over the semicircle'
  write ( *, '(a)' ) '    -1 <= x <= 1, y = sqrt ( 1 - x^2 )'
  write ( *, '(a)' ) '  using N Chebyshev points.'
  write ( *, '(a)' ) '  If p(x,y) involves any term of odd degree in y,'
  write ( *, '(a)' ) '  the estimate will only be approximate.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Polynomial    N    Integral        Estimate       Error'
  write ( *, '(a)' ) ''

  call chebyshev2_compute ( n, x, w )
!
!  f(x,y) = 1
!
  exact = 1.5707963267948966192D+00
  f(1:n) = 1.0D+00
  q = dot_product ( w(1:n), f(1:n) )
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) '1      ', n, exact, q, error
!
!  f(x,y) = x
!
  exact = 0.0D+00
  f(1:n) = x(1:n)
  q = dot_product ( w(1:n), f(1:n) )
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 'x      ', n, exact, q, error
!
!  f(x,y) = y = sqrt ( 1 - x^2 )
!
  exact = 0.66666666666666666667D+00
  f(1:n) = sqrt ( 1.0D+00 - x(1:n)**2 )
  q = dot_product ( w(1:n), f(1:n) ) / 2.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) '   y   ', n, exact, q, error
!
!  f(x,y) = x^2
!
  exact = 0.39269908169872415481D+00
  f(1:n) = x(1:n)**2
  q = dot_product ( w(1:n), f(1:n) )
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 'x^2    ', n, exact, q, error
!
!  f(x,y) = xy = x * sqrt ( 1 - x^2 )
!
  exact = 0.0D+00
  f(1:n) = x(1:n) * sqrt ( 1.0D+00 - x(1:n)**2 )
  q = dot_product ( w(1:n), f(1:n) ) / 2.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 'x  y   ', n, exact, q, error
!
!  f(x,y) = y^2 -> ( 1 - x^2 )
!
  exact = 0.39269908169872415481D+00
  f(1:n) = 1.0D+00 - x(1:n)**2
  q = dot_product ( w(1:n), f(1:n) ) / 3.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) '   y^2 ', n, exact, q, error
!
!  f(x,y) = x^3
!
  exact = 0.0D+00
  f(1:n) = x(1:n)**3
  q = dot_product ( w(1:n), f(1:n) )
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 'x^3    ', n, exact, q, error
!
!  f(x,y) = x^2 y = x^2 sqrt ( 1 - x^2 )
!
  exact = 0.13333333333333333333D+00
  f(1:n) = x(1:n)**2 * sqrt ( 1.0D+00 - x(1:n)**2 )
  q = dot_product ( w(1:n), f(1:n) ) / 2.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 'x^2y   ', n, exact, q, error
!
!  f(x,y) = x y^2 = x * ( 1 - x^2 )
!
  exact = 0.0D+00
  f(1:n) = x(1:n) * ( 1.0D+00 - x(1:n)**2 )
  q = dot_product ( w(1:n), f(1:n) ) / 3.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    'x  y^2 ', n, exact, q, error
!
!  f(x,y) = y^3
!
  exact = 0.26666666666666666667D+00
  f(1:n) = ( 1.0D+00 - x(1:n)**2 )**(1.5D+00)
  q = dot_product ( w(1:n), f(1:n) ) / 4.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    '   y^3 ', n, exact, q, error
!
!  f(x,y) = x^4
!
  exact = 0.19634954084936207740
  f(1:n) = x(1:n)**4
  q = dot_product ( w(1:n), f(1:n) )
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    'x^4    ', n, exact, q, error
!
!  f(x,y) = x^2y^2 -> x^2( 1 - x^2 )
!
  exact = 0.065449846949787359135D+00
  f(1:n) = x(1:n)**2 * ( 1.0D+00 - x(1:n)**2 )
  q = dot_product ( w(1:n), f(1:n) ) / 3.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    'x^2y^2 ', n, exact, q, error
!
!  f(x,y) = y^4 -> ( 1 - x^2 )^2
!
  exact = 0.19634954084936207740D+00
  f(1:n) = ( 1.0D+00 - x(1:n)**2 )**2
  q = dot_product ( w(1:n), f(1:n) ) / 5.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    '   y^4 ', n, exact, q, error
!
!  f(x,y) = x^4y = x^4 sqrt ( 1 - x^2 )
!
  exact = 0.057142857142857142857D+00
  f(1:n) = x(1:n)**4 * sqrt ( 1.0D+00 - x(1:n)**2 )
  q = dot_product ( w(1:n), f(1:n) ) / 2.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    'x^4y   ', n, exact, q, error
!
!  x^2y^3 = x^2 ( 1 - x^2 )**(3/2)
!
  exact = 0.038095238095238095238D+00
  f(1:n) = x(1:n)**2 * ( 1.0D+00 - x(1:n)**2 )**(1.5D+00)
  q = dot_product ( w(1:n), f(1:n) ) / 4.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    'x^2y^3 ', n, exact, q, error
!
!  f(x,y) = y^5
!
  exact = 0.15238095238095238095D+00
  f(1:n) = ( 1.0D+00 - x(1:n)**2 )**(2.5D+00)
  q = dot_product ( w(1:n), f(1:n) ) / 6.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    '   y^5 ', n, exact, q, error
!
!  f(x,y) = x^6
!
  exact = 0.12271846303085129838D+00
  f(1:n) = x(1:n)**6
  q = dot_product ( w(1:n), f(1:n) )
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
    'x^6    ', n, exact, q, error
!
!  f(x,y) = x^4y^2 -> x^4( 1 - x^2 )
!
  exact = 0.024543692606170259675D+00
  f(1:n) = x(1:n)**4 * ( 1.0D+00 - x(1:n)**2 )
  q = dot_product ( w(1:n), f(1:n) ) / 3.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 'x^4y^2 ', n, exact, q, error
!
!  f(x,y) = x^2y^4 -> x^2( 1 - x^2 )**2
!
  exact = 0.024543692606170259675D+00
  f(1:n) = x(1:n)**2 * ( 1.0D+00 - x(1:n)**2 )**2
  q = dot_product ( w(1:n), f(1:n) ) / 5.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 'x^2y^4 ', n, exact, q, error
!
!  f(x,y) = y^6 -> ( 1 - x^2 )^3
!
  exact = 0.12271846303085129838D+00
  f(1:n) = ( 1.0D+00 - x(1:n)**2 )**3
  q = dot_product ( w(1:n), f(1:n) ) / 7.0D+00
  error = abs ( q - exact )
  write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) '   y^6 ', n, exact, q, error

  return
end
subroutine chebyshev2_integral_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV2_INTEGRAL_TEST tests CHEBYSHEV2_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV2_INTEGRAL_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV2_INTEGRAL evaluates'
  write ( *, '(a)' ) '  Integral ( -1 < x < +1 ) x^n * sqrt(1-x^2) dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         Value'
  write ( *, '(a)' ) ''

  do n = 0, 10
 
    call chebyshev2_integral ( n, value )

    write ( *, '(2x,i8,2x,g24.16)' ) n, value

  end do
 
  return
end
subroutine chebyshev2_set_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV2_SET_TEST tests CHEBYSHEV2_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV2_SET_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV2_SET sets'
  write ( *, '(a)' ) '  a Chebyshev Type 2 quadrature rule over [-1,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call chebyshev2_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine chebyshev3_compute_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV3_COMPUTE_TEST tests CHEBYSHEV3_COMPUTE
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV3_COMPUTE_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV3_COMPUTE computes'
  write ( *, '(a)' ) '  a Chebyshev Type 3 quadrature rule over [-1,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call chebyshev3_compute ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine chebyshev3_integral_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV3_INTEGRAL_TEST tests CHEBYSHEV3_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV3_INTEGRAL_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV3_INTEGRAL evaluates'
  write ( *, '(a)' ) '  Integral ( -1 < x < +1 ) x^n / sqrt(1-x^2) dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         Value'
  write ( *, '(a)' ) ''

  do n = 0, 10
 
    call chebyshev3_integral ( n, value )

    write ( *, '(2x,i8,2x,g24.16)' ) n, value

  end do
 
  return
end
subroutine chebyshev3_set_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV3_SET_TEST tests CHEBYSHEV3_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV3_SET_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV3_SET sets'
  write ( *, '(a)' ) '  a Chebyshev Type 3 quadrature rule over [-1,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call chebyshev3_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine clenshaw_curtis_compute_test ( )

!*****************************************************************************80
!
!! CLENSHAW_CURTIS_COMPUTE_TEST tests CLENSHAW_CURTIS_COMPUTE
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CLENSHAW_CURTIS_COMPUTE_TEST'
  write ( *, '(a)' ) '  CLENSHAW_CURTIS_COMPUTE computes'
  write ( *, '(a)' ) '  a Clenshaw-Curtis quadrature rule over [-1,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call clenshaw_curtis_compute ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine clenshaw_curtis_set_test ( )

!*****************************************************************************80
!
!! CLENSHAW_CURTIS_SET_TEST tests CLENSHAW_CURTIS_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 June 2015
!
!  Author:
!
!    John Burkardt
!
   implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CLENSHAW_CURTIS_SET_TEST'
  write ( *, '(a)' ) '  CLENSHAW_CURTIS_SET sets'
  write ( *, '(a)' ) '  a Clenshaw-Curtis quadrature rule over [-1,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call clenshaw_curtis_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine fejer1_compute_test ( )

!*****************************************************************************80
!
!! FEJER1_COMPUTE_TEST tests FEJER1_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FEJER1_COMPUTE_TEST'
  write ( *, '(a)' ) '  FEJER1_COMPUTE computes a Fejer type 1 rule.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call fejer1_compute ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine fejer1_set_test ( )

!*****************************************************************************80
!
!! FEJER1_SET_TEST tests FEJER1_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FEJER1_SET_TEST'
  write ( *, '(a)' ) '  FEJER1_SET sets a Fejer type 1 rule.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call fejer1_set ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine fejer2_compute_test ( )

!*****************************************************************************80
!
!! FEJER2_COMPUTE_TEST tests FEJER2_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FEJER2_COMPUTE_TEST'
  write ( *, '(a)' ) '  FEJER2_COMPUTE computes a Fejer type 2 rule.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call fejer2_compute ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine fejer2_set_test ( )

!*****************************************************************************80
!
!! FEJER2_SET_TEST tests FEJER2_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FEJER2_SET_TEST'
  write ( *, '(a)' ) '  FEJER2_SET sets a Fejer type 2 rule.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( w(n) )
    allocate ( x(n) )

    call fejer2_set ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine gegenbauer_integral_test ( )

!*****************************************************************************80
!
!! GEGENBAUER_INTEGRAL_TEST tests GEGENBAUER_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  alpha = 0.25D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEGENBAUER_INTEGRAL_TEST'
  write ( *, '(a)' ) '  GEGENBAUER_INTEGRAL evaluates'
  write ( *, '(a)' ) '  Integral ( -1 < x < +1 ) x^n * (1-x^2)^alpha dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         Value'
  write ( *, '(a)' ) ''

  do n = 0, 10
 
    call gegenbauer_integral ( n, alpha, value )

    write ( *, '(2x,i8,2x,g24.16)' ) n, value

  end do
 
  return
end
subroutine gegenbauer_ek_compute_test ( )

!*****************************************************************************80
!
!! GEGENBAUER_EK_COMPUTE_TEST tests GEGENBAUER_EK_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  alpha = 0.50D+00
  a = -1.0D+00
  b = +1.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEGENBAUER_EK_COMPUTE_TEST'
  write ( *, '(a)' ) '  GEGENBAUER_EK_COMPUTE computes a Gauss-Gegenbauer rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Using parameter ALPHA = ', alpha
  write ( *, '(a,g14.6,a,g14.6)' ) '  Integration interval from  ', a, ' to ', b
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '                 W                         X'
  write ( *, '(a)' ) ''

  do n = 1, 10

    allocate ( w(n) )
    allocate ( x(n) )

    call gegenbauer_ek_compute ( n, alpha, a, b, x, w )
 
    write ( *, '(a)' ) ''
    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine gegenbauer_ss_compute_test ( )

!*****************************************************************************80
!
!! GEGENBAUER_SS_COMPUTE_TEST tests GEGENBAUER_SS_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  alpha = 0.50D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEGENBAUER_SS_COMPUTE_TEST'
  write ( *, '(a)' ) '  GEGENBAUER_SS_COMPUTE computes a Gauss-Gegenbauer rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Using parameter ALPHA = ', alpha
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '                 W                         X'
  write ( *, '(a)' ) ''

  do n = 1, 10

    allocate ( w(n) )
    allocate ( x(n) )

    call gegenbauer_ss_compute ( n, alpha, x, w )
 
    write ( *, '(a)' ) ''
    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine gen_hermite_ek_compute_test ( )

!*****************************************************************************80
!
!! GEN_HERMITE_EK_COMPUTE_TEST tests GEN_HERMITE_EK_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  alpha = 0.5D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEN_HERMITE_EK_COMPUTE_TEST'
  write ( *, '(a)' ) '  GEN_HERMITE_EK_COMPUTE computes a '
  write ( *, '(a)' ) '  generalized Hermite quadrature rule'
  write ( *, '(a)' ) '  using the Elhay-Kautsky algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Using ALPHA = ', alpha
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call gen_hermite_ek_compute ( n, alpha, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine gen_hermite_integral_test ( )

!*****************************************************************************80
!
!! GEN_HERMITE_INTEGRAL_TEST tests GEN_HERMITE_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  alpha = 0.5D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEN_HERMITE_INTEGRAL_TEST'
  write ( *, '(a)' ) '  GEN_HERMITE_INTEGRAL evaluates'
  write ( *, '(a)' ) '  Integral ( -oo < x < +oo ) exp(-x^2) x^n |x|^alpha dx'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Using ALPHA = ', alpha
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         Value'
  write ( *, '(a)' ) ''

  do n = 0, 10
 
    call gen_hermite_integral ( n, alpha, value )

    write ( *, '(2x,i8,2x,g24.16)' ) n, value

  end do
 
  return
end
subroutine gen_laguerre_ek_compute_test ( )

!*****************************************************************************80
!
!! GEN_LAGUERRE_EK_COMPUTE_TEST tests GEN_LAGUERRE_EK_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  alpha = 0.5D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEN_LAGUERRE_EK_COMPUTE_TEST'
  write ( *, '(a)' ) '  GEN_LAGUERRE_EK_COMPUTE computes a '
  write ( *, '(a)' ) '  generalized Laguerre quadrature rule'
  write ( *, '(a)' ) '  using the Elhay-Kautsky algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Using ALPHA = ', alpha
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call gen_laguerre_ek_compute ( n, alpha, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine gen_laguerre_integral_test ( )

!*****************************************************************************80
!
!! GEN_LAGUERRE_INTEGRAL_TEST tests GEN_LAGUERRE_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  alpha = 0.5D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEN_LAGUERRE_INTEGRAL_TEST'
  write ( *, '(a)' ) '  GEN_LAGUERRE_INTEGRAL evaluates'
  write ( *, '(a)' ) '  Integral ( 0 < x < +oo ) exp(-x) x^n x^alpha dx'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Using ALPHA = ', alpha
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         Value'
  write ( *, '(a)' ) ''

  do n = 0, 10
 
    call gen_laguerre_integral ( n, alpha, value )

    write ( *, '(2x,i8,2x,g24.16)' ) n, value

  end do
 
  return
end
subroutine gen_laguerre_ss_compute_test ( )

!*****************************************************************************80
!
!! GEN_LAGUERRE_SS_COMPUTE_TEST tests GEN_LAGUERRE_SS_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  alpha = 0.5D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEN_LAGUERRE_SS_COMPUTE_TEST'
  write ( *, '(a)' ) '  GEN_LAGUERRE_SS_COMPUTE computes a '
  write ( *, '(a)' ) '  generalized Laguerre quadrature rule'
  write ( *, '(a)' ) '  using the Stroud-Secrest algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Using ALPHA = ', alpha
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call gen_laguerre_ss_compute ( n, alpha, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine hermite_ek_compute_test ( )

!*****************************************************************************80
!
!! HERMITE_EK_COMPUTE_TEST tests HERMITE_EK_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HERMITE_EK_COMPUTE_TEST'
  write ( *, '(a)' ) '  HERMITE_EK_COMPUTE computes a Hermite quadrature rule'
  write ( *, '(a)' ) '  using the Elhay-Kautsky algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call hermite_ek_compute ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine hermite_integral_test ( )

!*****************************************************************************80
!
!! HERMITE_INTEGRAL_TEST tests HERMITE_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HERMITE_INTEGRAL_TEST'
  write ( *, '(a)' ) '  HERMITE_INTEGRAL evaluates'
  write ( *, '(a)' ) '  Integral ( -oo < x < +oo ) exp(-x^2) x^n dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         Value'
  write ( *, '(a)' ) ''

  do n = 0, 10
 
    call hermite_integral ( n, value )

    write ( *, '(2x,i8,2x,g24.16)' ) n, value

  end do
 
  return
end
subroutine hermite_set_test ( )

!*****************************************************************************80
!
!! HERMITE_SET_TEST tests HERMITE_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HERMITE_SET_TEST'
  write ( *, '(a)' ) '  HERMITE_SET sets'
  write ( *, '(a)' ) '  a Hermite quadrature rule over (-oo,+oo).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call hermite_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine hermite_ss_compute_test ( )

!*****************************************************************************80
!
!! HERMITE_SS_COMPUTE_TEST tests HERMITE_SS_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HERMITE_SS_COMPUTE_TEST'
  write ( *, '(a)' ) '  HERMITE_SS_COMPUTE computes a Hermite quadrature rule'
  write ( *, '(a)' ) '  using the Stroud-Secrest algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call hermite_ss_compute ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine hermite_gk16_set_test ( )

!*****************************************************************************80
!
!! HERMITE_GK16_SET_TEST tests HERMITE_GK16_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 June 2015
!
!  Author:
!
!    John Burkardt
!
   implicit none

  integer ( kind = 4 ), parameter :: l_max = 8

  integer ( kind = 4 ) i
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  integer ( kind = 4 ) :: n_list(0:l_max) = (/ &
    1, 3, 7, 9, 17, 19, 31, 33, 35 /)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HERMITE_GK16_SET_TEST'
  write ( *, '(a)' ) '  HERMITE_GK16_SET sets'
  write ( *, '(a)' ) '  a nested Hermite quadrature rule over (-oo,+oo).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do l = 0, l_max

    n = n_list(l)

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call hermite_gk16_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine hermite_gk18_set_test ( )

!*****************************************************************************80
!
!! HERMITE_GK18_SET_TEST tests HERMITE_GK18_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 June 2015
!
!  Author:
!
!    John Burkardt
!
   implicit none

  integer ( kind = 4 ), parameter :: l_max = 4

  integer ( kind = 4 ) i
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  integer ( kind = 4 ) :: n_list(0:l_max) = (/ &
    1, 3, 9, 19, 37 /)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HERMITE_GK18_SET_TEST'
  write ( *, '(a)' ) '  HERMITE_GK18_SET sets'
  write ( *, '(a)' ) '  a nested Hermite quadrature rule over (-oo,+oo).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do l = 0, l_max

    n = n_list(l)

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call hermite_gk18_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine hermite_gk22_set_test ( )

!*****************************************************************************80
!
!! HERMITE_GK22_SET_TEST tests HERMITE_GK22_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 June 2015
!
!  Author:
!
!    John Burkardt
!
   implicit none

  integer ( kind = 4 ), parameter :: l_max = 4

  integer ( kind = 4 ) i
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  integer ( kind = 4 ) :: n_list(0:l_max) = (/ &
    1, 3, 9, 19, 41 /)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HERMITE_GK22_SET_TEST'
  write ( *, '(a)' ) '  HERMITE_GK22_SET sets'
  write ( *, '(a)' ) '  a nested Hermite quadrature rule over (-oo,+oo).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do l = 0, l_max

    n = n_list(l)

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call hermite_gk22_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine hermite_gk24_set_test ( )

!*****************************************************************************80
!
!! HERMITE_GK24_SET_TEST tests HERMITE_GK24_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 June 2015
!
!  Author:
!
!    John Burkardt
!
   implicit none

  integer ( kind = 4 ), parameter :: l_max = 4

  integer ( kind = 4 ) i
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  integer ( kind = 4 ) :: n_list(0:l_max) = (/ &
    1, 3, 9, 19, 43 /)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HERMITE_GK24_SET_TEST'
  write ( *, '(a)' ) '  HERMITE_GK24_SET sets'
  write ( *, '(a)' ) '  a nested Hermite quadrature rule over (-oo,+oo).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do l = 0, l_max

    n = n_list(l)

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call hermite_gk24_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine hermite_1_set_test ( )

!*****************************************************************************80
!
!! HERMITE_1_SET_TEST tests HERMITE_1_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 June 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HERMITE_1_SET_TEST'
  write ( *, '(a)' ) '  HERMITE_1_SET sets'
  write ( *, '(a)' ) '  a Hermite unit-density quadrature rule over (-oo,+oo).'
  write ( *, '(a)' ) '  The weight is 1.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call hermite_1_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine hermite_probabilist_set_test ( )

!*****************************************************************************80
!
!! HERMITE_PROBABILIST_SET_TEST tests HERMITE_PROBABILIST_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 June 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HERMITE_PROBABILIST_SET_TEST'
  write ( *, '(a)' ) '  HERMITE_PROBABILIST_SET sets'
  write ( *, '(a)' ) '  a Hermite probabilist quadrature rule over (-oo,+oo).'
  write ( *, '(a)' ) '  The weight is exp ( - x * x / 2 ) / sqrt ( 2 * pi ).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call hermite_probabilist_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine imtqlx_test ( )

!*****************************************************************************80
!
!! IMTQLX_TEST tests IMTQLX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2015
!
!  Author:
!
!    John Burkardt.
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) angle
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) e(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) lam(n)
  real ( kind = 8 ) lam2(n)
  real ( kind = 8 ) qtz(n)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) z(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'IMTQLX_TEST'
  write ( *, '(a)' ) '  IMTQLX takes a symmetric tridiagonal matrix A'
  write ( *, '(a)' ) '  and computes its eigenvalues LAM.'
  write ( *, '(a)' ) '  It also accepts a vector Z and computes Q''*Z,'
  write ( *, '(a)' ) '  where Q is the matrix that diagonalizes A.'

  d(1:n) = 2.0D+00
  e(1:n-1) = -1.0D+00
  e(n) = 0.0D+00
  z(1:n) = 1.0D+00
!
!  On input, LAM is D, and QTZ is Z.
!
  lam(1:n) = d(1:n)
  qtz(1:n) = z(1:n)

  call imtqlx ( n, lam, e, qtz )

  call r8vec_print ( n, lam, '  Computed eigenvalues:' )

  do i = 1, n
    angle = real ( i, kind = 8 ) * r8_pi / real ( 2 * ( n + 1 ), kind = 8 )
    lam2(i) = 4.0D+00 * ( sin ( angle ) ) ** 2
  end do

  call r8vec_print ( n, lam2, '  Exact eigenvalues:' )

  call r8vec_print ( n, z, '  Vector Z:' )
  call r8vec_print ( n, qtz, '  Vector Q''*Z:' )

  return
end
subroutine jacobi_ek_compute_test ( )

!*****************************************************************************80
!
!! JACOBI_EK_COMPUTE_TEST tests JACOBI_EK_COMPUTE.
!
!  Discussion:
!
!    Compare with tabular values on page 178 of Stroud and Secrest.
!
!     In particular,
!
!             X              W
!
!     1  -0.9833999115   0.4615276287E-03
!     2  -0.9447138932   0.2732249104E-02
!     3  -0.8849310847   0.8045830455E-02
!    ..  .............   ................
!    19   0.9656375637   0.7613987785E-01
!    20   0.9934477866   0.3348337670E-01
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  alpha = 1.5D+00
  beta = 0.5D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JACOBI_EK_COMPUTE_TEST'
  write ( *, '(a)' ) '  JACOBI_EK_COMPUTE computes a Gauss-Jacobi rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a,f14.6)' ) '  ALPHA = ', alpha
  write ( *, '(a,f14.6)' ) '  BETA =  ', beta

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call jacobi_ek_compute ( n, alpha, beta, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine jacobi_integral_test ( )

!*****************************************************************************80
!
!! JACOBI_INTEGRAL_TEST tests JACOBI_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  alpha = 1.5D+00
  beta = 0.5D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JACOBI_INTEGRAL_TEST'
  write ( *, '(a)' ) '  JACOBI_INTEGRAL evaluates'
  write ( *, '(a)' ) '  Integral ( -1 < x < +1 ) x^n (1-x)^alpha (1+x)^beta dx'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Use ALPHA = ', alpha
  write ( *, '(a,g14.6)' ) '      BETA  = ', beta
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         Value'
  write ( *, '(a)' ) ''

  do n = 0, 10
 
    call jacobi_integral ( n, alpha, beta, value )

    write ( *, '(2x,i8,2x,g24.16)' ) n, value

  end do
 
  return
end
subroutine jacobi_ss_compute_test ( )

!*****************************************************************************80
!
!! JACOBI_SS_COMPUTE_TEST tests JACOBI_SS_COMPUTE.
!
!  Discussion:
!
!    Compare with tabular values on page 178 of Stroud and Secrest.
!
!     In particular,
!
!             X              W
!
!     1  -0.9833999115   0.4615276287E-03
!     2  -0.9447138932   0.2732249104E-02
!     3  -0.8849310847   0.8045830455E-02
!    ..  .............   ................
!    19   0.9656375637   0.7613987785E-01
!    20   0.9934477866   0.3348337670E-01
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  alpha = 1.5D+00
  beta = 0.5D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JACOBI_SS_COMPUTE_TEST'
  write ( *, '(a)' ) '  JACOBI_SS_COMPUTE computes a Gauss-Jacobi rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a,f14.6)' ) '  ALPHA = ', alpha
  write ( *, '(a,f14.6)' ) '  BETA =  ', beta

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call jacobi_ss_compute ( n, alpha, beta, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine kronrod_set_test ( )

!*****************************************************************************80
!
!! KRONROD_SET_TEST tests KRONROD_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) nk
  integer ( kind = 4 ) nl
  integer ( kind = 4 ), dimension ( 4 ) :: nl_test = (/ 7, 10, 15, 20 /)
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable :: wk(:)
  real ( kind = 8 ), allocatable :: wl(:)
  real ( kind = 8 ), allocatable :: xk(:)
  real ( kind = 8 ), allocatable :: xl(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KRONROD_SET_TEST'
  write ( *, '(a)' ) '  KRONROD_SET sets up a Kronrod quadrature rule;'
  write ( *, '(a)' ) '  This is used following a lower order Legendre rule.'

  do test = 1, 4

    write ( *, '(a)' ) ''
    write ( *, '(a,i1)' ) '  Legendre/Kronrod quadrature pair #', test
    write ( *, '(a)' ) '                W                         X'
    write ( *, '(a)' ) ''

    nl = nl_test(test)
    allocate ( xl(1:nl) )
    allocate ( wl(1:nl) )
    call legendre_set ( nl, xl, wl )

    do i = 1, nl
      write ( *, '(2x,i8,2x,g24.16,2x,g24.16)' ) i, wl(i), xl(i)
    end do

    deallocate ( xl )
    deallocate ( wl )

    write ( *, '(a)' ) ''

    nk = 2 * nl + 1
    allocate ( xk(1:nk) )
    allocate ( wk(1:nk) )
    call kronrod_set ( nk, xk, wk )

    do i = 1, nk
      write ( *, '(2x,i8,2x,g24.16,2x,g24.16)' ) i, wk(i), xk(i)
    end do

    deallocate ( xk )
    deallocate ( wk )

  end do

  return
end
subroutine laguerre_ek_compute_test ( )

!*****************************************************************************80
!
!! LAGUERRE_EK_COMPUTE_TEST tests LAGUERRE_EK_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAGUERRE_EK_COMPUTE_TEST'
  write ( *, '(a)' ) '  LAGUERRE_EK_COMPUTE computes a Laguerre quadrature rule'
  write ( *, '(a)' ) '  using the Elhay-Kautsky algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call laguerre_ek_compute ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine laguerre_integral_test ( )

!*****************************************************************************80
!
!! LAGUERRE_INTEGRAL_TEST tests LAGUERRE_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAGUERRE_INTEGRAL_TEST'
  write ( *, '(a)' ) '  LAGUERRE_INTEGRAL evaluates'
  write ( *, '(a)' ) '  Integral ( 0 < x < oo ) x^n * exp(-x) dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         Value'
  write ( *, '(a)' ) ''

  do n = 0, 10
 
    call laguerre_integral ( n, value )

    write ( *, '(2x,i8,2x,g24.16)' ) n, value

  end do
 
  return
end
subroutine laguerre_set_test ( )

!*****************************************************************************80
!
!! LAGUERRE_SET_TEST tests LAGUERRE_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAGUERRE_SET_TEST'
  write ( *, '(a)' ) '  LAGUERRE_SET sets a Laguerre rule from a table.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I       X            W'
 
  do n = 4, 12, 3

    allocate ( w(1:n) )
    allocate ( x(1:n) )

    call laguerre_set ( n, x, w )

    write ( * , '(a)' ) ''
    do i = 1, n
      write (  *, '(2x,i8,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine laguerre_ss_compute_test ( )

!*****************************************************************************80
!
!! LAGUERRE_SS_COMPUTE_TEST tests LAGUERRE_SS_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAGUERRE_SS_COMPUTE_TEST'
  write ( *, '(a)' ) '  LAGUERRE_SS_COMPUTE computes a Laguerre quadrature rule'
  write ( *, '(a)' ) '  using the Stroud-Secrest algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call laguerre_ss_compute ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine laguerre_1_set_test ( )

!*****************************************************************************80
!
!! LAGUERRE_1_SET_TEST tests LAGUERRE_1_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAGUERRE_1_SET_TEST'
  write ( *, '(a)' ) '  LAGUERRE_1_SET sets a Laguerre rule from a table.'
  write ( *, '(a)' ) '  The density function is rho(x)=1.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I       X            W'
 
  do n = 1, 10

    allocate ( w(1:n) )
    allocate ( x(1:n) )

    call laguerre_1_set ( n, x, w )

    write ( * , '(a)' ) ''
    do i = 1, n
      write (  *, '(2x,i8,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine legendre_dr_compute_test ( )

!*****************************************************************************80
!
!! LEGENDRE_DR_COMPUTE_TEST tests LEGENDRE_DR_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEGENDRE_DR_COMPUTE_TEST'
  write ( *, '(a)' ) '  LEGENDRE_DR_COMPUTE computes a Legendre quadrature rule'
  write ( *, '(a)' ) '  using the Davis-Rabinowitz algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( w(1:n) )
    allocate ( x(1:n) )

    call legendre_dr_compute ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine legendre_ek_compute_test ( )

!*****************************************************************************80
!
!! LEGENDRE_EK_COMPUTE_TEST tests LEGENDRE_EK_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEGENDRE_EK_COMPUTE_TEST'
  write ( *, '(a)' ) '  LEGENDRE_EK_COMPUTE computes a Legendre quadrature rule'
  write ( *, '(a)' ) '  using the Elhay-Kautsky algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( w(1:n) )
    allocate ( x(1:n) )

    call legendre_ek_compute ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine legendre_gw_compute_test ( )

!*****************************************************************************80
!
!! LEGENDRE_GW_COMPUTE_TEST tests LEGENDRE_GW_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEGENDRE_GW_COMPUTE_TEST'
  write ( *, '(a)' ) '  LEGENDRE_GW_COMPUTE computes a Legendre quadrature rule'
  write ( *, '(a)' ) '  using the Golub-Welsch algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( w(1:n) )
    allocate ( x(1:n) )

    call legendre_gw_compute ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine legendre_integral_test ( )

!*****************************************************************************80
!
!! LEGENDRE_INTEGRAL_TEST tests LEGENDRE_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEGENDRE_INTEGRAL_TEST'
  write ( *, '(a)' ) '  LEGENDRE_INTEGRAL evaluates'
  write ( *, '(a)' ) '  Integral ( -1 < x < +1 ) x^n dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         Value'
  write ( *, '(a)' ) ''

  do n = 0, 10
 
    call legendre_integral ( n, value )

    write ( *, '(2x,i8,2x,g24.16)' ) n, value

  end do
 
  return
end
subroutine legendre_set_test ( )

!*****************************************************************************80
!
!! LEGENDRE_SET_TEST tests LEGENDRE_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEGENDRE_SET_TEST'
  write ( *, '(a)' ) '  LEGENDRE_SET sets a Legendre quadrature rule.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I       X            W'
 
  do n = 1, 10

    allocate ( w(1:n) )
    allocate ( x(1:n) )

    call legendre_set ( n, x, w )

    write ( * , '(a)' ) ''
    do i = 1, n
      write (  *, '(2x,i8,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine legendre_ss_compute_test ( )

!*****************************************************************************80
!
!! LEGENDRE_SS_COMPUTE_TEST tests LEGENDRE_SS_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEGENDRE_SS_COMPUTE_TEST'
  write ( *, '(a)' ) '  LEGENDRE_SS_COMPUTE computes a Legendre quadrature rule'
  write ( *, '(a)' ) '  using the Stroud-Secrest algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Order       W                         X'

  do n = 1, 10

    allocate ( w(1:n) )
    allocate ( x(1:n) )

    call legendre_ss_compute ( n, x, w )

    write ( *, '(a)' ) ''
    write ( *, '(2x,i8)' ) n

    do i = 1, n
      write ( *, '(10x,2x,g24.16,2x,g24.16)' ) w(i), x(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine lobatto_compute_test ( )

!*****************************************************************************80
!
!! LOBATTO_COMPUTE_TEST tests LOBATTO_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOBATTO_COMPUTE_TEST'
  write ( *, '(a)' ) '  LOBATTO_COMPUTE computes a Lobatto rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I      X             W'
 
  do n = 4, 12, 3

    allocate ( w(1:n) )
    allocate ( x(1:n) )

    call lobatto_compute ( n, x, w )

    write ( * , '(a)' ) ''
    do i = 1, n
      write (  *, '(2x,i8,2x,g14.6,2x,f14.6)' ) i, x(i), w(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do
 
  return
end
subroutine lobatto_set_test ( )

!*****************************************************************************80
!
!! LOBATTO_SET_TEST tests LOBATTO_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOBATTO_SET_TEST'
  write ( *, '(a)' ) '  LOBATTO_SET sets a Lobatto rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I      X             W'
 
  do n = 4, 12, 3

    allocate ( w(1:n) )
    allocate ( x(1:n) )

    call lobatto_set ( n, x, w )

    write ( * , '(a)' ) ''
    do i = 1, n
      write (  *, '(2x,i8,2x,g14.6,2x,f14.6)' ) i, x(i), w(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do
 
  return
end
subroutine nc_compute_weights_test ( )

!*****************************************************************************80
!
!! NC_COMPUTE_WEIGHTS_TEST tests NC_COMPUTE_WEIGHTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NC_COMPUTE_WEIGHTS_TEST'
  write ( *, '(a)' ) '  NC_COMPUTE_WEIGHTS computes weights for'
  write ( *, '(a)' ) '  a closed Newton-Cotes rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  x_min = 0.0D+00
  x_max = 1.0D+00

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call r8vec_linspace ( n, x_min, x_max, x )

    call nc_compute_weights ( n, x_min, x_max, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine ncc_compute_test ( )

!*****************************************************************************80
!
!! NCC_COMPUTE_TEST tests NCC_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NCC_COMPUTE_TEST'
  write ( *, '(a)' ) '  NCC_COMPUTE computes a closed Newton-Cotes rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call ncc_compute ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine ncc_set_test ( )

!*****************************************************************************80
!
!! NCC_SET_TEST tests NCC_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NCC_SET_TEST'
  write ( *, '(a)' ) '  NCC_SET sets up a closed Newton-Cotes rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call ncc_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine nco_compute_test ( )

!*****************************************************************************80
!
!! NCO_COMPUTE_TEST tests NCO_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NCO_COMPUTE_TEST'
  write ( *, '(a)' ) '  NCO_COMPUTE computes an open Newton-Cotes rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call nco_compute ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine nco_set_test ( )

!*****************************************************************************80
!
!! NCO_SET_TEST tests NCO_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NCO_SET_TEST'
  write ( *, '(a)' ) '  NCO_SET sets up an open Newton-Cotes rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call nco_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine ncoh_compute_test ( )

!*****************************************************************************80
!
!! NCOH_COMPUTE_TEST tests NCOH_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NCOH_COMPUTE_TEST'
  write ( *, '(a)' ) '  NCOH_COMPUTE computes an open half Newton-Cotes rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call ncoh_compute ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine ncoh_set_test ( )

!*****************************************************************************80
!
!! NCOH_SET_TEST tests NCOH_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NCOH_SET_TEST'
  write ( *, '(a)' ) '  NCOH_SET sets up an open half Newton-Cotes rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do n = 1, 10

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call ncoh_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine patterson_set_test ( )

!*****************************************************************************80
!
!! PATTERSON_SET_TEST tests PATTERSON_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( 4 ) :: n_test = (/ 1, 3, 7, 15 /)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PATTERSON_SET_TEST'
  write ( *, '(a)' ) '  PATTERSON_SET sets up a Patterson quadrature rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Index       X                       W'

  do j = 1, 4

    n = n_test(j)

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call patterson_set ( n, x, w )

    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,i2,2x,g24.16,2x,g24.16)' ) i, x(i), w(i)
    end do

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine r8_psi_test ( )

!*****************************************************************************80
!
!! R8_PSI_TEST tests R8_PSI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 February 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_psi
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_PSI_TEST:'
  write ( *, '(a)' ) '  R8_PSI evaluates the Psi function.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      X         Psi(X)                     Psi(X)  ' &
  // '               DIFF'
  write ( * , '(a)' ) '               (Tabulated)                (R8_PSI)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call psi_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_psi ( x )

    write ( *, '(2x,f8.4,2x,g24.16,2x,g24.16,2x,g10.4)' ) &
    x, fx, fx2, abs ( fx - fx2 )

  end do

  return
end
subroutine radau_compute_test ( )

!*****************************************************************************80
!
!! RADAU_COMPUTE_TEST tests RADAU_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RADAU_COMPUTE_TEST'
  write ( *, '(a)' ) '  RADAU_COMPUTE computes a Radau rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I       X            W'
 
  do n = 4, 12, 3

    allocate ( w(1:n) )
    allocate ( x(1:n) )

    call radau_compute ( n, x, w )

    write ( * , '(a)' ) ''
    do i = 1, n
      write (  *, '(2x,i8,2x,g14.6,2x,f14.6)' ) i, x(i), w(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do
 
  return
end
subroutine radau_set_test ( )

!*****************************************************************************80
!
!! RADAU_SET_TEST tests RADAU_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RADAU_SET_TEST'
  write ( *, '(a)' ) '  RADAU_SET sets a Radau rule from a table.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I       X            W'
 
  do n = 4, 12, 3

    allocate ( w(1:n) )
    allocate ( x(1:n) )

    call radau_set ( n, x, w )

    write ( * , '(a)' ) ''
    do i = 1, n
      write (  *, '(2x,i8,2x,g14.6,2x,f14.6)' ) i, x(i), w(i)
    end do

    deallocate ( w )
    deallocate ( x )

  end do
 
  return
end

