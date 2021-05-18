program main

!*****************************************************************************80
!
!! MAIN is the main program for GEGENBAUER_POLYNOMIAL_TEST.
!
!  Discussion:
!
!    GEGENBAUER_POLYNOMIAL_TEST tests the GEGENBAUER_POLYNOMIAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GEGENBAUER_POLYNOMIAL_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the GEGENBAUER_POLYNOMIAL library.'

  call gegenbauer_alpha_check_test ( )
  call gegenbauer_ek_compute_test ( )
  call gegenbauer_integral_test ( )
  call gegenbauer_polynomial_value_test ( )
  call gegenbauer_ss_compute_test ( )

  call imtqlx_test ( )

  call r8_gamma_test ( )
  call r8_hyper_2f1_test ( )
  call r8_psi_test ( )
  call r8_uniform_ab_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GEGENBAUER_POLYNOMIAL_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine gegenbauer_alpha_check_test ( )

!*****************************************************************************80
!
!! GEGENBAUER_ALPHA_CHECK_TEST compares GEGENBAUER_ALPHA_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  logical ( kind = 4 ) check
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEGENBAUER_ALPHA_CHECK_TEST'
  write ( *, '(a)' ) '  GEGENBAUER_ALPHA_CHECK checks that ALPHA is legal.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       ALPHA   Check?'
  write ( *, '(a)' ) ''

  seed = 123456789

  do n = 1, 10

    alpha = r8_uniform_ab ( -5.0D+00, +5.0D+00, seed )
    call gegenbauer_alpha_check ( alpha, check )
    write ( *, '(2x,f10.4,7x,l1)' ) alpha, check

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

  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  alpha = 0.50D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEGENBAUER_EK_COMPUTE_TEST'
  write ( *, '(a)' ) '  GEGENBAUER_EK_COMPUTE computes a Gauss-Gegenbauer rule;'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Using parameter ALPHA = ', alpha
  write ( *, '(a,g14.6,a,g14.6)' ) '  Integration interval is [-1,+1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '                 W                         X'
  write ( *, '(a)' ) ''

  do n = 1, 10

    allocate ( w(n) )
    allocate ( x(n) )

    call gegenbauer_ek_compute ( n, alpha, x, w )
 
    write ( *, '(a)' ) ''
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

  alpha = 0.50D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEGENBAUER_INTEGRAL_TEST'
  write ( *, '(a)' ) '  GEGENBAUER_INTEGRAL evaluates'
  write ( *, '(a)' ) '  Integral ( -1 < x < +1 ) x^n * (1-x^2)^(alpha-1/2) dx'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         Value'
  write ( *, '(a)' ) ''

  do n = 0, 10
 
    call gegenbauer_integral ( n, alpha, value )

    write ( *, '(2x,i8,2x,g24.16)' ) n, value

  end do
 
  return
end
subroutine gegenbauer_polynomial_value_test ( )

!*****************************************************************************80
!
!! GEGENBAUER_POLYNOMIAL_VALUE_TEST tests GEGENBAUER_POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ), allocatable :: c(:,:)
  real ( kind = 8 ) fx(1)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x(1)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEGENBAUER_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  GEGENBAUER_POLYNOMIAL_VALUE evaluates the Gegenbauer polynomial.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       M     ALPHA         X           GPV    GEGENBAUER'
  write ( *, '(a)' ) ''

  n = 1

  n_data = 0

  do

    call gegenbauer_polynomial_values ( n_data, m, alpha, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    allocate ( c(0:m,1:n) )

    call gegenbauer_polynomial_value ( m, n, alpha, x, c )

    write ( *, '(2x,i6,2x,f8.2,2x,f8.2,2x,f12.4,2x,f12.4)' ) m, alpha, x(1), fx, c(m,1)

    deallocate ( c )

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
subroutine r8_gamma_test ( )

!*****************************************************************************80
!
!! R8_GAMMA_TEST tests R8_GAMMA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 Nvember 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_gamma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_GAMMA_TEST:'
  write ( *, '(a)' ) '  R8_GAMMA evaluates the Gamma function.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      X       Gamma(X)                   Gamma(X)  ' &
  // '               DIFF'
  write ( * , '(a)' ) '               (Tabulated)              (R8_GAMMA)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call gamma_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_gamma ( x )

    write ( *, '(2x,f8.4,2x,g24.16,2x,g24.16,2x,g10.4)' ) &
    x, fx, fx2, abs ( fx - fx2 )

  end do

  return
end
subroutine r8_hyper_2f1_test ( )

!*****************************************************************************80
!
!! R8_HYPER_2F1_TEST tests R8_HYPER_2F1.
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
  real ( kind = 8 ) c
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_HYPER_2F1_TEST:'
  write ( *, '(a)' ) '  R8_HYPER_2F1 evaluates the hypergeometric 2F1 function.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) '      A       B       C       X      ', &
  ' 2F1                       2F1                     DIFF'
  write ( *, '(a,a)' ) '                                     ', &
  '(tabulated)               (computed)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call hyper_2f1_values ( n_data, a, b, c, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    call r8_hyper_2f1 ( a, b, c, x, fx2 )

    write ( *, &
    '(2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,g24.16,2x,g24.16,2x,g10.4)' ) &
    a, b, c, x, fx, fx2, abs ( fx - fx2 )

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
subroutine r8_uniform_ab_test ( )

!*****************************************************************************80
!
!! R8_UNIFORM_AB_TEST tests R8_UNIFORM_AB.
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

  real ( kind = 8 ), parameter :: b = 10.0D+00
  real ( kind = 8 ), parameter :: c = 20.0D+00
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  R8_UNIFORM returns random values in a given range:'
  write ( *, '(a)' ) '  [ B, C ]'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  For this problem:'
  write ( *, '(a,g14.6)' ) '  B = ', b
  write ( *, '(a,g14.6)' ) '  C = ', c
  write ( *, '(a)' ) ' '

  seed = 123456789

  do i = 1, 10
    r = r8_uniform_ab ( b, c, seed )
    write ( *, '(2x,g14.6)' ) r
  end do

  return
end
