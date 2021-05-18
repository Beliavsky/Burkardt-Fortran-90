program main

!*****************************************************************************80
!
!! MAIN is the main program for LOBATTO_POLYNOMIAL_TEST.
!
!  Discussion:
!
!    LOBATTO_POLYNOMIAL_TEST tests the LOBATTO_POLYNOMIAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the LOBATTO_POLYNOMIAL library.'

  call lobatto_polynomial_value_test ( )
  call lobatto_polynomial_derivative_test ( )
  call lobatto_polynomial_plot_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( );

  stop 0
end
subroutine lobatto_polynomial_value_test ( )

!*****************************************************************************80
!
!! LOBATTO_POLYNOMIAL_VALUE_TEST tests LOBATTO_POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) e
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  real ( kind = 8 ), allocatable :: l(:,:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  m = 1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  LOBATTO_POLYNOMIAL_VALUES stores values of'
  write ( *, '(a)' ) '  the completed Lobatto polynomial L(n,x).'
  write ( *, '(a)' ) '  LOBATTO_POLYNOMIAL_VALUE evaluates the completed Lobatto polynomial.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '                                       Tabulated                 Computed'
  write ( *, '(a)' ) '     N        X                        L(N,X)                    L(N,X)        Error'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call lobatto_polynomial_values ( n_data, n, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    allocate ( l(1:m,1:n) )

    call lobatto_polynomial_value ( m, n, x, l )

    fx2 = l(1,n)

    e = fx1 - fx2

    write ( *, '(2x,i4,2x,f12.4,2x,g24.16,2x,g24.16,2x,g8.1)' ) &
      n, x, fx1, fx2, e

    deallocate ( l )

  end do

  return
end
subroutine lobatto_polynomial_derivative_test ( )

!*****************************************************************************80
!
!! LOBATTO_POLYNOMIAL_DERIVATIVE_TEST tests LOBATTO_POLYNOMIAL_DERIVATIVE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) e
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  real ( kind = 8 ), allocatable :: lp(:,:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  m = 1


  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_DERIVATIVE_TEST:'
  write ( *, '(a)' ) '  LOBATTO_POLYNOMIAL_DERIVATIVES stores derivatives of'
  write ( *, '(a)' ) '  the completed Lobatto polynomial L(n,x).'
  write ( *, '(a)' ) '  LOBATTO_POLYNOMIAL_DERIVATIVE evaluates the completed Lobatto polynomial.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '                                       Tabulated                 Computed'
  write ( *, '(a)' ) '     N        X                        L''(N,X)                   L''(N,X)       Error'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call lobatto_polynomial_derivatives ( n_data, n, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    allocate ( lp(1:m,1:n) )
    call lobatto_polynomial_derivative ( m, n, x, lp )
    fx2 = lp(1,n)

    e = fx1 - fx2

    write ( *, '(2x,i4,2x,f12.4,2x,g24.16,2x,g24.16,2x,g8.1)' ) &
      n, x, fx1, fx2, e

    deallocate ( lp )

  end do

  return
end
subroutine lobatto_polynomial_plot_test ( )

!*****************************************************************************80
!
!! LOBATTO_POLYNOMIAL_PLOT_TEST tests LOBATTO_POLYNOMIAL_PLOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: ndx_num = 7

  integer ( kind = 4 ), dimension ( ndx_num ) :: ndx = (/ &
    1, 2, 3, 4, 5, 6, 7 /)
  character ( len = 255 ) prefix

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_PLOT_TEST:'
  write ( *, '(a)' ) '  LOBATTO_POLYNOMIAL_PLOT plots Lobatto polynomials.'

  prefix = 'test'

  call lobatto_polynomial_plot ( ndx_num, ndx, prefix )

  return
end
