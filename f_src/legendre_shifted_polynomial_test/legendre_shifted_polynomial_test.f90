program main

!*****************************************************************************80
!
!! MAIN is the main program for LEGENDRE_SHIFTED_POLYNOMIAL_TEST.
!
!  Discussion:
!
!    LEGENDRE_POLYNOMIAL_TEST tests the LEGENDRE_SHIFTED_POLYNOMIAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LEGENDRE_SHIFTED_POLYNOMIAL_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the LEGENDRE_SHIFTED_POLYNOMIAL library.'

  call p01_polynomial_value_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LEGENDRE_SHIFTED_POLYNOMIAL_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine p01_polynomial_value_test ( )

!*****************************************************************************80
!
!! P01_POLYNOMIAL_VALUE_TEST tests P01_POLYNOMIAL_VALUE.
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
  write ( *, '(a)' ) 'P01_POLYNOMIAL_VALUE_TEST:'
  write ( *, '(a)' ) '  P01_POLYNOMIAL_VALUE evaluates the shifted Legendre polynomial P01(n,x).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                        Tabulated                 Computed'
  write ( *, '(a)' ) '     N        X         P01(N,X)                  P01(N,X)                     Error'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call p01_polynomial_values ( n_data, n, x, fx1 )

    x_vec(1) = x

    if ( n_data == 0 ) then
      exit
    end if

    allocate ( v(m,n+1) )
    call p01_polynomial_value ( m, n, x_vec, v )
    fx2 = v(1,n+1)
    deallocate ( v )

    e = fx1 - fx2

    write ( *, '(2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) &
      n, x, fx1, fx2, e

  end do

  return
end

