program main

!*****************************************************************************80
!
!! MAIN is the main program for BESSELJ_TEST.
!
!  Discussion:
!
!    BESSELJ_TEST tests the BESSELJ library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BESSELJ_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BESSELJ library.'

  call dbesj_test ( )
  call rjbesl_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BESSELJ_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine dbesj_test ( )

!*****************************************************************************80
!
!! DBESJ_TEST tests DBESJ.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) fx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) nz
  real ( kind = 8 ) x
  real ( kind = 8 ) y(1)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DBESJ_TEST:'
  write ( *, '(a)' ) '  DBESJ evaluates the Bessel J function with NONINTEGER'
  write ( *, '(a)' ) '  order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '        ALPHA           X                     FX                        FX'
  write ( *, '(a)' ) '                                              exact                     computed'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_jx_values ( n_data, alpha, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    n = 1
    call dbesj ( x, alpha, n, y, nz )

    write ( *, '(2x,f12.6,2x,f24.16,2x,g24.16,2x,g24.16)' ) &
      alpha, x, fx, y(1)

  end do

  return
end
subroutine rjbesl_test ( )

!*****************************************************************************80
!
!! RJBESL_TEST tests RJBESL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) fx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) nb
  integer ( kind = 4 ) ncalc
  real ( kind = 8 ) order
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'RJBESL_TEST:'
  write ( *, '(a)' ) '  RJBESL evaluates the Bessel J function with NONINTEGER'
  write ( *, '(a)' ) '  order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '        ORDER           X                     FX                        FX'
  write ( *, '(a)' ) '                                              exact                     computed'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_jx_values ( n_data, order, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    n = int ( order )
    alpha = order - real ( n, kind = 8 )
    allocate ( b(0:n) )
    nb = n + 1

    call rjbesl ( x, alpha, nb, b, ncalc )

    write ( *, '(2x,f12.6,2x,f24.16,2x,g24.16,2x,g24.16)' ) &
      order, x, fx, b(n)

    deallocate ( b )

  end do

  return
end
