program main

!*****************************************************************************80
!
!! MAIN is the main program for ASA109_TEST.
!
!  Discussion:
!
!    ASA109_TEST calls the ASA109 routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ASA109_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ASA109 library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ASA109_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 demonstrates the use of XINBTA
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) beta_log
  real ( kind = 8 ) fx
  integer ( kind = 4 ) ifault
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) xinbta

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  XINBTA inverts the incomplete Beta function.'
  write ( *, '(a)' ) '  Compare with tabulated values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) '      A       B           FX      ', &
    '    X                         X                       DIFF'
  write ( *, '(a,a)' ) '                                  ', &
    '   (tabulated)               (XINBTA)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call beta_inc_values ( n_data, a, b, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    beta_log = lgamma ( a ) &
             + lgamma ( b ) &
             - lgamma ( a + b )

    x2 = xinbta ( a, b, beta_log, fx, ifault )

    write ( *, &
    '(2x,f6.2,2x,f6.2,2x,f14.6,2x,g24.16,2x,g24.16,2x,g10.4)' ) &
    a, b, fx, x, x2, abs ( x - x2 )

  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests BETA_INC_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) beta_log
  real ( kind = 8 ) betain
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) ifault
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  BETA_INC_VALUES returns values of '
  write ( *, '(a)' ) '  the incomplete Beta function.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '          A               B               X           BETA_INC(A,B)(X)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call beta_inc_values ( n_data, a, b, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    beta_log = lgamma ( a ) &
             + lgamma ( b ) &
             - lgamma ( a + b )

    fx2 = betain ( x, a, b, beta_log, ifault )

    write ( *, &
      '(2x,f12.8,2x,f12.8,2x,f14.6,2x,g24.16,2x,g24.16,2x,e10.4)' ) &
      a, b, x, fx, fx2, abs ( fx - fx2 )

  end do

  return
end
