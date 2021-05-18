program main

!*****************************************************************************80
!
!! MAIN is the main program for ASA245_TEST.
!
!  Discussion:
!
!    ASA245_TEST calls the ASA245 routines.
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
  write ( *, '(a)' ) 'ASA245_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ASA245 library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ASA245_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 demonstrates the use of ALNGAM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alngam
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) ifault
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  ALNGAM computes the logarithm of the '
  write ( *, '(a)' ) '  Gamma function.  We compare the result'
  write ( *, '(a)' ) '  to tabulated values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) '          X                     ', &
    'FX                        FX2'
  write ( *, '(a,a)' ) '                                ', &
    '(Tabulated)               (ALNGAM)                DIFF'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call gamma_log_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = alngam ( x, ifault )

    write ( *, '(2x,f24.16,2x,g24.16,2x,g24.16,2x,g10.4)' ) &
    x, fx, fx2, abs ( fx - fx2 )

  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 demonstrates the use of LNGAMMA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) ier
  real ( kind = 8 ) lngamma
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  LNGAMMA computes the logarithm of the '
  write ( *, '(a)' ) '  Gamma function.  We compare the result'
  write ( *, '(a)' ) '  to tabulated values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) '          X                     ', &
    'FX                        FX2'
  write ( *, '(a,a)' ) '                                ', &
    '(Tabulated)               (LNGAMMA)                DIFF'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call gamma_log_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = lngamma ( x, ier )

    write ( *, '(2x,f24.16,2x,g24.16,2x,g24.16,2x,g10.4)' ) &
    x, fx, fx2, abs ( fx - fx2 )

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 demonstrates the use of LGAMMA.
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

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03:'
  write ( *, '(a)' ) '  LGAMMA computes the logarithm of the '
  write ( *, '(a)' ) '  Gamma function.  '
  write ( *, '(a)' ) '  It may be a built in function for some compilers.'
  write ( *, '(a)' ) '  We compare the result to tabulated values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) '          X                     ', &
    'FX                        FX2'
  write ( *, '(a,a)' ) '                                ', &
    '(Tabulated)               (LGAMMA)                 DIFF'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call gamma_log_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = lgamma ( x )

    write ( *, '(2x,f24.16,2x,g24.16,2x,g24.16,2x,g10.4)' ) &
    x, fx, fx2, abs ( fx - fx2 )

  end do

  return
end
