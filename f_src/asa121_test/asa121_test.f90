program main

!*****************************************************************************80
!
!! MAIN is the main program for ASA121_PRB.
!
!  Discussion:
!
!    ASA121_TEST tests ASA121.
!
!  Modified:
!
!    19 January 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  write ( *, '(a)' ) ' '
  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ASA121_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ASA121 library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ASA121_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 compares TRIGAMMA against tabulated values.
!
!  Modified:
!
!    19 January 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) ifault
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) trigamma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  Compare tabulated values of the trigamma'
  write ( *, '(a)' ) '  function against values computed'
  write ( *, '(a)' ) '  computed by TRIGAMMA.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        X          FX                        FX                     DIFF'
  write ( *, '(a)' ) &
    '                  (Tabulated)               (TRIGAMMA)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call trigamma_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = trigamma ( x, ifault )

    write ( *, '(2x,f10.4,2x,g24.16,2x,g24.16,2x,g10.4)' ) &
    x, fx, fx2, abs ( fx - fx2 )

  end do

  return
end
