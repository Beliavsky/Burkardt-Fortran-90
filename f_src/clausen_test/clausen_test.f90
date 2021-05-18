program main

!*****************************************************************************80
!
!! MAIN is the main program for CLAUSEN_TEST.
!
!  Discussion:
!
!    CLAUSEN_TEST tests the CLAUSEN library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CLAUSEN_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the CLAUSEN library.'

  call clausen_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CLAUSEN_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine clausen_test ( )

!*****************************************************************************80
!
!! CLAUSEN_TEST compares the CLAUSEN function to some tabulated values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) clausen
  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CLAUSEN_TEST:'
  write ( *, '(a)' ) '  CLAUSEN evaluates the Clausen function.'
  write ( *, '(a)' ) '  Compare its results to tabulated data.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '                               Tabulated               Computed'
  write ( *, '(a)' ) '           X                          FX                     FX        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call clausen_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = clausen ( x )

    diff = abs ( fx1 - fx2 )

    write ( *, '(2x,g14.6,2x,g24.16,2x,g24.16,2x,e7.1)' ) x, fx1, fx2, diff

  end do

  return
end
