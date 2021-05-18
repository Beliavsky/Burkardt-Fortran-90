program main

!*****************************************************************************80
!
!! MAIN is the main program for CUBE_FELIPPA_RULE_TEST.
!
!  Discussion:
!
!    CUBE_FELIPPA_RULE_TEST tests the CUBE_FELIPPA_RULE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) degree_max

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CUBE_FELIPPA_RULE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the CUBE_FELIPPA_RULE library.'

  degree_max = 4
  call cube_monomial_test ( degree_max )

  degree_max = 6
  call cube_quad_test ( degree_max )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CUBE_FELIPPA_RULE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end

