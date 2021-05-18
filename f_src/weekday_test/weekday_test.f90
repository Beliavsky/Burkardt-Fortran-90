program main

!*****************************************************************************80
!
!! MAIN is the main program for WEEKDAY_TEST.
!
!  Discussion:
!
!    WEEKDAY_TEST tests the WEEKDAY library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 July 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WEEKDAY_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the WEEKDAY library.'

  call jed_to_weekday_test ( )
  call ymd_to_weekday_common_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WEEKDAY_TEST:'
  write ( *, '(a)' ) '  Noraml end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
