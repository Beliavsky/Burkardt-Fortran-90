program main

!*****************************************************************************80
!
!! CONTINUED_FRACTION_TEST tests the CONTINUED_FRACTION library.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    05 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CONTINUED_FRACTION_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  CONTINUED_FRACTION is a library for dealing with'
  write ( *, '(a)' ) '  expresssions representing a continued fraction.'

  call i4cf_evaluate_test ( )
  call i4scf_evaluate_test ( )
  call i8cf_evaluate_test ( )
  call i8scf_evaluate_test ( )
  call r8_to_i4scf_test ( )
  call r8_to_i8scf_test ( )
  call r8cf_evaluate_test ( )
  call r8scf_evaluate_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CONTINUED_FRACTION_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  call timestamp ( )

  stop
end

