program main

!*****************************************************************************80
!
!! MAIN is the main program for WALKER_SAMPLE_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WALKER_SAMPLE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the WALKER_SAMPLE library.'
  
  call i4_choose_test ( )
  call i4_uniform_ab_test ( )
  call normalize_test ( )
  call r8_uniform_01_test ( )
  call r8vec_print_test ( )
  call r8vec_uniform_01_test ( )
  call random_permutation_test ( )
  call walker_build_test ( )
  call walker_sampler_test ( )
  call walker_verify_test ( )
  call zipf_probability_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WALKER_SAMPLE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
