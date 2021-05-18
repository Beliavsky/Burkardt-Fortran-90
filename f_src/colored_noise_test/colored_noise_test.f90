program main

!*****************************************************************************80
!
!! MAIN is the main program for COLORED_NOISE_TEST.
!
!  Discussion:
!
!    COLORED_NOISE_TEST tests the COLORED_NOISE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) q_d
  integer ( kind = 4 ) seed_init

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'COLORED_NOISE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the COLORED_NOISE library.'

  call r8vec_sftf_test ( )

  n = 128
  q_d = 1.0D+00
  alpha = 0.00D+00
  seed_init = 123456789

  do i = 0, 8
    alpha = 0.25D+00 * real ( i, kind = 8 )
    call colored_noise_test01 ( n, q_d, alpha, seed_init )
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'COLORED_NOISE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, * ) ' '
  call timestamp ( )

  stop 0
end
