program main

!*****************************************************************************80
!
!! MAIN is the main program for CAESAR_TEST.
!
!  Discussion:
!
!    CAESAR_TEST tests the CAESAR library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) k
  character ( len = 80 ) s1
  character ( len = 80 ) s2
  character ( len = 80 ) s3

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CAESAR_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the CAESAR library.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  call S_TO_CAESAR ( S1, K, S2 ), varying K.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   K  ---------------S1--------------  ---------------S2--------------'
  write ( *, '(a)' ) ''
  do k = -5, 5
    s1 = 'A man, a plan, a canal: Panama!'
    call s_to_caesar ( s1, k, s2 )
    write ( *, '(2x,i2,2x,a31,2x,a31)' ) k, s1, s2
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  call S_TO_CAESAR ( S1,  K, S2 ).'
  write ( *, '(a)' ) '  call S_TO_CAESAR ( S2, -K, S3 )'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   K  ------------S1----------  ------------S2----------  ------------S3----------'
  write ( *, '(a)' ) ''
  do k = -5, 5
    s1 = 'The key is under the mat'
    call s_to_caesar ( s1, k, s2 )
    call s_to_caesar ( s2, -k, s3 )
    write ( *, '(2x,i2,2x,a24,2x,a24,2x,a24)' ) k, s1, s2, s3
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CAESAR_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end

