program main

!*****************************************************************************80
!
!! ROT13_TEST tests the ROT13 library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 80 ) s1
  character ( len = 80 ) s2
  character ( len = 80 ) s3

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROT13_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the ROT13 library.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  call S_TO_ROT13 ( S1, S2 ).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  -------------------S1-------------------  -------------------S2-------------------'
  write ( *, '(a)' ) ''

  s1 = 'abcdefghijklmnopqrstuvwxyz'
  call s_to_rot13 ( s1, s2 )
  call s_quote ( s1, '"', s1 )
  call s_quote ( s2, '"', s2 )
  write ( *, '(2x,a40,2x,a40)' ) s1, s2

  s1 = 'Cher'
  call s_to_rot13 ( s1, s2 )
  call s_quote ( s1, '"', s1 )
  call s_quote ( s2, '"', s2 )
  write ( *, '(2x,a40,2x,a40)' ) s1, s2

  s1 = 'James Thurston Howell III'
  call s_to_rot13 ( s1, s2 )
  call s_quote ( s1, '"', s1 )
  call s_quote ( s2, '"', s2 )
  write ( *, '(2x,a40,2x,a40)' ) s1, s2

  s1 = 'The bill is $1,205,837.49 so pay now!'
  call s_to_rot13 ( s1, s2 )
  call s_quote ( s1, '"', s1 )
  call s_quote ( s2, '"', s2 )
  write ( *, '(2x,a40,2x,a40)' ) s1, s2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  call ROT13 ( S1, S2 ).'
  write ( *, '(a)' ) '  call ROT13 ( S2, S3 ).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  -------------------S1-------------------  -------------------S3-------------------'
  write ( *, '(a)' ) ''

  s1 = 'abcdefghijklmnopqrstuvwxyz'
  call s_to_rot13 ( s1, s2 )
  call s_to_rot13 ( s2, s3 )
  call s_quote ( s1, '"', s1 )
  call s_quote ( s3, '"', s3 )
  write ( *, '(2x,a40,2x,a40)' ) s1, s3

  s1 = 'Cher'
  call s_to_rot13 ( s1, s2 )
  call s_to_rot13 ( s2, s3 )
  call s_quote ( s1, '"', s1 )
  call s_quote ( s3, '"', s3 )
  write ( *, '(2x,a40,2x,a40)' ) s1, s3

  s1 = 'James Thurston Howell III'
  call s_to_rot13 ( s1, s2 )
  call s_to_rot13 ( s2, s3 )
  call s_quote ( s1, '"', s1 )
  call s_quote ( s3, '"', s3 )
  write ( *, '(2x,a40,2x,a40)' ) s1, s3

  s1 = 'The bill is $1,205,837.49 so pay now!'
  call s_to_rot13 ( s1, s2 )
  call s_to_rot13 ( s2, s3 )
  call s_quote ( s1, '"', s1 )
  call s_quote ( s3, '"', s3 )
  write ( *, '(2x,a40,2x,a40)' ) s1, s3
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROT13_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end

