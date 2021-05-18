program main

!*****************************************************************************80
!
!! ATBASH_TEST tests ATBASH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ATBASH_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  ATBASH_ENCRYPT encrypts a plain text using the Atbash'
  write ( *, '(a)' ) '  substitution cipher.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ATBASH_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests ATBASH with a short phrase.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 80 ) crypt
  character ( len = 80 ) decrypt
  character ( len = 80 ) plain

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Apply ATBASH to a short phrase.'

  plain = 'A man, a plan, a canal - Panama!'
  call atbash_encrypt ( plain, crypt )
  call atbash_encrypt ( crypt, decrypt )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PLAIN:   "' // trim ( plain ) // '"'
  write ( *, '(a)' ) 'CRYPT:   "' // trim ( crypt ) // '"'
  write ( *, '(a)' ) 'DECRYPT: "' // trim ( decrypt ) // '"'

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests ATBASH with another phrase.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 100 ) crypt
  character ( len = 100 ) decrypt
  character ( len = 100 ) plain

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Apply ATBASH to a longer phrase.'

  plain = 'There are a thousand hacking at the branches of evil for every one who is striking at its root.'
  call atbash_encrypt ( plain, crypt )
  call atbash_encrypt ( crypt, decrypt )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PLAIN:   "' // trim ( plain ) // '"'
  write ( *, '(a)' ) 'CRYPT:   "' // trim ( crypt ) // '"'
  write ( *, '(a)' ) 'DECRYPT: "' // trim ( decrypt ) // '"'

  return
end
