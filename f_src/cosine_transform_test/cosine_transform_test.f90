program main

!*****************************************************************************80
!
!! MAIN is the main program for COSINE_TRANSFORM_TEST.
!
!  Discussion:
!
!    COSINE_TRANSFORM_TEST tests the COSINE_TRANSFORM library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COSINE_TRANSFORM_TEST'
  write ( *, '(a)' ) '  MATLAB version.'
  write ( *, '(a)' ) '  Test the COSINE_TRANSFORM library.'

  call cosine_transform_test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COSINE_TRANSFORM_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine cosine_transform_test01 ( )

!*****************************************************************************80
!
!! COSINE_TRANSFORM_TEST01 does a DCT and its inverse.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) s(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COSINE_TRANSFORM_TEST01:'
  write ( *, '(a)' ) '  COSINE_TRANSFORM_DATA does a cosine transform of data'
  write ( *, '(a)' ) '  defined by a vector.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Apply the transform, then its inverse.'
  write ( *, '(a)' ) '  Let R be a random N vector.'
  write ( *, '(a)' ) '  Let S be the transform of D.'
  write ( *, '(a)' ) '  Let T be the transform of E.'
  write ( *, '(a)' ) '  Then R and T will be equal.'

  seed = 123456789
  call r8vec_uniform_01 ( n, seed, r )
  call cosine_transform_data ( n, r, s )
  call cosine_transform_inverse ( n, s, t )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I      R(I)        S(I)        T(I)'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,i4,2x,f10.6,2x,f10.6,2x,f10.6)' ) i, r(i), s(i), t(i)
  end do

  return
end
