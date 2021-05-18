program main

!*****************************************************************************80
!
!! RANDOM_SORTED_TEST tests the RANDOM_SORTED library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RANDOM_SORTED_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the RANDOM_SORTED library.'

  call r8vec_normal_01_sorted_test ( )
  call r8vec_uniform_01_test ( )
  call r8vec_uniform_01_sorted1_test ( )
  call r8vec_uniform_01_sorted2_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RANDOM_SORTED_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine r8vec_normal_01_sorted_test ( )

!*****************************************************************************80
!
!! R8VEC_NORMAL_01_SORTED_TEST tests R8VEC_NORMAL_01_SORTED_TEST,
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_NORMAL_01_SORTED_TEST:'
  write ( *, '(a)' ) '  R8VEC_NORMAL_01_SORTED generates a vector of N normal 01'
  write ( *, '(a)' ) '  random values in ascending sorted order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate several examples:'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10
    call r8vec_normal_01_sorted ( n, seed, r8vec )
    call r8vec_transpose_print ( n, r8vec, '  R8VEC:' )
  end do

  return
end
subroutine r8vec_uniform_01_test ( )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01_TEST tests R8VEC_UNIFORM_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_UNIFORM_01'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_01 returns a random R8VEC'
  write ( *, '(a)' ) '  with entries in [ 0.0, 1.0 ]'

  seed = 123456789

  do i = 1, 3

    write ( *, '(a)' ) ''
    write ( *, '(a,i12)' ) '  Input SEED = ', seed

    call r8vec_uniform_01 ( n, seed, r8vec )

    call r8vec_print ( n, r8vec, '  Random R8VEC:' )

  end do

  return
end
subroutine r8vec_uniform_01_sorted1_test ( )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01_SORTED1_TEST tests R8VEC_UNIFORM_01_SORTED1_TEST,
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_UNIFORM_01_SORTED1_TEST:'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_01_SORTED1 generates a vector of N random'
  write ( *, '(a)' ) '  values in ascending sorted order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate several examples:'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10
    call r8vec_uniform_01_sorted1 ( n, seed, r8vec )
    call r8vec_transpose_print ( n, r8vec, '  R8VEC:' )
  end do

  return
end
subroutine r8vec_uniform_01_sorted2_test ( )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01_SORTED2_TEST tests R8VEC_UNIFORM_01_SORTED2_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_UNIFORM_01_SORTED2_TEST:'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_01_SORTED2 generates a vector of N random'
  write ( *, '(a)' ) '  values in ascending sorted order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate several examples:'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10
    call r8vec_uniform_01_sorted2 ( n, seed, r8vec )
    call r8vec_transpose_print ( n, r8vec, '  R8VEC:' )
  end do

  return
end
