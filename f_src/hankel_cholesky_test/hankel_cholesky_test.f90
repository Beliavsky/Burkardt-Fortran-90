program main

!*****************************************************************************80
!
!! MAIN is the main program for HANKEL_CHOLESKY_TEST.
!
!  Discussion:
!
!    HANKEL_CHOLESKY_TEST tests HANKEL_CHOLESKY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 January 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HANKEL_CHOLESKY_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the HANKEL_CHOLESKY library.'

  call hankel_cholesky_upper_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HANKEL_CHOLESKY_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine hankel_cholesky_upper_test ( )

!*****************************************************************************80
!
!! HANKEL_CHOLESKY_UPPER_TEST tests HANKEL_CHOLESKY_UPPER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 January 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) flag
  real ( kind = 8 ) h(n,n)
  real ( kind = 8 ) hanti(2*n-1)
  real ( kind = 8 ) l(n,n)
  real ( kind = 8 ) lii(n)
  real ( kind = 8 ) liim1(n-1)
  real ( kind = 8 ) r1(n,n)
  real ( kind = 8 ) r2(n,n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HANKEL_CHOLESKY_UPPER_TEST'
  write ( *, '(a)' ) '  HANKEL_CHOLESKY_UPPER is given a Hankel matrix H and'
  write ( *, '(a)' ) '  computes an upper triangular matrix R such that'
  write ( *, '(a)' ) '  H = R'' * R'
!
!  Get a Hankel matrix that is positive definite.
!
  seed = 123456789
  call r8vec_uniform_01 ( n, seed, lii )
  call r8vec_uniform_01 ( n - 1, seed, liim1 )
  call hankel_pds_cholesky_lower ( n, lii, liim1, l )
  h = matmul ( l, transpose ( l ) )
  call r8mat_print ( n, n, h, '  The Hankel matrix H:' )
!
!  Compute R using R8MAT_CHOLESKY_FACTOR_UPPER.
!
  call r8mat_cholesky_factor_upper ( n, h, r1, flag )
  if ( flag /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) ' R8MAT_CHOLESKY_FACTOR_UPPER says H is not positive definite.'
  else
    call r8mat_print ( n, n, r1, '  R computed by R8MAT_CHOLESKY_FACTOR_UPPER:' )
  end if
!
!  Compute R using HANKEL_CHOLESKY.
!
  hanti(1:n) = h(1:n,1)
  hanti(n+1:2*n-1) = h(n,2:n)

  call hankel_cholesky_upper ( n, hanti, r2 )
  call r8mat_print ( n, n, r2, '  R computed by HANKEL_CHOLESKY:' )

  return
end
