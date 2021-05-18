program main

!*****************************************************************************80
!
!! MAIN is the main program for SOLVE_TEST.
!
!  Discussion:
!
!    SOLVE_TEST tests the SOLVE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SOLVE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SOLVE library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SOLVE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 demonstrates how a 3X3 linear system can be set up and solved.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  R8MAT_FS solves a linear system with Gauss elimination.'
!
!  Set the array.
!
  a(1,1) = 1.0
  a(1,2) = 2.0
  a(1,3) = 3.0

  a(2,1) = 4.0
  a(2,2) = 5.0
  a(2,3) = 6.0

  a(3,1) = 7.0
  a(3,2) = 8.0
  a(3,3) = 0.0
!
!  Set the right hand side.
!
  b(1) = 14.0
  b(2) = 32.0
  b(3) = 23.0
!
!  Request the solution of A*x=b.
!
  call r8mat_fs ( n, a, b, x )

  call r8vec_print ( n, x, '  Solution:' );

  return
end


