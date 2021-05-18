program main

!*****************************************************************************80
!
!! MAIN is the main program for snakes_and_ladders_test.
!
!  Discussion:
!
!    snakes_and_ladders_test tests snakes_and_ladders.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'snakes_and_ladders_test'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test snakes_and_ladders().'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'snakes_and_ladders_test'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests SPY_GE for the SNAKES matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(0:100,0:100)
  character ( len = 255 ) header
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  SNAKES sets up the snakes and ladders matrix.'
  write ( *, '(a)' ) '  SPY_GE generates a sparsity plot for a matrix stored'
  write ( *, '(a)' ) '  in general (GE) format.'

  call snakes ( a )
  header = 'snakes'

  m = 101
  n = 101
  call spy_ge ( m, n, a, header )

  return
end

