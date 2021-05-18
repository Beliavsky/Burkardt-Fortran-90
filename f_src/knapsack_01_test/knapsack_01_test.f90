program main

!*****************************************************************************80
!
!! KNAPSACK_01_TEST tests the KNAPSACK_01 library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KNAPSACK_01_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the KNAPSACK_01 library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KNAPSACK_01_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 seeks a solution of the 0/1 Knapsack problem.
!
!  Discussion:
!
!    In the 0/1 knapsack problem, a knapsack of capacity C is given,
!    as well as N items, with the I-th item of weight W(I).
!
!    A selection is "acceptable" if the total weight is no greater than C.
!
!    It is desired to find an optimal acceptable selection, that is,
!    an acceptable selection such that there is no acceptable selection
!    of greater weight.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) s(n)
  integer ( kind = 4 ) t
  integer ( kind = 4 ), dimension ( n ) :: w = (/ &
    16, 17, 23, 24, 39, 40 /)

  c = 100

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a,i4)' ) '  Knapsack maximum capacity is ', c
  write ( *, '(a)' ) '  Come as close as possible to filling the knapsack.'

  call knapsack_01 ( n, w, c, s )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   # 0/1  Weight'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i2,2x,i1,2x,i4)' ) i, s(i), w(i)
  end do
  t = dot_product ( s, w )
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Total: ', t

  return
end
