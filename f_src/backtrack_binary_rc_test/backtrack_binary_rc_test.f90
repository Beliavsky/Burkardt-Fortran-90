program main

!*****************************************************************************80
!
!! MAIN is the main program for BACKTRACK_BINARY_RC_TEST.
!
!  Discussion:
!
!    BACKTRACK_BINARY_RC_TEST tests BACKTRACK_BINARY_RC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BACKTRACK_BINARY_RC_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BACKTRACK_BINARY_RC library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BACKTRACK_BINARY_RC_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 seeks a selection of binary powers that have a given sum.
!
!  Discussion:
!
!    We consider the binary powers 1, 2, 4, ... 2^(n-1).
!
!    We wish to select some of these powers, so that the sum is equal
!    to a given target value.  We are actually simply seeking the binary
!    representation of an integer.
!
!    A partial solution is acceptable if it is less than the target value.
!
!    We list the powers in descending order, so that the bactracking
!    procedure makes the most significant choices first, thus quickly
!    eliminating many unsuitable choices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8
  integer ( kind = 4 ), parameter :: test_num = 3

  integer ( kind = 4 ) call_num
  integer ( kind = 4 ) choice(n)
  integer ( kind = 4 ) factor
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n2
  logical reject
  integer ( kind = 4 ) result
  integer ( kind = 4 ) target
  integer ( kind = 4 ), dimension ( test_num ) :: targets = (/ 73, 299, -3 /)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Use BACKBIN_RC to find the binary expansion of'
  write ( *, '(a)' ) '  an integer between 0 and 255.'
  write ( *, '(a)' ) '  The choices are 0/1 for the 8 digits.'

  do test = 1, test_num

    target = targets(test)
    write ( *, '(a)' ) ' '
    write ( *, '(a,i4)' ) '  TARGET = ', target
    call_num = 0
    n2 = -1

    do

      call backbin_rc ( n, reject, n2, choice )
      call_num = call_num + 1

      if ( n2 == -1 ) then
        write ( *, '(a)' ) '  Termination without solution.'
        exit
      end if
!
!  Evaluate the integer determined by the choices.
!
      factor = 1
      do i = n, n2 + 1, -1
        factor = factor * 2
      end do

      result = 0
      do i = 1, n2
        result = result * 2 + choice(i)
      end do

      result = result * factor
!
!  If the integer is too big, then we reject it, and
!  all the related integers formed by making additional choices.
!
      reject = ( target < result )
!
!  If we hit the target, then in this case, we can exit because
!  the solution is unique.
!
      if ( result == target ) then
        exit
      end if

    end do

    write ( *, '(a,i6)' ) '  Number of calls = ', call_num
    write ( *, '(a,i10)' ) '  Binary search space = ', 2 ** n
    write ( *, '(2x,10i2)' ) choice(1:n)

  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 seeks a subset of a set of numbers which add to a given sum.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8

  integer ( kind = 4 ) call_num
  integer ( kind = 4 ) choice(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n2
  logical reject
  integer ( kind = 4 ) result
  integer ( kind = 4 ), parameter :: target = 53
  integer ( kind = 4 ), dimension ( n ) :: w = (/ &
    15, 22, 14, 26, 32, 9, 16, 8 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Use BACKBIN_RC to seek subsets of a set W'
  write ( *, '(a)' ) '  that sum to a given target value.'
  write ( *, '(a)' ) '  The choices are 0/1 to select each element of W.'

  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  TARGET = ', target
  write ( *, '(a)' ) ' '
  call_num = 0
  n2 = -1

  do

    call backbin_rc ( n, reject, n2, choice )
    call_num = call_num + 1

    if ( n2 == -1 ) then
      exit
    end if
!
!  Evaluate the partial sum.
!
    result = 0
    do i = 1, n2
      result = result + choice(i) * w(i)
    end do
!
!  If the sum is too big, then we reject it, and
!  all the related sums formed by making additional choices.
!
    reject = ( target < result )
!
!  If we hit the target, print out the information.
!
    if ( result == target .and. n2 == n ) then
      write ( *, '(2x,10i2)' ) choice(1:n)
    end if

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of calls = ', call_num
  write ( *, '(a,i10)' ) '  Binary search space = ', 2 ** n

  return
end
