program main

!*****************************************************************************80
!
!! MAIN is the main program for CHANGE_MAKING_TEST.
!
!  Discussion:
!
!    CHANGE_MAKING_TEST tests the CHANGE_MAKING library.
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
  write ( *, '(a)' ) 'CHANGE_MAKING_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the CHANGE_MAKING library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHANGE_MAKING_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! CHANGE_MAKING_TEST01 lists the problem data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: coin_value_list_num = 33
  integer ( kind = 4 ), parameter :: test_num = 7

  integer ( kind = 4 ) coin_num
  integer ( kind = 4 ), dimension ( test_num ) :: coin_num_list = (/ &
    3, &
    5, &
    6, &
    7, &
    3, &
    6, &
    3 /)
  integer ( kind = 4 ) coin_offset
  integer ( kind = 4 ), dimension ( test_num ) :: coin_offset_list = (/ &
     1, &
     4, &
     9, &
    15, &
    22, &
    25, &
    31 /)
  integer ( kind = 4 ), allocatable :: coin_value(:)
  integer ( kind = 4 ), dimension ( coin_value_list_num ) :: coin_value_list = (/ &
     5,  9, 13, &
     1,  4,  5,  8, 11, &
     1,  5, 10, 25, 50, 100, &
     1,  2,  6, 12, 24,  48,  60, &
     1,  3,  4, &
    16, 17, 23, 24, 39,  40, &
     6,  9, 20 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) target
  integer ( kind = 4 ), dimension ( test_num ) :: target_list = (/ &
    19, &
    29, &
    96, &
    96, &
     6, &
   100, &
    43 /)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  List the problem data.'

  do test = 1, test_num

    coin_num = coin_num_list(test)
    coin_offset = coin_offset_list(test)
    allocate ( coin_value(1:coin_num) )
    coin_value(1:coin_num) = coin_value_list(coin_offset:coin_offset+coin_num-1)
    target = target_list(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,i1,a)' ) '  Test ', test, ':'
    write ( *, '(a,i4)' ) '  Number of coins = ', coin_num
    write ( *, '(a)' ) '  Values = '
    do i = 1, coin_num
      write ( *, '(2x,i4)' ) coin_value(i)
    end do
    write ( *, '(a)' ) ''
    write ( *, '(a,i4)' ) '  Target = ', target

    deallocate ( coin_value )

  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! CHANGE_MAKING_TEST02 uses CHANGE_MAKING_LIST on the problems.
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

  integer ( kind = 4 ), parameter :: coin_value_list_num = 33
  integer ( kind = 4 ), parameter :: test_num = 7

  integer ( kind = 4 ), allocatable :: a(:)
  integer ( kind = 4 ) coin_num
  integer ( kind = 4 ), dimension ( test_num ) :: coin_num_list = (/ &
    3, &
    5, &
    6, &
    7, &
    3, &
    6, &
    3 /)
  integer ( kind = 4 ) coin_offset
  integer ( kind = 4 ), dimension ( test_num ) :: coin_offset_list = (/ &
     1, &
     4, &
     9, &
    15, &
    22, &
    25, &
    31 /)
  integer ( kind = 4 ), allocatable :: coin_value(:)
  integer ( kind = 4 ), dimension ( coin_value_list_num ) :: coin_value_list = (/ &
     5,  9, 13, &
     1,  4,  5,  8, 11, &
     1,  5, 10, 25, 50, 100, &
     1,  2,  6, 12, 24,  48,  60, &
     1,  3,  4, &
    16, 17, 23, 24, 39,  40, &
     6,  9, 20 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) target
  integer ( kind = 4 ), dimension ( test_num ) :: target_list = (/ &
    19, &
    29, &
    96, &
    96, &
     6, &
   100, &
    43 /)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  CHANGE_MAKING LIST computes A(T), the smallest number'
  write ( *, '(a)' ) '  of coins needed to form a given sum T, by computing'
  write ( *, '(a)' ) '  the list A(0) through A(T).'

  do test = 1, test_num

    coin_num = coin_num_list(test)
    coin_offset = coin_offset_list(test)
    allocate ( coin_value(1:coin_num) )
    coin_value(1:coin_num) = coin_value_list(coin_offset:coin_offset+coin_num-1)
    target = target_list(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,i1,a)' ) '  Test ', test, ':'
    write ( *, '(a,i4)' ) '  Number of coins = ', coin_num
    write ( *, '(a)' ) '  Values = '
    do i = 1, coin_num
      write ( *, '(2x,i4)' ) coin_value(i)
    end do
    write ( *, '(a)' ) ''
    write ( *, '(a,i4)' ) '  Target = ', target

    allocate ( a(0:target) )

    call change_making_list ( coin_num, coin_value, target, a )

    write ( *, '(a)' ) ''
    do i = 0, target
      if ( a(i) == i4_huge ) then
        write ( *, '(2x,i4,a)' ) i, '  Not possible!'
      else
        write ( *, '(2x,i4,2x,i4)' ) i, a(i)
      end if
    end do

    deallocate ( a )
    deallocate ( coin_value )

  end do


  return
end

