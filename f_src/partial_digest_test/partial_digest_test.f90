program main

!*****************************************************************************80
!
!! MAIN is the main program for PARTIAL_DIGEST_TEST.
!
!  Discussion:
!
!    PARTIAL_DIGEST_TEST tests the PARTIAL_DIGEST library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 January 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PARTIAL_DIGEST_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the PARTIAL_DIGEST library.'

  call find_distances_test ( )
  call i4_uniform_ab_test ( )
  call i4vec_max_last_test ( )
  call i4vec_print_test ( )
  call partial_digest_recur_test01 ( )
  call partial_digest_recur_test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PARTIAL_DIGEST_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine find_distances_test ( ) 

!*****************************************************************************80
!
!! FIND_DISTANCES_TEST tests FIND_DISTANCES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 January 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i4vec_max_last
  integer ( kind = 4 ) l(n*(n-1)/2)
  integer ( kind = 4 ) l_length
  integer ( kind = 4 ) l_max
  logical success
  integer ( kind = 4 ) x(n)
  integer ( kind = 4 ) x_length
  integer ( kind = 4 ) y

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FIND_DISTANCES_TEST:'
  write ( *, '(a)' ) '  FIND_DISTANCES takes a candidate location Y'
  write ( *, '(a)' ) '  and determines whether its distance to each point'
  write ( *, '(a)' ) '  in the X array is listed in the L array.'

  l_length = n * ( n - 1 ) / 2
  l = (/  13, 15, 38, 90, 2, 25, 77, 23, 75, 52 /) 
  call i4vec_print ( l_length, l, '  Initial L array:' )

  l_max = i4vec_max_last ( l_length, l )
  l_length = l_length - 1

  x(1) = 0
  x(2) = l_max
  x_length = 2
!
!  Solution is X = (/ 0, 13, 15, 38, 90 /) or (/ 0, 52, 75, 77, 90 /)
!  So Y = 13, 15, 38, 52, 75 or 77 will be acceptable.
!
  y = i4vec_max_last ( l_length, l )
  call find_distances ( l_length, l, x_length, x, y, success )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Consider Y = ', y
  write ( *, '(a)' ) ''
  if ( success ) then
    write ( *, '(a)' ) '  This Y is acceptable'
    l_length = l_length - x_length
    x_length = x_length + 1
    x(x_length) = y
    call i4vec_print ( x_length, x, '  New X array:' )
    call i4vec_print ( l_length, l, '  New L array:' )
  else
    write ( *, '(a)' ) '  This Y is not acceptable'
  end if

  y = 35
  call find_distances ( l_length, l, x_length, x, y, success )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Consider Y = ', y
  write ( *, '(a)' ) ''
  if ( success ) then
    write ( *, '(a)' ) '  This Y is acceptable'
    l_length = l_length - x_length
    x_length = x_length + 1
    x(x_length) = y
    call i4vec_print ( x_length, x, '  New X array:' )
    call i4vec_print ( l_length, l, '  New L array:' )
  else
    write ( *, '(a)' ) '  This Y is not acceptable'
  end if

  return
end
subroutine i4_uniform_ab_test ( )

!*****************************************************************************80
!
!! I4_UNIFORM_AB_TEST tests I4_UNIFORM_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: a = -100
  integer ( kind = 4 ), parameter :: b = 200
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  I4_UNIFORM_AB computes pseudorandom values '
  write ( *, '(a)' ) '  in an interval [A,B].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  The lower endpoint A = ', a
  write ( *, '(a,i12)' ) '  The upper endpoint B = ', b
  write ( *, '(a,i12)' ) '  The initial seed is ', seed
  write ( *, '(a)' ) ' '

  do i = 1, 20

    j = i4_uniform_ab ( a, b, seed )

    write ( *, '(2x,i8,2x,i8)' ) i, j

  end do

  return
end
subroutine i4vec_max_last_test ( )

!*****************************************************************************80
!
!! I4VEC_MAX_LAST_TEST tests I4VEC_MAX_LAST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 January 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) i4vec_max_last
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(n)
  integer ( kind = 4 ) x_max

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_MAX_LAST_TEST'
  write ( *, '(a)' ) '  I4VEC_MAX_LAST identifies the largest element in an'
  write ( *, '(a)' ) '  I4VEC, and moves it to the final entry.'

  seed = 123456789

  do i = 1, n
    x(i) = i4_uniform_ab ( 1, 30, seed )
  end do

  call i4vec_print ( n, x, '  Input vector:' )

  x_max = i4vec_max_last ( n, x )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Maximum:                  ', x_max

  call i4vec_print ( n, x, '  Output vector:' )

  return
end
subroutine i4vec_print_test ( )

!*****************************************************************************80
!
!! I4VEC_PRINT_TEST tests I4VEC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), dimension ( n ) :: a = (/ &
    91, 92, 93, 94 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_PRINT_TEST'
  write ( *, '(a)' ) '  I4VEC_PRINT prints an I4VEC'

  call i4vec_print ( n, a, '  The I4VEC:' )

  return
end
subroutine partial_digest_recur_test01 ( )

!*****************************************************************************80
!
!! PARTIAL_DIGEST_RECUR_TEST01 tests PARTIAL_DIGEST_RECUR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: nn2 = ( n * ( n - 1 ) ) / 2
!
!  Set the distance array.
!
  integer ( kind = 4 ), dimension ( ((n-1)*n)/2 ) :: dist = (/ &
    2, 2, 3, 3, 4, 5, 6, 7, 8, 10 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PARTIAL_DIGEST_RECUR_TEST01'
  write ( *, '(a)' ) '  PARTIAL_DIGEST_RECUR generates solutions to the partial'
  write ( *, '(a)' ) '  digest problem, using recursion'

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number of objects to place is N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The original placement was 0,3,6,8,10.'
  write ( *, '(a)' ) '  These placements generate the following distances:'

  call i4vec_print ( nn2, dist, '  Distance array:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  PARTIAL_DIGEST_RECUR may recover the original placements'
  write ( *, '(a)' ) '  from the pairwise distances.  It may also find other'
  write ( *, '(a)' ) '  placements that have the same distance array.'

  call partial_digest_recur ( n, dist )

  return
end
subroutine partial_digest_recur_test02 ( )

!*****************************************************************************80
!
!! PARTIAL_DIGEST_RECUR_TEST02 considers tests from a library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: d(:)
  integer ( kind = 4 ) dmax
  integer ( kind = 4 ) k
  integer ( kind = 4 ), allocatable :: locate ( : )
  integer ( kind = 4 ) seed

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PARTIAL_DIGEST_RECUR_TEST02:'
  write ( *, '(a)' ) '  PARTIAL_DIGEST_RECUR generates solutions to the partial'
  write ( *, '(a)' ) '  digest problem, using recursion'
  write ( *, '(a)' ) '  TEST_PARTIAL_DIGEST creates test problems for the'
  write ( *, '(a)' ) '  partial digest problem.'
!
!  Request a sample problem.
!
  k = 6
  dmax = 20
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of nodes = ', k
  write ( *, '(a,i4)' ) '  Maximum distance = ', dmax

  allocate ( locate(1:k) )
  allocate ( d(1:k*(k-1)/2) )
  
  call test_partial_digest ( k, dmax, seed, locate, d )
!
!  Sort the data.
!
  call i4vec_sort_heap_a ( k, locate )
  call i4vec_sort_heap_a ( k*(k-1)/2, d )
!
!  Print the data.
!
  call i4vec_print ( k, locate, '  Locations:' )
  call i4vec_print ( k * ( k - 1 ) / 2, d, '  Distances:' )
!
!  Solve the problem.
!
  call partial_digest_recur ( k, d )
!
!  Free memory.
!
  deallocate ( d )
  deallocate ( locate )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PARTIAL_DIGEST_RECUR_TEST02:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine test_partial_digest ( k, dmax, seed, locate, d )

!*****************************************************************************80
!
!! TEST_PARTIAL_DIGEST returns a partial digest test problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the number of objects.
!    K must be at least 2.
!
!    Input, integer ( kind = 4 ) DMAX, the maximum possible distance.
!    DMAX must be at least K-1.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, integer ( kind = 4 ) LOCATE(K), the obect locations.
!
!    Output, integer ( kind = 4 ) D(K*(K-1)/2), the pairwise distances.
!
  implicit none

  integer ( kind = 4 ) k

  integer ( kind = 4 ) d(k*(k-1)/2)
  integer ( kind = 4 ) dmax
  integer ( kind = 4 ) locate(k)
  integer ( kind = 4 ) seed
!
!  Check input.
!
  if ( k < 2 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TEST_PARTIAL_DIGEST - Fatal error!'
    write ( *, '(a)' ) '  Input K < 2.'
    stop 1
  end if

  if ( dmax < k - 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TEST_PARTIAL_DIGEST - Fatal error!'
    write ( *, '(a)' ) '  DMAX < K - 1.'
    stop 1
  end if
!
!  Select LOCATE, which is a random subset of the integers 0 through DMAX.
!
  call ksub_random ( dmax - 1, k - 2, seed, locate(2) )
  locate(1) = 0
  locate(k) = dmax
!
!  Compute K*(K+1)/2 pairwise distances.
!
  call i4vec_distances ( k, locate, d )
 
  return
end
