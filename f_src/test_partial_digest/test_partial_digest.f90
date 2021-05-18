function i4_uniform_ab ( a, b, seed )

!*****************************************************************************80
!
!! I4_UNIFORM_AB returns a scaled pseudorandom I4 between A and B.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ) value.
!
!    The pseudorandom number will be scaled to be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) I4_UNIFORM_AB, a number between A and B.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  real ( kind = 4 ) r
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) value

  if ( seed == 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r = real ( seed, kind = 4 ) * 4.656612875E-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
  r = ( 1.0E+00 - r ) * ( real ( min ( a, b ), kind = 4 ) - 0.5E+00 ) & 
    +             r   * ( real ( max ( a, b ), kind = 4 ) + 0.5E+00 )
!
!  Use rounding to convert R to an integer between A and B.
!
  value = nint ( r, kind = 4 )

  value = max ( value, min ( a, b ) )
  value = min ( value, max ( a, b ) )

  i4_uniform_ab = value

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
subroutine i4vec_distances ( k, locate, d )

!*****************************************************************************80
!
!! I4VEC_DISTANCES computes a pairwise distance table.
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
!
!    Input, integer ( kind = 4 ) LOCATE(K), the obect locations.
!
!    Output, integer ( kind = 4 ) D(K*(K-1)/2), the pairwise distances.
!
  implicit none

  integer ( kind = 4 ) k

  integer ( kind = 4 ) d(k*(k-1)/2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) locate(k)
 
  l = 0
  do i = 1, k
    do j = i + 1, k
      l = l + 1
      d(l) = abs ( locate(i) - locate(j) )
    end do
  end do

  return
end
subroutine i4vec_distances_test ( )

!*****************************************************************************80
!
!! I4VEC_DISTANCES_TEST tests I4VEC_DISTANCES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 January 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5;

  integer ( kind = 4 ), allocatable :: d(:)
  integer ( kind = 4 ) :: locate(5) = (/ 0, 3, 10, 20, 100 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_DISTANCES_TEST'
  write ( *, '(a)' )  '  I4VEC_DISTANCES computes the pairwise distances'
  write ( *, '(a)' )  '  between elements of an I4VEC.'

  allocate ( d(1:n*(n-1)/2) )

  call i4vec_distances ( n, locate, d )

  call i4vec_print ( n, locate, '  Locations:' )
  call i4vec_print ( n * ( n - 1 ) / 2, d, '  Distances:' )

  deallocate ( d )

  return
end
subroutine i4vec_heap_d ( n, a )

!*****************************************************************************80
!
!! I4VEC_HEAP_D reorders an I4VEC into an descending heap.
!
!  Discussion:
!
!    An I4VEC is a vector of integer values.
!
!    A descending heap is an array A with the property that, for every index J,
!    A(J) >= A(2*J) and A(J) >= A(2*J+1), (as long as the indices
!    2*J and 2*J+1 are legal).
!
!                  A(1)
!                /      \
!            A(2)         A(3)
!          /     \        /  \
!      A(4)       A(5)  A(6) A(7)
!      /  \       /   \
!    A(8) A(9) A(10) A(11)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the input array.
!
!    Input/output, integer ( kind = 4 ) A(N).
!    On input, an unsorted array.
!    On output, the array has been reordered into a heap.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifree
  integer ( kind = 4 ) key
  integer ( kind = 4 ) m
!
!  Only nodes N/2 down to 1 can be "parent" nodes.
!
  do i = n / 2, 1, -1
!
!  Copy the value out of the parent node.
!  Position IFREE is now "open".
!
    key = a(i)
    ifree = i

    do
!
!  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
!  IFREE.  (One or both may not exist because they exceed N.)
!
      m = 2 * ifree
!
!  Does the first position exist?
!
      if ( n < m ) then
        exit
      end if
!
!  Does the second position exist?
!
      if ( m + 1 <= n ) then
!
!  If both positions exist, take the larger of the two values,
!  and update M if necessary.
!
        if ( a(m) < a(m+1) ) then
          m = m + 1
        end if

      end if
!
!  If the large descendant is larger than KEY, move it up,
!  and update IFREE, the location of the free position, and
!  consider the descendants of THIS position.
!
      if ( a(m) <= key ) then
        exit
      end if

      a(ifree) = a(m)
      ifree = m

    end do
!
!  Once there is no more shifting to do, KEY moves into the free spot IFREE.
!
    a(ifree) = key

  end do

  return
end
subroutine i4vec_heap_d_test ( )

!*****************************************************************************80
!
!! I4VEC_HEAP_D_TEST tests I4VEC_HEAP_D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_HEAP_D_TEST'
  write ( *, '(a)' ) '  For an I4VEC,'
  write ( *, '(a)' ) '  I4VEC_HEAP_D puts into descending heap form.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  do i = 1, n
    a(i) = i4_uniform_ab ( 0, n, seed )
  end do

  call i4vec_print ( n, a, '  Unsorted array:' )

  call i4vec_heap_d ( n, a )

  call i4vec_print ( n, a, '  Descending heap form:' )

  return
end
subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC, with an optional title.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) trim ( title )
  end if
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(i8,i10)' ) i, a(i)
  end do

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
subroutine i4vec_sort_heap_a ( n, a )

!*****************************************************************************80
!
!! I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
!
!  Discussion:
!
!    An I4VEC is a vector of integer values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input/output, integer ( kind = 4 ) A(N).
!    On input, the array to be sorted;
!    On output, the array has been sorted.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) t

  if ( n <= 1 ) then
    return
  end if
!
!  1: Put A into descending heap form.
!
  call i4vec_heap_d ( n, a )
!
!  2: Sort A.
!
!  The largest object in the heap is in A(1).
!  Move it to position A(N).
!
  t    = a(1)
  a(1) = a(n)
  a(n) = t
!
!  Consider the diminished heap of size N1.
!
  do n1 = n - 1, 2, -1
!
!  Restore the heap structure of A(1) through A(N1).
!
    call i4vec_heap_d ( n1, a )
!
!  Take the largest object from A(1) and move it to A(N1).
!
    t     = a(1)
    a(1)  = a(n1)
    a(n1) = t

  end do

  return
end
subroutine i4vec_sort_heap_a_test ( )

!*****************************************************************************80
!
!! I4VEC_SORT_HEAP_A_TEST tests I4VEC_SORT_HEAP_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_SORT_HEAP_A_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_HEAP_A ascending sorts an I4VEC,'

  seed = 123456789

  do i = 1, n
    a(i) = i4_uniform_ab ( 0, 3 * n, seed )
  end do

  call i4vec_print ( n, a, '  Unsorted:' )

  call i4vec_sort_heap_a ( n, a )

  call i4vec_print ( n, a, '  Ascending sorted:' )

  return
end
subroutine ksub_random ( n, k, seed, a )

!*****************************************************************************80
!
!! KSUB_RANDOM selects a random subset of size K from a set of size N.
!
!  Discussion:
!
!    Consider the set A(1:N) = 1, 2, ... N.  
!    Choose a random index I1 between 1 and N, and swap items A(1) and A(I1).
!    Choose a random index I2 between 2 and N, and swap items A(2) and A(I2).
!    repeat K times.
!    A(1:K) is your random K-subset.
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
!    Input, integer ( kind = 4 ) N, the size of the set from which subsets
!    are drawn.
!
!    Input, integer ( kind = 4 ) K, number of elements in desired subsets.
!    1 <= K <= N.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) A(K), the indices of the randomly
!    chosen elements.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) t
!
!  Let B index the set.
!
  do i = 1, n
    b(i) = i
  end do
!
!  Choose item 1 from N things,
!  choose item 2 from N-1 things,
!  choose item K from N-K+1 things.
!
  do i = 1, k

    j = i4_uniform_ab ( i, n, seed )

    t    = b(i)
    b(i) = b(j)
    b(j) = t

  end do
!
!  Copy the first K elements.
!
  a(1:k) = b(1:k)

  return
end
subroutine ksub_random_test ( )

!*****************************************************************************80
!
!! KSUB_RANDOM_TEST tests KSUB_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 June 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_RANDOM_TEST'
  write ( *, '(a)' ) '  KSUB_RANDOM generates a random K subset of an N set.'
  write ( *, '(a,i8)' ) '  Set size is N =    ', n
  write ( *, '(a,i8)' ) '  Subset size is K = ', k
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10

    call ksub_random ( n, k, seed, a )

    write ( *, '(2x,8i3)' ) a(1:k)

  end do
 
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
subroutine example_partial_digest_test ( )

!*****************************************************************************80
!
!! example_partial_digest_test tests TEST_PARTIAL_DIGEST.
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'example_partial_digest_test:'
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
!  Free memory.
!
  deallocate ( d )
  deallocate ( locate )

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
