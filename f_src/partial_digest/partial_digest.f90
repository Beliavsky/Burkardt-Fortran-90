subroutine find_distances ( l_length, l, x_length, x, y, success )

!*****************************************************************************80
!
!! FIND_DISTANCES determines if the "free" distances include every ||X(I)-Y||.
!
!  Discussion:
!
!    This routine is given a candidate point Y, a set of placed points
!    X(1:X_LENGTH), and a list of unused or "free" distances in
!    L(1:L_LENGTH).  The routine seeks to find in L a copy of the
!    distance from Y to each X.
!
!    If so, then the L array is reordered so that entries
!    L(L_LENGTH-X_LENGTH+1:L_LENGTH) contain theses distances.
!
!    In other words, Y can be added into X, and L_LENGTH reduced to
!    L_LENGTH-X_LENGTH.
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
!  Reference:
!
!    Pavel Pevzner,
!    Computational Molecular Biology,
!    MIT Press, 2000,
!    ISBN: 0-262-16197-4,
!    LC: QH506.P47.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) L_LENGTH, the length of the array.
!
!    Input/output, integer ( kind = 4 ) L(L_LENGTH), the array.  On output,
!    some entries have been shuffled.  In particular, if SUCCESS is TRUE,
!    the entries L(L_LENGTH-X_LENGTH+1:L_LENGTH) contain the distances
!    of X(1:X_LENGTH) to Y.
!
!    Input, integer ( kind = 4 ) X_LENGTH, the number of entries in X.
!
!    Input, integer ( kind = 4 ) X(X_LENGTH), the number of points
!    already accepted.
!
!    Input, integer ( kind = 4 ) Y, a new point that we are considering.
!
!    Output, logical SUCCESS, is TRUE if the entries of L included
!    the values of the distance of Y to each entry of X.
!
  implicit none

  integer ( kind = 4 ) l_length
  integer ( kind = 4 ) x_length

  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l(l_length)
  integer ( kind = 4 ) l2_length
  logical success
  integer ( kind = 4 ) x(x_length)
  integer ( kind = 4 ) y

  l2_length = l_length

  do i = 1, x_length

    d = abs ( x(i) - y )

    success = .false.

    do j = 1, l2_length

      if ( l(j) == d ) then
        l(j) = l(l2_length)
        l(l2_length) = d
        l2_length = l2_length - 1
        success = .true.
        exit
      end if

    end do

    if ( .not. success ) then
      return
    end if

  end do

  success = .true.

  return
end
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
    write ( *, '(a)' ) ' '
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
subroutine i4vec_heap_d ( n, a )

!*****************************************************************************80
!
!! I4VEC_HEAP_D reorders an I4VEC into an descending heap.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
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
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
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
function i4vec_max_last ( l_length, l )

!*****************************************************************************80
!
!! I4VEC_MAX_LAST moves the maximum entry of an I4VEC to the last position.
!
!  Discussion:
!
!    This routine finds the largest entry in an array and moves
!    it to the end of the array.
!
!    If we ignore this last array entry, then the effect is the same
!    as "deleting" the maximum entry from the array.
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
!  Reference:
!
!    Pavel Pevzner,
!    Computational Molecular Biology,
!    MIT Press, 2000,
!    ISBN: 0-262-16197-4,
!    LC: QH506.P47.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) L_LENGTH, the length of the array.
!
!    Input, integer ( kind = 4 ) L(L_LENGTH), the array.  On output,
!    the maximum entry has been "deleted", that is, the array has
!    been shifted so that this entry occurs at the end.
!
!    Output, integer ( kind = 4 ) I4VEC_MAX_LAST, the maximum entry in the
!    input array.
!
  implicit none

  integer ( kind = 4 ) l_length

  integer ( kind = 4 ) i4vec_max_last
  integer ( kind = 4 ) l(l_length)
  integer ( kind = 4 ) max_index(1)
  integer ( kind = 4 ) value

  max_index = maxloc ( l(1:l_length) )
  value = l(max_index(1))
  l(max_index(1):l_length-1) = l(max_index(1)+1:l_length)
  l(l_length) = value

  i4vec_max_last = value

  return
end
subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
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
!    28 November 2000
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,2x,i12)' ) i, a(i)
  end do

  return
end
subroutine i4vec_sort_heap_a ( n, a )

!*****************************************************************************80
!
!! I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
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
    t    = a(1)
    a(1) = a(n1)
    a(n1) = t

  end do

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
subroutine partial_digest_recur ( n, l )

!*****************************************************************************80
!
!! PARTIAL_DIGEST_RECUR uses recursion on the partial digest problem.
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
!  Reference:
!
!    Pavel Pevzner,
!    Computational Molecular Biology,
!    MIT Press, 2000,
!    ISBN: 0-262-16197-4,
!    LC: QH506.P47.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Input, integer ( kind = 4 ) L((N*(N-1))/2), the distances between all pairs
!    of distinct nodes.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i4vec_max_last
  integer ( kind = 4 ) l((n*(n-1))/2)
  integer ( kind = 4 ) l_length
  integer ( kind = 4 ) width
  integer ( kind = 4 ) x(n)
  integer ( kind = 4 ) x_length
!
!  How long is L?
!
  l_length = ( n * ( n - 1 ) ) / 2
!
!  Find WIDTH, the largest element of L, and move it to the last position.
!
  width = i4vec_max_last ( l_length, l )
!
!  Think of L as being 1 entry shorter.
!
  l_length = l_length - 1
!
!  Using WIDTH, set the first two entries of X.
!
  x(1) = 0
  x(2) = width
  x_length = 2
!
!  Begin recursive operation.
!
  call place ( l_length, l, x_length, x )

  return
end
recursive subroutine place ( l_length, l, x_length, x )

!*****************************************************************************80
!
!! PLACE tries to place the next point for the partial digest problem.
!
!  Discussion:
!
!    Note that this is a recursive subroutine.  A solution to the
!    partial digest problem is sought by calling this routine repeatedly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Pavel Pevzner,
!    Computational Molecular Biology,
!    MIT Press, 2000,
!    ISBN: 0-262-16197-4,
!    LC: QH506.P47.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) L_LENGTH, the number of entries in L.
!
!    Input/output, integer ( kind = 4 ) L(L_LENGTH), the array of distances.

!    Input/output, integer ( kind = 4 ) X_LENGTH, the number of entries in X.
!
!    Input/output, integer ( kind = 4 ) X(X_LENGTH), the current partial solution.
!
  implicit none

  integer ( kind = 4 ) l_length
  integer ( kind = 4 ) x_length

  integer ( kind = 4 ) i4vec_max_last
  integer ( kind = 4 ) l(l_length)
  integer ( kind = 4 ) l_length2
  logical success
  integer ( kind = 4 ) x(x_length)
  integer ( kind = 4 ) y
!
!  Are we done?
!
  if ( l_length <= 0 ) then
    call i4vec_print ( x_length, x, '  Solution:' )
    return
  end if
!
!  Find the maximum remaining distance.
!
  y = i4vec_max_last ( l_length, l )
!
!  We can add a point at Y if L contains all the distances from Y to
!  the current X's.
!
  call find_distances ( l_length, l, x_length, x, y, success )

  if ( success ) then
    l_length2 = l_length - x_length
    x_length = x_length + 1
    x(x_length) = y
    call place ( l_length2, l, x_length, x )
    x_length = x_length - 1
  end if
!
!  We must also consider the case where Y represents the distance
!  to X(2), not X(1).
!
  y = x(2) - y

  call find_distances ( l_length, l, x_length, x, y, success )

  if ( success ) then
    l_length2 = l_length - x_length
    x_length = x_length + 1
    x(x_length) = y
    call place ( l_length2, l, x_length, x )
    x_length = x_length - 1
  end if

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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
