function i4_choose ( n, k )

!*****************************************************************************80
!
!! I4_CHOOSE computes the binomial coefficient C(N,K) as an I4.
!
!  Discussion:
!
!    The value is calculated in such a way as to avoid overflow and
!    roundoff.  The calculation is done in integer arithmetic.
!
!    The formula used is:
!
!      C(N,K) = N! / ( K! * (N-K)! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    ML Wolfson, HV Wright,
!    Algorithm 160:
!    Combinatorial of M Things Taken N at a Time,
!    Communications of the ACM,
!    Volume 6, Number 4, April 1963, page 161.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, K, are the values of N and K.
!
!    Output, integer ( kind = 4 ) I4_CHOOSE, the number of combinations of N
!    things taken K at a time.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mn
  integer ( kind = 4 ) mx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) value

  mn = min ( k, n - k )

  if ( mn < 0 ) then

    value = 0

  else if ( mn == 0 ) then

    value = 1

  else

    mx = max ( k, n - k )
    value = mx + 1

    do i = 2, mn
      value = ( value * ( mx + i ) ) / i
    end do

  end if

  i4_choose = value

  return
end
function i4_fall ( x, n )

!*****************************************************************************80
!
!! I4_FALL computes the falling factorial function [X]_N.
!
!  Discussion:
!
!    Note that the number of "injections" or 1-to-1 mappings from
!    a set of N elements to a set of M elements is [M]_N.
!
!    The number of permutations of N objects out of M is [M]_N.
!
!    Moreover, the Stirling numbers of the first kind can be used
!    to convert a falling factorial into a polynomial, as follows:
!
!      [X]_N = S^0_N + S^1_N * X + S^2_N * X^2 + ... + S^N_N X^N.
!
!    The formula used is:
!
!      [X]_N = X * ( X - 1 ) * ( X - 2 ) * ... * ( X - N + 1 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the falling factorial
!    function.
!
!    Input, integer ( kind = 4 ) N, the order of the falling factorial function.
!    If N = 0, FALL = 1, if N = 1, FALL = X.  Note that if N is
!    negative, a "rising" factorial will be computed.
!
!    Output, integer ( kind = 4 ) I4_FALL, the value of the falling
!    factorial function.
!
  implicit none

  integer ( kind = 4 ) arg
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_fall
  integer ( kind = 4 ) n
  integer ( kind = 4 ) value
  integer ( kind = 4 ) x

  value = 1

  arg = x

  if ( 0 < n ) then

    do i = 1, n
      value = value * arg
      arg = arg - 1
    end do

  else if ( n < 0 ) then

    do i = -1, n, -1
      value = value * arg
      arg = arg + 1
    end do

  end if

  i4_fall = value

  return
end
subroutine i4vec_concatenate ( n1, a, n2, b, c )

!*****************************************************************************80
!
!! I4VEC_CONCATENATE concatenates two I4VEC's.
!
!  Discussion:
!
!    An I4VEC is a vector of I4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N1, the number of entries in the first vector.
!
!    Input, integer ( kind = 4 ) A(N1), the first vector.
!
!    Input, integer ( kind = 4 ) N2, the number of entries in the second vector.
!
!    Input, integer ( kind = 4 ) B(N2), the second vector.
!
!    Output, integer ( kind = 4 ) C(N1+N2), the concatenation of A and B.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  integer ( kind = 4 ) a(n1)
  integer ( kind = 4 ) b(n2)
  integer ( kind = 4 ) c(n1+n2)

  c(   1:n1)    = a(1:n1)
  c(n1+1:n1+n2) = b(1:n2)

  return
end
subroutine i4vec_permute ( n, p, a )

!*****************************************************************************80
!
!! I4VEC_PERMUTE permutes an I4VEC in place.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    This routine permutes an array of integer "objects", but the same
!    logic can be used to permute an array of objects of any arithmetic
!    type, or an array of objects of any complexity.  The only temporary
!    storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (   2,   4,   5,   1,   3 )
!      A = (   1,   2,   3,   4,   5 )
!
!    Output:
!
!      A    = (   2,   4,   5,   1,   3 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects.
!
!    Input, integer ( kind = 4 ) P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.
!
!    Input/output, integer ( kind = 4 ) A(N), the array to be permuted.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) a_temp
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) istart
  integer ( kind = 4 ) p(n)

  call perm_check1 ( n, p )
!
!  Search for the next element of the permutation that has not been used.
!
  do istart = 1, n

    if ( p(istart) < 0 ) then

      cycle

    else if ( p(istart) == istart ) then

      p(istart) = - p(istart)
      cycle

    else

      a_temp = a(istart)
      iget = istart
!
!  Copy the new value into the vacated entry.
!
      do

        iput = iget
        iget = p(iget)

        p(iput) = - p(iput)

        if ( iget < 1 .or. n < iget ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'I4VEC_PERMUTE - Fatal error!'
          write ( *, '(a)' ) '  A permutation index is out of range.'
          write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
          stop 1
        end if

        if ( iget == istart ) then
          a(iput) = a_temp
          exit
        end if

        a(iput) = a(iget)

      end do

    end if

  end do
!
!  Restore the signs of the entries.
!
  p(1:n) = - p(1:n)

  return
end
subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
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
!    02 May 2010
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
    write ( *, '(2x,i8,a,2x,i12)' ) i, ':', a(i)
  end do

  return
end
subroutine i4vec_sort_heap_index_a ( n, a, indx )

!*****************************************************************************80
!
!! I4VEC_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      A(INDX(1:N)) is sorted,
!
!    or explicitly, by the call
!
!      call i4vec_permute ( n, indx, a )
!
!    after which A(1:N) is sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, integer ( kind = 4 ) A(N), an array to be index-sorted.
!
!    Output, integer ( kind = 4 ) INDX(N), the sort index.  The
!    I-th element of the sorted array is A(INDX(I)).
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indxt
  integer ( kind = 4 ) ir
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) value

  if ( n < 1 ) then
    return
  end if

  do i = 1, n
    indx(i) = i
  end do

  if ( n == 1 ) then
    return
  end if

  l = n / 2 + 1
  ir = n

  do

    if ( 1 < l ) then

      l = l - 1
      indxt = indx(l)
      value = a(indxt)

    else

      indxt = indx(ir)
      value = a(indxt)
      indx(ir) = indx(1)
      ir = ir - 1

      if ( ir == 1 ) then
        indx(1) = indxt
        exit
      end if

    end if

    i = l
    j = l + l

    do while ( j <= ir )

      if ( j < ir ) then
        if ( a(indx(j)) < a(indx(j+1)) ) then
          j = j + 1
        end if
      end if

      if ( value < a(indx(j)) ) then
        indx(i) = indx(j)
        i = j
        j = j + j
      else
        j = ir + 1
      end if

    end do

    indx(i) = indxt

  end do

  return
end
function i4vec_sum ( n, a )

!*****************************************************************************80
!
!! I4VEC_SUM returns the sum of the entries of an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    In FORTRAN90, this facility is offered by the built in
!    SUM function:
!
!      I4VEC_SUM ( N, A ) = SUM ( A(1:N) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, integer ( kind = 4 ) A(N), the array.
!
!    Output, integer ( kind = 4 ) I4VEC_SUM, the sum of the entries.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i4vec_sum

  i4vec_sum = sum ( a(1:n) )

  return
end
subroutine mono_next_grlex ( m, x )

!*****************************************************************************80
!
!! MONO_NEXT_GRLEX returns the next monomial in grlex order.
!
!  Discussion:
!
!    Example:
!
!    M = 3
!
!    #  X(1)  X(2)  X(3)  Degree
!      +------------------------
!    1 |  0     0     0        0
!      |
!    2 |  0     0     1        1
!    3 |  0     1     0        1
!    4 |  1     0     0        1
!      |
!    5 |  0     0     2        2
!    6 |  0     1     1        2
!    7 |  0     2     0        2
!    8 |  1     0     1        2
!    9 |  1     1     0        2
!   10 |  2     0     0        2
!      |
!   11 |  0     0     3        3
!   12 |  0     1     2        3
!   13 |  0     2     1        3
!   14 |  0     3     0        3
!   15 |  1     0     2        3
!   16 |  1     1     1        3
!   17 |  1     2     0        3
!   18 |  2     0     1        3
!   19 |  2     1     0        3
!   20 |  3     0     0        3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the maximum degree.
!    0 <= N.
!
!    Input/output, integer ( kind = 4 ) X(M), the current monomial.
!    The first element is X = [ 0, 0, ..., 0, 0 ].
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) i
  integer ( kind = 4 ) im1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) t
  integer ( kind = 4 ) x(m)
!
!  Ensure that 1 <= M.
!
  if ( m < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MONO_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  M < 1'
    stop 1
  end if
!
!  Ensure that 0 <= X(I).
!
  do i = 1, m
    if ( x(i) < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MONO_NEXT_GRLEX - Fatal error!'
      write ( *, '(a)' ) '  X(I) < 0'
      stop 1
    end if
  end do
!
!  Find I, the index of the rightmost nonzero entry of X.
!
  i = 0
  do j = m, 1, -1

    if ( 0 < x(j) ) then
      i = j
      exit
    end if

  end do
!
!  set T = X(I)
!  set X(I) to zero,
!  increase X(I-1) by 1,
!  increment X(M) by T-1.
!
  if ( i == 0 ) then
    x(m) = 1
    return
  else if ( i == 1 ) then
    t = x(1) + 1
    im1 = m
  else if ( 1 < i ) then
    t = x(i)
    im1 = i - 1
  end if

  x(i) = 0
  x(im1) = x(im1) + 1
  x(m) = x(m) + t - 1

  return
end
subroutine mono_rank_grlex ( m, x, rank )

!*****************************************************************************80
!
!! MONO_RANK_GRLEX computes the graded lexicographic rank of a monomial.
!
!  Discussion:
!
!    The graded lexicographic ordering is used, over all monomials
!    of dimension M, for monomial degree NM = 0, 1, 2, ...
!
!    For example, if M = 3, the ranking begins:
!
!    Rank  Sum    1  2  3
!    ----  ---   -- -- --
!       1    0    0  0  0
!
!       2    1    0  0  1
!       3    1    0  1  0
!       4    1    1  0  1
!
!       5    2    0  0  2
!       6    2    0  1  1
!       7    2    0  2  0
!       8    2    1  0  1
!       9    2    1  1  0
!      10    2    2  0  0
!
!      11    3    0  0  3
!      12    3    0  1  2
!      13    3    0  2  1
!      14    3    0  3  0
!      15    3    1  0  2
!      16    3    1  1  1
!      17    3    1  2  0
!      18    3    2  0  1
!      19    3    2  1  0
!      20    3    3  0  0
!
!      21    4    0  0  4
!      ..   ..   .. .. ..
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of parts in the composition.
!    1 <= KC.
!
!    Input, integer ( kind = 4 ) XC(M), the composition.
!    For each 1 <= I <= D, we have 0 <= XC(I).
!
!    Output, integer ( kind = 4 ) RANK, the rank of the composition.
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) i4vec_sum
  integer ( kind = 4 ) j
  integer ( kind = 4 ) ks
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm
  integer ( kind = 4 ) ns
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) tim1
  integer ( kind = 4 ) x(m)
  integer ( kind = 4 ) xs(m-1)
!
!  Ensure that 1 <= D.
!
  if ( m < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_RANK_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  KC < 1'
    stop 1
  end if
!
!  Ensure that 0 <= X(I).
!
  do i = 1, m
    if ( x(i) < 0 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MONO_RANK_GRLEX - Fatal error!'
      write ( *, '(a)' ) '  XC(I) < 0'
      stop 1
    end if
  end do
!
!  NM = sum ( X )
!
  nm = i4vec_sum ( m, x )
!
!  Convert to KSUBSET format.
!
  ns = nm + m - 1
  ks = m - 1

  xs(1) = x(1) + 1
  do i = 2, ks
    xs(i) = xs(i-1) + x(i) + 1
  end do
!
!  Compute the rank.
!
  rank = 1

  do i = 1, ks

    if ( i == 1 ) then
      tim1 = 0
    else
      tim1 = xs(i-1)
    end if

    if ( tim1 + 1 <= xs(i) - 1 ) then
      do j = tim1 + 1, xs(i) - 1
        rank = rank + i4_choose ( ns - j, ks - i )
      end do
    end if

  end do

  do n = 0, nm - 1
    rank = rank + i4_choose ( n + m - 1, n )
  end do

  return
end
subroutine mono_total_next_grlex ( m, n, x )

!*****************************************************************************80
!
!! MONO_TOTAL_NEXT_GRLEX: grlex next monomial with total degree equal to N.
!
!  Discussion:
!
!    We consider all monomials in an M dimensional space, with total degree N.
!
!    For example:
!
!    M = 3
!    N = 3
!
!    #  X(1)  X(2)  X(3)  Degree
!      +------------------------
!    1 |  0     0     3        3
!    2 |  0     1     2        3
!    3 |  0     2     1        3
!    4 |  0     3     0        3
!    5 |  1     0     2        3
!    6 |  1     1     1        3
!    7 |  1     2     0        3
!    8 |  2     0     1        3
!    9 |  2     1     0        3
!   10 |  3     0     0        3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the degree.
!    0 <= N.
!
!    Input/output, integer ( kind = 4 ) X(M), the current monomial.
!    To start the sequence, set X = [ 0, 0, ..., 0, N ].
!    The last value in the sequence is X = [ N, 0, ..., 0, 0 ].
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) n
  integer ( kind = 4 ) x(m)

  if ( n < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_TOTAL_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  N < 0.'
    stop 1
  end if

  if ( sum ( x(1:m) ) /= n ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_TOTAL_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  Input X does not sum to N.'
    stop 1
  end if

  if ( n == 0 ) then
    return
  end if

  if ( x(1) == n ) then
    x(1) = 0
    x(m) = n
  else
    call mono_next_grlex ( m, x )
  end if

  return
end
subroutine mono_unrank_grlex ( m, rank, x )

!*****************************************************************************80
!
!! MONO_UNRANK_GRLEX computes the monomial of given grlex rank.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of parts of the composition.
!    1 <= KC.
!
!    Input, integer ( kind = 4 ) RANK, the rank of the composition.
!    1 <= RANK.
!
!    Output, integer ( kind = 4 ) XC(M), the composition of the given rank.
!    For each I, 0 <= XC(I) <= NC, and 
!    sum ( 1 <= I <= M ) XC(I) = NC.
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) j
  integer ( kind = 4 ) ks
  integer ( kind = 4 ) nksub
  integer ( kind = 4 ) nm
  integer ( kind = 4 ) ns
  integer ( kind = 4 ) r
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank1
  integer ( kind = 4 ) rank2
  integer ( kind = 4 ) x(m)
  integer ( kind = 4 ) xs(m-1)
!
!  Ensure that 1 <= M.
!
  if ( m < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_UNRANK_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  M < 1'
    stop 1
  end if
!
!  Ensure that 1 <= RANK.
!
  if ( rank < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_UNRANK_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  RANK < 1'
    stop 1
  end if
!
!  Special case M == 1.
!
  if ( m == 1 ) then
    x(1) = rank - 1
    return
  end if
!
!  Determine the appropriate value of NM.
!  Do this by adding up the number of compositions of sum 0, 1, 2, 
!  ..., without exceeding RANK.  Moreover, RANK - this sum essentially
!  gives you the rank of the composition within the set of compositions
!  of sum NM.  And that's the number you need in order to do the
!  unranking.
!
  rank1 = 1
  nm = -1
  do
    nm = nm + 1
    r = i4_choose ( nm + m - 1, nm )
    if ( rank < rank1 + r ) then
      exit
    end if
    rank1 = rank1 + r
  end do

  rank2 = rank - rank1
!
!  Convert to KSUBSET format.
!  Apology: an unranking algorithm was available for KSUBSETS,
!  but not immediately for compositions.  One day we will come back
!  and simplify all this.
!
  ks = m - 1
  ns = nm + m - 1

  nksub = i4_choose ( ns, ks )

  j = 1

  do i = 1, ks

    r = i4_choose ( ns - j, ks - i )

    do while ( r <= rank2 .and. 0 < r )
      rank2 = rank2 - r
      j = j + 1
      r = i4_choose ( ns - j, ks - i )
    end do

    xs(i) = j
    j = j + 1

  end do
!
!  Convert from KSUBSET format to COMP format.
!
  x(1) = xs(1) - 1
  do i = 2, m - 1
    x(i) = xs(i) - xs(i-1) - 1
  end do
  x(m) = ns - xs(ks)

  return
end
function mono_upto_enum ( m, n )

!*****************************************************************************80
!
!! MONO_UPTO_ENUM enumerates monomials in M dimensions of degree up to N.
!
!  Discussion:
!
!    For M = 2, we have the following values:
!
!    N  VALUE
!
!    0    1
!    1    3
!    2    6
!    3   10
!    4   15
!    5   21
!
!    In particular, VALUE(2,3) = 10 because we have the 10 monomials:
!
!      1, x, y, x^2, xy, y^2, x^3, x^2y, xy^2, y^3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the maximum degree.
!
!    Output, integer ( kind = 4 ) MONO_UPTO_ENUM, the number of monomials in
!    D variables, of total degree N or less.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) value

  value = i4_choose ( n + m, n )

  mono_upto_enum = value

  return
end
subroutine mono_value ( m, n, f, x, v )

!*****************************************************************************80
!
!! MONO_VALUE evaluates a monomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, integer ( kind = 4 ) F(M), the exponents of the monomial.
!
!    Input, real ( kind = 8 ) X(M,N), the coordinates of the evaluation points.
!
!    Output, real ( kind = 8 ) V(N), the value of the monomial at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) f(m)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(m,n)
  
  v(1:n) = 1.0D+00

  do i = 1, m
    v(1:n) = v(1:n) * x(i,1:n) ** f(i)
  end do

  return
end
subroutine perm_check0 ( n, p )

!*****************************************************************************80
!
!! PERM_CHECK0 checks a 0-based permutation.
!
!  Discussion:
!
!    The routine verifies that each of the integers from 0 to
!    to N-1 occurs among the N entries of the permutation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries.
!
!    Input, integer ( kind = 4 ) P(N), the array to check.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) location
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) value

  do value = 0, n - 1

    ierror = 1

    do location = 1, n
      if ( p(location) == value ) then
        ierror = 0
        exit
      end if
    end do

    if ( ierror /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PERM_CHECK0 - Fatal error!'
      write ( *, '(a,i4)' ) '  Permutation is missing value ', value
      stop 1
    end if

  end do

  return
end
subroutine perm_check1 ( n, p )

!*****************************************************************************80
!
!! PERM_CHECK1 checks a 1-based permutation.
!
!  Discussion:
!
!    The routine verifies that each of the integers from 1 to
!    to N occurs among the N entries of the permutation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries.
!
!    Input, integer ( kind = 4 ) P(N), the array to check.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) location
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) value

  do value = 1, n

    ierror = 1

    do location = 1, n
      if ( p(location) == value ) then
        ierror = 0
        exit
      end if
    end do

    if ( ierror /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PERM_CHECK1 - Fatal error!'
      write ( *, '(a,i4)' ) '  Permutation is missing value ', value
      stop 1
    end if

  end do

  return
end
subroutine polynomial_add ( o1, c1, e1, o2, c2, e2, o, c, e )

!*****************************************************************************80
!
!! POLYNOMIAL_ADD adds two polynomials.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) O1, the "order" of polynomial 1.
!
!    Input, real ( kind = 8 ) C1(O1), the coefficients of polynomial 1.
!
!    Input, integer ( kind = 4 ) E1(O1), the indices of the exponents of 
!    polynomial 1.
!
!    Input, integer ( kind = 4 ) O2, the "order" of polynomial 2.
!
!    Input, real ( kind = 8 ) C2(O2), the coefficients of polynomial 2.
!
!    Input, integer ( kind = 4 ) E2(O2), the indices of the exponents of 
!    polynomial 2.
!
!    Output, integer ( kind = 4 ) O, the "order" of the polynomial sum.
!
!    Output, real ( kind = 8 ) C(O), the coefficients of the polynomial sum.
!
!    Output, integer ( kind = 4 ) E(O), the indices of the exponents of 
!    the polynomial sum.
!  
  implicit none

  integer ( kind = 4 ) o1
  integer ( kind = 4 ) o2

  real ( kind = 8 ) c(o1+o2)
  real ( kind = 8 ) c1(o1)
  real ( kind = 8 ) c2(o2)
  integer ( kind = 4 ) e(o1+o2)
  integer ( kind = 4 ) e1(o1)
  integer ( kind = 4 ) e2(o2)
  integer ( kind = 4 ) o

  o = o1 + o2
  call r8vec_concatenate ( o1, c1, o2, c2, c )
  call i4vec_concatenate ( o1, e1, o2, e2, e )

  call polynomial_sort ( o, c, e )
  call polynomial_compress ( o, c, e, o, c, e )

  return
end
subroutine polynomial_axpy ( s, o1, c1, e1, o2, c2, e2, o, c, e )

!*****************************************************************************80
!
!! POLYNOMIAL_AXPY adds a multiple of one polynomial to another.
!
!  Discussion:
!
!    P(X) = P2(X) + S * P1(X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) S, the multiplier for the first polynomial.
!
!    Input, integer ( kind = 4 ) O1, the "order" of polynomial 1.
!
!    Input, real ( kind = 8 ) C1(O1), the coefficients of polynomial 1.
!
!    Input, integer ( kind = 4 ) E1(O1), the indices of the exponents of 
!    polynomial 1.
!
!    Input, integer ( kind = 4 ) O2, the "order" of polynomial 2.
!
!    Input, real ( kind = 8 ) C2(O2), the coefficients of polynomial 2.
!
!    Input, integer ( kind = 4 ) E2(O2), the indices of the exponents of 
!    polynomial 2.
!
!    Output, integer ( kind = 4 ) O, the "order" of the polynomial sum.
!
!    Output, real ( kind = 8 ) C(O), the coefficients of the polynomial sum.
!
!    Output, integer ( kind = 4 ) E(O), the indices of the exponents of 
!    the polynomial sum.
!  
  implicit none

  integer ( kind = 4 ) o1
  integer ( kind = 4 ) o2

  real ( kind = 8 ) c(*)
  real ( kind = 8 ) c1(o1)
  real ( kind = 8 ) c2(o2)
  real ( kind = 8 ) c3(o1+o2)
  integer ( kind = 4 ) e(*)
  integer ( kind = 4 ) e1(o1)
  integer ( kind = 4 ) e2(o2)
  integer ( kind = 4 ) e3(o1+o2)
  integer ( kind = 4 ) o
  integer ( kind = 4 ) o3
  real ( kind = 8 ) s
  real ( kind = 8 ) sc1(o1)

  o3 = o1 + o2
  sc1(1:o1) = s * c1(1:o1)
  call r8vec_concatenate ( o1, sc1, o2, c2, c3 )
  call i4vec_concatenate ( o1, e1, o2, e2, e3 )

  call polynomial_sort ( o3, c3, e3 )
  call polynomial_compress ( o3, c3, e3, o, c, e )

  return
end
subroutine polynomial_compress ( o1, c1, e1, o2, c2, e2 )

!*****************************************************************************80
!
!! POLYNOMIAL_COMPRESS compresses a polynomial.
!
!  Discussion:
!
!    The function polynomial_sort ( ) should be called first, or else
!    the E1 vector should be in ascending sorted order.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) O1, the "order" of the polynomial.
!
!    Input, real ( kind = 8 ) C1(O1), the coefficients of the polynomial.
!
!    Input, integer ( kind = 4 ) E1(O1), the indices of the exponents of 
!    the polynomial.
!
!    Output, integer ( kind = 4 ) O2, the "order" of the polynomial.
!
!    Output, real ( kind = 8 ) C2(O2), the coefficients of the polynomial.
!
!    Output, integer ( kind = 4 ) E2(O2), the indices of the exponents of 
!    the polynomial.
!
  implicit none

  integer ( kind = 4 ) o1
  integer ( kind = 4 ) o2

  real ( kind = 8 ) c1(o1)
  real ( kind = 8 ) c2(o2)
  integer ( kind = 4 ) e1(o1)
  integer ( kind = 4 ) e2(o2)
  integer ( kind = 4 ) get
  integer ( kind = 4 ) put
  real ( kind = 8 ), parameter :: r8_epsilon_sqrt = 0.1490116119384766D-07
!
!  Add coefficients with the same exponent.
!
  get = 0
  put = 0

  do while ( get < o1 )

    get = get + 1

    if ( 0 == put ) then

      put = put + 1
      c2(put) = c1(get)
      e2(put) = e1(get)

    else

      if ( e2(put) == e1(get) ) then
        c2(put) = c2(put) + c1(get)
      else
        put = put + 1
        c2(put) = c1(get)
        e2(put) = e1(get)
      end if

    end if

  end do
 
  o2 = put
!
!  Clear out zeros and tiny coefficients.
!
  get = 0
  put = 0

  do while ( get < o2 )

    get = get + 1

    if ( r8_epsilon_sqrt < abs ( c2(get) ) ) then
      put = put + 1
      c2(put) = c2(get)
      e2(put) = e2(get)
    end if

  end do

  o2 = put

  return
end
subroutine polynomial_dif ( m, o1, c1, e1, dif, o2, c2, e2 )

!*****************************************************************************80
!
!! POLYNOMIAL_DIF differentiates a polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) O1, the "order" of polynomial 1.
!
!    Input, real ( kind = 8 ) C1(O1), the coefficients of polynomial 1.
!
!    Input, integer ( kind = 4 ) E1(O1), the indices of the exponents of 
!    polynomial 1.
!
!    Input, integer ( kind = 4 ) DIF(M), indicates the number of 
!    differentiations in each component.
!
!    Output, integer ( kind = 4 ) O2, the "order" of the polynomial derivative.
!
!    Output, real ( kind = 8 ) C2(O2), the coefficients of the polynomial 
!    derivative.
!
!    Output, integer ( kind = 4 ) E2(O2), the indices of the exponents of the
!    polynomial derivative.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) o1

  real ( kind = 8 ) c1(o1)
  real ( kind = 8 ) c2(o1)
  integer ( kind = 4 ) dif(m)
  integer ( kind = 4 ) e1(o1)
  integer ( kind = 4 ) e2(o1)
  integer ( kind = 4 ) f1(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_fall
  integer ( kind = 4 ) j
  integer ( kind = 4 ) o2

  o2 = o1
  c2(1:o2) = c1(1:o1)

  do j = 1, o1
    call mono_unrank_grlex ( m, e1(j), f1 )
    do i = 1, m
      c2(j) = c2(j) * i4_fall ( f1(i), dif(i) )
      f1(i) = max ( f1(i) - dif(i), 0 )
    end do
    call mono_rank_grlex ( m, f1, e2(j) )
  end do

  call polynomial_sort ( o2, c2, e2 )

  call polynomial_compress ( o2, c2, e2, o2, c2, e2 )

  return
end
subroutine polynomial_mul ( m, o1, c1, e1, o2, c2, e2, o, c, e )

!*****************************************************************************80
!
!! POLYNOMIAL_MUL multiplies two polynomials.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) O1, the "order" of polynomial 1.
!
!    Input, real ( kind = 8 ) C1(O1), the coefficients of polynomial 1.
!
!    Input, integer ( kind = 4 ) E1(O1), the indices of the exponents of 
!    polynomial 1.
!
!    Input, integer ( kind = 4 ) O2, the "order" of polynomial 2.
!
!    Input, real ( kind = 8 ) C2(O2), the coefficients of polynomial 2.
!
!    Input, integer ( kind = 4 ) E2(O2), the indices of the exponents of 
!    polynomial 2.
!
!    Output, integer ( kind = 4 ) O, the "order" of the polynomial product.
!
!    Output, real ( kind = 8 ) C(O), the coefficients of the polynomial product.
!
!    Output, integer ( kind = 4 ) E(O), the indices of the exponents of the 
!    polynomial product.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) o1
  integer ( kind = 4 ) o2

  real ( kind = 8 ) c(o1*o2)
  real ( kind = 8 ) c1(o1)
  real ( kind = 8 ) c2(o2)
  integer ( kind = 4 ) e(o1*o2)
  integer ( kind = 4 ) e1(o1)
  integer ( kind = 4 ) e2(o2)
  integer ( kind = 4 ) f(m)
  integer ( kind = 4 ) f1(m)
  integer ( kind = 4 ) f2(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) o

  o = 0
  do j = 1, o2
    do i = 1, o1
      o = o + 1
      c(o) = c1(i) * c2(j)
      call mono_unrank_grlex ( m, e1(i), f1 )
      call mono_unrank_grlex ( m, e2(j), f2 )
      f(1:m) = f1(1:m) + f2(1:m)
      call mono_rank_grlex ( m, f, e(o) )
    end do
  end do

  call polynomial_sort ( o, c, e )
  call polynomial_compress ( o, c, e, o, c, e )

  return
end
subroutine polynomial_print ( m, o, c, e, title )

!*****************************************************************************80
!
!! POLYNOMIAL_PRINT prints a polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) O, the "order" of the polynomial, that is,
!    simply the number of terms.
!
!    Input, real ( kind = 8 ) C(O), the coefficients.
!
!    Input, integer ( kind = 4 ) E(O), the indices of the exponents.
!
!    Input, character ( len = * ) TITLE, a title.
!      
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) o

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  integer ( kind = 4 ) f(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = * ) title

  write ( *, '(a)' ) trim ( title )

  if ( o == 0 ) then
    write ( *, '(a)' ) '      0.'
  else
    do j = 1, o
      write (  *, '(a)', advance = 'no' ) '    '
      if ( c(j) < 0.0D+00 ) then
        write (  *, '(a)', advance = 'no' ) '- '
      else
        write (  *, '(a)', advance = 'no' ) '+ '
      end if
      write ( *, '(g14.6,a)', advance = 'no' ) abs ( c(j) ), ' * x^('
      call mono_unrank_grlex ( m, e(j), f )
      do i = 1, m
        write ( *, '(i2)', advance = 'no' ) f(i)
        if ( i < m ) then
          write (  *, '(a)', advance = 'no' ) ','
        else
          write (  *, '(a)', advance = 'no' ) ')'
        end if
      end do
      if ( j == o ) then
        write (  *, '(a)', advance = 'no' ) '.'
      end if
      write (  *, '(a)' ) ''
    end do
  end if

  return
end
subroutine polynomial_scale ( s, m, o, c, e )

!*****************************************************************************80
!
!! POLYNOMIAL_SCALE scales a polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) S, the scale factor.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) O, the "order" of the polynomial.
!
!    Input/output, real ( kind = 8 ) C(O), the coefficients of the polynomial.
!
!    Input, integer ( kind = 4 ) E(O), the indices of the exponents of 
!    the polynomial.
!
  implicit none

  integer ( kind = 4 ) o

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) e(o)
  real ( kind = 8 ) s

  c(1:o) = c(1:o) * s

  return
end
subroutine polynomial_sort ( o, c, e )

!*****************************************************************************80
!
!! POLYNOMIAL_SORT sorts the information in a polynomial.
!
!  Discussion
!
!    The coefficients C and exponents E are rearranged so that 
!    the elements of E are in ascending order.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) O, the "order" of the polynomial.
!
!    Input/output, real ( kind = 8 ) C(O), the coefficients of the polynomial.
!
!    Input/output, integer ( kind = 4 ) E(O), the indices of the exponents of 
!    the polynomial.
!
  implicit none

  integer ( kind = 4 ) o

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  integer ( kind = 4 ) indx(o)

  call i4vec_sort_heap_index_a ( o, e, indx )

  call i4vec_permute ( o, indx, e )
  call r8vec_permute ( o, indx, c )

  return
end
subroutine polynomial_value ( m, o, c, e, n, x, p )

!*****************************************************************************80
!
!! POLYNOMIAL_VALUE evaluates a polynomial.
!
!  Discussion:
!
!    The polynomial is evaluated term by term, and no attempt is made to
!    use an approach such as Horner's method to speed up the process.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) O, the "order" of the polynomial.
!
!    Input, real ( kind = 8 ) C(O), the coefficients of the polynomial.
!
!    Input, integer ( kind = 4 ) E(O), the indices of the exponents 
!    of the polynomial.
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(M,N), the coordinates of the evaluation points.
!
!    Output, real ( kind = 8 ) P(N), the value of the polynomial at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  integer ( kind = 4 ) f(m)
  integer ( kind = 4 ) j
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(m,n)

  p(1:n) = 0.0D+00

  do j = 1, o
    call mono_unrank_grlex ( m, e(j), f )
    call mono_value ( m, n, f, x, v )
    p(1:n) = p(1:n) + c(j) * v(1:n)
  end do

  return
end
subroutine r8vec_concatenate ( n1, a, n2, b, c )

!*****************************************************************************80
!
!! R8VEC_CONCATENATE concatenates two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N1, the number of entries in the first vector.
!
!    Input, real ( kind = 8 ) A(N1), the first vector.
!
!    Input, integer ( kind = 4 ) N2, the number of entries in the second vector.
!
!    Input, real ( kind = 8 ) B(N2), the second vector.
!
!    Output, real ( kind = 8 ) C(N1+N2), the concatenation of A and B.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  real ( kind = 8 ) a(n1)
  real ( kind = 8 ) b(n2)
  real ( kind = 8 ) c(n1+n2)
  
  c(   1:n1)    = a(1:n1)
  c(n1+1:n1+n2) = b(1:n2)

  return
end
subroutine r8vec_permute ( n, p, a )

!*****************************************************************************80
!
!! R8VEC_PERMUTE permutes an R8VEC in place.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This routine permutes an array of real "objects", but the same
!    logic can be used to permute an array of objects of any arithmetic
!    type, or an array of objects of any complexity.  The only temporary
!    storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!    P(I) = J means that the I-th element of the output array should be
!    the J-th element of the input array.  P must be a legal permutation
!    of the integers from 1 to N, otherwise the algorithm will
!    fail catastrophically.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (   2,   4,   5,   1,   3 )
!      A = ( 1.0, 2.0, 3.0, 4.0, 5.0 )
!
!    Output:
!
!      A    = ( 2.0, 4.0, 5.0, 1.0, 3.0 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects.
!
!    Input, integer ( kind = 4 ) P(N), the permutation.
!
!    Input/output, real ( kind = 8 ) A(N), the array to be permuted.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_temp
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) istart
  integer ( kind = 4 ) p(n)

  call perm_check1 ( n, p )
!
!  Search for the next element of the permutation that has not been used.
!
  do istart = 1, n

    if ( p(istart) < 0 ) then

      cycle

    else if ( p(istart) == istart ) then

      p(istart) = - p(istart)
      cycle

    else

      a_temp = a(istart)
      iget = istart
!
!  Copy the new value into the vacated entry.
!
      do

        iput = iget
        iget = p(iget)

        p(iput) = - p(iput)

        if ( iget < 1 .or. n < iget ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8VEC_PERMUTE - Fatal error!'
          write ( *, '(a)' ) '  A permutation index is out of range.'
          write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
          stop 1
        end if

        if ( iget == istart ) then
          a(iput) = a_temp
          exit
        end if

        a(iput) = a(iget)

      end do

    end if

  end do
!
!  Restore the signs of the entries.
!
  p(1:n) = - p(1:n)

  return
end
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

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
