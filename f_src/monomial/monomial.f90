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
subroutine i4vec_uniform_ab ( n, a, b, seed, x )

!*****************************************************************************80
!
!! I4VEC_UNIFORM_AB returns a scaled pseudorandom I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    The pseudorandom numbers should be scaled to be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vector.
!
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) X(N), a vector of numbers between A and B.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 4 ) r
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) value
  integer ( kind = 4 ) x(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

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

    x(i) = value

  end do

  return
end
function mono_between_enum ( m, n1, n2 )

!*****************************************************************************80
!
!! MONO_BETWEEN_ENUM enumerates monomials in M dimensions of degrees in a range.
!
!  Discussion:
!
!    For M = 3, we have the following table:
!
!     N2 0  1  2  3  4  5  6   7   8
!   N1 +----------------------------
!    0 | 1  4 10 20 35 56 84 120 165
!    1 | 0  3  9 19 34 55 83 119 164
!    2 | 0  0  6 16 31 52 80 116 161
!    3 | 0  0  0 10 25 46 74 110 155
!    4 | 0  0  0  0 15 36 64 100 145
!    5 | 0  0  0  0  0 21 49  85 130
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
!    Input, integer ( kind = 4 ) N1, N2, the minimum and maximum degrees.
!    0 <= N1 <= N2.
!
!    Output, integer ( kind = 4 ) MONO_BETWEEN_ENUM, the number of monomials 
!    in M variables, of total degree between N1 and N2 inclusive.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) mono_between_enum
  integer ( kind = 4 ) n0
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n1_copy
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) value

  n1_copy = max ( n1, 0 )

  if ( n2 < n1_copy ) then
    mono_between_enum = 0
    return
  end if

  if ( n1_copy == 0 ) then
    value = i4_choose ( n2 + m, n2 )
  else if ( n1_copy == n2 ) then
    value = i4_choose ( n2 + m - 1, n2 )
  else
    n0 = n1_copy - 1
    value = i4_choose ( n2 + m, n2 ) - i4_choose ( n0 + m, n0 )
  end if

  mono_between_enum = value

  return
end
subroutine mono_between_next_grevlex ( m, n1, n2, x )

!*****************************************************************************80
!
!! MONO_BETWEEN_NEXT_GREVLEX: grevlex next monomial, degree between N1 and N2.
!
!  Discussion:
!
!    We consider all monomials in a M dimensional space, with total
!    degree N between N1 and N2, inclusive.
!
!    For example:
!
!    M = 3
!    N1 = 2
!    N2 = 3
!
!    #  X(1)  X(2)  X(3)  Degree
!      +------------------------
!    1 |  0     0     2        2
!    2 |  0     1     1        2
!    3 |  1     0     1        2
!    4 |  0     2     0        2
!    5 |  1     1     0        2
!    6 |  2     0     0        2
!      |
!    7 |  0     0     3        3
!    8 |  0     1     2        3
!    9 |  1     0     2        3
!   10 |  0     2     1        3
!   11 |  1     1     1        3
!   12 |  2     0     1        3
!   13 |  0     3     0        3
!   14 |  1     2     0        3
!   15 |  2     1     0        3
!   16 |  3     0     0        3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N1, N2, the minimum and maximum degrees.
!    0 <= N1 <= N2.
!
!    Input, integer ( kind = 4 ) X(M), the current monomial.
!    To start the sequence, set X = [ 0, 0, ..., 0, N1 ].
!
!    Output, integer ( kind = 4 ) X(M), the next monomial.
!    The last value in the sequence is X = [ N2, 0, ..., 0, 0 ].
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) x(m)

  if ( n1 < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GREVLEX - Fatal error!'
    write ( *, '(a)' ) '  N1 < 0.'
    stop 1
  end if

  if ( n2 < n1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GREVLEX - Fatal error!'
    write ( *, '(a)' ) '  N2 < N1.'
    stop 1
  end if

  if ( sum ( x(1:m) ) < n1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GREVLEX - Fatal error!'
    write ( *, '(a)' ) '  Input X sums to less than N1.'
    stop 1
  end if

  if ( n2 < sum ( x(1:m) ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GREVLEX - Fatal error!'
    write ( *, '(a)' ) '  Input X sums to more than N2.'
    stop 1
  end if

  if ( n2 == 0 ) then
    return
  end if

  if ( x(1) == n2 ) then
    x(1) = 0
    x(m) = n1
  else
    call mono_next_grevlex ( m, x )
  end if

  return
end
subroutine mono_between_next_grlex ( m, n1, n2, x )

!*****************************************************************************80
!
!! MONO_BETWEEN_NEXT_GRLEX: grlex next monomial, degree between N1 and N2.
!
!  Discussion:
!
!    We consider all monomials in a M dimensional space, with total
!    degree N between N1 and N2, inclusive.
!
!    For example:
!
!    M = 3
!    N1 = 2
!    N2 = 3
!
!    #  X(1)  X(2)  X(3)  Degree
!      +------------------------
!    1 |  0     0     2        2
!    2 |  0     1     1        2
!    3 |  0     2     0        2
!    4 |  1     0     1        2
!    5 |  1     1     0        2
!    6 |  2     0     0        2
!      |
!    7 |  0     0     3        3
!    8 |  0     1     2        3
!    9 |  0     2     1        3
!   10 |  0     3     0        3
!   11 |  1     0     2        3
!   12 |  1     1     1        3
!   13 |  1     2     0        3
!   14 |  2     0     1        3
!   15 |  2     1     0        3
!   16 |  3     0     0        3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N1, N2, the minimum and maximum degrees.
!    0 <= N1 <= N2.
!
!    Input/output, integer ( kind = 4 ) X(M), the current monomial.
!    To start the sequence, set X = [ 0, 0, ..., 0, N1 ].
!    The last value in the sequence is X = [ N2, 0, ..., 0, 0 ].
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) x(m)

  if ( n1 < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  N1 < 0.'
    stop 1
  end if

  if ( n2 < n1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  N2 < N1.'
    stop 1
  end if

  if ( sum ( x(1:m) ) < n1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  Input X sums to less than N1.'
    stop 1
  end if

  if ( n2 < sum ( x(1:m) ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  Input X sums to more than N2.'
    stop 1
  end if

  if ( n2 == 0 ) then
    return
  end if

  if ( x(1) == n2 ) then
    x(1) = 0
    x(m) = n1
  else
    call mono_next_grlex ( m, x )
  end if

  return
end
subroutine mono_between_random ( m, n1, n2, seed, rank, x )

!*****************************************************************************80
!
!! MONO_BETWEEN_RANDOM: random monomial with total degree between N1 and N2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N1, N2, the minimum and maximum degrees.
!    0 <= N1 <= N2.
!
!    Input/output, integer ( kind = 4 ) SEED, the random number seed.
!
!    Output, integer ( kind = 4 ) RANK, the rank of the monomial.
!
!    Output, integer ( kind = 4 ) X(M), the random monomial.
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n1_copy
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_max
  integer ( kind = 4 ) rank_min
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(m)

  n1_copy = max ( n1, 0 )
  rank_min = mono_upto_enum ( m, n1_copy - 1 ) + 1
  rank_max = mono_upto_enum ( m, n2 )
  rank = i4_uniform_ab ( rank_min, rank_max, seed )
  call mono_unrank_grlex ( m, rank, x )

  return
end
subroutine mono_next_grevlex ( m, x )

!*****************************************************************************80
!
!! MONO_NEXT_GREVLEX: grevlex next monomial.
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
!    7 |  1     0     1        2
!    8 |  0     2     0        2
!    9 |  1     1     0        2
!   10 |  2     0     0        2
!      |
!   11 |  0     0     3        3
!   12 |  0     1     2        3
!   13 |  1     0     2        3
!   14 |  0     2     1        3
!   15 |  1     1     1        3
!   16 |  2     0     1        3
!   17 |  0     3     0        3
!   18 |  1     2     0        3
!   19 |  2     1     0        3
!   20 |  3     0     0        3
!
!    Thanks to Stefan Klus for pointing out a discrepancy in a previous
!    version of this code, 05 February 2015.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) X(M), the current monomial.
!    The first element is X = [ 0, 0, ..., 0, 0 ].
!
!    Output, integer ( kind = 4 ) X(M), the next monomial.
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) t
  integer ( kind = 4 ) x(m)
!
!  Ensure that 1 <= M.
!
  if ( m < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MONO_NEXT_GREVLEX - Fatal error!'
    write ( *, '(a)' ) '  D < 1'
    stop 1
  end if
!
!  Ensure that 0 <= X(I).
!
  do i = 1, m
    if ( x(i) < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MONO_NEXT_GREVLEX - Fatal error!'
      write ( *, '(a)' ) '  X(I) < 0'
      stop 1
    end if
  end do
!
!  Seek the first index 1 < I for which 0 < X(I).
!
  j = 1

  do i = 2, m
    if ( 0 < x(i) ) then
      j = i
      exit
    end if
  end do

  if ( j == 1 ) then
    t = x(1)
    x(1) = 0
    x(m) = t + 1
  else if ( j < m ) then
    x(j) = x(j) - 1
    t = x(1) + 1
    x(1) = 0
    x(j-1) = x(j-1) + t
  else if ( j == m ) then
    t = x(1)
    x(1) = 0
    x(j-1) = t + 1
    x(j) = x(j) - 1
  end if

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
subroutine mono_print ( m, f, title )

!*****************************************************************************80
!
!! MONO_PRINT prints a monomial.
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
!    Input, integer ( kind = 4 ) F(M), the exponents.
!
!    Input, character ( len = * ) TITLE, a title.
!      
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) f(m)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)', advance = 'no' ) title
  write ( *, '(a)', advance = 'no' ) 'x^('
  do i = 1, m
    write ( *, '(i2)', advance = 'no' ) f(i)
    if ( i < m ) then
      write (  *, '(a)', advance = 'no' ) ','
    else
      write (  *, '(a)', advance = 'yes' ) ').'
    end if
  end do

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
function mono_total_enum ( m, n )

!*****************************************************************************80
!
!! MONO_TOTAL_ENUM enumerates monomials in M dimensions of degree equal to N.
!
!  Discussion:
!
!    For M = 3, we have the following values:
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
!    In particular, VALUE(3,3) = 10 because we have the 10 monomials:
!
!      x^3, x^2y, x^2z, xy^2, xyz, xz^3, y^3, y^2z, yz^2, z^3.
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
!    Output, integer ( kind = 4 ) VALUE, the number of monomials in D variables,
!    of total degree N.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) mono_total_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) value

  value = i4_choose ( n + m - 1, n )

  mono_total_enum = value

  return
end
subroutine mono_total_next_grevlex ( m, n, x )

!*****************************************************************************80
!
!! MONO_TOTAL_NEXT_GREVLEX: grevlex next monomial with total degree equal to N.
!
!  Discussion:
!
!    We consider all monomials in a M dimensional space, with total degree N.
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
!    3 |  1     0     2        3
!    4 |  0     2     1        3
!    5 |  1     1     1        3
!    6 |  2     0     1        3
!    7 |  0     3     0        3
!    8 |  1     2     0        3
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
!    Input, integer ( kind = 4 ) X(M), the current monomial.
!    To start the sequence, set X = [ 0, 0, ..., 0, N ].
!
!    Output, integer ( kind = 4 ) X(M), the next monomial.
!    The last value in the sequence is X = [ N, 0, ..., 0, 0 ].
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) n
  integer ( kind = 4 ) x(m)

  if ( n < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_TOTAL_NEXT_GREVLEX - Fatal error!'
    write ( *, '(a)' ) '  N < 0.'
    stop 1
  end if

  if ( sum ( x(1:m) ) /= n ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_TOTAL_NEXT_GREVLEX - Fatal error!'
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
    call mono_next_grevlex ( m, x )
  end if

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
subroutine mono_total_random ( m, n, seed, rank, x )

!*****************************************************************************80
!
!! MONO_TOTAL_RANDOM: random monomial with total degree equal to N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2013
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
!    Input/output, integer ( kind = 4 ) SEED, the random number seed.
!
!    Output, integer ( kind = 4 ) RANK, the rank of the monomial.
!
!    Output, integer ( kind = 4 ) X(M), the random monomial.
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_max
  integer ( kind = 4 ) rank_min
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(m)

  rank_min = mono_upto_enum ( m, n - 1 ) + 1
  rank_max = mono_upto_enum ( m, n )
  rank = i4_uniform_ab ( rank_min, rank_max, seed )
  call mono_unrank_grlex ( m, rank, x )

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
subroutine mono_upto_next_grevlex ( m, n, x )

!*****************************************************************************80
!
!! MONO_UPTO_NEXT_GREVLEX: grevlex next monomial with total degree up to N.
!
!  Discussion:
!
!    We consider all monomials in an M dimensional space, with total
!    degree up to N.
!
!    For example:
!
!    M = 3
!    N = 3
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
!    7 |  1     0     1        2
!    8 |  0     2     0        2
!    9 |  1     1     0        2
!   10 |  2     0     0        2
!      |
!   11 |  0     0     3        3
!   12 |  0     1     2        3
!   13 |  1     0     2        3
!   14 |  0     2     1        3
!   15 |  1     1     1        3
!   16 |  2     0     1        3
!   17 |  0     3     0        3
!   18 |  1     2     0        3
!   19 |  2     1     0        3
!   20 |  3     0     0        3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2013
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
!    Input, integer ( kind = 4 ) X(M), the current monomial.
!    To start the sequence, set X = [ 0, 0, ..., 0, 0 ].
!
!    Output, integer ( kind = 4 ) X(M), the next monomial.
!    The last value in the sequence is X = [ N, 0, ..., 0, 0 ].
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) n
  integer ( kind = 4 ) x(m)

  if ( n < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_UPTO_NEXT_GREVLEX - Fatal error!'
    write ( *, '(a)' ) '  N < 0.'
    stop 1
  end if

  if ( sum ( x(1:m) ) < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_UPTO_NEXT_GREVLEX - Fatal error!'
    write ( *, '(a)' ) '  Input X sums to less than 0.'
    stop 1
  end if

  if ( n < sum ( x(1:m) ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_UPTO_NEXT_GREVLEX - Fatal error!'
    write ( *, '(a)' ) '  Input X sums to more than N.'
    stop 1
  end if

  if ( n == 0 ) then
    return
  end if

  if ( x(1) == n ) then
    x(1) = 0
  else
    call mono_next_grevlex ( m, x )
  end if

  return
end
subroutine mono_upto_next_grlex ( m, n, x )

!*****************************************************************************80
!
!! MONO_UPTO_NEXT_GRLEX: grlex next monomial with total degree up to N.
!
!  Discussion:
!
!    We consider all monomials in an M dimensional space, with total
!    degree up to N.
!
!    For example:
!
!    M = 3
!    N = 3
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
!    08 December 2013
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
!    To start the sequence, set X = [ 0, 0, ..., 0, 0 ].
!    The last value in the sequence is X = [ N, 0, ..., 0, 0 ].
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) n
  integer ( kind = 4 ) x(m)

  if ( n < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_UPTO_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  N < 0.'
    stop 1
  end if

  if ( sum ( x(1:m) ) < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_UPTO_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  Input X sums to less than 0.'
    stop 1
  end if

  if ( n < sum ( x(1:m) ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_UPTO_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  Input X sums to more than N.'
    stop 1
  end if

  if ( n == 0 ) then
    return
  end if

  if ( x(1) == n ) then
    x(1) = 0
  else
    call mono_next_grlex ( m, x )
  end if

  return
end
subroutine mono_upto_random ( m, n, seed, rank, x )

!*****************************************************************************80
!
!! MONO_UPTO_RANDOM: random monomial with total degree less than or equal to N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2013
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
!    Input/output, integer ( kind = 4 ) SEED, the random number seed.
!
!    Output, integer ( kind = 4 ) RANK, the rank of the monomial.
!
!    Output, integer ( kind = 4 ) X(M), the random monomial.
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_max
  integer ( kind = 4 ) rank_min
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(m)

  rank_min = 1
  rank_max = mono_upto_enum ( m, n )
  rank = i4_uniform_ab ( rank_min, rank_max, seed )
  call mono_unrank_grlex ( m, rank, x )

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
