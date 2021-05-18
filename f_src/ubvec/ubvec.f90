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
subroutine ksubset_colex_unrank ( rank, k, n, t )

!*****************************************************************************80
!
!! KSUBSET_COLEX_UNRANK computes the K subset of given colex rank.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998,
!    ISBN: 0-8493-3988-X,
!    LC: QA164.K73.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RANK, the rank of the K subset.
!
!    Input, integer ( kind = 4 ) K, the number of elements each K subset must
!    have.  0 <= K <= N.
!
!    Input, integer ( kind = 4 ) N, the number of elements in the master set.
!    N must be positive.
!
!    Output, integer ( kind = 4 ) T(K), describes the K subset of the given
!    rank.  T(I) is the I-th element.  The elements must be listed in
!    DESCENDING order.
!
  implicit none

  integer ( kind = 4 ) k

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nksub
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_copy
  integer ( kind = 4 ) t(k)
  integer ( kind = 4 ) x
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'KSUBSET_COLEX_UNRANK - Fatal error!'
    write ( *, '(a)' ) '  Input N is illegal.'
    stop 1
  end if

  if ( k == 0 ) then
    return
  endif

  if ( k < 0 .or. n < k ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'KSUBSET_COLEX_UNRANK - Fatal error!'
    write ( *, '(a)' ) '  Input K is illegal.'
    stop 1
  end if

! call ksubset_enum ( k, n, nksub )

  nksub = i4_choose ( n, k )

  if ( rank < 0 .or. nksub < rank ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'KSUBSET_COLEX_UNRANK - Fatal error!'
    write ( *, '(a)' ) '  The input rank is illegal.'
    stop 1
  end if
!
  rank_copy = rank

  x = n

  do i = 1, k

    do while ( rank_copy < i4_choose ( x, k + 1 - i ) )
      x = x - 1
    end do

    t(i) = x + 1
    rank_copy = rank_copy - i4_choose ( x, k + 1 - i )

  end do

  return
end
subroutine morse_thue ( i, s )

!*****************************************************************************80
!
!! MORSE_THUE generates a Morse_Thue number.
!
!  Discussion:
!
!    The Morse_Thue sequence can be defined in a number of ways.
!
!    A) Start with the string containing the single letter '0'; then
!       repeatedly apply the replacement rules '0' -> '01' and
!       '1' -> '10' to the letters of the string.  The Morse_Thue sequence
!       is the resulting letter sequence.
!
!    B) Starting with the string containing the single letter '0',
!       repeatedly append the binary complement of the string to itself.
!       Thus, '0' becomes '0' + '1' or '01', then '01' becomes
!       '01' + '10', which becomes '0110' + '1001', and so on.
!
!    C) Starting with I = 0, the I-th Morse-Thue number is determined
!       by taking the binary representation of I, adding the digits,
!       and computing the remainder modulo 2.
!
!  Example:
!
!     I  binary   S
!    --  ------  --
!     0       0   0
!     1       1   1
!     2      10   1
!     3      11   0
!     4     100   1
!     5     101   0
!     6     110   0
!     7     111   1
!     8    1000   1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the index of the Morse-Thue number.
!    Normally, I is 0 or greater, but any value is allowed.
!
!    Output, integer ( kind = 4 ) S, the Morse-Thue number of index I.
!
  implicit none

  integer ( kind = 4 ), parameter :: nbits = 32

  integer ( kind = 4 ) b(nbits)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_copy
  integer ( kind = 4 ) s

  i_copy = abs ( i )
!
!  Expand I into binary form.
!
  call ui4_to_ubvec ( i_copy, nbits, b )
!
!  Sum the 1's in the binary representation.
!
  s = sum ( b(1:nbits) )
!
!  Take the value modulo 2.
!
  s = mod ( s, 2 )

  return
end
subroutine nim_sum ( i, j, k )

!*****************************************************************************80
!
!! NIM_SUM computes the Nim sum of two integers.
!
!  Discussion:
!
!    If K is the Nim sum of I and J, then each bit of K is the exclusive
!    OR of the corresponding bits of I and J.
!
!  Example:
!
!     I     J     K     I base 2    J base 2    K base 2
!   ----  ----  ----  ----------  ----------  ----------
!      0     0     0           0           0           0
!      1     0     1           1           0           1
!      1     1     0           1           1           0
!      2     7     5          10         111         101
!     11    28    23        1011       11100       10111
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, J, the integers to be Nim-summed.
!    0 < I, J.
!
!    Output, integer ( kind = 4 ) K, the Nim sum of I and J.
!
  implicit none

  integer ( kind = 4 ), parameter :: nbits = 32

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ivec(nbits)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jvec(nbits)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kvec(nbits)
!
!  Convert I and J to UBVEC's.
!
  call ui4_to_ubvec ( i, nbits, ivec )
  call ui4_to_ubvec ( j, nbits, jvec )
!
!  XOR the UBVEC's.
!
  call ubvec_xor ( nbits, ivec, jvec, kvec )
!
!  Convert the UBVEC sum to a UI4.
!
  call ubvec_to_ui4 ( nbits, kvec, k )

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
subroutine ubvec_add ( n, ubvec1, ubvec2, ubvec3 )

!*****************************************************************************80
!
!! UBVEC_ADD adds two unsigned binary vectors.
!
!  Discussion:
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Example:
!
!    N = 4
!
!     UBVEC1       +  UBVEC2       =  UBVEC3
!
!    ( 0 0 0 1 )   + ( 0 0 1 1 )   = ( 0 1 0 0 )
!
!      1           +   3           =   4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the vectors.
!
!    Input, integer ( kind = 4 ) UBVEC1(N), UBVEC2(N), the vectors to be added.
!
!    Output, integer ( kind = 4 ) UBVEC3(N), the sum of the two input vectors.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)
  integer ( kind = 4 ) ubvec3(n)
  integer ( kind = 4 ) i
  logical overflow

  overflow = .false.

  ubvec3(1:n) = ubvec1(1:n) + ubvec2(1:n)

  do i = n, 1, - 1
    do while ( 2 <= ubvec3(i) )
      ubvec3(i) = ubvec3(i) - 2
      if ( 1 < i ) then
        ubvec3(i-1) = ubvec3(i-1) + 1
      else
        overflow = .true.
      end if
    end do
  end do

  return
end
subroutine ubvec_and ( n, ubvec1, ubvec2, ubvec3 )

!*****************************************************************************80
!
!! UBVEC_AND computes the AND of two unsigned binary vectors.
!
!  Discussion:
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the vectors.
!
!    Input, integer ( kind = 4 ) UBVEC1(N), UBVEC2(N), the vectors.
!
!    Input, integer ( kind = 4 ) UBVEC3(N), the AND of the two vectors.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)
  integer ( kind = 4 ) ubvec3(n)

  ubvec3(1:n) = min ( ubvec1(1:n), ubvec2(1:n) )

  return
end
subroutine ubvec_check ( n, ubvec, ierror )

!*****************************************************************************80
!
!! UBVEC_CHECK checks an unsigned binary vector.
!
!  Discussion:
!
!    The only check made is that the entries are all 0 or 1.
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the vectors.
!
!    Input, integer ( kind = 4 ) UBVEC(N), the vector to be checked.
!
!    Output, integer ( kind = 4 ) IERROR, is nonzero if an error occurred.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror

  ierror = 0

  do i = 1, n
    if ( ubvec(i) < 0 .or. 2 <= ubvec(i) ) then
      ierror = i
      return
    end if
  end do

  return
end
subroutine ubvec_complement1 ( n, ubvec1, ubvec2 )

!*****************************************************************************80
!
!! UBVEC_COMPLEMENT1 computes the one's complement of an unsigned binary vector.
!
!  Discussion:
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the vectors.
!
!    Input, integer ( kind = 4 ) UBVEC1(N), the vector to be complemented.
!
!    Output, integer ( kind = 4 ) UBVEC2(N), the complemented vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)

  ubvec2(1:n) = 1 - ubvec1(1:n)

  return
end
function ubvec_enum ( n )

!*****************************************************************************80
!
!! UBVEC_ENUM enumerates the unsigned binary vectors of length N.
!
!  Discussion:
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the vectors.
!
!    Output, integer ( kind = 4 ) UBVEC_ENUM, the number of binary vectors.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ubvec_enum
  integer ( kind = 4 ) value

  value = 2 ** n

  ubvec_enum = value

  return
end
subroutine ubvec_next ( n, ubvec )

!*****************************************************************************80
!
!! UBVEC_NEXT generates the next UBVEC.
!
!  Discussion:
!
!    The vectors are produced in the order:
!
!    (0,0,...,0),
!    (0,0,...,1),
!    ...
!    (1,1,...,1)
!
!    and the "next" vector after (1,1,...,1) is (0,0,...,0).  That is,
!    we allow wrap around.
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Example:
!
!    N = 3
!
!    Input      Output
!    -----      ------
!    0 0 0  =>  0 0 1
!    0 0 1  =>  0 1 0
!    0 1 0  =>  0 1 1
!    0 1 1  =>  1 0 0
!    1 0 0  =>  1 0 1
!    1 0 1  =>  1 1 0
!    1 1 0  =>  1 1 1
!    1 1 1  =>  0 0 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vectors.
!
!    Input/output, integer ( kind = 4 ) UBVEC(N), on output, the successor 
!    to the input vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec(n)
  integer ( kind = 4 ) i

  do i = n, 1, -1

    if ( ubvec(i) == 0 ) then
      ubvec(i) = 1
      return
    end if

    ubvec(i) = 0

  end do

  return
end
subroutine ubvec_next_gray ( n, t )

!*****************************************************************************80
!
!! UBVEC_NEXT_GRAY computes the next UBVEC in the Gray code.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998,
!    ISBN: 0-8493-3988-X,
!    LC: QA164.K73.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of digits in each element.
!    N must be positive.
!
!    Input/output, integer ( kind = 4 ) T(N).
!    On input, T contains an element of the Gray code, that is,
!    each entry T(I) is either 0 or 1.
!    On output, T contains the successor to the input value; this
!    is an element of the Gray code, which differs from the input
!    value in a single position.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) t(n)
  integer ( kind = 4 ) weight

  weight = sum ( t(1:n) )

  if ( mod ( weight, 2 ) == 0 ) then

    if ( t(n) == 0 ) then
      t(n) = 1
    else
      t(n) = 0
    end if

    return

  else

    do i = n, 2, -1
      if ( t(i) == 1 ) then
        if ( t(i-1) == 0 ) then
          t(i-1) = 1
        else
          t(i-1) = 0
        end if
        return
      end if
    end do
!
!  The final element was input.
!  Return the first element.
!
    t(1:n) = 0

  end if

  return
end
subroutine ubvec_next_grlex ( n, ubvec )

!*****************************************************************************80
!
!! UBVEC_NEXT_GRLEX generates the next UBVEC in GRLEX order.
!
!  Discussion:
!
!    N = 3
!
!    Input      Output
!    -----      ------
!    0 0 0  =>  0 0 1
!    0 0 1  =>  0 1 0
!    0 1 0  =>  1 0 0
!    1 0 0  =>  0 1 1
!    0 1 1  =>  1 0 1
!    1 0 1  =>  1 1 0
!    1 1 0  =>  1 1 1
!    1 1 1  =>  0 0 0
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 March 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension.
!
!    Input, integer ( kind = 4 ) UBVEC(N), the binary vector whose 
!    successor is desired.
!
!    Output, integer ( kind = 4 ) UBVEC(N), the successor to the input vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) o
  integer ( kind = 4 ) s
  integer ( kind = 4 ) z
!
!  Initialize locations of 0 and 1.
!
  if ( ubvec(1) == 0 ) then
    z = 1
    o = 0
  else
    z = 0
    o = 1
  end if
!
!  Moving from right to left, search for a "1", preceded by a "0".
!
  do i = n, 2, -1
    if ( ubvec(i) == 1 ) then
      o = i
      if ( ubvec(i-1) == 0 ) then
        z = i - 1
        exit
      end if
    end if
  end do
!
!  UBVEC = 0
!
  if ( o == 0 ) then
    ubvec(n) = 1
!
!  01 never occurs.  So for sure, B(1) = 1.
!
  else if ( z == 0 ) then
    s = sum ( ubvec(1:n) )
    if ( s == n ) then
      ubvec(1:n) = 0
    else
      ubvec(1:n-s-1) = 0
      ubvec(n-s:n) = 1
    end if
!
!  Found the rightmost "01" string.
!  Replace it by "10".
!  Shift following 1's to the right.
!
  else
    ubvec(z) = 1
    ubvec(o) = 0
    s = sum ( ubvec(o+1:n) )
    ubvec(o+1:n-s) = 0
    ubvec(n+1-s:n) = 1
  end if

  return
end
subroutine ubvec_or ( n, ubvec1, ubvec2, ubvec3 )

!*****************************************************************************80
!
!! UBVEC_OR computes the OR of two unsigned binary vectors.
!
!  Discussion:
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the vectors.
!
!    Input, integer ( kind = 4 ) UBVEC1(N), UBVEC2(N), the vectors.
!
!    Input, integer ( kind = 4 ) UBVEC3(N), the OR of the two vectors.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)
  integer ( kind = 4 ) ubvec3(n)

  ubvec3(1:n) = max ( ubvec1(1:n), ubvec2(1:n) )

  return
end
subroutine ubvec_print ( n, ubvec, title )

!*****************************************************************************80
!
!! UBVEC_PRINT prints a UBVEC, with an optional title.
!
!  Discussion:
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) UBVEC(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec(n)
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) trim ( title )
  end if

  do ilo = 1, n, 70
    ihi = min ( ilo + 70 - 1, n )
    write ( *, '(2x,70i1)' ) ubvec(ilo:ihi)
  end do

  return
end
subroutine ubvec_random ( n, seed, ubvec )

!*****************************************************************************80
!
!! UBVEC_RANDOM returns a pseudorandom UBVEC.
!
!  Discussion:
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 December 2014
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
!    Input, integer ( kind = 4 ) N, the order of the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) UBVEC(N), a pseudorandom binary vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec(n)
  integer ( kind = 4 ), parameter :: i4_huge      = 2147483647
  integer ( kind = 4 ), parameter :: i4_huge_half = 1073741823
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'UBVEC_RANDOM - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    if ( i4_huge_half < seed ) then
      ubvec(i) = 0
    else
      ubvec(i) = 1
    end if

  end do

  return
end
subroutine ubvec_rank_gray ( n, ubvec, rank )

!*****************************************************************************80
!
!! UBVEC_RANK_GRAY ranks a UBVEC according to the Gray ordering.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) UBVEC(N), the vector to be printed.
!
!    Output, integer ( kind = 4 ) RANK, the rank of the UBVEC.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec(n)
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) ui4

  call ubvec_to_ui4 ( n, ubvec, ui4 )
  call ui4_rank_gray ( ui4, rank )

  return
end
subroutine ubvec_reverse ( n, ubvec1, ubvec2 )

!*****************************************************************************80
!
!! UBVEC_REVERSE reverses a UBVEC.
!
!  Discussion:
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the vectors.
!
!    Input, integer ( kind = 4 ) UBVEC1(N), the vector to be reversed.
!
!    Output, integer ( kind = 4 ) UBVEC2(N), the reversed vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)

  ubvec2(1:n) = ubvec1(n:1:-1)

  return
end
subroutine ubvec_to_ui4 ( n, ubvec, ui4 )

!*****************************************************************************80
!
!! UBVEC_TO_UI4 makes an unsigned integer from an unsigned binary vector.
!
!  Discussion:
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Example:
!
!    N = 4
!
!         UBVEC   binary UI4
!    ----------  -----  --
!    1  2  3  4
!    ----------
!    0  0  0  1       1  1
!    0  0  1  0      10  2
!    0  0  1  1      11  3
!    0  1  0  0     100  4
!    1  0  0  1    1001  9
!    1  1  1  1    1111 15
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vector.
!
!    Input, integer ( kind = 4 ) UBVEC(N), the binary representation.
!
!    Output, integer ( kind = 4 ) UI4, the integer.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ui4

  ui4 = 0
  do i = 1, n
    ui4 = 2 * ui4 + ubvec(i)
  end do

  return
end
subroutine ubvec_unrank_gray ( rank, n, ubvec )

!*****************************************************************************80
!
!! UBVEC_UNRANK_GRAY unranks a UBVEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RANK, the rank of the UBVEC.
!    0 <= RANK < 2^N.
!
!    Input, integer ( kind = 4 ) N, the size of the UBVEC.
!
!    Output, integer ( kind = 4 ) UBVEC(N), the UBVEC of given rank.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec(n)
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) ui4

  call ui4_unrank_gray ( rank, ui4 )
  call ui4_to_ubvec ( ui4, n, ubvec )

  return
end
subroutine ubvec_unrank_grlex ( rank, n, b )

!*****************************************************************************80
!
!! UBVEC_UNRANK_GRLEX unranks a UBVEC using the GRLEX ordering.
!
!  Discussion:
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RANK, the rank.
!    0 <= RANK < 2^N.
!
!    Input, integer ( kind = 4 ) N, the size of the UBVEC.
!
!    Output, integer ( kind = 4 ) B(N), the UBVEC of the given rank.
!
  integer ( kind = 4 ) n

  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mk
  integer ( kind = 4 ) mk_old
  integer ( kind = 4 ) mk_plus
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_k
  integer ( kind = 4 ) t(n)

  mk = 0

  do k = 0, n

    mk_old = mk
    mk_plus = i4_choose ( n, k )
    mk = mk_old + mk_plus

    if ( rank < mk ) then
      rank_k = rank - mk_old
      call ksubset_colex_unrank ( rank_k, k, n, t )
      b(1:n) = 0
      b(t(1:k)) = 1
      b = b(n:1:-1)
      return
    end if

  end do
!
!  If we got here, the rank is too large.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_UNRANK_GRLEX - Fatal error!'
  write ( *, '(a)' ) '  Input value of rank is too high.'
  stop 1
end
subroutine ubvec_xor ( n, ubvec1, ubvec2, ubvec3 )

!*****************************************************************************80
!
!! UBVEC_XOR computes the exclusive OR of two unsigned binary vectors.
!
!  Discussion:
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the vectors.
!
!    Input, integer ( kind = 4 ) UBVEC1(N), UBVEC2(N), the vectors
!    to be XOR'ed.
!
!    Input, integer ( kind = 4 ) UBVEC3(N), the exclusive OR of the two vectors.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)
  integer ( kind = 4 ) ubvec3(n)

  ubvec3(1:n) = mod ( ubvec1(1:n) + ubvec2(1:n), 2 )

  return
end
subroutine ui4_rank_gray ( gray, rank )

!*****************************************************************************80
!
!! UI4_RANK_GRAY ranks a Gray code.
!
!  Discussion:
!
!    This routine is entirely arithmetical,
!    and does not require access to bit testing and setting routines.
!
!    Given the number GRAY, its ranking is the order in which it would be
!    visited in the Gray code ordering.  The Gray code ordering begins
!
!    Rank  Gray  Gray
!          (Dec) (Bin)
!
!       0     0  0000
!       1     1  0001
!       2     3  0011
!       3     2  0010
!       4     6  0110
!       5     7  0111
!       6     5  0101
!       7     4  0100
!       8    12  0110
!       etc
!
!   This routine is given a Gray code, and has to return the rank.
!
!  Example:
!
!    Gray  Gray  Rank
!    (Dec) (Bin)
!
!     0       0     0
!     1       1     1
!     2      10     3
!     3      11     2
!     4     100     7
!     5     101     6
!     6     110     4
!     7     111     5
!     8    1000    15
!     9    1001    14
!    10    1010    12
!    11    1011    13
!    12    1100     8
!    13    1101     9
!    14    1110    11
!    15    1111    10
!    16   10000    31
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) GRAY, the Gray code to be ranked.
!
!    Output, integer ( kind = 4 ) RANK, the rank of GRAY, and the integer
!    whose Gray code is GRAY.
!
  implicit none

  integer ( kind = 4 ) gray
  integer ( kind = 4 ) gray_copy
  integer ( kind = 4 ) k
  logical last
  logical next
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) two_k

  gray_copy = gray

  if ( gray_copy < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'UI4_RANK_GRAY - Fatal error!'
    write ( *, '(a)' ) '  Input value of GRAY < 0.'
    stop 1
  end if

  if ( gray_copy == 0 ) then
    rank = 0
    return
  end if
!
!  Find TWO_K, the largest power of 2 less than or equal to GRAY.
!
  k = 0
  two_k = 1
  do while ( 2 * two_k <= gray_copy )
    two_k = two_k * 2
    k = k + 1
  end do

  rank = two_k
  last = .true.
  gray_copy = gray_copy - two_k

  do while ( 0 < k )

    two_k = two_k / 2
    k = k - 1

    next = ( two_k <= gray_copy .and. gray_copy < two_k * 2 )

    if ( next ) then
      gray_copy = gray_copy - two_k
    end if

    if ( next .neqv. last ) then
      rank = rank + two_k
      last = .true.
    else
      last = .false.
    end if

  end do

  return
end
subroutine ui4_to_ubvec ( ui4, n, ubvec )

!*****************************************************************************80
!
!! UI4_TO_UBVEC makes an unsigned binary vector from an unsigned integer.
!
!  Discussion:
!
!    A UBVEC is a vector of N binary digits.
!
!    A UBVEC can be interpreted as a binary representation of an
!    unsigned integer, with the first entry being the coefficient of
!    2^(N-1) and the last entry the coefficient of 1.
!
!    UBVEC   #
!    -----  --
!    00000   0
!    00001   1
!    00010   2
!    10000  16
!
!  Example:
!
!     I       UBVEC         binary
!    --  ----------------  ------
!     1  0  0  0  0  0  1       1
!     2  0  0  0  0  1  0      10
!     3  0  0  0  0  1  1      11
!     4  0  0  0  1  0  0     100
!     9  0  0  1  0  0  1    1001
!    57  1  1  0  1  1  1  110111
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) UI4, an integer to be represented.
!
!    Input, integer ( kind = 4 ) N, the dimension of the vector.
!
!    Output, integer ( kind = 4 ) UBVEC(N), the binary representation.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ubvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ui4
  integer ( kind = 4 ) ui4_copy

  if ( ui4 < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'UI4_TO_UBVEC - Fatal error!'
    write ( *, '(a)' ) '  Input integer is negative.'
    stop 1
  end if

  ui4_copy = ui4

  do i = n, 1, -1

    ubvec(i) = mod ( ui4_copy, 2 )

    ui4_copy = ui4_copy / 2

  end do

  return
end
subroutine ui4_unrank_gray ( rank, gray )

!*****************************************************************************80
!
!! UI4_UNRANK_GRAY unranks a Gray code.
!
!  Discussion:
!
!    This routine is entirely arithmetical,
!    and does not require access to bit testing and setting routines.
!
!    The binary values of the Gray codes of successive integers differ in
!    just one bit.
!
!    The sequence of Gray codes for 0 to (2^N)-1 can be interpreted as a
!    Hamiltonian cycle on a graph of the cube in N dimensions.
!
!  Example:
!
!    Rank  Gray  Gray
!          (Dec) (Bin)
!
!     0     0       0
!     1     1       1
!     2     3      11
!     3     2      10
!     4     6     110
!     5     7     111
!     6     5     101
!     7     4     100
!     8    12    1100
!     9    14    1001
!    10    12    1010
!    11    13    1011
!    12     8    1100
!    13     9    1101
!    14    11    1110
!    15    10    1111
!    16    31   10000
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RANK, the integer whose Gray code is desired.
!
!    Output, integer ( kind = 4 ) GRAY, the Gray code of the given rank.
!
  implicit none

  integer ( kind = 4 ) gray
  integer ( kind = 4 ) k
  logical last
  logical next
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_copy
  integer ( kind = 4 ) two_k

  if ( rank <= 0 ) then
    gray = 0
    return
  end if

  rank_copy = rank
  k = 0
  two_k = 1
  do while ( 2 * two_k <= rank_copy )
    two_k = two_k * 2
    k = k + 1
  end do

  gray = two_k
  rank_copy = rank_copy - two_k
  next = .true.

  do while ( 0 < k )

    two_k = two_k / 2
    k = k - 1

    last = next
    next = ( two_k <= rank_copy .and. rank_copy <= two_k * 2 )

    if ( next .neqv. last ) then
      gray = gray + two_k
    end if

    if ( next ) then
      rank_copy = rank_copy - two_k
    end if

  end do

  return
end
