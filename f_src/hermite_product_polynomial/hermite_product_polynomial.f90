subroutine comp_enum ( n, k, number )

!*****************************************************************************80
!
!! COMP_ENUM returns the number of compositions of the integer N into K parts.
!
!  Discussion:
!
!    A composition of the integer N into K parts is an ordered sequence
!    of K nonnegative integers which sum to N.  The compositions (1,2,1)
!    and (1,1,2) are considered to be distinct.
!
!    The 28 compositions of 6 into three parts are:
!
!      6 0 0,  5 1 0,  5 0 1,  4 2 0,  4 1 1,  4 0 2,
!      3 3 0,  3 2 1,  3 1 2,  3 0 3,  2 4 0,  2 3 1,
!      2 2 2,  2 1 3,  2 0 4,  1 5 0,  1 4 1,  1 3 2,
!      1 2 3,  1 1 4,  1 0 5,  0 6 0,  0 5 1,  0 4 2,
!      0 3 3,  0 2 4,  0 1 5,  0 0 6.
!
!    The formula for the number of compositions of N into K parts is
!
!      Number = ( N + K - 1 )! / ( N! * ( K - 1 )! )
!
!    Describe the composition using N '1's and K-1 dividing lines '|'.
!    The number of distinct permutations of these symbols is the number
!    of compositions.  This is equal to the number of permutations of
!    N+K-1 things, with N identical of one kind and K-1 identical of another.
!
!    Thus, for the above example, we have:
!
!      Number = ( 6 + 3 - 1 )! / ( 6! * (3-1)! ) = 28
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
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Second Edition,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the integer whose compositions are desired.
!
!    Input, integer ( kind = 4 ) K, the number of parts in the composition.
!
!    Output, integer ( kind = 4 ) NUMBER, the number of compositions of N
!    into K parts.
!
  implicit none

  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  integer ( kind = 4 ) number

  number = i4_choose ( n + k - 1, n )

  return
end
subroutine comp_next_grlex ( kc, xc )

!*****************************************************************************80
!
!! COMP_NEXT_GRLEX returns the next composition in grlex order.
!
!  Discussion:
!
!    Example:
!
!    KC = 3
!
!    #   XC(1  XC(2) XC(3)  Degree
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
!    11 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) KC, the number of parts of the composition.
!    1 <= KC.
!
!    Input/output, integer ( kind = 4 ) XC(KC), the current composition.
!    Each entry of XC must be nonnegative.
!    On return, XC has been replaced by the next composition in the
!    grlex order.
!
  implicit none

  integer ( kind = 4 ) kc

  integer ( kind = 4 ) i
  integer ( kind = 4 ) im1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) t
  integer ( kind = 4 ) xc(kc)
!
!  Ensure that 1 <= KC.
!
  if ( kc < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COMP_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  KC < 1'
    stop 1
  end if
!
!  Ensure that 0 <= XC(I).
!
  do i = 1, kc
    if ( xc(i) < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COMP_NEXT_GRLEX - Fatal error!'
      write ( *, '(a)' ) '  XC(I) < 0'
      stop 1
    end if
  end do
!
!  Find I, the index of the rightmost nonzero entry of X.
!
  i = 0
  do j = kc, 1, -1
    if ( 0 < xc(j) ) then
      i = j
      exit
    end if
  end do
!
!  set T = X(I)
!  set XC(I) to zero,
!  increase XC(I-1) by 1,
!  increment XC(KC) by T-1.
!
  if ( i == 0 ) then
    xc(kc) = 1
    return
  else if ( i == 1 ) then
    t = xc(1) + 1
    im1 = kc
  else if ( 1 < i ) then
    t = xc(i)
    im1 = i - 1
  end if

  xc(i) = 0
  xc(im1) = xc(im1) + 1
  xc(kc) = xc(kc) + t - 1

  return
end
subroutine comp_random_grlex ( kc, rank1, rank2, seed, xc, rank )

!*****************************************************************************80
!
!! COMP_RANDOM_GRLEX: random composition with degree less than or equal to NC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) KC, the number of parts in the composition.
!
!    Input, integer ( kind = 4 ) RANK1, RANK2, the lowest and highest ranks 
!    to consider.  1 <= RANK1 <= RANK2.
!
!    Input/output, integer ( kind = 4 ) SEED, the random number seed.
!
!    Output, integer ( kind = 4 ) XC(KC), the random composition.
!
!    Output, integer ( kind = 4 ) RANK, the rank of the composition.
!
  implicit none

  integer ( kind = 4 ) kc

  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank1
  integer ( kind = 4 ) rank2
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) xc(kc)
!
!  Ensure that 1 <= KC.
!
  if ( kc < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'COMP_RANDOM_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  KC < 1'
    stop 1
  end if
!
!  Ensure that 1 <= RANK1.
!
  if ( rank1 < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'COMP_RANDOM_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  RANK1 < 1'
    stop 1
  end if
!
!  Ensure that RANK1 <= RANK2.
!
  if ( rank2 < rank1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'COMP_RANDOM_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  RANK2 < RANK1'
    stop 1
  end if

  rank = i4_uniform_ab ( rank1, rank2, seed )

  call comp_unrank_grlex ( kc, rank, xc )

  return
end
subroutine comp_rank_grlex ( kc, xc, rank )

!*****************************************************************************80
!
!! COMP_RANK_GRLEX computes the graded lexicographic rank of a composition.
!
!  Discussion:
!
!    The graded lexicographic ordering is used, over all KC-compositions
!    for NC = 0, 1, 2, ...
!
!    For example, if KC = 3, the ranking begins:
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
!    09 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) KC, the number of parts in the composition.
!    1 <= KC.
!
!    Input, integer ( kind = 4 ) XC(KC), the composition.
!    For each 1 <= I <= KC, we have 0 <= XC(I).
!
!    Output, integer ( kind = 4 ) RANK, the rank of the composition.
!
  implicit none

  integer ( kind = 4 ) kc

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) i4vec_sum
  integer ( kind = 4 ) j
  integer ( kind = 4 ) ks
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) ns
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) tim1
  integer ( kind = 4 ) xc(kc)
  integer ( kind = 4 ) xs(kc-1)
!
!  Ensure that 1 <= KC.
!
  if ( kc < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'COMP_RANK_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  KC < 1'
    stop 1
  end if
!
!  Ensure that 0 <= XC(I).
!
  do i = 1, kc
    if ( xc(i) < 0 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'COMP_RANK_GRLEX - Fatal error!'
      write ( *, '(a)' ) '  XC(I) < 0'
      stop 1
    end if
  end do
!
!  NC = sum ( XC )
!
  nc = i4vec_sum ( kc, xc )
!
!  Convert to KSUBSET format.
!
  ns = nc + kc - 1
  ks = kc - 1

  xs(1) = xc(1) + 1
  do i = 2, kc - 1
    xs(i) = xs(i-1) + xc(i) + 1
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

  do n = 0, nc - 1
    rank = rank + i4_choose ( n + kc - 1, n )
  end do

  return
end
subroutine comp_unrank_grlex ( kc, rank, xc )

!*****************************************************************************80
!
!! COMP_UNRANK_GRLEX computes the composition of given grlex rank.
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
!    Input, integer ( kind = 4 ) KC, the number of parts of the composition.
!    1 <= KC.
!
!    Input, integer ( kind = 4 ) RANK, the rank of the composition.
!    1 <= RANK.
!
!    Output, integer ( kind = 4 ) XC(KC), the composition of the given rank.
!    For each I, 0 <= XC(I) <= NC, and 
!    sum ( 1 <= I <= KC ) XC(I) = NC.
!
  implicit none

  integer ( kind = 4 ) kc

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) j
  integer ( kind = 4 ) ks
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) nksub
  integer ( kind = 4 ) ns
  integer ( kind = 4 ) r
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank1
  integer ( kind = 4 ) rank2
  integer ( kind = 4 ) xc(kc)
  integer ( kind = 4 ) xs(kc-1)
!
!  Ensure that 1 <= KC.
!
  if ( kc < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'COMP_UNRANK_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  KC < 1'
    stop 1
  end if
!
!  Ensure that 1 <= RANK.
!
  if ( rank < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'COMP_UNRANK_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  RANK < 1'
    stop 1
  end if
!
!  Determine the appropriate value of NC.
!  Do this by adding up the number of compositions of sum 0, 1, 2, 
!  ..., without exceeding RANK.  Moreover, RANK - this sum essentially
!  gives you the rank of the composition within the set of compositions
!  of sum NC.  And that's the number you need in order to do the
!  unranking.
!
  rank1 = 1
  nc = -1
  do
    nc = nc + 1
    r = i4_choose ( nc + kc - 1, nc )
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
  ks = kc - 1
  ns = nc + kc - 1

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
  xc(1) = xs(1) - 1
  do i = 2, kc - 1
    xc(i) = xs(i) - xs(i-1) - 1
  end do
  xc(kc) = ns - xs(ks)

  return
end
subroutine hep_coefficients ( n, o, c, f )

!*****************************************************************************80
!
!! HEP_COEFFICIENTS: coefficients of He(n,x).
!
!  Discussion:
!
!    He(i,x) represents the probabilist's Hermite polynomial.
!
!  First terms:
!
!    N/K     0     1      2      3       4     5      6    7      8    9   10
!
!     0      1
!     1      0     1
!     2     -1     0      1
!     3      0    -3      0      1
!     4      3     0     -6      0       1
!     5      0    15      0    -10       0     1
!     6    -15     0     45      0     -15     0      1
!     7      0  -105      0    105       0   -21      0     1
!     8    105     0   -420      0     210     0    -28     0      1
!     9      0   945      0  -1260       0   378      0   -36      0   1
!    10   -945     0   4725      0   -3150     0    630     0    -45   0    1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the degree of the polynomial to compute.
!
!    Output, integer ( kind = 4 ) O, the number of coefficients.
!
!    Output, real ( kind = 8 ) C((N+2)/2), the coefficients of the Legendre
!    polynomial of degree N.
!
!    Output, integer ( kind = 4 ) F((N+2)/2), the exponents.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) ct(0:n,0:n)
  real ( kind = 8 ) c((n+2)/2)
  integer ( kind = 4 ) f((n+2)/2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) o

  ct(0:n,0:n) = 0.0D+00

  ct(0,0) = 1.0D+00

  if ( 0 < n ) then
    ct(1,1) = 1.0D+00
  end if

  do i = 2, n
    ct(i,0)     =               - real ( i - 1, kind = 8 ) * ct(i-2,0)
    ct(i,1:i-2) = ct(i-1,0:i-3) - real ( i - 1, kind = 8 ) * ct(i-2,1:i-2)
    ct(i,  i-1) = ct(i-1,  i-2)
    ct(i,  i  ) = ct(i-1,  i-1)
  end do
!
!  Extract the nonzero data from the alternating columns of the last row.
!
  o = ( n + 2 ) / 2

  k = o
  do j = n, 0, -2
    c(k) = ct(n,j)
    f(k) = j
    k = k - 1
  end do

  return
end
subroutine hep_value ( n, o, x, v )

!*****************************************************************************80
!
!! HEP_VALUE evaluates the Hermite polynomials He(n,x).
!
!  Discussion:
!
!    He(i,x) represents the probabilist's Hermite polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, integer ( kind = 4 ) O, the degree of the polynomial.
!
!    Input, real ( kind = 8 ) X(N), the evaluation points.
!
!    Output, real ( kind = 8 ) V(N), the values of the polynomials 
!    of order O at the points X.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) o

  integer ( kind = 4 ) j
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) vtable(n,0:o)
  real ( kind = 8 ) x(n)

  vtable(1:n,0) = 1.0D+00

  if ( 1 <= o ) then
 
    vtable(1:n,1) = x(1:n)
 
    do j = 2, o
      vtable(1:n,j) =                   x(1:n) * vtable(1:n,j-1) &
                    - real ( j - 1, kind = 8 ) * vtable(1:n,j-2)
    end do
 
  end if

  v(1:n) = vtable(1:n,o)

  return
end
subroutine hep_values ( n_data, n, x, fx )

!*****************************************************************************80
!
!! HEP_VALUES: tabulated values of He(i,x).
!
!  Discussion:
!
!    He(i,x) represents the probabilist's Hermite polynomial.
!
!    In Mathematica, the function can be evaluated by:
!
!      He(n,x) = HermiteH[n,x/Sqrt[2]] / Sqrt [ 2^n ] 
!
!  First terms:
!
!   1
!   X
!   X^2  -  1
!   X^3  -  3 X
!   X^4  -  6 X^2 +   3
!   X^5  - 10 X^3 +  15 X
!   X^6  - 15 X^4 +  45 X^2 -   15
!   X^7  - 21 X^5 + 105 X^3 -  105 X
!   X^8  - 28 X^6 + 210 X^4 -  420 X^2 +  105
!   X^9  - 36 X^7 + 378 X^5 - 1260 X^3 +  945 X
!   X^10 - 45 X^8 + 630 X^6 - 3150 X^4 + 4725 X^2 - 945
!
!  Recursion:
!
!    He(0,X) = 1,
!    He(1,X) = X,
!    He(N,X) = X * He(N-1,X) - (N-1) * He(N-2,X)
!
!  Norm:
!
!    Integral ( -oo < X < +oo ) exp ( - 0.5 * X^2 ) * He(M,X) He(N,X) dX 
!    = sqrt ( 2 * pi ) * N! * delta ( N, M )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) N, the order of the polynomial.
!
!    Output, real ( kind = 8 ) X, the point where the polynomial is evaluated.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 18

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    1.000000000000000D+00, &
    5.000000000000000D+00, &
    24.00000000000000D+00, &
    110.0000000000000D+00, &
    478.0000000000000D+00, &
    1950.000000000000D+00, &
    7360.000000000000D+00, &
    25100.00000000000D+00, &
    73980.00000000000D+00, &
    169100.0000000000D+00, &
    179680.0000000000D+00, &
   -792600.0000000000D+00, &
   -5939480.000000000D+00, &
    0.000000000000000D+00, &
    6.281250000000000D+00, &
    6.000000000000000D+00, &
    18.00000000000000D+00, &
    90150.00000000000D+00 /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ), save, dimension ( n_max ) :: n_vec = (/ &
     0,  1,  2, &
     3,  4,  5, &
     6,  7,  8, &
     9, 10, 11, &
    12,  5,  5, &
     5,  5,  5 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    5.0D+00, &
    5.0D+00, &
    5.0D+00, &
    5.0D+00, &
    5.0D+00, &
    5.0D+00, &
    5.0D+00, &
    5.0D+00, &
    5.0D+00, &
    5.0D+00, &
    5.0D+00, &
    5.0D+00, &
    5.0D+00, &
    0.0D+00, &
    0.5D+00, &
    1.0D+00, &
    3.0D+00, &
    1.0D+01 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    n = n_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine hepp_to_polynomial ( m, l, o_max, o, c, e )

!*****************************************************************************80
!
!! HEPP_TO_POLYNOMIAL writes a Hermite Product Polynomial as a polynomial.
!
!  Discussion:
!
!    He(i,x) represents the probabilist's Hermite polynomial.
!
!    For example, if 
!      M = 3,
!      L = ( 1, 0, 2 ),
!    then
!      He(1,0,2)(X,Y,Z) 
!      = He(1)(X) * He(0)(Y) * He(2)(Z)
!      = X * 1 * ( Z^3-3Z)
!      = - 3XZ + X Z^3
!    so
!      O = 2 (2 nonzero terms)
!      C = -3.0
!           1.0
!      E =  8    <-- index in 3-space of exponent (1,0,1)
!          23    <-- index in 3-space of exponent (1,0,3)
!
!    The output value of O is no greater than
!      O_MAX = product ( 1 <= I <= M ) (L(I)+2)/2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) L(M), the index of each polynomial factor.  
!    0 <= L(*).
!
!    Input, integer ( kind = 4 ) O_MAX, an upper limit on the size of the 
!    output arrays.
!      O_MAX = product ( 1 <= I <= M ) (L(I)+2)/2.
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
  integer ( kind = 4 ) o_max

  real ( kind = 8 ) c(o_max)
  real ( kind = 8 ) c1(o_max)
  real ( kind = 8 ) c2(o_max)
  integer ( kind = 4 ) e(o_max)
  integer ( kind = 4 ) e1(o_max)
  integer ( kind = 4 ) f2(o_max)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) l(m)
  integer ( kind = 4 ) o
  integer ( kind = 4 ) o1
  integer ( kind = 4 ) o2
  integer ( kind = 4 ) p(m)

  o1 = 1
  c1(1) = 1.0D+00
  e1(1) = 1
!
!  Implicate one factor at a time.
!
  do i = 1, m

    call hep_coefficients ( l(i), o2, c2, f2 )
    
    o = 0

    do j2 = 1, o2
      do j1 = 1, o1
        o = o + 1
        c(o) = c1(j1) * c2(j2)
        if ( 1 < i ) then
          call mono_unrank_grlex ( i - 1, e1(j1), p(1:i-1) )
        end if
        p(i) = f2(j2)
        call mono_rank_grlex ( i, p, e(o) )
      end do
    end do

    call polynomial_sort ( o, c, e )
    call polynomial_compress ( o, c, e, o, c, e )

    o1 = o
    c1(1:o1) = c(1:o)
    e1(1:o1) = e(1:o)

  end do

  return
end
subroutine hepp_value ( m, n, o, x, v )

!*****************************************************************************80
!
!! HEPP_VALUE evaluates a Hermite Product Polynomial at several points X.
!
!  Discussion:
!
!    He(i,x) represents the probabilist's Hermite polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 October 2014
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
!    Input, integer ( kind = 4 ) O(M), the degree of the polynomial factors.
!    0 <= O(*).
!
!    Input, real ( kind = 8 ) X(M,N), the evaluation points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the Product 
!    Polynomial of degree O at the points X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) o(m)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) vi(n)
  real ( kind = 8 ) x(m,n)

  v(1:n) = 1.0D+00

  do i = 1, m

    call hep_value ( n, o(i), x(i,1:n), vi )

    v(1:n) = v(1:n) * vi(1:n)

  end do

  return
end
function i4_choose ( n, k )

!*****************************************************************************80
!
!! I4_CHOOSE computes the binomial coefficient C(N,K).
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
  integer ( kind = 4 ), parameter :: base = 1
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) istart
  integer ( kind = 4 ) p(n)

  call perm_check ( n, p, base, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_PERMUTE - Fatal error!'
    write ( *, '(a)' ) '  PERM_CHECK rejects this permutation.'
    stop 1
  end if
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
subroutine mono_rank_grlex ( m, x, rank )

!*****************************************************************************80
!
!! MONO_RANK_GRLEX computes the graded lexicographic rank of a monomial.
!
!  Discussion:
!
!    The graded lexicographic ordering is used, over all monomials
!    of spatial dimension M, for monomial degree NM = 0, 1, 2, ...
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
!    Input, integer ( kind = 4 ) M, spatial dimension.
!    1 <= M.
!
!    Input, integer ( kind = 4 ) XC(M), the composition.
!    For each 1 <= I <= M, we have 0 <= XC(I).
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
!  Ensure that 1 <= M.
!
  if ( m < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_RANK_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  M < 1'
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
!    14 January 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!    1 <= M.
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
    write ( *, '(a,i4)' ) '  RANK = ', rank
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
subroutine perm_check ( n, p, base, ierror )

!*****************************************************************************80
!
!! PERM_CHECK checks that a vector represents a permutation.
!
!  Discussion:
!
!    The routine verifies that each of the integers from BASE to
!    to BASE+N-1 occurs among the N entries of the permutation.
!
!    Set the input quantity BASE to 0, if P is a 0-based permutation,
!    or to 1 if P is a 1-based permutation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 October 2008
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
!    Input, integer ( kind = 4 ) BASE, the index base.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, the array represents a permutation.
!    nonzero, the array does not represent a permutation.  The smallest
!    missing value is equal to IERROR.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) base
  integer ( kind = 4 ) find
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seek

  ierror = 0

  do seek = base, base + n - 1

    ierror = 1

    do find = 1, n
      if ( p(find) == seek ) then
        ierror = 0
        exit
      end if
    end do

    if ( ierror /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PERM_CHECK - Fatal error!'
      write ( *, '(a)' ) '  The input array does not represent'
      write ( *, '(a)' ) '  a proper permutation.'
      stop 1
    end if

  end do

  return
end
subroutine polynomial_compress ( o1, c1, e1, o2, c2, e2 )

!*****************************************************************************80
!
!! POLYNOMIAL_COMPRESS compresses a polynomial.
!
!  Discussion:
!
!    The function polynomial_sort ( ) should be called first.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 January 2014
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

  get = 0
  put = 0

  do while ( get < o1 )

    get = get + 1

    if ( abs ( c1(get) ) <= r8_epsilon_sqrt ) then
      cycle
    end if

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
  integer ( kind = 4 ), parameter :: base = 1
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) istart
  integer ( kind = 4 ) p(n)

  call perm_check ( n, p, base, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_PERMUTE - Fatal error!'
    write ( *, '(a)' ) '  PERM_CHECK rejects this permutation.'
    stop 1
  end if
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
subroutine r8vec_uniform_ab ( n, a, b, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_AB returns a scaled pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Each dimension ranges from A to B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 May 2007
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
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

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
