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
subroutine i4mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! I4MAT_PRINT prints an I4MAT.
!
!  Discussion:
!
!    An I4MAT is a rectangular array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, integer ( kind = 4 ) A(M,N), the matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  ilo = 1
  ihi = m
  jlo = 1
  jhi = n

  call i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

  return
end
subroutine i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! I4MAT_PRINT_SOME prints some of an I4MAT.
!
!  Discussion:
!
!    An I4MAT is a rectangular array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, integer ( kind = 4 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 10
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  character ( len = 8 )  ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8)' ) j
    end do

    write ( *, '(''  Col '',10a8)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        write ( ctemp(j2), '(i8)' ) a(i,j)

      end do

      write ( *, '(i5,a,10a8)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

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
subroutine interpolant_value ( d, r, pn, po, pc, pe, pd, ni, xi, yi )

!*****************************************************************************80
!
!! INTERPOLANT_VALUE evaluates a Lagrange interpolant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) R, the maximum number of terms in a polynomial.
!
!    Input, integer ( kind = 4 ) PN, the number of polynomials.
!
!    Input, integer ( kind = 4 ) PO(PN), the "order" of the polynomials.
!
!    Input, real ( kind = 8 ) PC(PN,R), the coefficients of the polynomial.
!
!    Input, integer ( kind = 4 ) PE(PN,R), the indices of the exponents of 
!    the polynomial.
!
!    Input, real ( kind = 8 ) PD(PN), the coefficient of each polynomial.  
!    For a Lagrange interpolant, this is the data value at each Lagrange point.
!
!    Input, integer ( kind = 4 ) NI, the number of interpolant 
!    evaluation points.
!
!    Input, real ( kind = 8 ) XI(D,NI), the coordinates of the interpolation 
!    evaluation points.
!
!    Output, real ( kind = 8 ) YI(NI), the value of the interpolant at XI.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) ni
  integer ( kind = 4 ) pn
  integer ( kind = 4 ) r

  integer ( kind = 4 ) j
  integer ( kind = 4 ) oj
  real ( kind = 8 ) pc(pn,r)
  real ( kind = 8 ) pd(pn)
  integer ( kind = 4 ) pe(pn,r)
  integer ( kind = 4 ) po(pn)
  real ( kind = 8 ) value(ni)
  real ( kind = 8 ) xi(d,ni)
  real ( kind = 8 ) yi(ni)

  yi(1:ni) = 0.0D+00

  do j = 1, pn
    oj = po(j)
    call polynomial_value ( d, oj, pc(j,1:oj), pe(j,1:oj), ni, xi, value )
    yi(1:ni) = yi(1:ni) + pd(j) * value(1:ni)
  end do 

  return
end
subroutine lagrange_complete ( d, n, r, nd, xd, po, pc, pe )

!*****************************************************************************80
!
!! LAGRANGE_COMPLETE: Complete Lagrange polynomial basis from data.
!
!  Discussion:
!
!    This function represents algorithm 4.1 in the reference.
!
!    This function is given XD, a set of ND distinct data points in a 
!    D dimensional space, and returns information defining a set of 
!    ND Lagrange polynomials L(i)(X) with the property that:
!
!      L(i)(XD(j)) = delta(i,j)
!
!    In order for this computation to be carried out, it is necessary that
!    ND, the number of data points, is equal to R, the dimension of the 
!    space of polynomials in D dimensions and total degree N or less, that is:
!
!      ND = R = Choose ( N + D, N )
!
!    There will be ND polynomials returned.  Each polynomial can have
!    as many as R coefficients.
!
!    Each polynomial is given as a vector, with each entry corresponding
!    to a nonzero coefficient.  In particular, for polynomial L(i)(X):
!
!      PO(i) is the order, that is, the number of nonzero coefficients;
!      PC(i,j), for 1 <= j <= PO(i), is the coefficient of the J-th term.
!      PE(i,j), for 1 <= j <= PO(i), encodes the exponents of the J-th term.
!
!    The exponent codes are a compact way of recording the exponent vector
!    associated with each monomial.  If PE(i,j) = k, then the corresponding
!    vector of D exponents can be determined by:
!
!      E = mono_unrank_grlex ( D, k );
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Tomas Sauer, Yuan Xu,
!    On multivariate Lagrange interpolation,
!    Mathematics of Computation,
!    Volume 64, Number 211, July 1995, pages 1147-1170.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the maximum total degree.
!
!    Input, integer ( kind = 4 ) R, the number of monomials in D dimensions 
!    of total degree N or less.
!
!    Input, integer ( kind = 4 ) ND, the number of data points.
!    This function requires that the ND is equal to R!
!
!    Input, real ( kind = 8 ) XD(D,ND), the data points.
!    The data points must be distinct.
!
!    Output, integer ( kind = 4 ) PO(ND), the order (number of nonzero
!    coefficients), for the Lagrange basis polynomials.
!
!    Output, real ( kind = 8 ) PC(ND,R), the coefficients for the 
!    Lagrange basis polynomials.
!
!    Output, integer ( kind = 4 ) PE(ND,R), the exponent indices for the 
!    Lagrange basis polynomials.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) r

  real ( kind = 8 ) c(r)
  real ( kind = 8 ) d_max
  real ( kind = 8 ) d_min
  real ( kind = 8 ) d_tol
  integer ( kind = 4 ) e(r)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  integer ( kind = 4 ) oi
  integer ( kind = 4 ) oj
  integer ( kind = 4 ) ok
  real ( kind = 8 ) pc(r,r)
  integer ( kind = 4 ) pe(r,r)
  integer ( kind = 4 ) po(r)
  real ( kind = 8 ) qc(r,r)
  integer ( kind = 4 ) qe(r,r)
  integer ( kind = 4 ) qo(r)
  real ( kind = 8 ) r8_epsilon
  real ( kind = 8 ) value(1)
  real ( kind = 8 ) xd(d,nd)
!
!  Verify that R is correct.
!
  if ( r /= mono_upto_enum ( d, n ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_COMPLETE - Fatal error!'
    write ( *, '(a)' ) '  The value R is not correct.'
    stop 1
  end if

  if ( r /= nd ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_COMPLETE - Fatal error!'
    write ( *, '(a,i4)' ) '  The value R = ', r
    write ( *, '(a,i4)' ) '  does not equal ND = ', nd
    stop 1
  end if
!
!  Verify that the points are sufficiently distinct.
!
  call r8col_separation ( d, nd, xd, d_min, d_max )
  d_tol = sqrt ( r8_epsilon ( ) )

  if ( d_min < d_tol ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_COMPLETE - Fatal error!'
    write ( *, '(a)' ) '  Some points are too close!'
    write ( *, '(a,g14.6)' ) '  Minimum data point separation is = ', d_min
    stop 1
  end if
!
!  Initialize the polynomials Q which span the space of N-th degree
!  polynomials.
!
!  Default option:
!  * All ND-dimensional monomials of degree N or less.
!    in 2D, this might be 1, x, y, x^2, xy, y^2, ...
!  
  do k = 1, r
    qo(k) = 1
    qc(k,1) = 1.0D+00
    qc(k,2:r) = 0.0D+00
    qe(k,1) = k
    qe(k,2:r) = 0
  end do
!
!  Set space for the P polynomials.
!
  po(1:r) = 0
  pc(1:r,1:r) = 0.0D+00
  pe(1:r,1:r) = 0

  do k = 1, nd
!
!  Find the first polynomial Q(K:R)(X) which is nonzero at X(K).
!
    i = r + 1

    do j = k, r
      o = qo(j)
      call polynomial_value ( d, o, qc(j,1:o), qe(j,1:o), 1, xd(1:d,k), value )
      if ( value(1) /= 0.0D+00 ) then
        i = j
        exit
      end if
    end do

    if ( i == r + 1 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LAGRANGE_COMPLETE - Fatal error!'
      write ( *, '(a)' ) '  I = R+1.'
      stop 1
    end if
!
!  Define P(K)(X) = Q(I)(X) / Q(I)(X(k)
!
    o = qo(i)
    po(k) = qo(i)
    pc(k,1:o) = qc(i,1:o) / value(1)
    pe(k,1:o) = qe(i,1:o)
!
!  Modify P(1:k-1)(X).
!
    do j = 1, k - 1

      oj = po(j)
      ok = po(k)

      call polynomial_value ( d, oj, pc(j,1:oj), pe(j,1:oj), 1, xd(1:d,k), &
        value )

      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, pc(j,1:oj), pe(j,1:oj), o, c, e )

      po(j) = o
      pc(j,1:o) = c(1:o)
      pe(j,1:o) = e(1:o)

    end do
!
!  Modify Q(I:downto:K+1)
!
    do j = i, k + 1, -1

      oj = qo(j-1)
      ok = po(k)

      call polynomial_value ( d, oj, qc(j-1,1:oj), qe(j-1,1:oj), &
        1, xd(1:d,k), value )
 
      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, qc(j-1,1:oj), qe(j-1,1:oj), o, c, e )

      qo(j) = o
      qc(j,1:o) = c(1:o)
      qe(j,1:o) = e(1:o)

    end do
!
!  Modify Q(I+1:R)
!
    do j = i + 1, r

      oj = qo(j)
      ok = po(k)

      call polynomial_value ( d, oj, qc(j,1:oj), qe(j,1:oj), &
        1, xd(1:d,k), value )

      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, qc(j,1:oj), qe(j,1:oj), o, c, e )

      qo(j) = o
      qc(j,1:o) = c(1:o)
      qe(j,1:o) = e(1:o)

    end do

  end do
!
!  Get rid of tiny coefficients.
!
  do i = 1, nd
    oi = po(i)
    call polynomial_compress ( po(i), pc(i,1:oi), pe(i,1:oi), &
                               po(i), pc(i,1:oi), pe(i,1:oi) )
  end do

  return
end
subroutine lagrange_complete2 ( d, n, r, nd, xd, po, pc, pe )

!*****************************************************************************80
!
!! LAGRANGE_COMPLETE2: Complete Lagrange polynomial basis from data.
!
!  Discussion:
!
!    This function represents algorithm 4.1 in the reference,
!    with the further modification that a form of "pivoting" is used
!    to select the next polynomial as the one with maximum absolute
!    value at the current node.
!
!    This function is given XD, a set of ND distinct data points in a 
!    D dimensional space, and returns information defining a set of 
!    ND Lagrange polynomials L(i)(X) with the property that:
!
!      L(i)(XD(j)) = delta(i,j)
!
!    In order for this computation to be carried out, it is necessary that
!    ND, the number of data points, is equal to R, the dimension of the 
!    space of polynomials in D dimensions and total degree N or less, that is:
!
!      ND = R = Choose ( N + D, N )
!
!    There will be ND polynomials returned.  Each polynomial can have
!    as many as R coefficients.
!
!    Each polynomial is given as a vector, with each entry corresponding
!    to a nonzero coefficient.  In particular, for polynomial L(i)(X):
!
!      PO(i) is the order, that is, the number of nonzero coefficients;
!      PC(i,j), for 1 <= j <= PO(i), is the coefficient of the J-th term.
!      PE(i,j), for 1 <= j <= PO(i), encodes the exponents of the J-th term.
!
!    The exponent codes are a compact way of recording the exponent vector
!    associated with each monomial.  If PE(i,j) = k, then the corresponding
!    vector of D exponents can be determined by:
!
!      E = mono_unrank_grlex ( D, k );
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Tomas Sauer, Yuan Xu,
!    On multivariate Lagrange interpolation,
!    Mathematics of Computation,
!    Volume 64, Number 211, July 1995, pages 1147-1170.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the maximum total degree.
!
!    Input, integer ( kind = 4 ) R, the number of monomials in D dimensions 
!    of total degree N or less.
!
!    Input, integer ( kind = 4 ) ND, the number of data points.
!    This function requires that the ND is equal to R.  
!
!    Input, real ( kind = 8 ) XD(D,ND), the data points,
!    which must be distinct.
!
!    Output, integer ( kind = 4 ) PO(ND), the order (number of nonzero
!    coefficients), for the Lagrange basis polynomials.
!
!    Output, real ( kind = 8 ) PC(ND,R), the coefficients for the 
!    Lagrange basis polynomials.
!
!    Output, integer ( kind = 4 ) PE(ND,R), the exponent indices for the 
!    Lagrange basis polynomials.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) r

  real ( kind = 8 ) c(r)
  real ( kind = 8 ) d_max
  real ( kind = 8 ) d_min
  real ( kind = 8 ) d_tol
  integer ( kind = 4 ) e(r)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  integer ( kind = 4 ) oi
  integer ( kind = 4 ) oj
  integer ( kind = 4 ) ok
  real ( kind = 8 ) pc(r,r)
  integer ( kind = 4 ) pe(r,r)
  integer ( kind = 4 ) po(r)
  real ( kind = 8 ) qc(r,r)
  integer ( kind = 4 ) qe(r,r)
  integer ( kind = 4 ) qo(r)
  real ( kind = 8 ) r8_epsilon
  real ( kind = 8 ) value(1)
  real ( kind = 8 ) value_max
  real ( kind = 8 ) xd(d,nd)
!
!  Verify that R is correct.
!
  if ( r /= mono_upto_enum ( d, n ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_COMPLETE2 - Fatal error!'
    write ( *, '(a)' ) '  The value R is not correct.'
    stop 1
  end if

  if ( r /= nd ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_COMPLETE2 - Fatal error!'
    write ( *, '(a,i4)' ) '  The value R = ', r
    write ( *, '(a,i4)' ) '  does not equal ND = ', nd
    stop 1
  end if
!
!  Verify that the points are sufficiently distinct.
!
  call r8col_separation ( d, nd, xd, d_min, d_max )
  d_tol = sqrt ( r8_epsilon ( ) )

  if ( d_min < d_tol ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_COMPLETE2 - Fatal error!'
    write ( *, '(a)' ) '  Some points are too close!'
    write ( *, '(a,g14.6)' ) '  Minimum data point separation is = ', d_min
    stop 1
  end if
!
!  Initialize the polynomials Q which span the space of N-th degree
!  polynomials.
!
!  Default option:
!  * All ND-dimensional monomials of degree N or less.
!    in 2D, this might be 1, x, y, x^2, xy, y^2, ...
!  
  do k = 1, r
    qo(k) = 1
    qc(k,1) = 1.0D+00
    qc(k,2:r) = 0.0D+00
    qe(k,1) = k
    qe(k,2:r) = 0
  end do
!
!  Now set up the P polynomials.
!
  po(1:r) = 0
  pc(1:r,1:r) = 0.0D+00
  pe(1:r,1:r) = 0

  do k = 1, nd
!
!  Find the first polynomial Q(K:R)(X) which is nonzero at X(K).
!
    i = r + 1
    value_max = 0.0D+00

    do j = k, r
      o = qo(j)
      call polynomial_value ( d, o, qc(j,1:o), qe(j,1:o), 1, xd(1:d,k), value )
      if ( abs ( value_max ) <= abs ( value(1) ) ) then
        i = j
        value_max = value(1)
      end if
    end do

    if ( i == r + 1 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LAGRANGE_COMPLETE2 - Fatal error!'
      write ( *, '(a)' ) '  I = R+1.'
      stop 1
    end if

    value(1) = value_max
!
!  Define P(K)(X) = Q(I)(X) / Q(I)(X(k)
!
    o = qo(i)
    po(k) = qo(i)
    pc(k,1:o) = qc(i,1:o) / value(1)
    pe(k,1:o) = qe(i,1:o)
!
!  Modify P(1:k-1)(X).
!
    do j = 1, k - 1

      oj = po(j)
      ok = po(k)

      call polynomial_value ( d, oj, pc(j,1:oj), pe(j,1:oj), 1, xd(1:d,k), &
        value )

      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, pc(j,1:oj), pe(j,1:oj), o, c, e )

      po(j) = o
      pc(j,1:o) = c(1:o)
      pe(j,1:o) = e(1:o)

    end do
!
!  Modify Q(I:downto:K+1)
!
    do j = i, k + 1, -1

      oj = qo(j-1)
      ok = po(k)

      call polynomial_value ( d, oj, qc(j-1,1:oj), qe(j-1,1:oj), &
        1, xd(1:d,k), value )
 
      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, qc(j-1,1:oj), qe(j-1,1:oj), o, c, e )

      qo(j) = o
      qc(j,1:o) = c(1:o)
      qe(j,1:o) = e(1:o)

    end do
!
!  Modify Q(I+1:R)
!
    do j = i + 1, r

      oj = qo(j)
      ok = po(k)

      call polynomial_value ( d, oj, qc(j,1:oj), qe(j,1:oj), &
        1, xd(1:d,k), value )

      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, qc(j,1:oj), qe(j,1:oj), o, c, e )

      qo(j) = o
      qc(j,1:o) = c(1:o)
      qe(j,1:o) = e(1:o)

    end do

  end do
!
!  Get rid of tiny coefficients.
!
  do i = 1, nd
    oi = po(i)
    call polynomial_compress ( po(i), pc(i,1:oi), pe(i,1:oi), &
                               po(i), pc(i,1:oi), pe(i,1:oi) )
  end do

  return
end
subroutine lagrange_partial ( d, n, r, nd, xd, po, pc, pe )

!*****************************************************************************80
!
!! LAGRANGE_PARTIAL: Partial Lagrange polynomial basis from data.
!
!  Discussion:
!
!    This function represents algorithm 4.1 in the reference,
!    modified for the case where the number of data points is less
!    than the dimension of the desired polynomial space.
!
!    This function is given XD, a set of ND distinct data points in a 
!    D dimensional space, and returns information defining a set of 
!    ND Lagrange polynomials L(i)(X) with the property that:
!
!      L(i)(XD(j)) = delta(i,j)
!
!    This function is used in cases where ND, the number of data points, 
!    is less than or equal to R, the dimension of the space of polynomials 
!    in D dimensions and total degree N or less, that is:
!
!      ND <= R = Choose ( N + D, N )
!
!    There will be ND polynomials returned.  Each polynomial can have
!    as many as R coefficients.
!
!    Each polynomial is given as a vector, with each entry corresponding
!    to a nonzero coefficient.  In particular, for polynomial L(i)(X):
!
!      PO(i) is the order, that is, the number of nonzero coefficients;
!      PC(i,j), for 1 <= j <= PO(i), is the coefficient of the J-th term.
!      PE(i,j), for 1 <= j <= PO(i), encodes the exponents of the J-th term.
!
!    The exponent codes are a compact way of recording the exponent vector
!    associated with each monomial.  If PE(i,j) = k, then the corresponding
!    vector of D exponents can be determined by:
!
!      E = mono_unrank_grlex ( D, k );
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Tomas Sauer, Yuan Xu,
!    On multivariate Lagrange interpolation,
!    Mathematics of Computation,
!    Volume 64, Number 211, July 1995, pages 1147-1170.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the maximum total degree.
!
!    Input, integer ( kind = 4 ) R, the number of monomials in D dimensions 
!    of total degree N or less.
!
!    Input, integer ( kind = 4 ) ND, the number of data points.
!    It must be the case that ND <= R.
!
!    Input, real ( kind = 8 ) XD(D,ND), the data points, which must be distinct.
!
!    Output, integer ( kind = 4 ) PO(ND), the order (number of nonzero
!    coefficients), for the Lagrange basis polynomials.
!
!    Output, real ( kind = 8 ) PC(ND,R), the coefficients for the 
!    Lagrange basis polynomials.
!
!    Output, integer ( kind = 4 ) PE(ND,R), the exponent indices for the 
!    Lagrange basis polynomials.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) r

  real ( kind = 8 ) c(r)
  real ( kind = 8 ) d_max
  real ( kind = 8 ) d_min
  real ( kind = 8 ) d_tol
  integer ( kind = 4 ) e(r)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  integer ( kind = 4 ) oi
  integer ( kind = 4 ) oj
  integer ( kind = 4 ) ok
  real ( kind = 8 ) pc(nd,r)
  integer ( kind = 4 ) pe(nd,r)
  integer ( kind = 4 ) po(nd)
  real ( kind = 8 ) qc(r,r)
  integer ( kind = 4 ) qe(r,r)
  integer ( kind = 4 ) qo(r)
  real ( kind = 8 ) r8_epsilon
  real ( kind = 8 ) value(1)
  real ( kind = 8 ) xd(d,nd)
!
!  Verify that R is correct.
!
  if ( r /= mono_upto_enum ( d, n ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_PARTIAL - Fatal error!'
    write ( *, '(a)' ) '  The value R is not correct.'
    stop 1
  end if

  if ( r < nd ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_PARTIAL - Fatal error!'
    write ( *, '(a)' ) '  The value R = ', r
    write ( *, '(a)' ) '  is less than ND = ', nd
    stop 1
  end if
!
!  Verify that the points are sufficiently distinct.
!
  call r8col_separation ( d, nd, xd, d_min, d_max )
  d_tol = sqrt ( r8_epsilon ( ) )

  if ( d_min < d_tol ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_PARTIAL - Fatal error!'
    write ( *, '(a)' ) '  Some points are too close!'
    write ( *, '(a,g14.6)' ) '  Minimum data point separation is = ', d_min
    stop 1
  end if
!
!  Initialize the polynomials Q which span the space of N-th degree
!  polynomials.
!
!  Default option:
!  * All ND-dimensional monomials of degree N or less.
!    in 2D, this might be 1, x, y, x^2, xy, y^2, ...
!  
  do k = 1, r
    qo(k) = 1
    qc(k,1) = 1.0D+00
    qc(k,2:r) = 0.0D+00
    qe(k,1) = k
    qe(k,2:r) = 0
  end do
!
!  Now set up the P polynomials.
!
  po(1:nd) = 0
  pc(1:nd,1:r) = 0.0D+00
  pe(1:nd,1:r) = 0

  do k = 1, nd
!
!  Find the first polynomial Q(K:R)(X) which is nonzero at X(K).
!
    i = r + 1

    do j = k, r
      o = qo(j)
      call polynomial_value ( d, o, qc(j,1:o), qe(j,1:o), 1, xd(1:d,k), value )
      if ( value(1) /= 0.0D+00 ) then
        i = j
        exit
      end if
    end do

    if ( i == r + 1 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LAGRANGE_PARTIAL - Fatal error!'
      write ( *, '(a)' ) '  I = R+1.'
      stop 1
    end if
!
!  Define P(K)(X) = Q(I)(X) / Q(I)(X(k)
!
    o = qo(i)
    po(k) = qo(i)
    pc(k,1:o) = qc(i,1:o) / value(1)
    pe(k,1:o) = qe(i,1:o)
!
!  Modify P(1:k-1)(X).
!
    do j = 1, k - 1

      oj = po(j)
      ok = po(k)

      call polynomial_value ( d, oj, pc(j,1:oj), pe(j,1:oj), 1, xd(1:d,k), &
        value )

      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, pc(j,1:oj), pe(j,1:oj), o, c, e )

      po(j) = o
      pc(j,1:o) = c(1:o)
      pe(j,1:o) = e(1:o)

    end do
!
!  Modify Q(I:downto:K+1)
!
    do j = i, k + 1, -1

      oj = qo(j-1)
      ok = po(k)

      call polynomial_value ( d, oj, qc(j-1,1:oj), qe(j-1,1:oj), &
        1, xd(1:d,k), value )
 
      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, qc(j-1,1:oj), qe(j-1,1:oj), o, c, e )

      qo(j) = o
      qc(j,1:o) = c(1:o)
      qe(j,1:o) = e(1:o)

    end do
!
!  Modify Q(I+1:R)
!
    do j = i + 1, r

      oj = qo(j)
      ok = po(k)

      call polynomial_value ( d, oj, qc(j,1:oj), qe(j,1:oj), &
        1, xd(1:d,k), value )

      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, qc(j,1:oj), qe(j,1:oj), o, c, e )

      qo(j) = o
      qc(j,1:o) = c(1:o)
      qe(j,1:o) = e(1:o)

    end do

  end do
!
!  Get rid of tiny coefficients.
!
  do i = 1, nd
    oi = po(i)
    call polynomial_compress ( po(i), pc(i,1:oi), pe(i,1:oi), &
                               po(i), pc(i,1:oi), pe(i,1:oi) )
  end do

  return
end
subroutine lagrange_partial2 ( d, n, r, nd, xd, po, pc, pe )

!*****************************************************************************80
!
!! LAGRANGE_PARTIAL2: Partial Lagrange polynomial basis from data.
!
!  Discussion:
!
!    This function represents algorithm 4.1 in the reference,
!    modified for the case where the number of data points is less
!    than the dimension of the desired polynomial space,
!    with the further modification that a form of "pivoting" is used
!    to select the next polynomial as the one with maximum absolute
!    value at the current node.
!
!    This function is given XD, a set of ND distinct data points in a 
!    D dimensional space, and returns information defining a set of 
!    ND Lagrange polynomials L(i)(X) with the property that:
!
!      L(i)(XD(j)) = delta(i,j)
!
!    This function is used in cases where ND, the number of data points, 
!    is less than or equal to R, the dimension of the space of polynomials 
!    in D dimensions and total degree N or less, that is:
!
!      ND <= R = Choose ( N + D, N )
!
!    There will be ND polynomials returned.  Each polynomial can have
!    as many as R coefficients.
!
!    Each polynomial is given as a vector, with each entry corresponding
!    to a nonzero coefficient.  In particular, for polynomial L(i)(X):
!
!      PO(i) is the order, that is, the number of nonzero coefficients;
!      PC(i,j), for 1 <= j <= PO(i), is the coefficient of the J-th term.
!      PE(i,j), for 1 <= j <= PO(i), encodes the exponents of the J-th term.
!
!    The exponent codes are a compact way of recording the exponent vector
!    associated with each monomial.  If PE(i,j) = k, then the corresponding
!    vector of D exponents can be determined by:
!
!      E = mono_unrank_grlex ( D, k );
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Tomas Sauer, Yuan Xu,
!    On multivariate Lagrange interpolation,
!    Mathematics of Computation,
!    Volume 64, Number 211, July 1995, pages 1147-1170.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the maximum total degree.
!
!    Input, integer ( kind = 4 ) R, the number of monomials in D dimensions 
!    of total degree N or less.
!
!    Input, integer ( kind = 4 ) ND, the number of data points.
!    It must be the case that ND <= R.
!
!    Input, real ( kind = 8 ) XD(D,ND), the data points, which must be distinct.
!
!    Output, integer ( kind = 4 ) PO(ND), the order (number of nonzero
!    coefficients), for the Lagrange basis polynomials.
!
!    Output, real ( kind = 8 ) PC(ND,R), the coefficients for the 
!    Lagrange basis polynomials.
!
!    Output, integer ( kind = 4 ) PE(ND,R), the exponent indices for the 
!    Lagrange basis polynomials.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) r

  real ( kind = 8 ) c(r)
  real ( kind = 8 ) d_max
  real ( kind = 8 ) d_min
  real ( kind = 8 ) d_tol
  integer ( kind = 4 ) e(r)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  integer ( kind = 4 ) oi
  integer ( kind = 4 ) oj
  integer ( kind = 4 ) ok
  real ( kind = 8 ) pc(nd,r)
  integer ( kind = 4 ) pe(nd,r)
  integer ( kind = 4 ) po(nd)
  real ( kind = 8 ) qc(r,r)
  integer ( kind = 4 ) qe(r,r)
  integer ( kind = 4 ) qo(r)
  real ( kind = 8 ) r8_epsilon
  real ( kind = 8 ) value(1)
  real ( kind = 8 ) value_max
  real ( kind = 8 ) xd(d,nd)
!
!  Verify that R is correct.
!
  if ( r /= mono_upto_enum ( d, n ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_PARTIAL2 - Fatal error!'
    write ( *, '(a)' ) '  The value R is not correct.'
    stop 1
  end if

  if ( r < nd ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_PARTIAL2 - Fatal error!'
    write ( *, '(a)' ) '  The value R = ', r
    write ( *, '(a)' ) '  is less than ND = ', nd
    stop 1
  end if
!
!  Verify that the points are sufficiently distinct.
!
  call r8col_separation ( d, nd, xd, d_min, d_max )
  d_tol = sqrt ( r8_epsilon ( ) )

  if ( d_min < d_tol ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_PARTIAL2 - Fatal error!'
    write ( *, '(a)' ) '  Some points are too close!'
    write ( *, '(a,g14.6)' ) '  Minimum data point separation is = ', d_min
    stop 1
  end if
!
!  Initialize the polynomials Q which span the space of N-th degree
!  polynomials.
!
!  Default option:
!  * All ND-dimensional monomials of degree N or less.
!    in 2D, this might be 1, x, y, x^2, xy, y^2, ...
!  
  do k = 1, r
    qo(k) = 1
    qc(k,1) = 1.0D+00
    qc(k,2:r) = 0.0D+00
    qe(k,1) = k
    qe(k,2:r) = 0
  end do
!
!  Now set up the P polynomials.
!
  po(1:nd) = 0
  pc(1:nd,1:r) = 0.0D+00
  pe(1:nd,1:r) = 0

  do k = 1, nd
!
!  Find the first polynomial Q(K:R)(X) which is nonzero at X(K).
!
    i = r + 1
    value_max = 0.0D+00

    do j = k, r
      o = qo(j)
      call polynomial_value ( d, o, qc(j,1:o), qe(j,1:o), 1, xd(1:d,k), value )
      if ( abs ( value_max ) <= abs ( value(1) ) ) then
        i = j
        value_max = value(1)
      end if
    end do

    if ( i == r + 1 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LAGRANGE_PARTIAL2 - Fatal error!'
      write ( *, '(a)' ) '  I = R+1.'
      stop 1
    end if

    value(1) = value_max
!
!  Define P(K)(X) = Q(I)(X) / Q(I)(X(k)
!
    o = qo(i)
    po(k) = qo(i)
    pc(k,1:o) = qc(i,1:o) / value(1)
    pe(k,1:o) = qe(i,1:o)
!
!  Modify P(1:k-1)(X).
!
    do j = 1, k - 1

      oj = po(j)
      ok = po(k)

      call polynomial_value ( d, oj, pc(j,1:oj), pe(j,1:oj), 1, xd(1:d,k), &
        value )

      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, pc(j,1:oj), pe(j,1:oj), o, c, e )

      po(j) = o
      pc(j,1:o) = c(1:o)
      pe(j,1:o) = e(1:o)

    end do
!
!  Modify Q(I:downto:K+1)
!
    do j = i, k + 1, -1

      oj = qo(j-1)
      ok = po(k)

      call polynomial_value ( d, oj, qc(j-1,1:oj), qe(j-1,1:oj), &
        1, xd(1:d,k), value )
 
      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, qc(j-1,1:oj), qe(j-1,1:oj), o, c, e )

      qo(j) = o
      qc(j,1:o) = c(1:o)
      qe(j,1:o) = e(1:o)

    end do
!
!  Modify Q(I+1:R)
!
    do j = i + 1, r

      oj = qo(j)
      ok = po(k)

      call polynomial_value ( d, oj, qc(j,1:oj), qe(j,1:oj), &
        1, xd(1:d,k), value )

      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, qc(j,1:oj), qe(j,1:oj), o, c, e )

      qo(j) = o
      qc(j,1:o) = c(1:o)
      qe(j,1:o) = e(1:o)

    end do

  end do
!
!  Get rid of tiny coefficients.
!
  do i = 1, nd
    oi = po(i)
    call polynomial_compress ( po(i), pc(i,1:oi), pe(i,1:oi), &
                               po(i), pc(i,1:oi), pe(i,1:oi) )
  end do

  return
end
subroutine lagrange_partial3 ( d, n, nd, xd, option, po, pc, pe, n2 )

!*****************************************************************************80
!
!! LAGRANGE_PARTIAL3: Partial Lagrange polynomial basis from data.
!
!  Discussion:
!
!    This function, together with lagrange_partial4(), is a representation
!    of algorithm 4.1 in the reference, modified:
!    * for the case where the number of data points is less
!      than the dimension of the desired polynomial space,
!    * so that a form of "pivoting" is used
!      to select the next polynomial as the one with maximum absolute
!      value at the current node;
!    * so that if the problem is not well posed, successively higher
!      values of N are tried.
!
!    This function is given XD, a set of ND distinct data points in a 
!    D dimensional space, and returns information defining a set of 
!    ND Lagrange polynomials L(i)(X) with the property that:
!
!      L(i)(XD(j)) = delta(i,j)
!
!    This function is used in cases where ND, the number of data points, 
!    is less than or equal to R, the dimension of the space of polynomials 
!    in D dimensions and total degree N or less, that is:
!
!      ND <= R = Choose ( N + D, N )
!
!    There will be ND polynomials returned.  Each polynomial can have
!    as many as R coefficients.
!
!    Each polynomial is given as a vector, with each entry corresponding
!    to a nonzero coefficient.  In particular, for polynomial L(i)(X):
!
!      PO(i) is the order, that is, the number of nonzero coefficients;
!      PC(i,j), for 1 <= j <= PO(i), is the coefficient of the J-th term.
!      PE(i,j), for 1 <= j <= PO(i), encodes the exponents of the J-th term.
!
!    The exponent codes are a compact way of recording the exponent vector
!    associated with each monomial.  If PE(i,j) = k, then the corresponding
!    vector of D exponents can be determined by:
!
!      call mono_unrank_grlex ( D, k, E )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Tomas Sauer, Yuan Xu,
!    On multivariate Lagrange interpolation,
!    Mathematics of Computation,
!    Volume 64, Number 211, July 1995, pages 1147-1170.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the maximum total degree.
!
!    Input, integer ( kind = 4 ) ND, the number of data points.
!    It must be the case that ND <= R = the number of monomials 
!    of degree N in D dimensions.
!
!    Input, real ( kind = 8 ) XD(D,ND), the data points, which must be distinct.
!
!    Input, integer ( kind = 4 ) OPTION, determines the initial basis:
!    0, monomials, 1, x, y, x^2, xy, y^2, x^3, ...
!    1, Legendre products, 1, y, x, (3y^2-1)/2, xy, (3^x^2-1), (5y^3-3)/2, ...
!
!    Output, integer ( kind = 4 ) PO(ND), the order (number of nonzero 
!    coefficients) for the Lagrange basis polynomials.
!
!    Output, real ( kind = 8 ), allocatable :: PC(:,:), the ND by R 
!    array of coefficients for the Lagrange basis polynomials.
!
!    Output, integer ( kind = 4 ), allocatable :: PE(:,:), the ND by R 
!    array of exponent indices for the Lagrange basis polynomials.
!
!    Output, integer ( kind = 4 ) N2, the adjusted value of N, which may have 
!    been increased because the interpolation problem for N was not well posed.
!    Note that R = mono_upto_enum ( D, N2 ), so the value of R depends on N2.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) nd
  
  real ( kind = 8 ) d_max
  real ( kind = 8 ) d_min
  real ( kind = 8 ) d_tol
  logical lagrange_partial4
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) option
  real ( kind = 8 ), allocatable :: pc(:,:)
  integer ( kind = 4 ), allocatable :: pe(:,:)
  integer ( kind = 4 ) po(nd)
  integer ( kind = 4 ) r
  real ( kind = 8 ), parameter :: r8_epsilon = 2.220446049250313D-016
  logical success
  real ( kind = 8 ) tol
  real ( kind = 8 ) xd(d,nd)
!
!  Verify that the points are sufficiently distinct.
!
  call r8col_separation ( d, nd, xd, d_min, d_max )
  d_tol = sqrt ( r8_epsilon )

  if ( d_min < d_tol ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_PARTIAL3 - Fatal error!'
    write ( *, '(a)' ) '  Some points are too close!'
    write ( *, '(a,g14.6)' ) '  Minimum data point separation is = ', d_min
    stop 1
  end if
!
!  Search for the appropriate interpolation space.
!
  n2 = n
  tol = 0.0001D+00

  do

    r = mono_upto_enum ( d, n2 )
!
!  I need to allocate memory in a subroutine, pass it down, and if
!  successful, pass it UP.  So presumably I need to make it allocatable
!  in the calling routine.
!
    allocate ( pc(1:nd,1:r) )
    allocate ( pe(1:nd,1:r) )

    success = lagrange_partial4 ( d, n2, r, nd, xd, option, tol, po, pc, pe )

    if ( success ) then
      return
    end if

    deallocate ( pc )
    deallocate ( pe )

    n2 = n2 + 1
    write ( *, '(a,i4)' ) 'LAGRANGE_PARTIAL3 - Increase N to ', n2

  end do

  return
end
function lagrange_partial4 ( d, n, r, nd, xd, option, tol, po, pc, pe )

!*****************************************************************************80
!
!! LAGRANGE_PARTIAL4: Partial Lagrange polynomial basis from data.
!
!  Discussion:
!
!    This function, together with lagrange_partial3(), is a representation
!    of algorithm 4.1 in the reference, modified:
!    * for the case where the number of data points is less
!      than the dimension of the desired polynomial space,
!    * so that a form of "pivoting" is used
!      to select the next polynomial as the one with maximum absolute
!      value at the current node;
!    * so that if the problem is not well posed, successively higher
!      values of N are tried.
!
!    This function is given XD, a set of ND data points in a D dimensional
!    space, and returns information defining a set of ND Lagrange polynomials
!    L(i)(X) with the property that:
!
!      L(i)(XD(j)) = delta(i,j)
!
!    This function is used in cases where ND, the number of data points, 
!    is less than or equal to R, the dimension of the space of polynomials 
!    in D dimensions and total degree N or less, that is:
!
!      ND <= R = Choose ( N + D, N )
!
!    There will be ND polynomials returned.  Each polynomial can have
!    as many as R coefficients.
!
!    Each polynomial is given as a vector, with each entry corresponding
!    to a nonzero coefficient.  In particular, for polynomial L(i)(X):
!
!      PO(i) is the order, that is, the number of nonzero coefficients;
!      PC(i,j), for 1 <= j <= PO(i), is the coefficient of the J-th term.
!      PE(i,j), for 1 <= j <= PO(i), encodes the exponents of the J-th term.
!
!    The exponent codes are a compact way of recording the exponent vector
!    associated with each monomial.  If PE(i,j) = k, then the corresponding
!    vector of D exponents can be determined by:
!
!      E = mono_unrank_grlex ( D, k );
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Tomas Sauer, Yuan Xu,
!    On multivariate Lagrange interpolation,
!    Mathematics of Computation,
!    Volume 64, Number 211, July 1995, pages 1147-1170.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the maximum total degree.
!
!    Input, integer ( kind = 4 ) R, the number of monomials in D dimensions 
!    of total degree N or less.
!
!    Input, integer ( kind = 4 ) ND, the number of data points.
!    It must be the case that ND <= R.
!
!    Input, real ( kind = 8 ) XD(D,ND), the data points.
!
!    Input, integer ( kind = 4 ) OPTION, determines the initial basis:
!    0, monomials, 1, x, y, x^2, xy, y^2, x^3, ...
!    1, Legendre products, 1, y, x, (3y^2-1)/2, xy, (3^x^2-1), (5y^3-3)/2,...
!
!    Input, real ( kind = 8 ) TOL, a tolerance for the pivoting operation.
!    If no unused polynomial can be found with a value at least TOL
!    at the current point, the algorithm fails.
!
!    Output, integer ( kind = 4 ) PO(ND), the order (number of nonzero 
!    coefficients) for the Lagrange basis polynomials.
!
!    Output, real ( kind = 8 ) PC(ND,R), the coefficients for the 
!    Lagrange basis polynomials.
!
!    Output, integer ( kind = 4 ) PE(ND,R), the exponent indices for the 
!    Lagrange basis polynomials.
!
!    Output, logical LAGRANGE_PARTIAL4, is 0 if the algorithm failed
!    (in which case the other outputs are not useful),
!    and 1 if it was successful.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) r

  real ( kind = 8 ) c(r)
  integer ( kind = 4 ) e(r)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  logical lagrange_partial4
  integer ( kind = 4 ) lpp(d)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  integer ( kind = 4 ) oi
  integer ( kind = 4 ) oj
  integer ( kind = 4 ) ok
  integer ( kind = 4 ) option
  real ( kind = 8 ) pc(nd,r)
  integer ( kind = 4 ) pe(nd,r)
  integer ( kind = 4 ) po(nd)
  real ( kind = 8 ) qc(r,r)
  integer ( kind = 4 ) qe(r,r)
  integer ( kind = 4 ) qo(r)
  logical success
  real ( kind = 8 ) tol
  real ( kind = 8 ) value(1)
  real ( kind = 8 ) value_max
  real ( kind = 8 ) xd(d,nd)

  success = .true.
!
!  Verify that R is acceptable.
!
  if ( r < nd ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGRANGE_PARTIAL4 - Fatal error!'
    write ( *, '(a)' ) '  The value R = ', r
    write ( *, '(a)' ) '  is less than ND = ', nd
    stop 1
  end if
!
!  Initialize the polynomials Q which span the space of N-th degree polynomials.
!  There are R monomials in this space.
!
  qo(1:r) = 0
  qc(1:r,1:r) = 0.0D+00
  qe(1:r,1:r) = 0
!
!  Option 0: First R D-dimensional monomials
!  Option 1: First R D-dimensional Legendre product polynomials.
!
  do k = 1, r

    if ( option == 0 ) then
      o = 1
      c(1) = 1.0D+00
      e(1) = k
!     write ( title, '(a,i2,a)' ) '  Mono(', k, ',X)='
    else if ( option == 1 ) then
      call comp_unrank_grlex ( d, k, lpp )
      call lpp_to_polynomial ( d, lpp, r, o, c, e )
!     write ( title, '(a,i2,a)' ) '  LPP(', k, ',X)='
    end if

!   call polynomial_print ( d, o, c, e, title )

    qo(k) = o
    qc(k,1:o) = c(1:o)
    qe(k,1:o) = e(1:o)

  end do
!
!  Now set up the P polynomials.
!
  po(1:nd) = 0
  pc(1:nd,1:r) = 0.0D+00
  pe(1:nd,1:r) = 0

  do k = 1, nd
!
!  Find the polynomial Q(K:R)(X) which is most nonzero at X(K).
!
    i = r + 1
    value_max = 0.0D+00

    do j = k, r
      o = qo(j)
      call polynomial_value ( d, o, qc(j,1:o), qe(j,1:o), 1, xd(1:d,k), value )
      if ( abs ( value_max ) <= abs ( value(1) ) ) then
        i = j
        value_max = value(1)
      end if
    end do
!
!  If the best nonzero value was too small or zero, fail.
!
    if ( abs ( value_max ) < tol ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LAGRANGE_PARTIAL4 - Warning!'
      write ( *, '(a)' ) '  VALUE_MAX too small.'
      write ( *, '(a,g14.6)' ) '  |value_max| = ', abs ( value_max )
      write ( *, '(a,g14.6)' ) '  TOL = ', tol
      success = .false.
      lagrange_partial4 = success
      return
    end if

    if ( i == r + 1 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LAGRANGE_PARTIAL4 - Warning!'
      write ( *, '(a)' ) '  I = R+1.'
      write ( *, '(a,i4)' ) '  I = ', i
      write ( *, '(a,i4)' ) '  R = ', r
      success = .false.
      lagrange_partial4 = success
      return 
    end if

    value(1) = value_max
!
!  Define P(K)(X) = Q(I)(X) / Q(I)(X(k)
!
    o = qo(i)
    po(k) = qo(i)
    pc(k,1:o) = qc(i,1:o) / value(1)
    pe(k,1:o) = qe(i,1:o)
!
!  Modify P(1:k-1)(X).
!
    do j = 1, k - 1

      oj = po(j)
      ok = po(k)

      call polynomial_value ( d, oj, pc(j,1:oj), pe(j,1:oj), 1, xd(1:d,k), &
        value )

      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, pc(j,1:oj), pe(j,1:oj), o, c, e )

      po(j) = o
      pc(j,1:o) = c(1:o)
      pe(j,1:o) = e(1:o)

    end do
!
!  Modify Q(I:downto:K+1)
!
    do j = i, k + 1, -1

      oj = qo(j-1)
      ok = po(k)

      call polynomial_value ( d, oj, qc(j-1,1:oj), qe(j-1,1:oj), &
        1, xd(1:d,k), value )
 
      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, qc(j-1,1:oj), qe(j-1,1:oj), o, c, e )

      qo(j) = o
      qc(j,1:o) = c(1:o)
      qe(j,1:o) = e(1:o)

    end do
!
!  Modify Q(I+1:R)
!
    do j = i + 1, r

      oj = qo(j)
      ok = po(k)

      call polynomial_value ( d, oj, qc(j,1:oj), qe(j,1:oj), &
        1, xd(1:d,k), value )

      call polynomial_axpy ( - value(1), ok, pc(k,1:ok), pe(k,1:ok), &
        oj, qc(j,1:oj), qe(j,1:oj), o, c, e )

      qo(j) = o
      qc(j,1:o) = c(1:o)
      qe(j,1:o) = e(1:o)

    end do

  end do
!
!  Get rid of tiny coefficients.
!
  do i = 1, nd
    oi = po(i)
    call polynomial_compress ( po(i), pc(i,1:oi), pe(i,1:oi), &
                               po(i), pc(i,1:oi), pe(i,1:oi) )
  end do

  lagrange_partial4 = success

  return
end
subroutine lp_coefficients ( n, o, c, f )

!*****************************************************************************80
!
!! LP_COEFFICIENTS: coefficients of Legendre polynomials P(n,x).
!
!  Discussion:
!
!    P(i,x) represents the Legendre polynomial of degree I.
!
!    The Legendre polynomial with degree N will have O = 1 + (N/2) terms.
!    The monomials of orders N, N-2, N-4, ... will have nonzero coefficients.
!
!  First terms:
!
!     1
!     0     1
!    -1/2   0      3/2
!     0    -3/2    0     5/2
!     3/8   0    -30/8   0     35/8
!     0    15/8    0   -70/8    0     63/8
!    -5/16  0    105/16  0   -315/16   0    231/16
!     0   -35/16   0   315/16   0   -693/16   0    429/16
!
!     1.00000
!     0.00000  1.00000
!    -0.50000  0.00000  1.50000
!     0.00000 -1.50000  0.00000  2.5000
!     0.37500  0.00000 -3.75000  0.00000  4.37500
!     0.00000  1.87500  0.00000 -8.75000  0.00000  7.87500
!    -0.31250  0.00000  6.56250  0.00000 -19.6875  0.00000  14.4375
!     0.00000 -2.1875   0.00000  19.6875  0.00000 -43.3215  0.00000  26.8125
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 February 2014
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
!    Input, integer ( kind = 4 ) N, the degree of the polynomial to evaluate.
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

  real ( kind = 8 ) c((n+2)/2)
  real ( kind = 8 ) ctable(0:n,0:n)
  integer ( kind = 4 ) f((n+2)/2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) o

  ctable(0:n,0:n) = 0.0D+00

  ctable(0,0) = 1.0D+00

  if ( 0 < n ) then

    ctable(1,1) = 1.0D+00
 
    do i = 2, n

      ctable(i,0:i-2) = &
         real (   - i + 1, kind = 8 ) * ctable(i-2,0:i-2) &
       / real (     i,     kind = 8 )

      ctable(i,1:i) = ctable(i,1:i) &
        + real ( i + i - 1, kind = 8 ) * ctable(i-1,0:i-1) &
        / real (     i,     kind = 8 )

    end do

  end if
!
!  Extract the nonzero data from the alternating columns of the last row.
!
  o = ( n + 2 ) / 2

  k = o
  do j = n, 0, -2
    c(k) = ctable(n,j)
    f(k) = j
    k = k - 1
  end do

  return
end
subroutine lpp_to_polynomial ( m, l, o_max, o, c, e )

!*****************************************************************************80
!
!! LPP_TO_POLYNOMIAL writes a Legendre Product Polynomial as a polynomial.
!
!  Discussion:
!
!    For example, if 
!      M = 3,
!      L = ( 1, 0, 2 ),
!    then
!      L(1,0,2)(X,Y,Z) 
!      = L(1)(X) * L(0)(Y) * L(2)(Z)
!      = X * 1 * ( 3Z^2-1)/2
!      = - 1/2 X + (3/2) X Z^2
!    so
!      O = 2 (2 nonzero terms)
!      C = -0.5
!           1.5
!      E = 4    <-- index in 3-space of exponent (1,0,0)
!          15   <-- index in 3-space of exponent (1,0,2)
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
!    07 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) L(M), the index of each Legendre product 
!    polynomial factor.  0 <= L(*).
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

    call lp_coefficients ( l(i), o2, c2, f2 )
    
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
function mono_between_enum ( d, n1, n2 )

!*****************************************************************************80
!
!! MONO_BETWEEN_ENUM enumerates monomials in D dimensions of degrees in a range.
!
!  Discussion:
!
!    For D = 3, we have the following table:
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
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N1, N2, the minimum and maximum degrees.
!    0 <= N1 <= N2.
!
!    Output, integer ( kind = 4 ) MONO_BETWEEN_ENUM, the number of monomials 
!    in D variables, of total degree between N1 and N2 inclusive.
!
  implicit none

  integer ( kind = 4 ) d
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
    value = i4_choose ( n2 + d, n2 )
  else if ( n1_copy == n2 ) then
    value = i4_choose ( n2 + d - 1, n2 )
  else
    n0 = n1_copy - 1
    value = i4_choose ( n2 + d, n2 ) - i4_choose ( n0 + d, n0 )
  end if

  mono_between_enum = value

  return
end
subroutine mono_between_next_grlex ( d, n1, n2, x )

!*****************************************************************************80
!
!! MONO_BETWEEN_NEXT_GRLEX: grlex next monomial, degree between N1 and N2.
!
!  Discussion:
!
!    We consider all monomials in a D dimensional space, with total
!    degree N between N1 and N2, inclusive.
!
!    For example:
!
!    D = 3
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
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N1, N2, the minimum and maximum degrees.
!    0 <= N1 <= N2.
!
!    Input/output, integer ( kind = 4 ) X(D), the current monomial.
!    To start the sequence, set X = [ 0, 0, ..., 0, N1 ].
!    The last value in the sequence is X = [ N2, 0, ..., 0, 0 ].
!
  implicit none

  integer ( kind = 4 ) d

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) x(d)

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

  if ( sum ( x(1:d) ) < n1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  Input X sums to less than N1.'
    stop 1
  end if

  if ( n2 < sum ( x(1:d) ) ) then
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
    x(d) = n1
  else
    call mono_next_grlex ( d, x )
  end if

  return
end
subroutine mono_next_grlex ( d, x )

!*****************************************************************************80
!
!! MONO_NEXT_GRLEX returns the next monomial in grlex order.
!
!  Discussion:
!
!    Example:
!
!    D = 3
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
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the maximum degree.
!    0 <= N.
!
!    Input/output, integer ( kind = 4 ) X(D), the current monomial.
!    The first element is X = [ 0, 0, ..., 0, 0 ].
!
  implicit none

  integer ( kind = 4 ) d

  integer ( kind = 4 ) i
  integer ( kind = 4 ) im1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) t
  integer ( kind = 4 ) x(d)
!
!  Ensure that 1 <= D.
!
  if ( d < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MONO_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  D < 1'
    stop 1
  end if
!
!  Ensure that 0 <= X(I).
!
  do i = 1, d
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
  do j = d, 1, -1

    if ( 0 < x(j) ) then
      i = j
      exit
    end if

  end do
!
!  set T = X(I)
!  set X(I) to zero,
!  increase X(I-1) by 1,
!  increment X(D) by T-1.
!
  if ( i == 0 ) then
    x(d) = 1
    return
  else if ( i == 1 ) then
    t = x(1) + 1
    im1 = d
  else if ( 1 < i ) then
    t = x(i)
    im1 = i - 1
  end if

  x(i) = 0
  x(im1) = x(im1) + 1
  x(d) = x(d) + t - 1

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
function mono_total_enum ( d, n )

!*****************************************************************************80
!
!! MONO_TOTAL_ENUM enumerates monomials in D dimensions of degree equal to N.
!
!  Discussion:
!
!    For D = 3, we have the following values:
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
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the maximum degree.
!
!    Output, integer ( kind = 4 ) VALUE, the number of monomials in D variables,
!    of total degree N.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) mono_total_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) value

  value = i4_choose ( n + d - 1, n )

  mono_total_enum = value

  return
end
subroutine mono_total_next_grlex ( d, n, x )

!*****************************************************************************80
!
!! MONO_TOTAL_NEXT_GRLEX: grlex next monomial with total degree equal to N.
!
!  Discussion:
!
!    We consider all monomials in a D dimensional space, with total degree N.
!
!    For example:
!
!    D = 3
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
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the degree.
!    0 <= N.
!
!    Input/output, integer ( kind = 4 ) X(D), the current monomial.
!    To start the sequence, set X = [ 0, 0, ..., 0, N ].
!    The last value in the sequence is X = [ N, 0, ..., 0, 0 ].
!
  implicit none

  integer ( kind = 4 ) d

  integer ( kind = 4 ) n
  integer ( kind = 4 ) x(d)

  if ( n < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_TOTAL_NEXT_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  N < 0.'
    stop 1
  end if

  if ( sum ( x(1:d) ) /= n ) then
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
    x(d) = n
  else
    call mono_next_grlex ( d, x )
  end if

  return
end
subroutine mono_unrank_grlex ( d, rank, x )

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
!    Input, integer ( kind = 4 ) D, the number of parts of the composition.
!    1 <= KC.
!
!    Input, integer ( kind = 4 ) RANK, the rank of the composition.
!    1 <= RANK.
!
!    Output, integer ( kind = 4 ) XC(D), the composition of the given rank.
!    For each I, 0 <= XC(I) <= NC, and 
!    sum ( 1 <= I <= D ) XC(I) = NC.
!
  implicit none

  integer ( kind = 4 ) d

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
  integer ( kind = 4 ) x(d)
  integer ( kind = 4 ) xs(d-1)
!
!  Ensure that 1 <= D.
!
  if ( d < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_UNRANK_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  KC < 1'
    stop 1
  end if
!
!  Ensure that 1 <= RANK.
!
  if ( rank < 1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MONO_UNRANK_GRLEX - Fatal error!'
    write ( *, '(a)' ) '  RANK < 1'
    write ( *, '(a,i12)' ) '  RANK = ', rank
    stop 1
  end if
!
!  Special case D == 1.
!
  if ( d == 1 ) then
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
    r = i4_choose ( nm + d - 1, nm )
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
  ks = d - 1
  ns = nm + d - 1

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
  do i = 2, d - 1
    x(i) = xs(i) - xs(i-1) - 1
  end do
  x(d) = ns - xs(ks)

  return
end
function mono_upto_enum ( d, n )

!*****************************************************************************80
!
!! MONO_UPTO_ENUM enumerates monomials in D dimensions of degree up to N.
!
!  Discussion:
!
!    For D = 2, we have the following values:
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
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the maximum degree.
!
!    Output, integer ( kind = 4 ) MONO_UPTO_ENUM, the number of monomials in
!    D variables, of total degree N or less.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) value

  value = i4_choose ( n + d, n )

  mono_upto_enum = value

  return
end
subroutine mono_value ( d, nx, f, x, v )

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
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) NX, the number of evaluation points.
!
!    Input, integer ( kind = 4 ) F(D), the exponents of the monomial.
!
!    Input, real ( kind = 8 ) X(D,NX), the coordinates of the evaluation points.
!
!    Output, real ( kind = 8 ) V(NX), the value of the monomial at X.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) nx

  integer ( kind = 4 ) f(d)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(nx)
  real ( kind = 8 ) x(d,nx)
  
  v(1:nx) = 1.0D+00

  do i = 1, d
    v(1:nx) = v(1:nx) * x(i,1:nx) ** f(i)
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
!    21 January 2014
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
!    The function polynomial_sort ( ) should be called first.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2014
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
!
!  We use a tolerance to drop coefficients that are close to zero.
!
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
subroutine polynomial_print ( d, o, c, e, title )

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
!    Input, integer ( kind = 4 ) D, the spatial dimension.
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

  integer ( kind = 4 ) d
  integer ( kind = 4 ) o

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  integer ( kind = 4 ) f(d)
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
      call mono_unrank_grlex ( d, e(j), f )
      do i = 1, d
        write ( *, '(i2)', advance = 'no' ) f(i)
        if ( i < d ) then
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
subroutine polynomial_value ( d, o, c, e, nx, x, p )

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
!    Input, integer ( kind = 4 ) D, the spatial dimension.
!
!    Input, integer ( kind = 4 ) O, the "order" of the polynomial.
!
!    Input, real ( kind = 8 ) C(O), the coefficients of the polynomial.
!
!    Input, integer ( kind = 4 ) E(O), the indices of the exponents 
!    of the polynomial.
!
!    Input, integer ( kind = 4 ) NX, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(D,NX), the coordinates of the evaluation points.
!
!    Output, real ( kind = 8 ) P(NX), the value of the polynomial at X.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) o

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  integer ( kind = 4 ) f(d)
  integer ( kind = 4 ) j
  real ( kind = 8 ) p(nx)
  real ( kind = 8 ) v(nx)
  real ( kind = 8 ) x(d,nx)

  p(1:nx) = 0.0D+00

  do j = 1, o
    call mono_unrank_grlex ( d, e(j), f )
    call mono_value ( d, nx, f, x, v )
    p(1:nx) = p(1:nx) + c(j) * v(1:nx)
  end do

  return
end
function r8_epsilon ( )

!*****************************************************************************80
!
!! R8_EPSILON returns the R8 roundoff unit.
!
!  Discussion:
!
!    The roundoff unit is a number R which is a power of 2 with the
!    property that, to the precision of the computer's arithmetic,
!      1 < 1 + R
!    but
!      1 = ( 1 + R / 2 )
!
!    FORTRAN90 provides the superior library routine
!
!      EPSILON ( X )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_EPSILON, the round-off unit.
!
  implicit none

  real ( kind = 8 ) r8_epsilon

  r8_epsilon = 2.220446049250313D-016

  return
end
function r8_huge ( )

!*****************************************************************************80
!
!! R8_HUGE returns a very large R8.
!
!  Discussion:
!
!    The value returned by this function is NOT required to be the
!    maximum representable R8.  This value varies from machine to machine,
!    from compiler to compiler, and may cause problems when being printed.
!    We simply want a "very large" but non-infinite number.
!
!    FORTRAN90 provides a built-in routine HUGE ( X ) that
!    can return the maximum representable number of the same datatype
!    as X, if that is what is really desired.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 October 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_HUGE, a "huge" value.
!
  implicit none

  real ( kind = 8 ) r8_huge

  r8_huge = 1.0D+30

  return
end
subroutine r8col_separation ( m, n, a, d_min, d_max )

!*****************************************************************************80
!
!! R8COL_SEPARATION returns the "separation" of an R8COL.
!
!  Discussion:
!
!    D_MIN is the minimum distance between two columns,
!    D_MAX is the maximum distance between two columns.
!
!    The distances are measured using the Loo norm.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns 
!    in the array.  If N < 2, it does not make sense to call this routine.
!
!    Input, real ( kind = 8 ) A(M,N), the array whose variances are desired.
!
!    Output, real ( kind = 8 ) D_MIN, D_MAX, the minimum and maximum distances.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) d
  real ( kind = 8 ) d_max
  real ( kind = 8 ) d_min
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  real ( kind = 8 ) r8_huge

  d_min = r8_huge ( )
  d_max = 0.0D+00

  do j1 = 1, n
    do j2 = j1 + 1, n
      d = maxval ( abs ( a(1:m,j1) - a(1:m,j2) ) )
      d_min = min ( d_min, d )
      d_max = max ( d_max, d )
    end do
  end do

  return
end
subroutine r8mat_is_identity ( n, a, error_frobenius )

!*****************************************************************************80
!
!! R8MAT_IS_IDENTITY determines if an R8MAT is the identity.
!
!  Discussion:
!
!    An R8MAT is a matrix of real ( kind = 8 ) values.
!
!    The routine returns the Frobenius norm of A - I.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix.
!
!    Output, real ( kind = 8 ) ERROR_FROBENIUS, the Frobenius norm
!    of the difference matrix A - I, which would be exactly zero
!    if A were the identity matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) error_frobenius
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  error_frobenius = 0.0D+00

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        error_frobenius = error_frobenius + ( a(i,j) - 1.0D+00 )**2
      else
        error_frobenius = error_frobenius + a(i,j)**2
      end if
    end do 
  end do

  error_frobenius = sqrt ( error_frobenius )

  return
end
subroutine r8mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_PRINT prints an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME prints some of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8,6x)' ) j
    end do

    write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( a(i,j) == real ( int ( a(i,j) ), kind = 8 ) ) then
          write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
        else
          write ( ctemp(j2), '(g14.6)' ) a(i,j)
        end if

      end do

      write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end
subroutine r8mat_transpose_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

    i2hi = i2lo + incx - 1
    i2hi = min ( i2hi, m )
    i2hi = min ( i2hi, ihi )

    inc = i2hi + 1 - i2lo

    write ( *, '(a)' ) ' '

    do i = i2lo, i2hi
      i2 = i + 1 - i2lo
      write ( ctemp(i2), '(i8,6x)' ) i
    end do

    write ( *, '(''  Row   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Col'
    write ( *, '(a)' ) ' '

    j2lo = max ( jlo, 1 )
    j2hi = min ( jhi, n )

    do j = j2lo, j2hi

      do i2 = 1, inc
        i = i2lo - 1 + i2
        write ( ctemp(i2), '(g14.6)' ) a(i,j)
      end do

      write ( *, '(i5,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

    end do

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
