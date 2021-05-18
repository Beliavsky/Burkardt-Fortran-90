subroutine comp_next ( n, k, a, more, h, t )

!*****************************************************************************80
!
!! COMP_NEXT computes the compositions of the integer N into K parts.
!
!  Discussion:
!
!    A composition of the integer N into K parts is an ordered sequence
!    of K nonnegative integers which sum to N.  The compositions (1,2,1)
!    and (1,1,2) are considered to be distinct.
!
!    The routine computes one composition on each call until there are no more.
!    For instance, one composition of 6 into 3 parts is
!    3+2+1, another would be 6+0+0.
!
!    On the first call to this routine, set MORE = FALSE.  The routine
!    will compute the first element in the sequence of compositions, and
!    return it, as well as setting MORE = TRUE.  If more compositions
!    are desired, call again, and again.  Each time, the routine will
!    return with a new composition.
!
!    However, when the LAST composition in the sequence is computed
!    and returned, the routine will reset MORE to FALSE, signaling that
!    the end of the sequence has been reached.
!
!  Example:
!
!    The 28 compositions of 6 into three parts are:
!
!      6 0 0,  5 1 0,  5 0 1,  4 2 0,  4 1 1,  4 0 2,
!      3 3 0,  3 2 1,  3 1 2,  3 0 3,  2 4 0,  2 3 1,
!      2 2 2,  2 1 3,  2 0 4,  1 5 0,  1 4 1,  1 3 2,
!      1 2 3,  1 1 4,  1 0 5,  0 6 0,  0 5 1,  0 4 2,
!      0 3 3,  0 2 4,  0 1 5,  0 0 6.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 July 2007
!
!  Author:
!
!    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
!    FORTRAN90 version by John Burkardt
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
!    Input/output, integer ( kind = 4 ) A(K), the parts of the composition.
!
!    Input/output, logical ( kind = 4 ) MORE, set by the user to start the
!    computation, and by the routine to terminate it.
!
!    Input/output, integer ( kind = 4 ) H, T, values used by the program.
!    The user should NOT set or alter these quantities.
!
  implicit none

  integer ( kind = 4 ) k

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) h
  logical ( kind = 4 ) more
  integer ( kind = 4 ) n
  integer ( kind = 4 ) t
!
!  The first computation.
!
  if ( .not. more ) then

    t = n
    h = 0
    a(1) = n
    a(2:k) = 0
!
!  The next computation.
!
  else

    if ( 1 < t ) then
      h = 0
    end if

    h = h + 1
    t = a(h)
    a(h) = 0
    a(1) = t - 1
    a(h+1) = a(h+1) + 1

  end if
!
!  This is the last element of the sequence if all the
!  items are in the last slot.
!
  more = ( a(k) /= n )

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer( kind = 4 )  IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical ( kind = 4 ) lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
subroutine gm_general_rule_set ( rule, m, n, t, w, x )

!*****************************************************************************80
!
!! GM_GENERAL_RULE_SET sets a Grundmann-Moeller rule for a general simplex.
!
!  Discussion:
!
!    The vertices of the simplex are given by the array T.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 August 2020
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Axel Grundmann, Michael Moeller,
!    Invariant Integration Formulas for the N-Simplex 
!    by Combinatorial Methods,
!    SIAM Journal on Numerical Analysis,
!    Volume 15, Number 2, April 1978, pages 282-290.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule.
!    0 <= RULE.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!    1 <= M.
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!
!    Input, real ( kind = 8 ) T(M,M+1), the vertices of the simplex.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
!    Output, real ( kind = 8 ) X(M,N), the abscissas.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) rule
  real ( kind = 8 ) t(m,m+1)
  real ( kind = 8 ) volume
  real ( kind = 8 ) volume1
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) w1(n)
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) x1(m,n)
!
!  Get the unit rule.
!
  call gm_unit_rule_set ( rule, m, n, w1, x1 )
!
!  Compute the volume of the unit simplex.
!
  call simplex_unit_volume ( m, volume1 )
!
!  Compute the volume of the general simplex.
!
  call simplex_general_volume ( m, t, volume )
!
!  Convert the points.
!
  call simplex_unit_to_general ( m, n, t, x1, x )
!
!  Convert the weights.
!
  w(1:n) = w1(1:n) * volume / volume1

  return
end
subroutine gm_rule_size ( rule, m, n )

!*****************************************************************************80
!
!! GM_RULE_SIZE determines the size of a Grundmann-Moeller rule.
!
!  Discussion:
!
!    This rule returns the value of N, the number of points associated
!    with a GM rule of given index.
!
!    After calling this rule, the user can use the value of N to
!    allocate space for the weight vector as W(N) and the abscissa 
!    vector as X(M,N), and then call GM_UNIT_RULE_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 July 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Axel Grundmann, Michael Moeller,
!    Invariant Integration Formulas for the N-Simplex 
!    by Combinatorial Methods,
!    SIAM Journal on Numerical Analysis,
!    Volume 15, Number 2, April 1978, pages 282-290.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule.
!    0 <= RULE.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!    1 <= M.
!
!    Output, integer ( kind = 4 ) N, the number of points in the rule.
!
  implicit none

  integer ( kind = 4 ) arg1
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rule

  arg1 = m + rule + 1

  n = i4_choose ( arg1, rule )

  return
end
subroutine gm_unit_rule_set ( rule, m, n, w, x )

!*****************************************************************************80
!
!! GM_UNIT_RULE_SET sets a Grundmann-Moeller rule for the unit simplex.
!
!  Discussion:
!
!    This is a revised version of the calculation which seeks to compute
!    the value of the weight in a cautious way that avoids intermediate
!    overflow.  Thanks to John Peterson for pointing out the problem on
!    26 June 2008.
!
!    This rule returns weights and abscissas of a Grundmann-Moeller 
!    quadrature rule for the M-dimensional unit simplex.
!
!    The dimension N can be determined by calling GM_RULE_SIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 March 2017
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Axel Grundmann, Michael Moeller,
!    Invariant Integration Formulas for the N-Simplex 
!    by Combinatorial Methods,
!    SIAM Journal on Numerical Analysis,
!    Volume 15, Number 2, April 1978, pages 282-290.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule.
!    0 <= RULE.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!    1 <= M.
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
!    Output, real ( kind = 8 ) X(M,N), the abscissas.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) beta(m+1)
  integer ( kind = 4 ) beta_sum
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  logical more
  integer ( kind = 4 ) one_pm
  integer ( kind = 4 ) rule
  integer ( kind = 4 ) s
  integer ( kind = 4 ) t
  real ( kind = 8 ) volume1
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) weight
  real ( kind = 8 ) x(m,n)

  s = rule
  d = 2 * s + 1
  k = 0
  one_pm = 1

  do i = 0, s

    weight = real ( one_pm )

    do j = 1, max ( m, d, d + m - i )

      if ( j <= m ) then
        weight = weight * real ( j, kind = 8 )
      end if
      if ( j <= d ) then
        weight = weight * real ( d + m - 2 * i, kind = 8 )
      end if
      if ( j <= 2 * s ) then
        weight = weight / 2.0D+00
      end if
      if ( j <= i ) then
        weight = weight / real ( j, kind = 8 )
      end if
      if ( j <= d + m - i ) then
        weight = weight / real ( j, kind = 8 )
      end if

    end do

    one_pm = - one_pm

    beta_sum = s - i
    more = .false.
    h = 0
    t = 0

    do

      call comp_next ( beta_sum, m + 1, beta, more, h, t )

      k = k + 1

      w(k) = weight

      x(1:m,k) = real ( 2 * beta(2:m+1) + 1, kind = 8 ) &
                     / real ( d + m - 2 * i, kind = 8 )

      if ( .not. more ) then
        exit
      end if

    end do
  
  end do
!
!  Normalize.
!
  call simplex_unit_volume ( m, volume1 )
  w(1:n) = w(1:n) * volume1

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
function i4_huge ( )

!*****************************************************************************80
!
!! I4_HUGE returns a "huge" I4.
!
!  Discussion:
!
!    On an IEEE 32 bit machine, I4_HUGE should be 2^31 - 1, and its
!    bit pattern should be
!
!     01111111111111111111111111111111
!
!    In this case, its numerical value is 2147483647.
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
!  Parameters:
!
!    Output, integer ( kind = 4 ) I4_HUGE, a "huge" I4.
!
  implicit none

  integer ( kind = 4 ) i4_huge

  i4_huge = 2147483647

  return
end
subroutine monomial_value ( m, n, e, x, v )

!*****************************************************************************80
!
!! MONOMIAL_VALUE evaluates a monomial.
!
!  Discussion:
!
!    This routine evaluates a monomial of the form
!
!      product ( 1 <= i <= m ) x(i)^e(i)
!
!    The combination 0.0^0, if encountered, is treated as 1.0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 April 2014
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
!    Input, integer ( kind = 4 ) E(M), the exponents.
!
!    Input, real ( kind = 8 ) X(M,N), the point coordinates.
!
!    Output, real ( kind = 8 ) V(N), the monomial values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) e(m)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(m,n)

  v(1:n) = 1.0D+00

  do i = 1, m
    if ( 0 /= e(i) ) then
      v(1:n) = v(1:n) * x(i,1:n) ** e(i)
    end if
  end do

  return
end
function r8_factorial ( n )

!*****************************************************************************80
!
!! R8_FACTORIAL computes the factorial.
!
!  Discussion:
!
!    The formula used is:
!
!      FACTORIAL ( N ) = PRODUCT ( 1 <= I <= N ) I
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 June 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the argument of the factorial function.
!    If N is less than 1, R8_FACTORIAL is returned as 1.
!
!    Output, real ( kind = 8 ) R8_FACTORIAL, the factorial of N.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_factorial

  r8_factorial = 1.0D+00

  do i = 1, n
    r8_factorial = r8_factorial * real ( i, kind = 8 )
  end do

  return
end
subroutine r8ge_det ( n, a, pivot, det )

!*****************************************************************************80
!
!! R8GE_DET computes the determinant of a matrix factored by R8GE_FA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jack Dongarra, James Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, real ( kind = 8 ) A(N,N), the LU factors computed by R8GE_FA.
!
!    Input, integer ( kind = 4 ) PIVOT(N), as computed by R8GE_FA.
!
!    Output, real ( kind = 8 ) DET, the determinant of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) pivot(n)

  det = 1.0D+00

  do i = 1, n
    det = det * a(i,i)
    if ( pivot(i) /= i ) then
      det = - det
    end if
  end do

  return
end
subroutine r8ge_fa ( n, a, pivot, info )

!*****************************************************************************80
!
!! R8GE_FA factors a general matrix.
!
!  Discussion:
!
!    R8GE_FA is a simplified version of the LINPACK routine DGEFA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jack Dongarra, James Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input/output, real ( kind = 8 ) A(N,N), the matrix to be factored.
!    On output, A contains an upper triangular matrix and the multipliers
!    which were used to obtain it.  The factorization can be written
!    A = L * U, where L is a product of permutation and unit lower
!    triangular matrices and U is upper triangular.
!
!    Output, integer ( kind = 4 ) PIVOT(N), a vector of pivot indices.
!
!    Output, integer ( kind = 4 ) INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) t

  info = 0

  do k = 1, n - 1
!
!  Find L, the index of the pivot row.
!
    l = k
    do i = k+1, n
      if ( abs ( a(l,k) ) < abs ( a(i,k) ) ) then
        l = i
      end if
    end do

    pivot(k) = l
!
!  If the pivot index is zero, the algorithm has failed.
!
    if ( a(l,k) == 0.0D+00 ) then
      info = k
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8GE_FA - Warning!'
      write ( *, '(a,i8)' ) '  Zero pivot on step ', info
      return
    end if
!
!  Interchange rows L and K if necessary.
!
    if ( l /= k ) then
      t      = a(l,k)
      a(l,k) = a(k,k)
      a(k,k)= t
    end if
!
!  Normalize the values that lie below the pivot entry A(K,K).
!
    a(k+1:n,k) = - a(k+1:n,k) / a(k,k)
!
!  Row elimination with column indexing.
!
    do j = k+1, n

      if ( l /= k ) then
        t      = a(l,j)
        a(l,j) = a(k,j)
        a(k,j) = t
      end if

      a(k+1:n,j) = a(k+1:n,j) + a(k+1:n,k) * a(k,j)

    end do

  end do

  pivot(n) = n

  if ( a(n,n) == 0.0D+00 ) then
    info = n
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8GE_FA - Warning!'
    write ( *, '(a,i8)' ) '  Zero pivot on step ', info
  end if

  return
end
subroutine r8vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of real ( kind = 8 ) values.
!
!    For now, the input quantity SEED is an integer variable.
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
!    Volume 8, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_huge
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge ( )
    end if

    r(i) = real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end
subroutine simplex_general_volume ( m, t, volume )

!*****************************************************************************80
!
!! SIMPLEX_GENERAL_VOLUME computes the volume of a simplex in N dimensions.
!
!  Discussion:
!
!    The formula is: 
!
!      volume = 1/M! * det ( B )
!
!    where B is the M by M matrix obtained by subtracting one
!    vector from all the others.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the dimension of the space.
!
!    Input, real ( kind = 8 ) T(M,M+1), the vertices.
!
!    Output, real ( kind = 8 ) VOLUME, the volume of the simplex.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) b(m,m)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) pivot(m)
  real ( kind = 8 ) t(m,m+1)
  real ( kind = 8 ) volume

  b(1:m,1:m) = t(1:m,1:m)
  do j = 1, m
    b(1:m,j) = b(1:m,j) - t(1:m,m+1)
  end do

  call r8ge_fa ( m, b, pivot, info )

  if ( info /= 0 ) then

    volume = 0.0D+00

  else

    call r8ge_det ( m, b, pivot, det )

    volume = abs ( det )
    do i = 1, m
      volume = volume / real ( i, kind = 8 )
    end do

  end if

  return
end
subroutine simplex_unit_monomial_integral ( m, expon, value )

!*****************************************************************************80
!
!! SIMPLEX_UNIT_MONOMIAL_INTEGRAL integrates a monomial over a simplex.
!
!  Discussion:
!
!    This routine evaluates a monomial of the form
!
!      product ( 1 <= dim <= m ) x(dim)^expon(dim)
!
!    where the exponents are nonnegative integers.  Note that
!    if the combination 0^0 is encountered, it should be treated
!    as 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 July 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) EXPON(M), the exponents.
!
!    Output, real ( kind = 8 ) VALUE, the value of the integral of the
!    monomial.
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) dim
  integer ( kind = 4 ) expon(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real ( kind = 8 ) value
!
!  The first computation ends with VALUE = 1.0;
!
  value = 1.0D+00

  k = 0

  do dim = 1, m

    do i = 1, expon(dim)
      k = k + 1
      value = value * real ( i, kind = 8 ) / real ( k, kind = 8 )
    end do

  end do

  do dim = 1, m

    k = k + 1
    value = value / real ( k, kind = 8 )

  end do

  return
end
subroutine simplex_unit_monomial_quadrature ( m, expon, n, x, &
  w, quad_error )

!*****************************************************************************80
!
!! SIMPLEX_UNIT_MONOMIAL_QUADRATURE: quadrature of monomials in a unit simplex.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 March 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) EXPON(M), the exponents.
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!
!    Input, real ( kind = 8 ) X(M,N), the quadrature points.
!
!    Input, real ( kind = 8 ) W(N), the quadrature weights.
!
!    Output, real ( kind = 8 ) QUAD_ERROR, the quadrature error.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) exact
  integer ( kind = 4 ) expon(m)
  real ( kind = 8 ) quad
  real ( kind = 8 ) quad_error
  real ( kind = 8 ) scale
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(m,n)
!
!  Get the exact value of the integral of the unscaled monomial.
!
  call simplex_unit_monomial_integral ( m, expon, scale )
!
!  Evaluate the monomial at the quadrature points.
!
  call monomial_value ( m, n, expon, x, value )
!
!  Compute the weighted sum and divide by the exact value.
!
  quad = dot_product ( w, value ) / scale
!
!  Error:
!
  exact = 1.0D+00
  quad_error = abs ( quad - exact )

  return
end
subroutine simplex_unit_sample ( m, n, seed, x )

!*****************************************************************************80
!
!! SIMPLEX_UNIT_SAMPLE returns uniformly random points from a general simplex.
!
!  Discussion:
!
!    The interior of the unit M-dimensional simplex is the set of 
!    points X(1:M) such that each X(I) is nonnegative, and 
!    sum(X(1:M)) <= 1.
!
!    This routine is valid for any spatial dimension M.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 July 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Reuven Rubinstein,
!    Monte Carlo Optimization, Simulation, and Sensitivity 
!    of Queueing Networks,
!    Krieger, 1992,
!    ISBN: 0894647644,
!    LC: QA298.R79.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the dimension of the space.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) X(M,N), the points.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) e(m+1)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(m,n)
!
!  The construction begins by sampling M+1 points from the
!  exponential distribution with parameter 1.
!
  do j = 1, n

    call r8vec_uniform_01 ( m + 1, seed, e )

    e(1:m+1) = - log ( e(1:m+1) )

    x(1:m,j) = e(1:m) / sum ( e(1:m+1) )

  end do

  return
end
subroutine simplex_unit_to_general ( m, n, t, ref, phy )

!*****************************************************************************80
!
!! SIMPLEX_UNIT_TO_GENERAL maps the unit simplex to a general simplex.
!
!  Discussion:
!
!    Given that the unit simplex has been mapped to a general simplex
!    with vertices T, compute the images in T, under the same linear
!    mapping, of points whose coordinates in the unit simplex are REF.
!
!    The vertices of the unit simplex are listed as suggested in the
!    following:
!
!      (0,0,0,...,0)
!      (1,0,0,...,0)
!      (0,1,0,...,0)
!      (0,0,1,...,0)
!      (...........)
!      (0,0,0,...,1)
!
!    Thanks to Andrei ("spiritualworlds") for pointing out a mistake in the
!    previous implementation of this routine, 02 March 2008.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 March 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points to transform.
!
!    Input, real ( kind = 8 ) T(M,M+1), the vertices of the
!    general simplex.  
!
!    Input, real ( kind = 8 ) REF(M,N), points in the 
!    reference triangle.
!
!    Output, real ( kind = 8 ) PHY(M,N), corresponding points 
!    in the physical triangle.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real ( kind = 8 ) phy(m,n)
  real ( kind = 8 ) ref(m,n)
  real ( kind = 8 ) t(m,m+1)
!
!  The image of each point is initially the image of the origin.
!
!  Insofar as the pre-image differs from the origin in a given vertex
!  direction, add that proportion of the difference between the images
!  of the origin and the vertex.
!
  do i = 1, m

    phy(i,1:n) = t(i,1)

    do k = 2, m + 1

      phy(i,1:n) = phy(i,1:n) + ( t(i,k) - t(i,1) ) * ref(k-1,1:n)

    end do

  end do

  return
end
subroutine simplex_unit_volume ( m, volume )

!*****************************************************************************80
!
!! SIMPLEX_UNIT_VOLUME computes the volume of the unit simplex.
!
!  Discussion:
!
!    The formula is simple: volume = 1/N!.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Output, real ( kind = 8 ) VOLUME, the volume of the cone.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  real ( kind = 8 ) volume

  volume = 1.0D+00
  do i = 1, m
    volume = volume / real ( i, kind = 8 )
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
