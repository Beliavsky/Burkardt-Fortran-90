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
!    This routine originally used a SAVE statement to maintain the
!    variables H and T.  I have decided that it is safer
!    to pass these variables as arguments, even though the user should
!    never alter them.  This allows this routine to safely shuffle
!    between several ongoing calculations.
!
!
!    There are 28 compositions of 6 into three parts.  This routine will
!    produce those compositions in the following order:
!
!     I         A
!     -     ---------
!     1     6   0   0
!     2     5   1   0
!     3     4   2   0
!     4     3   3   0
!     5     2   4   0
!     6     1   5   0
!     7     0   6   0
!     8     5   0   1
!     9     4   1   1
!    10     3   2   1
!    11     2   3   1
!    12     1   4   1
!    13     0   5   1
!    14     4   0   2
!    15     3   1   2
!    16     2   2   2
!    17     1   3   2
!    18     0   4   2
!    19     3   0   3
!    20     2   1   3
!    21     1   2   3
!    22     0   3   3
!    23     2   0   4
!    24     1   1   4
!    25     0   2   4
!    26     1   0   5
!    27     0   1   5
!    28     0   0   6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 July 2008
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
!    Input/output, integer ( kind = 4 )  H, T, two internal parameters needed
!    for the computation.  The user should allocate space for these in the
!    calling program, include them in the calling sequence, but never alter
!    them!
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
!    The combination 0.0^0 is encountered is treated as 1.0.
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
subroutine subcomp_next ( n, k, a, more, h, t )

!*****************************************************************************80
!
!! SUBCOMP_NEXT computes the next subcomposition of N into K parts.
!
!  Discussion:
!
!    A composition of the integer N into K parts is an ordered sequence
!    of K nonnegative integers which sum to a value of N.
!
!    A subcomposition of the integer N into K parts is a composition
!    of M into K parts, where 0 <= M <= N.
!
!    A subcomposition of the integer N into K parts is also a lattice
!    point in the simplex whose vertices are the origin, and the K direction
!    vectors N*E(I) for I = 1 to K.
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
!    Input, integer ( kind = 4 ) N, the integer whose subcompositions
!    are desired.
!
!    Input, integer ( kind = 4 ) K, the number of parts in the subcomposition.
!
!    Input/output, integer ( kind = 4 ) A(K), the parts of the subcomposition.
!
!    Input/output, logical MORE, set by the user to start the computation,
!    and by the routine to terminate it.
!
!    Input/output, integer H, T, two internal parameters needed for the
!    computation.  The user should allocate space for these in the calling
!    program, include them in the calling sequence, but never alter them!
!
  implicit none

  integer ( kind = 4 ) k

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) h
  logical more
  logical, save :: more2 = .false.
  integer ( kind = 4 ) n
  integer ( kind = 4 ), save :: n2 = 0
  integer ( kind = 4 ) t
!
!  The first computation.
!
  if ( .not. more ) then

    n2 = 0
    a(1:k) = 0
    more2 = .false.
    h = 0
    t = 0

    more = .true.
!
!  Do the next element at the current value of N.
!
  else if ( more2 ) then

    call comp_next ( n2, k, a, more2, h, t )

  else

    more2 = .false.
    n2 = n2 + 1

    call comp_next ( n2, k, a, more2, h, t )

  end if
!
!  Termination occurs if MORE2 = FALSE and N2 = N.
!
  if ( .not. more2 .and. n2 == n ) then
    more = .false.
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
subroutine triangle_unit_monomial ( expon, value )

!*****************************************************************************80
!
!! TRIANGLE_UNIT_MONOMIAL integrates a monomial over the unit triangle.
!
!  Discussion:
!
!    This routine integrates a monomial of the form
!
!      product ( 1 <= dim <= 2 ) x(dim)^expon(dim)
!
!    where the exponents are nonnegative integers.  Note that
!    if the combination 0^0 is encountered, it should be treated
!    as 1.
!
!    Integral ( over unit triangle ) x^m y^n dx dy = m! * n! / ( m + n + 2 )!
!
!    The integration region is:
!
!      0 <= X
!      0 <= Y
!      X + Y <= 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) EXPON(2), the exponents.
!
!    Output, real ( kind = 8 ) VALUE, the integral of the monomial.
!
  implicit none

  integer ( kind = 4 ) expon(2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real ( kind = 8 ) value
!
!  The first computation ends with VALUE = 1.0;
!
  value = 1.0D+00

! k = 0
!
!  The first loop simply computes 1 so we short circuit it!
!
! do i = 1, expon(1)
!   k = k + 1
!   value = value * real ( i, kind = 8 ) / real ( k, kind = 8 )
! end do

  k = expon(1)

  do i = 1, expon(2)
    k = k + 1
    value = value * real ( i, kind = 8 ) / real ( k, kind = 8 )
  end do

  k = k + 1
  value = value / real ( k, kind = 8 )

  k = k + 1
  value = value / real ( k, kind = 8 )

  return
end
subroutine triangle_unit_o01 ( w, xy )

!*****************************************************************************80
!
!! TRIANGLE_UNIT_O01 returns a 1 point quadrature rule for the unit triangle.
!
!  Discussion:
!
!    This rule is precise for monomials through degree 1.
!
!    The integration region is:
!
!      0 <= X
!      0 <= Y
!      X + Y <= 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carlos Felippa,
!    A compendium of FEM integration formulas for symbolic work,
!    Engineering Computation,
!    Volume 21, Number 8, 2004, pages 867-890.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) W(1), the weights.
!
!    Output, real ( kind = 8 ) XY(2,1), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 1

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(1) = (/ &
    1.0D+00 /)
  real ( kind = 8 ) xy(2,order)
  real ( kind = 8 ) :: xy_save(2,1) = reshape ( (/ &
    0.33333333333333333333D+00, &
    0.33333333333333333333D+00 /), &
  (/ 2, 1 /) )

  w(1:order) = w_save(1:order)
  xy(1:2,1:order) = xy_save(1:2,1:order)

  return
end
subroutine triangle_unit_o03 ( w, xy )

!*****************************************************************************80
!
!! TRIANGLE_UNIT_O03 returns a 3 point quadrature rule for the unit triangle.
!
!  Discussion:
!
!    This rule is precise for monomials through degree 2.
!
!    The integration region is:
!
!      0 <= X
!      0 <= Y
!      X + Y <= 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carlos Felippa,
!    A compendium of FEM integration formulas for symbolic work,
!    Engineering Computation,
!    Volume 21, Number 8, 2004, pages 867-890.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) W(3), the weights.
!
!    Output, real ( kind = 8 ) XY(2,3), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 3

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(3) = (/ &
    0.33333333333333333333D+00, &
    0.33333333333333333333D+00, &
    0.33333333333333333333D+00 /)
  real ( kind = 8 ) xy(2,order)
  real ( kind = 8 ) :: xy_save(2,3) = reshape ( (/ &
    0.66666666666666666667D+00, &
    0.16666666666666666667D+00, &
    0.16666666666666666667D+00, &
    0.66666666666666666667D+00, &
    0.16666666666666666667D+00, &
    0.16666666666666666667D+00 /), &
  (/ 2, 3 /) )

  w(1:order) = w_save(1:order)
  xy(1:2,1:order) = xy_save(1:2,1:order)

  return
end
subroutine triangle_unit_o03b ( w, xy )

!*****************************************************************************80
!
!! TRIANGLE_UNIT_O03B returns a 3 point quadrature rule for the unit triangle.
!
!  Discussion:
!
!    This rule is precise for monomials through degree 2.
!
!    The integration region is:
!
!      0 <= X
!      0 <= Y
!      X + Y <= 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carlos Felippa,
!    A compendium of FEM integration formulas for symbolic work,
!    Engineering Computation,
!    Volume 21, Number 8, 2004, pages 867-890.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) W(3), the weights.
!
!    Output, real ( kind = 8 ) XY(2,3), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 3

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(3) = (/ &
    0.33333333333333333333D+00, &
    0.33333333333333333333D+00, &
    0.33333333333333333333D+00 /)
  real ( kind = 8 ) xy(2,order)
  real ( kind = 8 ) :: xy_save(2,3) = reshape ( (/ &
    0.0D+00, &
    0.5D+00, &
    0.5D+00, &
    0.0D+00, &
    0.5D+00, &
    0.5D+00 /), &
  (/ 2, 3 /) )

  w(1:order) = w_save(1:order)
  xy(1:2,1:order) = xy_save(1:2,1:order)

  return
end
subroutine triangle_unit_o06 ( w, xy )

!*****************************************************************************80
!
!! TRIANGLE_UNIT_O06 returns a 6 point quadrature rule for the unit triangle.
!
!  Discussion:
!
!    This rule is precise for monomials through degree 4.
!
!    The integration region is:
!
!      0 <= X
!      0 <= Y
!      X + Y <= 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carlos Felippa,
!    A compendium of FEM integration formulas for symbolic work,
!    Engineering Computation,
!    Volume 21, Number 8, 2004, pages 867-890.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) W(6), the weights.
!
!    Output, real ( kind = 8 ) XY(2,6), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 6

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(6) = (/ &
    0.22338158967801146570D+00, &
    0.22338158967801146570D+00, &
    0.22338158967801146570D+00, &
    0.10995174365532186764D+00, &
    0.10995174365532186764D+00, &
    0.10995174365532186764D+00 /)
  real ( kind = 8 ) xy(2,order)
  real ( kind = 8 ) :: xy_save(2,6) = reshape ( (/ &
    0.10810301816807022736D+00, &
    0.44594849091596488632D+00, &
    0.44594849091596488632D+00, &
    0.10810301816807022736D+00, &
    0.44594849091596488632D+00, &
    0.44594849091596488632D+00, &
    0.81684757298045851308D+00, &
    0.091576213509770743460D+00, &
    0.091576213509770743460D+00, &
    0.81684757298045851308D+00, &
    0.091576213509770743460D+00, &
    0.091576213509770743460D+00 /), &
  (/ 2, 6 /) )

  w(1:order) = w_save(1:order)
  xy(1:2,1:order) = xy_save(1:2,1:order)

  return
end
subroutine triangle_unit_o06b ( w, xy )

!*****************************************************************************80
!
!! TRIANGLE_UNIT_O06B returns a 6 point quadrature rule for the unit triangle.
!
!  Discussion:
!
!    This rule is precise for monomials through degree 3.
!
!    The integration region is:
!
!      0 <= X
!      0 <= Y
!      X + Y <= 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carlos Felippa,
!    A compendium of FEM integration formulas for symbolic work,
!    Engineering Computation,
!    Volume 21, Number 8, 2004, pages 867-890.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) W(6), the weights.
!
!    Output, real ( kind = 8 ) XY(2,6), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 6

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(6) = (/ &
    0.30000000000000000000D+00, &
    0.30000000000000000000D+00, &
    0.30000000000000000000D+00, &
    0.033333333333333333333D+00, &
    0.033333333333333333333D+00, &
    0.033333333333333333333D+00 /)
  real ( kind = 8 ) xy(2,order)
  real ( kind = 8 ) :: xy_save(2,6) = reshape ( (/ &
    0.66666666666666666667D+00, &
    0.16666666666666666667D+00, &
    0.16666666666666666667D+00, &
    0.66666666666666666667D+00, &
    0.16666666666666666667D+00, &
    0.16666666666666666667D+00, &
    0.0D+00, &
    0.5D+00, &
    0.5D+00, &
    0.0D+00, &
    0.5D+00, &
    0.5D+00 /), &
  (/ 2, 6 /) )

  w(1:order) = w_save(1:order)
  xy(1:2,1:order) = xy_save(1:2,1:order)

  return
end
subroutine triangle_unit_o07 ( w, xy )

!*****************************************************************************80
!
!! TRIANGLE_UNIT_O07 returns a 7 point quadrature rule for the unit triangle.
!
!  Discussion:
!
!    This rule is precise for monomials through degree 5.
!
!    The integration region is:
!
!      0 <= X
!      0 <= Y
!      X + Y <= 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carlos Felippa,
!    A compendium of FEM integration formulas for symbolic work,
!    Engineering Computation,
!    Volume 21, Number 8, 2004, pages 867-890.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) W(7), the weights.
!
!    Output, real ( kind = 8 ) XY(2,7), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 7

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(7) = (/ &
    0.12593918054482715260D+00, &
    0.12593918054482715260D+00, &
    0.12593918054482715260D+00, &
    0.13239415278850618074D+00, &
    0.13239415278850618074D+00, &
    0.13239415278850618074D+00, &
    0.22500000000000000000D+00 /)
  real ( kind = 8 ) xy(2,order)
  real ( kind = 8 ) :: xy_save(2,7) = reshape ( (/ &
    0.79742698535308732240D+00, &
    0.10128650732345633880D+00, &
    0.10128650732345633880D+00, &
    0.79742698535308732240D+00, &
    0.10128650732345633880D+00, &
    0.10128650732345633880D+00, &
    0.059715871789769820459D+00, &
    0.47014206410511508977D+00, &
    0.47014206410511508977D+00, &
    0.059715871789769820459D+00, &
    0.47014206410511508977D+00, &
    0.47014206410511508977D+00, &
    0.33333333333333333333D+00, &
    0.33333333333333333333D+00 /), &
  (/ 2, 7 /) )

  w(1:order) = w_save(1:order)
  xy(1:2,1:order) = xy_save(1:2,1:order)

  return
end
subroutine triangle_unit_o12 ( w, xy )

!*****************************************************************************80
!
!! TRIANGLE_UNIT_O12 returns a 12 point quadrature rule for the unit triangle.
!
!  Discussion:
!
!    This rule is precise for monomials through degree 6.
!
!    The integration region is:
!
!      0 <= X
!      0 <= Y
!      X + Y <= 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carlos Felippa,
!    A compendium of FEM integration formulas for symbolic work,
!    Engineering Computation,
!    Volume 21, Number 8, 2004, pages 867-890.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) W(12), the weights.
!
!    Output, real ( kind = 8 ) XY(2,12), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 12

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(12) = (/ &
     0.050844906370206816921D+00, &
     0.050844906370206816921D+00, &
     0.050844906370206816921D+00, &
     0.11678627572637936603D+00, &
     0.11678627572637936603D+00, &
     0.11678627572637936603D+00, &
     0.082851075618373575194D+00, &
     0.082851075618373575194D+00, &
     0.082851075618373575194D+00, &
     0.082851075618373575194D+00, &
     0.082851075618373575194D+00, &
     0.082851075618373575194D+00 /)
  real ( kind = 8 ) xy(2,order)
  real ( kind = 8 ) :: xy_save(2,12) = reshape ( (/ &
    0.87382197101699554332D+00, &
    0.063089014491502228340D+00, &
    0.063089014491502228340D+00, &
    0.87382197101699554332D+00, &
    0.063089014491502228340D+00, &
    0.063089014491502228340D+00, &
    0.50142650965817915742D+00, &
    0.24928674517091042129D+00, &
    0.24928674517091042129D+00, &
    0.50142650965817915742D+00, &
    0.24928674517091042129D+00, &
    0.24928674517091042129D+00, &
    0.053145049844816947353D+00, &
    0.31035245103378440542D+00, &
    0.31035245103378440542D+00, &
    0.053145049844816947353D+00, &
    0.053145049844816947353D+00, &
    0.63650249912139864723D+00, &
    0.31035245103378440542D+00, &
    0.63650249912139864723D+00, &
    0.63650249912139864723D+00, &
    0.053145049844816947353D+00, &
    0.63650249912139864723D+00, &
    0.31035245103378440542D+00 /), &
  (/ 2, 12 /) )

  w(1:order) = w_save(1:order)
  xy(1:2,1:order) = xy_save(1:2,1:order)

  return
end
function triangle_unit_volume ( )

!*****************************************************************************80
!
!! TRIANGLE_UNIT_VOLUME: volume of a unit triangle.
!
!  Discussion:
!
!    The integration region is:
!
!      0 <= X
!      0 <= Y
!      X + Y <= 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) TRIANGLE_UNIT_VOLUME, the volume.
!
  implicit none

  real ( kind = 8 ) triangle_unit_volume

  triangle_unit_volume = 0.5D+00

  return
end

