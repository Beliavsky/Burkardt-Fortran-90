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
subroutine pyramid_unit_monomial ( expon, value )

!*****************************************************************************80
!
!! PYRAMID_UNIT_MONOMIAL: monomial integral in a unit pyramid.
!
!  Discussion:
!
!    This routine returns the integral of
!
!      product ( 1 <= I <= 3 ) X(I)^EXPON(I)
!
!    over the unit pyramid.
!
!    The integration region is:
!
!    - ( 1 - Z ) <= X <= 1 - Z
!    - ( 1 - Z ) <= Y <= 1 - Z
!              0 <= Z <= 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2008
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Arthur Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971,
!    ISBN: 0130438936,
!    LC: QA311.S85.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) EXPON(3), the exponents.
!
!    Output, real ( kind = 8 ) VALUE, the integral of the monomial.
!
  implicit none

  integer ( kind = 4 ) expon(3)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_hi
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) r8_mop
  real ( kind = 8 ) value

  value = 0.0D+00

  if ( mod ( expon(1), 2 ) == 0 .and. mod ( expon(2), 2 ) == 0 ) then

    i_hi = 2 + expon(1) + expon(2)

    do i = 0, i_hi
      value = value + r8_mop ( i ) * r8_choose ( i_hi, i ) &
      / real ( i + expon(3) + 1, kind = 8 )
    end do

    value = value &
          * 2.0D+00 / real ( expon(1) + 1, kind = 8 ) &
          * 2.0D+00 / real ( expon(2) + 1, kind = 8 )

  end if

  return
end
subroutine pyramid_unit_o01 ( w, xyz )

!*****************************************************************************80
!
!! PYRAMID_UNIT_O01 returns a 1 point quadrature rule for the unit pyramid.
!
!  Discussion:
!
!    The integration region is:
!
!    - ( 1 - Z ) <= X <= 1 - Z
!    - ( 1 - Z ) <= Y <= 1 - Z
!              0 <= Z <= 1.
!
!    When Z is zero, the integration region is a square lying in the (X,Y)
!    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
!    radius of the square diminishes, and when Z reaches 1, the square has
!    contracted to the single point (0,0,1).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2009
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
!    Output, real ( kind = 8 ) XYZ(3,1), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 1

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(1) = (/ &
    1.0D+00 /)
  real ( kind = 8 ) xyz(3,order)
  real ( kind = 8 ) :: xyz_save(3,1) = reshape ( (/ &
    0.0D+00, 0.0D+00, 0.25D+00 /), &
  (/ 3, 1 /) )

  w(1:order) = w_save(1:order)
  xyz(1:3,1:order) = xyz_save(1:3,1:order)

  return
end
subroutine pyramid_unit_o05 ( w, xyz )

!*****************************************************************************80
!
!! PYRAMID_UNIT_O05 returns a 5 point quadrature rule for the unit pyramid.
!
!  Discussion:
!
!    The integration region is:
!
!    - ( 1 - Z ) <= X <= 1 - Z
!    - ( 1 - Z ) <= Y <= 1 - Z
!              0 <= Z <= 1.
!
!    When Z is zero, the integration region is a square lying in the (X,Y)
!    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
!    radius of the square diminishes, and when Z reaches 1, the square has
!    contracted to the single point (0,0,1).
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
!    Output, real ( kind = 8 ) W(5), the weights.
!
!    Output, real ( kind = 8 ) XYZ(3,5), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 5

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(5) = (/ &
   0.21093750000000000000D+00, &
   0.21093750000000000000D+00, &
   0.21093750000000000000D+00, &
   0.21093750000000000000D+00, &
   0.15625000000000000000D+00 /)
  real ( kind = 8 ) xyz(3,order)
  real ( kind = 8 ) :: xyz_save(3,5) = reshape ( (/ &
  -0.48686449556014765641D+00, &
  -0.48686449556014765641D+00, &
   0.16666666666666666667D+00, &
   0.48686449556014765641D+00, &
  -0.48686449556014765641D+00, &
   0.16666666666666666667D+00, &
   0.48686449556014765641D+00, &
   0.48686449556014765641D+00, &
   0.16666666666666666667D+00, &
  -0.48686449556014765641D+00, &
   0.48686449556014765641D+00, &
   0.16666666666666666667D+00, &
   0.00000000000000000000D+00, &
   0.00000000000000000000D+00, &
   0.70000000000000000000D+00 /), &
  (/ 3, 5 /) )

  w(1:order) = w_save(1:order)
  xyz(1:3,1:order) = xyz_save(1:3,1:order)

  return
end
subroutine pyramid_unit_o06 ( w, xyz )

!*****************************************************************************80
!
!! PYRAMID_UNIT_O06 returns a 6 point quadrature rule for the unit pyramid.
!
!  Discussion:
!
!    The integration region is:
!
!    - ( 1 - Z ) <= X <= 1 - Z
!    - ( 1 - Z ) <= Y <= 1 - Z
!              0 <= Z <= 1.
!
!    When Z is zero, the integration region is a square lying in the (X,Y)
!    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
!    radius of the square diminishes, and when Z reaches 1, the square has
!    contracted to the single point (0,0,1).
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
!    Output, real ( kind = 8 ) XYZ(3,6), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 6

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(6) = (/ &
   0.21000000000000000000D+00, &
   0.21000000000000000000D+00, &
   0.21000000000000000000D+00, &
   0.21000000000000000000D+00, &
   0.06000000000000000000D+00, &
   0.10000000000000000000D+00 /)
  real ( kind = 8 ) xyz(3,order)
  real ( kind = 8 ) :: xyz_save(3,6) = reshape ( (/ &
  -0.48795003647426658968D+00, &
  -0.48795003647426658968D+00, &
   0.16666666666666666667D+00, &
   0.48795003647426658968D+00, &
  -0.48795003647426658968D+00, &
   0.16666666666666666667D+00, &
   0.48795003647426658968D+00, &
   0.48795003647426658968D+00, &
   0.16666666666666666667D+00, &
  -0.48795003647426658968D+00, &
   0.48795003647426658968D+00, &
   0.16666666666666666667D+00, &
   0.00000000000000000000D+00, &
   0.00000000000000000000D+00, &
   0.58333333333333333333D+00, &
   0.00000000000000000000D+00, &
   0.00000000000000000000D+00, &
   0.75000000000000000000D+00 /), &
  (/ 3, 6 /) )

  w(1:order) = w_save(1:order)
  xyz(1:3,1:order) = xyz_save(1:3,1:order)

  return
end
subroutine pyramid_unit_o08 ( w, xyz )

!*****************************************************************************80
!
!! PYRAMID_UNIT_O08 returns an 8 point quadrature rule for the unit pyramid.
!
!  Discussion:
!
!    The integration region is:
!
!    - ( 1 - Z ) <= X <= 1 - Z
!    - ( 1 - Z ) <= Y <= 1 - Z
!              0 <= Z <= 1.
!
!    When Z is zero, the integration region is a square lying in the (X,Y)
!    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
!    radius of the square diminishes, and when Z reaches 1, the square has
!    contracted to the single point (0,0,1).
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
!    Output, real ( kind = 8 ) W(8), the weights.
!
!    Output, real ( kind = 8 ) XYZ(3,8), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 8

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(8) = (/ &
   0.075589411559869072938D+00, &
   0.075589411559869072938D+00, &
   0.075589411559869072938D+00, &
   0.075589411559869072938D+00, &
   0.17441058844013092706D+00, &
   0.17441058844013092706D+00, &
   0.17441058844013092706D+00, &
   0.17441058844013092706D+00 /)
  real ( kind = 8 ) xyz(3,order)
  real ( kind = 8 ) :: xyz_save(3,8) = reshape ( (/ &
  -0.26318405556971359557D+00, &
  -0.26318405556971359557D+00, &
   0.54415184401122528880D+00, &
   0.26318405556971359557D+00, &
  -0.26318405556971359557D+00, &
   0.54415184401122528880D+00, &
   0.26318405556971359557D+00, &
   0.26318405556971359557D+00, &
   0.54415184401122528880D+00, &
  -0.26318405556971359557D+00, &
   0.26318405556971359557D+00, &
   0.54415184401122528880D+00, &
  -0.50661630334978742377D+00, &
  -0.50661630334978742377D+00, &
   0.12251482265544137787D+00, &
   0.50661630334978742377D+00, &
  -0.50661630334978742377D+00, &
   0.12251482265544137787D+00, &
   0.50661630334978742377D+00, &
   0.50661630334978742377D+00, &
   0.12251482265544137787D+00, &
  -0.50661630334978742377D+00, &
   0.50661630334978742377D+00, &
   0.12251482265544137787D+00 /), &
  (/ 3, 8 /) )

  w(1:order) = w_save(1:order)
  xyz(1:3,1:order) = xyz_save(1:3,1:order)

  return
end
subroutine pyramid_unit_o08b ( w, xyz )

!*****************************************************************************80
!
!! PYRAMID_UNIT_O08B returns an 8 point quadrature rule for the unit pyramid.
!
!  Discussion:
!
!    The integration region is:
!
!    - ( 1 - Z ) <= X <= 1 - Z
!    - ( 1 - Z ) <= Y <= 1 - Z
!              0 <= Z <= 1.
!
!    When Z is zero, the integration region is a square lying in the (X,Y)
!    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
!    radius of the square diminishes, and when Z reaches 1, the square has
!    contracted to the single point (0,0,1).
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
!    Output, real ( kind = 8 ) W(8), the weights.
!
!    Output, real ( kind = 8 ) XYZ(3,8), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 1

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(8) = (/ &
   0.16438287736328777572D+00, &
   0.16438287736328777572D+00, &
   0.16438287736328777572D+00, &
   0.16438287736328777572D+00, &
   0.085617122636712224276D+00, &
   0.085617122636712224276D+00, &
   0.085617122636712224276D+00, &
   0.085617122636712224276D+00 /)
  real ( kind = 8 ) xyz(3,order)
  real ( kind = 8 ) :: xyz_save(3,8) = reshape ( (/ &
  -0.51197009372656270107D+00, &
  -0.51197009372656270107D+00, &
   0.11024490204163285720D+00, &
   0.51197009372656270107D+00, &
  -0.51197009372656270107D+00, &
   0.11024490204163285720D+00, &
   0.51197009372656270107D+00, &
   0.51197009372656270107D+00, &
   0.11024490204163285720D+00, &
  -0.51197009372656270107D+00, &
   0.51197009372656270107D+00, &
   0.11024490204163285720D+00, &
  -0.28415447557052037456D+00, &
  -0.28415447557052037456D+00, &
   0.518326526529795714229D+00, &
   0.28415447557052037456D+00, &
  -0.28415447557052037456D+00, &
   0.518326526529795714229D+00, &
   0.28415447557052037456D+00, &
   0.28415447557052037456D+00, &
   0.518326526529795714229D+00, &
  -0.28415447557052037456D+00, &
   0.28415447557052037456D+00, &
   0.518326526529795714229D+00 /), &
  (/ 3, 8 /) )

  w(1:order) = w_save(1:order)
  xyz(1:3,1:order) = xyz_save(1:3,1:order)

  return
end
subroutine pyramid_unit_o09 ( w, xyz )

!*****************************************************************************80
!
!! PYRAMID_UNIT_O09 returns a 9 point quadrature rule for the unit pyramid.
!
!  Discussion:
!
!    The integration region is:
!
!    - ( 1 - Z ) <= X <= 1 - Z
!    - ( 1 - Z ) <= Y <= 1 - Z
!              0 <= Z <= 1.
!
!    When Z is zero, the integration region is a square lying in the (X,Y)
!    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
!    radius of the square diminishes, and when Z reaches 1, the square has
!    contracted to the single point (0,0,1).
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
!    Output, real ( kind = 8 ) W(9), the weights.
!
!    Output, real ( kind = 8 ) XYZ(3,9), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 9

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(9) = (/ &
   0.13073389672275944791D+00, &
   0.13073389672275944791D+00, &
   0.13073389672275944791D+00, &
   0.13073389672275944791D+00, &
   0.10989110327724055209D+00, &
   0.10989110327724055209D+00, &
   0.10989110327724055209D+00, &
   0.10989110327724055209D+00, &
   0.03750000000000000000D+00 /)
  real ( kind = 8 ) xyz(3,order)
  real ( kind = 8 ) :: xyz_save(3,9) = reshape ( (/ &
  -0.52966422253852215131D+00, &
  -0.52966422253852215131D+00, &
   0.08176876558246862335D+00, &
   0.52966422253852215131D+00, &
  -0.52966422253852215131D+00, &
   0.08176876558246862335D+00, &
   0.52966422253852215131D+00, &
   0.52966422253852215131D+00, &
   0.08176876558246862335D+00, &
  -0.52966422253852215131D+00, &
   0.52966422253852215131D+00, &
   0.08176876558246862335D+00, &
  -0.34819753825720418039D+00, &
  -0.34819753825720418039D+00, &
   0.400374091560388519511D+00, &
   0.34819753825720418039D+00, &
  -0.34819753825720418039D+00, &
   0.400374091560388519511D+00, &
   0.34819753825720418039D+00, &
   0.34819753825720418039D+00, &
   0.400374091560388519511D+00, &
  -0.34819753825720418039D+00, &
   0.34819753825720418039D+00, &
   0.400374091560388519511D+00, &
   0.00000000000000000000D+00, &
   0.00000000000000000000D+00, &
   0.83333333333333333333D+00 /), &
  (/ 3, 9 /) )

  w(1:order) = w_save(1:order)
  xyz(1:3,1:order) = xyz_save(1:3,1:order)

  return
end
subroutine pyramid_unit_o13 ( w, xyz )

!*****************************************************************************80
!
!! PYRAMID_UNIT_O13 returns a 13 point quadrature rule for the unit pyramid.
!
!  Discussion:
!
!    The integration region is:
!
!    - ( 1 - Z ) <= X <= 1 - Z
!    - ( 1 - Z ) <= Y <= 1 - Z
!              0 <= Z <= 1.
!
!    When Z is zero, the integration region is a square lying in the (X,Y)
!    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
!    radius of the square diminishes, and when Z reaches 1, the square has
!    contracted to the single point (0,0,1).
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
!    Output, real ( kind = 8 ) W(13), the weights.
!
!    Output, real ( kind = 8 ) XYZ(3,13), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 13

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(13) = (/ &
   0.063061594202898550725D+00, &
   0.063061594202898550725D+00, &
   0.063061594202898550725D+00, &
   0.063061594202898550725D+00, &
   0.042101946815575556199D+00, &
   0.042101946815575556199D+00, &
   0.042101946815575556199D+00, &
   0.042101946815575556199D+00, &
   0.13172030707666776585D+00, &
   0.13172030707666776585D+00, &
   0.13172030707666776585D+00, &
   0.13172030707666776585D+00, &
   0.05246460761943250889D+00 /)
  real ( kind = 8 ) xyz(3,order)
  real ( kind = 8 ) :: xyz_save(3,13) = reshape ( (/ &
  -0.38510399211870384331D+00, &
  -0.38510399211870384331D+00, &
  0.428571428571428571429D+00, &
   0.38510399211870384331D+00, &
  -0.38510399211870384331D+00, &
  0.428571428571428571429D+00, &
   0.38510399211870384331D+00, &
   0.38510399211870384331D+00, &
  0.428571428571428571429D+00, &
  -0.38510399211870384331D+00, &
   0.38510399211870384331D+00, &
  0.428571428571428571429D+00, &
  -0.40345831960728204766D+00, &
   0.00000000000000000000D+00, &
  0.33928571428571428571D+00,  &
   0.40345831960728204766D+00, &
   0.00000000000000000000D+00, &
  0.33928571428571428571D+00,  &
   0.00000000000000000000D+00, &
  -0.40345831960728204766D+00, &
  0.33928571428571428571D+00,  &
   0.00000000000000000000D+00, &
   0.40345831960728204766D+00, &
  0.33928571428571428571D+00,  &
  -0.53157877436961973359D+00, &
  -0.53157877436961973359D+00, &
  0.08496732026143790850D+00,  &
   0.53157877436961973359D+00, &
  -0.53157877436961973359D+00, &
  0.08496732026143790850D+00,  &
   0.53157877436961973359D+00, &
   0.53157877436961973359D+00, &
  0.08496732026143790850D+00,  &
  -0.53157877436961973359D+00, &
   0.53157877436961973359D+00, &
  0.08496732026143790850D+00,  &
   0.00000000000000000000D+00, &
   0.00000000000000000000D+00, &
  0.76219701803768503595D+00 /), &
  (/ 3, 13 /) )

  w(1:order) = w_save(1:order)
  xyz(1:3,1:order) = xyz_save(1:3,1:order)

  return
end
subroutine pyramid_unit_o18 ( w, xyz )

!*****************************************************************************80
!
!! PYRAMID_UNIT_O18 returns an 18 point quadrature rule for the unit pyramid.
!
!  Discussion:
!
!    The integration region is:
!
!    - ( 1 - Z ) <= X <= 1 - Z
!    - ( 1 - Z ) <= Y <= 1 - Z
!              0 <= Z <= 1.
!
!    When Z is zero, the integration region is a square lying in the (X,Y)
!    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
!    radius of the square diminishes, and when Z reaches 1, the square has
!    contracted to the single point (0,0,1).
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
!    Output, real ( kind = 8 ) W(18), the weights.
!
!    Output, real ( kind = 8 ) XYZ(3,18), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 18

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(18) = (/ &
   0.023330065296255886709D+00, &
   0.037328104474009418735D+00, &
   0.023330065296255886709D+00, &
   0.037328104474009418735D+00, &
   0.059724967158415069975D+00, &
   0.037328104474009418735D+00, &
   0.023330065296255886709D+00, &
   0.037328104474009418735D+00, &
   0.023330065296255886709D+00, &
   0.05383042853090460712D+00, &
   0.08612868564944737139D+00, &
   0.05383042853090460712D+00, &
   0.08612868564944737139D+00, &
   0.13780589703911579422D+00, &
   0.08612868564944737139D+00, &
   0.05383042853090460712D+00, &
   0.08612868564944737139D+00, &
   0.05383042853090460712D+00 /)
  real ( kind = 8 ) xyz(3,order)
  real ( kind = 8 ) :: xyz_save(3,18) = reshape ( (/ &
  -0.35309846330877704481D+00, &
  -0.35309846330877704481D+00, &
  0.544151844011225288800D+00, &
   0.00000000000000000000D+00, &
  -0.35309846330877704481D+00, &
  0.544151844011225288800D+00, &
   0.35309846330877704481D+00, &
  -0.35309846330877704481D+00, &
  0.544151844011225288800D+00, &
  -0.35309846330877704481D+00, &
   0.00000000000000000000D+00, &
  0.544151844011225288800D+00, &
   0.00000000000000000000D+00, &
   0.00000000000000000000D+00, &
  0.544151844011225288800D+00, &
   0.35309846330877704481D+00, &
   0.00000000000000000000D+00, &
  0.544151844011225288800D+00, &
  -0.35309846330877704481D+00, &
   0.35309846330877704481D+00, &
  0.544151844011225288800D+00, &
   0.00000000000000000000D+00, &
   0.35309846330877704481D+00, &
  0.544151844011225288800D+00, &
   0.35309846330877704481D+00, &
   0.35309846330877704481D+00, &
  0.544151844011225288800D+00, &
  -0.67969709567986745790D+00, &
  -0.67969709567986745790D+00, &
  0.12251482265544137787D+00, &
   0.00000000000000000000D+00, &
  -0.67969709567986745790D+00, &
  0.12251482265544137787D+00, &
   0.67969709567986745790D+00, &
  -0.67969709567986745790D+00, &
  0.12251482265544137787D+00, &
  -0.67969709567986745790D+00, &
   0.00000000000000000000D+00, &
  0.12251482265544137787D+00, &
   0.00000000000000000000D+00, &
   0.00000000000000000000D+00, &
  0.12251482265544137787D+00, &
   0.67969709567986745790D+00, &
   0.00000000000000000000D+00, &
  0.12251482265544137787D+00, &
  -0.67969709567986745790D+00, &
   0.67969709567986745790D+00, &
  0.12251482265544137787D+00, &
   0.00000000000000000000D+00, &
   0.67969709567986745790D+00, &
  0.12251482265544137787D+00, &
   0.67969709567986745790D+00, &
   0.67969709567986745790D+00, &
  0.12251482265544137787D+00 /), &
  (/ 3, 18 /) )

  w(1:order) = w_save(1:order)
  xyz(1:3,1:order) = xyz_save(1:3,1:order)

  return
end
subroutine pyramid_unit_o27 ( w, xyz )

!*****************************************************************************80
!
!! PYRAMID_UNIT_O27 returns a 27 point quadrature rule for the unit pyramid.
!
!  Discussion:
!
!    The integration region is:
!
!    - ( 1 - Z ) <= X <= 1 - Z
!    - ( 1 - Z ) <= Y <= 1 - Z
!              0 <= Z <= 1.
!
!    When Z is zero, the integration region is a square lying in the (X,Y)
!    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
!    radius of the square diminishes, and when Z reaches 1, the square has
!    contracted to the single point (0,0,1).
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
!    Output, real ( kind = 8 ) W(27), the weights.
!
!    Output, real ( kind = 8 ) XYZ(3,27), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 27

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(27) = (/ &
   0.036374157653908938268D+00, &
   0.05819865224625430123D+00, &
   0.036374157653908938268D+00, &
   0.05819865224625430123D+00, &
   0.09311784359400688197D+00, &
   0.05819865224625430123D+00, &
   0.036374157653908938268D+00, &
   0.05819865224625430123D+00, &
   0.036374157653908938268D+00, &
   0.033853303069413431019D+00, &
   0.054165284911061489631D+00, &
   0.033853303069413431019D+00, &
   0.054165284911061489631D+00, &
   0.08666445585769838341D+00, &
   0.054165284911061489631D+00, &
   0.033853303069413431019D+00, &
   0.054165284911061489631D+00, &
   0.033853303069413431019D+00, &
   0.006933033103838124540D+00, &
   0.011092852966140999264D+00, &
   0.006933033103838124540D+00, &
   0.011092852966140999264D+00, &
   0.017748564745825598822D+00, &
   0.011092852966140999264D+00, &
   0.006933033103838124540D+00, &
   0.011092852966140999264D+00, &
   0.006933033103838124540D+00 /)
  real ( kind = 8 ) xyz(3,order)
  real ( kind = 8 ) :: xyz_save(3,27) = reshape ( (/ &
  -0.7180557413198889387D+00, &
   -0.7180557413198889387D+00, &
   0.07299402407314973216D+00, &
   0.00000000000000000000D+00, &
  -0.7180557413198889387D+00, &
   0.07299402407314973216D+00, &
   0.7180557413198889387D+00, &
   -0.7180557413198889387D+00, &
   0.07299402407314973216D+00, &
  -0.7180557413198889387D+00, &
    0.00000000000000000000D+00, &
  0.07299402407314973216D+00, &
   0.00000000000000000000D+00, &
   0.00000000000000000000D+00, &
  0.07299402407314973216D+00, &
   0.7180557413198889387D+00, &
    0.00000000000000000000D+00, &
  0.07299402407314973216D+00, &
  -0.7180557413198889387D+00, &
    0.7180557413198889387D+00, &
   0.07299402407314973216D+00, &
   0.00000000000000000000D+00, &
   0.7180557413198889387D+00, &
   0.07299402407314973216D+00, &
   0.7180557413198889387D+00, &
    0.7180557413198889387D+00, &
   0.07299402407314973216D+00, &
  -0.50580870785392503961D+00, &
  -0.50580870785392503961D+00, &
  0.34700376603835188472D+00, &
   0.00000000000000000000D+00, &
  -0.50580870785392503961D+00, &
  0.34700376603835188472D+00, &
   0.50580870785392503961D+00, &
  -0.50580870785392503961D+00, &
  0.34700376603835188472D+00, &
  -0.50580870785392503961D+00, &
   0.00000000000000000000D+00, &
  0.34700376603835188472D+00, &
   0.00000000000000000000D+00, &
   0.00000000000000000000D+00, &
  0.34700376603835188472D+00, &
   0.50580870785392503961D+00, &
   0.00000000000000000000D+00, &
  0.34700376603835188472D+00, &
  -0.50580870785392503961D+00, &
   0.50580870785392503961D+00, &
  0.34700376603835188472D+00, &
   0.00000000000000000000D+00, &
   0.50580870785392503961D+00, &
  0.34700376603835188472D+00, &
   0.50580870785392503961D+00, &
   0.50580870785392503961D+00, &
  0.34700376603835188472D+00, &
  -0.22850430565396735360D+00, &
  -0.22850430565396735360D+00, &
  0.70500220988849838312D+00, &
   0.00000000000000000000D+00, &
  -0.22850430565396735360D+00, &
  0.70500220988849838312D+00, &
   0.22850430565396735360D+00, &
  -0.22850430565396735360D+00, &
  0.70500220988849838312D+00, &
  -0.22850430565396735360D+00, &
   0.00000000000000000000D+00, &
  0.70500220988849838312D+00, &
   0.00000000000000000000D+00, &
   0.00000000000000000000D+00, &
  0.70500220988849838312D+00, &
   0.22850430565396735360D+00, &
   0.00000000000000000000D+00, &
  0.70500220988849838312D+00, &
  -0.22850430565396735360D+00, &
   0.22850430565396735360D+00, &
  0.70500220988849838312D+00, &
   0.00000000000000000000D+00, &
   0.22850430565396735360D+00, &
  0.70500220988849838312D+00, &
   0.22850430565396735360D+00, &
   0.22850430565396735360D+00, &
  0.70500220988849838312D+00 /), &
  (/ 3, 27 /) )

  w(1:order) = w_save(1:order)
  xyz(1:3,1:order) = xyz_save(1:3,1:order)

  return
end
subroutine pyramid_unit_o48 ( w, xyz )

!*****************************************************************************80
!
!! PYRAMID_UNIT_O48 returns a 48 point quadrature rule for the unit pyramid.
!
!  Discussion:
!
!    The integration region is:
!
!    - ( 1 - Z ) <= X <= 1 - Z
!    - ( 1 - Z ) <= Y <= 1 - Z
!              0 <= Z <= 1.
!
!    When Z is zero, the integration region is a square lying in the (X,Y)
!    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
!    radius of the square diminishes, and when Z reaches 1, the square has
!    contracted to the single point (0,0,1).
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
!    Arthur Stroud,
!    Approximate Calculation of Multiple Integrals,
!    Prentice Hall, 1971,
!    ISBN: 0130438936,
!    LC: QA311.S85.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) W(48), the weights.
!
!    Output, real ( kind = 8 ) XYZ(3,48), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 48

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(48) = (/ &
  2.01241939442682455D-002, &
  2.01241939442682455D-002, &
  2.01241939442682455D-002, &
  2.01241939442682455D-002, &
  2.60351137043010779D-002, &
  2.60351137043010779D-002, &
  2.60351137043010779D-002, &
  2.60351137043010779D-002, &
  1.24557795239745531D-002, &
  1.24557795239745531D-002, &
  1.24557795239745531D-002, &
  1.24557795239745531D-002, &
  1.87873998794808156D-003, &
  1.87873998794808156D-003, &
  1.87873998794808156D-003, &
  1.87873998794808156D-003, &
  4.32957927807745280D-002, &
  4.32957927807745280D-002, &
  4.32957927807745280D-002, &
  4.32957927807745280D-002, &
  1.97463249834127288D-002, &
  1.97463249834127288D-002, &
  1.97463249834127288D-002, &
  1.97463249834127288D-002, &
  5.60127223523590526D-002, &
  5.60127223523590526D-002, &
  5.60127223523590526D-002, &
  5.60127223523590526D-002, &
  2.55462562927473852D-002, &
  2.55462562927473852D-002, &
  2.55462562927473852D-002, &
  2.55462562927473852D-002, &
  2.67977366291788643D-002, &
  2.67977366291788643D-002, &
  2.67977366291788643D-002, &
  2.67977366291788643D-002, &
  1.22218992265373354D-002, &
  1.22218992265373354D-002, &
  1.22218992265373354D-002, &
  1.22218992265373354D-002, &
  4.04197740453215038D-003, &
  4.04197740453215038D-003, &
  4.04197740453215038D-003, &
  4.04197740453215038D-003, &
  1.84346316995826843D-003, &
  1.84346316995826843D-003, &
  1.84346316995826843D-003, &
  1.84346316995826843D-003 /)
  real ( kind = 8 ) xyz(3,order)
  real ( kind = 8 ) :: xyz_save(3,48) = reshape ( (/ &
  0.88091731624450909D+00, &
   0.0000000000000000D+00, &
   4.85005494469969989D-02, &
 -0.88091731624450909D+00, &
   0.0000000000000000D+00, &
   4.85005494469969989D-02, &
   0.0000000000000000D+00, &
   0.88091731624450909D+00, &
  4.85005494469969989D-02, &
   0.0000000000000000D+00, &
  -0.88091731624450909D+00, &
  4.85005494469969989D-02, &
  0.70491874112648223D+00, &
   0.0000000000000000D+00, &
   0.23860073755186201D+00, &
 -0.70491874112648223D+00, &
   0.0000000000000000D+00, &
   0.23860073755186201D+00, &
   0.0000000000000000D+00, &
   0.70491874112648223D+00, &
  0.23860073755186201D+00, &
   0.0000000000000000D+00, &
  -0.70491874112648223D+00, &
  0.23860073755186201D+00, &
  0.44712732143189760D+00, &
   0.0000000000000000D+00, &
   0.51704729510436798D+00, &
 -0.44712732143189760D+00, &
   0.0000000000000000D+00, &
   0.51704729510436798D+00, &
   0.0000000000000000D+00, &
   0.44712732143189760D+00, &
  0.51704729510436798D+00, &
   0.0000000000000000D+00, &
  -0.44712732143189760D+00, &
  0.51704729510436798D+00, &
  0.18900486065123448D+00, &
   0.0000000000000000D+00, &
   0.79585141789677305D+00, &
 -0.18900486065123448D+00, &
   0.0000000000000000D+00, &
   0.79585141789677305D+00, &
   0.0000000000000000D+00, &
   0.18900486065123448D+00, &
  0.79585141789677305D+00, &
   0.0000000000000000D+00, &
  -0.18900486065123448D+00, &
  0.79585141789677305D+00, &
  0.36209733410322176D+00, &
   0.36209733410322176D+00, &
  4.85005494469969989D-02, &
 -0.36209733410322176D+00, &
   0.36209733410322176D+00, &
  4.85005494469969989D-02, &
 -0.36209733410322176D+00, &
  -0.36209733410322176D+00, &
  4.85005494469969989D-02, &
  0.36209733410322176D+00, &
  -0.36209733410322176D+00, &
  4.85005494469969989D-02, &
  0.76688932060387538D+00, &
   0.76688932060387538D+00, &
  4.85005494469969989D-02, &
 -0.76688932060387538D+00, &
   0.76688932060387538D+00, &
  4.85005494469969989D-02, &
 -0.76688932060387538D+00, &
  -0.76688932060387538D+00, &
  4.85005494469969989D-02, &
  0.76688932060387538D+00, &
  -0.76688932060387538D+00, &
  4.85005494469969989D-02, &
  0.28975386476618070D+00, &
   0.28975386476618070D+00, &
  0.23860073755186201D+00, &
 -0.28975386476618070D+00, &
   0.28975386476618070D+00, &
  0.23860073755186201D+00, &
 -0.28975386476618070D+00, &
  -0.28975386476618070D+00, &
  0.23860073755186201D+00, &
  0.28975386476618070D+00, &
  -0.28975386476618070D+00, &
  0.23860073755186201D+00, &
  0.61367241226233160D+00, &
   0.61367241226233160D+00, &
  0.23860073755186201D+00, &
 -0.61367241226233160D+00, &
   0.61367241226233160D+00, &
  0.23860073755186201D+00, &
 -0.61367241226233160D+00, &
  -0.61367241226233160D+00, &
  0.23860073755186201D+00, &
  0.61367241226233160D+00, &
  -0.61367241226233160D+00, &
  0.23860073755186201D+00, &
  0.18378979287798017D+00, &
   0.18378979287798017D+00, &
  0.51704729510436798D+00, &
 -0.18378979287798017D+00, &
   0.18378979287798017D+00, &
  0.51704729510436798D+00, &
 -0.18378979287798017D+00, &
  -0.18378979287798017D+00, &
  0.51704729510436798D+00, &
  0.18378979287798017D+00, &
  -0.18378979287798017D+00, &
  0.51704729510436798D+00, &
  0.38925011625173161D+00, &
   0.38925011625173161D+00, &
  0.51704729510436798D+00, &
 -0.38925011625173161D+00, &
   0.38925011625173161D+00, &
  0.51704729510436798D+00, &
 -0.38925011625173161D+00, &
  -0.38925011625173161D+00, &
  0.51704729510436798D+00, &
  0.38925011625173161D+00, &
  -0.38925011625173161D+00, &
  0.51704729510436798D+00, &
  7.76896479525748113D-02, &
   7.76896479525748113D-02, &
  0.79585141789677305D+00, &
 -7.76896479525748113D-02, &
   7.76896479525748113D-02, &
  0.79585141789677305D+00, &
 -7.76896479525748113D-02, &
  -7.76896479525748113D-02, &
  0.79585141789677305D+00, &
  7.76896479525748113D-02, &
  -7.76896479525748113D-02, &
  0.79585141789677305D+00, &
  0.16453962988669860D+00, &
   0.16453962988669860D+00, &
  0.79585141789677305D+00, &
 -0.16453962988669860D+00, &
   0.16453962988669860D+00, &
  0.79585141789677305D+00, &
 -0.16453962988669860D+00, &
  -0.16453962988669860D+00, &
  0.79585141789677305D+00, &
  0.16453962988669860D+00, &
  -0.16453962988669860D+00, &
  0.79585141789677305D+00 /), &
  (/ 3, 48 /) )

  w(1:order) = w_save(1:order)
  xyz(1:3,1:order) = xyz_save(1:3,1:order)

  return
end
function pyramid_unit_volume ( )

!*****************************************************************************80
!
!! PYRAMID_UNIT_VOLUME: volume of a unit pyramid with square base.
!
!  Discussion:
!
!    The volume of this unit pyramid is 4/3.
!
!    The integration region is:
!
!      - ( 1 - Z ) <= X <= 1 - Z
!      - ( 1 - Z ) <= Y <= 1 - Z
!                0 <= Z <= 1.
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
!    Output, real ( kind = 8 ) PYRAMID_UNIT_VOLUME, the volume.
!
  implicit none

  real ( kind = 8 ) pyramid_unit_volume

  pyramid_unit_volume = 4.0D+00 / 3.0D+00

  return
end
function r8_choose ( n, k )

!*****************************************************************************80
!
!! R8_CHOOSE computes the binomial coefficient C(N,K) as an R8.
!
!  Discussion:
!
!    The value is calculated in such a way as to avoid overflow and
!    roundoff.  The calculation is done in R8 arithmetic.
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
!    24 March 2008
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
!    Output, real ( kind = 8 ) R8_CHOOSE, the number of combinations of N
!    things taken K at a time.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mn
  integer ( kind = 4 ) mx
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) value

  mn = min ( k, n - k )

  if ( mn < 0 ) then

    value = 0.0D+00

  else if ( mn == 0 ) then

    value = 1.0D+00

  else

    mx = max ( k, n - k )
    value = real ( mx + 1, kind = 8 )

    do i = 2, mn
      value = ( value * real ( mx + i, kind = 8 ) ) / real ( i, kind = 8 )
    end do

  end if

  r8_choose = value

  return
end
function r8_mop ( i )

!*****************************************************************************80
!
!! R8_MOP returns the I-th power of -1 as an R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 November 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the power of -1.
!
!    Output, real ( kind = 8 ) R8_MOP, the I-th power of -1.
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_mop
  real ( kind = 8 ) value

  if ( mod ( i, 2 ) == 0 ) then
    value = + 1.0D+00
  else
    value = - 1.0D+00
  end if

  r8_mop = value

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
