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
subroutine cube_monomial ( a, b, expon, value )

!*****************************************************************************80
!
!! CUBE_MONOMIAL integrates a monomial over a cube in 3D.
!
!  Discussion:
!
!    This routine integrates a monomial of the form
!
!      product ( 1 <= dim <= 3 ) x(dim)^expon(dim)
!
!    The combination 0^0 should be treated as 1.
!
!    The integration region is:
!      A(1) <= X <= B(1)
!      A(2) <= Y <= B(2)
!      A(3) <= Z <= B(3)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(3), B(3), the lower and upper limits.
!
!    Input, integer ( kind = 4 ) EXPON(3), the exponents.
!
!    Output, real ( kind = 8 ) VALUE, the integral of the monomial.
!
  implicit none

  real ( kind = 8 ) a(3)
  real ( kind = 8 ) b(3)
  integer ( kind = 4 ) ep1
  integer ( kind = 4 ) expon(3)
  integer ( kind = 4 ) i
  real ( kind = 8 ) value

  do i = 1, 3

    if ( expon(i) == -1 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CUBE_MONOMIAL - Fatal error!'
      write ( *, '(a)' ) '  Exponent of -1 encountered.'
      stop 1
    end if

  end do

  value = 1.0D+00

  do i = 1, 3

    ep1 = expon(i) + 1
    
    value = value * ( b(i) ** ep1 - a(i) ** ep1 ) / real ( ep1, kind = 8 )

  end do

  return
end
subroutine cube_monomial_test ( degree_max )

!*****************************************************************************80
!
!! CUBE_MONOMIAL_TEST tests CUBE_MONOMIAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DEGREE_MAX, the maximum total degree of the
!    monomials to check.
!
  implicit none

  real ( kind = 8 ) a(3)
  integer ( kind = 4 ) alpha
  real ( kind = 8 ) b(3)
  integer ( kind = 4 ) beta
  integer ( kind = 4 ) degree_max
  integer ( kind = 4 ) expon(3)
  integer ( kind = 4 ) gamma
  real ( kind = 8 ) cube_volume
  real ( kind = 8 ) value

  a(1:3) = -1.0D+00
  b(1:3) = +1.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CUBE_MONOMIAL_TEST'
  write ( *, '(a)' ) '  For the cube_,'
  write ( *, '(a)' ) '  CUBE_MONOMIAL returns the exact value of the'
  write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA Z^GAMMA'
  write ( *, '(a)' ) '  over the cube [A1,B1]x[A2,B2]x[A3,B3]'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Volume = ', cube_volume ( a, b )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     ALPHA      BETA     GAMMA      INTEGRAL'
  write ( *, '(a)' ) ' '

  do alpha = 0, degree_max
    expon(1) = alpha
    do beta = 0, degree_max - alpha
      expon(2) = beta
      do gamma = 0, degree_max - alpha - beta
        expon(3) = gamma
        call cube_monomial ( a, b, expon, value )
        write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) expon(1:3), value
      end do
    end do
  end do

  return
end
subroutine cube_quad_test ( degree_max )

!*****************************************************************************80
!
!! CUBE_QUAD_TEST tests the rules for a cube in 3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DEGREE_MAX, the maximum total degree of the
!    monomials to check.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) a(3)
  real ( kind = 8 ) b(3)
  integer ( kind = 4 ) degree_max
  integer ( kind = 4 ) expon(dim_num)
  integer ( kind = 4 ) h
  integer ( kind = 4 ) k
  logical ( kind = 4 ) more
  integer ( kind = 4 ) order
  integer ( kind = 4 ) order_1d(dim_num)
  real ( kind = 8 ) quad
  integer ( kind = 4 ) t
  real ( kind = 8 ), allocatable :: v(:)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: xyz(:,:)

  a(1:3) = -1.0D+00
  b(1:3) = +1.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CUBE_QUAD_TEST'
  write ( *, '(a)' ) '  For the cube,'
  write ( *, '(a)' ) '  we approximate monomial integrals with'
  write ( *, '(a)' ) &
    '  CUBE_RULE, which returns N1 by N2 by N3 point rules.'

  more = .false.

  do

    call subcomp_next ( degree_max, dim_num, expon, more, h, t )

    write ( *, '(a)' ) ' '
    write ( *, '(a,2x,i2,2x,i2,2x,i2)' ) &
      '  Monomial exponents: ', expon(1:dim_num)
    write ( *, '(a)' ) ' '

    do k = 1, 5

      order_1d(1:dim_num) = k
      order = product ( order_1d(1:dim_num) )
      allocate ( v(1:order) )
      allocate ( w(1:order) )
      allocate ( xyz(1:dim_num,1:order) )
      call cube_rule ( a, b, order_1d, w, xyz )
      call monomial_value ( dim_num, order, expon, xyz, v )
      quad = dot_product ( w(1:order), v(1:order) )
      write ( *, '(2x,i6,2x,i6,2x,i6,2x,g14.6)' ) order_1d(1:dim_num), quad
      deallocate ( v )
      deallocate ( w )
      deallocate ( xyz )

    end do
!
!  Try a rule of mixed orders.
!
    order_1d(1) = 3
    order_1d(2) = 5
    order_1d(3) = 2
    order = product ( order_1d(1:dim_num) )
    allocate ( v(1:order) )
    allocate ( w(1:order) )
    allocate ( xyz(1:dim_num,1:order) )
    call cube_rule ( a, b, order_1d, w, xyz )
    call monomial_value ( dim_num, order, expon, xyz, v )
    quad = dot_product ( w(1:order), v(1:order) )
    write ( *, '(2x,i6,2x,i6,2x,i6,2x,g14.6)' ) order_1d(1:dim_num), quad
    deallocate ( v )
    deallocate ( w )
    deallocate ( xyz )

    write ( *, '(a)' ) ' '
    call cube_monomial ( a, b, expon, quad )
    write ( *, '(2x,a,2x,6x,2x,6x,2x,g14.6)' ) ' Exact', quad

    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine cube_rule ( a, b, order_1d, w, xyz )

!*****************************************************************************80
!
!! CUBE_RULE returns a quadrature rule for a cube in 3D.
!
!  Discussion:
!
!    The integration region is:
!      A(1) <= X <= B(1)
!      A(2) <= Y <= B(2)
!      A(3) <= Z <= B(3)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2009
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
!    Input, real ( kind = 8 ) A(3), B(3), the lower and upper limits.
!
!    Input, integer ( kind = 4 ) ORDER_1D(3), the order of the rule in each
!    dimension.  1 <= ORDER_1D(I) <= 5.
!
!    Output, real ( kind = 8 ) W(ORDER_1D(1)*ORDER_1D(2)*ORDER_1D(3)), 
!    the weights.
!
!    Output, real ( kind = 8 ) XYZ(3,ORDER_1D(1)*ORDER_1D(2)*ORDER_1D(3)), 
!    the abscissas.
!
  implicit none

  real ( kind = 8 ) a(3)
  real ( kind = 8 ) b(3)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) o
  integer ( kind = 4 ) order
  integer ( kind = 4 ) order_1d(3)
  real ( kind = 8 ) w(order_1d(1)*order_1d(2)*order_1d(3))
  real ( kind = 8 ), allocatable :: w_1d(:)
  real ( kind = 8 ), allocatable :: x_1d(:)
  real ( kind = 8 ) xyz(3,order_1d(1)*order_1d(2)*order_1d(3))

  order = product ( order_1d(1:3) )

  do i = 1, 3

    o = order_1d(i)

    allocate ( w_1d(o) )
    allocate ( x_1d(o) )

    if ( o == 1 ) then
      call line_unit_o01 ( w_1d, x_1d )
    else if ( o == 2 ) then
      call line_unit_o02 ( w_1d, x_1d )
    else if ( o == 3 ) then
      call line_unit_o03 ( w_1d, x_1d )
    else if ( o == 4 ) then
      call line_unit_o04 ( w_1d, x_1d )
    else if ( o == 5 ) then
      call line_unit_o05 ( w_1d, x_1d )
    else
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CUBE_RULE - Fatal error!'
      write ( *, '(a)' ) '  Illegal value of ORDER_1D(*).'
      stop 1
    end if
!
!  Transform from [-1,+1] to [Ai,Bi]
!
    w_1d(1:o) = w_1d(1:o) * ( b(i) - a(i) ) / 2.0D+00
    x_1d(1:o) = ( ( 1.0D+00 - x_1d(1:o) ) * a(i)   &
                + ( 1.0D+00 + x_1d(1:o) ) * b(i) ) &
                /   2.0D+00
!
!  Add this information to the rule.
!
    call r8vec_direct_product ( i, o, x_1d, 3, order, xyz )

    call r8vec_direct_product2 ( i, o, w_1d, 3, order, w )

    deallocate ( w_1d )
    deallocate ( x_1d )

  end do

  return
end
function cube_volume ( a, b )

!*****************************************************************************80
!
!! CUBE_VOLUME: volume of a cube in 3D.
!
!  Discussion:
!
!    The integration region is:
!      A(1) <= X <= B(1)
!      A(2) <= Y <= B(2)
!      A(3) <= Z <= B(3)
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
!  Parameters:
!
!    Input, real ( kind = 8 ) A(3), B(3), the lower and upper limits.
!
!    Output, real ( kind = 8 ) CUBE_UNIT_VOLUME, the volume.
!
  implicit none

  real ( kind = 8 ) a(3)
  real ( kind = 8 ) b(3)
  real ( kind = 8 ) cube_volume

  cube_volume = product ( b(1:3) - a(1:3) )

  return
end
subroutine i4_fake_use ( n )

!*****************************************************************************80
!
!! i4_fake_use pretends to use a variable.
!
!  Discussion:
!
!    Some compilers will issue a warning if a variable is unused.
!    Sometimes there's a good reason to include a variable in a program,
!    but not to use it.  Calling this function with that variable as
!    the argument will shut the compiler up.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the variable to be "used".
!
  implicit none

  integer ( kind = 4 ) n

  if ( n /= n ) then
    write ( *, '(a)' ) '  i4_fake_use: variable is NAN.'
  end if

  return
end
subroutine line_unit_o01 ( w, x )

!*****************************************************************************80
!
!! LINE_UNIT_O01 returns a 1 point quadrature rule for the unit line.
!
!  Discussion:
!
!    The integration region is:
!
!    - 1.0 <= X <= 1.0
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
!    Output, real ( kind = 8 ) X(1), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 1

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(1) = (/ &
    2.0D+00 /)
  real ( kind = 8 ) x(order)
  real ( kind = 8 ) :: x_save(1) = (/ &
    0.0D+00 /)

  w(1:order) = w_save(1:order)
  x(1:order) = x_save(1:order)

  return
end
subroutine line_unit_o02 ( w, x )

!*****************************************************************************80
!
!! LINE_UNIT_O02 returns a 2 point quadrature rule for the unit line.
!
!  Discussion:
!
!    The integration region is:
!
!    - 1.0 <= X <= 1.0
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
!    Output, real ( kind = 8 ) W(2), the weights.
!
!    Output, real ( kind = 8 ) X(2), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 2

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(2) = (/ &
    1.0000000000000000000D+00, &
    1.0000000000000000000D+00 /)
  real ( kind = 8 ) x(order)
  real ( kind = 8 ) :: x_save(2) = (/ &
    -0.57735026918962576451D+00, &
     0.57735026918962576451D+00 /)

  w(1:order) = w_save(1:order)
  x(1:order) = x_save(1:order)

  return
end
subroutine line_unit_o03 ( w, x )

!*****************************************************************************80
!
!! LINE_UNIT_O03 returns a 3 point quadrature rule for the unit line.
!
!  Discussion:
!
!    The integration region is:
!
!    - 1.0 <= X <= 1.0
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
!    Output, real ( kind = 8 ) W(3), the weights.
!
!    Output, real ( kind = 8 ) X(3), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 3

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(3) = (/ &
    0.55555555555555555556D+00, &
    0.88888888888888888889D+00, &
    0.55555555555555555556D+00 /)
  real ( kind = 8 ) x(order)
  real ( kind = 8 ) :: x_save(3) = (/ &
    -0.77459666924148337704D+00, &
     0.00000000000000000000D+00, &
     0.77459666924148337704D+00 /)

  w(1:order) = w_save(1:order)
  x(1:order) = x_save(1:order)

  return
end
subroutine line_unit_o04 ( w, x )

!*****************************************************************************80
!
!! LINE_UNIT_O04 returns a 4 point quadrature rule for the unit line.
!
!  Discussion:
!
!    The integration region is:
!
!    - 1.0 <= X <= 1.0
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
!    Output, real ( kind = 8 ) W(4), the weights.
!
!    Output, real ( kind = 8 ) X(4), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 4

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(4) = (/ &
    0.34785484513745385737D+00, &
    0.65214515486254614263D+00, &
    0.65214515486254614263D+00, &
    0.34785484513745385737D+00 /)
  real ( kind = 8 ) x(order)
  real ( kind = 8 ) :: x_save(4) = (/ &
    -0.86113631159405257522D+00, &
    -0.33998104358485626480D+00, &
     0.33998104358485626480D+00, &
     0.86113631159405257522D+00 /)

  w(1:order) = w_save(1:order)
  x(1:order) = x_save(1:order)

  return
end
subroutine line_unit_o05 ( w, x )

!*****************************************************************************80
!
!! LINE_UNIT_O05 returns a 5 point quadrature rule for the unit line.
!
!  Discussion:
!
!    The integration region is:
!
!    - 1.0 <= X <= 1.0
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
!    Output, real ( kind = 8 ) W(5), the weights.
!
!    Output, real ( kind = 8 ) X(5), the abscissas.
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 5

  real ( kind = 8 ) w(order)
  real ( kind = 8 ) :: w_save(5) = (/ &
    0.23692688505618908751D+00, &
    0.47862867049936646804D+00, &
    0.56888888888888888889D+00, &
    0.47862867049936646804D+00, &
    0.23692688505618908751D+00 /)
  real ( kind = 8 ) x(order)
  real ( kind = 8 ) :: x_save(5) = (/ &
    -0.90617984593866399280D+00, &
    -0.53846931010568309104D+00, &
     0.00000000000000000000D+00, &
     0.53846931010568309104D+00, &
     0.90617984593866399280D+00 /)

  w(1:order) = w_save(1:order)
  x(1:order) = x_save(1:order)

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
subroutine r8vec_direct_product ( factor_index, factor_order, factor_value, &
  factor_num, point_num, x )

!*****************************************************************************80
!
!! R8VEC_DIRECT_PRODUCT creates a direct product of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    To explain what is going on here, suppose we had to construct
!    a multidimensional quadrature rule as the product of K rules
!    for 1D quadrature.
!
!    The product rule will be represented as a list of points and weights.
!
!    The J-th item in the product rule will be associated with
!      item J1 of 1D rule 1,
!      item J2 of 1D rule 2,
!      ...,
!      item JK of 1D rule K.
!
!    In particular,
!      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
!    and
!      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)
!
!    So we can construct the quadrature rule if we can properly
!    distribute the information in the 1D quadrature rules.
!
!    This routine carries out that task for the abscissas X.
!
!    Another way to do this would be to compute, one by one, the
!    set of all possible indices (J1,J2,...,JK), and then index
!    the appropriate information.  An advantage of the method shown
!    here is that you can process the K-th set of information and
!    then discard it.
!
!  Example:
!
!    Rule 1:
!      Order = 4
!      X(1:4) = ( 1, 2, 3, 4 )
!
!    Rule 2:
!      Order = 3
!      X(1:3) = ( 10, 20, 30 )
!
!    Rule 3:
!      Order = 2
!      X(1:2) = ( 100, 200 )
!
!    Product Rule:
!      Order = 24
!      X(1:24) =
!        ( 1, 10, 100 )
!        ( 2, 10, 100 )
!        ( 3, 10, 100 )
!        ( 4, 10, 100 )
!        ( 1, 20, 100 )
!        ( 2, 20, 100 )
!        ( 3, 20, 100 )
!        ( 4, 20, 100 )
!        ( 1, 30, 100 )
!        ( 2, 30, 100 )
!        ( 3, 30, 100 )
!        ( 4, 30, 100 )
!        ( 1, 10, 200 )
!        ( 2, 10, 200 )
!        ( 3, 10, 200 )
!        ( 4, 10, 200 )
!        ( 1, 20, 200 )
!        ( 2, 20, 200 )
!        ( 3, 20, 200 )
!        ( 4, 20, 200 )
!        ( 1, 30, 200 )
!        ( 2, 30, 200 )
!        ( 3, 30, 200 )
!        ( 4, 30, 200 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) FACTOR_INDEX, the index of the factor being
!    processed.  The first factor processed must be factor 1!
!
!    Input, integer ( kind = 4 ) FACTOR_ORDER, the order of the factor.
!
!    Input, real ( kind = 8 ) FACTOR_VALUE(FACTOR_ORDER), the factor values
!    for factor FACTOR_INDEX.
!
!    Input, integer ( kind = 4 ) FACTOR_NUM, the number of factors.
!
!    Input, integer ( kind = 4 ) POINT_NUM, the number of elements in the
!    direct product.
!
!    Input/output, real ( kind = 8 ) X(FACTOR_NUM,POINT_NUM), the elements of
!    the direct product, which are built up gradually.
!
!  Local Parameters:
!
!    Local, integer START, the first location of a block of values to set.
!
!    Local, integer CONTIG, the number of consecutive values to set.
!
!    Local, integer SKIP, the distance from the current value of START
!    to the next location of a block of values to set.
!
!    Local, integer REP, the number of blocks of values to set.
!
  implicit none

  integer ( kind = 4 ) factor_num
  integer ( kind = 4 ) factor_order
  integer ( kind = 4 ) point_num

  integer ( kind = 4 ), save :: contig
  integer ( kind = 4 ) factor_index
  real ( kind = 8 ) factor_value(factor_order)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ), save :: rep
  integer ( kind = 4 ), save :: skip
  integer ( kind = 4 ) start
  real ( kind = 8 ) x(factor_num,point_num)

  if ( factor_index == 1 ) then
    contig = 1
    skip = 1
    rep = point_num
    x(1:factor_num,1:point_num) = 0.0D+00
  end if

  rep = rep / factor_order
  skip = skip * factor_order

  do j = 1, factor_order

    start = 1 + ( j - 1 ) * contig

    do k = 1, rep
      x(factor_index,start:start+contig-1) = factor_value(j)
      start = start + skip
    end do

  end do

  contig = contig * factor_order

  return
end
subroutine r8vec_direct_product2 ( factor_index, factor_order, factor_value, &
  factor_num, point_num, w )

!*****************************************************************************80
!
!! R8VEC_DIRECT_PRODUCT2 creates a direct product of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    To explain what is going on here, suppose we had to construct
!    a multidimensional quadrature rule as the product of K rules
!    for 1D quadrature.
!
!    The product rule will be represented as a list of points and weights.
!
!    The J-th item in the product rule will be associated with
!      item J1 of 1D rule 1,
!      item J2 of 1D rule 2,
!      ...,
!      item JK of 1D rule K.
!
!    In particular,
!      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
!    and
!      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)
!
!    So we can construct the quadrature rule if we can properly
!    distribute the information in the 1D quadrature rules.
!
!    This routine carries out the task involving the weights W.
!
!    Another way to do this would be to compute, one by one, the
!    set of all possible indices (J1,J2,...,JK), and then index
!    the appropriate information.  An advantage of the method shown
!    here is that you can process the K-th set of information and
!    then discard it.
!
!  Example:
!
!    Rule 1:
!      Order = 4
!      W(1:4) = ( 2, 3, 5, 7 )
!
!    Rule 2:
!      Order = 3
!      W(1:3) = ( 11, 13, 17 )
!
!    Rule 3:
!      Order = 2
!      W(1:2) = ( 19, 23 )
!
!    Product Rule:
!      Order = 24
!      W(1:24) =
!        ( 2 * 11 * 19 )
!        ( 3 * 11 * 19 )
!        ( 4 * 11 * 19 )
!        ( 7 * 11 * 19 )
!        ( 2 * 13 * 19 )
!        ( 3 * 13 * 19 )
!        ( 5 * 13 * 19 )
!        ( 7 * 13 * 19 )
!        ( 2 * 17 * 19 )
!        ( 3 * 17 * 19 )
!        ( 5 * 17 * 19 )
!        ( 7 * 17 * 19 )
!        ( 2 * 11 * 23 )
!        ( 3 * 11 * 23 )
!        ( 5 * 11 * 23 )
!        ( 7 * 11 * 23 )
!        ( 2 * 13 * 23 )
!        ( 3 * 13 * 23 )
!        ( 5 * 13 * 23 )
!        ( 7 * 13 * 23 )
!        ( 2 * 17 * 23 )
!        ( 3 * 17 * 23 )
!        ( 5 * 17 * 23 )
!        ( 7 * 17 * 23 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) FACTOR_INDEX, the index of the factor being
!    processed.  The first factor processed must be factor 1!
!
!    Input, integer ( kind = 4 ) FACTOR_ORDER, the order of the factor.
!
!    Input, real ( kind = 8 ) FACTOR_VALUE(FACTOR_ORDER), the factor values
!    for factor FACTOR_INDEX.
!
!    Input, integer ( kind = 4 ) FACTOR_NUM, the number of factors.
!
!    Input, integer ( kind = 4 ) POINT_NUM, the number of elements in the
!    direct product.
!
!    Input/output, real ( kind = 8 ) W(POINT_NUM), the elements of the
!    direct product, which are built up gradually.
!
!  Local Parameters:
!
!    Local, integer ( kind = 4 ) START, the first location of a block of values
!    to set.
!
!    Local, integer ( kind = 4 ) CONTIG, the number of consecutive values 
!    to set.
!
!    Local, integer SKIP, the distance from the current value of START
!    to the next location of a block of values to set.
!
!    Local, integer REP, the number of blocks of values to set.
!
  implicit none

  integer ( kind = 4 ) factor_num
  integer ( kind = 4 ) factor_order
  integer ( kind = 4 ) point_num

  integer ( kind = 4 ), save :: contig
  integer ( kind = 4 ) factor_index
  real ( kind = 8 ) factor_value(factor_order)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ), save :: rep
  integer ( kind = 4 ), save :: skip
  integer ( kind = 4 ) start
  real ( kind = 8 ) w(point_num)

  call i4_fake_use ( factor_num )

  if ( factor_index == 1 ) then
    contig = 1
    skip = 1
    rep = point_num
    w(1:point_num) = 1.0D+00
  end if

  rep = rep / factor_order
  skip = skip * factor_order

  do j = 1, factor_order

    start = 1 + ( j - 1 ) * contig

    do k = 1, rep
      w(start:start+contig-1) = w(start:start+contig-1) * factor_value(j)
      start = start + skip
    end do

  end do

  contig = contig * factor_order

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

