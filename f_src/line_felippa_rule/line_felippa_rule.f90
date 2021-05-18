subroutine line_monomial ( a, b, alpha, value )

!*****************************************************************************80
!
!! LINE_MONOMIAL: monomial integral over a line segment in 1D.
!
!  Discussion:
!
!    This function returns the integral of X^ALPHA.
!
!    The integration region is:
!    A <= X <= B
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
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input, integer ( kind = 4 ) ALPHA, the exponent of X.
!    ALPHA must not be -1.
!
!    Output, real ( kind = 8 ) value, the integral of the monomial.
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) alpha
  real ( kind = 8 ) b
  real ( kind = 8 ) value

  if ( alpha == - 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LINE_MONOMIAL - Fatal error!'
    write ( *, '(a)' ) '  ALPHA = -1 is not a legal input.'
    stop 1
  end if

  value = ( b ** ( alpha + 1 ) - a ** ( alpha + 1 ) ) &
    / real ( alpha + 1, kind = 8 )

  return
end
subroutine line_monomial_test ( degree_max )

!*****************************************************************************80
!
!! LINE_MONOMIAL_TEST tests LINE_MONOMIAL.
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
!    Input, integer ( kind = 4 ) DEGREE_MAX, the maximum total degree of the
!    monomials to check.
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) alpha
  real ( kind = 8 ) b
  integer ( kind = 4 ) degree_max
  real ( kind = 8 ) line_volume
  real ( kind = 8 ) value

  a = 0.0D+00
  b = 1.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINE_MONOMIAL_TEST'
  write ( *, '(a)' ) '  For a line segment in 1D,'
  write ( *, '(a)' ) '  LINE_MONOMIAL returns the exact value of the'
  write ( *, '(a)' ) '  integral of X^ALPHA'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Volume = ', line_volume ( a, b )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     ALPHA      INTEGRAL'
  write ( *, '(a)' ) ' '

  do alpha = 0, degree_max
    call line_monomial ( a, b, alpha, value )
    write ( *, '(2x,i8,2x,g14.6)' ) alpha, value
  end do

  return
end
subroutine line_quad_test ( degree_max )

!*****************************************************************************80
!
!! LINE_QUAD_TEST tests the rules for a line segment in 1D.
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
!    Input, integer ( kind = 4 ) DEGREE_MAX, the maximum total degree of the
!    monomials to check.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) degree_max
  integer ( kind = 4 ) expon
  integer ( kind = 4 ) order
  real ( kind = 8 ) quad
  real ( kind = 8 ), allocatable :: v(:)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  a = 0.0D+00
  b = 1.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINE_QUAD_TEST'
  write ( *, '(a)' ) '  For a line segment in 1D,'
  write ( *, '(a)' ) '  we approximate monomial integrals with:'
  write ( *, '(a)' ) '  LINE_UNIT_O01, a 1 point rule.'
  write ( *, '(a)' ) '  LINE_UNIT_O02, a 2 point rule.'
  write ( *, '(a)' ) '  LINE_UNIT_O03, a 3 point rule.'
  write ( *, '(a)' ) '  LINE_UNIT_O04, a 4 point rule.'
  write ( *, '(a)' ) '  LINE_UNIT_O05, a 5 point rule.'

  do expon = 0, degree_max

    write ( *, '(a)' ) ' '
    write ( *, '(a,2x,i2)' ) '  Monomial exponent:  ', expon
    write ( *, '(a)' ) ' '

    do order = 1, 5

      allocate ( v(1:order) )
      allocate ( w(1:order) )
      allocate ( x(1:order) )
      call line_rule ( a, b, order, w, x )
      v(1:order) = x(1:order) ** expon
      quad = dot_product ( w(1:order), v(1:order) )
      write ( *, '(2x,i6,2x,g14.6)' ) order, quad
      deallocate ( v )
      deallocate ( w )
      deallocate ( x )
    end do

    write ( *, '(a)' ) ' '
    call line_monomial ( a, b, expon, quad )
    write ( *, '(2x,a,2x,g14.6)' ) ' Exact', quad

  end do

  return
end
subroutine line_rule ( a, b, order, w, x )

!*****************************************************************************80
!
!! LINE_RULE returns a quadrature rule for a line segment in 1D.
!
!  Discussion:
!
!    The integration region is:
!      A <= X <= B
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
!  Reference:
!
!    Carlos Felippa,
!    A compendium of FEM integration formulas for symbolic work,
!    Engineering Computation,
!    Volume 21, Number 8, 2004, pages 867-890.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input, integer ( kind = 4 ) ORDER, the order of the rule.
!
!    Output, real ( kind = 8 ) W(ORDER), the weights.
!
!    Output, real ( kind = 8 ) X(ORDER), the abscissas.
!
  implicit none

  integer ( kind = 4 ) order

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) j
  real ( kind = 8 ) w(order)
  real ( kind = 8 ) x(order)

  if ( order == 1 ) then
    call line_unit_o01 ( w, x )
  else if ( order == 2 ) then
    call line_unit_o02 ( w, x )
  else if ( order == 3 ) then
    call line_unit_o03 ( w, x )
  else if ( order == 4 ) then
    call line_unit_o04 ( w, x )
  else if ( order == 5 ) then
    call line_unit_o05 ( w, x )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LINE_RULE - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of ORDER.'
    stop 1
  end if
!
!  Transform from [-1,+1] to [A,B]
!
  do j = 1, order
    w(j) = w(j) * ( b - a ) / 2.0D+00
    x(j) = ( ( 1.0D+00 - x(j) ) * a   &
           + ( 1.0D+00 + x(j) ) * b ) &
           /   2.0D+00
  end do

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
!    - 1.0 <= X <= 1.0
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
!    -1.0 <= X <= 1.0
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
function line_volume ( a, b )

!*****************************************************************************80
!
!! LINE_VOLUME: volume of a line segment in 1D.
!
!  Discussion:
!
!    The integration region is:
!    A <= X <= B
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
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Output, real ( kind = 8 ) LINE_VOLUME, the volume.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) line_volume

  line_volume = b - a

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

