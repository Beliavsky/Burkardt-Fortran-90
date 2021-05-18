subroutine legendre_2d_exactness ( a, b, n, x, y, w, t )

!*****************************************************************************80
!
!! LEGENDRE_2D_EXACTNESS: monomial exactness for the 2D Legendre integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2), the lower limits of integration.
!
!    Input, real ( kind = 8 ) B(2), the upper limits of integration.
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the quadrature points.
!
!    Input, real ( kind = 8 ) W(N), the quadrature weights.
!
!    Input, integer ( kind = 4 ) T, the maximum total degree.
!    0 <= T.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) b(2)
  real ( kind = 8 ) e
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) p(2)
  real ( kind = 8 ) q
  real ( kind = 8 ) s
  integer ( kind = 4 ) t
  integer ( kind = 4 ) tt
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Quadrature rule for the 2D Legendre integral.'
  write ( *, '(a,i3)' ) '  Number of points in rule is ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   D   I       J          Relative Error'

  do tt = 0, t

    write ( *, '(2x,i2)' )  tt

    do j = 0, tt

      i = tt - j

      p(1) = i
      p(2) = j

      call legendre_2d_monomial_integral ( a, b, p, s )

      v(1:n) = x(1:n) ** p(1) * y(1:n) ** p(2)

      q = dot_product ( w, v )

      if ( s == 0.0D+00 ) then
        e = abs ( q )
      else
        e = abs ( q - s ) / abs ( s )
      end if

      write ( *, '(2x,i6,2x,i6,2x,f24.16)' ) p(1:2), e

    end do

  end do

  return
end
subroutine legendre_2d_monomial_integral ( a, b, p, exact )

!*****************************************************************************80
!
!! LEGENDRE_2D_MONOMIAL_INTEGRAL the Legendre integral of a monomial.
!
!  Discussion:
!
!    The Legendre integral to be evaluated has the form
!
!      I(f) = integral ( y1 <= y <= y2 ) 
!             integral ( x1 <= x <= x2 ) x^i y^j dx dy
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2), the lower limits of integration.
!
!    Input, real ( kind = 8 ) B(2), the upper limits of integration.
!
!    Input, integer ( kind = 4 ) P(2), the exponents of X and Y.
!
!    Output, real ( kind = 8 ) EXACT, the value of the exact integral.
!
  implicit none

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) b(2)
  real ( kind = 8 ) exact
  integer ( kind = 4 ) p(2)

  exact = ( b(1) ** ( p(1) + 1 ) - a(1) ** ( p(1) + 1 ) ) &
        / real ( p(1) + 1, kind = 8 ) &
        * ( b(2) ** ( p(2) + 1 ) - a(2) ** ( p(2) + 1 ) ) &
        / real ( p(2) + 1, kind = 8 )

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
subroutine r8vec2_print ( n, a1, a2, title )

!*****************************************************************************80
!
!! R8VEC2_PRINT prints an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), the vectors to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, a1(i), a2(i)
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
