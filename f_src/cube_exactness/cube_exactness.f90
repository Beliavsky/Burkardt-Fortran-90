subroutine legendre_3d_exactness ( a, b, n, x, y, z, w, t )

!*****************************************************************************80
!
!! LEGENDRE_3D_EXACTNESS: monomial exactness for the 3D Legendre integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(3), the lower limits of integration.
!
!    Input, real ( kind = 8 ) B(3), the upper limits of integration.
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!
!    Input, real ( kind = 8 ) X(N), Y(N), Z(N), the quadrature points.
!
!    Input, real ( kind = 8 ) W(N), the quadrature weights.
!
!    Input, integer ( kind = 4 ) T, the maximum total degree.
!    0 <= T.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(3)
  real ( kind = 8 ) b(3)
  real ( kind = 8 ) e
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) p(3)
  real ( kind = 8 ) q
  real ( kind = 8 ) s
  integer ( kind = 4 ) t
  integer ( kind = 4 ) tt
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Quadrature rule for the 3D Legendre integral.'
  write ( *, '(a,i3)' ) '  Number of points in rule is ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   D   I       J       K          Relative Error'

  do tt = 0, t

    write ( *, '(2x,i2)' )  tt

    do k = 0, tt

      do j = 0, tt - k

        i = tt - j - k

        p(1) = i
        p(2) = j
        p(3) = k

        call legendre_3d_monomial_integral ( a, b, p, s )

        v(1:n) = x(1:n) ** p(1) * y(1:n) ** p(2) * z(1:n) ** p(3)

        q = dot_product ( w, v )

        if ( s == 0.0D+00 ) then
          e = abs ( q )
        else
          e = abs ( q - s ) / abs ( s )
        end if

        write ( *, '(2x,i6,2x,i6,2x,i6,2x,f24.16)' ) p(1:3), e

      end do

    end do

  end do

  return
end
subroutine legendre_3d_monomial_integral ( a, b, p, value )

!*****************************************************************************80
!
!! LEGENDRE_3D_MONOMIAL_INTEGRAL the Legendre integral of a monomial.
!
!  Discussion:
!
!    The Legendre integral to be evaluated has the form
!
!      I(f) = integral ( z1 <= z <= z2 )
!             integral ( y1 <= y <= y2 ) 
!             integral ( x1 <= x <= x2 ) x^i y^j z^k dx dy dz
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(3), the lower limits of integration.
!
!    Input, real ( kind = 8 ) B(3), the upper limits of integration.
!
!    Input, integer ( kind = 4 ) P(3), the exponents of X and Y.
!
!    Output, real ( kind = 8 ) VALUE, the value of the exact integral.
!
  implicit none

  real ( kind = 8 ) a(3)
  real ( kind = 8 ) b(3)
  integer ( kind = 4 ) p(3)
  real ( kind = 8 ) value

  value = ( b(1) ** ( p(1) + 1 ) - a(1) ** ( p(1) + 1 ) ) &
        / real ( p(1) + 1, kind = 8 ) &
        * ( b(2) ** ( p(2) + 1 ) - a(2) ** ( p(2) + 1 ) ) &
        / real ( p(2) + 1, kind = 8 ) &
        * ( b(3) ** ( p(3) + 1 ) - a(3) ** ( p(3) + 1 ) ) &
        / real ( p(3) + 1, kind = 8 )

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
