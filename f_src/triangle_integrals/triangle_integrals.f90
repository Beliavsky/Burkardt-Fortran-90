subroutine i4_to_pascal ( k, i, j )

!*****************************************************************************80
!
!! I4_TO_PASCAL converts a linear index to Pascal triangle coordinates.
!
!  Discussion:
!
!    We describe the grid points in Pascal's triangle in two ways:
!
!    As a linear index K:
!
!                     1
!                   2   3
!                 4   5   6
!               7   8   9   10
!
!    As elements (I,J) of Pascal's triangle:
!
!                     0,0
!                  1,0   0,1
!               2,0   1,1    0,2
!            3,0   2,1   1,2    0,3
!
!  Example:
!
!     K  I  J
!
!     1  0  0
!     2  1  0
!     3  0  1
!     4  2  0
!     5  1  1
!     6  0  2
!     7  3  0
!     8  2  1
!     9  1  2
!    10  0  3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the linear index of the (I,J) element.
!    1 <= K.
!
!    Output, integer ( kind = 4 ) I, J, the Pascal indices.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  if ( k <= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'I4_TO_PASCAL - Fatal error!'
    write ( *, '(a)' ) '  K must be positive.'
    stop 1
  end if

  call i4_to_pascal_degree ( k, d )

  j = k - ( d * ( d + 1 ) ) / 2 - 1
  i = d - j

  return
end
subroutine i4_to_pascal_degree ( k, d )

!*****************************************************************************80
!
!! I4_TO_PASCAL_DEGREE converts a linear index to a Pascal triangle degree.
!
!  Discussion:
!
!    We describe the grid points in Pascal's triangle in two ways:
!
!    As a linear index K:
!
!                     1
!                   2   3
!                 4   5   6
!               7   8   9   10
!
!    As elements (I,J) of Pascal's triangle:
!
!                     0,0
!                  1,0   0,1
!               2,0   1,1    0,2
!            3,0   2,1   1,2    0,3
!
!    The quantity D represents the "degree" of the corresponding monomial,
!    that is, D = I + J.
!
!    We can compute D directly from K using the quadratic formula.
!
!  Example:
!
!     K  I  J  D
!
!     1  0  0  0
!
!     2  1  0  1
!     3  0  1  1
!
!     4  2  0  2
!     5  1  1  2
!     6  0  2  2
!
!     7  3  0  3
!     8  2  1  3
!     9  1  2  3
!    10  0  3  3
!
!    11  4  0  4
!    12  3  1  4
!    13  2  2  4
!    14  1  3  4
!    15  0  4  4
!
!    16  5  0  5
!    17  4  1  5
!    18  3  2  5
!    19  2  3  5
!    20  1  4  5
!    21  0  5  5
!
!    22  6  0  6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the linear index of the (I,J) element.
!    1 <= K.
!
!    Output, integer ( kind = 4 ) D, the degree (sum) of the corresponding 
!    Pascal indices.
!
  implicit none

  real ( kind = 8 ) arg
  integer ( kind = 4 ) d
  integer ( kind = 4 ) k

  if ( k <= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'I4_TO_PASCAL_DEGREE - Fatal error!'
    write ( *, '(a)' ) '  K must be positive.'
    stop 1
  end if

  arg = real ( 1 + 8 * ( k - 1 ), kind = 8 )

  d = int ( 0.5D+00 * ( -1.0D+00 + sqrt ( arg ) ) )

  return
end
subroutine pascal_to_i4 ( i, j, k )

!*****************************************************************************80
!
!! PASCAL_TO_I4 converts Pacal triangle coordinates to a linear index.
!
!  Discussion:
!
!    We describe the grid points in a Pascal triangle in two ways:
!
!    As a linear index K:
!
!                     1
!                   2   3
!                 4   5   6
!               7   8   9   10
!
!    As elements (I,J) of Pascal's triangle:
!
!                     0,0
!                  1,0   0,1
!               2,0   1,1    0,2
!            3,0   2,1   1,2    0,3
!
!  Example:
!
!     K  I  J
!
!     1  0  0
!     2  1  0
!     3  0  1
!     4  2  0
!     5  1  1
!     6  0  2
!     7  3  0
!     8  2  1
!     9  1  2
!    10  0  3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, J, the row and column indices.  I and J 
!    must be nonnegative.
!
!    Output, integer ( kind = 4 ) K, the linear index of the (I,J) element.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  if ( i < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PASCAL_TO_I4 - Fatal error!'
    write ( *, '(a)' ) '  I < 0.'
    write ( *, '(a,i8)' ) '  I = ', i
    stop 1
  else if ( j < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PASCAL_TO_I4 - Fatal error!'
    write ( *, '(a)' ) '  J < 0.'
    write ( *, '(a,i8)' ) '  J = ', j
    stop 1
  end if

  d = i + j

  k = ( d * ( d + 1 ) ) / 2 + j + 1

  return
end
subroutine poly_power ( d1, p1, n, d2, p2 )

!*****************************************************************************80
!
!! POLY_POWER computes a power of a polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D1, the degree of the polynomial.
!
!    Input, real ( kind = 8 ) P1(M1), the polynomial coefficients.
!    M1 = ((D1+1)*(D1+2))/2.
!
!    Input, integer ( kind = 4 ) N, the nonnegative integer power.
!
!    Input, integer ( kind = 4 ) D2, the degree of the power polynomial.
!    D2 = N * D1.
!
!    Output, real ( kind = 8 ) P2(M2), the polynomial power.
!    M2 = ((D2+1)*(D2+2))/2.
!
  implicit none

  integer ( kind = 4 ) d1
  integer ( kind = 4 ) d2
  integer ( kind = 4 ) n

  integer ( kind = 4 ) d3
  integer ( kind = 4 ) d4
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) m3
  integer ( kind = 4 ) m4
  real ( kind = 8 ) p1(( ( d1 + 1 ) * ( d1 + 2 ) ) / 2)
  real ( kind = 8 ) p2(( ( d2 + 1 ) * ( d2 + 2 ) ) / 2)
  real ( kind = 8 ), allocatable :: p3(:)
  real ( kind = 8 ), allocatable :: p4(:)

  m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2
!
!  Create P3, a polynomial representation of 1, that is
!  big enough to store the final result.
!
  d3 = n * d1
  m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2
  allocate ( p3(1:m3) )
  p3(1) = 1.0D+00
  p3(2:m3) = 0.0D+00
!
!  Create P4, big enough to hold the result.
!
  d4 = n * d1
  m4 = ( ( d4 + 1 ) * ( d4 + 2 ) ) / 2
  allocate ( p4(1:m4) )
  p4(1:m4) = 0.0D+00
!
!  Now set D3 to 0, to indicate that P3 currently contains only
!  a constant term.
!
  d3 = 0
!
!  Iterate N times:
!    P <= P1 * P
!
  do i = 1, n
    d4 = d1 + d3
    call poly_product ( d1, p1, d3, p3, d4, p4 )
    d3 = d4
    p3(1:m3) = p4(1:m4)
  end do
!
!  Copy the result to the user.
!
  p2(1:m2) = p3(1:m3)
!
!  Free memory.
!
  deallocate ( p3 )
  deallocate ( p4 )

  return
end
subroutine poly_power_linear ( d1, p1, n, d2, p2 )

!*****************************************************************************80
!
!! POLY_POWER_LINEAR computes the polynomial ( a + b*x + c*y ) ^ n.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D1, the degree of the linear polynomial,
!    which should be 1 (or possibly 0).
!
!    Input, real ( kind = 8 ) P1(M1), the coefficients of the linear polynomial.
!    M1 = ( (D1+1)*(D1+2) ) / 2, which should be 3.
!
!    Input, integer ( kind = 4 ) N, the power to which the polynomial is to be 
!    raised.  0 <= N.
!
!    Input, integer ( kind = 4 ) D2, the degree of the power polyynomial.
!    D2 = N * D1 = N.
!
!    Output, real ( kind = 8 ) P2(M2), the coefficients of the power polynomial.
!    M2 = ( (D2+1)*(D2+2) ) / 2, which should be ((N+1)*(N+2))/2.
!
  implicit none

  integer ( kind = 4 ) d1
  integer ( kind = 4 ) d2
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) p1(( ( d1 + 1 ) * ( d1 + 2 ) ) / 2 )
  real ( kind = 8 ) p2(( ( d2 + 1 ) * ( d2 + 2 ) ) / 2 )
  integer ( kind = 4 ) trinomial

  if ( d1 < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'POLY_POWER_LINEAR - Fatal error!'
    write ( *, '(a)' ) '  D1 < 0.'
    stop 1
  end if

  if ( n < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'POLY_POWER_LINEAR - Fatal error!'
    write ( *, '(a)' ) '  N < 0.'
    stop 1
  end if

  if ( d1 == 0 ) then
    p2(1) = p1(1) ** n
    return
  end if

  if ( n == 0 ) then
    p2(1) = 1.0D+00
    return
  end if
!
!  Use the Trinomial formula.
!
  do i = 0, n
    do j = 0, n - i
      do k = 0, n - i - j
!
!  We store X^J Y^K in location L.
!
        call pascal_to_i4 ( j, k, l )
        p2(l) = real ( trinomial ( i, j, k ), kind = 8 ) &
          * p1(1) ** i * p1(2) ** j * p1(3) ** k
      end do
    end do
  end do

  return
end
subroutine poly_print ( d, p, title )

!*****************************************************************************80
!
!! POLY_PRINT prints an XY polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D, the degree of the polynomial.
!
!    Output, real ( kind = 8 ) P(M), the coefficients of all monomials of 
!    degree 0 through D.  P must contain ((D+1)*(D+2))/2 entries.
!
!    Input, character ( len = * ) TITLE, a title string.
!
  implicit none

  integer ( kind = 4 ) d

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  real ( kind = 8 ) p(( ( d + 1 ) * ( d + 2 ) ) / 2)
  character ( len = * ) title

  m = ( ( d + 1 ) * ( d + 2 ) ) / 2

  if ( all ( p(1:m) == 0.0D+00 ) ) then

    write ( *, '(a)' ) trim ( title ) // ' = 0'

  else

    write ( *, '(a)' ) trim ( title ) // ' = '

    do k = 1, m

      call i4_to_pascal ( k, i, j )

      if ( p(k) /= 0.0D+00 ) then

        if ( p(k) < 0.0D+00 ) then
          write ( *, '(2x,a,g14.6)', advance = 'no' ) '-', abs ( p(k) )
        else
          write ( *, '(2x,a,g14.6)', advance = 'no' ) '+', p(k)
        end if

        if ( i + j /= 0 ) then
          write ( *, '(a)', advance = 'no' ) ' '
        end if

        if ( i == 0 ) then
        else if ( i == 1 ) then
          write ( *, '(a)', advance = 'no' ) 'x'
        else
          write ( *, '(a,i2)', advance = 'no' ) 'x^', i
        end if

        if ( j == 0 ) then
        else if ( j == 1 ) then
          write ( *, '(a)', advance = 'no' ) 'y'
        else
          write ( *, '(a,i2)', advance = 'no' ) 'y^', j
        end if

        write ( *, '(a)' )

      end if

    end do

  end if

  return
end
subroutine poly_product ( d1, p1, d2, p2, d3, p3 )

!*****************************************************************************80
!
!! POLY_PRODUCT computes P3(x,y) = P1(x,y) * P2(x,y) for polynomials.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D1, the degree of factor 1.
!
!    Input, real ( kind = 8 ) P1(M1), the factor 1 coefficients.
!    M1 = ((D1+1)*(D1+2))/2.
!
!    Input, integer ( kind = 4 ) D2, the degree of factor 2.
!
!    Input, real ( kind = 8 ) P2(M2), the factor2 coefficients.
!    M2 = ((D2+1)*(D2+2))/2.
!
!    Input, integer ( kind = 4 ) D3, the degree of the result.
!    D3 = D1 + D2.
!
!    Output, real ( kind = 8 ) P3(M3), the result coefficients.
!    M3 = ((D3+1)*(D3+2))/2.
!
  implicit none

  integer ( kind = 4 ) d1
  integer ( kind = 4 ) d2
  integer ( kind = 4 ) d3

  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j3
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) k2
  integer ( kind = 4 ) k3
  integer ( kind = 4 ) m1
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) m3
  real ( kind = 8 ) p1(( ( d1 + 1 ) * ( d1 + 2 ) ) / 2)
  real ( kind = 8 ) p2(( ( d2 + 1 ) * ( d2 + 2 ) ) / 2)
  real ( kind = 8 ) p3(( ( d3 + 1 ) * ( d3 + 2 ) ) / 2)

  m1 = ( ( d1 + 1 ) * ( d1 + 2 ) ) / 2
  m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2
  m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2
!
!  Consider each entry in P1:
!    P1(K1) * X^I1 * Y^J1
!  and multiply it by each entry in P2:
!    P2(K2) * X^I2 * Y^J2
!  getting 
!    P3(K3) = P3(K3) + P1(K1) * P2(X2) * X^(I1+I2) * Y(J1+J2)
!
  p3(1:m3) = 0.0D+00

  do k1 = 1, m1
    call i4_to_pascal ( k1, i1, j1 )
    do k2 = 1, m2
      call i4_to_pascal ( k2, i2, j2 )
      i3 = i1 + i2
      j3 = j1 + j2
      call pascal_to_i4 ( i3, j3, k3 )
      p3(k3) = p3(k3) + p1(k1) * p2(k2)
    end do
  end do

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
subroutine rs_to_xy_map ( t, a, b, c, d, e, f )

!*****************************************************************************80
!
!! RS_TO_XY_MAP returns the linear map from reference to physical triangle.
!
!  Discussion:
!
!    This function returns the coefficients of the linear map that sends
!    the vertices of the reference triangle, (0,0), (1,0) and (0,1), to
!    the vertices of a physical triangle T, of the form:
!
!      X = A + B * R + C * S;
!      Y = D + E * R + F * S.
!
!  Reference Element:
!
!    |
!    1  3
!    |  |\
!    |  | \
!    S  |  \
!    |  |   \
!    |  |    \
!    0  1-----2
!    |
!    +--0--R--1-->
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the coordinates of the vertices.  The
!    vertices are assumed to be the images of (0,0), (1,0) and (0,1) 
!    respectively.
!
!    Output, real ( kind = 8 ) A, B, C, D, E, F, the mapping coefficients.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) f
  real ( kind = 8 ) t(2,3)

  a = t(1,1)
  b = t(1,2) - t(1,1)
  c = t(1,3) - t(1,1)

  d = t(2,1)
  e = t(2,2) - t(2,1)
  f = t(2,3) - t(2,1)

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
function triangle01_monomial_integral ( i, j )

!*****************************************************************************80
!
!! TRIANGLE01_MONOMIAL_INTEGRAL: monomial integrals in the unit triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, J, the exponents.  
!    Each exponent must be nonnegative.
!
!    Output, real ( kind = 8 ) TRIANGLE01_MONOMIAL_INTEGRAL, the integral.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) q
  real ( kind = 8 ) triangle01_monomial_integral

  k = 0
  q = 1.0D+00

  do l = 1, i
    k = k + 1
    q = q * real ( l, kind = 8 ) / real ( k, kind = 8 )
  end do

  do l = 1, j
    k = k + 1
    q = q * real ( l, kind = 8 ) / real ( k, kind = 8 )
  end do

  do l = 1, 2
    k = k + 1
    q = q / real ( k, kind = 8 )
  end do

  triangle01_monomial_integral = q

  return
end
function triangle01_poly_integral ( d, p )

!*****************************************************************************80
!
!! TRIANGLE01_POLY_INTEGRAL: polynomial integral over the unit triangle.
!
!  Discussion:
!
!    The unit triangle is T = ( (0,0), (1,0), (0,1) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D, the degree of the polynomial.
!
!    Input, real ( kind = 8 ) P(M), the polynomial coefficients.
!    M = ((D+1)*(D+2))/2.
!
!    Output, real ( kind = 8 ) TRIANGLE01_POLY_INTEGRAL, the integral.
!
  implicit none

  integer ( kind = 4 ) d

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  real ( kind = 8 ) p(( ( d + 1 ) * ( d + 2 ) ) / 2)
  real ( kind = 8 ) q
  real ( kind = 8 ) triangle01_poly_integral
  real ( kind = 8 ) triangle01_monomial_integral

  m = ( ( d + 1 ) * ( d + 2 ) ) / 2

  q = 0.0D+00
  do k = 1, m
    call i4_to_pascal ( k, i, j )
    q = q + p(k) * triangle01_monomial_integral ( i, j )
  end do

  triangle01_poly_integral = q

  return
end
function triangle_area ( t )

!*****************************************************************************80
!
!! TRIANGLE_AREA returns the area of a triangle.
!
!  Discussion:
!
!    If the vertices are given in counter clockwise order, the area
!    will be positive.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    18 April 2015
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the vertices of the triangle.
!
!    Output, real ( kind = 8 ) TRIANGLE_AREA, the area of the triangle.
!
  implicit none

  real ( kind = 8 ) t(2,3)
  real ( kind = 8 ) triangle_area

  triangle_area = 0.5D+00 * &
    ( &
        ( t(1,2) - t(1,1) ) * ( t(2,3) - t(2,1) ) &
      - ( t(1,3) - t(1,1) ) * ( t(2,2) - t(2,1) ) &
    )

  return
end
function triangle_monomial_integral ( i, j, t )

!*****************************************************************************80
!
!! TRIANGLE_MONOMIAL_INTEGRAL integrates a monomial over an arbitrary triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, J, the exponents of X and Y in the monomial.
!    0 <= I, J.
!
!    Input, real ( kind = 8 ) T(2,3), the vertices of the triangle.
!
!    Output, real ( kind = 8 ) TRIANGLE_MONOMIAL_INTEGRAL, the integral 
!    of X^I * Y^J over triangle T.
!
  implicit none

  integer ( kind = 4 ), parameter :: d1 = 1
  integer ( kind = 4 ), parameter :: d2 = 1

  integer ( kind = 4 ), parameter :: m1 = ( ( d1 + 1 ) * ( d1 + 2 ) ) / 2
  integer ( kind = 4 ), parameter :: m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  integer ( kind = 4 ) d3
  integer ( kind = 4 ) d4
  integer ( kind = 4 ) d5
  real ( kind = 8 ) e
  real ( kind = 8 ) f
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m3
  integer ( kind = 4 ) m4
  integer ( kind = 4 ) m5
  real ( kind = 8 ) p1(m1)
  real ( kind = 8 ) p2(m2)
  real ( kind = 8 ), allocatable :: p3(:)
  real ( kind = 8 ), allocatable :: p4(:)
  real ( kind = 8 ), allocatable :: p5(:)
  real ( kind = 8 ) q
  real ( kind = 8 ) t(2,3)
  real ( kind = 8 ) triangle_area
  real ( kind = 8 ) triangle_monomial_integral
  real ( kind = 8 ) triangle01_poly_integral
!
!  Get map coefficients from reference RS triangle to general XY triangle.
!    R = a+b*X+c*Y
!    S = d+e*X+f*Y
!
  call rs_to_xy_map ( t, a, b, c, d, e, f )
!
!  Set
!    P1(R,S) = a+b*R+c*S
!    P2(R,S) = d+e*R+f*S
!
  p1(1) = a
  p1(2) = b
  p1(3) = c

  p2(1) = d
  p2(2) = e
  p2(3) = f
!
!  Exponentiate:
!    P3(R,S) = P1(R,S)^i
!    P4(R,S) = P2(R,S)^j
!
  d3 = i * d1
  m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2
  allocate ( p3(1:m3) )
  call poly_power_linear ( d1, p1, i, d3, p3 )

  d4 = j * d2
  m4 = ( ( d4 + 1 ) * ( d4 + 2 ) ) / 2
  allocate ( p4(1:m4) )
  call poly_power_linear ( d2, p2, j, d4, p4 )
!
!  Compute the product 
!    P5(R,S) = P3(R,S) * P4(R,S)
!
  d5 = d3 + d4
  m5 = ( ( d5 + 1 ) * ( d5 + 2 ) ) / 2
  allocate ( p5(1:m5) )
  call poly_product ( d3, p3, d4, p4, d5, p5 )
!
!  Compute the integral of P5(R,S) over the reference triangle.
!
  q = triangle01_poly_integral ( d5, p5 )
!
!  Multiply by the area of the physical triangle T(X,Y) divided by
!  the area of the reference triangle.
!
  q = q * triangle_area ( t ) / 0.5D+00

  triangle_monomial_integral = q

  deallocate ( p3 )
  deallocate ( p4 )
  deallocate ( p5 )

  return
end
function triangle_poly_integral ( d, p, t )

!*****************************************************************************80
!
!! TRIANGLE_POLY_INTEGRAL: polynomial integral over a triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) D, the degree of the polynomial.
!
!    Input, real ( kind = 8 ) P(M), the polynomial coefficients.
!    M = ((D+1)*(D+2))/2.
!
!    Input, real ( kind = 8 ) T(2,3), the vertices of the triangle.
!
!    Output, real ( kind = 8 ) TRIANGLE_POLY_INTEGRAL, the integral.
!
  implicit none

  integer ( kind = 4 ) d

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  real ( kind = 8 ) p(( ( d + 1 ) * ( d + 2 ) ) / 2)
  real ( kind = 8 ) q
  real ( kind = 8 ) t(2,3)
  real ( kind = 8 ) triangle_monomial_integral
  real ( kind = 8 ) triangle_poly_integral

  m = ( ( d + 1 ) * ( d + 2 ) ) / 2

  q = 0.0D+00
  do k = 1, m
    call i4_to_pascal ( k, i, j )
    q = q + p(k) * triangle_monomial_integral ( i, j, t )
  end do

  triangle_poly_integral = q

  return
end
function triangle_xy_integral ( x1, y1, x2, y2, x3, y3 )

!*****************************************************************************80
!
!! TRIANGLE_XY_INTEGRAL computes the integral of XY over a triangle.
!
!  Discussion:
!
!    This function was written as a special test case for the general
!    problem of integrating a monomial x^alpha * y^beta over a general 
!    triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X1, Y1, X2, Y2, X3, Y3, the coordinates of the
!    triangle vertices.
!
!    Output, real ( kind = 8 ) TRIANGLE_XY_INTEGRAL, the integral of X*Y 
!    over the triangle.
!
  implicit none

  real ( kind = 8 ) det
  real ( kind = 8 ) p00
  real ( kind = 8 ) p01
  real ( kind = 8 ) p02
  real ( kind = 8 ) p10
  real ( kind = 8 ) p11
  real ( kind = 8 ) p20
  real ( kind = 8 ) q
  real ( kind = 8 ) triangle01_monomial_integral
  real ( kind = 8 ) triangle_xy_integral
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
!
!  x = x1 * ( 1 - xi - eta )
!    + x2 *       xi
!    + x3 *            eta
!
!  y = y1 * ( 1 - xi - eta )
!    + y2 *       xi
!    + y3 *            eta
!
!  Rewrite as linear polynomials in (xi,eta):
!
!  x = x1 + ( x2 - x1 ) * xi + ( x3 - x1 ) * eta
!  y = y1 + ( y2 - y1 ) * xi + ( y3 - y1 ) * eta
!
!  Jacobian:
!
!    J = [ ( x2 - x1 )  ( x3 - x1 ) ]
!        [ ( y2 - y1 )  ( y3 - y1 ) ]
!
!    det J = ( x2 - x1 ) * ( y3 - y1 ) - ( y2 - y1 ) * ( x3 - x1 )
!
!  Integrand
!
!    x * y = ( x1 + ( x2 - x1 ) * xi + ( x3 - x1 ) * eta )
!          * ( y1 + ( y2 - y1 ) * xi + ( y3 - y1 ) * eta )
!
!  Rewrite as linear combination of monomials:
!
!    x * y = 1      * x1 * y1
!          + eta    * ( x1 * ( y3 - y1 ) + ( x3 - x1 ) * y1 )
!          + xi     * ( x1 * ( y2 - y1 ) + ( x2 - x1 ) * y1 )
!          + eta^2  * ( x3 - x1 ) * ( y3 - y1 )
!          + xi*eta * ( ( x2 - x1 ) * ( y3 - y1 ) + ( x3 - x1 ) * ( y2 - y1 ) )
!          + xi^2   * ( x2 - x1 ) * ( y2 - y1 )
!
  det = ( x2 - x1 ) * ( y3 - y1 ) - ( y2 - y1 ) * ( x3 - x1 )

  p00 = x1 * y1

  p01 = x1 * ( y3 - y1 ) + ( x3 - x1 ) * y1
  p10 = x1 * ( y2 - y1 ) + ( x2 - x1 ) * y1

  p02 = ( x3 - x1 ) * ( y3 - y1 )
  p11 = ( x2 - x1 ) * ( y3 - y1 ) + ( x3 - x1 ) * ( y2 - y1 )
  p20 = ( x2 - x1 ) * ( y2 - y1 )

  q = 0.0D+00
  q = q + p00 * triangle01_monomial_integral ( 0, 0 )
  q = q + p10 * triangle01_monomial_integral ( 1, 0 )
  q = q + p01 * triangle01_monomial_integral ( 0, 1 )
  q = q + p20 * triangle01_monomial_integral ( 2, 0 )
  q = q + p11 * triangle01_monomial_integral ( 1, 1 )
  q = q + p02 * triangle01_monomial_integral ( 0, 2 )

  q = q * det

  triangle_xy_integral = q

  return
end
function trinomial ( i, j, k )

!*****************************************************************************80
!
!! TRINOMIAL computes a trinomial coefficient.
!
!  Discussion:
!
!    The trinomial coefficient is a generalization of the binomial
!    coefficient.  It may be interpreted as the number of combinations of
!    N objects, where I objects are of type 1, J of type 2, and K of type 3.
!    and N = I + J + K.
!
!    T(I,J,K) = N! / ( I! J! K! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, J, K, the factors.
!    All should be nonnegative.
!
!    Output, integer ( kind = 4 ) TRINOMIAL, the trinomial coefficient.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) t
  integer ( kind = 4 ) trinomial
  integer ( kind = 4 ) value
!
!  Each factor must be nonnegative.
!
  if ( i < 0 .or. j < 0 .or. k < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRINOMIAL - Fatal error!'
    write ( *, '(a)' ) '  Negative factor encountered.'
    stop 1
  end if

  value = 1

  t = 1

  do l = 1, i
!   value = value * t / l
    t = t + 1
  end do

  do l = 1, j
    value = value * t / l
    t = t + 1
  end do

  do l = 1, k
    value = value * t / l
    t = t + 1
  end do
  
  trinomial = value

  return
end
subroutine xy_to_rs_map ( t, a, b, c, d, e, f )

!*****************************************************************************80
!
!! XY_TO_RS_MAP returns the linear map from physical to reference triangle.
!
!  Discussion:
!
!    Given the vertices T of an arbitrary triangle in the (X,Y) coordinate
!    system, this function returns the coefficients of the linear map
!    that sends the vertices of T to (0,0), (1,0) and (0,1) respectively
!    in the reference triangle with coordinates (R,S):
!
!      R = A + B * X + C * Y;
!      S = D + E * X + F * Y.
!
!  Reference Element T3:
!
!    |
!    1  3
!    |  |\
!    |  | \
!    S  |  \
!    |  |   \
!    |  |    \
!    0  1-----2
!    |
!    +--0--R--1-->
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the X and Y coordinates
!    of the vertices.  The vertices are assumed to be the images of
!    (0,0), (1,0) and (0,1) respectively.
!
!    Output, real ( kind = 8 ) A, B, C, D, E, F, the mapping coefficients.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) f
  real ( kind = 8 ) g
  real ( kind = 8 ) t(2,3)

  g =   ( ( t(2,3) - t(2,1) ) * ( t(1,2) - t(1,1) )   &
        - ( t(1,3) - t(1,1) ) * ( t(2,2) - t(2,1) ) )

  a = ( - ( t(2,3) - t(2,1) ) * t(1,1)  &
        + ( t(1,3) - t(1,1) ) * t(2,1) ) / g

  b =     ( t(2,3) - t(2,1) ) / g

  c =   - ( t(1,3) - t(1,1) ) / g

  d = (   ( t(2,2) - t(2,1) ) * t(1,1) &
        - ( t(1,2) - t(1,1) ) * t(2,1) ) / g

  e =   - ( t(2,2) - t(2,1) ) / g

  f =     ( t(1,2) - t(1,1) ) / g

  return
end
