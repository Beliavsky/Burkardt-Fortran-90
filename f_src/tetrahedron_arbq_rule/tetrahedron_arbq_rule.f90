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
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
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
subroutine kjacoypols3 ( x, y, a, b, n, pols )

!*****************************************************************************80
!
!! KJACOYPOLS3 evaluates modified Jacobi polynomials.
!
!  Discussion:
!
!    This procedure evaluates Jacobi polynomials multiplied by
!    specific polynomials given by the formula
!      P_n^{(a,b)} (x/y) y^n
!    at the user-provided point x/y.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    13 June 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, Y, define the evaluation point X/Y.
!
!    Input, real ( kind = 8 ) A, B, the parameters.
!
!    Input, integer ( kind = 4 ) N, the highest degree to compute.
!
!    Output, real ( kind = 8 ) POLS(N+1), the polynomial values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  integer ( kind = 4 ) k
  real ( kind = 8 ) k_r8
  real ( kind = 8 ) pk
  real ( kind = 8 ) pkm1
  real ( kind = 8 ) pkp1
  real ( kind = 8 ) pols(n+1)
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  pkp1 = 1.0D+00
  pols(1) = pkp1

  if ( n == 0 ) then
    return
  end if

  pk = pkp1
  pkp1 = 0.5D+00 * ( ( a - b ) * y + ( 2.0D+00 + a + b ) * x )
  pols(2)= pkp1

  if ( n == 1 ) then
    return
  end if

  do k = 2, n

    k_r8 = real ( k, kind = 8 )

    alpha = ( 2.0D+00 * k_r8 + a + b - 1.0D+00 ) &
      * ( a - b ) * ( a + b ) * y &
      + ( 2.0D+00 * k_r8 + a + b - 1.0D+00 ) &
      * ( 2.0D+00 * k_r8 + a + b - 2.0D+00 ) &
      * ( 2.0D+00 * k_r8 + a + b ) * x

    beta = 2.0D+00 * ( k_r8 + a - 1.0D+00 ) &
      * ( k_r8 + b - 1.0D+00 ) &
      * ( 2.0D+00 * k_r8 + a + b ) * y * y

    pkm1 = pk
    pk = pkp1
    pkp1 = ( alpha * pk - beta * pkm1 ) &
      / ( 2.0D+00 * k_r8 * ( k_r8 + a + b ) &
      * ( 2.0D+00 * k_r8 + a + b - 2.0D+00 ) )

    pols(k+1) = pkp1

  end do

  return
end
subroutine klegeypols ( x, y, n, pols )

!*****************************************************************************80
!
!! KLEGEYPOLS evaluates scaled Legendre polynomials.
!
!  Discussion:
!
!    This routine evaluate a sequence of scaled Legendre polynomials
!    P_n(x/y) y^n, with the parameter y in [0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    13 June 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Input, real ( kind = 8 ) Y, the parameter.
!
!    Input, integer ( kind = 4 ) N, the highest degree to be evaluated.
!
!    Output, real ( kind = 8 ) POLS(N+1), the polynomial values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) k
  real ( kind = 8 ) pk
  real ( kind = 8 ) pkm1
  real ( kind = 8 ) pkp1
  real ( kind = 8 ) pols(n+1)
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  pkp1 = 1.0D+00
  pols(1) = pkp1
  if ( n == 0 ) then
    return
  end if

  pk = pkp1
  pkp1 = x
  pols(2) = pkp1
  if ( n == 1 ) then
    return
  end if

  do k = 1, n - 1
    pkm1 = pk
    pk = pkp1
    pkp1 = ( ( 2.0D+00 * k + 1.0D+00 ) * x * pk &
      - k * pkm1 * y * y ) / ( k + 1.0D+00 )
    pols(k+2) = pkp1
  end do

  return
end
subroutine ortho3eva ( degree, xyz, fvals )

!*****************************************************************************80
!
!! ORTHO3EVA evaluates polynomials orthogonal in the reference triangle.
!
!  Discussion:
!
!    This procedure evaluates the Koornwinder's orthogonal polynomial
!    up to order DEGREE on the reference tetrahedron with vertices
!      (-1, -1/Sqrt(3), -1/Sqrt(6)),
!      ( 0,  2/Sqrt(3), -1/Sqrt(6)),
!      ( 1, -1/Sqrt(3), -1/Sqrt(6)),
!      ( 0,  0,      3/Sqrt(6))
!
!    The polynomials are ordered by their order, and in each order,
!    they are ordered lexicographically in terms of their indices
!    (m,n,k).
!
!    The total number of polynomials should be
!      NVALS = ( ( DEGREE + 1 ) * ( DEGREE + 2 ) * ( DEGREE + 3 ) ) / 6.
!
!    The calculations are based on Koornwinder's representation
!    of the orthogonal polynomials on the right triangle
!      (-1,-1,-1), (-1,1,-1), (1,-1,-1),(-1,-1,1)
!    given by:
!      K_mnk(x,y,z) =
!        P_m ((2x+2+y+z)/(-y-z)) * ((-y-z)/2)^m *
!        P_n^{2m+1,0}((2y+1+z)/(1-z)) * ((1-z)/2)^{n}
!        P_k^{2m+2n+2,0} (z)
!    with the input (x,y,z) transformed as
!      x = -1/2 + xold -yold/s3 - zold/s6
!      y = -1/2 + 2/s3 * yold - zold/s6
!      z = -1/2 + s6/2 * zold
!    where
!      s3=sqrt(3)
!      s6=sqrt(6)
!    and
!      P_m is the Legendre polynomial of degree m,
!      P_n^{2m+1,0} the Jacobi polynomial of degree n, parameters 2*m+1 and 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DEGREE, the maximum degree.
!
!    Input, real ( kind = 8 ) XYZ(3), the evaluation point.
!
!    Output, real ( kind = 8 ) FVALS(NVALS), the polynomial values.
!
  implicit none

  integer ( kind = 4 ) degree

  real ( kind = 8 ) f1(degree+1)
  real ( kind = 8 ) f2s(degree+1,degree+1)
  real ( kind = 8 ) f3s(degree+1,degree+1)
  real ( kind = 8 ) fvals(((degree+1)*(degree+2)*(degree+3))/6)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mmax
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncount
  real ( kind = 8 ) p1
  real ( kind = 8 ) p2
  real ( kind = 8 ) scale
  real ( kind = 8 ) uvw(3)
  real ( kind = 8 ) x1
  real ( kind = 8 ) xyz(3)
  real ( kind = 8 ) y1
!
!  Convert coordinates from reference to Koornwinder tetrahedron.
!
  call ref_to_koorn ( xyz, uvw )
!
!  Compute F1.
!
  p1 = 0.5D+00 * ( 2.0D+00 * uvw(1) + 2.0D+00 + uvw(2) + uvw(3) )
  p2 = - 0.5D+00 * ( uvw(2) + uvw(3) )

  call klegeypols ( p1, p2, degree, f1 )
!
!  Compute F2S.
!
  do j = 1, degree + 1
    do i = 1, degree + 1
      f2s(i,j) = 0.0D+00
    end do
  end do

  do m = 0, degree

    x1 = 0.5D+00 * ( 2.0D+00 * uvw(2) + 1.0D+00 + uvw(3) )
    y1 = 0.5D+00 * ( 1.0D+00 - uvw(3) )
    p1 = real ( 2 * m + 1, kind = 8 )
    p2 = 0.0D+00

    call kjacoypols3 ( x1, y1, p1, p2, degree - m, f2s(1,m+1) )

  end do
!
!  Compute F3S.
!
  do j = 1, degree + 1
    do i = 1, degree + 1
      f3s(i,j) = 0.0D+00
    end do
  end do

  x1 = uvw(3)
  y1 = 1.0D+00
  p2 = 0.0D+00

  do j = 1, degree + 1

    p1 = real ( 2 * j, kind = 8 )

    call kjacoypols3 ( x1, y1, p1, p2, degree + 1 - j, f3s(1,j) )

  end do
!
!  Construct FVALS.
!
  ncount = 0

  do mmax = 0, degree

    do m = 0, mmax

      do n = 0, mmax - m

        k = mmax - m - n
        ncount = ncount + 1

        scale = sqrt &
        ( &
          4.0D+00 &
          / real ( 2 * mmax + 3, kind = 8 ) &
          / real ( 2 * m + 1, kind = 8 ) &
          / real ( n + m + 1, kind = 8 ) &
          / sqrt ( 2.0D+00 ) &
        )

        fvals(ncount) = &
          f1(m+1) * &
          f2s(n+1,m+1) * &
          f3s(k+1,m+n+1) / scale

      end do
    end do
  end do

  return
end
subroutine r8mat_row_copy ( m, n, i, v, a )

!*****************************************************************************80
!
!! R8MAT_ROW_COPY copies a vector into a row of an R8MAT.
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
!    30 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the order of the matrix.
!
!    Input, integer ( kind = 4 ) I, the index of the row.
!    1 <= I <= M.
!
!    Input, real ( kind = 8 ) V(N), the row to be copied.
!
!    Input/output, real ( kind = 8 ) A(M,N), the matrix into which
!    the row is to be copied.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)

  a(i,1:n) = v(1:n)

  return
end
subroutine r8vec_copy ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_COPY copies an R8VEC.
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
!    17 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the vectors.
!
!    Input, real ( kind = 8 ) A1(N), the vector to be copied.
!
!    Output, real ( kind = 8 ) A2(N), a copy of A1.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)

  a2(1:n) = a1(1:n)

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
function r8vec_sum ( n, a )

!*****************************************************************************80
!
!! R8VEC_SUM returns the sum of the entries of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In FORTRAN90, this facility is offered by the built in
!    SUM function:
!
!      R8VEC_SUM ( N, A ) = SUM ( A(1:N) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) R8VEC_SUM, the sum of the entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_sum

  r8vec_sum = sum ( a(1:n) )

  return
end
subroutine ref_to_koorn ( r, u )

!*****************************************************************************80
!
!! REF_TO_KOORN maps points from the reference to Koornwinder's tetrahedron.
!
!  Discussion:
!
!    The reference tetrahedron has vertices:
!
!  (-1, -1/Sqrt(3), -1/Sqrt(6) )
!  ( 0,  2/Sqrt(3), -1/Sqrt(6) )
!  ( 1, -1/Sqrt(3), -1/Sqrt(6) )
!  ( 0,  0,      3/Sqrt(6) )
!
!    Koornwinder's tetrahedron has vertices:
!
!  ( -1, -1, -1 )
!  ( -1, +1, -1 )
!  ( +1, -1, -1 )
!  ( -1, -1, +1 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R(3), the coordinates of a point in the
!    reference tetrahedron.
!
!    Output, real ( kind = 8 ) U(3), the coordinates of the point in
!    the Koornwinder tetrahedron.
!
  implicit none

  real ( kind = 8 ) a10
  real ( kind = 8 ) a11
  real ( kind = 8 ) a12
  real ( kind = 8 ) a13
  real ( kind = 8 ) a20
  real ( kind = 8 ) a21
  real ( kind = 8 ) a22
  real ( kind = 8 ) a23
  real ( kind = 8 ) a30
  real ( kind = 8 ) a31
  real ( kind = 8 ) a32
  real ( kind = 8 ) a33
  real ( kind = 8 ) s3
  real ( kind = 8 ) s6
  real ( kind = 8 ) r(3)
  real ( kind = 8 ) u(3)

  s3 = sqrt ( 3.0D+00 )
  s6 = sqrt ( 6.0D+00 )

  a10 = - 0.5D+00
  a11 =   1.0D+00
  a12 = - 1.0D+00 / s3
  a13 = - 1.0D+00 / s6

  a20 = - 0.5D+00
  a21 =   0.0D+00
  a22 =   2.0D+00 / s3
  a23 = - 1.0D+00 / s6

  a30 = - 0.5D+00
  a31 =   0.0D+00
  a32 =   0.0D+00
  a33 =   0.5D+00 * s6

  u(1) = a10 + a11 * r(1) + a12 * r(2) + a13 * r(3)
  u(2) = a20 +              a22 * r(2) + a23 * r(3)
  u(3) = a30 +                           a33 * r(3)

  return
end
subroutine rule01 ( n, x, w )

!*****************************************************************************80
!
!! RULE01 returns the rule of degree 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 1 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
       0.00000000000000000D+00 /
  data ys / &
       0.00000000000000000D+00 /
  data zs / &
       0.00000000000000000D+00 /
  data ws / &
       0.9709835434146467D+00 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule02 ( n, x, w )

!*****************************************************************************80
!
!! RULE02 returns the rule of degree 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 4 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -.2677028585910073D+00,0.1510931841533142D+00, &
    -.1367699195237390D+00,0.8067449309051964D+00 /
  data ys / &
    0.5939017006199488D-01,0.4354087732476309D+00, &
    -.3280341115590410D+00,-.3400977314285288D+00 /
  data zs / &
    -.3969426941142150D+00,0.2151719254306919D+00, &
    0.2950846519937133D+00,-.3430378951002550D+00 /
  data ws / &
    0.2918008865477151D+00,0.2706884392356724D+00, &
    0.3098349601753470D+00,0.9865925745591227D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule03 ( n, x, w )

!*****************************************************************************80
!
!! RULE03 returns the rule of degree 3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 6 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -.1685037180276000D+00,0.2783799427534418D-01, &
    -.3512216177343445D+00,0.4308532463549043D+00, &
    -.4676763747967377D+00,0.1492831253848431D+00 /
  data ys / &
    0.1910914916271708D+00,-.2304932838839657D-01, &
    0.1835144026339993D+00,-.2474715823180446D+00, &
    -.4235250827264375D+00,0.6397847685164516D+00 /
  data zs / &
    -.3896267314585163D+00,0.5481350663241830D+00, &
    0.5147815330343534D-01,-.1683315641007033D+00, &
    -.1586973077889307D+00,-.1080219253055393D+00 /
  data ws / &
    0.1287213727402025D+00,0.2179034339695993D+00, &
    0.1243503792062836D+00,0.2446917182410072D+00, &
    0.1365439875826512D+00,0.1187726516749031D+00 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule04 ( n, x, w )

!*****************************************************************************80
!
!! RULE04 returns the rule of degree 4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 11 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -.1459612280979987D-01,-.2937244896309993D+00, &
    0.3230878337158274D+00,-.8819654179396583D-01, &
    0.1270203663314710D+00,-.4414307688091184D+00, &
    0.3939418589633429D+00,-.6034469714614210D-01, &
    -.8914059834601185D-01,0.6545213033603760D+00, &
    -.7307642259677857D+00 /
  data ys / &
    0.2426564579199708D-02,0.1764396506764613D+00, &
    -.1932070410956556D+00,0.2317884270105980D-01, &
    0.5451410677215219D+00,-.3848225631590180D+00, &
    0.2068744670639530D+00,-.4573739074927080D+00, &
    0.7268092659599459D+00,-.3970356243870571D+00, &
    -.2669420088648982D+00 /
  data zs / &
    0.7093565780103633D+00,0.1108860494941134D+00, &
    0.1695426396704650D+00,-.2011819391325586D+00, &
    0.1309503990759315D+00,0.9225429679162532D-01, &
    -.3111560426198242D+00,-.3302215329322376D+00, &
    -.3507931737363739D+00,-.2970424833951137D+00, &
    -.3861037570241846D+00 /
  data ws / &
    0.1033787090646894D+00,0.1070356256090164D+00, &
    0.1504792582740940D+00,0.1877987156186583D+00, &
    0.7395274312521298D-01,0.7712199925411270D-01, &
    0.6745419368701999D-01,0.5819413648173244D-01, &
    0.5378646711152148D-01,0.5366953183949744D-01, &
    0.3811216334909158D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule05 ( n, x, w )

!*****************************************************************************80
!
!! RULE05 returns the rule of degree 5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 14 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.1664180721577685D-15,0.4089925917487008D+00, &
    0.6338467275429999D-16,-.4089925917487006D+00, &
    0.3723900931035782D-16,-.2435436770532023D+00, &
    0.7718657600705262D-17,0.2435436770532026D+00, &
    0.6290589987564350D+00,-.6290589987564351D+00, &
    -.1407768953799327D-15,0.4089925917487006D+00, &
    -.1047965211464895D-15,-.4089925917487007D+00 /
  data ys / &
    0.1094362324146226D-15,-.2361319829426750D+00, &
    0.4722639658853500D+00,-.2361319829426750D+00, &
    0.3548001489301807D-16,0.1406100075060977D+00, &
    -.2812200150121953D+00,0.1406100075060977D+00, &
    -.3631873822681842D+00,-.3631873822681841D+00, &
    0.7263747645363684D+00,0.2361319829426750D+00, &
    -.4722639658853500D+00,0.2361319829426750D+00 /
  data zs / &
    0.7704367825296720D+00,0.3339410527875835D+00, &
    0.3339410527875834D+00,0.3339410527875834D+00, &
    -.2982788694307592D+00,0.9942628981025299D-01, &
    0.9942628981025306D-01,0.9942628981025299D-01, &
    -.2568122608432240D+00,-.2568122608432239D+00, &
    -.2568122608432239D+00,-.3339410527875835D+00, &
    -.3339410527875834D+00,-.3339410527875835D+00 /
  data ws / &
    0.7136053542145054D-01,0.4131148601232373D-01, &
    0.4131148601232375D-01,0.4131148601232376D-01, &
    0.1094181214137256D+00,0.1094181214137255D+00, &
    0.1094181214137255D+00,0.1094181214137255D+00, &
    0.7136053542145050D-01,0.7136053542145052D-01, &
    0.7136053542145050D-01,0.4131148601232370D-01, &
    0.4131148601232373D-01,0.4131148601232375D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule06 ( n, x, w )

!*****************************************************************************80
!
!! RULE06 returns the rule of degree 6.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 23 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.4919941951523113D-02,0.3415058060219544D-01, &
    -.4561198328617235D+00,0.2144499229769493D+00, &
    -.1540693059995943D+00,0.3064525403222947D+00, &
    -.1305882959514773D-01,0.1535244689838072D-01, &
    -.1567784487775340D+00,-.3973616940506757D+00, &
    0.9764884616649552D-02,-.5228087984647453D+00, &
    0.5565605702210328D+00,0.8423081979023464D-01, &
    0.2174166091160236D+00,0.3212158945225647D+00, &
    -.2593648078545398D+00,0.5636454767202584D+00, &
    -.6410949644625729D+00,-.3344151090582155D+00, &
    -.1894588386892019D-01,-.8214655896199351D+00, &
    0.8597415475615655D+00 /
  data ys / &
    -.1392238503430922D-01,0.2542073830543402D+00, &
    -.3110385447445375D+00,-.1234903873249722D+00, &
    -.6865214472302168D-01,0.1818495501923178D+00, &
    -.2488891169073396D-01,-.3261143100532925D+00, &
    0.2522454301000923D+00,-.1977159083378496D+00, &
    0.6504184291133676D+00,-.6635055304123420D-01, &
    -.3186690054220115D+00,0.2138059822864521D-01, &
    0.4599507004997997D+00,-.4260052421545051D+00, &
    0.3851980211137577D+00,-.1111606227821696D+00, &
    -.1934318338507612D+00,-.4491758699723651D+00, &
    0.9010599459930961D+00,-.5109860359355179D+00, &
    -.5208085141225744D+00 /
  data zs / &
    0.1066228263967720D+01,0.6315886967039234D+00, &
    0.2300267655106985D+00,0.5632351636302827D+00, &
    0.6206264896282621D+00,0.1254685876442911D+00, &
    -.3357705926282663D+00,0.1147788985927003D+00, &
    0.2140597800705522D+00,-.7963361802882329D-01, &
    0.5868446281578688D-02,0.1934272688455952D-02, &
    0.1379740251424839D-01,0.3195756126912550D-01, &
    -.3027232303753162D+00,-.2994855602604263D+00, &
    -.2825197099253122D+00,-.3068140495300409D+00, &
    -.3660473605161587D+00,-.3102423292746128D+00, &
    -.3473952050436902D+00,-.2635645240101362D+00, &
    -.3603432585369335D+00 /
  data ws / &
    0.6889900028407776D-02,0.3059511845881479D-01, &
    0.2352818079375775D-01,0.4747145913756433D-01, &
    0.5149448476741100D-01,0.4198464552012798D-01, &
    0.6518777271610311D-01,0.5808554412413398D-01, &
    0.6079762507769643D-01,0.6246727661754427D-01, &
    0.4232555916776466D-01,0.2191089157324707D-01, &
    0.4526058614388953D-01,0.1093321687620292D+00, &
    0.5237000773398501D-01,0.4832452079715138D-01, &
    0.6123639953387314D-01,0.4577996169297793D-01, &
    0.2476427914920903D-01,0.3856173525882120D-01, &
    0.1546337820963114D-01,0.1012088745519476D-01, &
    0.7031160695311595D-02 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule07 ( n, x, w )

!*****************************************************************************80
!
!! RULE07 returns the rule of degree 7.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 31 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -.3082522913562920D+00,0.3230068564765168D+00, &
    0.1790030943099486D-02,-.1749858562850217D+00, &
    0.3515818224512229D-01,-.1148912802505062D+00, &
    0.1080242619580473D+00,0.9960886019209524D-01, &
    0.2552589657173040D+00,0.3018972093964544D+00, &
    -.3637995251662868D-01,-.1979748432674351D+00, &
    0.1705547608587194D+00,0.5382714575357732D+00, &
    -.5826255951559661D+00,-.6638827484255140D-01, &
    0.1394865450483365D+00,-.2525448211561483D+00, &
    -.3364179479356783D+00,0.5088174095091343D+00, &
    -.3997676771299581D+00,0.2233579208997628D+00, &
    -.2887689832327979D+00,0.2430056805988458D+00, &
    -.2402459184668794D+00,-.5737122495168941D+00, &
    0.5530740586345179D+00,0.7632160871540358D+00, &
    0.1108323525316276D-02,-.7666589725519221D+00, &
    0.3314379268437029D+00 /
  data ys / &
    -.1978432879678884D+00,-.1166608144800289D+00, &
    0.2760223041671345D-02,0.1492553071272583D+00, &
    0.3222570009831960D+00,0.3598150246997987D-01, &
    -.2123211082745265D+00,0.3370600346165786D+00, &
    0.2345931387652712D-01,-.2797440812728529D+00, &
    0.6784158147792628D+00,-.1099260120706853D+00, &
    0.5854481149716676D-01,-.3326202438156060D+00, &
    -.2866394288669600D+00,0.3396643732745171D+00, &
    -.3078502318368667D+00,-.9687707852320762D-01, &
    -.4222809305623556D+00,-.4607960014652947D-01, &
    0.1694117934001565D+00,0.5070263922318456D+00, &
    -.4320411612241725D+00,0.5262155488082486D+00, &
    0.4919239659783042D+00,-.7453182334351910D-01, &
    -.3355026530466742D-02,-.4345394288375978D+00, &
    0.8835815093231754D+00,-.4540327424996255D+00, &
    -.5419034033722774D+00 /
  data zs / &
    0.6553771405341420D+00,0.5963217268212818D+00, &
    0.9191670342435462D+00,0.5162921387398244D+00, &
    0.5492103599470217D+00,-.3886917653966460D+00, &
    0.5138214657068572D+00,-.2732502226402171D+00, &
    -.1289266426427488D+00,-.3368155028155599D+00, &
    0.4965154590499942D-01,0.3688101367310880D+00, &
    0.3245398450336227D+00,0.6530998463389483D-01, &
    0.4050073689194288D-01,0.4339137621154699D-01, &
    -.8554188228126980D-02,-.1212234343128167D+00, &
    0.2244936992133664D-01,-.5389155149780182D-01, &
    -.1331769110761944D-01,-.3996173060097081D-01, &
    -.3304370757138854D+00,-.3633697548034391D+00, &
    -.3184670075538235D+00,-.3348000741414298D+00, &
    -.3507444035401421D+00,-.3120479891575616D+00, &
    -.3155336884007630D+00,-.3148702231578014D+00, &
    -.3104410268762152D+00 /
  data ws / &
    0.7484517057961263D-02,0.1165548467333077D-01, &
    0.2254313978794985D-01,0.2176106698437745D-01, &
    0.2261516504544270D-01,0.2653689704536702D-01, &
    0.2796071748247393D-01,0.4174371077929897D-01, &
    0.5125012874361469D-01,0.4062452567161350D-01, &
    0.2011109093086575D-01,0.5855177928202636D-01, &
    0.6477548431925077D-01,0.3049968783939012D-01, &
    0.2749125083938057D-01,0.6480294592357103D-01, &
    0.6232630627722486D-01,0.7994900688083093D-01, &
    0.2026705321864424D-01,0.2384971520555609D-01, &
    0.2170019222676944D-01,0.2153672490681497D-01, &
    0.3586406280991152D-01,0.1621736817489311D-01, &
    0.3281522904442925D-01,0.2724694235165297D-01, &
    0.2048072851756261D-01,0.1999288967511468D-01, &
    0.1864561424137827D-01,0.1753312507928042D-01, &
    0.1215099239866789D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule08 ( n, x, w )

!*****************************************************************************80
!
!! RULE08 returns the rule of degree 8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 44 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -.1611772551670525D-01,-.7148827236840712D-02, &
    0.4589923372088558D+00,-.4520990492494178D+00, &
    0.1109560356430631D+00,0.1825224293648890D+00, &
    -.1507489805419971D+00,-.2877324734878600D+00, &
    0.3680747212378487D+00,0.4788156424890513D+00, &
    -.3478779367400819D+00,-.3548285868734744D-01, &
    0.2591650442934043D+00,-.1305037378946309D-02, &
    -.3493996070828597D-01,-.1660462859707297D+00, &
    0.1010085522422429D+00,-.3587468163605144D+00, &
    -.3473067563985682D+00,-.8635741121401184D-01, &
    0.2901168201778487D+00,0.1042093355150105D+00, &
    -.6419387451918364D-01,-.2016986972118968D+00, &
    0.1846170677356399D+00,0.3788893958160157D+00, &
    -.4818716163165796D+00,0.6821719050778170D+00, &
    0.3978642293015137D+00,-.6975889470159129D+00, &
    0.2742964749336314D-02,0.1645687860050991D-01, &
    -.4618349214785903D+00,0.1575090568397094D+00, &
    0.5399175203215238D+00,-.1403905262089725D+00, &
    0.4453396570037857D-01,-.4234360302104348D+00, &
    -.7214176376894034D+00,0.7278118716190342D+00, &
    0.4901890779701343D+00,-.8758887352507779D+00, &
    0.7632377205619759D-02,0.9317431147169187D+00 /
  data ys / &
    0.7998600119500696D-03,0.1723914595007140D+00, &
    -.1744716351651229D+00,-.2632941629071404D+00, &
    -.6295720376005641D-01,0.1678382818593359D+00, &
    -.7826941873249307D-01,-.1234686974783635D+00, &
    -.2169876766339741D+00,-.4629437300594242D-01, &
    -.1295630336164733D+00,-.2268108031307481D+00, &
    0.1330071443027294D+00,0.4919409115894537D+00, &
    0.3001995169122194D+00,0.1407736457687509D+00, &
    -.2259257575315161D+00,-.1083199795573463D+00, &
    -.3742304739030127D+00,-.1478062482085008D+00, &
    -.3449512373726749D+00,0.2883671141680468D-01, &
    0.2558125570853043D+00,0.4404368079041183D+00, &
    0.3998568605285963D+00,-.9956585162815586D-01, &
    -.1509747383909747D-01,-.3956518618817587D+00, &
    0.2256616013248351D+00,-.3899651023918449D+00, &
    0.8134998230879619D+00,-.4451036700294634D+00, &
    -.4535647479937944D+00,0.6650145460171435D+00, &
    -.4674624469973033D+00,0.7012653983759632D+00, &
    -.5107241847309127D+00,0.2263324997687613D+00, &
    -.2727155170636352D+00,-.2861627803070728D+00, &
    0.1856306523332632D+00,-.5566374678887420D+00, &
    0.1056056946669113D+01,-.5664247124705913D+00 /
  data zs / &
    0.1140750120480647D+01,0.7860696072793454D+00, &
    -.3991430912330994D+00,0.3536092960240441D+00, &
    0.8189404142301465D+00,0.4764962716361752D+00, &
    0.7628974295081231D+00,-.3880058323817796D+00, &
    0.3997553695274408D+00,0.1144656464995089D+00, &
    0.2509171968778639D+00,0.4174615578343315D+00, &
    -.2747778345118032D+00,0.3031929324753210D+00, &
    -.3442503524230918D+00,0.3970537434180833D+00, &
    -.3070893258601431D+00,-.1989904396903917D+00, &
    0.1526258958568332D-01,-.1542350189843949D-03, &
    0.4121026001314489D-01,0.3248913848279931D+00, &
    -.6957900515388780D-01,-.3340987564879216D-01, &
    -.1303527431545757D-01,-.8390322512212595D-01, &
    -.4501404031339341D-01,-.7451523522744341D-01, &
    -.3993939022009145D+00,-.1254210458205579D+00, &
    -.1415815795205319D+00,-.1858244299410800D+00, &
    -.3245512424809017D+00,-.3317121001508232D+00, &
    -.3168159505701121D+00,-.3372150239150091D+00, &
    -.3950013670511707D+00,-.3336444006035868D+00, &
    -.3519392970001664D+00,-.3226167582915808D+00, &
    -.2780042325707565D+00,-.3720577799104590D+00, &
    -.3972751008974220D+00,-.3821073616790632D+00 /
  data ws / &
    0.1694544493019982D-02,0.1462667227182682D-01, &
    0.9589935274093377D-02,0.9835015847019813D-02, &
    0.1710148279355958D-01,0.1516325692442914D-01, &
    0.1746691605511999D-01,0.1604974314640067D-01, &
    0.2185156320663785D-01,0.1242799062066869D-01, &
    0.2646409426919389D-01,0.3135597218974045D-01, &
    0.2736783716876112D-01,0.2345735439005822D-01, &
    0.3130572641601596D-01,0.3548953658742431D-01, &
    0.4036134913447077D-01,0.3792924852161072D-01, &
    0.2440963887102594D-01,0.5132903383025206D-01, &
    0.3177764520649842D-01,0.5417358749775593D-01, &
    0.5180566415546370D-01,0.2633068360731991D-01, &
    0.3893527990282211D-01,0.5027716272897634D-01, &
    0.2208377845212325D-01,0.1429278762793107D-01, &
    0.6697289387082959D-02,0.1648266825522313D-01, &
    0.1688462952130244D-01,0.2190464958161271D-01, &
    0.2281706804403004D-01,0.1956212468616595D-01, &
    0.1994889218942623D-01,0.1737944394368884D-01, &
    0.7302913219204132D-02,0.2128832734454982D-01, &
    0.1333720911018169D-01,0.1494192725418561D-01, &
    0.1152112823254964D-01,0.2472998490996246D-02, &
    0.2018756899942699D-02,0.1470016064285112D-02 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule09 ( n, x, w )

!*****************************************************************************80
!
!! RULE09 returns the rule of degree 9.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 57 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.2108052017103265D+00,0.1283729355850052D-02, &
    -.2918734175018473D-01,-.4357949297689266D+00, &
    -.1799341637870203D+00,0.6942222186204158D-01, &
    -.1005642991135230D+00,0.1904855696753355D+00, &
    0.3231238851996552D+00,0.6819987943340138D-01, &
    0.9708131623556143D-01,0.4246239788055203D+00, &
    -.3569664256203109D+00,-.2145752061141752D+00, &
    -.4818674305781609D-02,-.4314954104621454D+00, &
    -.1312453567417025D+00,-.4083558222913988D+00, &
    -.1245612605849638D+00,0.1136348780330888D+00, &
    0.3351668846328467D+00,0.2159589333207287D+00, &
    -.1228639493897767D+00,0.1380276243456064D+00, &
    0.3905319783823730D+00,-.4580867844324543D+00, &
    0.2310228564389770D+00,0.3554068884610396D+00, &
    -.6505241861040961D-01,-.1786506541288197D+00, &
    0.1265104521534247D-01,-.4573827823867556D+00, &
    -.5542731199877091D-01,-.6387630000068671D-01, &
    -.3413749732146179D+00,0.5966077890976830D+00, &
    0.6917314710518101D+00,0.1078283022003015D-01, &
    0.1833983579629060D+00,-.1566832100689738D+00, &
    -.7215475195156269D+00,0.5634418100399829D+00, &
    -.6038175317729408D+00,0.4424617155479634D+00, &
    0.5376954508544742D+00,0.5801674951413821D+00, &
    0.1418634443888370D+00,0.3802052805108170D-01, &
    -.6766546338276799D+00,-.1483779428740810D+00, &
    0.7588538984987838D+00,-.4009332704109223D+00, &
    0.4294022281773516D+00,-.4911820218387012D+00, &
    -.8230436696819400D+00,0.8307385272442451D+00, &
    0.2847756354434797D-02 /
  data ys / &
    -.8955776048075967D-01,0.6121197754911472D-03, &
    0.2122758579659528D+00,-.2839355480318341D+00, &
    -.9582556152029258D-01,-.1394666680707507D+00, &
    0.4836397528261835D+00,0.3248843951738821D+00, &
    -.3091097002134723D+00,-.1799541017656358D+00, &
    0.1346690341273634D+00,-.1974037451364399D+00, &
    0.5240665094885540D-01,0.2761387382865308D+00, &
    0.4840041246351735D+00,-.3972752286396523D+00, &
    0.7569076187744307D-01,-.1239859742013766D+00, &
    -.2344790300437711D+00,-.3305476987034425D-01, &
    0.1770622072827525D-01,0.2385246764606510D+00, &
    0.5313763678851683D-01,0.7344859267512056D-02, &
    -.2434286634053894D+00,-.2589556174266634D+00, &
    -.2578369917972710D+00,0.1458366018963618D+00, &
    -.3194064610603558D+00,-.8361295659204318D-01, &
    0.2789606323941811D+00,-.3620544381079852D+00, &
    0.4583494650640864D+00,-.4110512725485398D+00, &
    0.1453586652323175D+00,-.2817335623108552D+00, &
    -.4244951895917183D+00,0.7740686042096789D+00, &
    0.5096501737807261D+00,0.5871627777302516D+00, &
    -.4069288134308984D+00,-.8745287448038948D-01, &
    -.1605011166676197D+00,-.4658949840050826D+00, &
    -.4822919051612324D+00,-.2630487459758545D-01, &
    0.7093134008050596D+00,-.4981319573109797D+00, &
    -.1608102554139131D+00,0.7476578114817578D+00, &
    -.2863864956290275D+00,0.3184909341350718D+00, &
    0.3342451266830045D+00,-.5259906817121476D+00, &
    -.4752298758892802D+00,-.4894484556416516D+00, &
    0.9800293546928647D+00 /
  data zs / &
    0.7735882074261043D+00,0.1020940836140086D+01, &
    0.7799358723388078D+00,0.3644377643783332D+00, &
    0.7572044348058194D+00,0.7055487604983020D+00, &
    -.4071603611745789D+00,0.2784337835342537D+00, &
    0.3220003691351323D+00,-.3955135683176328D+00, &
    0.6632209417507691D+00,0.3671897521183273D+00, &
    -.3747481356463467D+00,0.2325932263186099D+00, &
    0.3487840306818015D+00,-.4035704541972099D+00, &
    0.5374900196303547D+00,0.2654318104765763D+00, &
    0.3698883029355781D+00,0.4369873036895401D+00, &
    0.1860393220233105D+00,-.3479911186004724D+00, &
    -.2731450419753432D+00,-.8193864872695196D-01, &
    -.2812924318728223D+00,-.2546313790752990D+00, &
    0.8678030145840356D-01,-.1428438498378993D+00, &
    -.2156622116049685D+00,0.9674903232992661D-01, &
    0.1583036737187768D+00,-.2371578880066309D-01, &
    -.2083640565350588D+00,-.1327703130967625D-01, &
    -.8664488303113425D-01,-.7748272430107629D-01, &
    -.9669501947122619D-02,-.6316745396431407D-01, &
    -.1041997605600258D+00,-.7558004547861685D-01, &
    -.7999199041063003D-01,-.3751069250486347D+00, &
    -.1299227031705554D+00,-.1892728310331580D+00, &
    -.3910612496171095D+00,-.1907765273821742D+00, &
    -.3477134529420335D+00,-.3474543531762018D+00, &
    -.3606479588180986D+00,-.3440499686491684D+00, &
    -.3474563411684222D+00,-.3276892436683218D+00, &
    -.3542380917718721D+00,-.3040019129455090D+00, &
    -.3440705957046394D+00,-.3298160634602457D+00, &
    -.3378012113084769D+00 /
  data ws / &
    0.5680408778529046D-02,0.6887854441113948D-02, &
    0.6899595536569939D-02,0.6990873936739353D-02, &
    0.1142122940842178D-01,0.1220808590880294D-01, &
    0.7651487209643779D-02,0.9540263940869291D-02, &
    0.9495638963926394D-02,0.1385248437188017D-01, &
    0.1446333231867884D-01,0.1128763818550772D-01, &
    0.1290073048331596D-01,0.1468008559737106D-01, &
    0.1526131467815127D-01,0.7960414659472547D-02, &
    0.2551792590874883D-01,0.1931170779707098D-01, &
    0.2561026230172263D-01,0.3169262013385435D-01, &
    0.2455872440888530D-01,0.2465571070153197D-01, &
    0.3419839251013865D-01,0.4066027350436995D-01, &
    0.3092126671144214D-01,0.2731106642703576D-01, &
    0.3711204543388613D-01,0.2490755526192437D-01, &
    0.3425568258765496D-01,0.4767735489615401D-01, &
    0.4389950894493362D-01,0.2234183537671572D-01, &
    0.3468016547781724D-01,0.1599733331684861D-01, &
    0.3216001820424372D-01,0.2173130208768448D-01, &
    0.4172519829863862D-02,0.1261067900439839D-01, &
    0.2073467359947484D-01,0.1259876220642243D-01, &
    0.6842300320404830D-02,0.1001228308171065D-01, &
    0.1328037050111305D-01,0.1493735138014892D-01, &
    0.6932861063756816D-02,0.6827617690151055D-02, &
    0.1270616397760185D-01,0.1288875403806118D-01, &
    0.8899252930026689D-02,0.8881169613715286D-02, &
    0.5288619835392034D-02,0.1284718318260967D-01, &
    0.6409598847098365D-02,0.8055508398015583D-02, &
    0.7788463427997768D-02,0.6545788868966830D-02, &
    0.5341431206058898D-02 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule10 ( n, x, w )

!*****************************************************************************80
!
!! RULE10 returns the rule of degree 10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 74 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.4135441926108436D+00,0.6920344574148628D-02, &
    -.4204645371849727D+00,0.1212827019050737D+00, &
    0.2550746585402268D+00,-.3763573604446380D+00, &
    -.4793578815415788D-03,-.9545817212686222D-01, &
    0.9593753000785579D-01,0.3126301401176562D+00, &
    -.6029042538437952D+00,0.2902741137267026D+00, &
    0.1793124962974703D+00,-.3341633308670676D+00, &
    0.1548508345696874D+00,0.4006666604011494D+00, &
    -.3875674230187136D+00,-.1309923738259358D-01, &
    -.1159749898125322D+00,-.6294076737424348D+00, &
    0.7453826635553922D+00,-.1465165887848432D+00, &
    -.4643141868299223D+00,0.6108307756147643D+00, &
    0.1526773226986329D+00,-.6092465369941313D+00, &
    0.4565692142952937D+00,-.2349836288652037D+00, &
    0.5557206177695652D+00,-.3207369889038079D+00, &
    0.2617190792164227D+00,-.2353754650695125D+00, &
    -.2634361414712093D-01,-.1518707842414898D+00, &
    -.2491539125596758D+00,0.4010246968008866D+00, &
    0.2108318943729519D+00,0.7968056612120857D-02, &
    -.2187999509850842D+00,-.6451845341149960D+00, &
    0.5779406907458116D+00,0.6724384336957467D-01, &
    -.1117027003111241D+00,-.2717471055583957D+00, &
    0.3834498058697393D+00,0.2353864631064914D+00, &
    0.1649339445101810D+00,-.4003204076167762D+00, &
    0.7813866474350551D+00,0.1310349087707534D-02, &
    -.7826969965232643D+00,-.1446486485942199D+00, &
    -.1795841794748884D-02,0.1464444903888344D+00, &
    0.5662897411515679D+00,0.1839447497576211D-02, &
    -.5681291886496517D+00,0.1959958820407995D-02, &
    -.3041697942355038D+00,0.3022098354145521D+00, &
    -.7138061627291874D-01,-.3075192230121023D+00, &
    0.3788998392856530D+00,-.3187848524903898D-01, &
    -.9081742825948123D+00,0.9400527678433584D+00, &
    0.9976676831438748D-01,-.8701982408315725D+00, &
    0.7704314725177221D+00,-.8207757778140335D-14, &
    -.3478417597234231D-12,0.4790952016774938D-12, &
    0.1374787712765062D-12,0.1420358457394610D-12 /
  data ys / &
    -.2467507765313092D+00,0.4815151646539115D+00, &
    -.2347643881227681D+00,-.3645574461360758D+00, &
    0.2873126239566789D+00,0.7724482217897545D-01, &
    0.1105023601492690D+00,-.5566631617759855D-01, &
    -.5483604397160680D-01,0.5156767709512212D+00, &
    0.1290725785546668D-01,-.5285840288063388D+00, &
    0.2823324600522004D+00,0.1412294698335291D-01, &
    -.2964554070359728D+00,0.2161993077800489D+00, &
    0.2388878524669672D+00,-.4550871602470489D+00, &
    0.7937355713187059D+00,-.4973050730407310D+00, &
    -.2964304982776649D+00,0.6207352334853357D+00, &
    -.4372547047061620D+00,-.1834805287791172D+00, &
    0.6153490109135903D+00,-.1754520654179484D+00, &
    -.4398969454955785D+00,-.5060230351463111D+00, &
    0.4950972550220373D-01,0.4565133096445042D+00, &
    0.1206845953990400D+00,0.1663130735568127D+00, &
    -.2869976689562073D+00,0.3753807951356096D+00, &
    -.3192143548135662D+00,-.5616644032198326D-01, &
    -.1309245702301312D+00,0.2480480615696827D+00, &
    -.1171234913398124D+00,-.2948509623067830D+00, &
    -.4113207155193597D+00,0.7061716778256595D+00, &
    0.3782781131850991D+00,-.2858764327334718D+00, &
    -.9240168045211911D-01,-.3263497523611192D+00, &
    0.3670255329377183D+00,-.4067578057659137D-01, &
    -.4526468520348789D+00,0.9030241128745099D+00, &
    -.4503772608395989D+00,0.8558659569093126D-01, &
    -.1680627021511816D+00,0.8247610646027727D-01, &
    -.3290715455089481D+00,0.6549570744945620D+00, &
    -.3258855289855604D+00,0.3500934424096372D+00, &
    -.1733493470760727D+00,-.1767440953334651D+00, &
    0.3963042303943790D+00,-.2599695422276034D+00, &
    -.1363346881672204D+00,0.1067074385094486D+01, &
    -.5611447706066169D+00,-.5059296144870963D+00, &
    0.9472180066426262D+00,-.3872084475068061D+00, &
    -.5600095591352807D+00,-.1466739111569232D-12, &
    0.2419854376387837D-13,-.1552488340937713D-12, &
    -.2306323605219641D-12,-.1706174368865759D-12 /
  data zs / &
    -.2246678712539802D-01,-.2246678712556187D-01, &
    -.2246678712549978D-01,-.3674985430672734D+00, &
    -.3674985430672704D+00,-.3674985430673924D+00, &
    0.9168884763080956D+00,0.9168884763077307D+00, &
    0.9168884763085783D+00,-.3553336558198409D+00, &
    -.3553336558198190D+00,-.3553336558198354D+00, &
    -.1983165808272536D+00,-.1983165808275630D+00, &
    -.1983165808272446D+00,-.1447858405450569D+00, &
    -.1447858405451866D+00,-.1447858405451966D+00, &
    -.3542393281108072D+00,-.3542393281107521D+00, &
    -.3542393281107588D+00,-.1251511927365907D+00, &
    -.1251511927364737D+00,-.1251511927364298D+00, &
    -.1387177797398434D+00,-.1387177797398834D+00, &
    -.1387177797396668D+00,-.3549137865746493D+00, &
    -.3549137865746083D+00,-.3549137865746373D+00, &
    0.8659130596210637D-01,0.8659130596171444D-01, &
    0.8659130596207916D-01,0.2597687530320102D+00, &
    0.2597687530321350D+00,0.2597687530322305D+00, &
    0.2630632081142404D+00,0.2630632081143893D+00, &
    0.2630632081144756D+00,-.3453833089184027D+00, &
    -.3453833089183551D+00,-.3453833089183527D+00, &
    -.2327889534298980D+00,-.2327889534297419D+00, &
    -.2327889534296802D+00,0.2415268120578684D+00, &
    0.2415268120576211D+00,0.2415268120576665D+00, &
    -.2133068003220094D+00,-.2133068003224423D+00, &
    -.2133068003227821D+00,0.6055816506735499D+00, &
    0.6055816506736809D+00,0.6055816506737702D+00, &
    0.1414131684170811D+00,0.1414131684165572D+00, &
    0.1414131684162387D+00,0.5593538346997974D+00, &
    0.5593538346994973D+00,0.5593538347003276D+00, &
    -.3961007879024273D+00,-.3961007879023746D+00, &
    -.3961007879023505D+00,-.3950741090480416D+00, &
    -.3950741090482457D+00,-.3950741090476022D+00, &
    -.3779492208572051D+00,-.3779492208572193D+00, &
    -.3779492208571862D+00,-.3464548867786856D-01, &
    0.1201009856985286D+01,0.4068123865912653D+00, &
    -.3341407786078385D+00,0.5699475205900819D+00 /
  data ws / &
    0.2007080596188233D-01,0.2007080596192337D-01, &
    0.2007080596192014D-01,0.1202850982705908D-01, &
    0.1202850982706234D-01,0.1202850982703673D-01, &
    0.6428246923028596D-02,0.6428246923032827D-02, &
    0.6428246923025550D-02,0.5961017737373873D-02, &
    0.5961017737376429D-02,0.5961017737354793D-02, &
    0.2466985253776778D-01,0.2466985253776116D-01, &
    0.2466985253775616D-01,0.1220343371669821D-01, &
    0.1220343371668563D-01,0.1220343371666686D-01, &
    0.6782282187245643D-02,0.6782282187250343D-02, &
    0.6782282187251558D-02,0.1276699476009991D-01, &
    0.1276699476009263D-01,0.1276699476009060D-01, &
    0.1330898468041284D-01,0.1330898468040929D-01, &
    0.1330898468040468D-01,0.8872828138103311D-02, &
    0.8872828138107657D-02,0.8872828138112581D-02, &
    0.2604852195750169D-01,0.2604852195750782D-01, &
    0.2604852195751094D-01,0.1091598279364878D-01, &
    0.1091598279363667D-01,0.1091598279364397D-01, &
    0.2727741583607240D-01,0.2727741583609616D-01, &
    0.2727741583610115D-01,0.1214067234472188D-01, &
    0.1214067234470734D-01,0.1214067234471387D-01, &
    0.2653047387010774D-01,0.2653047387011107D-01, &
    0.2653047387012045D-01,0.1147407678956960D-01, &
    0.1147407678960903D-01,0.1147407678957783D-01, &
    0.6941339438094368D-02,0.6941339438087489D-02, &
    0.6941339438089641D-02,0.1618088676127407D-01, &
    0.1618088676125095D-01,0.1618088676125473D-01, &
    0.9879744049167588D-02,0.9879744049174469D-02, &
    0.9879744049169482D-02,0.1106165676903961D-01, &
    0.1106165676903509D-01,0.1106165676904102D-01, &
    0.7973762810133187D-02,0.7973762810143904D-02, &
    0.7973762810145713D-02,0.7671732380845806D-03, &
    0.7671732380905287D-03,0.7671732380951896D-03, &
    0.1643453575966134D-02,0.1643453575957693D-02, &
    0.1643453575955291D-02,0.4522697832344839D-01, &
    0.3708344580879497D-03,0.1141401437229523D-01, &
    0.2710167289616961D-01,0.1108569325544364D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule11 ( n, x, w )

!*****************************************************************************80
!
!! RULE11 returns the rule of degree 11.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 95 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.9651177776899700D-01,-.7093619453993043D+00, &
    0.6128501676297941D+00,-.4527836424396549D-02, &
    -.4612438490926584D+00,0.4657716855189367D+00, &
    0.2438655531438615D-01,0.3220723301923039D+00, &
    -.3464588855081770D+00,-.1267500664585161D+00, &
    -.5368057828378728D+00,0.6635558492970384D+00, &
    -.9547972451775022D-01,-.2357077639614438D+00, &
    0.3311874884792527D+00,-.7908328086591814D-02, &
    -.7854922635641037D+00,0.7934005916498822D+00, &
    -.1494644432070149D+00,-.3155198764172914D+00, &
    0.4649843196230657D+00,0.1165276860785583D+00, &
    -.4038862538014747D+00,0.2873585677227137D+00, &
    -.2047755067090988D-02,-.9205200026937792D-01, &
    0.9409975533966558D-01,0.3468354574885807D+00, &
    0.1682368211989309D+00,-.5150722786890125D+00, &
    -.1144319727418180D+00,-.7020417246489494D+00, &
    0.8164736973908203D+00,-.3048702567593000D+00, &
    -.1613129802547456D+00,0.4661832370146523D+00, &
    0.2101640308259509D+00,0.3270815374994656D+00, &
    -.5372455683250684D+00,-.6404070599046534D-01, &
    -.5801912598472644D+00,0.6442319658370544D+00, &
    0.5470009647103604D-02,-.2517469475214085D+00, &
    0.2462769378777770D+00,0.1008461981380965D-01, &
    0.1731441764929287D+00,-.1832287963083161D+00, &
    0.1748982078445799D+00,-.3794913490849509D+00, &
    0.2045931412407059D+00,0.1065391638487668D-01, &
    -.3452291078932529D+00,0.3345751915058239D+00, &
    -.7083686826885625D-03,0.2101573677291210D+00, &
    -.2094489990466886D+00,-.2851273048890965D+00, &
    -.3267460563855006D+00,0.6118733612746845D+00, &
    -.2323209396920720D-02,-.2164769352136706D+00, &
    0.2188001446096009D+00,-.2051577904029304D-01, &
    -.5570838828147304D+00,0.5775996618543481D+00, &
    -.3026455196997997D-02,0.1457949533626576D+00, &
    -.1427684981666913D+00,-.1973538549140939D+00, &
    -.1582348302473636D+00,0.3555886851614904D+00, &
    0.1068857744362264D+00,-.4126086236419754D+00, &
    0.3057228492051779D+00,-.3003364082942412D+00, &
    0.3997857603256970D+00,-.9944935203203158D-01, &
    0.1221835246093986D-01,-.8799955089249785D+00, &
    0.8677771564639284D+00,0.1534568838650121D+00, &
    -.7253511736488428D+00,0.5718942897840599D+00, &
    0.2235456216473980D-02,-.5785135622840735D+00, &
    0.5762781060658508D+00,0.9436416592156352D-01, &
    0.3995572440244000D+00,-.4939214099453045D+00, &
    0.1578951817317316D-11,-.7234508899977211D-12, &
    -.1195010503052243D-12,0.1273193864088492D-12, &
    -.1398313045262104D-12 /
  data ys / &
    0.7633795193833395D+00,-.2981081083795290D+00, &
    -.4652714110037153D+00,0.5352126684505152D+00, &
    -.2715275555928822D+00,-.2636851128574337D+00, &
    -.3859766773459927D+00,0.2141077150870582D+00, &
    0.1718689622588539D+00,0.6930291114382893D+00, &
    -.4562833332036821D+00,-.2367457782343889D+00, &
    0.3272971265990525D+00,-.2463364302786462D+00, &
    -.8096069632058686D-01,0.9115742149797984D+00, &
    -.4626359205144959D+00,-.4489382944649009D+00, &
    0.4506243076880464D+00,-.3547521586231100D+00, &
    -.9587214906505584D-01,0.3990903837832518D+00, &
    -.9862925550286258D-01,-.3004611282798994D+00, &
    0.1074747662091034D+00,-.5551079101315480D-01, &
    -.5196397519770712D-01,-.3945086927598004D+00, &
    0.4976226634994518D+00,-.1031139707392109D+00, &
    0.8767152876835527D+00,-.5374586392410802D+00, &
    -.3392566484421098D+00,0.3622851099556234D+00, &
    -.4451679421897679D+00,0.8288283223322354D-01, &
    -.4990194872156122D+00,0.4315171332646398D+00, &
    0.6750235395125617D-01,0.7069210789517217D+00, &
    -.4089214177391241D+00,-.2979996612119608D+00, &
    0.2875342242963981D+00,-.1390299448345922D+00, &
    -.1485042794638735D+00,-.2057520317788134D+00, &
    0.1116095528368255D+00,0.9414247894240614D-01, &
    0.3372213377187678D+00,-.1714437779017452D-01, &
    -.3200769599289869D+00,0.3924851952555710D+00, &
    -.1870160353880812D+00,-.2054691598668353D+00, &
    -.2422598488114232D+00,0.1205164591315570D+00, &
    0.1217433896809788D+00,0.5419121734527502D+00, &
    -.5178835760728716D+00,-.2402859737978143D-01, &
    0.2513073392081394D+00,-.1276656279597885D+00, &
    -.1236417112471668D+00,0.6551098499602489D+00, &
    -.3453221108076170D+00,-.3097877391525884D+00, &
    -.1666021864185573D+00,0.8068010612598300D-01, &
    0.8592208029345208D-01,0.2966561449375769D+00, &
    -.3192415243590572D+00,0.2258537942164062D-01, &
    0.4147288692159332D+00,-.1147986386430089D+00, &
    -.2999302305730746D+00,-.2882335265091744D+00, &
    -.1159821960086673D+00,0.4042157225193962D+00, &
    0.1009077018844530D+01,-.4939571057987255D+00, &
    -.5151199130457724D+00,0.7489650175178515D+00, &
    -.2415849489462737D+00,-.5073800685715635D+00, &
    0.6667192805807411D+00,-.3314236784178127D+00, &
    -.3352956021620059D+00,-.5158501413847826D+00, &
    0.3396468355868865D+00,0.1762033057983631D+00, &
    -.8069526265224892D-12,0.4596851666550035D-12, &
    0.1156254386801148D-12,0.5765654131403360D-13, &
    -.1226198587930237D-12 /
  data zs / &
    -.1868268863122697D+00,-.1868268863123053D+00, &
    -.1868268863112705D+00,0.4194136456710284D+00, &
    0.4194136456717518D+00,0.4194136456711013D+00, &
    0.5030086583798776D-01,0.5030086583729960D-01, &
    0.5030086583707338D-01,-.1505956145194099D+00, &
    -.1505956145190243D+00,-.1505956145192429D+00, &
    -.1725275785423899D+00,-.1725275785429845D+00, &
    -.1725275785414913D+00,-.1709810615420194D+00, &
    -.1709810615421794D+00,-.1709810615408042D+00, &
    0.7674742839401762D-01,0.7674742839470597D-01, &
    0.7674742839681360D-01,-.3720134569923326D+00, &
    -.3720134569922893D+00,-.3720134569921418D+00, &
    0.9342960665019454D+00,0.9342960665021878D+00, &
    0.9342960664953203D+00,-.3611736289802844D-01, &
    -.3611736289860155D-01,-.3611736289902880D-01, &
    -.3535560367475518D+00,-.3535560367476202D+00, &
    -.3535560367476884D+00,-.1926114352197085D+00, &
    -.1926114352198034D+00,-.1926114352197086D+00, &
    -.2818523519641055D+00,-.2818523519643090D+00, &
    -.2818523519648298D+00,-.3888552761464975D+00, &
    -.3888552761464523D+00,-.3888552761454053D+00, &
    0.6654848063127317D+00,0.6654848063130780D+00, &
    0.6654848063078891D+00,0.1794683181875785D-01, &
    0.1794683181975163D-01,0.1794683181866871D-01, &
    -.2272794453431112D+00,-.2272794453432979D+00, &
    -.2272794453429204D+00,0.8078480103364902D-01, &
    0.8078480103319574D-01,0.8078480103608091D-01, &
    0.3406111942551650D+00,0.3406111942551089D+00, &
    0.3406111942537021D+00,-.3549131236009490D+00, &
    -.3549131236011042D+00,-.3549131236012491D+00, &
    0.4213057676587053D+00,0.4213057676584742D+00, &
    0.4213057676619541D+00,-.2430250873757710D+00, &
    -.2430250873755122D+00,-.2430250873736784D+00, &
    0.6939507005767215D+00,0.6939507005768448D+00, &
    0.6939507005757009D+00,-.3547895907184612D+00, &
    -.3547895907186004D+00,-.3547895907184947D+00, &
    0.3273634204692249D+00,0.3273634204691543D+00, &
    0.3273634204697847D+00,0.3797479097987934D+00, &
    0.3797479098019154D+00,0.3797479097984070D+00, &
    -.3626672020688875D+00,-.3626672020689380D+00, &
    -.3626672020686431D+00,-.3656623860600263D+00, &
    -.3656623860600038D+00,-.3656623860598407D+00, &
    0.1056953360682411D+00,0.1056953360682777D+00, &
    0.1056953360707847D+00,-.4015122352990265D+00, &
    -.4015122352990430D+00,-.4015122352995484D+00, &
    0.1118223609071020D+01,0.6664104583675564D+00, &
    -.3809990858052303D+00,-.2253945502682283D+00, &
    0.2604940999755755D+00 /
  data ws / &
    0.6188005592967251D-02,0.6188005592972490D-02, &
    0.6188005592984317D-02,0.1421063031043561D-02, &
    0.1421063031047919D-02,0.1421063030985486D-02, &
    0.9481646089825704D-02,0.9481646089870772D-02, &
    0.9481646089801222D-02,0.6814905118771046D-02, &
    0.6814905118757536D-02,0.6814905118753749D-02, &
    0.1912041674313553D-01,0.1912041674309802D-01, &
    0.1912041674324984D-01,0.3076029506690844D-02, &
    0.3076029506690773D-02,0.3076029506710983D-02, &
    0.1135836724916717D-01,0.1135836724921729D-01, &
    0.1135836724932070D-01,0.1091603321391938D-01, &
    0.1091603321391454D-01,0.1091603321395138D-01, &
    0.4213691001392970D-02,0.4213691001384827D-02, &
    0.4213691001487200D-02,0.1206837849134008D-01, &
    0.1206837849134312D-01,0.1206837849132945D-01, &
    0.2578861292324762D-02,0.2578861292345221D-02, &
    0.2578861292334968D-02,0.1252456101442731D-01, &
    0.1252456101442267D-01,0.1252456101444618D-01, &
    0.8063913155156468D-02,0.8063913155154395D-02, &
    0.8063913155182456D-02,0.4993280626450522D-02, &
    0.4993280626459499D-02,0.4993280626580781D-02, &
    0.6150188384809636D-02,0.6150188384813883D-02, &
    0.6150188384677927D-02,0.2678388417077401D-01, &
    0.2678388417077927D-01,0.2678388417078649D-01, &
    0.1946856127752551D-01,0.1946856127748248D-01, &
    0.1946856127749281D-01,0.2102094189170934D-01, &
    0.2102094189169974D-01,0.2102094189174428D-01, &
    0.1685367818736497D-01,0.1685367818729912D-01, &
    0.1685367818732997D-01,0.5900869705091857D-02, &
    0.5900869705093673D-02,0.5900869705073673D-02, &
    0.1690563324458130D-01,0.1690563324459602D-01, &
    0.1690563324456019D-01,0.1476341187397782D-01, &
    0.1476341187398414D-01,0.1476341187397479D-01, &
    0.6992794755775152D-02,0.6992794755747075D-02, &
    0.6992794755746150D-02,0.1513056742691520D-01, &
    0.1513056742688911D-01,0.1513056742692685D-01, &
    0.7015883109811540D-02,0.7015883109809210D-02, &
    0.7015883109746114D-02,0.4965174122051565D-02, &
    0.4965174121903291D-02,0.4965174122041231D-02, &
    0.2712699778890213D-02,0.2712699778885687D-02, &
    0.2712699778899985D-02,0.5900718944404856D-02, &
    0.5900718944405426D-02,0.5900718944424988D-02, &
    0.8768888887154097D-02,0.8768888887160882D-02, &
    0.8768888887196669D-02,0.3175432879836244D-02, &
    0.3175432879833069D-02,0.3175432879794895D-02, &
    0.1078986495318477D-02,0.1747404076054822D-01, &
    0.1129982873905144D-01,0.2890093028298477D-01, &
    0.2624431483486055D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule12 ( n, x, w )

!*****************************************************************************80
!
!! RULE12 returns the rule of degree 12.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 122 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.3958544098912666D+00,-.4870511331351648D+00, &
    0.9119672324389515D-01,-.5926431804946591D+00, &
    0.5193516147881150D+00,0.7329156570653904D-01, &
    -.4544141850245140D-01,-.2756993864806933D+00, &
    0.3211408049831440D+00,-.3413860630862161D+00, &
    0.5248742662396945D+00,-.1834882031534880D+00, &
    -.1319722431139201D+00,0.2771078804366659D+00, &
    -.1451356373227354D+00,0.2916719156941026D+00, &
    -.6416905801467393D+00,0.3500186644526196D+00, &
    -.4070439485536645D-01,-.4815006651936328D+00, &
    0.5222050600490048D+00,-.9820856549819251D-01, &
    -.4276075601779065D+00,0.5258161256761047D+00, &
    0.2184089033681613D+00,-.4656593625800790D+00, &
    0.2472504592119225D+00,-.6606777491144762D-02, &
    0.1271987824323769D+00,-.1205920049412379D+00, &
    -.1862346674942773D+00,-.1097662119993451D+00, &
    0.2960008794936306D+00,-.5890402754221185D+00, &
    0.6424505517737594D+00,-.5341027635165645D-01, &
    -.1196400210307819D-01,-.7808240524677493D+00, &
    0.7927880545708264D+00,0.6235239461686299D-01, &
    -.4408318818916021D+00,0.3784794872747496D+00, &
    0.2235903863093564D+00,-.5240787479715297D-01, &
    -.1711825115122079D+00,0.1666477050331823D+00, &
    -.2729955606600447D+00,0.1063478556268736D+00, &
    -.7415049139037595D-01,0.7389518464069887D-01, &
    0.2553067496801190D-03,0.1955908717228039D+00, &
    0.1141488778114551D+00,-.3097397495342445D+00, &
    -.1802680258591541D-01,-.2161777731993448D+00, &
    0.2342045757852541D+00,-.1532613811702923D+00, &
    -.1800985367097719D+00,0.3333599178800782D+00, &
    0.5776336665233524D-02,-.6762886678803212D+00, &
    0.6705123312150877D+00,-.8304590790913337D-01, &
    0.3554044991859694D+00,-.2723585912768360D+00, &
    0.5423845315967933D+00,-.2526088517557438D+00, &
    -.2897756798410528D+00,0.5573431967310101D+00, &
    -.3595768372350124D+00,-.1977663594959926D+00, &
    0.4731366238769863D+00,-.1975148047452081D-01, &
    -.4533851434024622D+00,0.3153219324427421D+00, &
    -.2494039878683618D-01,-.2903815336558955D+00, &
    -.1831653764008917D-01,-.9317223914837645D+00, &
    0.9500389291238570D+00,-.6699974553713883D+00, &
    0.5322783600952914D+00,0.1377190952760970D+00, &
    0.2782783282780965D+00,-.3217122219145910D+00, &
    0.4343389363650290D-01,0.9746770878915403D-01, &
    -.3733686186921615D+00,0.2759009099030095D+00, &
    0.1089081465931238D-01,-.8195375330783097D+00, &
    0.8086467184189974D+00,-.1251938886030335D+00, &
    0.3429675592810749D-01,0.9089713267491691D-01, &
    0.4868852723886817D+00,0.1198290818690802D+00, &
    -.6067143542577578D+00,0.3382540465835279D+00, &
    -.4603539675728300D+00,0.1220999209893009D+00, &
    -.9480210749913888D-01,-.6451285680033118D+00, &
    0.7399306755024498D+00,0.1140113067761596D+00, &
    -.8552704011446526D+00,0.7412590943684850D+00, &
    -.1658421801556803D+00,-.6245895743792733D+00, &
    0.7904317545349575D+00,0.2557413056493574D+00, &
    0.3270954418922731D-01,-.2884508498385872D+00, &
    -.1613157569571640D+00,-.4583565335140027D+00, &
    0.6196722904711670D+00,0.2156998987651826D-14, &
    0.2157685773910294D-14,0.9329761593328095D-14, &
    -.1937826095544789D-14,0.5112100598082816D-14 /
  data ys / &
    0.3338515555387722D+00,0.1758941973965540D+00, &
    -.5097457529353365D+00,-.2575328894119814D+00, &
    -.3844776049819921D+00,0.6420104943939733D+00, &
    0.3445858452048264D+00,-.2116463454095397D+00, &
    -.1329394997952951D+00,-.4089732623879643D+00, &
    -.9116237193661333D-01,0.5001356343245807D+00, &
    -.2437824086419522D+00,0.7599889189904093D-02, &
    0.2361825194520526D+00,0.5725635993272842D+00, &
    -.3368651110208424D-01,-.5388770882252032D+00, &
    0.5794897706560004D+00,-.3249959253184209D+00, &
    -.2544938453375776D+00,0.5504594216795844D+00, &
    -.3602808234304559D+00,-.1901785982491242D+00, &
    0.4115986775195391D+00,-.1665168003024589D-01, &
    -.3949469974892933D+00,-.1430620777928657D+00, &
    0.6580940175195596D-01,0.7725267604090996D-01, &
    0.2342697395017627D+00,-.2784188228662802D+00, &
    0.4414908336450658D-01,-.4017554364367290D+00, &
    -.3092461241493650D+00,0.7110015605860860D+00, &
    0.9085253735987711D+00,-.4646238165515825D+00, &
    -.4439015570471948D+00,0.4730296395382988D+00, &
    -.1825160620441602D+00,-.2905135774941487D+00, &
    -.6857456848032077D-01,0.2279222388260412D+00, &
    -.1593476703457152D+00,0.2190140235085516D+00, &
    0.3481413428683416D-01,-.2538281577953898D+00, &
    -.4251600332346974D-01,-.4295820758543226D-01, &
    0.8547421090889233D-01,-.2447322131044543D+00, &
    0.2917527702125314D+00,-.4702055710805701D-01, &
    0.2600283704245143D+00,-.1456258542006649D+00, &
    -.1144025162238410D+00,0.2964453769751457D+00, &
    -.2809509380001344D+00,-.1549443897500673D-01, &
    0.7775759193725890D+00,-.3837855053933897D+00, &
    -.3937904139792007D+00,-.3624391892660122D+00, &
    0.1092997287033503D+00,0.2531394605626634D+00, &
    -.2145827819997083D-01,0.4804479220825398D+00, &
    -.4589896438825594D+00,0.9342132288033096D-01, &
    0.4359627055553254D+00,-.5293840284356405D+00, &
    -.2503585120211285D+00,0.5349275917488283D+00, &
    -.2845690797277077D+00,-.1532525106706588D+00, &
    0.3497030592011267D+00,-.1964505485304861D+00, &
    0.1086435405003431D+01,-.5590802894074118D+00, &
    -.5273551155960269D+00,-.2277988977546228D+00, &
    -.4663353679452422D+00,0.6941342656998574D+00, &
    0.2108172081069621D+00,0.1355874975580061D+00, &
    -.3464047056649888D+00,0.3748559371110348D+00, &
    -.1030184566954502D+00,-.2718374804155939D+00, &
    0.9400326158922729D+00,-.4605845857832677D+00, &
    -.4794480301090108D+00,0.3267824275100505D-01, &
    -.1247602093042907D+00,0.9208196655328471D-01, &
    -.4194700484257879D+00,0.6313900388299887D+00, &
    -.2119199904042083D+00,0.3362799093518891D+00, &
    0.1247966425982771D+00,-.4610765519501666D+00, &
    0.7996643270816305D+00,-.4819331969673737D+00, &
    -.3177311301142533D+00,0.9217567340036824D+00, &
    -.3621416790150311D+00,-.5596150549886635D+00, &
    0.8169629451576943D+00,-.5521050136126617D+00, &
    -.2648579315450346D+00,-.1854220399449956D+00, &
    0.3141894874618389D+00,-.1287674475168422D+00, &
    0.6224002317220075D+00,-.4509036594166286D+00, &
    -.1714965723053837D+00,-.1747363709921250D-14, &
    -.7841339838758449D-14,-.1038742413216324D-13, &
    -.6434088705165255D-14,0.8511611593137641D-14 /
  data zs / &
    -.3905577451659811D+00,-.3905577451659810D+00, &
    -.3905577451659885D+00,-.4194761577597843D-02, &
    -.4194761577604291D-02,-.4194761577584145D-02, &
    0.1874027021856000D+00,0.1874027021856146D+00, &
    0.1874027021856214D+00,-.3071102013390232D+00, &
    -.3071102013390279D+00,-.3071102013390283D+00, &
    -.2701961597911837D-01,-.2701961597911512D-01, &
    -.2701961597910549D-01,-.3650223874563840D+00, &
    -.3650223874563843D+00,-.3650223874563830D+00, &
    -.8612392000622994D-01,-.8612392000621880D-01, &
    -.8612392000621745D-01,0.1070791221730147D+00, &
    0.1070791221730203D+00,0.1070791221730225D+00, &
    0.3230374822491166D-01,0.3230374822490626D-01, &
    0.3230374822491062D-01,0.3289827199375365D+00, &
    0.3289827199375268D+00,0.3289827199375392D+00, &
    -.2002324469987915D+00,-.2002324469988037D+00, &
    -.2002324469988010D+00,-.2771825970811163D+00, &
    -.2771825970811178D+00,-.2771825970811177D+00, &
    -.3794145952130761D+00,-.3794145952130777D+00, &
    -.3794145952130777D+00,0.2887592197883361D+00, &
    0.2887592197883261D+00,0.2887592197883259D+00, &
    0.7307953050563157D+00,0.7307953050563196D+00, &
    0.7307953050563138D+00,0.4124918246328610D+00, &
    0.4124918246328517D+00,0.4124918246328527D+00, &
    0.9839020121236216D+00,0.9839020121236166D+00, &
    0.9839020121236378D+00,0.6829751492607604D-01, &
    0.6829751492606395D-01,0.6829751492607447D-01, &
    0.5090267712108498D+00,0.5090267712108671D+00, &
    0.5090267712108663D+00,0.3851617855258526D+00, &
    0.3851617855258565D+00,0.3851617855258507D+00, &
    0.2348420776072121D-01,0.2348420776071207D-01, &
    0.2348420776071097D-01,0.7747281662419676D-01, &
    0.7747281662420324D-01,0.7747281662419568D-01, &
    -.1630338566084656D+00,-.1630338566084718D+00, &
    -.1630338566084695D+00,-.3584562395667113D+00, &
    -.3584562395667106D+00,-.3584562395667084D+00, &
    0.3698626219613211D+00,0.3698626219613297D+00, &
    0.3698626219613188D+00,-.3667298228875690D+00, &
    -.3667298228875713D+00,-.3667298228875721D+00, &
    -.3790081337166384D+00,-.3790081337166408D+00, &
    -.3790081337166418D+00,-.2197227060814419D+00, &
    -.2197227060814434D+00,-.2197227060814370D+00, &
    -.3654645869052863D+00,-.3654645869052863D+00, &
    -.3654645869052867D+00,-.2014726326233556D+00, &
    -.2014726326233535D+00,-.2014726326233503D+00, &
    -.2464405285751480D+00,-.2464405285751552D+00, &
    -.2464405285751559D+00,0.7375395719855852D+00, &
    0.7375395719855836D+00,0.7375395719855902D+00, &
    -.3689240261223760D+00,-.3689240261223751D+00, &
    -.3689240261223750D+00,-.2202508600260902D+00, &
    -.2202508600260935D+00,-.2202508600260907D+00, &
    -.1594720366091775D+00,-.1594720366091723D+00, &
    -.1594720366091740D+00,-.3758886073617869D+00, &
    -.3758886073617894D+00,-.3758886073617892D+00, &
    -.3542700621002714D+00,-.3542700621002700D+00, &
    -.3542700621002702D+00,0.6895589091912253D+00, &
    0.6895589091912366D+00,0.6895589091912278D+00, &
    -.4073916692470354D+00,-.4073916692470370D+00, &
    -.4073916692470400D+00,0.6482117053575507D+00, &
    0.1198603539538932D+01,-.4041509145471567D+00, &
    -.2710584986072719D+00,0.8757555073873696D-02 /
  data ws / &
    0.2025764222341204D-02,0.2025764222341283D-02, &
    0.2025764222340314D-02,0.6116300216663094D-02, &
    0.6116300216663310D-02,0.6116300216663541D-02, &
    0.1275819159212241D-01,0.1275819159212196D-01, &
    0.1275819159212162D-01,0.8334684897884583D-02, &
    0.8334684897884422D-02,0.8334684897884272D-02, &
    0.1507614954107500D-01,0.1507614954107359D-01, &
    0.1507614954107202D-01,0.2734637742973762D-02, &
    0.2734637742973644D-02,0.2734637742974016D-02, &
    0.1154303798541227D-01,0.1154303798541274D-01, &
    0.1154303798541255D-01,0.7163952410599667D-02, &
    0.7163952410599083D-02,0.7163952410598778D-02, &
    0.7580503447649259D-02,0.7580503447650207D-02, &
    0.7580503447650126D-02,0.1894249152240793D-01, &
    0.1894249152240805D-01,0.1894249152240727D-01, &
    0.1641784697591419D-01,0.1641784697591236D-01, &
    0.1641784697591317D-01,0.8436640400263927D-02, &
    0.8436640400264633D-02,0.8436640400264418D-02, &
    0.3036004079076158D-02,0.3036004079076059D-02, &
    0.3036004079076094D-02,0.7492415688372185D-02, &
    0.7492415688372261D-02,0.7492415688372197D-02, &
    0.3062456464426824D-02,0.3062456464427586D-02, &
    0.3062456464427105D-02,0.8856133214568079D-02, &
    0.8856133214567973D-02,0.8856133214568157D-02, &
    0.3071402699546415D-02,0.3071402699546453D-02, &
    0.3071402699546178D-02,0.1823835135198804D-01, &
    0.1823835135198757D-01,0.1823835135198796D-01, &
    0.1121658861163283D-01,0.1121658861163258D-01, &
    0.1121658861163293D-01,0.5177567930581361D-02, &
    0.5177567930581481D-02,0.5177567930580846D-02, &
    0.3242723020974106D-02,0.3242723020974469D-02, &
    0.3242723020974430D-02,0.1136422978208590D-01, &
    0.1136422978208548D-01,0.1136422978208555D-01, &
    0.8174247103577847D-02,0.8174247103578128D-02, &
    0.8174247103578329D-02,0.4083194002127334D-02, &
    0.4083194002127503D-02,0.4083194002128160D-02, &
    0.3128101586450114D-02,0.3128101586450456D-02, &
    0.3128101586450350D-02,0.1081664090579396D-01, &
    0.1081664090579364D-01,0.1081664090579339D-01, &
    0.4910680133298682D-03,0.4910680133297917D-03, &
    0.4910680133297780D-03,0.8074513061447679D-02, &
    0.8074513061447566D-02,0.8074513061448123D-02, &
    0.1112708329974781D-01,0.1112708329974803D-01, &
    0.1112708329974812D-01,0.2131526487678964D-01, &
    0.2131526487679003D-01,0.2131526487678986D-01, &
    0.3571026646377801D-02,0.3571026646377656D-02, &
    0.3571026646377794D-02,0.7768447076866043D-02, &
    0.7768447076865915D-02,0.7768447076866019D-02, &
    0.7618830679555172D-02,0.7618830679555604D-02, &
    0.7618830679555573D-02,0.1180399216908969D-01, &
    0.1180399216908947D-01,0.1180399216908933D-01, &
    0.2753708273597594D-02,0.2753708273597404D-02, &
    0.2753708273597407D-02,0.1252040626033346D-02, &
    0.1252040626033167D-02,0.1252040626033129D-02, &
    0.2035505637330654D-02,0.2035505637330660D-02, &
    0.2035505637330673D-02,0.2152733016776995D-02, &
    0.2152733016776493D-02,0.2152733016776350D-02, &
    0.2538969965943544D-02,0.2538969965943405D-02, &
    0.2538969965943241D-02,0.1078372873958245D-01, &
    0.1943908493037603D-03,0.4799528389705479D-02, &
    0.2477673544485754D-01,0.2864883777302190D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule13 ( n, x, w )

!*****************************************************************************80
!
!! RULE13 returns the rule of degree 13.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 146 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -.4777385017466611D-01,-.7568808830760128D+00, &
    0.8046547332507396D+00,0.3005907944967990D-01, &
    -.3472424886466524D+00,0.3171834091971876D+00, &
    -.3529824413110864D-01,-.4209487812102883D+00, &
    0.4562470253414500D+00,-.7153537080888878D+00, &
    0.5989558965388279D+00,0.1163978115501301D+00, &
    0.3710519456025519D-02,0.4652335033519287D+00, &
    -.4689440228078527D+00,-.3599160575353618D-01, &
    0.1939924024452817D+00,-.1580007966914678D+00, &
    -.2161537359848647D-01,0.2912599485905940D+00, &
    -.2696445749921000D+00,-.4913573074965884D+00, &
    0.2288888183023164D+00,0.2624684891944182D+00, &
    -.1426281098822192D+00,-.3118486240867259D+00, &
    0.4544767339690260D+00,0.7618754763810419D+00, &
    -.1422306180040716D+00,-.6196448583768703D+00, &
    0.5776897880027803D-01,0.1164689180950719D+00, &
    -.1742378968955057D+00,-.1414719142296262D+00, &
    -.3229372289650024D+00,0.4644091431945710D+00, &
    -.1398258170326137D+00,-.5098850522745304D+00, &
    0.6497108693071825D+00,0.3610318921371553D+00, &
    0.1054629929099188D+00,-.4664948850470391D+00, &
    0.2459013465212476D-01,-.7178146556427887D+00, &
    0.6932245209907225D+00,-.1065595141175732D-01, &
    -.9217121855780308D+00,0.9323681369897492D+00, &
    -.4115285528872360D+00,0.2668526805669106D+00, &
    0.1446758723202021D+00,0.1571190412397715D-02, &
    -.6922217370423006D-01,0.6765098329171684D-01, &
    -.2991189861629327D+00,-.1245621803681009D+00, &
    0.4236811665311025D+00,0.3871696430393148D+00, &
    0.1500785041234294D+00,-.5372481471625822D+00, &
    -.8064886300342536D-01,-.4844598247542774D+00, &
    0.5651086877577737D+00,0.6705603853993255D+00, &
    -.2613693382805031D+00,-.4091910471186840D+00, &
    -.2994914861160214D+00,-.2674832138785850D+00, &
    0.5669746999946459D+00,0.4510147973428894D-01, &
    0.2172069032075606D+00,-.2623083829418457D+00, &
    -.2495785492724049D+00,-.2085110500880744D+00, &
    0.4580895993605156D+00,0.2707504828489703D+00, &
    -.4821600751443009D+00,0.2114095922954477D+00, &
    -.4554280384514680D-01,-.7996627976605564D+00, &
    0.8452056015056444D+00,0.2029674430134095D-01, &
    -.7997175993984930D+00,0.7794208550971885D+00, &
    0.1441756236446537D+00,-.6915442312985614D+00, &
    0.5473686076540003D+00,0.3528170518753222D-01, &
    -.3311753761570134D+00,0.2958936709696454D+00, &
    -.7827835701762442D-02,0.2760083802741976D+00, &
    -.2681805445724308D+00,0.6806655606966093D-01, &
    -.5973241026022454D+00,0.5292575465325308D+00, &
    -.9685067665231929D-01,0.2259602937925122D+00, &
    -.1291096171402099D+00,-.1001708030503953D-01, &
    -.5630399697700922D+00,0.5730570500751420D+00, &
    0.3465920273834422D-01,-.1777529113458167D+00, &
    0.1430937086076311D+00,0.7285263734559304D-01, &
    -.8758889229618175D+00,0.8030362856162614D+00, &
    0.1343389663918027D+00,0.3445210697535890D+00, &
    -.4788600361453874D+00,-.1348321313217423D-01, &
    0.1154680832801965D+00,-.1019848701479677D+00, &
    -.5176848990301737D-01,-.2375948027597131D+00, &
    0.2893632926627010D+00,0.1729207472857360D-01, &
    -.4389324777852445D+00,0.4216404030566085D+00, &
    -.3914791775164566D-02,-.2309684344749786D+00, &
    0.2348832262501366D+00,-.6881144138718593D+00, &
    0.4266860230809687D+00,0.2614283907909357D+00, &
    0.4960973006288234D-02,-.2137441872471665D+00, &
    0.2087832142407756D+00,-.7325006065249269D-01, &
    -.2659488483143491D+00,0.3391989089668666D+00, &
    -.4678501489682662D-01,-.5410293892575722D+00, &
    0.5878144041543488D+00,-.2500405721723705D-01, &
    -.6313699376952513D+00,0.6563739949124212D+00, &
    0.1364199568193839D+00,-.2996193314702689D+00, &
    0.1631993746509763D+00,0.1198743702984181D-12, &
    -.7662979863046558D-13,-.4829662024390088D-14, &
    -.2111050829994401D-13,-.1270671025297558D-13 /
  data ys / &
    0.9015530084355994D+00,-.4921498721057091D+00, &
    -.4094031363297998D+00,0.3836064709764307D+00, &
    -.1657713090704851D+00,-.2178351619061508D+00, &
    0.5064492350445715D+00,-.2837937936488191D+00, &
    -.2226554413958301D+00,-.2786050402678961D+00, &
    -.4802119637624816D+00,0.7588170040302994D+00, &
    -.5393476461992264D+00,0.2728872272096964D+00, &
    0.2664604189895141D+00,-.2032233682745297D+00, &
    0.7044203923151297D-01,0.1327813290428652D+00, &
    -.3238383776801540D+00,0.1431997261914898D+00, &
    0.1806386514886541D+00,0.1938723202896774D-01, &
    -.4352215266416702D+00,0.4158342946127344D+00, &
    0.4424381517603146D+00,-.3447386423319839D+00, &
    -.9769950942847148D-01,-.2756352401942940D+00, &
    0.7976211371634238D+00,-.5219858969692130D+00, &
    -.1678396578899558D+00,0.1339492321368408D+00, &
    0.3389042575316226D-01,0.4545746399118207D+00, &
    -.3498055916007531D+00,-.1047690483110467D+00, &
    0.6694930174765074D+00,-.4558392183934259D+00, &
    -.2136537990830517D+00,-.3302200348036231D+00, &
    0.4777728075688429D+00,-.1475527727653846D+00, &
    0.8146638484662959D+00,-.3860362429420576D+00, &
    -.4286276055244945D+00,0.1070453773333782D+01, &
    -.5444552112909320D+00,-.5259985620427312D+00, &
    -.7053881312997473D-01,-.3211247746179037D+00, &
    0.3916635877479940D+00,0.7902375403647920D-01, &
    -.3815118620693258D-01,-.4087256782951131D-01, &
    0.3165284439137526D+00,-.4173088627282204D+00, &
    0.1007804188144937D+00,-.3968282271412536D+00, &
    0.5337128600166933D+00,-.1368846328756112D+00, &
    0.6059686632316962D+00,-.3728282957631877D+00, &
    -.2331403674686730D+00,-.8534490338988292D-01, &
    0.6233947802222222D+00,-.5380498768323672D+00, &
    0.4817745012020841D+00,-.5002544857946896D+00, &
    0.1847998459249884D-01,-.2768482795388788D+00, &
    0.1774831669675317D+00,0.9936511257127259D-01, &
    0.3848620644010382D+00,-.4085723961101285D+00, &
    0.2371033170906487D-01,0.4004326341980497D+00, &
    0.3426047913492075D-01,-.4346931133331124D+00, &
    0.9496652130401684D+00,-.5142738316095435D+00, &
    -.4353913814305450D+00,0.9117160117907287D+00, &
    -.4382805097163334D+00,-.4734355020744341D+00, &
    0.7152866610716716D+00,-.2327835778531099D+00, &
    -.4825030832186010D+00,0.3620384831590592D+00, &
    -.1504643885983839D+00,-.2115740945607743D+00, &
    -.3141876222502283D+00,0.1503147065506677D+00, &
    0.1638729156994478D+00,0.6504322183920584D+00, &
    -.2662687424915907D+00,-.3841634759004985D+00, &
    -.2049997086581880D+00,0.1862470797446676D-01, &
    0.1863750006837488D+00,0.6559259202331654D+00, &
    -.3366380061325622D+00,-.3192879141007369D+00, &
    0.1852408823986869D+00,-.6260469115301880D-01, &
    -.1226361912458057D+00,0.9693279211217602D+00, &
    -.4215717258868716D+00,-.5477561952349157D+00, &
    -.4753793031363919D+00,0.3540306091815932D+00, &
    0.1213486939547357D+00,-.1255465211978510D+00, &
    0.5109645550180968D-01,0.7445006569601835D-01, &
    0.3042393982437668D+00,-.1969525264934776D+00, &
    -.1072868717503699D+00,0.4968519844113532D+00, &
    -.2334506162065882D+00,-.2634013682047193D+00, &
    0.2689595817220580D+00,-.1378700999889159D+00, &
    -.1310894817332517D+00,-.9541153848828572D-01, &
    -.5482187938791565D+00,0.6436303323674273D+00, &
    0.2439463089891027D+00,-.1176768258436301D+00, &
    -.1262694831454210D+00,0.3493822205657847D+00, &
    -.2381275236366834D+00,-.1112546969291192D+00, &
    0.6517382679995124D+00,-.3663861454168269D+00, &
    -.2853521225825763D+00,0.7434793061384100D+00, &
    -.3933938018169897D+00,-.3500855043213671D+00, &
    0.2672085045651081D+00,-.1546110409379758D-01, &
    -.2517474004713509D+00,-.6195967642546056D-13, &
    0.3002370388193685D-13,-.5457794979132388D-13, &
    0.1164494042115649D-12,0.3123283355285955D-13 /
  data zs / &
    -.2327715323996341D+00,-.2327715323996078D+00, &
    -.2327715323995431D+00,0.3496762696550786D+00, &
    0.3496762696549099D+00,0.3496762696549561D+00, &
    0.2717988565409110D+00,0.2717988565409942D+00, &
    0.2717988565407555D+00,-.3874128688211647D+00, &
    -.3874128688212131D+00,-.3874128688212028D+00, &
    -.3783145937148669D+00,-.3783145937148664D+00, &
    -.3783145937148837D+00,-.3908054363428844D+00, &
    -.3908054363428517D+00,-.3908054363428521D+00, &
    0.2214621132658698D+00,0.2214621132658886D+00, &
    0.2214621132658825D+00,-.5332156026748039D-01, &
    -.5332156026744841D-01,-.5332156026745260D-01, &
    -.1366126720097484D+00,-.1366126720097481D+00, &
    -.1366126720097814D+00,-.3405718815923187D+00, &
    -.3405718815923307D+00,-.3405718815923774D+00, &
    0.2482158558753812D+00,0.2482158558753821D+00, &
    0.2482158558753401D+00,0.1493078550521198D+00, &
    0.1493078550521607D+00,0.1493078550521824D+00, &
    -.1246861001425667D+00,-.1246861001425311D+00, &
    -.1246861001424509D+00,-.8804090671666169D-01, &
    -.8804090671661184D-01,-.8804090671646460D-01, &
    -.2903904416529835D+00,-.2903904416529482D+00, &
    -.2903904416530703D+00,-.3588716811515187D+00, &
    -.3588716811515073D+00,-.3588716811515079D+00, &
    0.1748649644824907D+00,0.1748649644825259D+00, &
    0.1748649644824762D+00,0.1005626022480115D+01, &
    0.1005626022480066D+01,0.1005626022480308D+01, &
    -.6085363845277859D-01,-.6085363845275459D-01, &
    -.6085363845275343D-01,-.2907416212662024D+00, &
    -.2907416212662147D+00,-.2907416212661059D+00, &
    -.2928675049685535D+00,-.2928675049684912D+00, &
    -.2928675049686165D+00,-.3986190384060752D+00, &
    -.3986190384060841D+00,-.3986190384061074D+00, &
    -.2763005028815692D+00,-.2763005028815834D+00, &
    -.2763005028815310D+00,-.3171132373617958D-01, &
    -.3171132373613690D-01,-.3171132373618723D-01, &
    -.3670072924650872D+00,-.3670072924650821D+00, &
    -.3670072924650642D+00,-.3918966809387769D+00, &
    -.3918966809387540D+00,-.3918966809387686D+00, &
    -.3945112928982144D+00,-.3945112928982283D+00, &
    -.3945112928982709D+00,-.1689813827450514D+00, &
    -.1689813827450869D+00,-.1689813827450585D+00, &
    -.1988456360856388D+00,-.1988456360856537D+00, &
    -.1988456360856610D+00,-.3581788966687771D+00, &
    -.3581788966687613D+00,-.3581788966687278D+00, &
    -.2652570624981906D+00,-.2652570624981415D+00, &
    -.2652570624981432D+00,0.9127655651197962D-01, &
    0.9127655651192465D-01,0.9127655651200027D-01, &
    0.4712027028361149D+00,0.4712027028361768D+00, &
    0.4712027028361248D+00,-.4789101434801453D-01, &
    -.4789101434802212D-01,-.4789101434808656D-01, &
    0.5562071598537035D+00,0.5562071598536467D+00, &
    0.5562071598533351D+00,-.3666902411131864D+00, &
    -.3666902411132085D+00,-.3666902411132190D+00, &
    -.2500081968302335D+00,-.2500081968302558D+00, &
    -.2500081968302762D+00,0.7953910781041313D+00, &
    0.7953910781041719D+00,0.7953910781041406D+00, &
    0.1246295654192337D+00,0.1246295654192380D+00, &
    0.1246295654192949D+00,0.4430780439124138D+00, &
    0.4430780439124452D+00,0.4430780439125641D+00, &
    -.1789190687389412D+00,-.1789190687388659D+00, &
    -.1789190687388622D+00,-.3621753277358900D+00, &
    -.3621753277358689D+00,-.3621753277358712D+00, &
    0.7590201625172059D+00,0.7590201625171641D+00, &
    0.7590201625173268D+00,0.5385097619725802D+00, &
    0.5385097619725214D+00,0.5385097619724014D+00, &
    -.4034889863680245D+00,-.4034889863679839D+00, &
    -.4034889863680722D+00,0.9129480050841532D-01, &
    0.9129480050844316D-01,0.9129480050850451D-01, &
    0.5076476008019328D+00,0.5076476008020036D+00, &
    0.5076476008019484D+00,0.7587686360919684D+00, &
    0.1174064403165666D+01,-.2917588131252908D+00, &
    0.4072850512806022D+00,-.2737975651676979D-02 /
  data ws / &
    0.2068909148962774D-02,0.2068909148960302D-02, &
    0.2068909148962221D-02,0.6862179883351744D-02, &
    0.6862179883357749D-02,0.6862179883341077D-02, &
    0.4745055930506758D-02,0.4745055930499521D-02, &
    0.4745055930498638D-02,0.2639271833496386D-02, &
    0.2639271833492975D-02,0.2639271833493415D-02, &
    0.2420284628312029D-02,0.2420284628310808D-02, &
    0.2420284628311964D-02,0.5428029730193044D-02, &
    0.5428029730198515D-02,0.5428029730197811D-02, &
    0.7363269664287002D-02,0.7363269664287796D-02, &
    0.7363269664288242D-02,0.4622589345403796D-02, &
    0.4622589345400746D-02,0.4622589345402929D-02, &
    0.1113014179512893D-01,0.1113014179512515D-01, &
    0.1113014179513268D-01,0.3019898782414975D-02, &
    0.3019898782415646D-02,0.3019898782415196D-02, &
    0.1497918593186746D-01,0.1497918593186317D-01, &
    0.1497918593186622D-01,0.6570279132551560D-02, &
    0.6570279132554112D-02,0.6570279132553927D-02, &
    0.4504033084972921D-02,0.4504033084968776D-02, &
    0.4504033084972839D-02,0.1239429415995445D-01, &
    0.1239429415996196D-01,0.1239429415995679D-01, &
    0.5367281142254301D-02,0.5367281142251953D-02, &
    0.5367281142250357D-02,0.6316169904420855D-03, &
    0.6316169904431759D-03,0.6316169904440820D-03, &
    0.8876101030265546D-02,0.8876101030282007D-02, &
    0.8876101030274161D-02,0.2162421606464688D-02, &
    0.2162421606465037D-02,0.2162421606462924D-02, &
    0.8054280667477870D-02,0.8054280667477506D-02, &
    0.8054280667478849D-02,0.9049885628396499D-02, &
    0.9049885628398672D-02,0.9049885628405025D-02, &
    0.9196546074381425D-02,0.9196546074380063D-02, &
    0.9196546074381355D-02,0.1389679560298547D-02, &
    0.1389679560298251D-02,0.1389679560296835D-02, &
    0.5469361348986402D-02,0.5469361348984248D-02, &
    0.5469361348984599D-02,0.1725450508839409D-01, &
    0.1725450508839278D-01,0.1725450508839148D-01, &
    0.6733913747887457D-02,0.6733913747885728D-02, &
    0.6733913747886398D-02,0.4022564578398640D-02, &
    0.4022564578402713D-02,0.4022564578399363D-02, &
    0.1301944787144861D-02,0.1301944787144253D-02, &
    0.1301944787143136D-02,0.2027792916616635D-02, &
    0.2027792916617886D-02,0.2027792916616735D-02, &
    0.4379548102164245D-02,0.4379548102164029D-02, &
    0.4379548102160293D-02,0.9788773323829804D-02, &
    0.9788773323830982D-02,0.9788773323832441D-02, &
    0.1491498412616913D-01,0.1491498412617151D-01, &
    0.1491498412617137D-01,0.3816494659634458D-02, &
    0.3816494659634902D-02,0.3816494659631925D-02, &
    0.1053753256419563D-01,0.1053753256419800D-01, &
    0.1053753256419685D-01,0.9378412737876001D-02, &
    0.9378412737873921D-02,0.9378412737876034D-02, &
    0.1034943926448387D-01,0.1034943926449078D-01, &
    0.1034943926449734D-01,0.1253515438165336D-02, &
    0.1253515438163976D-02,0.1253515438162651D-02, &
    0.7631115735488028D-02,0.7631115735488942D-02, &
    0.7631115735487678D-02,0.4662247664665583D-02, &
    0.4662247664663175D-02,0.4662247664669866D-02, &
    0.1924508745358327D-01,0.1924508745357723D-01, &
    0.1924508745358518D-01,0.2150599791391388D-02, &
    0.2150599791389261D-02,0.2150599791389503D-02, &
    0.1815794181007045D-01,0.1815794181007355D-01, &
    0.1815794181007060D-01,0.2074509937828351D-02, &
    0.2074509937826631D-02,0.2074509937827053D-02, &
    0.3856301126724407D-02,0.3856301126725213D-02, &
    0.3856301126727490D-02,0.2342776832115179D-02, &
    0.2342776832117610D-02,0.2342776832119853D-02, &
    0.2485903899501151D-02,0.2485903899504086D-02, &
    0.2485903899497665D-02,0.1958269886432824D-02, &
    0.1958269886433108D-02,0.1958269886434393D-02, &
    0.3112037442333638D-02,0.3112037442336383D-02, &
    0.3112037442329971D-02,0.8380938111968996D-02, &
    0.2408616607470909D-03,0.1651223797192308D-01, &
    0.1363274166717042D-01,0.2507433395640973D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule14 ( n, x, w )

!*****************************************************************************80
!
!! RULE14 returns the rule of degree 14.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 177 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -.5752422835841802D+00,0.4314967658691802D+00, &
    0.1437455177139820D+00,-.3303076984155172D+00, &
    0.4866309056595519D+00,-.1563232072429510D+00, &
    -.5770168798465761D-02,-.7016819527781105D+00, &
    0.7074521215770434D+00,-.3032416710412216D+00, &
    -.2339888323701999D+00,0.5372305034104569D+00, &
    0.2995840525991514D+00,-.4325354814520043D+00, &
    0.1329514288534004D+00,0.2313978508942549D+00, &
    -.4060579401191659D-01,-.1907920568806182D+00, &
    0.1198459969693036D+00,-.5485013403177168D+00, &
    0.4286553433493205D+00,-.4223067018453286D+00, &
    0.2051376117542425D+00,0.2171690900904567D+00, &
    -.1995552059487330D+00,-.3336302067365579D+00, &
    0.5331854126840413D+00,-.3362367775957516D-02, &
    -.2082502914064486D+00,0.2116126591830406D+00, &
    0.3901789196843061D+00,-.4123770748876975D-03, &
    -.3897665426036371D+00,0.5440475521812093D-02, &
    -.5671976659757272D+00,0.5617571904593656D+00, &
    0.5109903009675820D-01,-.8812309166767075D+00, &
    0.8301318865798517D+00,-.3334033396451877D+00, &
    -.4849473999181729D-01,0.3818980796337723D+00, &
    0.2967209217147687D+00,0.6320475373066934D-02, &
    -.3030413970906470D+00,0.3947081540530285D+00, &
    -.2978075731654519D+00,-.9690058088341083D-01, &
    0.4602277425410929D+00,-.2588695285519879D+00, &
    -.2013582139898412D+00,0.4320204013442107D+00, &
    0.2158666376867163D+00,-.6478870390304328D+00, &
    0.3243502588043465D-02,-.8153377567221353D+00, &
    0.8120942541315551D+00,-.2929639748058975D+00, &
    0.3937181324850053D-01,0.2535921615554882D+00, &
    -.2911712364207900D+00,0.4448737339859005D+00, &
    -.1537024975672874D+00,0.3770577628402459D+00, &
    -.4566844608555435D+00,0.7962669801609278D-01, &
    -.7343568438702067D-03,-.6012198115212343D-01, &
    0.6085633799849959D-01,-.7860612444751536D+00, &
    0.7072341550237046D+00,0.7882708945288715D-01, &
    -.1798079430257987D+00,-.5716591672723068D+00, &
    0.7514671102985679D+00,0.2349121749284232D-01, &
    -.2977965988756665D+00,0.2743053813811669D+00, &
    -.3508185900315901D+00,0.3561089208016359D+00, &
    -.5290330768361618D-02,-.4799997070573949D+00, &
    0.3284477153105494D+00,0.1515519917450605D+00, &
    0.5503749362395663D+00,-.1236979845612246D+00, &
    -.4266769516797990D+00,-.1160913996018244D+00, &
    0.1218716230766462D+00,-.5780223478460763D-02, &
    -.1795966192456130D+00,-.5071498846594161D+00, &
    0.6867465039037227D+00,-.4731311596169461D-02, &
    -.6889415863849245D+00,0.6936728979813912D+00, &
    0.1838019755826437D+00,-.7414338401434005D+00, &
    0.5576318645605018D+00,-.3157482778904093D+00, &
    0.1186260398816200D-01,0.3038856739015107D+00, &
    -.3760156844932397D+00,0.2533165859904101D+00, &
    0.1226990985037814D+00,0.3132847887437243D-01, &
    0.1567813343830147D+00,-.1881098132555542D+00, &
    -.5940398288585815D+00,0.6767061397330466D+00, &
    -.8266631087504343D-01,0.5799063749491890D+00, &
    0.7755651500224905D-01,-.6574628899522570D+00, &
    0.1959288531923632D+00,0.8941019681549953D-01, &
    -.2853390500081148D+00,0.2938476934905306D+00, &
    0.2019773488401272D-02,-.2958674669799366D+00, &
    0.3655020525321305D+00,-.5615458414677339D+00, &
    0.1960437889355228D+00,-.5706595245720943D-01, &
    -.7715171223939032D+00,0.8285830748522059D+00, &
    0.1011528057210596D+00,-.1051837271533038D+00, &
    0.4030921431232027D-02,-.2124197258439737D+00, &
    0.3605072524272380D+00,-.1480875265845512D+00, &
    -.1651782199629099D+00,0.7878215733569789D-02, &
    0.1573000042284481D+00,-.3617927525610758D+00, &
    -.2009201308789826D+00,0.5627128834404685D+00, &
    -.5015820600281335D+00,0.4144850071548899D+00, &
    0.8709705287395679D-01,0.9434068372117834D+00, &
    0.4463509082121593D-02,-.9478703462960307D+00, &
    0.2005751720829750D+00,-.1945620110873523D+00, &
    -.6013160997523850D-02,-.1255658338458569D-01, &
    -.4674319437690188D+00,0.4799885271519170D+00, &
    -.1716908737168969D-01,0.1956591346765856D+00, &
    -.1784900473056204D+00,-.5462757915789873D-03, &
    -.5780834828017836D+00,0.5786297585932533D+00, &
    -.1833877013210589D+00,0.2818210581349795D+00, &
    -.9843335681437262D-01,0.1916617895075464D+00, &
    -.7885465203364792D-03,-.1908732429835174D+00, &
    -.4813891231174998D-01,-.8618855209039119D+00, &
    0.9100244332179739D+00,-.4424462871708599D+00, &
    0.5232153544540827D+00,-.8076906728405848D-01, &
    0.2330164669103972D+00,-.5137198179676699D+00, &
    0.2807033510564895D+00,0.3967080654003286D-12, &
    0.1722425458471176D-11,0.3502410373140708D-12, &
    -.2947942461866533D-11,-.1032091026644403D-11, &
    -.4359393565820349D-12 /
  data ys / &
    -.1661332605832194D+00,-.4151078006247312D+00, &
    0.5812410612053462D+00,-.3712097301616572D+00, &
    -.1004499928153783D+00,0.4716597229753685D+00, &
    0.8135639371543177D+00,-.4117790813394923D+00, &
    -.4017848558132318D+00,0.4452636911188574D+00, &
    -.4852468361674822D+00,0.3998314505168596D-01, &
    0.3264840198869604D+00,0.9620539017564665D-01, &
    -.4226894100630985D+00,-.8671007929617915D-01, &
    0.2437514569045547D+00,-.1570413776087273D+00, &
    0.5641616743542007D+00,-.1782911592597502D+00, &
    -.3858705150939825D+00,0.6946377256844247D-02, &
    -.3692015206135402D+00,0.3622551433574067D+00, &
    0.5004562312099742D+00,-.4230479934143149D+00, &
    -.7740823779440852D-01,0.2424079875448238D+00, &
    -.1241158896833233D+00,-.1182920978621629D+00, &
    -.2247937322813834D+00,0.4503017226062891D+00, &
    -.2255079903239726D+00,0.6518023902669200D+00, &
    -.3211896051207328D+00,-.3306127851443094D+00, &
    0.9880557751410730D+00,-.4497748293987996D+00, &
    -.5382809457428603D+00,0.2484874102685690D+00, &
    -.4129794669742624D+00,0.1644920567082871D+00, &
    -.1786101603431211D+00,0.3462729362116060D+00, &
    -.1676627758691386D+00,0.1159937060740129D+00, &
    0.2838304354535469D+00,-.3998241415280471D+00, &
    0.3320417294495800D-01,0.3819668300942372D+00, &
    -.4151710030395980D+00,-.4986884177668357D+00, &
    0.6234848514007479D+00,-.1247964336323278D+00, &
    0.9395983095546842D+00,-.4669901991392865D+00, &
    -.4726081104148907D+00,0.1236801757609007D+00, &
    -.3155543324553964D+00,0.1918741566946638D+00, &
    -.3455881484181401D+00,-.7936761338079117D-01, &
    0.4249557617992578D+00,0.3096393919455638D+00, &
    0.1717219053442890D+00,-.4813612972863747D+00, &
    0.6984686512807867D-01,-.3555940424550629D-01, &
    -.3428746088226146D-01,-.3628109884665136D+00, &
    -.4993435124130320D+00,0.8621545008801837D+00, &
    0.7639073125277335D+00,-.5376719027263360D+00, &
    -.2262354098013041D+00,0.3303032323049931D+00, &
    -.1448076250378262D+00,-.1854956072669932D+00, &
    -.2086539551781068D+00,-.1994908334986071D+00, &
    0.4081447886767858D+00,-.1021307936198597D+00, &
    -.3646265433107137D+00,0.4667573369306311D+00, &
    -.1749249882226239D+00,0.5641011705032603D+00, &
    -.3891761822782046D+00,-.7369982797233314D-01, &
    -.6368818722810367D-01,0.1373880152000548D+00, &
    0.6892964013212863D+00,-.5001834353611165D+00, &
    -.1891129659598641D+00,0.7982528447344757D+00, &
    -.4032238584029919D+00,-.3950289863308680D+00, &
    0.7500159343054114D+00,-.2158307870331475D+00, &
    -.5341851472730395D+00,0.1685995980230182D+00, &
    -.3577458288655779D+00,0.1891462308433379D+00, &
    -.7541204156110640D-01,-.2879331142125014D+00, &
    0.3633451557721906D+00,-.1991229969303482D+00, &
    0.1266927570309605D+00,0.7243023989959384D-01, &
    -.4384238887740483D+00,-.2952416382641486D+00, &
    0.7336655270377042D+00,-.4243636513101600D+00, &
    0.7143954781786902D+00,-.2900318268675506D+00, &
    -.2163615785319861D+00,0.2778601534641544D+00, &
    -.6149857493356037D-01,-.1719852784726787D+00, &
    0.3404722066425572D+00,-.1684869281697858D+00, &
    0.4373945770482864D+00,0.9783677410376379D-01, &
    -.5352313511523523D+00,0.9238182796099402D+00, &
    -.5113297043238586D+00,-.4124885752868345D+00, &
    0.6305510675913090D-01,0.5607334603928896D-01, &
    -.1191284527989022D+00,-.2936373325714834D+00, &
    -.3714221255853142D-01,0.3307795451305264D+00, &
    0.8626870981050186D-01,-.1861828895445977D+00, &
    0.9991417973422896D-01,0.4408837263795412D+00, &
    -.5337635778128931D+00,0.9287985143315733D-01, &
    -.1890175235323525D+00,-.3398740442993634D+00, &
    0.5288915678318241D+00,-.5498302077599997D+00, &
    0.1091929391010154D+01,-.5420991832506766D+00, &
    0.1088587293550235D+00,0.1192738297139006D+00, &
    -.2281325590696736D+00,0.5469934639217403D+00, &
    -.2843710521572813D+00,-.2626224117648735D+00, &
    -.2160151309343958D+00,0.9313869964408443D-01, &
    0.1228764312899015D+00,0.6678287012945778D+00, &
    -.3343874393606580D+00,-.3334412619346900D+00, &
    -.2195399888316988D+00,-.4904841366906035D-01, &
    0.2685884025019004D+00,-.1097454506740659D+00, &
    0.2208567039838767D+00,-.1111112533091888D+00, &
    0.1023012688991847D+01,-.5531958654682623D+00, &
    -.4698168235239601D+00,-.3487105684781340D+00, &
    -.2088144402600705D+00,0.5575250087387288D+00, &
    0.4586604304883936D+00,-.2753203539748657D-01, &
    -.4311283950878784D+00,0.1424849530503414D-12, &
    0.4163207520226179D-12,0.2564407634679153D-12, &
    -.9489253825677672D-12,0.5725707601904812D-12, &
    -.4307858177802721D-12 /
  data zs / &
    -.3002408419822953D+00,-.3002408419833086D+00, &
    -.3002408419833821D+00,-.2367243537500485D+00, &
    -.2367243537456669D+00,-.2367243537470976D+00, &
    -.1124874139319764D-01,-.1124874139141120D-01, &
    -.1124874139183386D-01,-.2091200491939335D+00, &
    -.2091200491951212D+00,-.2091200491943568D+00, &
    -.3871070169557922D+00,-.3871070169557220D+00, &
    -.3871070169561885D+00,0.1576758205577684D+00, &
    0.1576758205557184D+00,0.1576758205553296D+00, &
    -.3804994466636671D+00,-.3804994466631279D+00, &
    -.3804994466634047D+00,-.2566785126781401D+00, &
    -.2566785126792934D+00,-.2566785126782982D+00, &
    -.7550445623527601D-02,-.7550445625559176D-02, &
    -.7550445625114355D-02,0.3966685271521035D+00, &
    0.3966685271469443D+00,0.3966685271536675D+00, &
    0.5436295368746583D+00,0.5436295368772041D+00, &
    0.5436295368824269D+00,0.2108533972691848D+00, &
    0.2108533972755834D+00,0.2108533972664533D+00, &
    -.3846227632566736D+00,-.3846227632563715D+00, &
    -.3846227632570768D+00,0.4320199156927147D-01, &
    0.4320199156864359D-01,0.4320199157222126D-01, &
    -.3942552261036596D+00,-.3942552261036111D+00, &
    -.3942552261035123D+00,-.2900823721656310D+00, &
    -.2900823721654074D+00,-.2900823721650085D+00, &
    -.4001954700908103D+00,-.4001954700902223D+00, &
    -.4001954700893645D+00,-.2514599966999538D+00, &
    -.2514599967001752D+00,-.2514599967005651D+00, &
    -.2052993465249218D+00,-.2052993465263826D+00, &
    -.2052993465223245D+00,0.2946594740406009D+00, &
    0.2946594740416467D+00,0.2946594740425612D+00, &
    -.5795226862357377D-01,-.5795226862121653D-01, &
    -.5795226862189089D-01,-.2212157144490026D+00, &
    -.2212157144492802D+00,-.2212157144486162D+00, &
    0.1031915041617615D+01,0.1031915041621368D+01, &
    0.1031915041615854D+01,-.2843664701680810D+00, &
    -.2843664701691355D+00,-.2843664701684534D+00, &
    -.3916632709551901D+00,-.3916632709550742D+00, &
    -.3916632709550005D+00,-.5853404503565347D-01, &
    -.5853404503336719D-01,-.5853404503471410D-01, &
    0.4349032593314631D+00,0.4349032593318472D+00, &
    0.4349032593321483D+00,-.7166469320204438D-01, &
    -.7166469320319802D-01,-.7166469320166638D-01, &
    -.3651020078507813D+00,-.3651020078509204D+00, &
    -.3651020078510956D+00,0.6666741608359100D+00, &
    0.6666741608427517D+00,0.6666741608415513D+00, &
    -.2850607980246553D+00,-.2850607980246767D+00, &
    -.2850607980239678D+00,-.3767139049041499D+00, &
    -.3767139049041446D+00,-.3767139049041464D+00, &
    -.3847323113293710D+00,-.3847323113294659D+00, &
    -.3847323113294736D+00,-.1430711791014896D-01, &
    -.1430711790970269D-01,-.1430711791041984D-01, &
    0.2226893434846138D+00,0.2226893434844011D+00, &
    0.2226893434848597D+00,0.1861368718435910D+00, &
    0.1861368718416253D+00,0.1861368718431125D+00, &
    -.1072795622749953D+00,-.1072795622745489D+00, &
    -.1072795622748554D+00,-.6485516948547954D-01, &
    -.6485516948561629D-01,-.6485516948569679D-01, &
    0.5725622693459446D+00,0.5725622693472699D+00, &
    0.5725622693460108D+00,-.2791087112264201D+00, &
    -.2791087112266160D+00,-.2791087112254542D+00, &
    -.3714154371111505D+00,-.3714154371112492D+00, &
    -.3714154371110855D+00,-.3161738417128371D+00, &
    -.3161738417122791D+00,-.3161738417134797D+00, &
    0.8373277100424955D+00,0.8373277100425663D+00, &
    0.8373277100420310D+00,0.2498873918545072D+00, &
    0.2498873918552929D+00,0.2498873918556660D+00, &
    0.5108904140436563D+00,0.5108904140435110D+00, &
    0.5108904140435037D+00,-.3706147756740159D+00, &
    -.3706147756740524D+00,-.3706147756740479D+00, &
    0.2433004560275582D+00,0.2433004560300772D+00, &
    0.2433004560291551D+00,-.3538702545014873D+00, &
    -.3538702545021283D+00,-.3538702545024522D+00, &
    -.3582651744000784D+00,-.3582651743999515D+00, &
    -.3582651744000865D+00,0.8755062971445479D-01, &
    0.8755062971455986D-01,0.8755062971379197D-01, &
    -.1397307998126365D+00,-.1397307998123260D+00, &
    -.1397307998124737D+00,-.2183761124774179D+00, &
    -.2183761124772129D+00,-.2183761124776710D+00, &
    0.5763507816362148D+00,0.5763507816368744D+00, &
    0.5763507816365103D+00,0.8062208425433149D+00, &
    0.8062208425454057D+00,0.8062208425485021D+00, &
    -.4049658784349283D+00,-.4049658784337947D+00, &
    -.4049658784361176D+00,0.2225706813796632D+00, &
    0.2225706813790471D+00,0.2225706813801265D+00, &
    -.4096725239466332D-02,-.4096725240921904D-02, &
    -.4096725238250503D-02,-.2592447810085974D+00, &
    0.1191149121741652D+01,0.4120653127511454D+00, &
    0.8435262601344188D+00,-.4003639673386698D+00, &
    0.3321002545060888D-01 /
  data ws / &
    0.4194830168311239D-02,0.4194830168326290D-02, &
    0.4194830168354955D-02,0.6505251369757091D-02, &
    0.6505251369749837D-02,0.6505251369724089D-02, &
    0.1511857423469778D-02,0.1511857423531536D-02, &
    0.1511857423435278D-02,0.3576693638591392D-02, &
    0.3576693638610991D-02,0.3576693638618915D-02, &
    0.3373656058688358D-02,0.3373656058691433D-02, &
    0.3373656058671795D-02,0.1076784154760114D-01, &
    0.1076784154761919D-01,0.1076784154751974D-01, &
    0.3758938857122322D-02,0.3758938857165091D-02, &
    0.3758938857143189D-02,0.8302444588410703D-02, &
    0.8302444588357655D-02,0.8302444588429193D-02, &
    0.2942777031213149D-02,0.2942777031243674D-02, &
    0.2942777031245480D-02,0.8843589566618218D-02, &
    0.8843589566569863D-02,0.8843589566624911D-02, &
    0.9759028519288406D-03,0.9759028519397565D-03, &
    0.9759028519575427D-03,0.2004802238525724D-02, &
    0.2004802238518510D-02,0.2004802238530063D-02, &
    0.9695765259457370D-03,0.9695765259577475D-03, &
    0.9695765259399248D-03,0.1954897938090337D-02, &
    0.1954897938078232D-02,0.1954897938036924D-02, &
    0.3691846518220940D-02,0.3691846518218663D-02, &
    0.3691846518232707D-02,0.7970801887754822D-02, &
    0.7970801887704541D-02,0.7970801887626933D-02, &
    0.1979189926163023D-02,0.1979189926220139D-02, &
    0.1979189926268928D-02,0.3698742482337427D-02, &
    0.3698742482364018D-02,0.3698742482353030D-02, &
    0.1687317723859350D-02,0.1687317723863163D-02, &
    0.1687317723875827D-02,0.3835254087109209D-02, &
    0.3835254087144783D-02,0.3835254087114301D-02, &
    0.1016260451104831D-01,0.1016260451098046D-01, &
    0.1016260451102241D-01,0.5267012161743862D-02, &
    0.5267012161757533D-02,0.5267012161774349D-02, &
    0.1528430187195588D-02,0.1528430187157122D-02, &
    0.1528430187208298D-02,0.2982437800867588D-02, &
    0.2982437800865273D-02,0.2982437800863566D-02, &
    0.1466557323209961D-02,0.1466557323217223D-02, &
    0.1466557323219012D-02,0.1461057795662250D-01, &
    0.1461057795662684D-01,0.1461057795667047D-01, &
    0.5561076940885510D-02,0.5561076940819906D-02, &
    0.5561076940859880D-02,0.9751977499183117D-02, &
    0.9751977499261835D-02,0.9751977499259098D-02, &
    0.5302530732607095D-02,0.5302530732562353D-02, &
    0.5302530732501225D-02,0.7136244952723934D-02, &
    0.7136244952687442D-02,0.7136244952687213D-02, &
    0.3962851386077182D-02,0.3962851386075291D-02, &
    0.3962851386117417D-02,0.3518864309482585D-02, &
    0.3518864309488224D-02,0.3518864309487660D-02, &
    0.1910503466789297D-02,0.1910503466785413D-02, &
    0.1910503466785542D-02,0.1114372702758599D-01, &
    0.1114372702763622D-01,0.1114372702759230D-01, &
    0.9339073844350225D-02,0.9339073844312899D-02, &
    0.9339073844325385D-02,0.1427519172629088D-01, &
    0.1427519172633135D-01,0.1427519172637184D-01, &
    0.4705321020321325D-02,0.4705321020382762D-02, &
    0.4705321020369120D-02,0.4787930353379107D-02, &
    0.4787930353341169D-02,0.4787930353337981D-02, &
    0.3363836009342347D-02,0.3363836009325845D-02, &
    0.3363836009335837D-02,0.1284569887457350D-01, &
    0.1284569887456974D-01,0.1284569887463706D-01, &
    0.2752818417406538D-02,0.2752818417393949D-02, &
    0.2752818417406429D-02,0.2544892612698709D-02, &
    0.2544892612676544D-02,0.2544892612707782D-02, &
    0.3052619386398206D-02,0.3052619386307692D-02, &
    0.3052619386349196D-02,0.8865680919273325D-02, &
    0.8865680919243184D-02,0.8865680919267345D-02, &
    0.9030103498125034D-02,0.9030103498122236D-02, &
    0.9030103498131978D-02,0.2991177381103681D-02, &
    0.2991177381087045D-02,0.2991177381095963D-02, &
    0.2211164766946887D-02,0.2211164766969601D-02, &
    0.2211164766996503D-02,0.2973562062279324D-03, &
    0.2973562062207861D-03,0.2973562062075669D-03, &
    0.9172628789611696D-02,0.9172628789611484D-02, &
    0.9172628789627685D-02,0.9783236108387362D-02, &
    0.9783236108363432D-02,0.9783236108405431D-02, &
    0.1738125981964304D-01,0.1738125981965133D-01, &
    0.1738125981965202D-01,0.9766706161581359D-02, &
    0.9766706161581049D-02,0.9766706161590663D-02, &
    0.3005216389199320D-02,0.3005216389178637D-02, &
    0.3005216389160241D-02,0.2785222700150757D-02, &
    0.2785222700144989D-02,0.2785222700178524D-02, &
    0.3753359578155891D-03,0.3753359578293081D-03, &
    0.3753359577958143D-03,0.2191182863239439D-02, &
    0.2191182863316704D-02,0.2191182863281961D-02, &
    0.2531026444894308D-02,0.2531026444936393D-02, &
    0.2531026444945725D-02,0.1445748928029682D-01, &
    0.1287820052356487D-03,0.1574864463007665D-01, &
    0.4621613088145150D-02,0.2792502892512972D-02, &
    0.1851564470805342D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine rule15 ( n, x, w )

!*****************************************************************************80
!
!! RULE15 returns the rule of degree 15.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 214 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.2581521194788427D+00,-.3924996680742822D+00, &
    0.1343475485954617D+00,0.1747936627600043D+00, &
    -.3848787335398570D+00,0.2100850707798483D+00, &
    -.2686158385486640D+00,0.3159539810469303D+00, &
    -.4733814249823749D-01,0.5356808415765576D-02, &
    -.8659172184635908D-01,0.8123491343054050D-01, &
    -.8144631134845116D+00,0.8470041241206219D+00, &
    -.3254101063610144D-01,0.2249358441935477D+00, &
    -.7323416768865754D+00,0.5074058326930275D+00, &
    -.5473570384341295D+00,0.4843265718105462D+00, &
    0.6303046662358983D-01,-.2378791442311287D-01, &
    -.2016252113668169D+00,0.2254131257898949D+00, &
    -.7391871819240758D-01,0.5113049221289290D+00, &
    -.4373862039365108D+00,-.2968004530699073D+00, &
    0.1205383163903803D+00,0.1762621366794993D+00, &
    -.4241098255347354D-02,-.3585193417940622D+00, &
    0.3627604400494130D+00,0.4202386364031931D-02, &
    0.1599370643332776D-01,-.2019609279738776D-01, &
    -.5388551273867861D-01,-.7018353952881240D+00, &
    0.7557209080267955D+00,0.3646027806298292D-01, &
    -.6343658219832248D+00,0.5979055439202614D+00, &
    0.1258503466822884D-01,0.2393555213511634D+00, &
    -.2519405560194015D+00,0.3851625317792869D-02, &
    0.1534194382150093D+00,-.1572710635328328D+00, &
    -.3485301409937485D-02,0.1619818108911392D+00, &
    -.1584965094812263D+00,0.5304635114647127D-01, &
    0.3545913748848099D+00,-.4076377260313040D+00, &
    0.6653928546486268D-01,-.2403180998360646D+00, &
    0.1737788143712072D+00,-.1667348931062531D+00, &
    -.2713747582479675D+00,0.4381096513542055D+00, &
    0.9493532822319652D-01,0.1868738963948708D-02, &
    -.9680406718713280D-01,-.5105981722468006D-01, &
    0.3251121848754134D+00,-.2740523676507155D+00, &
    -.2080245286773480D-01,-.1497129535134266D+00, &
    0.1705154063812837D+00,0.2921781238709439D+00, &
    0.1381062592802215D+00,-.4302843831511968D+00, &
    -.8780905828000338D-01,0.2673000828923510D+00, &
    -.1794910246123719D+00,-.1944587456799732D-01, &
    -.2823011707456111D+00,0.3017470453136293D+00, &
    -.7313431320411609D-01,-.5236633756518274D+00, &
    0.5967976888559381D+00,-.3762507052448836D+00, &
    0.2871315908267298D+00,0.8911911441816055D-01, &
    -.3797896271838643D-01,-.4650879543817582D+00, &
    0.5030669171001346D+00,0.2750555493489867D-02, &
    -.7277556508895471D+00,0.7250050953960729D+00, &
    0.6123006138565600D+00,-.1192649298638286D+00, &
    -.4930356839927218D+00,-.2645727220089428D+00, &
    -.1730736170401170D+00,0.4376463390490505D+00, &
    -.6630962451131286D-01,-.1763224174379790D+00, &
    0.2426320419492632D+00,-.4200384093190587D-02, &
    -.5418782850881636D+00,0.5460786691813587D+00, &
    0.5394663228522688D-01,0.1507657818092707D+00, &
    -.2047124140944948D+00,0.1018171426999433D+00, &
    -.5394039692520657D+00,0.4375868265521267D+00, &
    0.1245513301303478D+00,-.6455557549708775D+00, &
    0.5210044248405197D+00,-.9855929555799212D-01, &
    -.7473211662573033D+00,0.8458804618152944D+00, &
    -.1755388020283085D+00,-.5318861672552897D+00, &
    0.7074249692836033D+00,-.6267629157220122D-02, &
    0.6632219831012681D-01,-.6005456915289890D-01, &
    -.2023381108267559D+00,0.1094534186561336D+00, &
    0.9288469217061772D-01,-.6473499743579236D-01, &
    -.3234505788317799D+00,0.3881855762675665D+00, &
    -.1233025545427212D+00,-.2864321270324415D+00, &
    0.4097346815751876D+00,0.1989648914526389D+00, &
    -.5364226464888502D+00,0.3374577550361906D+00, &
    0.2178991630195039D-01,-.5426229008583486D+00, &
    0.5208329845563997D+00,-.2014523059829409D+00, &
    -.1081005526244278D+00,0.3095528586073633D+00, &
    -.1169567835631198D-01,0.1260374294457714D+00, &
    -.1143417510894922D+00,0.6829681010950600D-01, &
    -.2897229857831844D+00,0.2214261756736523D+00, &
    -.4298175003068387D+00,0.2110526002984961D+00, &
    0.2187649000083270D+00,0.6578307149076966D+00, &
    -.1808749039318607D+00,-.4769558109758388D+00, &
    -.3704630736636612D+00,0.3211543940711805D+00, &
    0.4930867959248819D-01,-.1193005517808560D+00, &
    -.3998990653549201D+00,0.5191996171357768D+00, &
    -.1504520555101911D+00,0.4460532087610402D+00, &
    -.2956011532508667D+00,0.1975830054753616D+00, &
    -.6709593039696905D+00,0.4733762984943273D+00, &
    -.7463137388742009D-02,-.4211815306853685D+00, &
    0.4286446680740814D+00,0.2049473200886011D-02, &
    -.2026603403979144D+00,0.2006108671970308D+00, &
    0.7152086010197926D-01,-.7652447634530471D+00, &
    0.6937239033510662D+00,0.2166839105383212D+00, &
    0.3458968024421906D+00,-.5625807129804981D+00, &
    0.1279853939984014D+00,0.3081799665774158D+00, &
    -.4361653605757960D+00,-.1577812374505563D-01, &
    -.6857246456300986D+00,0.7015027693751511D+00, &
    -.7105996230949263D-03,-.9227565022751169D+00, &
    0.9234671018982127D+00,-.1882955447783474D-01, &
    -.7344571519218671D+00,0.7532867063996859D+00, &
    -.3375217299349303D+00,-.2041740513278847D+00, &
    0.5416957812628146D+00,0.4996795038021838D+00, &
    0.1342360979189166D+00,-.6339156017211199D+00, &
    0.7596408799506882D+00,0.8346506611711336D-01, &
    -.8431059460678011D+00,-.8820027427037123D+00, &
    0.8657493528015160D+00,0.1625338990220750D-01, &
    0.7067742334596752D+00,-.2761780760336056D+00, &
    -.4305961574260422D+00,-.9186459446440826D-01, &
    -.1188760024130717D+00,0.2107405968774760D+00, &
    0.5405133012525005D+00,-.2620825757773786D+00, &
    -.2784307254751027D+00,0.1458059413495244D+00, &
    0.3829161242112978D+00,-.5287220655607965D+00, &
    -.1823158001392015D-13,-.1699106971810907D-13, &
    0.1569922552833598D-12,0.5359985656924711D-13 /
  data ys / &
    0.3041753823660573D+00,0.7147860232646702D-01, &
    -.3756539846925305D+00,0.3435025125820099D+00, &
    -.2037550392031693D-01,-.3231270086616979D+00, &
    -.2097468053232621D+00,-.1277547373803833D+00, &
    0.3375015427036143D+00,0.9689475305431051D-01, &
    -.4380824435589394D-01,-.5308650869840935D-01, &
    -.5078056203162217D+00,-.4514429365648450D+00, &
    0.9592485568810678D+00,0.7157685583829504D+00, &
    -.1630841238981528D+00,-.5526844344847958D+00, &
    -.2432354197382121D+00,-.3524073903550577D+00, &
    0.5956428100933059D+00,0.2465506989117151D+00, &
    -.1438762876493171D+00,-.1026744112624022D+00, &
    -.5477270770116898D+00,0.2098480507360208D+00, &
    0.3378790262756754D+00,0.3217216264418476D-01, &
    -.2731228135353417D+00,0.2409506508911702D+00, &
    0.4164310762083722D+00,-.2118884369332560D+00, &
    -.2045426392751013D+00,-.2089419032777848D-01, &
    0.1408646851164702D-01,0.6807721816119913D-02, &
    0.8415205240779101D+00,-.4674264849665979D+00, &
    -.3740940391113035D+00,0.7114522048190210D+00, &
    -.3241505753779200D+00,-.3873016294411025D+00, &
    -.2836499225217341D+00,0.1527239209910285D+00, &
    0.1309260015306806D+00,-.1793772448187649D+00, &
    0.9302422778043601D-01,0.8635301703831089D-01, &
    -.1850282445363872D+00,0.8949576270733584D-01, &
    0.9553248182902167D-01,-.4400731765980834D+00, &
    0.2659760759699537D+00,0.1740971006281070D+00, &
    0.2390789648881394D+00,-.6191477088183470D-01, &
    -.1771641940062932D+00,0.4096210148696401D+00, &
    -.3492071605621237D+00,-.6041385430752706D-01, &
    -.5696877119302565D-01,0.1107007915544150D+00, &
    -.5373202036138137D-01,-.3459278156898313D+00, &
    0.1287448090157230D+00,0.2171830066740836D+00, &
    0.1848839297874223D+00,-.1104574175380845D+00, &
    -.7442651224930893D-01,-.3281604904126481D+00, &
    0.4171139229086880D+00,-.8895343249598349D-01, &
    -.2579549661893832D+00,0.5293260794183284D-01, &
    0.2050223582475361D+00,0.3372003947615109D+00, &
    -.1854408187554140D+00,-.1517595760060760D+00, &
    0.6468984972100610D+00,-.3867854217281152D+00, &
    -.2601130754819361D+00,-.1143225565573846D+00, &
    -.2686813906552311D+00,0.3830039472125820D+00, &
    0.5589644756672905D+00,-.3123729843571679D+00, &
    -.2465914913101748D+00,0.8387518079361266D+00, &
    -.4169938530361755D+00,-.4217579548999488D+00, &
    -.2157966455115084D+00,0.6381662091083642D+00, &
    -.4223695635968309D+00,0.3525993310475737D+00, &
    -.4054263639319154D+00,0.5282703288436957D-01, &
    0.2418834699054176D+00,-.1783675542949067D+00, &
    -.6351591561047869D-01,0.6281322404142361D+00, &
    -.3177037595374698D+00,-.3104284808767618D+00, &
    -.2052354320960713D+00,0.1493368700556973D+00, &
    0.5589856204040973D-01,0.5640658989533277D+00, &
    -.1938567173577759D+00,-.3702091815955630D+00, &
    0.6735138338400053D+00,-.2288923009519804D+00, &
    -.4446215328880427D+00,0.9198353888410614D+00, &
    -.5452725481528549D+00,-.3745628406882141D+00, &
    0.7155166182904237D+00,-.5097793710516186D+00, &
    -.2057372472388190D+00,-.7296366071409344D-01, &
    0.3105390428539375D-01,0.4190975642870679D-01, &
    -.9565958696542072D-02,-.1704469647814408D+00, &
    0.1800129234779798D+00,0.4108633257116824D+00, &
    -.2614938151491584D+00,-.1493695105625245D+00, &
    0.4019320943505255D+00,-.3077491917607414D+00, &
    -.9418290258973967D-01,0.5045350850600017D+00, &
    -.7995889207082835D-01,-.4245761929892107D+00, &
    0.6139865417154907D+00,-.2881226497939412D+00, &
    -.3258638919215674D+00,0.2411323094026556D+00, &
    -.2950289693335045D+00,0.5389665993088002D-01, &
    -.1387829845896145D+00,0.5926273772376190D-01, &
    0.7952024686583803D-01,0.2951121059631410D+00, &
    -.8840928042931671D-01,-.2067028255338328D+00, &
    0.4452698313519278D-02,-.3744592234135989D+00, &
    0.3700065251000597D+00,-.1709423913837525D+00, &
    0.6551693061916171D+00,-.4842269148078915D+00, &
    -.1569501964323080D+00,-.2423553347406391D+00, &
    0.3993055311729469D+00,0.5306418717478397D+00, &
    -.3686382444016378D+00,-.1620036273461921D+00, &
    -.4281943455532308D+00,0.8380187065320079D-01, &
    0.3443924749000115D+00,0.6606824681258688D+00, &
    -.1592293319651774D+00,-.5014531361606821D+00, &
    0.4906473846181765D+00,-.2517869588796617D+00, &
    -.2388604257384930D+00,0.2328287402613732D+00, &
    -.1146394742743420D+00,-.1181892659870265D+00, &
    0.8423359525185951D+00,-.3592290945104593D+00, &
    -.4831068580081231D+00,-.5245097380820163D+00, &
    0.4499086401585516D+00,0.7460109792348690D-01, &
    -.4297479750019432D+00,0.3257125900169633D+00, &
    0.1040353849849871D+00,0.8009161214805088D+00, &
    -.4141223167275365D+00,-.3867938047529831D+00, &
    0.1065917694853711D+01,-.5335742447523749D+00, &
    -.5323434501013379D+00,0.8589493170871557D+00, &
    -.4457815310633328D+00,-.4131677860238335D+00, &
    0.4306281486266579D+00,-.5076164667662489D+00, &
    0.7698831813957928D-01,-.4434925905656182D+00, &
    0.6544814393259342D+00,-.2109888487602922D+00, &
    -.5349560233082641D+00,0.9253463114445929D+00, &
    -.3903902881363264D+00,-.4904567228554135D+00, &
    -.5186084199612601D+00,0.1009065142816689D+01, &
    -.8915332085968262D-01,0.6566611013461990D+00, &
    -.5675077804865333D+00,0.1903042323297749D+00, &
    -.1747091886794188D+00,-.1559504365035713D-01, &
    -.9438608628742183D-02,0.4728175542824327D+00, &
    -.4633789456536745D+00,-.5263345542684673D+00, &
    0.3894389263656265D+00,0.1368956279028533D+00, &
    -.1738343488671503D-14,-.2490206331023665D-15, &
    0.1028727833546315D-12,0.7810443937532853D-13 /
  data zs / &
    0.1149415369298274D+00,0.1149415369297980D+00, &
    0.1149415369297721D+00,-.1425141964887960D+00, &
    -.1425141964887534D+00,-.1425141964887875D+00, &
    -.3897391650202160D+00,-.3897391650202120D+00, &
    -.3897391650202178D+00,0.7425283167186030D+00, &
    0.7425283167186189D+00,0.7425283167186136D+00, &
    -.2856112242082660D+00,-.2856112242082698D+00, &
    -.2856112242082572D+00,-.3891867546444293D+00, &
    -.3891867546444389D+00,-.3891867546444343D+00, &
    0.1847325582324109D+00,0.1847325582324027D+00, &
    0.1847325582324080D+00,0.5802704682522071D+00, &
    0.5802704682522261D+00,0.5802704682522338D+00, &
    -.3914413911049179D+00,-.3914413911049140D+00, &
    -.3914413911049208D+00,0.4061500673930710D+00, &
    0.4061500673931097D+00,0.4061500673930845D+00, &
    0.5808632391400850D+00,0.5808632391400872D+00, &
    0.5808632391401026D+00,0.1122300778479799D+01, &
    0.1122300778479790D+01,0.1122300778479803D+01, &
    -.1702382601410010D+00,-.1702382601409925D+00, &
    -.1702382601409868D+00,-.2928517214460706D-01, &
    -.2928517214460719D-01,-.2928517214460035D-01, &
    -.2378639271913669D-01,-.2378639271913052D-01, &
    -.2378639271915441D-01,-.3876002482780669D+00, &
    -.3876002482780664D+00,-.3876002482780634D+00, &
    -.1573276400467179D+00,-.1573276400467495D+00, &
    -.1573276400467503D+00,-.9846677659403662D-01, &
    -.9846677659401415D-01,-.9846677659402990D-01, &
    0.4195749508887953D+00,0.4195749508888120D+00, &
    0.4195749508888156D+00,0.1698706478280650D+00, &
    0.1698706478280417D+00,0.1698706478280619D+00, &
    0.1001045633396894D+01,0.1001045633396896D+01, &
    0.1001045633396904D+01,0.1278252986614525D+00, &
    0.1278252986614319D+00,0.1278252986614156D+00, &
    0.6699851292944298D-01,0.6699851292952046D-01, &
    0.6699851292943053D-01,0.1888603052005784D+00, &
    0.1888603052005364D+00,0.1888603052005152D+00, &
    0.4162319117452538D+00,0.4162319117452708D+00, &
    0.4162319117452612D+00,-.1429377419107346D+00, &
    -.1429377419107034D+00,-.1429377419107302D+00, &
    0.7526757476604347D-01,0.7526757476604869D-01, &
    0.7526757476604784D-01,0.5502259745637508D-01, &
    0.5502259745639024D-01,0.5502259745637604D-01, &
    -.3783645730290734D+00,-.3783645730290731D+00, &
    -.3783645730290733D+00,-.3557858186989302D-01, &
    -.3557858186987815D-01,-.3557858186989157D-01, &
    -.1180094770935530D+00,-.1180094770935434D+00, &
    -.1180094770935428D+00,-.1637480027557202D+00, &
    -.1637480027557221D+00,-.1637480027557144D+00, &
    0.2977454860475704D+00,0.2977454860475816D+00, &
    0.2977454860475674D+00,0.2596712604598457D+00, &
    0.2596712604598533D+00,0.2596712604598482D+00, &
    0.2271508926955859D+00,0.2271508926955554D+00, &
    0.2271508926955697D+00,-.2779926178974129D+00, &
    -.2779926178973848D+00,-.2779926178974117D+00, &
    -.3823432777224465D+00,-.3823432777224400D+00, &
    -.3823432777224473D+00,-.3775386322444910D+00, &
    -.3775386322444932D+00,-.3775386322444943D+00, &
    -.2829688627011475D+00,-.2829688627011491D+00, &
    -.2829688627011544D+00,0.9266659902735654D+00, &
    0.9266659902735608D+00,0.9266659902735637D+00, &
    0.7042408708164912D+00,0.7042408708164883D+00, &
    0.7042408708164785D+00,0.4116884047492347D+00, &
    0.4116884047492301D+00,0.4116884047492345D+00, &
    0.1190964787818936D-01,0.1190964787820830D-01, &
    0.1190964787819720D-01,-.9824886296290353D-01, &
    -.9824886296290938D-01,-.9824886296292371D-01, &
    -.1210076932793752D+00,-.1210076932793519D+00, &
    -.1210076932793500D+00,-.2970323317075053D+00, &
    -.2970323317075050D+00,-.2970323317075011D+00, &
    0.5828065214604544D+00,0.5828065214604353D+00, &
    0.5828065214604685D+00,-.3017876570907457D+00, &
    -.3017876570907429D+00,-.3017876570907401D+00, &
    -.3862244277990527D+00,-.3862244277990511D+00, &
    -.3862244277990521D+00,-.3804346304483138D+00, &
    -.3804346304483134D+00,-.3804346304483132D+00, &
    0.4614518667838061D+00,0.4614518667838234D+00, &
    0.4614518667838163D+00,-.2664567796530529D+00, &
    -.2664567796530461D+00,-.2664567796530568D+00, &
    -.3834440210666236D+00,-.3834440210666228D+00, &
    -.3834440210666243D+00,-.2809660745834901D+00, &
    -.2809660745835049D+00,-.2809660745834982D+00, &
    0.1843225423322873D+00,0.1843225423322890D+00, &
    0.1843225423322869D+00,0.7949931745648482D+00, &
    0.7949931745648681D+00,0.7949931745648552D+00, &
    -.2296019504049502D+00,-.2296019504049503D+00, &
    -.2296019504049410D+00,-.3768873297184356D+00, &
    -.3768873297184376D+00,-.3768873297184345D+00, &
    -.2779742390429553D+00,-.2779742390429600D+00, &
    -.2779742390429549D+00,-.3088199065240608D+00, &
    -.3088199065240601D+00,-.3088199065240508D+00, &
    -.3819799973245133D+00,-.3819799973245119D+00, &
    -.3819799973245152D+00,-.4015361795222331D+00, &
    -.4015361795222343D+00,-.4015361795222284D+00, &
    -.3045707129688655D+00,-.3045707129688613D+00, &
    -.3045707129688553D+00,-.3905755491124832D-01, &
    -.3905755491127184D-01,-.3905755491126780D-01, &
    -.3726108784532423D+00,-.3726108784532439D+00, &
    -.3726108784532431D+00,-.2575990026123318D+00, &
    -.2575990026123464D+00,-.2575990026123427D+00, &
    -.3954674471205766D+00,-.3954674471205773D+00, &
    -.3954674471205850D+00,0.7158508859841455D+00, &
    0.7158508859841465D+00,0.7158508859841323D+00, &
    -.9143187100549784D-01,-.9143187100549642D-01, &
    -.9143187100549681D-01,-.2639610543972574D+00, &
    -.2639610543972473D+00,-.2639610543972535D+00, &
    -.3003943802989339D+00,0.4275416212259733D+00, &
    0.1454645115620674D+00,-.1072798570797851D+00 /
  data ws / &
    0.2565379947273549D-02,0.2565379947271849D-02, &
    0.2565379947272177D-02,0.6723785911414816D-02, &
    0.6723785911411640D-02,0.6723785911413937D-02, &
    0.3085562545744757D-02,0.3085562545745181D-02, &
    0.3085562545744572D-02,0.4446982777989968D-02, &
    0.4446982777989351D-02,0.4446982777990241D-02, &
    0.1185105459270273D-02,0.1185105459270278D-02, &
    0.1185105459270443D-02,0.8364165694309715D-03, &
    0.8364165694306848D-03,0.8364165694306367D-03, &
    0.1867492375962935D-02,0.1867492375963348D-02, &
    0.1867492375962387D-02,0.4264615556690722D-02, &
    0.4264615556689985D-02,0.4264615556689865D-02, &
    0.1036750961119152D-02,0.1036750961119384D-02, &
    0.1036750961118855D-02,0.2897017453521770D-02, &
    0.2897017453522957D-02,0.2897017453522235D-02, &
    0.9931774636746942D-03,0.9931774636749676D-03, &
    0.9931774636745359D-03,0.2968267531327685D-03, &
    0.2968267531331858D-03,0.2968267531325610D-03, &
    0.2082824878589059D-02,0.2082824878589280D-02, &
    0.2082824878589393D-02,0.3532360099744144D-02, &
    0.3532360099743258D-02,0.3532360099742647D-02, &
    0.9852850436647919D-02,0.9852850436649995D-02, &
    0.9852850436648092D-02,0.4465622015302662D-02, &
    0.4465622015302984D-02,0.4465622015303046D-02, &
    0.1064447126836857D-01,0.1064447126836189D-01, &
    0.1064447126836531D-01,0.4362160716691730D-02, &
    0.4362160716692162D-02,0.4362160716691511D-02, &
    0.7013986090707679D-02,0.7013986090706681D-02, &
    0.7013986090707788D-02,0.4089047890127443D-02, &
    0.4089047890127423D-02,0.4089047890126841D-02, &
    0.8331696920946515D-03,0.8331696920946096D-03, &
    0.8331696920947063D-03,0.5692536319085584D-02, &
    0.5692536319084854D-02,0.5692536319085869D-02, &
    0.1193994326736248D-01,0.1193994326737174D-01, &
    0.1193994326736250D-01,0.4894866890556946D-02, &
    0.4894866890557526D-02,0.4894866890558155D-02, &
    0.4559845100025953D-02,0.4559845100026132D-02, &
    0.4559845100026570D-02,0.1112520926249036D-01, &
    0.1112520926249222D-01,0.1112520926248886D-01, &
    0.2944181342559152D-02,0.2944181342559592D-02, &
    0.2944181342559789D-02,0.8953416346063897D-02, &
    0.8953416346061954D-02,0.8953416346063781D-02, &
    0.3952683058023190D-02,0.3952683058023330D-02, &
    0.3952683058022607D-02,0.1439142717612964D-02, &
    0.1439142717612978D-02,0.1439142717612582D-02, &
    0.5039224704625283D-02,0.5039224704625251D-02, &
    0.5039224704625310D-02,0.7617567889070472D-02, &
    0.7617567889069997D-02,0.7617567889070361D-02, &
    0.1054110867291372D-01,0.1054110867291354D-01, &
    0.1054110867291408D-01,0.1741617181694419D-02, &
    0.1741617181694472D-02,0.1741617181694386D-02, &
    0.1100640373498494D-01,0.1100640373498203D-01, &
    0.1100640373498438D-01,0.6926338144663090D-02, &
    0.6926338144663959D-02,0.6926338144663389D-02, &
    0.2939871895670393D-02,0.2939871895670890D-02, &
    0.2939871895670184D-02,0.1098371982889171D-02, &
    0.1098371982888983D-02,0.1098371982889088D-02, &
    0.2867324218775673D-02,0.2867324218775326D-02, &
    0.2867324218775453D-02,0.2627584056743849D-02, &
    0.2627584056743863D-02,0.2627584056743943D-02, &
    0.2450903116528004D-02,0.2450903116528749D-02, &
    0.2450903116529087D-02,0.3781264707127557D-02, &
    0.3781264707127677D-02,0.3781264707128319D-02, &
    0.9717660774153364D-02,0.9717660774155940D-02, &
    0.9717660774153394D-02,0.5844696436169811D-02, &
    0.5844696436168778D-02,0.5844696436168716D-02, &
    0.7798609566561363D-02,0.7798609566560966D-02, &
    0.7798609566562299D-02,0.9537993724461966D-02, &
    0.9537993724462559D-02,0.9537993724462396D-02, &
    0.6470868601806971D-02,0.6470868601806687D-02, &
    0.6470868601806209D-02,0.9788338589999005D-02, &
    0.9788338589999012D-02,0.9788338589999477D-02, &
    0.4149912310618085D-02,0.4149912310618563D-02, &
    0.4149912310618460D-02,0.2801823664802904D-02, &
    0.2801823664802910D-02,0.2801823664803036D-02, &
    0.3473047605746596D-02,0.3473047605746489D-02, &
    0.3473047605747130D-02,0.8146696772355114D-02, &
    0.8146696772356077D-02,0.8146696772355570D-02, &
    0.4104346629768971D-02,0.4104346629769071D-02, &
    0.4104346629768999D-02,0.3673293016416287D-02, &
    0.3673293016415687D-02,0.3673293016415978D-02, &
    0.7385814346097036D-02,0.7385814346096885D-02, &
    0.7385814346097676D-02,0.1952835342529582D-02, &
    0.1952835342529196D-02,0.1952835342529467D-02, &
    0.2956577367937866D-02,0.2956577367938132D-02, &
    0.2956577367937872D-02,0.2429727102722713D-02, &
    0.2429727102722471D-02,0.2429727102722800D-02, &
    0.7838569405690889D-02,0.7838569405690287D-02, &
    0.7838569405690948D-02,0.4786830078294293D-02, &
    0.4786830078294275D-02,0.4786830078294352D-02, &
    0.6464879373388097D-03,0.6464879373388156D-03, &
    0.6464879373387403D-03,0.1094759729336998D-02, &
    0.1094759729336943D-02,0.1094759729337179D-02, &
    0.4183486775030887D-02,0.4183486775030987D-02, &
    0.4183486775031043D-02,0.1388382329059297D-02, &
    0.1388382329058254D-02,0.1388382329057900D-02, &
    0.1547261679853963D-02,0.1547261679853909D-02, &
    0.1547261679853913D-02,0.5245559039887955D-03, &
    0.5245559039888078D-03,0.5245559039885548D-03, &
    0.4714073461717449D-03,0.4714073461717063D-03, &
    0.4714073461712849D-03,0.1734297963526248D-02, &
    0.1734297963526404D-02,0.1734297963527124D-02, &
    0.2194380264311005D-02,0.2194380264311055D-02, &
    0.2194380264311861D-02,0.1505920846848116D-02, &
    0.1505920846847692D-02,0.1505920846848549D-02, &
    0.1092191482369307D-01,0.1402579642206070D-01, &
    0.9137907285788647D-02,0.8807144129412933D-02 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  call r8vec_copy ( n, ws, w )

  return
end
subroutine tetrahedron_arbq ( degree, n, x, w )

!*****************************************************************************80
!
!! TETRAHEDRON_ARBQ returns a quadrature rule for a tetrahedron.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DEGREE, the desired degree of exactness.
!    1 <= DEGREE <= 15.
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!    This value should be requested first from TETRAHEDRON_ARBQ_SIZE.
!
!    Output, real ( kind = 8 ) X(3,N), the quadrature nodes.
!
!    Output, real ( kind = 8 ) W(N), the quadrature weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) d
  integer ( kind = 4 ) degree
  real ( kind = 8 ) r8vec_sum
  real ( kind = 8 ) volume
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(3,n)

  if ( degree == 1 ) then
    call rule01 ( n, x, w )
  else if ( degree == 2 ) then
    call rule02 ( n, x, w )
  else if ( degree == 3 ) then
    call rule03 ( n, x, w )
  else if ( degree == 4 ) then
    call rule04 ( n, x, w )
  else if ( degree == 5 ) then
    call rule05 ( n, x, w )
  else if ( degree == 6 ) then
    call rule06 ( n, x, w )
  else if ( degree == 7 ) then
    call rule07 ( n, x, w )
  else if ( degree == 8 ) then
    call rule08 ( n, x, w )
  else if ( degree == 9 ) then
    call rule09 ( n, x, w )
  else if ( degree == 10 ) then
    call rule10 ( n, x, w )
  else if ( degree == 11 ) then
    call rule11 ( n, x, w )
  else if ( degree == 12 ) then
    call rule12 ( n, x, w )
  else if ( degree == 13 ) then
    call rule13 ( n, x, w )
  else if ( degree == 14 ) then
    call rule14 ( n, x, w )
  else if ( degree == 15 ) then
    call rule15 ( n, x, w )
  else
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TETRAHEDRON_ARBQ - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of DEGREE.'
    stop 1
  end if

  d = r8vec_sum ( n, w )

  volume = sqrt ( 8.0D+00 ) / 3.0D+00
  w(1:n) = w(1:n) * volume / d

  return
end
subroutine tetrahedron_arbq_gnuplot ( n, x, header )

!*****************************************************************************80
!
!! TETRAHEDRON_ARBQ_GNUPLOT: plot of a quadrature rule in the tetrahedron.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    11 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) N, the number of nodes.
!
!    Input, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Input, character * ( * ) HEADER, a string to be used to identify
!    the files created.
!
  implicit none

  integer ( kind = 4 ) n

  character * ( 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character * ( * ) header
  integer ( kind = 4 ) j
  character * ( 255 ) node_filename
  integer ( kind = 4 ) node_unit
  character * ( 255 ) plot_filename
  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)
  real ( kind = 8 ) v4(3)
  real ( kind = 8 ) x(3,n)
  character * ( 255 ) vertex_filename
  integer ( kind = 4 ) vertex_unit
!
!  Create the tetrahedron file.
!
  call tetrahedron_ref ( v1, v2, v3, v4 )

  call get_unit ( vertex_unit )
  vertex_filename = trim ( header ) // '_vertices.txt'
  open ( unit = vertex_unit, file = vertex_filename, &
    status = 'replace' )

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v1(1), v1(2), v1(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v2(1), v2(2), v2(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v3(1), v3(2), v3(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v1(1), v1(2), v1(3)
  write ( vertex_unit, '(a)' ) ''

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v1(1), v1(2), v1(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v4(1), v4(2), v4(3)
  write ( vertex_unit, '(a)' ) ''

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v2(1), v2(2), v2(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v4(1), v4(2), v4(3)
  write ( vertex_unit, '(a)' ) ''

  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v3(1), v3(2), v3(3)
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    v4(1), v4(2), v4(3)

  close ( unit = vertex_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created vertex file "' // &
    trim ( vertex_filename ) // '".'
!
!  Create node file.
!
  call get_unit ( node_unit )
  node_filename = trim ( header ) // '_nodes.txt'
  open ( unit = node_unit, file = node_filename, &
    status = 'replace' )
  do j = 1, n
    write ( node_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) x(1:3,j)
  end do
  close ( unit = node_unit )
  write ( *, '(a)' ) '  Created node file "' // &
    trim ( node_filename ) // '".'
!
!  Create graphics command file.
!
  call get_unit ( command_unit )
  command_filename = trim ( header ) // '_commands.txt'
  open ( unit = command_unit, file = command_filename, &
    status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // &
    trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  plot_filename = trim ( header ) // '.png'
  write ( command_unit, '(a)' ) 'set output "' // &
    trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
  write ( command_unit, '(a)' ) &
    'set title "' // trim ( header ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set key off'
  write ( command_unit, '(a)' ) 'set view equal xyz'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'splot "' // &
    trim ( vertex_filename ) // &
    '" with lines lw 3, \'
  write ( command_unit, '(a)' ) '     "' // &
    trim ( node_filename ) // '" with points pt 7 lt 0'
  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  return
end
function tetrahedron_arbq_size ( degree )

!*****************************************************************************80
!
!! TETRAHEDRON_ARBQ_SIZE returns the size of quadrature rule for a tetrahedron.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 July 2014
!
!  Author:
!
!    John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DEGREE, the desired degree of exactness.
!    1 <= DEGREE <= 15.
!
!    Output, integer ( kind = 4 ) TETRAHEDRON_ARBQ_SIZE, the number of points 
!    in the corresponding rule.
!
  implicit none

  integer ( kind = 4 ) degree
  integer ( kind = 4 ) n_save(15)
  integer ( kind = 4 ) tetrahedron_arbq_size

  save n_save

  data n_save / &
      1,   4,   6,  11,  14, &
     23,  31,  44,  57,  74, &
     95, 122, 146, 177, 214 /

  if ( degree < 1 .or. 15 < degree ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TETRAHEDRON_ARBQ_SIZE - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of DEGREE.'
    stop 1
  end if

  tetrahedron_arbq_size = n_save ( degree )

  return
end
subroutine tetrahedron_ref ( v1, v2, v3, v4 )

!*****************************************************************************80
!
!! TETRAHEDRON_REF returns the vertices of the reference tetrahedron.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 July 2014
!
!  Author:
!
!    John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) V1(3), V2(3), V3(3), V4(3), the vertices.
!
  implicit none

  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)
  real ( kind = 8 ) v4(3)

  v1(1) = - 1.0D+00
  v1(2) = - 1.0D+00 / sqrt ( 3.0D+00 )
  v1(3) = - 1.0D+00 / sqrt ( 6.0D+00 )

  v2(1) =   0.0D+00
  v2(2) =   2.0D+00 / sqrt ( 3.0D+00 )
  v2(3) = - 1.0D+00 / sqrt ( 6.0D+00 )

  v3(1) =   1.0D+00
  v3(2) = - 1.0D+00 / sqrt ( 3.0D+00 )
  v3(3) = - 1.0D+00 / sqrt ( 6.0D+00 )

  v4(1) =   0.0D+00
  v4(2) =   0.0D+00
  v4(3) =   3.0D+00 / sqrt ( 6.0D+00 )

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
