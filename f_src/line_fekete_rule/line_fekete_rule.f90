subroutine cheby_van1 ( m, a, b, n, x, v )

!*****************************************************************************80
!
!! CHEBY_VAN1 returns the Chebyshev Vandermonde-like matrix for [A,B].
!
!  Discussion:
!
!    Normally, the Chebyshev polynomials are defined on -1 <= XI <= +1.
!    Here, we assume the Chebyshev polynomials have been defined on the
!    interval A <= X <= B, using the mapping
!      XI = ( - ( B - X ) + ( X - A ) ) / ( B - A )
!    so that
!      ChebyAB(A,B;X) = Cheby(XI).
!
!    if ( I == 1 ) then
!      V(1,1:N) = 1;
!    elseif ( I == 2 ) then
!      V(2,1:N) = XI(1:N);
!    else
!      V(I,1:N) = 2.0 * XI(1:N) * V(I-1,1:N) - V(I-2,1:N);
!
!  Example:
!
!    M = 5, N = 5, X = ( 1, 2, 3, 4, 5 )
!
!    1  1   1    1    1
!    1  2   3    4    5
!    1  7  17   31   49
!    1 26  99  244  485
!    1 97 577 1921 4801
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Nicholas Higham,
!    Stability analysis of algorithms for solving confluent
!    Vandermonde-like systems,
!    SIAM Journal on Matrix Analysis and Applications,
!    Volume 11, 1990, pages 23-41.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows of the matrix.
!
!    Input, real ( kind = 8 ) A, B, the interval.
!
!    Input, integer ( kind = 4 ) N, the number of values in X, and the number
!    of columns in the matrix.
!
!    Input, real ( kind = 8 ) X(N), the vector that defines A.
!
!    Output, real ( kind = 8 ) V(M,N), the matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xi(n)
  real ( kind = 8 ) v(m,n)
!
!  Compute the normalized abscissas in [-1,+1].
!
  xi(1:n) = ( - 1.0D+00 * ( b - x(1:n)     )   &
              + 1.0D+00 * (     x(1:n) - a ) ) &
            /             ( b          - a )

  if ( 1 <= m ) then
    v(1,1:n) = 1.0D+00
  end if

  if ( 2 <= m ) then
    v(2,1:n) = xi(1:n)
  end if

  do i = 3, m
    v(i,1:n) = 2.0D+00 * xi(1:n) * v(i-1,1:n) - v(i-2,1:n)
  end do

  return
end
subroutine legendre_van ( m, a, b, n, x, v )

!*****************************************************************************80
!
!! LEGENDRE_VAN returns the LEGENDRE_VAN matrix.
!
!  Discussion:
!
!    The LEGENDRE_VAN matrix is the Legendre Vandermonde-like matrix.
!
!    Normally, the Legendre polynomials are defined on -1 <= XI <= +1.
!    Here, we assume the Legendre polynomials have been defined on the
!    interval A <= X <= B, using the mapping
!      XI = ( - ( B - X ) + ( X - A ) ) / ( B - A )
!    so that
!      Lab(A,B;X) = L(XI).
!
!    if ( I = 1 ) then
!      V(1,1:N) = 1
!    else if ( I = 2 ) then
!      V(2,1:N) = XI(1:N)
!    else
!      V(I,1:N) = ( (2*I-1) * XI(1:N) * V(I-1,1:N) - (I-1)*V(I-2,1:N) ) / I
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows of the matrix.
!
!    Input, real ( kind = 8 ) A, B, the interval.
!
!    Input, integer ( kind = 4 ) N, the number of columns of the matrix.
!
!    Input, real ( kind = 8 ) X(N), the vector that defines the matrix.
!
!    Output, real ( kind = 8 ) V(M,N), the matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) v(m,n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xi

  do j = 1, n

    xi = ( - ( b - x(j) ) + ( x(j) - a ) ) / ( b - a )

    do i = 1, m

      if ( i == 1 ) then
        v(i,j) = 1.0D+00
      else if ( i == 2 ) then
        v(i,j) = xi
      else
        v(i,j) = ( real ( 2 * i - 1, kind = 8 ) * xi * v(i-1,j) + &
                   real (   - i + 1, kind = 8 ) *      v(i-2,j) ) &
                 / real (     i, kind = 8 )
      end if

    end do
  end do

  return
end
subroutine line_fekete_chebyshev ( m, a, b, n, x, nf, xf, wf )

!*****************************************************************************80
!
!! LINE_FEKETE_CHEBYSHEV: approximate Fekete points in an interval [A,B].
!
!  Discussion:
!
!    We use the Chebyshev basis.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Len Bos, Norm Levenberg,
!    On the calculation of approximate Fekete points: the univariate case,
!    Electronic Transactions on Numerical Analysis, 
!    Volume 30, pages 377-397, 2008.
!    
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of basis polynomials.
!
!    Input, real ( kind = 8 ) A, B, the endpoints of the interval.
!
!    Input, integer ( kind = 4 ) N, the number of sample points.
!    M <= N.
!
!    Input, real ( kind = 8 ) X(N), the coordinates of the sample points.
!
!    Output, integer ( kind = 4 ) NF, the number of Fekete points.
!    If the computation is successful, NF = M.
!
!    Output, real ( kind = 8 ) XF(NF), the coordinates of the Fekete points.
!
!    Output, real ( kind = 8 ) WF(NF), the weights of the Fekete points.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) j
  real ( kind = 8 ) mom(m)
  integer ( kind = 4 ) nf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) v(m,n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) wf(m)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xf(m)

  if ( n < m ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LINE_FEKETE_CHEBYSHEV - Fatal error!'
    write ( *, '(a)' ) '  N < M.'
    stop 1
  end if
!
!  Compute the Chebyshev-Vandermonde matrix.
!
  call cheby_van1 ( m, a, b, n, x, v )
!
!  MOM(I) = Integral ( A <= x <= B ) Tab(A,B,I;x) dx
!
  mom(1) = r8_pi * ( b - a ) / 2.0D+00
  mom(2:m) = 0.0D+00
!
!  Solve the system for the weights W.
!
  call qr_solve ( m, n, v, mom, w )
!
!  Extract the data associated with the nonzero weights.
!
  nf = 0
  do j = 1, n
    if ( w(j) /= 0.0D+00 ) then
      if ( nf < m ) then
        nf = nf + 1
        xf(nf) = x(j)
        wf(nf) = w(j)
      end if
    end if
  end do

  return
end
subroutine line_fekete_legendre ( m, a, b, n, x, nf, xf, wf )

!*****************************************************************************80
!
!! LINE_FEKETE_LEGENDRE computes approximate Fekete points in an interval [A,B].
!
!  Discussion:
!
!    We use the uniform weight and the Legendre basis:
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Len Bos, Norm Levenberg,
!    On the calculation of approximate Fekete points: the univariate case,
!    Electronic Transactions on Numerical Analysis, 
!    Volume 30, pages 377-397, 2008.
!    
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of basis polynomials.
!
!    Input, real ( kind = 8 ) A, B, the endpoints of the interval.
!
!    Input, integer ( kind = 4 ) N, the number of sample points.
!    M <= N.
!
!    Input, real ( kind = 8 ) X(N), the coordinates of the sample points.
!
!    Output, integer ( kind = 4 ) NF, the number of Fekete points.
!    If the computation is successful, NF = M.
!
!    Output, real ( kind = 8 ) XF(NF), the coordinates of the Fekete points.
!
!    Output, real ( kind = 8 ) WF(NF), the weights of the Fekete points.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) j
  real ( kind = 8 ) mom(m)
  integer ( kind = 4 ) nf
  real ( kind = 8 ) v(m,n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) wf(m)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xf(m)

  if ( n < m ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LINE_FEKETE_LEGENDRE - Fatal error!'
    write ( *, '(a)' ) '  N < M.'
    stop 1
  end if
!
!  Compute the Legendre-Vandermonde matrix.
!
  call legendre_van ( m, a, b, n, x, v )
!
!  MOM(i) = integral ( A <= X <= B ) Lab(A,B,I;X) dx
!
  mom(1) = b - a
  mom(2:m) = 0.0D+00
!
!  Solve the system for the weights W.
!
  call qr_solve ( m, n, v, mom, w )
!
!  Extract the data associated with the nonzero weights.
!
  nf = 0
  do j = 1, n
    if ( w(j) /= 0.0D+00 ) then
      if ( nf < m ) then
        nf = nf + 1
        xf(nf) = x(j)
        wf(nf) = w(j)
      end if
    end if
  end do

  return
end
subroutine line_fekete_monomial ( m, a, b, n, x, nf, xf, wf )

!*****************************************************************************80
!
!! LINE_FEKETE_MONOMIAL computes approximate Fekete points in an interval [A,B].
!
!  Discussion:
!
!    We use the uniform weight and the monomial basis:
!
!      P(j) = x^(j-1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Alvise Sommariva, Marco Vianello,
!    Computing approximate Fekete points by QR factorizations of Vandermonde 
!    matrices,
!    Computers and Mathematics with Applications,
!    Volume 57, 2009, pages 1324-1336.
!    
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of basis polynomials.
!
!    Input, real ( kind = 8 ) A, B, the endpoints of the interval.
!
!    Input, integer ( kind = 4 ) N, the number of sample points.
!    M <= N.
!
!    Input, real ( kind = 8 ) X(N), the coordinates of the sample points.
!
!    Output, integer ( kind = 4 ) NF, the number of Fekete points.
!    If the computation is successful, NF = M.
!
!    Output, real ( kind = 8 ) XF(NF), the coordinates of the Fekete points.
!
!    Output, real ( kind = 8 ) WF(NF), the weights of the Fekete points.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) mom(m)
  integer ( kind = 4 ) nf
  real ( kind = 8 ) v(m,n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) wf(m)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xf(m)

  if ( n < m ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LINE_FEKETE_MONOMIAL - Fatal error!'
    write ( *, '(a)' ) '  N < M.'
    stop 1
  end if
!
!  Form the moments.
!
  call line_monomial_moments ( a, b, m, mom )
!
!  Form the rectangular Vandermonde matrix V for the polynomial basis.
!
  v(1,1:n) = 1.0D+00
  do i = 2, m
    v(i,1:n) = v(i-1,1:n) * x(1:n)
  end do
!
!  Solve the system for the weights W.
!
  call qr_solve ( m, n, v, mom, w )
!
!  Extract the data associated with the nonzero weights.
!
  nf = 0
  do j = 1, n
    if ( w(j) /= 0.0D+00 ) then
      if ( nf < m ) then
        nf = nf + 1
        xf(nf) = x(j)
        wf(nf) = w(j)
      end if
    end if
  end do

  return
end
subroutine line_monomial_moments ( a, b, m, mom )

!*****************************************************************************80
!
!! LINE_MONOMIAL_MOMENTS computes monomial moments in [A,B].
!
!  Discussion:
!
!    We use the uniform weight and the shifted and scaled monomial basis:
!
!      P(a,b,i;x) = xi(a,b;x)^(i-1)
!       xi(a,b;x) = ( - ( b - x ) + ( x - a ) ) / ( b - a )
!
!    The i-th moment is
!
!      mom(i) = integral ( a <= x <= b ) P(a,b,i;x) dx
!             = integral ( a <= x <= b ) xi(a,b;x)^(i-1) dx
!             = 0.5 * ( b - a ) * integral ( -1 <= xi <= +1 ) xi^(i-1) dxi
!             = 0.5 * ( b - a ) * xi^i / i | ( -1 <= xi <= +1 )
!             = 0.5 * ( b - a ) * ( 1 - (-1)^i ) / i
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the endpoints of the interval.
!
!    Input, integer ( kind = 4 ) M, the number of basis polynomials.
!
!    Output, real ( kind = 8 ) MOM(M), the moments.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mom(m)

  do i = 1, m
    mom(i) = ( b - a ) * real ( mod ( i, 2 ), kind = 8 ) / real ( i, kind = 8 )
  end do

  return
end

