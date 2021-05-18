subroutine jacobi_eigenvalue ( n, a, it_max, v, d, it_num, rot_num )

!*****************************************************************************80
!
!! JACOBI_EIGENVALUE carries out the Jacobi eigenvalue iteration.
!
!  Discussion:
!
!    This function computes the eigenvalues and eigenvectors of a
!    real symmetric matrix, using Rutishauser's modfications of the classical
!    Jacobi rotation method with threshold pivoting. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix, which must be square, real,
!    and symmetric.
!
!    Input, integer ( kind = 4 ) IT_MAX, the maximum number of iterations.
!
!    Output, real ( kind = 8 ) V(N,N), the matrix of eigenvectors.
!
!    Output, real ( kind = 8 ) D(N), the eigenvalues, in descending order.
!
!    Output, integer ( kind = 4 ) IT_NUM, the total number of iterations.
!
!    Output, integer ( kind = 4 ) ROT_NUM, the total number of rotations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) bw(n)
  real ( kind = 8 ) c
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) g
  real ( kind = 8 ) gapq
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) p
  integer ( kind = 4 ) q
  integer ( kind = 4 ) rot_num
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) tau
  real ( kind = 8 ) term
  real ( kind = 8 ) termp
  real ( kind = 8 ) termq
  real ( kind = 8 ) theta
  real ( kind = 8 ) thresh
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) zw(n)

  do j = 1, n
    do i = 1, n
      v(i,j) = 0.0D+00
    end do
    v(j,j) = 1.0D+00
  end do

  do i = 1, n
    d(i) = a(i,i)
  end do

  bw(1:n) = d(1:n)
  zw(1:n) = 0.0D+00
  it_num = 0
  rot_num = 0

  do while ( it_num < it_max )

    it_num = it_num + 1
!
!  The convergence threshold is based on the size of the elements in
!  the strict upper triangle of the matrix.
!
    thresh = 0.0D+00
    do j = 1, n
      do i = 1, j - 1
        thresh = thresh + a(i,j) ** 2
      end do
    end do

    thresh = sqrt ( thresh ) / real ( 4 * n, kind = 8 )

    if ( thresh == 0.0D+00 ) then
      exit 
    end if

    do p = 1, n
      do q = p + 1, n

        gapq = 10.0D+00 * abs ( a(p,q) )
        termp = gapq + abs ( d(p) )
        termq = gapq + abs ( d(q) )
!
!  Annihilate tiny offdiagonal elements.
!
        if ( 4 < it_num .and. &
             termp == abs ( d(p) ) .and. &
             termq == abs ( d(q) ) ) then

          a(p,q) = 0.0D+00
!
!  Otherwise, apply a rotation.
!
        else if ( thresh <= abs ( a(p,q) ) ) then

          h = d(q) - d(p)
          term = abs ( h ) + gapq

          if ( term == abs ( h ) ) then
            t = a(p,q) / h
          else
            theta = 0.5D+00 * h / a(p,q)
            t = 1.0D+00 / ( abs ( theta ) + sqrt ( 1.0D+00 + theta * theta ) )
            if ( theta < 0.0D+00 ) then 
              t = - t
            end if
          end if

          c = 1.0D+00 / sqrt ( 1.0D+00 + t * t )
          s = t * c
          tau = s / ( 1.0D+00 + c )
          h = t * a(p,q)
!
!  Accumulate corrections to diagonal elements.
!
          zw(p) = zw(p) - h                  
          zw(q) = zw(q) + h
          d(p) = d(p) - h
          d(q) = d(q) + h

          a(p,q) = 0.0D+00
!
!  Rotate, using information from the upper triangle of A only.
!
          do j = 1, p - 1
            g = a(j,p)
            h = a(j,q)
            a(j,p) = g - s * ( h + g * tau )
            a(j,q) = h + s * ( g - h * tau )
          end do

          do j = p + 1, q - 1
            g = a(p,j)
            h = a(j,q)
            a(p,j) = g - s * ( h + g * tau )
            a(j,q) = h + s * ( g - h * tau )
          end do

          do j = q + 1, n
            g = a(p,j)
            h = a(q,j)
            a(p,j) = g - s * ( h + g * tau )
            a(q,j) = h + s * ( g - h * tau )
          end do
!
!  Accumulate information in the eigenvector matrix.
!
          do j = 1, n
            g = v(j,p)
            h = v(j,q)
            v(j,p) = g - s * ( h + g * tau )
            v(j,q) = h + s * ( g - h * tau )
          end do

          rot_num = rot_num + 1

        end if

      end do
    end do

    bw(1:n) = bw(1:n) + zw(1:n)
    d(1:n) = bw(1:n)
    zw(1:n) = 0.0D+00

  end do
!
!  Restore upper triangle of input matrix.
!
  do j = 1, n
    do i = 1, j - 1
      a(i,j) = a(j,i)
    end do
  end do
!
!  Ascending sort the eigenvalues and eigenvectors.
!
  do k = 1, n - 1

    m = k

    do l = k + 1, n
      if ( d(l) < d(m) ) then
        m = l
      end if
    end do

    if ( m /= k ) then

      t    = d(m)
      d(m) = d(k)
      d(k) = t

      w(1:n)   = v(1:n,m)
      v(1:n,m) = v(1:n,k)
      v(1:n,k) = w(1:n)

    end if

  end do

  return
end
subroutine moment_method ( n, moment, x, w )

!*****************************************************************************80
!
!! MOMENT_METHOD computes a quadrature rule by the method of moments.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Gene Golub, John Welsch,
!    Calculation of Gaussian Quadrature Rules,
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 221-230.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the quadrature rule.
!
!    Input, real ( kind = 8 ) MOMENT(2*N+1), moments 0 through 2*N.
!
!    Output, real ( kind = 8 ) X(N), W(N), the points and weights of the 
!    quadrature rule.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ), allocatable :: alpha(:)
  real ( kind = 8 ), allocatable :: beta(:)
  logical debug
  integer ( kind = 4 ) flag
  real ( kind = 8 ), allocatable :: h(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) j
  real ( kind = 8 ), allocatable :: jacobi(:,:)
  real ( kind = 8 ) moment(0:2*n)
  real ( kind = 8 ), allocatable :: r(:,:)
  integer ( kind = 4 ) rot_num
  real ( kind = 8 ), allocatable :: v(:,:)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  debug = .false.

  if ( debug ) then
    call r8vec_print ( 2 * n + 1, moment, '  Moments:' )
  end if
!
!  Define the N+1 by N+1 Hankel matrix H(I,J) = moment(I+J).
!
  allocate ( h(0:n,0:n) )

  do i = 0, n
    do j = 0, n
      h(i,j) = moment(i+j);
    end do
  end do

  if ( debug ) then
    call r8mat_print ( n + 1, n + 1, h, '  Hankel matrix:' )
  end if
!
!  Compute R, the upper triangular Cholesky factor of H.
!
  allocate ( r(1:n+1,1:n+1) )

  call r8mat_cholesky_factor_upper ( n + 1, h, r, flag )

  if ( flag /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'QUADMOM_PRB01 - Fatal error!'
    write ( *, '(a,i6)' ) '  R8MAT_CHOLESKY_FACTOR_UPPER returned FLAG = ', flag
    stop 1
  end if
!
!  Compute ALPHA and BETA from R, using Golub and Welsch's formula.
!
  allocate ( alpha(1:n) )

  alpha(1) = r(1,2) / r(1,1)
  do i = 2, n
    alpha(i) = r(i,i+1) / r(i,i) - r(i-1,i) / r(i-1,i-1)
  end do

  allocate ( beta(1:n-1) )

  do i = 1, n - 1
    beta(i) = r(i+1,i+1) / r(i,i)
  end do
!
!  Compute the points and weights from the moments.
!
  allocate ( jacobi(1:n,1:n) )

  jacobi(1:n,1:n) = 0.0D+00

  do i = 1, n
    jacobi(i,i) = alpha(i)
  end do

  do i = 1, n - 1
    jacobi(i,i+1) = beta(i)
    jacobi(i+1,i) = beta(i)
  end do

  if ( debug ) then
    call r8mat_print ( n, n, jacobi, '  The Jacobi matrix:' )
  end if
!
!  Get the eigendecomposition of the Jacobi matrix.
!
  it_max = 100
  allocate ( v(1:n,1:n) )

  call jacobi_eigenvalue ( n, jacobi, it_max, v, x, it_num, rot_num )

  if ( debug ) then
    call r8mat_print ( n, n, v, '  Eigenvector' )
  end if

  w(1:n) = moment(0) * v(1,1:n) ** 2
!
!  Free memory.
!
  deallocate ( alpha )
  deallocate ( beta )
  deallocate ( h )
  deallocate ( jacobi )
  deallocate ( r )
  deallocate ( v )

  return
end
subroutine moments_laguerre ( m, w )

!*****************************************************************************80
!
!! MOMENTS_LAGUERRE returns moments of the Laguerre distribution.
!
!  Discussion:
!
!    pdf(x) = exp ( -x )
!    mu(k) = integral ( 0 <= x < +oo ) x^k pdf(x) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of moments desired.
!
!    Output, real ( kind = 8 ) W(0:M-1), the weighted integrals of X^0 
!    through X^(M-1).
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_factorial
  real ( kind = 8 ) w(0:m-1)

  do k = 0, m - 1
    w(k) = r8_factorial ( k )
  end do

  return
end
subroutine moments_legendre ( m, a, b, w )

!*****************************************************************************80
!
!! MOMENTS_LEGENDRE returns moments of the Legendre weight on [A,B].
!
!  Discussion:
!
!    mu(k) = integral ( a <= x <= b ) x^k dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of moments desired.
!
!    Input, real ( kind = 8 ) A, B, the left and right endpoints 
!    of the interval.
!
!    Output, real ( kind = 8 ) W(0:M-1), the weighted integrals of X^0 
!    through X^(M-1).
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) a
  real ( kind = 8 ) ak
  real ( kind = 8 ) b
  real ( kind = 8 ) bk
  integer ( kind = 4 ) k
  real ( kind = 8 ) w(0:m-1)

  bk = 1.0D+00
  ak = 1.0D+00
  do k = 0, m - 1
    bk = bk * b
    ak = ak * a
    w(k) = ( bk - ak ) / real ( k + 1, kind = 8 )
  end do

  return
end
subroutine moments_normal_01 ( m, w )

!*****************************************************************************80
!
!! MOMENTS_NORMAL_01 returns moments of the standard Normal distribution.
!
!  Discussion:
!
!    pdf(x) = exp ( -x^2/2 ) / sqrt ( pi * 2 )
!    mu(k) = integral ( -oo < x < +oo ) x^k pdf(x) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of moments desired.
!
!    Output, real ( kind = 8 ) W(0:M-1), the weighted integrals of X^0 
!    through X^(M-1).
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ) w(0:m-1)

  w(0) = 1.0D+00

  do k = 2, m - 1, 2
    w(k) = r8_factorial2 ( k - 1 )
  end do

  do k = 1, m - 1, 2
    w(k) = 0.0D+00
  end do

  return
end
subroutine moments_normal ( m, mu, sigma, w )

!*****************************************************************************80
!
!! MOMENTS_NORMAL returns moments of the standard Normal distribution.
!
!  Discussion:
!
!    pdf(x) = exp ( -((x-mu)/sigma)^2/2 ) / sigma / sqrt ( pi * 2 )
!    mu(k) = integral ( -oo < x < +oo ) x^k pdf(x) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of moments desired.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!
!    Output, real ( kind = 8 ) W(0:M-1), the weighted integrals of X^0 
!    through X^(M-1).
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) j
  integer ( kind = 4 ) j_hi
  integer ( kind = 4 ) k
  real ( kind = 8 ) mu
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ) sigma
  real ( kind = 8 ) t
  real ( kind = 8 ) w(0:m-1)

  do k = 0, m - 1
    t = 0.0D+00
    j_hi = k / 2
    do j = 0, j_hi
      t = t + r8_choose ( k, 2 * j ) * r8_factorial2 ( 2 * j - 1 ) &
        * sigma ** ( 2 * j ) * mu ** ( k - 2 * j )
    end do
    w(k) = t
  end do

  return
end
subroutine moments_truncated_normal_ab ( m, mu, sigma, a, b, w )

!*****************************************************************************80
!
!! MOMENTS_TRUNCATED_NORMAL_AB: moments of truncated Normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of moments desired.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) W(0:M-1), the weighted integrals of X^0 
!    through X^(M-1).
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) sigma
  real ( kind = 8 ) w(0:m-1)

  do order = 0, m - 1
    call truncated_normal_ab_moment ( order, mu, sigma, a, b, w(order) )
  end do

  return
end
subroutine moments_truncated_normal_a ( m, mu, sigma, a, w )

!*****************************************************************************80
!
!! MOMENTS_TRUNCATED_NORMAL_A: moments of lower truncated Normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of moments desired.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) W(0:M-1), the weighted integrals of X^0 
!    through X^(M-1).
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) a
  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) sigma
  real ( kind = 8 ) w(0:m-1)

  do order = 0, m - 1
    call truncated_normal_a_moment ( order, mu, sigma, a, w(order) )
  end do

  return
end
subroutine moments_truncated_normal_b ( m, mu, sigma, b, w )

!*****************************************************************************80
!
!! MOMENTS_TRUNCATED_NORMAL_B: moments of upper truncated Normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of moments desired.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) W(0:M-1), the weighted integrals of X^0 
!    through X^(M-1).
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) b
  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) sigma
  real ( kind = 8 ) w(0:m-1)

  do order = 0, m - 1
    call truncated_normal_b_moment ( order, mu, sigma, b, w(order) )
  end do

  return
end
subroutine normal_01_cdf ( x, cdf )

!*****************************************************************************80
!
!! NORMAL_01_CDF evaluates the Normal 01 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    AG Adams,
!    Algorithm 39,
!    Areas Under the Normal Curve,
!    Computer Journal,
!    Volume 12, pages 197-198, 1969.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ), parameter :: a1 = 0.398942280444D+00
  real ( kind = 8 ), parameter :: a2 = 0.399903438504D+00
  real ( kind = 8 ), parameter :: a3 = 5.75885480458D+00
  real ( kind = 8 ), parameter :: a4 = 29.8213557808D+00
  real ( kind = 8 ), parameter :: a5 = 2.62433121679D+00
  real ( kind = 8 ), parameter :: a6 = 48.6959930692D+00
  real ( kind = 8 ), parameter :: a7 = 5.92885724438D+00
  real ( kind = 8 ), parameter :: b0 = 0.398942280385D+00
  real ( kind = 8 ), parameter :: b1 = 3.8052D-08
  real ( kind = 8 ), parameter :: b2 = 1.00000615302D+00
  real ( kind = 8 ), parameter :: b3 = 3.98064794D-04
  real ( kind = 8 ), parameter :: b4 = 1.98615381364D+00
  real ( kind = 8 ), parameter :: b5 = 0.151679116635D+00
  real ( kind = 8 ), parameter :: b6 = 5.29330324926D+00
  real ( kind = 8 ), parameter :: b7 = 4.8385912808D+00
  real ( kind = 8 ), parameter :: b8 = 15.1508972451D+00
  real ( kind = 8 ), parameter :: b9 = 0.742380924027D+00
  real ( kind = 8 ), parameter :: b10 = 30.789933034D+00
  real ( kind = 8 ), parameter :: b11 = 3.99019417011D+00
  real ( kind = 8 ) cdf
  real ( kind = 8 ) q
  real ( kind = 8 ) x
  real ( kind = 8 ) y
!
!  |X| <= 1.28.
!
  if ( abs ( x ) <= 1.28D+00 ) then

    y = 0.5D+00 * x * x

    q = 0.5D+00 - abs ( x ) * ( a1 - a2 * y / ( y + a3 - a4 / ( y + a5 &
      + a6 / ( y + a7 ) ) ) )
!
!  1.28 < |X| <= 12.7
!
  else if ( abs ( x ) <= 12.7D+00 ) then

    y = 0.5D+00 * x * x

    q = exp ( - y ) * b0 / ( abs ( x ) - b1 &
      + b2 / ( abs ( x ) + b3 &
      + b4 / ( abs ( x ) - b5 &
      + b6 / ( abs ( x ) + b7 &
      - b8 / ( abs ( x ) + b9 &
      + b10 / ( abs ( x ) + b11 ) ) ) ) ) )
!
!  12.7 < |X|
!
  else

    q = 0.0D+00

  end if
!
!  Take account of negative X.
!
  if ( x < 0.0D+00 ) then
    cdf = q
  else
    cdf = 1.0D+00 - q
  end if

  return
end
subroutine normal_01_pdf ( x, pdf )

!*****************************************************************************80
!
!! NORMAL_01_PDF evaluates the Normal 01 PDF.
!
!  Discussion:
!
!    The Normal 01 PDF is also called the "Standard Normal" PDF, or
!    the Normal PDF with 0 mean and variance 1.
!
!    PDF(X) = exp ( - 0.5 * X^2 ) / sqrt ( 2 * PI )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  pdf = exp ( -0.5D+00 * x * x ) / sqrt ( 2.0D+00 * pi )

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
function r8_factorial ( n )

!*****************************************************************************80
!
!! R8_FACTORIAL computes the factorial of N.
!
!  Discussion:
!
!    factorial ( N ) = product ( 1 <= I <= N ) I
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the argument of the factorial function.
!    If N is less than 1, the function value is returned as 1.
!
!    Output, real ( kind = 8 ) R8_FACTORIAL, the factorial of N.
!
  implicit none

  real ( kind = 8 ) r8_factorial
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n

  r8_factorial = 1.0D+00

  do i = 1, n
    r8_factorial = r8_factorial * real ( i, kind = 8 )
  end do

  return
end
function r8_factorial2 ( n )

!*****************************************************************************80
!
!! R8_FACTORIAL2 computes the double factorial function.
!
!  Discussion:
!
!    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
!                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
!
!  Example:
!
!     N Value
!
!     0     1
!     1     1
!     2     2
!     3     3
!     4     8
!     5    15
!     6    48
!     7   105
!     8   384
!     9   945
!    10  3840
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the argument of the double factorial
!    function.  If N is less than 1, the value is returned as 1.0.
!
!    Output, real ( kind = 8 ) R8_FACTORIAL2, the value.
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ) r8_n

  if ( n < 1 ) then
    r8_factorial2 = 1.0D+00
    return
  end if

  r8_n = real ( n, kind = 8 )
  r8_factorial2 = 1.0D+00

  do while ( 1.0D+00 < r8_n )
    r8_factorial2 = r8_factorial2 * r8_n
    r8_n = r8_n - 2.0D+00
  end do

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

  if ( mod ( i, 2 ) == 0 ) then
    r8_mop = + 1.0D+00
  else
    r8_mop = - 1.0D+00
  end if

  return
end
subroutine r8mat_cholesky_factor_upper ( n, a, c, flag )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_FACTOR_UPPER: upper Cholesky factor of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    For a positive semidefinite symmetric matrix A, the Cholesky factorization
!    is an upper triangular matrix R such that:
!
!      A = R * R'
!
!    The lower Cholesky factor is a lower triangular matrix L such that
!
!      A = L * L'
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix A.
!
!    Input, real ( kind = 8 ) A(N,N), the N by N matrix.
!
!    Output, real ( kind = 8 ) C(N,N), the N by N upper triangular
!    Cholesky factor.
!
!    Output, integer ( kind = 4 ) FLAG:
!    0, no error occurred.
!    1, the matrix is not positive definite.
!    2, the matrix is not nonnegative definite.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c(n,n)
  integer ( kind = 4 ) flag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) sum2

  flag = 0

  c(1:n,1:n) = a(1:n,1:n)

  do j = 1, n

    c(j,1:j-1) = 0.0D+00

    do i = j, n

      sum2 = c(i,j) - dot_product ( c(1:j-1,j), c(1:j-1,i) )

      if ( i == j ) then
        if ( sum2 <= 0.0D+00 ) then
          flag = 1
          return
        else
          c(j,i) = sqrt ( sum2 )
        end if
      else
        if ( c(j,j) /= 0.0D+00 ) then
          c(j,i) = sum2 / c(j,j)
        else
          c(j,i) = 0.0D+00
        end if
      end if

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
subroutine truncated_normal_ab_moment ( order, mu, s, a, b, moment )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_MOMENT: moments of the truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Phoebus Dhrymes,
!    Moments of Truncated Normal Distributions,
!    May 2005.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the moment.
!    0 <= ORDER.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!    0.0 < S.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!    A < B.
!
!    Output, real ( kind = 8 ) MOMENT, the moment of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a_h
  real ( kind = 8 ) a_cdf
  real ( kind = 8 ) a_pdf
  real ( kind = 8 ) b
  real ( kind = 8 ) b_h
  real ( kind = 8 ) b_cdf
  real ( kind = 8 ) b_pdf
  real ( kind = 8 ) ir
  real ( kind = 8 ) irm1
  real ( kind = 8 ) irm2
  real ( kind = 8 ) moment
  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  integer ( kind = 4 ) r
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) s

  if ( order < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  ORDER < 0.'
    stop 1
  end if

  if ( s <= 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  S <= 0.0.'
    stop 1
  end if

  if ( b <= a ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  B <= A.'
    stop 1
  end if

  a_h = ( a - mu ) / s
  call normal_01_pdf ( a_h, a_pdf )
  call normal_01_cdf ( a_h, a_cdf )

  if ( a_cdf == 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  PDF/CDF ratio fails, because A_CDF is too small.'
    write ( *, '(a,g14.6)' ) '  A_PDF = ', a_pdf
    write ( *, '(a,g14.6)' ) '  A_CDF = ', a_cdf
    stop 1
  end if

  b_h = ( b - mu ) / s
  call normal_01_pdf ( b_h, b_pdf )
  call normal_01_cdf ( b_h, b_cdf )

  if ( b_cdf == 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  PDF/CDF ratio fails, because B_CDF is too small.'
    write ( *, '(a,g14.6)' ) '  B_PDF = ', b_pdf
    write ( *, '(a,g14.6)' ) '  B_CDF = ', b_cdf
    stop 1
  end if

  moment = 0.0D+00
  irm2 = 0.0D+00
  irm1 = 0.0D+00

  do r = 0, order

    if ( r == 0 ) then
      ir = 1.0D+00
    else if ( r == 1 ) then
      ir = - ( b_pdf - a_pdf ) / ( b_cdf - a_cdf )
    else
      ir = real ( r - 1, kind = 8 ) * irm2 &
        - ( b_h ** ( r - 1 ) * b_pdf - a_h ** ( r - 1 ) * a_pdf ) &
        / ( b_cdf - a_cdf )
    end if

    moment = moment + r8_choose ( order, r ) * mu ** ( order - r ) &
      * ( s ** r ) * ir

    irm2 = irm1
    irm1 = ir

  end do

  return
end
subroutine truncated_normal_a_moment ( order, mu, s, a, moment )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_MOMENT: moments of the lower truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Phoebus Dhrymes,
!    Moments of Truncated Normal Distributions,
!    May 2005.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the moment.
!    0 <= ORDER.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!    0.0 < S.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) MOMENT, the moment of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) moment
  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) r8_mop
  real ( kind = 8 ) s

  call truncated_normal_b_moment ( order, - mu, s, - a, moment )
  moment = r8_mop ( order ) * moment

  return
end
subroutine truncated_normal_b_moment ( order, mu, s, b, moment )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_MOMENT: moments of the upper truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Phoebus Dhrymes,
!    Moments of Truncated Normal Distributions,
!    May 2005.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the moment.
!    0 <= ORDER.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!    0.0 < S.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) MOMENT, the moment of the PDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) f
  real ( kind = 8 ) h
  real ( kind = 8 ) h_cdf
  real ( kind = 8 ) h_pdf
  real ( kind = 8 ) ir
  real ( kind = 8 ) irm1
  real ( kind = 8 ) irm2
  real ( kind = 8 ) moment
  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  integer ( kind = 4 ) r
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) s

  if ( order < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  ORDER < 0.'
    stop 1
  end if

  h = ( b - mu ) / s
  call normal_01_pdf ( h, h_pdf )
  call normal_01_cdf ( h, h_cdf )

  if ( h_cdf == 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MOMENT - Fatal error!'
    write ( *, '(a)' ) '  CDF((B-MU)/S) = 0.'
    stop 1
  end if

  f = h_pdf / h_cdf

  moment = 0.0D+00
  irm2 = 0.0D+00
  irm1 = 0.0D+00

  do r = 0, order

    if ( r == 0 ) then
      ir = 1.0D+00
    else if ( r == 1 ) then
      ir = - f
    else
      ir = - h ** ( r - 1 ) * f + real ( r - 1, kind = 8 ) * irm2
    end if

    moment = moment + r8_choose ( order, r ) * mu ** ( order - r ) &
      * ( s ** r ) * ir

    irm2 = irm1
    irm1 = ir

  end do

  return
end
