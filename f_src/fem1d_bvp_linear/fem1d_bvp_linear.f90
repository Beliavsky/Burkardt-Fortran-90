subroutine fem1d_bvp_linear ( n, a, c, f, x, u )

!*****************************************************************************80
!
!! FEM1D_BVP_LINEAR solves a two point boundary value problem.
!
!  Location:
!
!    http://people.sc.fsu.edu/~jburkardt/f_src/fem1d_bvp_linear/fem1d_bvp_linear.f90
!
!  Discussion:
!
!    The program uses the finite element method, with piecewise linear basis
!    functions to solve a boundary value problem in one dimension.
!
!    The problem is defined on the region 0 <= x <= 1.
!
!    The following differential equation is imposed between 0 and 1:
!
!      - d/dx a(x) du/dx + c(x) * u(x) = f(x)
!
!    where a(x), c(x), and f(x) are given functions.
!
!    At the boundaries, the following conditions are applied:
!
!      u(0.0) = 0.0
!      u(1.0) = 0.0
!
!    A set of N equally spaced nodes is defined on this
!    interval, with 0 = X(1) < X(2) < ... < X(N) = 1.0.
!
!    At each node I, we associate a piecewise linear basis function V(I,X),
!    which is 0 at all nodes except node I.  This implies that V(I,X) is
!    everywhere 0 except that
!
!    for X(I-1) <= X <= X(I):
!
!      V(I,X) = ( X - X(I-1) ) / ( X(I) - X(I-1) ) 
!
!    for X(I) <= X <= X(I+1):
!
!      V(I,X) = ( X(I+1) - X ) / ( X(I+1) - X(I) )
!
!    We now assume that the solution U(X) can be written as a linear
!    sum of these basis functions:
!
!      U(X) = sum ( 1 <= J <= N ) U(J) * V(J,X)
!
!    where U(X) on the left is the function of X, but on the right,
!    is meant to indicate the coefficients of the basis functions.
!
!    To determine the coefficient U(J), we multiply the original
!    differential equation by the basis function V(J,X), and use
!    integration by parts, to arrive at the I-th finite element equation:
!
!        Integral A(X) * U'(X) * V'(I,X) + C(X) * U(X) * V(I,X) dx 
!      = Integral F(X) * V(I,X) dx
!
!    We note that the functions U(X) and U'(X) can be replaced by
!    the finite element form involving the linear sum of basis functions,
!    but we also note that the resulting integrand will only be nonzero
!    for terms where J = I - 1, I, or I + 1.
!
!    By writing this equation for basis functions I = 2 through N - 1,
!    and using the boundary conditions, we have N linear equations
!    for the N unknown coefficients U(1) through U(N), which can
!    be easily solved.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Input, function A ( X ), evaluates a(x);
!
!    Input, function C ( X ), evaluates c(x);
!
!    Input, function F ( X ), evaluates f(x);
!
!    Input, real ( kind = 8 ) X(N), the mesh points.
!
!    Output, real ( kind = 8 ) U(N), the finite element coefficients, which 
!    are also the value of the computed solution at the mesh points.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: quad_num = 2

  real ( kind = 8 ), external :: a
  real ( kind = 8 ) abscissa(quad_num)
  real ( kind = 8 ) amat(n,n)
  real ( kind = 8 ) axq
  real ( kind = 8 ) b(n)
  real ( kind = 8 ), external :: c
  real ( kind = 8 ) cxq
  integer ( kind = 4 ) e
  integer ( kind = 4 ) e_num
  real ( kind = 8 ), external :: f
  real ( kind = 8 ) fxq
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) l
  integer ( kind = 4 ) q
  integer ( kind = 4 ) r
  real ( kind = 8 ) weight(quad_num)
  real ( kind = 8 ) wq
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) vl
  real ( kind = 8 ) vlp
  real ( kind = 8 ) vr
  real ( kind = 8 ) vrp
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xl
  real ( kind = 8 ) xq
  real ( kind = 8 ) xr
!
!  Quadrature definitions.
!
  abscissa(1) = -0.577350269189625764509148780502D+00
  abscissa(2) = +0.577350269189625764509148780502D+00
  weight(1) = 1.0D+00
  weight(2) = 1.0D+00
!
!  Zero out the matrix and right hand side.
!
  amat(1:n,1:n) = 0.0D+00
  b(1:n) = 0.0D+00

  e_num = n - 1

  do e = 1, e_num

    l = e
    r = e + 1

    xl = x(l)
    xr = x(r)

    do q = 1, quad_num

      xq = ( ( 1.0D+00 - abscissa(q) ) * xl   &
           + ( 1.0D+00 + abscissa(q) ) * xr ) &
           /   2.0D+00

      wq = weight(q) * ( xr - xl ) / 2.0D+00

      vl =  ( xr - xq ) / ( xr - xl )
      vlp =  - 1.0D+00  / ( xr - xl )

      vr =  ( xq - xl ) / ( xr - xl )
      vrp =  + 1.0D+00  / ( xr - xl )

      axq = a ( xq )
      cxq = c ( xq )
      fxq = f ( xq )

      amat(l,l) = amat(l,l) + wq * ( vlp * axq * vlp + vl * cxq * vl )
      amat(l,r) = amat(l,r) + wq * ( vlp * axq * vrp + vl * cxq * vr )
      b(l)      = b(l)      + wq * ( vl * fxq )

      amat(r,l) = amat(r,l) + wq * ( vrp * axq * vlp + vr * cxq * vl )
      amat(r,r) = amat(r,r) + wq * ( vrp * axq * vrp + vr * cxq * vr )
      b(r)      = b(r)      + wq * ( vr * fxq )

    end do

  end do
!
!  Equation 1 is the left boundary condition, U(0.0) = 0.0;
!
  amat(1,1:n) = 0.0D+00
  b(1) = 0.0D+00
  b(2:n) = b(2:n) - amat(2:n,1) * b(1)
  amat(1:n,1) = 0.0D+00
  amat(1,1) = 1.0D+00
!
!  Equation N is the right boundary condition, U(1.0) = 0.0;
!
  amat(n,1:n) = 0.0D+00
  b(n) = 0.0D+00
  b(1:n-1) = b(1:n-1) - amat(1:n-1,1) * b(n)
  amat(1:n,n) = 0.0D+00
  amat(n,n) = 1.0D+00
!
!  Solve the linear system.
!
  call r8mat_solve2 ( n, amat, b, u, ierror )

  return
end
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
subroutine h1s_error_linear ( n, x, u, exact_ux, h1s )

!*****************************************************************************80
!
!! H1S_ERROR_LINEAR: seminorm error of a finite element solution.
!
!  Discussion:
!
!    We assume the finite element method has been used, over an interval [A,B]
!    involving N nodes, with piecewise linear elements used for the basis.
!    The coefficients U(1:N) have been computed, and a formula for the
!    exact derivative is known.
!
!    This function estimates the seminorm of the error:
!
!      SEMINORM = Integral ( A <= X <= B ) ( dU(X)/dx - EXACT_UX(X) )^2 dX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Input, real ( kind = 8 ) X(N), the mesh points.
!
!    Input, real ( kind = 8 ) U(N), the finite element coefficients.
!
!    Input, function EQ = EXACT_UX ( X ), returns the value of the exact
!    derivative at the point X.
!
!    Output, real ( kind = 8 ) H1S, the estimated seminorm of 
!    the error.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) abscissa(2)
  real ( kind = 8 ), external :: exact_ux
  real ( kind = 8 ) exq
  real ( kind = 8 ) h1s
  integer ( kind = 4 ) i
  integer ( kind = 4 ) q
  integer ( kind = 4 ) quad_num
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) ul
  real ( kind = 8 ) ur
  real ( kind = 8 ) uxq
  real ( kind = 8 ) weight(2)
  real ( kind = 8 ) wq
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xl
  real ( kind = 8 ) xq
  real ( kind = 8 ) xr

  h1s = 0.0D+00
!
!  Quadrature definitions.
!
  quad_num = 2
  abscissa(1) = -0.577350269189625764509148780502D+00
  abscissa(2) = +0.577350269189625764509148780502D+00
  weight(1) = 1.0D+00
  weight(2) = 1.0D+00
!
!  Integrate over each interval.
!
  do i = 1, n - 1

    xl = x(i)
    xr = x(i+1)
    ul = u(i)
    ur = u(i+1)

    do q = 1, quad_num

      xq = ( ( 1.0D+00 - abscissa(q) ) * xl   &
           + ( 1.0D+00 + abscissa(q) ) * xr ) &
           /   2.0D+00

      wq = weight(q) * ( xr - xl ) / 2.0D+00
!
!  The piecewise linear derivative is a constant in the interval.
!
      uxq = ( ur - ul ) / ( xr - xl )

      exq = exact_ux ( xq )
 
      h1s = h1s + wq * ( uxq - exq )**2

    end do

  end do

  h1s = sqrt ( h1s )

  return
end
subroutine l1_error ( n, x, u, exact, e1 )

!*****************************************************************************80
!
!! L1_ERROR estimates the l1 error norm of a finite element solution.
!
!  Discussion:
!
!    We assume the finite element method has been used, over an interval [A,B]
!    involving N nodes.
!
!    The coefficients U(1:N) have been computed, and a formula for the
!    exact solution is known.
!
!    This function estimates the little l1 norm of the error:
!      L1_NORM = sum ( 1 <= I <= N ) abs ( U(i) - EXACT(X(i)) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Input, real ( kind = 8 ) X(N), the mesh points.
!
!    Input, real ( kind = 8 ) U(N), the finite element coefficients.
!
!    Input, function EQ = EXACT ( X ), returns the value of the exact
!    solution at the point X.
!
!    Output, real ( kind = 8 ) E1, the estimated L2 norm of the error.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) e1
  real ( kind = 8 ), external :: exact
  integer ( kind = 4 ) i
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) x(n)

  e1 = 0.0D+0
  do i = 1, n
    e1 = e1 + abs ( u(i) - exact ( x(i) ) )
  end do

  e1 = e1 / real ( n, kind = 8 )

  return
end
subroutine l2_error_linear ( n, x, u, exact, e2 )

!*****************************************************************************80
!
!! L2_ERROR_LINEAR estimates the L2 error norm of a finite element solution.
!
!  Discussion:
!
!    We assume the finite element method has been used, over an interval [A,B]
!    involving N nodes, with piecewise linear elements used for the basis.
!
!    The coefficients U(1:N) have been computed, and a formula for the
!    exact solution is known.
!
!    This function estimates the L2 norm of the error:
!
!      L2_NORM = Integral ( A <= X <= B ) ( U(X) - EXACT(X) )^2 dX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Input, real ( kind = 8 ) X(N), the mesh points.
!
!    Input, real ( kind = 8 ) U(N), the finite element coefficients.
!
!    Input, function EQ = EXACT ( X ), returns the value of the exact
!    solution at the point X.
!
!    Output, real ( kind = 8 ) E2, the estimated L2 norm of the error.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) abscissa(2)
  real ( kind = 8 ) e2
  real ( kind = 8 ) eq
  real ( kind = 8 ), external :: exact
  integer ( kind = 4 ) i
  integer ( kind = 4 ) q
  integer ( kind = 4 ) quad_num
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) ul
  real ( kind = 8 ) ur
  real ( kind = 8 ) uq
  real ( kind = 8 ) weight(2)
  real ( kind = 8 ) wq
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xl
  real ( kind = 8 ) xq
  real ( kind = 8 ) xr

  e2 = 0.0D+00
!
!  Quadrature definitions.
!
  quad_num = 2
  abscissa(1) = -0.577350269189625764509148780502D+00
  abscissa(2) = +0.577350269189625764509148780502D+00
  weight(1) = 1.0D+00
  weight(2) = 1.0D+00
!
!  Integrate over each interval.
!
  do i = 1, n - 1

    xl = x(i)
    xr = x(i+1)
    ul = u(i)
    ur = u(i+1)

    do q = 1, quad_num

      xq = ( ( 1.0D+00 - abscissa(q) ) * xl   &
           + ( 1.0D+00 + abscissa(q) ) * xr ) &
           /   2.0D+00

      wq = weight(q) * ( xr - xl ) / 2.0D+00
!
!  Use the fact that U is a linear combination of piecewise linears.
!
      uq = ( ( xr - xq      ) * ul &
           + (      xq - xl ) * ur ) &
           / ( xr      - xl )

      eq = exact ( xq )

      e2 = e2 + wq * ( uq - eq )**2

    end do

  end do

  e2 = sqrt ( e2 )

  return
end
subroutine max_error_linear ( n, x, u, exact, value )

!*****************************************************************************80
!
!! MAX_ERROR_LINEAR estimates the max error norm of a finite element solution.
!
!  Discussion:
!
!    We assume the finite element method has been used, over an interval [A,B]
!    involving N nodes, with piecewise linear elements used for the basis.
!    The coefficients U(1:N) have been computed, and a formula for the
!    exact solution is known.
!
!    This function estimates the max norm of the error:
!
!      MAX_NORM = Integral ( A <= X <= B ) max ( abs ( U(X) - EXACT(X) ) ) dX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Input, real ( kind = 8 ) X(N), the mesh points.
!
!    Input, real ( kind = 8 ) U(N), the finite element coefficients.
!
!    Input, function EQ = EXACT ( X ), returns the value of the exact
!    solution at the point X.
!
!    Output, real ( kind = 8 ) VALUE, the estimated max norm of the error.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) e
  integer ( kind = 4 ) e_num
  real ( kind = 8 ) eq
  real ( kind = 8 ), external :: exact
  integer ( kind = 4 ) l
  integer ( kind = 4 ) q
  integer ( kind = 4 ) quad_num
  integer ( kind = 4 ) r
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) ul
  real ( kind = 8 ) ur
  real ( kind = 8 ) uq
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xl
  real ( kind = 8 ) xq
  real ( kind = 8 ) xr

  quad_num = 8
  value = 0.0D+00
!
!  Examine QUAD_NUM points in each element, including left node but not right.
!
  e_num = n - 1

  do e = 1, e_num

    l = e
    xl = x(l)
    ul = u(l)

    r = e + 1
    xr = x(r)
    ur = u(r)

    do q = 0, quad_num - 1

      xq = ( real ( quad_num - q, kind = 8 ) * xl   &
           + real (            q, kind = 8 ) * xr ) &
         /   real ( quad_num, kind = 8 )
!
!  Use the fact that U is a linear combination of piecewise linears.
!
      uq = ( ( xr - xq      ) * ul   &
           + (      xq - xl ) * ur ) &
           / ( xr      - xl )

      eq = exact ( xq )

      value = max ( value, abs ( uq - eq ) )

    end do

  end do
!
!  For completeness, check last node.
!
  xq = x(n)
  uq = u(n)
  eq = exact ( xq )

  value = max ( value, abs ( uq - eq ) )
!
!  Integral approximation requires multiplication by interval length.
!
  value = value * ( x(n) - x(1) )

  return
end
subroutine r8mat_solve2 ( n, a, b, x, ierror )

!*****************************************************************************80
!
!! R8MAT_SOLVE2 computes the solution of an N by N linear system.
!
!  Discussion:
!
!    An R8MAT is an array of R8 values.
!
!    The linear system may be represented as
!
!      A*X = B
!
!    If the linear system is singular, but consistent, then the routine will
!    still produce a solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of equations.
!
!    Input/output, real ( kind = 8 ) A(N,N).
!    On input, A is the coefficient matrix to be inverted.
!    On output, A has been overwritten.
!
!    Input/output, real ( kind = 8 ) B(N).
!    On input, B is the right hand side of the system.
!    On output, B has been overwritten.
!
!    Output, real ( kind = 8 ) X(N), the solution of the linear system.
!
!    Output, integer ( kind = 4 ) IERROR.
!    0, no error detected.
!    1, consistent singularity.
!    2, inconsistent singularity.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) imax
  integer ( kind = 4 ) ipiv(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(n)

  ierror = 0

  ipiv(1:n) = 0
  x(1:n) = 0.0D+00
!
!  Process the matrix.
!
  do k = 1, n
!
!  In column K:
!    Seek the row IMAX with the properties that:
!      IMAX has not already been used as a pivot;
!      A(IMAX,K) is larger in magnitude than any other candidate.
!
    amax = 0.0D+00
    imax = 0
    do i = 1, n
      if ( ipiv(i) == 0 ) then
        if ( amax < abs ( a(i,k) ) ) then
          imax = i
          amax = abs ( a(i,k) )
        end if
      end if
    end do
!
!  If you found a pivot row IMAX, then,
!    eliminate the K-th entry in all rows that have not been used for pivoting.
!
    if ( imax /= 0 ) then

      ipiv(imax) = k
      a(imax,k+1:n) = a(imax,k+1:n) / a(imax,k)
      b(imax) = b(imax) / a(imax,k)
      a(imax,k) = 1.0D+00

      do i = 1, n

        if ( ipiv(i) == 0 ) then
          a(i,k+1:n) = a(i,k+1:n) - a(i,k) * a(imax,k+1:n)
          b(i) = b(i) - a(i,k) * b(imax)
          a(i,k) = 0.0D+00
        end if

      end do

    end if

  end do
!
!  Now, every row with nonzero IPIV begins with a 1, and
!  all other rows are all zero.  Begin solution.
!
  do j = n, 1, -1

    imax = 0
    do k = 1, n
      if ( ipiv(k) == j ) then
        imax = k
      end if
    end do

    if ( imax == 0 ) then

      x(j) = 0.0D+00

      if ( b(j) == 0.0D+00 ) then
        ierror = 1
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_SOLVE2 - Warning:'
        write ( *, '(a,i8)' ) '  Consistent singularity, equation = ', j
      else
        ierror = 2
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_SOLVE2 - Error:'
        write ( *, '(a,i8)' ) '  Inconsistent singularity, equation = ', j
      end if

    else

      x(j) = b(imax)

      do i = 1, n
        if ( i /= imax ) then
          b(i) = b(i) - a(i,j) * x(j)
        end if
      end do

    end if

  end do

  return
end
subroutine r8vec_linspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_LINSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
!
!    In other words, the interval is divided into N-1 even subintervals,
!    and the endpoints of intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the first and last entries.
!
!    Output, real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n
      x(i) = ( real ( n - i,     kind = 8 ) * a   &
             + real (     i - 1, kind = 8 ) * b ) &
             / real ( n     - 1, kind = 8 )
    end do

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
