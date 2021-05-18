subroutine fem2d_bvp_linear ( nx, ny, a, c, f, x, y, u )

!*****************************************************************************80
!
!! FEM2D_BVP_LINEAR solves a boundary value problem on a rectangle.
!
!  Discussion:
!
!    The procedure uses the finite element method, with piecewise linear basis
!    functions to solve a 2D boundary value problem over a rectangle
!
!    The following differential equation is imposed inside the region:
!
!      - d/dx a(x,y) du/dx - d/dy a(x,y) du/dy + c(x,y) * u(x,y) = f(x,y)
!
!    where a(x,y), c(x,y), and f(x,y) are given functions.
!
!    On the boundary, the solution is constrained to have the value 0.
!
!    The finite element method will use a regular grid of NX nodes in X, and 
!    NY nodes in Y.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, the number of X and Y grid values.
!
!    Input, function A ( X ), evaluates a(x);
!
!    Input, function C ( X ), evaluates c(x);
!
!    Input, function F ( X ), evaluates f(x);
!
!    Input, real ( kind = 8 ) X(NX), Y(NY), the mesh points.
!
!    Output, real ( kind = 8 ) U(NX,NY), the finite element coefficients, which 
!    are also the value of the computed solution at the mesh points.
!
  implicit none

  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ), parameter :: quad_num = 3

  real ( kind = 8 ), external :: a
  real ( kind = 8 ), dimension ( quad_num ) :: abscissa = (/ &
    -0.774596669241483377035853079956D+00, &
     0.000000000000000000000000000000D+00, &
     0.774596669241483377035853079956D+00 /)
  real ( kind = 8 ) amat(nx*ny,nx*ny)
  real ( kind = 8 ) aq
  real ( kind = 8 ) b(nx*ny)
  real ( kind = 8 ), external :: c
  real ( kind = 8 ) cq
  integer ( kind = 4 ) e
  integer ( kind = 4 ) ex
  integer ( kind = 4 ) ey
  real ( kind = 8 ), external :: f
  real ( kind = 8 ) fq
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mn
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ne
  integer ( kind = 4 ) nw
  integer ( kind = 4 ) qx
  integer ( kind = 4 ) qy
  integer ( kind = 4 ) s
  integer ( kind = 4 ) se
  integer ( kind = 4 ) sw
  integer ( kind = 4 ) w
  real ( kind = 8 ), dimension ( quad_num ) :: weight = (/ &
    0.555555555555555555555555555556D+00, &
    0.888888888888888888888888888889D+00, &
    0.555555555555555555555555555556D+00 /)
  real ( kind = 8 ) wq
  real ( kind = 8 ) u(nx,ny)
  real ( kind = 8 ) vne
  real ( kind = 8 ) vnex
  real ( kind = 8 ) vney
  real ( kind = 8 ) vnw
  real ( kind = 8 ) vnwx
  real ( kind = 8 ) vnwy
  real ( kind = 8 ) vse
  real ( kind = 8 ) vsex
  real ( kind = 8 ) vsey
  real ( kind = 8 ) vsw
  real ( kind = 8 ) vswx
  real ( kind = 8 ) vswy
  real ( kind = 8 ) x(nx)
  real ( kind = 8 ) xe
  real ( kind = 8 ) xq
  real ( kind = 8 ) xw
  real ( kind = 8 ) y(ny)
  real ( kind = 8 ) yn
  real ( kind = 8 ) yq
  real ( kind = 8 ) ys
!
!  Zero out the matrix and right hand side.
!
  mn = nx * ny
  amat(1:mn,1:mn) = 0.0D+00
  b(1:mn) = 0.0D+00

  do ex = 1, nx - 1

    w = ex
    e = ex + 1

    xw = x(w)
    xe = x(e)

    do ey = 1, ny - 1

      s = ey
      n = ey + 1

      ys = y(s)
      yn = y(n)

      sw = ( ey - 1 ) * nx + ex
      se = ( ey - 1 ) * nx + ex + 1
      nw =   ey       * nx + ex
      ne =   ey       * nx + ex + 1

      do qx = 1, quad_num

        xq = ( ( 1.0D+00 - abscissa(qx) ) * xw   &
             + ( 1.0D+00 + abscissa(qx) ) * xe ) &
               / 2.0D+00

        do qy = 1, quad_num

          yq = ( ( 1.0D+00 - abscissa(qy) ) * ys   &
               + ( 1.0D+00 + abscissa(qy) ) * yn ) &
                 / 2.0D+00

          wq = weight(qx) * ( xe - xw ) / 2.0D+00 &
             * weight(qy) * ( yn - ys ) / 2.0D+00

          aq = a ( xq, yq )
          cq = c ( xq, yq )
          fq = f ( xq, yq )

          vsw  = ( xe - xq ) / ( xe - xw ) * ( yn - yq ) / ( yn - ys )
          vswx = (-1.0D+00 ) / ( xe - xw ) * ( yn - yq ) / ( yn - ys )
          vswy = ( xe - xq ) / ( xe - xw ) * (-1.0D+00 ) / ( yn - ys )

          vse  = ( xq - xw ) / ( xe - xw ) * ( yn - yq ) / ( yn - ys )
          vsex = ( 1.0D+00 ) / ( xe - xw ) * ( yn - yq ) / ( yn - ys ) 
          vsey = ( xq - xw ) / ( xe - xw ) * (-1.0D+00 ) / ( yn - ys )

          vnw  = ( xe - xq ) / ( xe - xw ) * ( yq - ys ) / ( yn - ys )
          vnwx = (-1.0D+00 ) / ( xe - xw ) * ( yq - ys ) / ( yn - ys )
          vnwy = ( xe - xq ) / ( xe - xw ) * ( 1.0D+00 ) / ( yn - ys )

          vne  = ( xq - xw ) / ( xe - xw ) * ( yq - ys ) / ( yn - ys )
          vnex = ( 1.0D+00 ) / ( xe - xw ) * ( yq - ys ) / ( yn - ys )
          vney = ( xq - xw ) / ( xe - xw ) * ( 1.0D+00 ) / ( yn - ys )

          amat(sw,sw) = amat(sw,sw) + wq * ( vswx * aq * vswx &
                                           + vswy * aq * vswy &
                                           + vsw  * cq * vsw )
          amat(sw,se) = amat(sw,se) + wq * ( vswx * aq * vsex &
                                           + vswy * aq * vsey &
                                           + vsw  * cq * vse )
          amat(sw,nw) = amat(sw,nw) + wq * ( vswx * aq * vnwx &
                                           + vswy * aq * vnwy &
                                           + vsw  * cq * vnw )
          amat(sw,ne) = amat(sw,ne) + wq * ( vswx * aq * vnex &
                                           + vswy * aq * vney &
                                           + vsw  * cq * vne )
          b(sw) =    b(sw)          + wq * ( vsw * fq )

          amat(se,sw) = amat(se,sw) + wq * ( vsex * aq * vswx &
                                           + vsey * aq * vswy &
                                           + vse  * cq * vsw )
          amat(se,se) = amat(se,se) + wq * ( vsex * aq * vsex &
                                           + vsey * aq * vsey &
                                           + vse  * cq * vse )
          amat(se,nw) = amat(se,nw) + wq * ( vsex * aq * vnwx &
                                           + vsey * aq * vnwy &
                                           + vse  * cq * vnw )
          amat(se,ne) = amat(se,ne) + wq * ( vsex * aq * vnex &
                                           + vsey * aq * vney &
                                           + vse  * cq * vne )
          b(se) =    b(se)          + wq * ( vse * fq )

          amat(nw,sw) = amat(nw,sw) + wq * ( vnwx * aq * vswx &
                                           + vnwy * aq * vswy &
                                           + vnw  * cq * vsw )
          amat(nw,se) = amat(nw,se) + wq * ( vnwx * aq * vsex &
                                           + vnwy * aq * vsey &
                                           + vnw  * cq * vse )
          amat(nw,nw) = amat(nw,nw) + wq * ( vnwx * aq * vnwx &
                                           + vnwy * aq * vnwy &
                                           + vnw  * cq * vnw )
          amat(nw,ne) = amat(nw,ne) + wq * ( vnwx * aq * vnex &
                                           + vnwy * aq * vney &
                                           + vnw  * cq * vne )
          b(nw) =    b(nw)          + wq * ( vnw * fq )

          amat(ne,sw) = amat(ne,sw) + wq * ( vnex * aq * vswx &
                                           + vney * aq * vswy &
                                           + vne  * cq * vsw )
          amat(ne,se) = amat(ne,se) + wq * ( vnex * aq * vsex &
                                           + vney * aq * vsey &
                                           + vne  * cq * vse )
          amat(ne,nw) = amat(ne,nw) + wq * ( vnex * aq * vnwx &
                                           + vney * aq * vnwy &
                                           + vne  * cq * vnw )
          amat(ne,ne) = amat(ne,ne) + wq * ( vnex * aq * vnex &
                                           + vney * aq * vney &
                                           + vne  * cq * vne )
          b(ne) =    b(ne)          + wq * ( vne * fq )


        end do

      end do

    end do

  end do
!
!  Where a node is on the boundary, 
!  replace the finite element equation by a boundary condition.
!
  k = 0
  do j = 1, ny
    do i = 1, nx
      k = k + 1
      if ( i == 1 .or. i == nx .or. j == 1 .or. j == ny ) then
        amat(k,1:mn) = 0.0D+00
        amat(1:mn,k) = 0.0D+00
        amat(k,k) = 1.0D+00
        b(k) = 0.0D+00
      end if
    end do
  end do
!
!  Solve the linear system.
!
  call r8mat_solve2 ( mn, amat, b, u, ierror )

  return
end
subroutine fem2d_h1s_error_linear ( nx, ny, x, y, u, exact_ux, exact_uy, h1s )

!*****************************************************************************80
!
!! FEM2D_H1S_ERROR_LINEAR: seminorm error of a finite element solution.
!
!  Discussion:
!
!    The finite element method has been used, over a rectangle,
!    involving a grid of NX*NY nodes, with piecewise linear elements used 
!    for the basis.
!
!    The finite element solution U(x,y) has been computed, and formulas for the
!    exact derivatives Vx(x,y) and Vy(x,y) are known.
!
!    This function estimates the H1 seminorm of the error:
!
!      H1S = sqrt ( integral ( x, y )   ( Ux(x,y) - Vx(x,y) )^2 
!                                     + ( Uy(x,y) - Vy(x,y) )^2 dx dy )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, the number of nodes in the X 
!    and Y directions.
!
!    Input, real ( kind = 8 ) X(NX), Y(NY), the grid coordinates.
!
!    Input, real ( kind = 8 ) U(NX,NY), the finite element coefficients.
!
!    Input, function EXACT_UX(X,Y), EXACT_UY(X,Y), return the exact
!    derivatives with respect to X and Y, at the point (X,Y).
!
!    Output, real ( kind = 8 ) H1S, the estimated seminorm of 
!    the error.
!
  implicit none

  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ), parameter :: quad_num = 3

  real ( kind = 8 ), dimension ( quad_num ) :: abscissa = (/ &
    -0.774596669241483377035853079956D+00, &
     0.000000000000000000000000000000D+00, &
     0.774596669241483377035853079956D+00 /)
  integer ( kind = 4 ) e
  integer ( kind = 4 ) ex
  integer ( kind = 4 ) ey
  real ( kind = 8 ), external :: exact_ux
  real ( kind = 8 ), external :: exact_uy
  real ( kind = 8 ) exq
  real ( kind = 8 ) eyq
  real ( kind = 8 ) h1s
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ne
  integer ( kind = 4 ) nw
  integer ( kind = 4 ) qx
  integer ( kind = 4 ) qy
  integer ( kind = 4 ) s
  integer ( kind = 4 ) se
  integer ( kind = 4 ) sw
  real ( kind = 8 ) u(nx,ny)
  real ( kind = 8 ) uxq
  real ( kind = 8 ) uyq
  real ( kind = 8 ) vnex
  real ( kind = 8 ) vney
  real ( kind = 8 ) vnwx
  real ( kind = 8 ) vnwy
  real ( kind = 8 ) vsex
  real ( kind = 8 ) vsey
  real ( kind = 8 ) vswx
  real ( kind = 8 ) vswy
  integer ( kind = 4 ) w
  real ( kind = 8 ), dimension ( quad_num ) :: weight = (/ &
    0.555555555555555555555555555556D+00, &
    0.888888888888888888888888888889D+00, &
    0.555555555555555555555555555556D+00 /)
  real ( kind = 8 ) wq
  real ( kind = 8 ) x(nx)
  real ( kind = 8 ) xe
  real ( kind = 8 ) xq
  real ( kind = 8 ) xw
  real ( kind = 8 ) y(ny)
  real ( kind = 8 ) yn
  real ( kind = 8 ) yq
  real ( kind = 8 ) ys

  h1s = 0.0D+00

  do ex = 1, nx - 1

    w = ex
    e = ex + 1

    xw = x(w)
    xe = x(e)

    do ey = 1, ny - 1

      s = ey
      n = ey + 1

      ys = y(s)
      yn = y(n)

      sw = ( ey - 1 ) * nx + ex
      se = ( ey - 1 ) * nx + ex + 1
      nw =   ey       * nx + ex
      ne =   ey       * nx + ex + 1

      do qx = 1, quad_num

        xq = ( ( 1.0D+00 - abscissa(qx) ) * xw   &
             + ( 1.0D+00 + abscissa(qx) ) * xe ) &
               / 2.0D+00

        do qy = 1, quad_num

          yq = ( ( 1.0D+00 - abscissa(qy) ) * ys   &
               + ( 1.0D+00 + abscissa(qy) ) * yn ) &
                 / 2.0D+00

          wq = weight(qx) * ( xe - xw ) / 2.0D+00 &
             * weight(qy) * ( yn - ys ) / 2.0D+00

          vswx = (-1.0D+00 ) / ( xe - xw ) * ( yn - yq ) / ( yn - ys )
          vswy = ( xe - xq ) / ( xe - xw ) * (-1.0D+00 ) / ( yn - ys )

          vsex = ( 1.0D+00 ) / ( xe - xw ) * ( yn - yq ) / ( yn - ys ) 
          vsey = ( xq - xw ) / ( xe - xw ) * (-1.0D+00 ) / ( yn - ys )

          vnwx = (-1.0D+00 ) / ( xe - xw ) * ( yq - ys ) / ( yn - ys )
          vnwy = ( xe - xq ) / ( xe - xw ) * ( 1.0D+00 ) / ( yn - ys )

          vnex = ( 1.0D+00 ) / ( xe - xw ) * ( yq - ys ) / ( yn - ys )
          vney = ( xq - xw ) / ( xe - xw ) * ( 1.0D+00 ) / ( yn - ys )
!
!  Note that the south-west component of U is stored in U(W,S), not U(S,W)!
!
          uxq = u(w,s) * vswx + u(e,s) * vsex + u(w,n) * vnwx + u(e,n) * vnex
          uyq = u(w,s) * vswy + u(e,s) * vsey + u(w,n) * vnwy + u(e,n) * vney

          exq = exact_ux ( xq, yq )
          eyq = exact_uy ( xq, yq )

          h1s = h1s + wq * ( ( uxq - exq ) ** 2 + ( uyq - eyq ) ** 2 )

        end do
      end do
    end do
  end do

  h1s = sqrt ( h1s )

  return
end
subroutine fem2d_l1_error ( nx, ny, x, y, u, exact, e1 )

!*****************************************************************************80
!
!! FEM2D_L1_ERROR estimates the l1 error norm of a finite element solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, the number of nodes in the X 
!    and Y directions.
!
!    Input, real ( kind = 8 ) X(NX), Y(NY), the grid coordinates.
!
!    Input, real ( kind = 8 ) U(NX,NY), the finite element coefficients.
!
!    Input, function EQ = EXACT ( X, Y ), returns the value of the exact
!    solution at the point (X,Y).
!
!    Output, real ( kind = 8 ) E1, the estimated L2 norm of the error.
!
  implicit none

  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny

  real ( kind = 8 ) e1
  real ( kind = 8 ), external :: exact
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) u(nx,ny)
  real ( kind = 8 ) x(nx)
  real ( kind = 8 ) y(ny)

  e1 = 0.0D+0
  do j = 1, ny
    do i = 1, nx
      e1 = e1 + abs ( u(i,j) - exact ( x(i), y(j) ) )
    end do
  end do

  e1 = e1 / real ( nx, kind = 8 ) / real ( ny, kind = 8 )

  return
end
subroutine fem2d_l2_error_linear ( nx, ny, x, y, u, exact, e2 )

!*****************************************************************************80
!
!! FEM2D_L2_ERROR_LINEAR: L2 error norm of a finite element solution.
!
!  Discussion:
!
!    The finite element method has been used, over a rectangle,
!    involving a grid of NX*NY nodes, with piecewise linear elements used 
!    for the basis.
!
!    The finite element coefficients have been computed, and a formula for the
!    exact solution is known.
!
!    This function estimates E2, the L2 norm of the error:
!
!      E2 = Integral ( X, Y ) ( U(X,Y) - EXACT(X,Y) )^2 dX dY
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, the number of nodes in the X 
!    and Y directions.
!
!    Input, real ( kind = 8 ) X(NX), Y(NY), the grid coordinates.
!
!    Input, real ( kind = 8 ) U(NX,NY), the finite element coefficients.
!
!    Input, real ( kind = 8 ), external EXACT(X,Y), returns the value of 
!    the exact solution at the point (X,Y).
!
!    Output, real ( kind = 8 ) E2, the estimated L2 norm of the error.
!
  implicit none

  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ), parameter :: quad_num = 3

  real ( kind = 8 ), dimension ( quad_num ) :: abscissa = (/ &
    -0.774596669241483377035853079956D+00, &
     0.000000000000000000000000000000D+00, &
     0.774596669241483377035853079956D+00 /)
  integer ( kind = 4 ) e
  real ( kind = 8 ) e2
  real ( kind = 8 ) eq
  integer ( kind = 4 ) ex
  real ( kind = 8 ), external :: exact
  integer ( kind = 4 ) ey
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ne
  integer ( kind = 4 ) nw
  integer ( kind = 4 ) qx
  integer ( kind = 4 ) qy
  integer ( kind = 4 ) s
  integer ( kind = 4 ) se
  integer ( kind = 4 ) sw
  real ( kind = 8 ) u(nx,ny)
  real ( kind = 8 ) uq
  real ( kind = 8 ) vne
  real ( kind = 8 ) vnw
  real ( kind = 8 ) vse
  real ( kind = 8 ) vsw
  integer ( kind = 4 ) w
  real ( kind = 8 ), dimension ( quad_num ) :: weight = (/ &
    0.555555555555555555555555555556D+00, &
    0.888888888888888888888888888889D+00, &
    0.555555555555555555555555555556D+00 /)
  real ( kind = 8 ) wq
  real ( kind = 8 ) x(nx)
  real ( kind = 8 ) xe
  real ( kind = 8 ) xq
  real ( kind = 8 ) xw
  real ( kind = 8 ) y(ny)
  real ( kind = 8 ) yn
  real ( kind = 8 ) yq
  real ( kind = 8 ) ys

  e2 = 0.0D+00
!
!  Integrate over each interval.
!
  do ex = 1, nx - 1

    w = ex
    e = ex + 1

    xw = x(w)
    xe = x(e)

    do ey = 1, ny - 1

      s = ey
      n = ey + 1

      ys = y(s)
      yn = y(n)

      sw = ( ey - 1 ) * nx + ex
      se = ( ey - 1 ) * nx + ex + 1
      nw =   ey       * nx + ex
      ne =   ey       * nx + ex + 1

      do qx = 1, quad_num

        xq = ( ( 1.0D+00 - abscissa(qx) ) * xw  &
             + ( 1.0D+00 + abscissa(qx) ) * xe ) &
               / 2.0D+00

        do qy = 1, quad_num

          yq = ( ( 1.0D+00 - abscissa(qy) ) * ys   &
               + ( 1.0D+00 + abscissa(qy) ) * yn ) &
                 / 2.0D+00

          wq = weight(qx) * ( xe - xw ) / 2.0D+00 &
             * weight(qy) * ( yn - ys ) / 2.0D+00

          vsw  = ( xe - xq ) / ( xe - xw ) * ( yn - yq ) / ( yn - ys )
          vse  = ( xq - xw ) / ( xe - xw ) * ( yn - yq ) / ( yn - ys )
          vnw  = ( xe - xq ) / ( xe - xw ) * ( yq - ys ) / ( yn - ys )
          vne  = ( xq - xw ) / ( xe - xw ) * ( yq - ys ) / ( yn - ys )
!
!  Note that the south-west component of U is stored in U(W,S), not U(S,W)!
!
          uq = u(w,s) * vsw + u(e,s) * vse + u(w,n) * vnw + u(e,n) * vne
          eq = exact ( xq, yq )

          e2 = e2 + wq * ( uq - eq ) ** 2

        end do
      end do
    end do
  end do

  e2 = sqrt ( e2 )

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
!    Input/output, real ( kind = 8 ) amat(N,N).
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
!      amat(IMAX,K) is larger in magnitude than any other candidate.
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
subroutine r8vec_even ( n, alo, ahi, a )

!*****************************************************************************80
!
!! R8VEC_EVEN returns an R8VEC of evenly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If N is 1, then the midpoint is returned.
!
!    Otherwise, the two endpoints are returned, and N-2 evenly
!    spaced points between them.
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
!    Input, integer ( kind = 4 ) N, the number of values.
!
!    Input, real ( kind = 8 ) ALO, AHI, the low and high values.
!
!    Output, real ( kind = 8 ) amat(N), N evenly spaced values.
!    Normally, amat(1) = ALO and amat(N) = AHI.
!    However, if N = 1, then amat(1) = 0.5*(ALO+AHI).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) ahi
  real ( kind = 8 ) alo
  integer ( kind = 4 ) i

  if ( n == 1 ) then

    a(1) = 0.5D+00 * ( alo + ahi )

  else

    do i = 1, n
      a(i) = ( real ( n - i,     kind = 8 ) * alo   &
             + real (     i - 1, kind = 8 ) * ahi ) &
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
