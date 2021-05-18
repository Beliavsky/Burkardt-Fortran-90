subroutine basis_serene ( xq, yq, xw, ys, xe, yn, xx, yy, v )

!*****************************************************************************80
!
!! BASIS_SERENE evaluates the serendipity basis functions.
!
!  Discussion:
!
!    This procedure assumes that a serendipity element has been defined,
!    whose sides are parallel to coordinate axes.
!
!    The local element numbering is
!
!      YN  3--2--1
!       |  |     |
!       |  4     8
!       |  |     |
!      YS  5--6--7
!       |
!       +--XW---XE--
!
!    We note that each basis function can be written as the product of
!    three linear terms, which never result in an x^2y^2 term.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XQ, YQ, the evaluation point.
!
!    Input, real ( kind = 8 ) XW, YS, the coordinates of the lower left corner.
!
!    Input, real ( kind = 8 ) XE, YN, the coordinates of the upper right corner.
!
!    Input, real ( kind = 8 ) XX(8), YY(8), the coordinates of the 8 nodes.
!
!    Output, real ( kind = 8 ) V(8), the value of the basis functions 
!    at (XQ,YQ).
!
  implicit none

  real ( kind = 8 ), external :: not1
  real ( kind = 8 ), external :: not2
  real ( kind = 8 ) v(8)
  real ( kind = 8 ) xe
  real ( kind = 8 ) xq
  real ( kind = 8 ) xw
  real ( kind = 8 ) xx(8)
  real ( kind = 8 ) yn
  real ( kind = 8 ) yq
  real ( kind = 8 ) ys
  real ( kind = 8 ) yy(8)

  v(1) = not1 ( xq, xw, xx(1) ) &
       * not1 ( yq, ys, yy(1) ) &
       * not2 ( xq, yq, xx(8), yy(8), xx(2), yy(2), xx(1), yy(1) )

  v(2) = not1 ( xq, xw, xx(2) ) &
       * not1 ( xq, xe, xx(2) ) &
       * not1 ( yq, ys, yy(2) )

  v(3) = not1 ( xq, xe, xx(3) ) &
       * not1 ( yq, ys, yy(3) ) &
       * not2 ( xq, yq, xx(2), yy(2), xx(4), yy(4), xx(3), yy(3) )

  v(4) = not1 ( xq, xe, xx(4) ) &
       * not1 ( yq, yn, yy(4) ) &
       * not1 ( yq, ys, yy(4) )

  v(5) = not1 ( xq, xe, xx(5) ) &
       * not1 ( yq, yn, yy(5) ) &
       * not2 ( xq, yq, xx(4), yy(4), xx(6), yy(6), xx(5), yy(5) )

  v(6) = not1 ( xq, xe, xx(6) ) &
       * not1 ( xq, xw, xx(6) ) &
       * not1 ( yq, yn, yy(6) )

  v(7) = not1 ( xq, xw, xx(7) ) &
       * not1 ( yq, yn, yy(7) ) &
       * not2 ( xq, yq, xx(6), yy(6), xx(8), yy(8), xx(7), yy(7) )

  v(8) = not1 ( yq, ys, yy(8) ) &
       * not1 ( yq, yn, yy(8) ) &
       * not1 ( xq, xw, xx(8) )

  return
end
subroutine basisd_serene ( xq, yq, xw, ys, xe, yn, xx, yy, vx, vy )

!*****************************************************************************80
!
!! BASISD_SERENE differentiates the serendipity basis functions.
!
!  Discussion:
!
!    This procedure assumes that a serendipity element has been defined,
!    whose sides are parallel to coordinate axes.
!
!    The local element numbering is
!
!      YN  3--2--1
!       |  |     |
!       |  4     8
!       |  |     |
!      YS  5--6--7
!       |
!       +--XW---XE--
!
!    We note that each basis function can be written as the product of
!    three linear terms, which never result in an x^2y^2 term.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XQ, YQ, the evaluation point.
!
!    Input, real ( kind = 8 ) XW, YS, the coordinates of the lower left corner.
!
!    Input, real ( kind = 8 ) XE, YN, the coordinates of the upper right corner.
!
!    Input, real ( kind = 8 ) XX(8), YY(8), the coordinates of the 8 nodes.
!
!    Output, real ( kind = 8 ) VX(8), VY(8), the derivatives of the basis 
!    functions at (XQ,YQ) with respect to X and Y.
!
  implicit none

  real ( kind = 8 ), external :: not1
  real ( kind = 8 ), external :: not1d
  real ( kind = 8 ), external :: not2
  real ( kind = 8 ), external :: not2dx
  real ( kind = 8 ), external :: not2dy
  real ( kind = 8 ) vx(8)
  real ( kind = 8 ) vy(8)
  real ( kind = 8 ) xe
  real ( kind = 8 ) xq
  real ( kind = 8 ) xw
  real ( kind = 8 ) xx(8)
  real ( kind = 8 ) yn
  real ( kind = 8 ) yq
  real ( kind = 8 ) ys
  real ( kind = 8 ) yy(8)

  vx(1) = &
      not1d ( xw, xx(1) ) &
    * not1 ( yq, ys, yy(1) ) &
    * not2 ( xq, yq, xx(8), yy(8), xx(2), yy(2), xx(1), yy(1) ) &
    + not1 ( xq, xw, xx(1) ) &
    * not1 ( yq, ys, yy(1) ) &
    * not2dx ( xx(8), yy(8), xx(2), yy(2), xx(1), yy(1) )

  vy(1) = &
      not1 ( xq, xw, xx(1) ) &
    * not1d ( ys, yy(1) ) &
    * not2 ( xq, yq, xx(8), yy(8), xx(2), yy(2), xx(1), yy(1) ) &
    + not1 ( xq, xw, xx(1) ) &
    * not1 ( yq, ys, yy(1) ) &
    * not2dy ( xx(8), yy(8), xx(2), yy(2), xx(1), yy(1) )

  vx(2) = &
      not1d ( xw, xx(2) ) &
    * not1 ( xq, xe, xx(2) ) &
    * not1 ( yq, ys, yy(2) ) &
    + not1 ( xq, xw, xx(2) ) &
    * not1d ( xe, xx(2) ) &
    * not1 ( yq, ys, yy(2) )

  vy(2) = &
      not1 ( xq, xw, xx(2) ) &
    * not1 ( xq, xe, xx(2) ) &
    * not1d ( ys, yy(2) )

  vx(3) = &
      not1d ( xe, xx(3) ) &
    * not1 ( yq, ys, yy(3) ) &
    * not2 ( xq, yq, xx(2), yy(2), xx(4), yy(4), xx(3), yy(3) ) &
    + not1 ( xq, xe, xx(3) ) &
    * not1 ( yq, ys, yy(3) ) &
    * not2dx ( xx(2), yy(2), xx(4), yy(4), xx(3), yy(3) )

  vy(3) = not1 ( xq, xe, xx(3) ) &
    * not1d ( ys, yy(3) ) &
    * not2 ( xq, yq, xx(2), yy(2), xx(4), yy(4), xx(3), yy(3) ) &
    + not1 ( xq, xe, xx(3) ) &
    * not1 ( yq, ys, yy(3) ) &
    * not2dy ( xx(2), yy(2), xx(4), yy(4), xx(3), yy(3) )

  vx(4) = &
      not1d ( xe, xx(4) ) &
    * not1 ( yq, yn, yy(4) ) &
    * not1 ( yq, ys, yy(4) )

  vy(4) = &
      not1 ( xq, xe, xx(4) ) &
    * not1d ( yn, yy(4) ) &
    * not1 ( yq, ys, yy(4) ) &
    + not1 ( xq, xe, xx(4) ) &
    * not1 ( yq, yn, yy(4) ) &
    * not1d ( ys, yy(4) )

  vx(5) = &
      not1d ( xe, xx(5) ) &
    * not1 ( yq, yn, yy(5) ) &
    * not2 ( xq, yq, xx(4), yy(4), xx(6), yy(6), xx(5), yy(5) ) &
    + not1 ( xq, xe, xx(5) ) &
    * not1 ( yq, yn, yy(5) ) &
    * not2dx ( xx(4), yy(4), xx(6), yy(6), xx(5), yy(5) )

  vy(5) = &
      not1 ( xq, xe, xx(5) ) &
    * not1d ( yn, yy(5) ) &
    * not2 ( xq, yq, xx(4), yy(4), xx(6), yy(6), xx(5), yy(5) ) &
    + not1 ( xq, xe, xx(5) ) &
    * not1 ( yq, yn, yy(5) ) &
    * not2dy ( xx(4), yy(4), xx(6), yy(6), xx(5), yy(5) )

  vx(6) = &
      not1d ( xe, xx(6) ) &
    * not1 ( xq, xw, xx(6) ) &
    * not1 ( yq, yn, yy(6) ) &
    + not1 ( xq, xe, xx(6) ) &
    * not1d ( xw, xx(6) ) &
    * not1 ( yq, yn, yy(6) )

  vy(6) = &
      not1 ( xq, xe, xx(6) ) &
    * not1 ( xq, xw, xx(6) ) &
    * not1d ( yn, yy(6) )

  vx(7) = &
      not1d ( xw, xx(7) ) &
    * not1 ( yq, yn, yy(7) ) &
    * not2 ( xq, yq, xx(6), yy(6), xx(8), yy(8), xx(7), yy(7) ) &
    + not1 ( xq, xw, xx(7) ) &
    * not1 ( yq, yn, yy(7) ) &
    * not2dx ( xx(6), yy(6), xx(8), yy(8), xx(7), yy(7) )

  vy(7) = &
      not1 ( xq, xw, xx(7) ) &
    * not1d ( yn, yy(7) ) &
    * not2 ( xq, yq, xx(6), yy(6), xx(8), yy(8), xx(7), yy(7) ) &
    + not1 ( xq, xw, xx(7) ) &
    * not1 ( yq, yn, yy(7) ) &
    * not2dy ( xx(6), yy(6), xx(8), yy(8), xx(7), yy(7) )

  vx(8) = &
      not1 ( yq, ys, yy(8) ) &
    * not1 ( yq, yn, yy(8) ) &
    * not1d ( xw, xx(8) )

  vy(8) = &
      not1d ( ys, yy(8) ) &
    * not1 ( yq, yn, yy(8) ) &
    * not1 ( xq, xw, xx(8) ) &
    + not1 ( yq, ys, yy(8) ) &
    * not1d ( yn, yy(8) ) &
    * not1 ( xq, xw, xx(8) )

  return
end
subroutine fem2d_bvp_serene ( nx, ny, a, c, f, x, y, show11, u )

!*****************************************************************************80
!
!! FEM2D_BVP_SERENE solves boundary value problem on a rectangle.
!
!  Discussion:
!
!    The program uses the finite element method, with piecewise 
!    serendipity basis functions to solve a 2D boundary value problem 
!    over a rectangle.
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
!    NY nodes in Y.  Both NX and NY must be odd.
!
!    The local element numbering is
!
!      3--2--1
!      |     |
!      4     8
!      |     |
!      5--6--7
!
!    The serendipity element mass matrix is a multiple of:
!
!       6.0, -6.0,  2.0, -8.0,  3.0, -8.0,  2.0, -6.0
!      -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, -8.0, 20.0
!       2.0, -6.0,  6.0, -6.0,  2.0, -8.0,  3.0, -8.0
!      -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, -8.0, 16.0
!       3.0, -8.0,  2.0, -6.0,  6.0, -6.0,  2.0, -8.0
!      -8.0, 16.0, -8.0, 20.0, -6.0, 32.0, -6.0, 20.0
!       2.0, -8.0,  3.0, -8.0,  2.0, -6.0,  6.0, -6.0
!      -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, -6.0, 32.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, the number of X and Y grid values.
!    NX and NY must be odd and at least 3.
!
!    Input, function A(X,Y), evaluates a(x,y)
!
!    Input, function C(X,Y), evaluates c(x,y)
!
!    Input, function F(X,Y), evaluates f(x,y)
!
!    Input, real ( kind = 8 ) X(NX), Y(NY), the mesh points.
!
!    Input, integer ( kind = 4 ) SHOW11, is 1 to print out the element matrix
!    for the element in row 1, column 1.
!
!    Output, real ( kind = 8 ) U(MN), the finite element coefficients, which 
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
  real ( kind = 8 ) ae(8,8)
  real ( kind = 8 ), allocatable :: amat(:,:)
  real ( kind = 8 ) aq
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) be(8)
  real ( kind = 8 ), external :: c
  integer ( kind = 4 ) cc
  real ( kind = 8 ) cq
  integer ( kind = 4 ) e
  integer ( kind = 4 ) ex
  integer ( kind = 4 ) ex_num
  integer ( kind = 4 ) ey
  integer ( kind = 4 ) ey_num
  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) fem2d_bvp_serene_node_num
  real ( kind = 8 ) fq
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jj
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mm
  integer ( kind = 4 ) mn
  integer ( kind = 4 ) n
  integer ( kind = 4 ) node(8)
  integer ( kind = 4 ) qx
  integer ( kind = 4 ) qy
  integer ( kind = 4 ) s
  real ( kind = 8 ) scale
  logical show11
  real ( kind = 8 ) u(nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)
  real ( kind = 8 ) v(8)
  real ( kind = 8 ) vx(8)
  real ( kind = 8 ) vy(8)
  integer ( kind = 4 ) w
  real ( kind = 8 ), dimension ( quad_num ) :: weight = (/ &
    0.555555555555555555555555555556D+00, &
    0.888888888888888888888888888889D+00, &
    0.555555555555555555555555555556D+00 /)
  real ( kind = 8 ) wq
  real ( kind = 8 ) x(nx)
  real ( kind = 8 ) xc
  real ( kind = 8 ) xe
  real ( kind = 8 ) xq
  real ( kind = 8 ) xw
  real ( kind = 8 ) xx(8)
  real ( kind = 8 ) y(ny)
  real ( kind = 8 ) ym
  real ( kind = 8 ) yn
  real ( kind = 8 ) yq
  real ( kind = 8 ) ys
  real ( kind = 8 ) yy(8)
!
!  Make room for the matrix A and right hand side b.
!
  mn = fem2d_bvp_serene_node_num ( nx, ny )

  allocate ( amat(1:mn,1:mn) )
  allocate ( b(1:mn) )

  amat(1:mn,1:mn) = 0.0D+00
  b(1:mn) = 0.0D+00
!
!  Compute the matrix entries by integrating over each element.
!
  ex_num = ( nx - 1 ) / 2
  ey_num = ( ny - 1 ) / 2

  do ey = 1, ey_num

    s = 2 * ey - 1
    mm = 2 * ey
    n = 2 * ey + 1

    ys = y(s)
    ym = y(mm)
    yn = y(n)

    yy(1) = y(n)
    yy(2) = y(n)
    yy(3) = y(n)
    yy(4) = y(mm)
    yy(5) = y(s)
    yy(6) = y(s)
    yy(7) = y(s)
    yy(8) = y(mm)

    do ex = 1, ex_num

      w = 2 * ex - 1
      cc = 2 * ex
      e = 2 * ex + 1

      xe = x(e)
      xc = x(cc)
      xw = x(w)

      xx(1) = x(e)
      xx(2) = x(cc)
      xx(3) = x(w)
      xx(4) = x(w)
      xx(5) = x(w)
      xx(6) = x(cc)
      xx(7) = x(e)
      xx(8) = x(e)
!
!  Node indices
!
!  3  2  1  wn  cn  en
!  4     8  wm      em
!  5  6  7  ws  cs  es
!
      node(1) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex + 1
      node(2) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex
      node(3) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex - 1
      node(4) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex - 1
      node(5) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 3
      node(6) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 2
      node(7) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 1
      node(8) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex

      if ( show11 ) then
        if ( ey == 1 .and. ex == 1 ) then
          ae(1:8,1:8) = 0.0D+00
          be(1:8) = 0.0D+00
        end if
      end if

      do qx = 1, quad_num

        xq = ( ( 1.0D+00 - abscissa(qx) ) * x(e)   &
             + ( 1.0D+00 + abscissa(qx) ) * x(w) ) &
               / 2.0D+00

        do qy = 1, quad_num

          yq = ( ( 1.0D+00 - abscissa(qy) ) * y(n)   &
               + ( 1.0D+00 + abscissa(qy) ) * y(s) ) &
                 / 2.0D+00

          wq = weight(qx) * ( x(e) - x(w) ) / 2.0D+00 &
             * weight(qy) * ( y(n) - y(s) ) / 2.0D+00

          call basis_serene ( xq, yq, xw, ys, xe, yn, xx, yy, v )
          call basisd_serene ( xq, yq, xw, ys, xe, yn, xx, yy, vx, vy )

          aq = a ( xq, yq )
          cq = c ( xq, yq )
          fq = f ( xq, yq )
!
!  Build the element matrix.
!
          if ( show11 ) then
            if ( ey == 1 .and. ex == 1 ) then
              do i = 1, 8
                do j = 1, 8
                  ae(i,j) = ae(i,j) + wq * ( vx(i) * aq * vx(j) &
                                           + vy(i) * aq * vy(j) &
                                           + v(i)  * cq * v(j) )
                end do
                be(i) = be(i) + wq * ( v(i) * fq )
              end do  
            end if
          end if

          do i = 1, 8
            ii = node(i)
            do j = 1, 8
              jj = node(j)
              amat(ii,jj) = amat(ii,jj) + wq * ( vx(i) * aq * vx(j) &
                                               + vy(i) * aq * vy(j) &
                                               + v(i)  * cq * v(j) )
            end do
            b(ii) = b(ii) + wq * ( v(i) * fq )
          end do

        end do
      end do
!
!  Print a sample element matrix.
!
      if ( show11 ) then
        if ( ey == 1 .and. ex == 1 ) then
          scale = 0.5D+00 * ae(1,3)
          ae(1:8,1:8) = ae(1:8,1:8) / scale
          call r8mat_print ( 8, 8, ae, '  Wathen elementary mass matrix:' )
        end if
      end if

    end do
  end do
!
!  Where a node is on the boundary, 
!  replace the finite element equation by a boundary condition.
!
  k = 0

  do j = 1, ny

    if ( mod ( j, 2 ) == 1 ) then
      inc = 1
    else
      inc = 2
    end if

    do i = 1, nx, inc
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

  deallocate ( amat )
  deallocate ( b )

  return
end
function fem2d_bvp_serene_node_num ( nx, ny )

!*****************************************************************************80
!
!! FEM2D_BVP_SERENE_NODE_NUM counts the number of nodes.
!
!  Discussion:
!
!    The program uses the finite element method, with piecewise serendipity 
!    basis functions to solve a 2D boundary value problem over a rectangle.
!
!    The grid uses NX nodes in the X direction and NY nodes in the Y direction.
!
!    Both NX and NY must be odd.
!
!    Because of the peculiar shape of the serendipity elements, counting the
!    number of nodes and variables is a little tricky.  Here is a grid for
!    the case when NX = 7 and NY = 5, for which there are 29 nodes 
!    and variables.
!
!     23 24 25 26 27 28 29
!     19    20    21    22
!     12 13 14 15 16 17 18
!      8     9    10    11
!      1  2  3  4  5  6  7
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, the number of X and Y grid values.
!    NX and NY must be odd and at least 3.
!
!    Output, integer ( kind = 4 ) NODE_NUM, the number of nodes and variables.
!
  implicit none

  integer ( kind = 4 ) fem2d_bvp_serene_node_num
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny

  fem2d_bvp_serene_node_num = &
        nx           * ( ny + 1 ) / 2 &
    + ( nx + 1 ) / 2 * ( ny - 1 ) / 2 

  return
end
subroutine fem2d_h1s_error_serene ( nx, ny, x, y, u, exact_ux, exact_uy, h1s )

!*****************************************************************************80
!
!! FEM2D_H1S_ERROR_SERENE: seminorm error of a finite element solution.
!
!  Discussion:
!
!    We assume the finite element method has been used, over a product region
!    involving a grid of NX*NY nodes, with serendipity elements used 
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
!    01 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, the number of nodes.
!
!    Input, real ( kind = 8 ) X(NX), Y(NY), the grid coordinates.
!
!    Input, real ( kind = 8 ) U(*), the finite element coefficients.
!
!    Input, function EQX = EXACT_UX(X,Y), function EQY = EXACT_UY(X,Y)
!    returns the exact derivatives with respect to X and Y.
!
!    Output, real ( kind = 8 ) H1S, the estimated seminorm of the error.
!
  implicit none

  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ), parameter :: quad_num = 3

  real ( kind = 8 ), dimension ( quad_num ) :: abscissa = (/ &
    -0.774596669241483377035853079956D+00, &
     0.000000000000000000000000000000D+00, &
     0.774596669241483377035853079956D+00 /)
  integer ( kind = 4 ) cc
  integer ( kind = 4 ) e
  integer ( kind = 4 ) ex
  integer ( kind = 4 ) ex_num
  integer ( kind = 4 ) ey
  integer ( kind = 4 ) ey_num
  real ( kind = 8 ), external :: exact_ux
  real ( kind = 8 ), external :: exact_uy
  real ( kind = 8 ) exq
  real ( kind = 8 ) eyq
  real ( kind = 8 ) h1s
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mm
  integer ( kind = 4 ) n
  integer ( kind = 4 ) node(8)
  integer ( kind = 4 ) qx
  integer ( kind = 4 ) qy
  integer ( kind = 4 ) s
  real ( kind = 8 ) u(nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)
  real ( kind = 8 ) uxq
  real ( kind = 8 ) uyq
  real ( kind = 8 ) vx(8)
  real ( kind = 8 ) vy(8)
  integer ( kind = 4 ) w
  real ( kind = 8 ), dimension ( quad_num ) :: weight = (/ &
    0.555555555555555555555555555556D+00, &
    0.888888888888888888888888888889D+00, &
    0.555555555555555555555555555556D+00 /)
  real ( kind = 8 ) wq
  real ( kind = 8 ) x(nx)
  real ( kind = 8 ) xc
  real ( kind = 8 ) xe
  real ( kind = 8 ) xq
  real ( kind = 8 ) xw
  real ( kind = 8 ) xx(8)
  real ( kind = 8 ) y(ny)
  real ( kind = 8 ) ym
  real ( kind = 8 ) yn
  real ( kind = 8 ) yq
  real ( kind = 8 ) ys
  real ( kind = 8 ) yy(8)

  h1s = 0.0D+00
!
!  Quadrature definitions.
!
  ex_num = ( nx - 1 ) / 2
  ey_num = ( ny - 1 ) / 2

  do ey = 1, ey_num

    s = 2 * ey - 1
    mm = 2 * ey
    n = 2 * ey + 1

    ys = y(s)
    ym = y(mm)
    yn = y(n)

    yy(1) = y(n)
    yy(2) = y(n)
    yy(3) = y(n)
    yy(4) = y(mm)
    yy(5) = y(s)
    yy(6) = y(s)
    yy(7) = y(s)
    yy(8) = y(mm)

    do ex = 1, ex_num

      w = 2 * ex - 1
      cc = 2 * ex
      e = 2 * ex + 1

      xe = x(e)
      xc = x(cc)
      xw = x(w)

      xx(1) = x(e)
      xx(2) = x(cc)
      xx(3) = x(w)
      xx(4) = x(w)
      xx(5) = x(w)
      xx(6) = x(cc)
      xx(7) = x(e)
      xx(8) = x(e)
!
!  Node indices
!
!  3  2  1  wn  cn  en
!  4     8  wm      em
!  5  6  7  ws  cs  es
!
      node(1) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex + 1
      node(2) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex
      node(3) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex - 1
      node(4) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex - 1
      node(5) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 3
      node(6) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 2
      node(7) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 1
      node(8) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex

      do qx = 1, quad_num

        xq = ( ( 1.0D+00 - abscissa(qx) ) * x(e)   &
             + ( 1.0D+00 + abscissa(qx) ) * x(w) ) &
               / 2.0D+00

        do qy = 1, quad_num

          yq = ( ( 1.0D+00 - abscissa(qy) ) * y(n)   &
               + ( 1.0D+00 + abscissa(qy) ) * y(s) ) &
                 / 2.0D+00

          wq = weight(qx) * ( x(e) - x(w) ) / 2.0D+00 &
             * weight(qy) * ( y(n) - y(s) ) / 2.0D+00

          call basisd_serene ( xq, yq, xw, ys, xe, yn, xx, yy, vx, vy )

          uxq = 0.0D+00
          uyq = 0.0D+00
          do k = 1, 8
            uxq = uxq + u(node(k)) * vx(k)
            uyq = uyq + u(node(k)) * vy(k)
          end do

          exq = exact_ux ( xq, yq )
          eyq = exact_uy ( xq, yq )

          h1s = h1s + wq * ( ( uxq - exq )**2 + ( uyq - eyq )**2 )
 
        end do
      end do
    end do
  end do

  h1s = sqrt ( h1s )

  return
end
subroutine fem2d_l1_error_serene ( nx, ny, x, y, u, exact, e1 )

!*****************************************************************************80
!
!! FEM2D_L1_ERROR_SERENE: l1 error norm of a finite element solution.
!
!  Discussion:
!
!    We assume the finite element method has been used, over a product
!    region with NX*NY nodes and the serendipity element.
!
!    The coefficients U(*) have been computed, and a formula for the
!    exact solution is known.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, the number of nodes in the X and 
!    Y directions.
!
!    Input, real ( kind = 8 ) X(NX), Y(NY), the X and Y grid values.
!
!    Input, real ( kind = 8 ) U(*), the finite element coefficients.
!
!    Input, function EQ = EXACT(X,Y), returns the value of the exact
!    solution at the point (X,Y).
!
!    Output, real ( kind = 8 ) E1, the little l1 norm of the error.
!
  implicit none

  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny

  real ( kind = 8 ) e1
  real ( kind = 8 ), external :: exact
  integer ( kind = 4 ) i
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) u(nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)
  real ( kind = 8 ) x(nx)
  real ( kind = 8 ) y(ny)

  e1 = 0.0D+00
  k = 0
  do j = 1, ny
    if ( mod ( j, 2 ) == 1 ) then
      inc = 1
    else
      inc = 2
    end if
    do i = 1, nx, inc
      k = k + 1
      e1 = e1 + abs ( u(k) - exact ( x(i), y(j) ) )
    end do
  end do
!
!  Average the error.
!
  e1 = e1 / real ( k, kind = 8 )

  return
end
subroutine fem2d_l2_error_serene ( nx, ny, x, y, u, exact, e2 )

!*****************************************************************************80
!
!! FEM2D_L2_ERROR_SERENE: L2 error norm of a finite element solution.
!
!  Discussion:
!
!    The finite element method has been used, over a rectangle,
!    involving a grid of NXxNY nodes, with serendipity elements used 
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
!    01 July 2014
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
!    Input, real ( kind = 8 ) U(*), the finite element coefficients.
!
!    Input, function EQ = EXACT(X,Y), returns the value of the exact
!    solution at the point (X,Y).
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
  integer ( kind = 4 ) cc
  integer ( kind = 4 ) e
  real ( kind = 8 ) e2
  real ( kind = 8 ) eq
  integer ( kind = 4 ) ex
  integer ( kind = 4 ) ex_num
  real ( kind = 8 ), external :: exact
  integer ( kind = 4 ) ey
  integer ( kind = 4 ) ey_num
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mm
  integer ( kind = 4 ) n
  integer ( kind = 4 ) node(8)
  integer ( kind = 4 ) qx
  integer ( kind = 4 ) qy
  integer ( kind = 4 ) s
  real ( kind = 8 ) u(nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)
  real ( kind = 8 ) uq
  real ( kind = 8 ) v(8)
  integer ( kind = 4 ) w
  real ( kind = 8 ), dimension ( quad_num ) :: weight = (/ &
    0.555555555555555555555555555556D+00, &
    0.888888888888888888888888888889D+00, &
    0.555555555555555555555555555556D+00 /)
  real ( kind = 8 ) wq
  real ( kind = 8 ) x(nx)
  real ( kind = 8 ) xc
  real ( kind = 8 ) xe
  real ( kind = 8 ) xq
  real ( kind = 8 ) xw
  real ( kind = 8 ) xx(8)
  real ( kind = 8 ) y(ny)
  real ( kind = 8 ) ym
  real ( kind = 8 ) yn
  real ( kind = 8 ) yq
  real ( kind = 8 ) ys
  real ( kind = 8 ) yy(8)

  e2 = 0.0D+00
!
!  Compute the matrix entries by integrating over each element.
!
  ex_num = ( nx - 1 ) / 2
  ey_num = ( ny - 1 ) / 2

  do ey = 1, ey_num

    s = 2 * ey - 1
    mm = 2 * ey
    n = 2 * ey + 1

    ys = y(s)
    ym = y(mm)
    yn = y(n)

    yy(1) = y(n)
    yy(2) = y(n)
    yy(3) = y(n)
    yy(4) = y(mm)
    yy(5) = y(s)
    yy(6) = y(s)
    yy(7) = y(s)
    yy(8) = y(mm)

    do ex = 1, ex_num

      w = 2 * ex - 1
      cc = 2 * ex
      e = 2 * ex + 1

      xe = x(e)
      xc = x(cc)
      xw = x(w)

      xx(1) = x(e)
      xx(2) = x(cc)
      xx(3) = x(w)
      xx(4) = x(w)
      xx(5) = x(w)
      xx(6) = x(cc)
      xx(7) = x(e)
      xx(8) = x(e)
!
!  Node indices
!
!  3  2  1  wn  cn  en
!  4     8  wm      em
!  5  6  7  ws  cs  es
!
      node(1) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex + 1
      node(2) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex
      node(3) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex - 1
      node(4) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex - 1
      node(5) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 3
      node(6) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 2
      node(7) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 1
      node(8) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex

      do qx = 1, quad_num

        xq = ( ( 1.0D+00 - abscissa(qx) ) * x(e)   &
             + ( 1.0D+00 + abscissa(qx) ) * x(w) ) &
               / 2.0D+00

        do qy = 1, quad_num

          yq = ( ( 1.0D+00 - abscissa(qy) ) * y(n)   &
               + ( 1.0D+00 + abscissa(qy) ) * y(s) ) &
                 / 2.0D+00

          wq = weight(qx) * ( x(e) - x(w) ) / 2.0D+00 &
             * weight(qy) * ( y(n) - y(s) ) / 2.0D+00

          call basis_serene ( xq, yq, xw, ys, xe, yn, xx, yy, v )

          uq = 0.0D+00
          do k = 1, 8
            uq = uq + u(node(k)) * v(k)
          end do

          eq = exact ( xq, yq )
          e2 = e2 + wq * ( uq - eq )**2
 
        end do
      end  do
    end do
  end do

  e2 = sqrt ( e2 )

  return
end
function not1 ( x1, x2, x3 )

!*****************************************************************************80
!
!! NOT1 evaluates a factor for serendipity basis functions.
!
!  Discussion:
!
!    not1(x1,x2,x3) evaluates at the point x1, the basis factor that
!    is 0 at x2 and 1 at x3:
!
!      not1(x1,x2,x3) = ( x1 - x2 ) / ( x3 - x2 )  
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X1, the evaluation point.
!
!    Input, real ( kind = 8 ) X2, X3, values that define the factor.
!
!    Output, real ( kind = 8 ) NOT1, the value of the basis function factor.
!
  implicit none

  real ( kind = 8 ) not1
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3

  not1 = ( x1 - x2 ) / ( x3 - x2 )

  return
end
function not1d ( x2, x3 )

!*****************************************************************************80
!
!! NOT1D differentiates a factor for serendipity basis functions.
!
!  Discussion:
!
!    not1(x1,x2,x3) evaluates at the point x1, the basis factor that
!    is 0 at x2 and 1 at x3:
!
!      not1(x1,x2,x3) = ( x1 - x2 ) / ( x3 - x2 )  
!
!    This function returns the derivative of the factor with respect to x1:
!
!      not1d(x1,x2,x3) = 1 / ( x3 - x2 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X2, X3, values that define the factor.
!
!    Output, real ( kind = 8 ) NOT1D, the derivative of the basis function 
!    factor.
!
  implicit none

  real ( kind = 8 ) not1d
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3

  not1d = 1.0D+00 / ( x3 - x2 )

  return
end
function not2 ( x1, y1, x2, y2, x3, y3, x4, y4 )

!*****************************************************************************80
!
!! NOT2 evaluates a factor for serendipity basis functions.
!
!  Discussion:
!
!    not2(x1,y1,x2,y2,x3,y3,x4,y4) evaluates at the point (x1,y1), the basis 
!    factor that is 0 at (x2,y2) and (x3,y3) and 1 at (x4,y4):
!
!          ( ( x1 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y1 - y2 ) )
!        / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X1, Y2, the evaluation point.
!
!    Input, real ( kind = 8 ) X2, Y2, X3, Y3, values that define the factor.
!
!    Output, real ( kind = 8 ) NOT2, the value of the basis function factor.
!
  implicit none

  real ( kind = 8 ) not2
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) x4
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) y4

  not2 = ( ( x1 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y1 - y2 ) ) &
       / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )

  return
end
function not2dx ( x2, y2, x3, y3, x4, y4 )

!*****************************************************************************80
!
!! NOT2DX evaluates a factor for serendipity basis functions.
!
!  Discussion:
!
!    not2(x1,y1,x2,y2,x3,y3,x4,y4) evaluates at the point (x1,y1), the basis 
!    factor that is 0 at (x2,y2) and (x3,y3) and 1 at (x4,y4):
!
!          ( ( x1 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y1 - y2 ) )
!        / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )
!
!    not2dx returns the derivative of this function with respect to X1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X2, Y2, X3, Y3, values that define the factor.
!
!    Output, real ( kind = 8 ) NOT2DX, the derivative of the basis function 
!    factor with respect to X1.
!
  implicit none

  real ( kind = 8 ) not2dx
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) x4
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) y4

  not2dx = (   1.0D+00   * ( y3 - y2 ) +                 0.0D+00   ) &
         / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )

  return
end
function not2dy ( x2, y2, x3, y3, x4, y4 )

!*****************************************************************************80
!
!! NOT2DY evaluates a factor for serendipity basis functions.
!
!  Discussion:
!
!    not2(x1,y1,x2,y2,x3,y3,x4,y4) evaluates at the point (x1,y1), the basis 
!    factor that is 0 at (x2,y2) and (x3,y3) and 1 at (x4,y4):
!
!          ( ( x1 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y1 - y2 ) )
!        / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )
!
!    not2dy returns the derivatives of this function with respect to Y1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X2, Y2, X3, Y3, values that define the factor.
!
!    Output, real ( kind = 8 ) NOT2DY, the derivative of the basis function 
!    factor with respect to Y1.
!
  implicit none

  real ( kind = 8 ) not2dy
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) x4
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) y4

  not2dy = (   0.0D+00                 - ( x3 - x2 ) *   1.0D+00   ) &
         / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )

  return
end
function r8_uniform_01 ( seed )

!*****************************************************************************80
!
!! R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer variable.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2^31 - 1 )
!      r8_uniform_01 = seed / ( 2^31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R8_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0. On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
  implicit none

  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_01 = real ( seed, kind = 8 ) * 4.656612875D-10

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
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
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
subroutine wathen ( nx, ny, n, a )

!*****************************************************************************80
!
!! WATHEN returns the WATHEN matrix.
!
!  Discussion:
!
!    The Wathen matrix is a finite element matrix which is sparse.
!
!    The entries of the matrix depend in part on a physical quantity
!    related to density.  That density is here assigned random values between
!    0 and 100.
!
!    The matrix order N is determined by the input quantities NX and NY,
!    which would usually be the number of elements in the X and Y directions.
!    The value of N is
!
!      N = 3*NX*NY + 2*NX + 2*NY + 1,
!
!    and sufficient storage in A must have been set aside to hold
!    the matrix.
!
!    A is the consistent mass matrix for a regular NX by NY grid
!    of 8 node serendipity elements.  
!
!    The local element numbering is
!
!      3--2--1
!      |     |
!      4     8
!      |     |
!      5--6--7
!
!    Here is an illustration for NX = 3, NX = 2:
!
!     23-24-25-26-27-28-29
!      |     |     |     |
!     19    20    21    22
!      |     |     |     |
!     12-13-14-15-16-17-18
!      |     |     |     |
!      8     9    10    11
!      |     |     |     |
!      1--2--3--4--5--6--7
!
!    For this example, the total number of nodes is, as expected,
!
!      N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
!
!  Properties:
!
!    A is symmetric positive definite for any positive values of the
!    density RHO(NX,NY), which is here given the value 1.
!
!    The problem could be reprogrammed so that RHO is nonconstant,
!    but positive.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Nicholas Higham,
!    Algorithm 694: A Collection of Test Matrices in MATLAB,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 3, September 1991, pages 289-305.
!
!    Andrew Wathen,
!    Realistic eigenvalue bounds for the Galerkin mass matrix,
!    IMA Journal of Numerical Analysis,
!    Volume 7, Number 4, October 1987, pages 449-457.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, values which determine the size of A.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Output, real ( kind = 8 ) A(N,N), the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ), dimension ( 8, 8 ), save :: em =  reshape ( (/ &
     6.0, -6.0,  2.0, -8.0,  3.0, -8.0,  2.0, -6.0, &
    -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, &
     2.0, -6.0,  6.0, -6.0,  2.0, -8.0,  3.0, -8.0, &
    -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, &
     3.0, -8.0,  2.0, -6.0,  6.0, -6.0,  2.0, -8.0, &
    -8.0, 16.0, -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, &
     2.0, -8.0,  3.0, -8.0,  2.0, -6.0,  6.0, -6.0, &
    -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, -6.0, 32.0 /), &
    (/ 8, 8 /) )
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) kcol
  integer ( kind = 4 ) krow
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) node(8)
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) rho
  integer ( kind = 4 ), save :: seed = 123456789

  a(1:n,1:n) = 0.0D+00

  do j = 1, ny

    do i = 1, nx
!
!  For the element (I,J), determine the indices of the 8 nodes.
!
      node(1) = 3 * j * nx + 2 * j + 2 * i + 1
      node(2) = node(1) - 1
      node(3) = node(1) - 2
      node(4) = ( 3 * j - 1 ) * nx + 2 * j + i - 1
      node(5) = ( 3 * j - 3 ) * nx + 2 * j + 2 * i - 3
      node(6) = node(5) + 1
      node(7) = node(5) + 2
      node(8) = node(4) + 1

      rho = 100.0D+00 * r8_uniform_01 ( seed )

      do krow = 1, 8
        do kcol = 1, 8

          a(node(krow),node(kcol)) = a(node(krow),node(kcol)) &
            + rho * em(krow,kcol)

        end do
      end do

    end do
  end do

  return
end
