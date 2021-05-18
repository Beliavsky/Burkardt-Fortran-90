program main

!*****************************************************************************80
!
!! MAIN is the main program for FEM2D_BVP_LINEAR_TEST.
!
!  Discussion:
!
!    FEM2D_BVP_LINEAR_TEST tests the FEM2D_BVP_LINEAR library.
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
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FEM2D_BVP_LINEAR_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the FEM2D_BVP_LINEAR library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FEM2D_BVP_LINEAR_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 carries out test case #1.
!
!  Discussion:
!
!    Use A1, C1, F1, EXACT1, EXACT_UX1, EXACT_UY1.
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
!  Reference:
!
!    Dianne O'Leary,
!    Scientific Computing with Case Studies,
!    SIAM, 2008,
!    ISBN13: 978-0-898716-66-5,
!    LC: QA401.O44.
!
  implicit none

  integer ( kind = 4 ), parameter :: nx = 3
  integer ( kind = 4 ), parameter :: ny = 3

  real ( kind = 8 ), external :: a1
  real ( kind = 8 ), external :: c1
  real ( kind = 8 ), external :: exact1
  real ( kind = 8 ), external :: exact_ux1
  real ( kind = 8 ), external :: exact_uy1
  real ( kind = 8 ), external :: f1
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) h1s
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) u(nx,ny)
  real ( kind = 8 ) uexact
  real ( kind = 8 ) x(nx)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last
  real ( kind = 8 ) y(nx)
  real ( kind = 8 ) y_first
  real ( kind = 8 ) y_last

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Solve - del ( A del U ) + C U = F '
  write ( *, '(a)' ) '  on the unit square with zero boundary conditions.'
  write ( *, '(a)' ) '  A1(X,Y) = 1.0'
  write ( *, '(a)' ) '  C1(X,Y) = 0.0'
  write ( *, '(a)' ) '  F1(X,Y) = 2*X*(1-X)+2*Y*(1-Y)'
  write ( *, '(a)' ) '  U1(X,Y) = X * ( 1 - X ) * Y * ( 1 - Y )'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of X grid values NX = ', nx
  write ( *, '(a,i8)' ) '  Number of Y grid values NY = ', ny
!
!  Geometry definitions.
!
  x_first = 0.0D+00
  x_last = 1.0D+00
  call r8vec_even ( nx, x_first, x_last, x )

  y_first = 0.0D+00
  y_last = 1.0D+00
  call r8vec_even ( ny, y_first, y_last, y )

  call fem2d_bvp_linear ( nx, ny, a1, c1, f1, x, y, u )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I     J    X         Y         U         Uexact    Error'
  write ( *, '(a)' ) ' '

  do j = 1, ny
    do i = 1, nx
      uexact = exact1 ( x(i), y(j) )
      write ( *, '(2x,i4,2x,i4,2x,f8.2,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
        i, j, x(i), y(j), u(i,j), uexact, abs ( u(i,j) - uexact )
    end do
  end do

  call fem2d_l1_error ( nx, ny, x, y, u, exact1, e1 )
  call fem2d_l2_error_linear ( nx, ny, x, y, u, exact1, e2 )
  call fem2d_h1s_error_linear ( nx, ny, x, y, u, exact_ux1, exact_uy1, h1s )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
  write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
  write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s

  return
end
function a1 ( x, y )

!*****************************************************************************80
!
!! A1 evaluates A function #1.
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
!    Input, real ( kind = 8 ) X, Y, the evaluation point.
!
!    Output, real ( kind = 8 ) A1, the value of A(X,Y).
!
  implicit none

  real ( kind = 8 ) a1
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  a1 = 1.0D+00

  return
end
function c1 ( x, y )

!*****************************************************************************80
!
!! C1 evaluates C function #1.
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
!    Input, real ( kind = 8 ) X, Y, the evaluation point.
!
!    Output, real ( kind = 8 ) C1, the value of C(X,Y).
!
  implicit none

  real ( kind = 8 ) c1
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  c1 = 0.0D+00

  return
end
function exact1 ( x, y )

!*****************************************************************************80
!
!! EXACT1 evaluates exact solution #1.
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
!    Input, real ( kind = 8 ) X, Y, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT1, the value of U(X,Y).
!
  implicit none

  real ( kind = 8 ) exact1
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  exact1 = x * ( 1.0D+00 - x ) * y * ( 1.0D+00 - y )

  return
end
function exact_ux1 ( x, y )

!*****************************************************************************80
!
!! EXACT_UX1 evaluates the X derivative of the exact solution #1.
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
!    Input, real ( kind = 8 ) X, Y, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX1, the value of dUdX(X,Y).
!
  implicit none

  real ( kind = 8 ) exact_ux1
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  exact_ux1 = ( 1.0D+00 - 2.0D+00 * x ) * ( y - y * y )

  return
end
function exact_uy1 ( x, y )

!*****************************************************************************80
!
!! EXACT_UY1 evaluates the Y derivative of the exact solution #1.
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
!    Input, real ( kind = 8 ) X, Y, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UY1, the value of dUdY(X,Y).
!
  implicit none

  real ( kind = 8 ) exact_uy1
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  exact_uy1 = ( x - x * x ) * ( 1.0D+00 - 2.0D+00 * y )

  return
end
function f1 ( x, y )

!*****************************************************************************80
!
!! F1 evaluates right hand side function #1.
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
!    Input, real ( kind = 8 ) X, Y, the evaluation point.
!
!    Output, real ( kind = 8 ) F1, the value of F(X,Y).
!
  implicit none

  real ( kind = 8 ) f1
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  f1 = 2.0D+00 * x * ( 1.0D+00 - x ) &
     + 2.0D+00 * y * ( 1.0D+00 - y )

  return
end

