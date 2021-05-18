program main

!*****************************************************************************80
!
!! MAIN is the main program for FEM1D_BVP_QUADRATIC_TEST.
!
!  Discussion:
!
!    FEM1D_BVP_QUADRATIC_TEST tests the FEM1D_BVP_QUADRATIC library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FEM1D_BVP_QUADRATIC_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the FEM1D_BVP_QUADRATIC library.'

  call test00 ( )
  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
  call test06 ( )
  call test07 ( )
  call test08 ( )
  call test09 ( )
  call test10 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FEM1D_BVP_QUADRATIC_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test00 ( )

!*****************************************************************************80
!
!! TEST00 carries out test case #0.
!
!  Discussion:
!
!    - uxx + u = x  for 0 < x < 1
!    u(0) = u(1) = 0
!
!    exact  = x - sinh(x) / sinh(1)
!    exact' = 1 - cosh(x) / sinh(1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2015
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

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ), external :: a00
  real ( kind = 8 ), external :: c00
  real ( kind = 8 ), external :: exact00
  real ( kind = 8 ), external :: exact_ux00
  real ( kind = 8 ), external :: f00
  integer ( kind = 4 ) i
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) h1s
  real ( kind = 8 ) mx
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) uexact
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST00'
  write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
  write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
  write ( *, '(a)' ) '  A(X)  = 1.0'
  write ( *, '(a)' ) '  C(X)  = 1.0'
  write ( *, '(a)' ) '  F(X)  = X'
  write ( *, '(a)' ) '  U(X)  = X - SINH(X) / SINH(1)'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of nodes = ', n
!
!  Geometry definitions.
!
  x_first = 0.0D+00
  x_last = 1.0D+00
  call r8vec_linspace ( n, x_first, x_last, x )

  call fem1d_bvp_quadratic ( n, a00, c00, f00, x, u )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I    X         U         Uexact    Error'
  write ( *, '(a)' ) ' '

  do i = 1, n
    uexact = exact00 ( x(i) )
    write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, x(i), u(i), uexact, abs ( u(i) - uexact )
  end do

  call l1_error ( n, x, u, exact00, e1 )
  call l2_error_quadratic ( n, x, u, exact00, e2 )
  call h1s_error_quadratic ( n, x, u, exact_ux00, h1s )
  call max_error_quadratic ( n, x, u, exact00, mx )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
  write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
  write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s
  write ( *, '(a,g14.6)' ) '  Max norm of error = ', mx

  return
end
function a00 ( x )

!*****************************************************************************80
!
!! A00 evaluates A function #0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) A00, the value of A(X).
!
  implicit none

  real ( kind = 8 ) a00
  real ( kind = 8 ) x

  a00 = 1.0D+00

  return
end
function c00 ( x )

!*****************************************************************************80
!
!! C00 evaluates C function #0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) C00, the value of C(X).
!
  implicit none

  real ( kind = 8 ) c00
  real ( kind = 8 ) x

  c00 = 1.0D+00

  return
end
function exact00 ( x )

!*****************************************************************************80
!
!! EXACT00 evaluates exact solution #0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT00, the value of U(X).
!
  implicit none

  real ( kind = 8 ) exact00
  real ( kind = 8 ) x

  exact00 = x - sinh ( x ) / sinh ( 1.0D+00 )

  return
end
function exact_ux00 ( x )

!*****************************************************************************80
!
!! EXACT_UX00 evaluates the derivative of exact solution #0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX00, the value of dUdX(X).
!
  implicit none

  real ( kind = 8 ) exact_ux00
  real ( kind = 8 ) x

  exact_ux00 = 1.0D+00 - cosh ( x ) / sinh ( 1.0D+00 )

  return
end
function f00 ( x )

!*****************************************************************************80
!
!! F00 evaluates right hand side function #0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F00, the value of F(X).
!
  implicit none

  real ( kind = 8 ) f00
  real ( kind = 8 ) x

  f00 = x

  return
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 carries out test case #1.
!
!  Discussion:
!
!    Use A1, C1, F1, EXACT1, EXACT_UX1.
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
!  Reference:
!
!    Dianne O'Leary,
!    Scientific Computing with Case Studies,
!    SIAM, 2008,
!    ISBN13: 978-0-898716-66-5,
!    LC: QA401.O44.
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ), external :: a1
  real ( kind = 8 ), external :: c1
  real ( kind = 8 ), external :: exact1
  real ( kind = 8 ), external :: exact_ux1
  real ( kind = 8 ), external :: f1
  integer ( kind = 4 ) i
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) h1s
  real ( kind = 8 ) mx
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) uexact
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
  write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
  write ( *, '(a)' ) '  A1(X)  = 1.0'
  write ( *, '(a)' ) '  C1(X)  = 0.0'
  write ( *, '(a)' ) '  F1(X)  = X * ( X + 3 ) * exp ( X )'
  write ( *, '(a)' ) '  U1(X)  = X * ( 1 - X ) * exp ( X )'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of nodes = ', n
!
!  Geometry definitions.
!
  x_first = 0.0D+00
  x_last = 1.0D+00
  call r8vec_linspace ( n, x_first, x_last, x )

  call fem1d_bvp_quadratic ( n, a1, c1, f1, x, u )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I    X         U         Uexact    Error'
  write ( *, '(a)' ) ' '

  do i = 1, n
    uexact = exact1 ( x(i) )
    write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, x(i), u(i), uexact, abs ( u(i) - uexact )
  end do

  call l1_error ( n, x, u, exact1, e1 )
  call l2_error_quadratic ( n, x, u, exact1, e2 )
  call h1s_error_quadratic ( n, x, u, exact_ux1, h1s )
  call max_error_quadratic ( n, x, u, exact1, mx )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
  write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
  write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s
  write ( *, '(a,g14.6)' ) '  Max norm of error = ', mx

  return
end
function a1 ( x )

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
!    14 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) A1, the value of A(X).
!
  implicit none

  real ( kind = 8 ) a1
  real ( kind = 8 ) x

  a1 = 1.0D+00

  return
end
function c1 ( x )

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
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) C1, the value of C(X).
!
  implicit none

  real ( kind = 8 ) c1
  real ( kind = 8 ) x

  c1 = 0.0D+00

  return
end
function exact1 ( x )

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
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT1, the value of U(X).
!
  implicit none

  real ( kind = 8 ) exact1
  real ( kind = 8 ) x

  exact1 = x * ( 1.0D+00 - x ) * exp ( x )

  return
end
function exact_ux1 ( x )

!*****************************************************************************80
!
!! EXACT_UX1 evaluates the derivative of exact solution #1.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX1, the value of dUdX(X).
!
  implicit none

  real ( kind = 8 ) exact_ux1
  real ( kind = 8 ) x

  exact_ux1 = ( 1.0D+00 - x - x * x ) * exp ( x )

  return
end
function f1 ( x )

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
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F1, the value of F(X).
!
  implicit none

  real ( kind = 8 ) f1
  real ( kind = 8 ) x

  f1 = x * ( x + 3.0D+00 ) * exp ( x )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 carries out test case #2.
!
!  Discussion:
!
!    Use A2, C2, F2, EXACT2, EXACT_UX2.
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
!  Reference:
!
!    Dianne O'Leary,
!    Scientific Computing with Case Studies,
!    SIAM, 2008,
!    ISBN13: 978-0-898716-66-5,
!    LC: QA401.O44.
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ), external :: a2
  real ( kind = 8 ), external :: c2
  real ( kind = 8 ), external :: exact2
  real ( kind = 8 ), external :: exact_ux2
  real ( kind = 8 ), external :: f2
  integer ( kind = 4 ) i
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) h1s
  real ( kind = 8 ) mx
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) uexact
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
  write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
  write ( *, '(a)' ) '  A2(X)  = 1.0'
  write ( *, '(a)' ) '  C2(X)  = 2.0'
  write ( *, '(a)' ) '  F2(X)  = X * ( 5 - X ) * exp ( X )'
  write ( *, '(a)' ) '  U2(X)  = X * ( 1 - X ) * exp ( X )'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of nodes = ', n
!
!  Geometry definitions.
!
  x_first = 0.0D+00
  x_last = 1.0D+00
  call r8vec_linspace ( n, x_first, x_last, x )

  call fem1d_bvp_quadratic ( n, a2, c2, f2, x, u )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I    X         U         Uexact    Error'
  write ( *, '(a)' ) ' '

  do i = 1, n
    uexact = exact2 ( x(i) )
    write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, x(i), u(i), uexact, abs ( u(i) - uexact )
  end do

  call l1_error ( n, x, u, exact2, e1 )
  call l2_error_quadratic ( n, x, u, exact2, e2 )
  call h1s_error_quadratic ( n, x, u, exact_ux2, h1s )
  call max_error_quadratic ( n, x, u, exact2, mx )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
  write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
  write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s
  write ( *, '(a,g14.6)' ) '  Max norm of error = ', mx

  return
end
function a2 ( x )

!*****************************************************************************80
!
!! A2 evaluates A function #2.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) A2, the value of A(X).
!
  implicit none

  real ( kind = 8 ) a2
  real ( kind = 8 ) x

  a2 = 1.0D+00

  return
end
function c2 ( x )

!*****************************************************************************80
!
!! C2 evaluates C function #2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) C2, the value of C(X).
!
  implicit none

  real ( kind = 8 ) c2
  real ( kind = 8 ) x

  c2 = 2.0D+00

  return
end
function exact2 ( x )

!*****************************************************************************80
!
!! EXACT2 evaluates exact solution #2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT2, the value of U(X).
!
  implicit none

  real ( kind = 8 ) exact2
  real ( kind = 8 ) x

  exact2 = x * ( 1.0D+00 - x ) * exp ( x )

  return
end
function exact_ux2 ( x )

!*****************************************************************************80
!
!! EXACT_UX2 evaluates the derivative of exact solution #2.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX2, the value of dUdX(X).
!
  implicit none

  real ( kind = 8 ) exact_ux2
  real ( kind = 8 ) x

  exact_ux2 = ( 1.0D+00 - x - x * x ) * exp ( x )

  return
end
function f2 ( x )

!*****************************************************************************80
!
!! F2 evaluates right hand side function #2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F2, the value of F(X).
!
  implicit none

  real ( kind = 8 ) f2
  real ( kind = 8 ) x

  f2 = x * ( 5.0D+00 - x ) * exp ( x )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 carries out test case #3.
!
!  Discussion:
!
!    Use A3, C3, F3, EXACT3, EXACT_UX3.
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
!  Reference:
!
!    Dianne O'Leary,
!    Scientific Computing with Case Studies,
!    SIAM, 2008,
!    ISBN13: 978-0-898716-66-5,
!    LC: QA401.O44.
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ), external :: a3
  real ( kind = 8 ), external :: c3
  real ( kind = 8 ), external :: exact3
  real ( kind = 8 ), external :: exact_ux3
  real ( kind = 8 ), external :: f3
  integer ( kind = 4 ) i
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) h1s
  real ( kind = 8 ) mx
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) uexact
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
  write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
  write ( *, '(a)' ) '  A3(X)  = 1.0'
  write ( *, '(a)' ) '  C3(X)  = 2.0 * X'
  write ( *, '(a)' ) '  F3(X)  = - X * ( 2 * X * X - 3 * X - 3 ) * exp ( X )'
  write ( *, '(a)' ) '  U3(X)  = X * ( 1 - X ) * exp ( X )'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of nodes = ', n
!
!  Geometry definitions.
!
  x_first = 0.0D+00
  x_last = 1.0D+00
  call r8vec_linspace ( n, x_first, x_last, x )

  call fem1d_bvp_quadratic ( n, a3, c3, f3, x, u )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I    X         U         Uexact    Error'
  write ( *, '(a)' ) ' '

  do i = 1, n
    uexact = exact3 ( x(i) )
    write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, x(i), u(i), uexact, abs ( u(i) - uexact )
  end do

  call l1_error ( n, x, u, exact3, e1 )
  call l2_error_quadratic ( n, x, u, exact3, e2 )
  call h1s_error_quadratic ( n, x, u, exact_ux3, h1s )
  call max_error_quadratic ( n, x, u, exact3, mx )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
  write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
  write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s
  write ( *, '(a,g14.6)' ) '  Max norm of error = ', mx

  return
end
function a3 ( x )

!*****************************************************************************80
!
!! A3 evaluates A function #3.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) A3, the value of A(X).
!
  implicit none

  real ( kind = 8 ) a3
  real ( kind = 8 ) x

  a3 = 1.0D+00

  return
end
function c3 ( x )

!*****************************************************************************80
!
!! C3 evaluates C function #3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) C3, the value of C(X).
!
  implicit none

  real ( kind = 8 ) c3
  real ( kind = 8 ) x

  c3 = 2.0D+00 * x

  return
end
function exact3 ( x )

!*****************************************************************************80
!
!! EXACT3 evaluates exact solution #3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT3, the value of U(X).
!
  implicit none

  real ( kind = 8 ) exact3
  real ( kind = 8 ) x

  exact3 = x * ( 1.0D+00 - x ) * exp ( x )

  return
end
function exact_ux3 ( x )

!*****************************************************************************80
!
!! EXACT_UX3 evaluates the derivative of exact solution #3.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX3, the value of dUdX(X).
!
  implicit none

  real ( kind = 8 ) exact_ux3
  real ( kind = 8 ) x

  exact_ux3 = ( 1.0D+00 - x - x * x ) * exp ( x )

  return
end
function f3 ( x )

!*****************************************************************************80
!
!! F3 evaluates right hand side function #3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F3, the value of F(X).
!
  implicit none

  real ( kind = 8 ) f3
  real ( kind = 8 ) x

  f3 = - x * ( 2.0D+00 * x * x - 3.0D+00 * x - 3.0D+00 ) * exp ( x )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 carries out test case #4.
!
!  Discussion:
!
!    Use A4, C4, F4, EXACT4, EXACT_UX4.
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
!  Reference:
!
!    Dianne O'Leary,
!    Scientific Computing with Case Studies,
!    SIAM, 2008,
!    ISBN13: 978-0-898716-66-5,
!    LC: QA401.O44.
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ), external :: a4
  real ( kind = 8 ), external :: c4
  real ( kind = 8 ), external :: exact4
  real ( kind = 8 ), external :: exact_ux4
  real ( kind = 8 ), external :: f4
  integer ( kind = 4 ) i
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) h1s
  real ( kind = 8 ) mx
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) uexact
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
  write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
  write ( *, '(a)' ) '  A4(X)  = 1.0 + X * X'
  write ( *, '(a)' ) '  C4(X)  = 0.0'
  write ( *, '(a)' ) '  F4(X)  = ( X + 3 X^2 + 5 X^3 + X^4 ) * exp ( X )'
  write ( *, '(a)' ) '  U4(X)  = X * ( 1 - X ) * exp ( X )'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of nodes = ', n
!
!  Geometry definitions.
!
  x_first = 0.0D+00
  x_last = 1.0D+00
  call r8vec_linspace ( n, x_first, x_last, x )

  call fem1d_bvp_quadratic ( n, a4, c4, f4, x, u )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I    X         U         Uexact    Error'
  write ( *, '(a)' ) ' '

  do i = 1, n
    uexact = exact4 ( x(i) )
    write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, x(i), u(i), uexact, abs ( u(i) - uexact )
  end do

  call l1_error ( n, x, u, exact4, e1 )
  call l2_error_quadratic ( n, x, u, exact4, e2 )
  call h1s_error_quadratic ( n, x, u, exact_ux4, h1s )
  call max_error_quadratic ( n, x, u, exact4, mx )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
  write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
  write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s
  write ( *, '(a,g14.6)' ) '  Max norm of error = ', mx

  return
end
function a4 ( x )

!*****************************************************************************80
!
!! A4 evaluates A function #4.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) A4, the value of A(X).
!
  implicit none

  real ( kind = 8 ) a4
  real ( kind = 8 ) x

  a4 = 1.0D+00 + x * x

  return
end
function c4 ( x )

!*****************************************************************************80
!
!! C4 evaluates C function #4.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) C4, the value of C(X).
!
  implicit none

  real ( kind = 8 ) c4
  real ( kind = 8 ) x

  c4 = 0.0D+00

  return
end
function exact4 ( x )

!*****************************************************************************80
!
!! EXACT4 evaluates exact solution #4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT4, the value of U(X).
!
  implicit none

  real ( kind = 8 ) exact4
  real ( kind = 8 ) x

  exact4 = x * ( 1.0D+00 - x ) * exp ( x )

  return
end
function exact_ux4 ( x )

!*****************************************************************************80
!
!! EXACT_UX4 evaluates the derivative of exact solution #4.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX4, the value of dUdX(X).
!
  implicit none

  real ( kind = 8 ) exact_ux4
  real ( kind = 8 ) x

  exact_ux4 = ( 1.0D+00 - x - x * x ) * exp ( x )

  return
end
function f4 ( x )

!*****************************************************************************80
!
!! F4 evaluates right hand side function #4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F4, the value of F(X).
!
  implicit none

  real ( kind = 8 ) f4
  real ( kind = 8 ) x

  f4 = ( x + 3.0D+00 * x * x + 5.0D+00 * x * x * x + x * x * x * x ) * exp ( x )

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 carries out test case #5.
!
!  Discussion:
!
!    Use A5, C5, F5, EXACT5, EXACT_UX5.
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
!  Reference:
!
!    Dianne O'Leary,
!    Scientific Computing with Case Studies,
!    SIAM, 2008,
!    ISBN13: 978-0-898716-66-5,
!    LC: QA401.O44.
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ), external :: a5
  real ( kind = 8 ), external :: c5
  real ( kind = 8 ), external :: exact5
  real ( kind = 8 ), external :: exact_ux5
  real ( kind = 8 ), external :: f5
  integer ( kind = 4 ) i
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) h1s
  real ( kind = 8 ) mx
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) uexact
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
  write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
  write ( *, '(a)' ) '  A5(X)  = 1.0 + X * X for X <= 1/3'
  write ( *, '(a)' ) '         = 7/9 + X     for      1/3 < X'
  write ( *, '(a)' ) '  C5(X)  = 0.0'
  write ( *, '(a)' ) '  F5(X)  = ( X + 3 X^2 + 5 X^3 + X^4 ) * exp ( X )'
  write ( *, '(a)' ) '                       for X <= 1/3'
  write ( *, '(a)' ) '         = ( - 1 + 10/3 X + 43/9 X^2 + X^3 ) .* exp ( X )'
  write ( *, '(a)' ) '                       for      1/3 <= X'
  write ( *, '(a)' ) '  U5(X)  = X * ( 1 - X ) * exp ( X )'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of nodes = ', n
!
!  Geometry definitions.
!
  x_first = 0.0D+00
  x_last = 1.0D+00
  call r8vec_linspace ( n, x_first, x_last, x )

  call fem1d_bvp_quadratic ( n, a5, c5, f5, x, u )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I    X         U         Uexact    Error'
  write ( *, '(a)' ) ' '

  do i = 1, n
    uexact = exact5 ( x(i) )
    write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, x(i), u(i), uexact, abs ( u(i) - uexact )
  end do

  call l1_error ( n, x, u, exact5, e1 )
  call l2_error_quadratic ( n, x, u, exact5, e2 )
  call h1s_error_quadratic ( n, x, u, exact_ux5, h1s )
  call max_error_quadratic ( n, x, u, exact5, mx )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
  write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
  write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s
  write ( *, '(a,g14.6)' ) '  Max norm of error = ', mx

  return
end
function a5 ( x )

!*****************************************************************************80
!
!! A5 evaluates A function #5.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) A5, the value of A(X).
!
  implicit none

  real ( kind = 8 ) a5
  real ( kind = 8 ) x

  if ( x <= 1.0D+00 / 3.0D+00 ) then
    a5 = 1.0D+00 + x * x
  else
    a5 = x + 7.0D+00 / 9.0D+00
  end if

  return
end
function c5 ( x )

!*****************************************************************************80
!
!! C5 evaluates C function #5.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) C5, the value of C(X).
!
  implicit none

  real ( kind = 8 ) c5
  real ( kind = 8 ) x

  c5 = 0.0D+00

  return
end
function exact5 ( x )

!*****************************************************************************80
!
!! EXACT5 evaluates exact solution #5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT5, the value of U(X).
!
  implicit none

  real ( kind = 8 ) exact5
  real ( kind = 8 ) x

  exact5 = x * ( 1.0D+00 - x ) * exp ( x )

  return
end
function exact_ux5 ( x )

!*****************************************************************************80
!
!! EXACT_UX5 evaluates the derivative of exact solution #5.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX5, the value of dUdX(X).
!
  implicit none

  real ( kind = 8 ) exact_ux5
  real ( kind = 8 ) x

  exact_ux5 = ( 1.0D+00 - x - x * x ) * exp ( x )

  return
end
function f5 ( x )

!*****************************************************************************80
!
!! F5 evaluates right hand side function #5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F5, the value of F(X).
!
  implicit none

  real ( kind = 8 ) f5
  real ( kind = 8 ) x

  if ( x <= 1.0D+00 / 3.0D+00 ) then
    f5 = ( x + 3.0D+00 * x * x + 5.0D+00 * x * x * x + x * x * x * x ) &
      * exp ( x )
  else
    f5 = ( - 1.0D+00 + ( 10.0D+00 / 3.0D+00 ) * x &
      + ( 43.0D+00 / 9.0D+00 ) * x * x + x * x * x ) * exp ( x )
  end if

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 does an error analysis.
!
!  Discussion:
!
!    Use A6, C6, F6, EXACT6, EXACT_UX6.
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
  implicit none

  real ( kind = 8 ), external :: a6
  real ( kind = 8 ), external :: c6
  real ( kind = 8 ), external :: exact6
  real ( kind = 8 ), external :: exact_ux6
  real ( kind = 8 ), external :: f6
  integer ( kind = 4 ) i
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  integer ( kind = 4 ) n
  real ( kind = 8 ) h1s
  real ( kind = 8 ) mx
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ) uexact
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST06'
  write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
  write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
  write ( *, '(a)' ) '  A6(X)  = 1.0 '
  write ( *, '(a)' ) '  C6(X)  = 0.0'
  write ( *, '(a)' ) '  F6(X)  = pi*pi*sin(pi*X)'
  write ( *, '(a)' ) '  U6(X)  = sin(pi*x)'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Compute L2 norm and seminorm of error for various N.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     N        L1 error         L2 error      Seminorm error  Maxnorm error'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '

  n = 11

  do i = 0, 4
!
!  Geometry definitions.
!
    x_first = 0.0D+00
    x_last = 1.0D+00
    allocate ( x(1:n) )
    call r8vec_linspace ( n, x_first, x_last, x )

    allocate ( u(1:n) )
    call fem1d_bvp_quadratic ( n, a6, c6, f6, x, u )

    call l1_error ( n, x, u, exact6, e1 )
    call l2_error_quadratic ( n, x, u, exact6, e2 )
    call h1s_error_quadratic ( n, x, u, exact_ux6, h1s )
    call max_error_quadratic ( n, x, u, exact6, mx )

    write ( *, '(2x,i4,2x,f14.6,2x,f14.6,2x,f14.6,2x,g14.6)' ) &
      n, e1, e2, h1s, mx

    deallocate ( u )
    deallocate ( x )

    n = 2 * ( n - 1 ) + 1

  end do

  return
end
function a6 ( x )

!*****************************************************************************80
!
!! A6 evaluates A function #6.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) A6, the value of A(X).
!
  implicit none

  real ( kind = 8 ) a6
  real ( kind = 8 ) x

  a6 = 1.0D+00

  return
end
function c6 ( x )

!*****************************************************************************80
!
!! C6 evaluates C function #6.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) C6, the value of C(X).
!
  implicit none

  real ( kind = 8 ) c6
  real ( kind = 8 ) x

  c6 = 0.0D+00

  return
end
function exact6 ( x )

!*****************************************************************************80
!
!! EXACT6 evaluates exact solution #6.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT6, the value of U(X).
!
  implicit none

  real ( kind = 8 ) exact6
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  exact6 = sin ( pi * x )

  return
end
function exact_ux6 ( x )

!*****************************************************************************80
!
!! EXACT_UX6 evaluates the derivative of exact solution #6.
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
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX6, the value of dUdX(X).
!
  implicit none

  real ( kind = 8 ) exact_ux6
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  exact_ux6 = pi * cos ( pi * x )

  return
end
function f6 ( x )

!*****************************************************************************80
!
!! F6 evaluates right hand side function #6.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F6, the value of F(X).
!
  implicit none

  real ( kind = 8 ) f6
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  f6 = pi * pi * sin ( pi * x )

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! TEST07 does an error analysis.
!
!  Discussion:
!
!    Use A7, C7, F7, EXACT7, EXACT_UX7.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Becker, Graham Carey, John Oden,
!    Finite Elements, An Introduction, Volume I,
!    Prentice-Hall, 1981, page 123-124,
!    ISBN: 0133170578,
!    LC: TA347.F5.B4.
!
  implicit none

  real ( kind = 8 ), external :: a7
  real ( kind = 8 ), external :: c7
  real ( kind = 8 ), external :: exact7
  real ( kind = 8 ), external :: exact_ux7
  real ( kind = 8 ), external :: f7
  integer ( kind = 4 ) i
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  integer ( kind = 4 ) n
  real ( kind = 8 ) h1s
  real ( kind = 8 ) mx
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ) uexact
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST07'
  write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
  write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
  write ( *, '(a)' ) '  Becker/Carey/Oden example'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Compute L2 norm and seminorm of error for various N.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     N        L1 error        L2 error      Seminorm error  Maxnorm error'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '

  n = 11

  do i = 0, 4
!
!  Geometry definitions.
!
    x_first = 0.0D+00
    x_last = 1.0D+00
    allocate ( x(1:n) )
    call r8vec_linspace ( n, x_first, x_last, x )

    allocate ( u(1:n) )
    call fem1d_bvp_quadratic ( n, a7, c7, f7, x, u )

    call l1_error ( n, x, u, exact7, e1 )
    call l2_error_quadratic ( n, x, u, exact7, e2 )
    call h1s_error_quadratic ( n, x, u, exact_ux7, h1s )
    call max_error_quadratic ( n, x, u, exact7, mx )

    write ( *, '(2x,i4,2x,f14.6,2x,f14.6,2x,f14.6,2x,g14.6)' ) &
      n, e1, e2, h1s, mx

    deallocate ( u )
    deallocate ( x )

    n = 2 * ( n - 1 ) + 1

  end do

  return
end
function a7 ( x )

!*****************************************************************************80
!
!! A7 evaluates A function #7.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) A7, the value of A(X).
!
  implicit none

  real ( kind = 8 ) a7
  real ( kind = 8 ) alpha
  real ( kind = 8 ) x
  real ( kind = 8 ) x0

  alpha = 30.0D+00
  x0 = 1.0D+00 / 3.0D+00
  a7 = 1.0D+00 / alpha + alpha * ( x - x0 ) ** 2

  return
end
function c7 ( x )

!*****************************************************************************80
!
!! C7 evaluates C function #7.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) C7, the value of C(X).
!
  implicit none

  real ( kind = 8 ) c7
  real ( kind = 8 ) x

  c7 = 0.0D+00

  return
end
function exact7 ( x )

!*****************************************************************************80
!
!! EXACT7 evaluates exact solution #7.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT7, the value of U(X).
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) exact7
  real ( kind = 8 ) x
  real ( kind = 8 ) x0

  alpha = 30.0D+00
  x0 = 1.0D+00 / 3.0D+00
  exact7 = ( 1.0D+00 - x ) &
    * ( atan ( alpha * ( x - x0 ) ) + atan ( alpha * x0 ) )

  return
end
function exact_ux7 ( x )

!*****************************************************************************80
!
!! EXACT_UX7 evaluates the derivative of exact solution #7.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX7, the value of dUdX(X).
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) exact_ux7
  real ( kind = 8 ) x
  real ( kind = 8 ) x0

  alpha = 30.0D+00
  x0 = 1.0D+00 / 3.0D+00
  exact_ux7 = - atan ( alpha * ( x - x0 ) ) - atan ( alpha * x0 ) &
    + ( 1.0 - x ) * alpha / ( 1.0 + alpha * alpha * ( x - x0 ) ** 2 )

  return
end
function f7 ( x )

!*****************************************************************************80
!
!! F7 evaluates right hand side function #7.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F7, the value of F(X).
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) f7
  real ( kind = 8 ) x
  real ( kind = 8 ) x0

  alpha = 30.0D+00
  x0 = 1.0D+00 / 3.0D+00
  f7 = 2.0D+00 * ( 1.0D+00 + alpha * ( x - x0 ) * &
    ( atan ( alpha * ( x - x0 ) ) + atan ( alpha * x0 ) ) )

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 carries out test case #8.
!
!  Discussion:
!
!    Use A8, C8, F8, EXACT8, EXACT_UX8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
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

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ), external :: a8
  real ( kind = 8 ), external :: c8
  real ( kind = 8 ), external :: exact8
  real ( kind = 8 ), external :: exact_ux8
  real ( kind = 8 ), external :: f8
  integer ( kind = 4 ) i
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) h1s
  real ( kind = 8 ) mx
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) uexact
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST08'
  write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
  write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
  write ( *, '(a)' ) '  A8(X) = 1.0'
  write ( *, '(a)' ) '  C8(X) = 0.0'
  write ( *, '(a)' ) '  F8(X) = X * ( X + 3 ) * exp ( X ),   X <= 2/3'
  write ( *, '(a)' ) '        = 2 * exp ( 2/3),                   2/3 < X'
  write ( *, '(a)' ) '  U8(X) = X * ( 1 - X ) * exp ( X ),   X <= 2/3'
  write ( *, '(a)' ) '        = X * ( 1 - X ) * exp ( 2/3 ),      2/3 < X'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of nodes = ', n
!
!  Geometry definitions.
!
  x_first = 0.0D+00
  x_last = 1.0D+00
  call r8vec_linspace ( n, x_first, x_last, x )

  call fem1d_bvp_quadratic ( n, a8, c8, f8, x, u )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I    X         U         Uexact    Error'
  write ( *, '(a)' ) ' '

  do i = 1, n
    uexact = exact8 ( x(i) )
    write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, x(i), u(i), uexact, abs ( u(i) - uexact )
  end do

  call l1_error ( n, x, u, exact8, e1 )
  call l2_error_quadratic ( n, x, u, exact8, e2 )
  call h1s_error_quadratic ( n, x, u, exact_ux8, h1s )
  call max_error_quadratic ( n, x, u, exact8, mx )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
  write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
  write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s
  write ( *, '(a,g14.6)' ) '  Max norm of error = ', mx

  return
end
function a8 ( x )

!*****************************************************************************80
!
!! A8 evaluates A function #8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) A8, the value of A(X).
!
  implicit none

  real ( kind = 8 ) a8
  real ( kind = 8 ) x

  a8 = 1.0D+00

  return
end
function c8 ( x )

!*****************************************************************************80
!
!! C8 evaluates C function #8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) C8, the value of C(X).
!
  implicit none

  real ( kind = 8 ) c8
  real ( kind = 8 ) x

  c8 = 0.0D+00

  return
end
function exact8 ( x )

!*****************************************************************************80
!
!! EXACT8 evaluates exact solution #8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT8, the value of U(X).
!
  implicit none

  real ( kind = 8 ) exact8
  real ( kind = 8 ) x

  if ( x <= 2.0D+00 / 3.0D+00 ) then
    exact8 = x * ( 1.0D+00 - x ) * exp ( x )
  else
    exact8 = x * ( 1.0D+00 - x ) * exp ( 2.0D+00 / 3.0D+00 )
  end if

  return
end
function exact_ux8 ( x )

!*****************************************************************************80
!
!! EXACT_UX8 evaluates the derivative of exact solution #8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX8, the value of dUdX(X).
!
  implicit none

  real ( kind = 8 ) exact_ux8
  real ( kind = 8 ) x

  if ( x <= 2.0D+00 / 3.0D+00 ) then
    exact_ux8 = ( 1.0D+00 - x - x * x ) * exp ( x )
  else
    exact_ux8 = ( 1.0D+00 - 2.0D+00 * x ) * exp ( 2.0D+00 / 3.0D+00 )
  end if

  return
end
function f8 ( x )

!*****************************************************************************80
!
!! F8 evaluates the F function for case #8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F8, the value of F(X).
!
  implicit none

  real ( kind = 8 ) f8
  real ( kind = 8 ) x

  if ( x <= 2.0D+00 / 3.0D+00 ) then
    f8 = x * ( x + 3.0D+00 ) * exp ( x )
  else
    f8 = 2.0D+00 * exp ( 2.0D+00 / 3.0D+00 )
  end if

  return
end
subroutine test09 ( )

!*****************************************************************************80
!
!! TEST09 carries out test case #9.
!
!  Discussion:
!
!    Use A9, C9, F9, EXACT9, EXACT_UX9.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
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

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ), external :: a9
  real ( kind = 8 ), external :: c9
  real ( kind = 8 ), external :: exact9
  real ( kind = 8 ), external :: exact_ux9
  real ( kind = 8 ), external :: f9
  integer ( kind = 4 ) i
  real ( kind = 8 ) e1
  real ( kind = 8 ) e2
  real ( kind = 8 ) h1s
  real ( kind = 8 ) mx
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) uexact
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST09'
  write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
  write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
  write ( *, '(a)' ) '  A9(X) = 1.0'
  write ( *, '(a)' ) '  C9(X) = 0.0'
  write ( *, '(a)' ) '  F9(X) = X * ( X + 3 ) * exp ( X ),   X <= 2/3'
  write ( *, '(a)' ) '        = 2 * exp ( 2/3),                   2/3 < X'
  write ( *, '(a)' ) '  U9(X) = X * ( 1 - X ) * exp ( X ),   X <= 2/3'
  write ( *, '(a)' ) '        = X * ( 1 - X ),                    2/3 < X'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of nodes = ', n
!
!  Geometry definitions.
!
  x_first = 0.0D+00
  x_last = 1.0D+00
  call r8vec_linspace ( n, x_first, x_last, x )

  call fem1d_bvp_quadratic ( n, a9, c9, f9, x, u )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I    X         U         Uexact    Error'
  write ( *, '(a)' ) ' '

  do i = 1, n
    uexact = exact9 ( x(i) )
    write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, x(i), u(i), uexact, abs ( u(i) - uexact )
  end do

  call l1_error ( n, x, u, exact9, e1 )
  call l2_error_quadratic ( n, x, u, exact9, e2 )
  call h1s_error_quadratic ( n, x, u, exact_ux9, h1s )
  call max_error_quadratic ( n, x, u, exact9, mx )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
  write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
  write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s
  write ( *, '(a,g14.6)' ) '  Max norm of error = ', mx

  return
end
function a9 ( x )

!*****************************************************************************80
!
!! A9 evaluates A function #9.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) A9, the value of A(X).
!
  implicit none

  real ( kind = 8 ) a9
  real ( kind = 8 ) x

  a9 = 1.0D+00

  return
end
function c9 ( x )

!*****************************************************************************80
!
!! C9 evaluates C function #9.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) C9, the value of C(X).
!
  implicit none

  real ( kind = 8 ) c9
  real ( kind = 8 ) x

  c9 = 0.0D+00

  return
end
function exact9 ( x )

!*****************************************************************************80
!
!! EXACT9 evaluates exact solution #9.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT9, the value of U(X).
!
  implicit none

  real ( kind = 8 ) exact9
  real ( kind = 8 ) x

  if ( x <= 2.0D+00 / 3.0D+00 ) then
    exact9 = x * ( 1.0D+00 - x ) * exp ( x )
  else
    exact9 = x * ( 1.0D+00 - x )
  end if

  return
end
function exact_ux9 ( x )

!*****************************************************************************80
!
!! EXACT_UX9 evaluates the derivative of exact solution #9.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX9, the value of dUdX(X).
!
  implicit none

  real ( kind = 8 ) exact_ux9
  real ( kind = 8 ) x

  if ( x <= 2.0D+00 / 3.0D+00 ) then
    exact_ux9 = ( 1.0D+00 - x - x * x ) * exp ( x )
  else
    exact_ux9 = 1.0D+00 - 2.0D+00 * x
  end if

  return
end
function f9 ( x )

!*****************************************************************************80
!
!! F9 evaluates the F function for case #9.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F9, the value of F(X).
!
  implicit none

  real ( kind = 8 ) f9
  real ( kind = 8 ) x

  if ( x <= 2.0D+00 / 3.0D+00 ) then
    f9 = x * ( x + 3.0D+00 ) * exp ( x )
  else
    f9 = 2.0D+00
  end if

  return
end
subroutine test10 ( )

!*****************************************************************************80
!
!! TEST10 tests FEM1D_BVP_QUADRATIC.
!
!  Discussion:
!
!    We want to compute errors and do convergence rates for the 
!    following problem:
!
!    - uxx + u = x  for 0 < x < 1
!    u(0) = u(1) = 0
!
!    exact  = x - sinh(x) / sinh(1)
!    exact' = 1 - cosh(x) / sinh(1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
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

  integer ( kind = 4 ), parameter :: e_log_max = 6

  real ( kind = 8 ), external :: a10
  real ( kind = 8 ), external :: c10
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ), external :: exact10
  real ( kind = 8 ), external :: exact_ux10
  integer ( kind = 4 ) e_log
  real ( kind = 8 ), external :: f10
  real ( kind = 8 ) h_plot(0:e_log_max)
  real ( kind = 8 ) h1
  real ( kind = 8 ) h1_plot(0:e_log_max)
  real ( kind = 8 ) l2
  real ( kind = 8 ) l2_plot(0:e_log_max)
  real ( kind = 8 ) mx
  real ( kind = 8 ) mx_plot(0:e_log_max)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ne
  integer ( kind = 4 ) ne1
  integer ( kind = 4 ) ne2
  integer ( kind = 4 ) ne_plot(e_log_max+1)
  character ( len = 255 ) output_filename
  real ( kind = 8 ) r
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST10'
  write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
  write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
  write ( *, '(a)' ) '  A(X)  = 1.0'
  write ( *, '(a)' ) '  C(X)  = 1.0'
  write ( *, '(a)' ) '  F(X)  = X'
  write ( *, '(a)' ) '  U(X)  = X - SINH(X) / SINH(1)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) ' log(E)    E         L2error         H1error        Maxerror'
  write ( *, '(a)' ) ''

  do e_log = 0, e_log_max

    ne = 2 ** ( e_log + 1 )

    n = ne + 1
    x_lo = 0.0D+00
    x_hi = 1.0D+00
    allocate ( x(1:n) )
    call r8vec_linspace ( n, x_lo, x_hi, x )

    allocate ( u(1:n) )
    call fem1d_bvp_quadratic ( n, a10, c10, f10, x, u )

    ne_plot(e_log) = ne

    h_plot(e_log) = ( x_hi - x_lo ) / real ( ne, kind = 8 )

    call l2_error_quadratic ( n, x, u, exact10, l2 )
    l2_plot(e_log) = l2

    call h1s_error_quadratic ( n, x, u, exact_ux10, h1 )
    h1_plot(e_log) = h1

    call max_error_quadratic ( n, x, u, exact10, mx )
    mx_plot(e_log) = mx

    write ( *, '(2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      e_log, ne, l2, h1, mx

    deallocate ( u )
    deallocate ( x )

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    ' log(E1)  E1 / E2          L2rate          H1rate         Maxrate'
  write ( *, '(a)' ) ''

  do e_log = 0, e_log_max - 1
    ne1 = ne_plot(e_log)
    ne2 = ne_plot(e_log+1)
    r = real ( ne2, kind = 8 ) / real ( ne1, kind = 8 )
    l2 = l2_plot(e_log) / l2_plot(e_log+1)
    l2 = log ( l2 ) / log ( r )
    h1 = h1_plot(e_log) / h1_plot(e_log+1)
    h1 = log ( h1 ) / log ( r )
    mx = mx_plot(e_log) / mx_plot(e_log+1)
    mx = log ( mx ) / log ( r )
    write ( *, '(2x,i4,2x,i4,a1,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      e_log, ne1, '/', ne2, l2, h1, mx
  end do
!
!  Create data file.
!
  data_filename = 'data.txt'
  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do e_log = 0, e_log_max
    write ( data_unit, '(2x,i6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      ne_plot(e_log), l2_plot(e_log), h1_plot(e_log), mx_plot(e_log)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Created graphics data file "' // trim ( data_filename ) // '".'
!
!  Plot the L2 error as a function of NE.
!
  command_filename = 'commands_l2.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  output_filename = 'l2.png'

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( output_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<---NE--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---L2(NE)--->"'
  write ( command_unit, '(a)' ) &
    'set title "L2 error versus number of elements NE"'
  write ( command_unit, '(a)' ) 'set logscale xy'
  write ( command_unit, '(a)' ) 'set size ratio -1'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 with points pt 7 ps 2 lc rgb "blue",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "red"'

  close ( unit = command_unit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Created plot file "' // trim ( output_filename ) //'".'
!
!  Plot the h1 error as a function of NE.
!
  command_filename = 'commands_h1.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  output_filename = 'h1.png'

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( output_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<---NE--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---H1(NE)--->"'
  write ( command_unit, '(a)' ) &
    'set title "H1 error versus number of elements NE"'
  write ( command_unit, '(a)' ) 'set logscale xy'
  write ( command_unit, '(a)' ) 'set size ratio -1'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:3 with points pt 7 ps 2 lc rgb "blue",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:3 lw 3 linecolor rgb "red"'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Created plot file "' // trim ( output_filename ) //'".'
!
!  Plot the max error as a function of NE.
!
  command_filename = 'commands_mx.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  output_filename = 'mx.png'

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( output_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<---NE--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---MX(NE)--->"'
  write ( command_unit, '(a)' ) &
    'set title "Max error versus number of elements NE"'
  write ( command_unit, '(a)' ) 'set logscale xy'
  write ( command_unit, '(a)' ) 'set size ratio -1'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:4 with points pt 7 ps 2 lc rgb "blue",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:4 lw 3 linecolor rgb "red"'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Created plot file "' // trim ( output_filename ) //'".'


  return
end
function a10 ( x )

!*****************************************************************************80
!
!! A10 evaluates A function #10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) A10, the value of A(X).
!
  implicit none

  real ( kind = 8 ) a10
  real ( kind = 8 ) x

  a10 = 1.0D+00

  return
end
function c10 ( x )

!*****************************************************************************80
!
!! C10 evaluates C function #10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) C10, the value of C(X).
!
  implicit none

  real ( kind = 8 ) c10
  real ( kind = 8 ) x

  c10 = 1.0D+00

  return
end
function exact10 ( x )

!*****************************************************************************80
!
!! EXACT10 evaluates exact solution #10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT10, the value of U(X).
!
  implicit none

  real ( kind = 8 ) exact10
  real ( kind = 8 ) x

  exact10 = x - sinh ( x ) / sinh ( 1.0D+00 )

  return
end
function exact_ux10 ( x )

!*****************************************************************************80
!
!! EXACT_UX10 evaluates the derivative of exact solution #10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT_UX10, the value of dUdX(X).
!
  implicit none

  real ( kind = 8 ) exact_ux10
  real ( kind = 8 ) x

  exact_ux10 = 1.0D+00 - cosh ( x ) / sinh ( 1.0D+00 )

  return
end
function f10 ( x )

!*****************************************************************************80
!
!! F10 evaluates right hand side function #10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) F10, the value of F(X).
!
  implicit none

  real ( kind = 8 ) f10
  real ( kind = 8 ) x

  f10 = x

  return
end
