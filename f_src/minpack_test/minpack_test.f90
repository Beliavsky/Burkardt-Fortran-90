program main

!*****************************************************************************80
!
!! MAIN is the main program for MINPACK_TEST.
!
!  Discussion:
!
!    MINPACK_TEST tests the MINPACK library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MINPACK_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test minpack().'

  call chkder_test ( )
  call hybrd1_test ( )
  call hybrj1_test ( )
  call lmder1_test ( )
  call lmder1_2_test ( )
  call lmdif1_test ( )
  call lmdif1_2_test ( )
  call lmstr1_test ( )
  call lmstr1_2_test ( )
  call qform_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'minpack_test():'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine chkder_test ( )

!*****************************************************************************80
!
!! CHKDER_TEST tests CHKDER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: m = n
  integer ( kind = 4 ), parameter :: ldfjac = n

  real ( kind = 8 ) err(m)
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(m)
  real ( kind = 8 ) fvecp(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) mode
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xp(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CHKDER_TEST'
  write ( *, '(a)' ) '  CHKDER compares a user supplied jacobian'
  write ( *, '(a)' ) '  and a finite difference approximation to it'
  write ( *, '(a)' ) '  and judges whether the jacobian is correct.'

  do ido = 1, 2

    if ( ido == 1 ) then

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  On the first test, use a correct jacobian.'

    else if ( ido == 2 ) then

       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) '  Repeat the test, but use a "bad" jacobian'
       write ( *, '(a)' ) '  and see if the routine notices!'

     end if
!
!  Set the point at which the test is to be made:
!
    x(1:n) = 0.5D+00

    call r8vec_print ( n, x, '  Evaluation point X:' )

    mode = 1
    call chkder ( m, n, x, fvec, fjac, ldfjac, xp, fvecp, mode, err )

    iflag = 1

    call chkder_f ( n, x, fvec, fjac, ldfjac, iflag )
    call chkder_f ( n, xp, fvecp, fjac, ldfjac, iflag )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Sampled function values F(X) and F(XP)'
    write ( *, '(a)' ) ' '
    do i = 1, m
      write ( *, '(i3,2g14.6)' ) i, fvec(i), fvecp(i)
    end do

    iflag = 2
    call chkder_f ( n, x, fvec, fjac, ldfjac, iflag )
!
!  Here's where we put a mistake into the jacobian, on purpose.
!
    if ( ido == 2 ) then
      fjac(1,1) = 1.01D+00 * fjac(1,1)
      fjac(2,3) = - fjac(2,3)
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Computed jacobian'
    write ( *, '(a)' ) ' '
    do i = 1, m
      write ( *, '(5g14.6)' ) fjac(i,1:n)
    end do

    mode = 2
    call chkder ( m, n, x, fvec, fjac, ldfjac, xp, fvecp, mode, err )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  CHKDER gradient component error estimates:'
    write ( *, '(a)' ) '     > 0.5, the component is probably correct.'
    write ( *, '(a)' ) '     < 0.5, the component is probably incorrect.'
    write ( *, '(a)' ) ' '
    do i = 1, m
      write ( *, '(i6,g14.6)' ) i, err(i)
    end do

  end do

  return
end
subroutine chkder_f ( n, x, fvec, fjac, ldfjac, iflag )

!*****************************************************************************80
!
!! CHKDER_F is a function/jacobian routine for use with CHKDER_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, real ( kind = 8 ) X(N), the variable values.
!
!    Output, real ( kind = 8 ) FVEC(N), the function values at X,
!    if IFLAG = 1.
!
!    Output, real ( kind = 8 ) FJAC(LDFJAC,N), the N by N jacobian at X,
!    if IFLAG = 2.
!
!    Input, integer ( kind = 4 ) LDFJAC, the leading dimension of FJAC,
!    which must be at least N.
!
!    Input, integer ( kind = 4 ) IFLAG:
!    0, user requests printout of current iterate X.
!    1, please compute F(I) (X).
!    2, please compute FJAC(I,J) (X).
!
  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) n

  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) j
  real ( kind = 8 ) prod
  real ( kind = 8 ) x(n)
!
!  If IFLAG is 0, print out X (and anything else about this iterate).
!
  if ( iflag == 0 ) then

    write ( *, '(a)' ) ''
    do i = 1, n
      write ( *, '(g14.6)' ) x(i)
    end do
!
!  If IFLAG is 1, we are supposed to evaluate F(X).
!
  else if ( iflag == 1 ) then

    do i = 1, n - 1
      fvec(i) = x(i) - real ( n + 1, kind = 8 ) + sum ( x(1:n) )
    end do

    fvec(n) = product ( x(1:n) ) - 1.0D+00
!
!  If IFLAG is 2, we are supposed to evaluate FJAC(I,J) = d F(I)/d X(J)
!
  else if ( iflag == 2 ) then

    fjac(1:n-1,1:n) = 1.0D+00

    do i = 1, n - 1
      fjac(i,i) = 2.0D+00
    end do

    prod = product ( x(1:n) )

    do j = 1, n
      fjac(n,j) = prod / x(j)
    end do

  end if

  return
end
subroutine hybrd1_test ( )

!*****************************************************************************80
!
!! HYBRD1_TEST tests HYBRD1.
!
!  Discussion:
!
!    This is an example of what your main program would look
!    like if you wanted to use MINPACK to solve N nonlinear equations
!    in N unknowns.  In this version, we avoid computing the jacobian
!    matrix, and request that MINPACK approximate it for us.
!
!    The set of nonlinear equations is:
!
!      x1^2 - 10 * x1 + x2^2 + 8 = 0
!      x1 * x2^2 + x1 - 10 * x2 + 8 = 0
!
!    with solution x1 = x2 = 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 2

  external hybrd1_f
  real ( kind = 8 ) fvec(n)
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HYBRD1_TEST'
  write ( *, '(a)' ) '  HYBRD1 solves a nonlinear system of equations.'

  x(1:2) = (/ 3.0D+00, 0.0D+00 /)
  call r8vec_print ( n, x, '  Initial X:' )
  call hybrd1_f ( n, x, fvec, iflag )
  call r8vec_print ( n, fvec, '  Initial F(X):' )

  tol = 0.00001D+00

  call hybrd1 ( hybrd1_f, n, x, fvec, tol, info )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  call r8vec_print ( n, x, '  Final X:' )
  call r8vec_print ( n, fvec, '  Final F(X):' )

  return
end
subroutine hybrd1_f ( n, x, fvec, iflag )

!*****************************************************************************80
!
!! HYBRD1_F is a function routine for use with HYBRD1_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of variables.
!
!    real ( kind = 8 ) X(N), the variable values.
!
!  Output:
!
!    real ( kind = 8 ) FVEC(N), the function values at X,
!    if IFLAG = 1.
!
!    integer ( kind = 4 ) IFLAG, error flag.
!    If IFLAG < 0, an error has occurred and the computation should stop.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fvec(n)
  integer ( kind = 4 ) iflag
  real ( kind = 8 ) x(n)

  fvec(1) = x(1) * x(1) - 10.0D+00 * x(1) + x(2) * x(2) + 8.0D+00
  fvec(2) = x(1) * x(2) * x(2) + x(1) - 10.0D+00 * x(2) + 8.0D+00
  iflag = 1

  return
end
subroutine hybrj1_test ( )

!*****************************************************************************80
!
!! HYBRJ1_TEST tests HYBRJ1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 2
  integer ( kind = 4 ), parameter :: ldfjac = n

  external hybrj1_f
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(n)
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HYBRJ1_TEST'
  write ( *, '(a)' ) '  HYBRJ1 solves a nonlinear system of equations.'

  x(1:2) = (/ 3.0D+00, 0.0D+00 /)
  call r8vec_print ( n, x, '  Initial X:' )
  call hybrd1_f ( n, x, fvec, iflag )
  call r8vec_print ( n, fvec, '  F(X):' )

  tol = 0.00001D+00

  call hybrj1 ( hybrj1_f, n, x, fvec, fjac, ldfjac, tol, info )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  call r8vec_print ( n, x, '  X:' )
  call r8vec_print ( n, fvec, '  F(X):' )

  return
end
subroutine hybrj1_f ( n, x, fvec, fjac, ldfjac, iflag )

!*****************************************************************************80
!
!! HYBRJ1_F is a function/jacobian routine for use with HYBRJ1_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, real ( kind = 8 ) X(N), the variable values.
!
!    Output, real ( kind = 8 ) FVEC(N), the function values at X,
!    if IFLAG = 1.
!
!    Output, real ( kind = 8 ) FJAC(LDFJAC,N), the N by N jacobian at X,
!    if IFLAG = 2.
!
!    Input, integer ( kind = 4 ) LDFJAC, the leading dimension of FJAC,
!    which must be at least N.
!
!    Input, integer ( kind = 4 ) IFLAG:
!    0, user requests printout of current iterate X.
!    1, please compute F(I) (X).
!    2, please compute FJAC(I,J) (X).
!
  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) n

  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iflag
  real ( kind = 8 ) x(n)

  if ( iflag == 0 ) then

    write ( *, '(a)' ) ''
    do i = 1, n
      write ( *, '(g14.6)' ) x(i)
    end do

  else if ( iflag == 1 ) then

    fvec(1) = x(1) * x(1) - 10.0D+00 * x(1) + x(2) * x(2) + 8.0D+00
    fvec(2) = x(1) * x(2) * x(2) + x(1) - 10.0D+00 * x(2) + 8.0D+00

  else if ( iflag == 2 ) then

    fjac(1,1) = 2.0D+00 * x(1) - 10.0D+00
    fjac(1,2) = 2.0D+00 * x(2)
    fjac(2,1) = x(2) * x(2) + 1.0D+00
    fjac(2,2) = 2.0D+00 * x(1) * x(2) - 10.0D+00

  end if

  return
end
subroutine lmder1_test ( )

!*****************************************************************************80
!
!! LMDER1_TEST tests LMDER1.
!
!  Discussion:
!
!    LMDER1 solves M nonlinear equations in N unknowns, with M >= N.
!
!    LMDER1 seeks a solution X minimizing the euclidean norm of the residual.
!
!    In this example, the set of equations is actually linear, but
!    normally they are nonlinear.
!
!    In this problem, we have a set of pairs of data points, and we
!    seek a functional relationship between them.  We assume the
!    relationship is of the form
!
!      y = a * x + b
!
!    and we want to know the values of a and b.  Therefore, we would like
!    to find numbers a and b which satisfy a set of equations.
!
!    The data points are (2,2), (4,11), (6,28) and (8,40).
!
!    Therefore, the equations we want to satisfy are:
!
!      a * 2 + b -  2 = 0
!      a * 4 + b - 11 = 0
!      a * 6 + b - 28 = 0
!      a * 8 + b - 40 = 0
!
!    The least squares solution of this system is a=6.55, b=-12.5,
!    In other words, the line y=6.55*x-12.5 is the line which "best"
!    models the data in the least squares sense.
!
!    Problems with more variables, or higher degree polynomials, would
!    be solved similarly.  For example, suppose we have (x,y,z) data,
!    and we wish to find a relationship of the form f(x,y,z).  We assume
!    that x and y occur linearly, and z quadratically.  Then the equation
!    we seek has the form:
!
!      a*x+b*y+c*z + d*z*z + e = 0
!
!    and, supposing that our first two points were (1,2,3), (1,3,8), our set of
!    equations would begin:
!
!      a*1+b*2+c*3 + d*9  + e = 0
!      a*1+b*3+c*8 + d*64 + e = 0
!
!    and so on.
!
!    M is the number of equations, which in this case is the number of
!    (x,y) data values.
!
!    N is the number of variables, which in this case is the number of
!    'free' coefficients in the relationship we are trying to determine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 2
  integer ( kind = 4 ), parameter :: ldfjac = m

  external lmder1_f
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LMDER1_TEST'
  write ( *, '(a)' ) '  LMDER1 minimizes M functions in N variables.'

  x(1:2) = (/ 0.0D+00, 5.0D+00 /)
  call r8vec_print ( n, x, '  Initial X:' )
  iflag = 1
  call lmder1_f ( m, n, x, fvec, fjac, ldfjac, iflag )
  call r8vec_print ( m, fvec, '  F(X):' )

  tol = 0.00001D+00

  call lmder1 ( lmder1_f, m, n, x, fvec, fjac, ldfjac, tol, info )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  call r8vec_print ( n, x, '  X:' )
  call r8vec_print ( m, fvec, '  F(X):' )

  return
end
subroutine lmder1_f ( m, n, x, fvec, fjac, ldfjac, iflag )

!*****************************************************************************80
!
!! LMDER1_F is a function/jacobian routine for use with LMDER1_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of variables.
!
!    real ( kind = 8 ) X(N), the variable values.
!
!    Input, integer ( kind = 4 ) LDFJAC, the leading dimension of FJAC,
!    which must be at least N.
!
!    Input, integer ( kind = 4 ) IFLAG:
!    0, user requests printout of current iterate X.
!    1, please compute F(I) (X).
!    2, please compute FJAC(I,J) (X).
!
!  Output:
!
!    real ( kind = 8 ) FVEC(N), the function values at X,
!    if IFLAG = 1.
!
!    real ( kind = 8 ) FJAC(LDFJAC,N), the N by N jacobian at X,
!    if IFLAG = 2.
!
  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iflag
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), parameter, dimension ( 4 ) :: xdat = (/ &
    2.0D+00,  4.0D+00,  6.0D+00,  8.0D+00 /)
  real ( kind = 8 ), parameter, dimension ( 4 ) :: ydat = (/ &
    2.0D+00, 11.0D+00, 28.0D+00, 40.0D+00 /)

  if ( iflag == 0 ) then

    write ( *, '(a)' ) ''
    do i = 1, n
      write ( *, '(g14.6)' ) x(i)
    end do

  else if ( iflag == 1 ) then

    fvec(1:m) = x(1) * xdat(1:m) + x(2) - ydat(1:m)

  else if ( iflag == 2 ) then

    fjac(1:m,1) = xdat(1:m)
    fjac(1:m,2) = 1.0D+00

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LMDER1_F - Fatal error!'
    write ( *, '(a,i6)' ) '  Called with unexpected value of IFLAG = ', iflag
    stop

  end if

  return
end
subroutine lmder1_2_test ( )

!*****************************************************************************80
!
!! LMDER1_2_TEST tests LMDER1.
!
!  Discussion:
!
!    LMDER1 solves M nonlinear equations in n unknowns, where M is greater
!    than N.  The functional fit is nonlinear this time, of the form
!
!      y=a+b*x^c,
!
!    with x and y data, and a, b and c unknown.
!
!    This problem is set up so that the data is exactly fit by by
!    a=1, b=3, c=2.  Normally, the data would only be approximately
!    fit by the best possible solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 10
  integer ( kind = 4 ), parameter :: n = 3
  integer ( kind = 4 ), parameter :: ldfjac = m

  external lmder1_2_f
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LMDER1_2_TEST'
  write ( *, '(a)' ) '  LMDER1 minimizes M functions in N variables.'

  x(1:3) = (/ 0.0D+00, 5.0D+00, 1.3D+00 /)
  call r8vec_print ( n, x, '  Initial X:' )
  iflag = 1
  call lmder1_2_f ( m, n, x, fvec, fjac, ldfjac, iflag )
  call r8vec_print ( m, fvec, '  F(X):' )

  tol = 0.00001D+00

  call lmder1 ( lmder1_2_f, m, n, x, fvec, fjac, ldfjac, tol, info )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  call r8vec_print ( n, x, '  X:' )
  call r8vec_print ( m, fvec, '  F(X):' )

  return
end
subroutine lmder1_2_f ( m, n, x, fvec, fjac, ldfjac, iflag )

!*****************************************************************************80
!
!! LMDER1_2_F is a function/jacobian routine for use with LMDER1_2_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, real ( kind = 8 ) X(N), the variable values.
!
!    Output, real ( kind = 8 ) FVEC(N), the function values at X,
!    if IFLAG = 1.
!
!    Output, real ( kind = 8 ) FJAC(LDFJAC,N), the N by N jacobian at X,
!    if IFLAG = 2.
!
!    Input, integer ( kind = 4 ) LDFJAC, the leading dimension of FJAC,
!    which must be at least N.
!
!    Input, integer ( kind = 4 ) IFLAG:
!    0, user requests printout of current iterate X.
!    1, please compute F(I) (X).
!    2, please compute FJAC(I,J) (X).
!
  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iflag
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( 10 ) :: xdat = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, &
    6.0D+00, 7.0D+00, 8.0D+00, 9.0D+00, 10.0D+00 /)
  real ( kind = 8 ), dimension ( 10 ) :: ydat = (/ &
    4.0D+00, 13.0D+00, 28.0D+00, 49.0D+00, 76.0D+00, &
    109.0D+00, 148.0D+00, 193.0D+00, 244.0D+00, 301.0D+00 /)

  if ( iflag == 0 ) then

    write ( *, '(a)' ) ''
    do i = 1, n
      write ( *, '(g14.6)' ) x(i)
    end do

  else if ( iflag == 1 ) then

    fvec(1:m) = x(1) + x(2) * xdat(1:m) ** x(3) - ydat(1:m)

  else if ( iflag == 2 ) then

    fjac(1:m,1) = 1.0D+00
    fjac(1:m,2) = xdat(1:m) ** x(3)
    fjac(1:m,3) = x(2) * log ( xdat(1:m) ) * xdat(1:m) ** x(3)

  end if

  return
end
subroutine lmdif1_test ( )

!*****************************************************************************80
!
!! LMDIF1_TEST tests LMDIF1.
!
!  Discussion:
!
!    LMDIF1 solves M nonlinear equations in N unknowns, where M is greater
!    than N.  Generally, you cannot get a solution vector x which will satisfy
!    all the equations.  That is, the vector equation f(x)=0 cannot
!    be solved exactly.  Instead, minpack seeks a solution x so that
!    the euclidean norm transpose(f(x))*f(x) is minimized.  The size
!    of the euclidean norm is a measure of how good the solution is.
!
!    In this example, the set of equations is actually linear, but
!    normally they are nonlinear.
!
!    In this problem, we have a set of pairs of data points, and we
!    seek a functional relationship between them.  We assume the
!    relationship is of the form
!
!      y=a*x+b
!
!    and we want to know the values of a and b.  Therefore, we would like
!    to find numbers a and b which satisfy a set of equations.
!
!    The data points are (2,2), (4,11), (6,28) and (8,40).
!
!    Therefore, the equations we want to satisfy are:
!
!      a * 2 + b -  2 = 0
!      a * 4 + b - 11 = 0
!      a * 6 + b - 28 = 0
!      a * 8 + b - 40 = 0
!
!    The least squares solution of this system is a=6.55, b=-12.5,
!    In other words, the line y=6.55*x-12.5 is the line which "best"
!    models the data in the least squares sense.
!
!    Problems with more variables, or higher degree polynomials, would
!    be solved similarly.  For example, suppose we have (x,y,z) data,
!    and we wish to find a relationship of the form f(x,y,z).  We assume
!    that x and y occur linearly, and z quadratically.  Then the equation
!    we seek has the form:
!
!      a*x+b*y+c*z + d*z*z + e = 0
!
!    and, supposing that our first two points were (1,2,3), (1,3,8), our set of
!    equations would begin:
!
!      a*1+b*2+c*3 + d*9  + e = 0
!      a*1+b*3+c*8 + d*64 + e = 0
!
!    and so on.
!
!    M is the number of equations, which in this case is the number of
!    (x,y) data values.
!
!    N is the number of variables, which in this case is the number of
!    'free' coefficients in the relationship we are trying to determine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 2

  external lmdif1_f
  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LMDIF1_TEST'
  write ( *, '(a)' ) '  LMDIF1 minimizes M functions in N variables.'

  x(1:2) = (/ 0.0D+00, 5.0D+00 /)
  call r8vec_print ( n, x, '  Initial X:' )
  iflag = 1
  call lmdif1_f ( m, n, x, fvec, iflag )
  call r8vec_print ( m, fvec, '  F(X):' )

  tol = 0.00001D+00

  call lmdif1 ( lmdif1_f, m, n, x, fvec, tol, info )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  call r8vec_print ( n, x, '  X:' )
  call r8vec_print ( m, fvec, '  F(X):' )

  return
end
subroutine lmdif1_f ( m, n, x, fvec, iflag )

!*****************************************************************************80
!
!! LMDIF1_F is a function routine for use with LMDIF1_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of functions.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, real ( kind = 8 ) X(N), the variable values.
!
!    Output, real ( kind = 8 ) FVEC(M), the function values at X,
!    if IFLAG = 1.
!
!    Input, integer ( kind = 4 ) IFLAG:
!    0, user requests printout of current iterate X.
!    1, please compute F(I) (X).
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iflag
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( 4 ) :: xdat = (/ &
    2.0D+00,  4.0D+00,  6.0D+00,  8.0D+00 /)
  real ( kind = 8 ), dimension ( 4 ) :: ydat = (/ &
    2.0D+00, 11.0D+00, 28.0D+00, 40.0D+00 /)

  if ( iflag == 0 ) then

    write ( *, '(a)' ) ''
    do i = 1, n
      write ( *, '(g14.6)' ) x(i)
    end do

  else if ( iflag == 1 ) then

    fvec(1:m) = x(1) * xdat(1:m) + x(2) - ydat(1:m)

  end if

  return
end
subroutine lmdif1_2_test ( )

!*****************************************************************************80
!
!! LMDIF1_2_TEST tests LMDIF1.
!
!  Discussion:
!
!    LMDIF1 solves M nonlinear equations in N unknowns, where M is greater
!    than N.  It is similar to test02, except that the functional fit is
!    nonlinear this time, of the form
!
!      y = a + b * x^c,
!
!    with x and y data, and a, b and c unknown.
!
!    This problem is set up so that the data is exactly fit by by
!    a=1, b=3, c=2.  Normally, the data would only be approximately
!    fit by the best possible solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 10
  integer ( kind = 4 ), parameter :: n = 3

  external lmdif1_2_f
  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LMDIF1_2_TEST'
  write ( *, '(a)' ) '  LMDIF1 minimizes M functions in N variables.'

  x(1:3) = (/ 0.0D+00, 5.0D+00, 1.3D+00 /)
  call r8vec_print ( n, x, '  X:' )
  iflag = 1
  call lmdif1_2_f ( m, n, x, fvec, iflag )
  call r8vec_print ( m, fvec, '  F(X):' )

  tol = 0.00001D+00

  call lmdif1 ( lmdif1_2_f, m, n, x, fvec, tol, info )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  call r8vec_print ( n, x, '  X:' )
  call r8vec_print ( m, fvec, '  F(X):' )

  return
end
subroutine lmdif1_2_f ( m, n, x, fvec, iflag )

!*****************************************************************************80
!
!! LMDIF1_2_F is a function routine for use with LMDIF1_2_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of functions.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, real ( kind = 8 ) X(N), the variable values.
!
!    Output, real ( kind = 8 ) FVEC(M), the function values at X,
!    if IFLAG = 1.
!
!    Input, integer ( kind = 4 ) IFLAG:
!    0, user requests printout of current iterate X.
!    1, please compute F(I) (X).
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iflag
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( 10 ) :: xdat = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, &
    6.0D+00, 7.0D+00, 8.0D+00, 9.0D+00, 10.0D+00 /)
  real ( kind = 8 ), dimension ( 10 ) :: ydat = (/ &
    4.0D+00, 13.0D+00, 28.0D+00, 49.0D+00, 76.0D+00, &
    109.0D+00, 148.0D+00, 193.0D+00, 244.0D+00, 301.0D+00 /)

  if ( iflag == 0 ) then

    write ( *, '(a)' ) ''
    do i = 1, n
      write ( *, '(g14.6)' ) x(i)
    end do

  else if ( iflag == 1 ) then

    fvec(1:m) = x(1) + x(2) * xdat(1:m) ** x(3) - ydat(1:m)

  end if

  return
end
subroutine lmstr1_test ( )

!*****************************************************************************80
!
!! LMSTR1_TEST tests LMSTR1.
!
!  Discussion:
!
!    LMSTR1 solves M nonlinear equations in N unknowns, where M is greater
!    than N.  Generally, you cannot get a solution vector x which will satisfy
!    all the equations.  That is, the vector equation f(x)=0 cannot
!    be solved exactly.  Instead, minpack seeks a solution x so that
!    the euclidean norm transpose(f(x))*f(x) is minimized.  The size
!    of the euclidean norm is a measure of how good the solution is.
!
!    In this example, the set of equations is actually linear, but
!    normally they are nonlinear.
!
!    In this problem, we have a set of pairs of data points, and we
!    seek a functional relationship between them.  We assume the
!    relationship is of the form
!
!      y=a*x+b
!
!    and we want to know the values of a and b.  Therefore, we would like
!    to find numbers a and b which satisfy a set of equations.
!
!    The data points are (2,2), (4,11), (6,28) and (8,40).
!
!    Therefore, the equations we want to satisfy are:
!
!      a * 2 + b -  2 = 0
!      a * 4 + b - 11 = 0
!      a * 6 + b - 28 = 0
!      a * 8 + b - 40 = 0
!
!    The least squares solution of this system is a=6.55, b=-12.5,
!    In other words, the line y=6.55*x-12.5 is the line which "best"
!    models the data in the least squares sense.
!
!    Problems with more variables, or higher degree polynomials, would
!    be solved similarly.  For example, suppose we have (x,y,z) data,
!    and we wish to find a relationship of the form f(x,y,z).  We assume
!    that x and y occur linearly, and z quadratically.  Then the equation
!    we seek has the form:
!
!      a*x + b*y + c*z + d*z*z + e = 0
!
!    and, supposing that our first two points were (1,2,3), (1,3,8), our set of
!    equations would begin:
!
!      a*1 + b*2 + c*3 + d*9  + e = 0
!      a*1 + b*3 + c*8 + d*64 + e = 0
!
!    and so on.
!
!    M is the number of equations, which in this case is the number of
!    (x,y) data values.
!
!    N is the number of variables, which in this case is the number of
!    'free' coefficients in the relationship we are trying to determine.
!
!    Thanks to Jacques Le Bourlot for pointing out that the line
!      integer ( kind = 4 ) fjrow
!    should instead read
!      real ( kind = 8 ) fjrow(n)
!    19 August 2016
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 2
  integer ( kind = 4 ), parameter :: ldfjac = m

  external lmstr1_f
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fjrow(n)
  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LMSTR1_TEST'
  write ( *, '(a)' ) '  LMSTR1 minimizes M functions in N variables.'

  x(1:2) = (/ 0.0D+00, 5.0D+00 /)
  call r8vec_print ( n, x, '  Initial X:' )
  iflag = 1
  call lmstr1_f ( m, n, x, fvec, fjrow, iflag )
  call r8vec_print ( m, fvec, '  F(X):' )

  tol = 0.00001D+00

  call lmstr1 ( lmstr1_f, m, n, x, fvec, fjac, ldfjac, tol, info )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  call r8vec_print ( n, x, '  X:' )
  call r8vec_print ( m, fvec, '  F(X):' )

  return
end
subroutine lmstr1_f ( m, n, x, fvec, fjrow, iflag )

!*****************************************************************************80
!
!! LMSTR1_F is a function/jacobian routine for use with LMSTR1_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of functions, and the
!    number of rows in the jacobian.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, real ( kind = 8 ) X(N), the variable values.
!
!    Output, real ( kind = 8 ) FVEC(M), the function values at X,
!    if IFLAG = 1.
!
!    Output, real ( kind = 8 ) FJROW(N), space to return one row
!    of the jacobian,  if IFLAG = 2.
!
!    Input, integer ( kind = 4 ) LDFJAC, the leading dimension of FJAC,
!    which must be at least N.
!
!    Input, integer ( kind = 4 ) IFLAG:
!    0, user requests printout of current iterate X.
!    1, please compute F(I) (X).
!    2, please compute row (I-1) of the jacobian and return it in FJROW.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) fjrow(n)
  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) iflag
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( 4 ) :: xdat = (/ &
    2.0D+00,  4.0D+00,  6.0D+00,  8.0D+00 /)
  real ( kind = 8 ), dimension ( 4 ) :: ydat = (/ &
    2.0D+00, 11.0D+00, 28.0D+00, 40.0D+00 /)

  if ( iflag == 1 ) then

    fvec(1:m) = x(1) * xdat(1:m) + x(2) - ydat(1:m)

  else if ( 2 <= iflag ) then

    fjrow(1) = xdat(iflag-1)
    fjrow(2) = 1.0D+00

  end if

  return
end
subroutine lmstr1_2_test ( )

!*****************************************************************************80
!
!! LMSTR1_2_TEST tests LMSTR1.
!
!  Discussion:
!
!    LMSTR1 solves M nonlinear equations in N unknowns, where M is greater
!    than N.  This test is similar to test02, except that the functional fit
!    is nonlinear this time, of the form
!
!      y = a + b * x^c,
!
!    with x and y data, and a, b and c unknown.
!
!    This problem is set up so that the data is exactly fit by by
!    a=1, b=3, c=2.  Normally, the data would only be approximately
!    fit by the best possible solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 10
  integer ( kind = 4 ), parameter :: n = 3
  integer ( kind = 4 ), parameter :: ldfjac = m

  external lmstr1_2_f
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fjrow(n)
  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LMSTR1_2_TEST'
  write ( *, '(a)' ) '  LMSTR1 minimizes M functions in N variables.'

  x(1:3) = (/ 0.0D+00, 5.0D+00, 1.3D+00 /)
  call r8vec_print ( n, x, '  Initial X:' )
  iflag = 1
  call lmstr1_2_f ( m, n, x, fvec, fjrow, iflag )
  call r8vec_print ( m, fvec, '  F(X):' )

  tol = 0.00001D+00

  call lmstr1 ( lmstr1_2_f, m, n, x, fvec, fjac, ldfjac, tol, info )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  call r8vec_print ( n, x, '  X:' )
  call r8vec_print ( m, fvec, '  F(X):' )

  return
end
subroutine lmstr1_2_f ( m, n, x, fvec, fjrow, iflag )

!*****************************************************************************80
!
!! LMSTR1_2_F is a function/jacobian routine for use with LMSTR1_2_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of functions, and the
!    number of rows in the jacobian.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, real ( kind = 8 ) X(N), the variable values.
!
!    Output, real ( kind = 8 ) FVEC(M), the function values at X,
!    if IFLAG = 1.
!
!    Output, real ( kind = 8 ) FJROW(N), space to return one row
!    of the jacobian,  if IFLAG = 2.
!
!    Input, integer ( kind = 4 ) LDFJAC, the leading dimension of FJAC,
!    which must be at least N.
!
!    Input, integer ( kind = 4 ) IFLAG:
!    0, user requests printout of current iterate X.
!    1, please compute F(I) (X).
!    2, please compute row (I-1) of the jacobian and return it in FJROW.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) fjrow(n)
  real ( kind = 8 ) fvec(m)
  integer ( kind = 4 ) iflag
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( 10 ) :: xdat = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, &
    6.0D+00, 7.0D+00, 8.0D+00, 9.0D+00, 10.0D+00 /)
  real ( kind = 8 ), dimension ( 10 ) :: ydat = (/ &
    4.0D+00, 13.0D+00, 28.0D+00, 49.0D+00, 76.0D+00, &
    109.0D+00, 148.0D+00, 193.0D+00, 244.0D+00, 301.0D+00 /)

  if ( iflag == 1 ) then

    fvec(1:m) = x(1) + x(2) * xdat(1:m) ** x(3) - ydat(1:m)

  else if ( 2 <= iflag ) then

    fjrow(1) = 1.0D+00
    fjrow(2) = xdat(iflag-1) ** x(3)
    fjrow(3) = x(2) * log ( xdat(iflag-1) ) * xdat(iflag-1) ** x(3)

  end if

  return
end
subroutine qform_test ( )

!*****************************************************************************80
!
!! QFORM_TEST tests QFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ), parameter :: lda = m

  real ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) a2(m,n)
  real ( kind = 8 ) acnorm(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ipivot(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) lipvt
  logical pivot
  real ( kind = 8 ) q(m,m)
  real ( kind = 8 ) r(m,n)
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) rdiag(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QFORM_TEST:'
  write ( *, '(a)' ) '  QFORM constructs the Q factor explicitly'
  write ( *, '(a)' ) '  after the use of QRFAC.'
!
!  Set the matrix A.
!
  seed = 123456789
  do j = 1, n
    do i = 1, m
      a(i,j) = r8_uniform_01 ( seed )
    end do
  end do
  call r8mat_print ( m, n, a, '  Matrix A:' )
!
!  Compute the QR factors.
!
  pivot = .false.
  lipvt = n

  call qrfac ( m, n, a, lda, pivot, ipivot, lipvt, rdiag, acnorm )
!
!  Extract the R factor.
!
  r(1:m,1:n) = 0.0D+00

  do k = 1, min ( m, n )
    r(k,k) = rdiag(k)
  end do

  do j = 1, n
    r(1:min(j-1,m),j) = a(1:min(j-1,m),j)
  end do
  call r8mat_print ( m, n, r, '  Matrix R:' )
!
!  Call QRFORM to form the Q factor.
!
  q(1:m,1:m) = 0.0D+00
  q(1:m,1:min(m,n)) = a(1:m,1:min(m,n))

  call qform ( m, n, q, m )

  call r8mat_print ( m, m, q, '  Matrix Q:' )
!
!  Compute Q*R.
!
  a2 = matmul ( q, r )
!
!  Compare Q*R to A.
!
  call r8mat_print ( m, n, a2, '  Matrix A2 = Q * R:' )

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
    write ( *, '(2x,i8,2x,g16.8)' ) i, a(i)
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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end

