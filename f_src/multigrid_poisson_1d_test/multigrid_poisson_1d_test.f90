program main

!*****************************************************************************80
!
!! MAIN is the main program for MULTIGRID_POISSON_1D_TEST.
!
!  Discussion:
!
!    MULTIGRID_POISSON_1D_TEST tests the MULTIGRID_POISSON_1D library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 November 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MULTIGRID_POISSON_1D_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the MULTIGRID_POISSON_1D multigrid library.'

  call test01_mono ( ) 
  call test01_multi ( ) 
  call test02_mono ( ) 
  call test02_multi ( ) 
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MULTIGRID_POISSON_1D_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01_mono ( ) 

!*****************************************************************************80
!
!! TEST01_MONO tests MONOGRID_POISSON_1D on test case 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 November 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) difmax
  real ( kind = 8 ), external :: exact1
  real ( kind = 8 ), external :: force1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ) ua
  real ( kind = 8 ) ub
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01_MONO'
  write ( *, '(a)' ) '  MONOGRID_POISSON_1D solves a 1D Poisson BVP'
  write ( *, '(a)' ) '  using the Gauss-Seidel method.'

  a = 0.0D+00
  b = 1.0D+00
  ua = 0.0D+00
  ub = 0.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  -u"(x) = 1, for 0 < x < 1'
  write ( *, '(a)' ) '  u(0) = u(1) = 0.'
  write ( *, '(a)' ) '  Solution is u(x) = ( -x^2 + x ) / 2'

  do k = 5, 5

    n = 2**k
    allocate ( u(1:n+1) )
    allocate ( x(1:n+1) )
    call r8vec_linspace ( n + 1, a, b, x )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i4)' ) '  Mesh index K = ', k
    write ( *, '(a,i6)' ) '  Number of intervals N=2^K = ', n
    write ( *, '(a,i6)' ) '  Number of nodes = 2^K+1 =   ', n + 1

    call monogrid_poisson_1d ( n, a, b, ua, ub, force1, exact1, it_num, u )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     I        X(I)      U(I)         U Exact(X(I))'
    write ( *, '(a)' ) ' '
    do i = 1, n + 1
      write ( *, '(2x,i4,2x,f10.4,2x,g14.6,2x,g14.6)' ) i, x(i), u(i), exact1 ( x(i) )
    end do

    write ( *, '(a)' ) ' '

    difmax = 0.0D+00
    do i = 1, n + 1
      difmax = max ( difmax, abs ( u(i) - exact1 ( x(i) ) ) )
    end do 
    write ( *, '(a,g14.6)' ) '  Maximum error = ', difmax
    write ( *, '(a,i6)' ) '  Number of iterations = ', it_num

    deallocate ( u )
    deallocate ( x )

  end do

  return
end
subroutine test01_multi ( ) 

!*****************************************************************************80
!
!! TEST01_MULTI tests MULTIGRID_POISSON_1D on test case 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) difmax
  real ( kind = 8 ), external :: exact1
  real ( kind = 8 ), external :: force1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ) ua
  real ( kind = 8 ) ub
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01_MULTI'
  write ( *, '(a)' ) '  MULTIGRID_POISSON_1D solves a 1D Poisson BVP'
  write ( *, '(a)' ) '  using the multigrid method.'

  a = 0.0D+00
  b = 1.0D+00
  ua = 0.0D+00
  ub = 0.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  -u"(x) = 1, for 0 < x < 1'
  write ( *, '(a)' ) '  u(0) = u(1) = 0.'
  write ( *, '(a)' ) '  Solution is u(x) = ( -x^2 + x ) / 2'

  do k = 5, 5

    n = 2**k
    allocate ( u(1:n+1) )
    allocate ( x(1:n+1) )
    call r8vec_linspace ( n + 1, a, b, x )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i4)' ) '  Mesh index K = ', k
    write ( *, '(a,i6)' ) '  Number of intervals N=2^K = ', n
    write ( *, '(a,i6)' ) '  Number of nodes = 2^K+1 =   ', n + 1

    call multigrid_poisson_1d ( n, a, b, ua, ub, force1, exact1, it_num, u )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     I        X(I)      U(I)         U Exact(X(I))'
    write ( *, '(a)' ) ' '
    do i = 1, n + 1
      write ( *, '(2x,i4,2x,f10.4,2x,g14.6,2x,g14.6)' ) i, x(i), u(i), exact1 ( x(i) )
    end do

    write ( *, '(a)' ) ' '

    difmax = 0.0D+00
    do i = 1, n + 1
      difmax = max ( difmax, abs ( u(i) - exact1 ( x(i) ) ) )
    end do 
    write ( *, '(a,g14.6)' ) '  Maximum error = ', difmax
    write ( *, '(a,i6)' ) '  Number of iterations = ', it_num

    deallocate ( u )
    deallocate ( x )

  end do

  return
end
function exact1 ( x )

!*****************************************************************************80
!                                                    
!! EXACT1 evaluates the exact solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Hager,
!    Applied Numerical Linear Algebra,
!    Prentice-Hall, 1988,
!    ISBN13: 978-0130412942,
!    LC: QA184.H33.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT1, the value of the exact solution at X.
!
  real ( kind = 8 ) exact1
  real ( kind = 8 ) x

  exact1 = 0.5D+00 * ( - x * x + x )

  return
end
function force1 ( x )

!*****************************************************************************80
!                                                    
!! FORCE1 evaluates the forcing function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Hager,
!    Applied Numerical Linear Algebra,
!    Prentice-Hall, 1988,
!    ISBN13: 978-0130412942,
!    LC: QA184.H33.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) FORCE1, the value of the forcing function at X.
!
  real ( kind = 8 ) force1
  real ( kind = 8 ) x

  force1 = 1.0D+00

  return
end
subroutine test02_mono ( ) 

!*****************************************************************************80
!
!! TEST02_MONO tests MONOGRID_POISSON_1D on test case 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 November 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) difmax
  real ( kind = 8 ), external :: exact2
  real ( kind = 8 ), external :: force2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ) ua
  real ( kind = 8 ) ub
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02_MONO'
  write ( *, '(a)' ) '  MONOGRID_POISSON_1D solves a 1D Poisson BVP'
  write ( *, '(a)' ) '  using the Gauss-Seidel method.'

  a = 0.0D+00
  b = 1.0D+00
  ua = 0.0D+00
  ub = 0.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  -u"(x) = - x * (x+3) * exp(x), for 0 < x < 1'
  write ( *, '(a)' ) '  u(0) = u(1) = 0.'
  write ( *, '(a)' ) '  Solution is u(x) = x * (x-1) * exp(x)'

  do k = 5, 5

    n = 2**k
    allocate ( u(1:n+1) )
    allocate ( x(1:n+1) )
    call r8vec_linspace ( n + 1, a, b, x )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i4)' ) '  Mesh index K = ', k
    write ( *, '(a,i6)' ) '  Number of intervals N=2^K = ', n
    write ( *, '(a,i6)' ) '  Number of nodes = 2^K+1 =   ', n + 1

    call monogrid_poisson_1d ( n, a, b, ua, ub, force2, exact2, it_num, u )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     I        X(I)      U(I)         U Exact(X(I))'
    write ( *, '(a)' ) ' '
    do i = 1, n + 1
      write ( *, '(2x,i4,2x,f10.4,2x,g14.6,2x,g14.6)' ) i, x(i), u(i), exact2 ( x(i) )
    end do

    write ( *, '(a)' ) ' '

    difmax = 0.0D+00
    do i = 1, n + 1
      difmax = max ( difmax, abs ( u(i) - exact2 ( x(i) ) ) )
    end do 
    write ( *, '(a,g14.6)' ) '  Maximum error = ', difmax
    write ( *, '(a,i6)' ) '  Number of iterations = ', it_num

    deallocate ( u )
    deallocate ( x )

  end do

  return
end
subroutine test02_multi ( ) 

!*****************************************************************************80
!
!! TEST02_MULTI tests MULTIGRID_POISSON_1D on test case 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) difmax
  real ( kind = 8 ), external :: exact2
  real ( kind = 8 ), external :: force2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: u(:)
  real ( kind = 8 ) ua
  real ( kind = 8 ) ub
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02_MULTI'
  write ( *, '(a)' ) '  MULTIGRID_POISSON_1D solves a 1D Poisson BVP'
  write ( *, '(a)' ) '  using the multigrid method.'

  a = 0.0D+00
  b = 1.0D+00
  ua = 0.0D+00
  ub = 0.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  -u"(x) = - x * (x+3) * exp(x), for 0 < x < 1'
  write ( *, '(a)' ) '  u(0) = u(1) = 0.'
  write ( *, '(a)' ) '  Solution is u(x) = x * (x-1) * exp(x)'

  do k = 5, 5

    n = 2**k
    allocate ( u(1:n+1) )
    allocate ( x(1:n+1) )
    call r8vec_linspace ( n + 1, a, b, x )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i4)' ) '  Mesh index K = ', k
    write ( *, '(a,i6)' ) '  Number of intervals N=2^K = ', n
    write ( *, '(a,i6)' ) '  Number of nodes = 2^K+1 =   ', n + 1

    call multigrid_poisson_1d ( n, a, b, ua, ub, force2, exact2, it_num, u )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     I        X(I)      U(I)         U Exact(X(I))'
    write ( *, '(a)' ) ' '
    do i = 1, n + 1
      write ( *, '(2x,i4,2x,f10.4,2x,g14.6,2x,g14.6)' ) i, x(i), u(i), exact2 ( x(i) )
    end do

    write ( *, '(a)' ) ' '

    difmax = 0.0D+00
    do i = 1, n + 1
      difmax = max ( difmax, abs ( u(i) - exact2 ( x(i) ) ) )
    end do 
    write ( *, '(a,g14.6)' ) '  Maximum error = ', difmax
    write ( *, '(a,i6)' ) '  Number of iterations = ', it_num

    deallocate ( u )
    deallocate ( x )

  end do

  return
end
function exact2 ( x )

!*****************************************************************************80
!                                                    
!! EXACT2 evaluates the exact solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Hager,
!    Applied Numerical Linear Algebra,
!    Prentice-Hall, 1988,
!    ISBN13: 978-0130412942,
!    LC: QA184.H33.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) EXACT2, the value of the exact solution at X.
!
  real ( kind = 8 ) exact2
  real ( kind = 8 ) x

  exact2 = x * ( x - 1.0D+00 ) * exp ( x )

  return
end
function force2 ( x )

!*****************************************************************************80
!                                                    
!! FORCE2 evaluates the forcing function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Hager,
!    Applied Numerical Linear Algebra,
!    Prentice-Hall, 1988,
!    ISBN13: 978-0130412942,
!    LC: QA184.H33.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) FORCE2, the value of the forcing function at X.
!
  real ( kind = 8 ) force2
  real ( kind = 8 ) x

  force2 =  - x * ( x + 3.0D+00 ) * exp ( x )

  return
end
