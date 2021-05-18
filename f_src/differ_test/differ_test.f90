program main

!*****************************************************************************80
!
!! MAIN is the main program for DIFFER_TEST.
!
!  Discussion:
!
!    DIFFER_TEST tests the DIFFER library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DIFFER_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the DIFFER library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DIFFER_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests DIFFER_MATRIX.
!
!  Discussion:
!
!    DIFFER_MATRIX computes a modified Vandermonde matrix A1.
!
!    The solution of a system A1 * X1 = B is related to the solution
!    of the system A2 * X2 = B, where A2 is the standard Vandermonde
!    matrix, simply by X2(I) = X1(I) * A(I,1).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) job
  real ( kind = 8 ) stencil(n)
  real ( kind = 8 ) x1(n)
  real ( kind = 8 ) x2(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Demonstrate that the DIFFER matrix is "really"'
  write ( *, '(a)' ) '  a Vandermonde matrix.'

  stencil(1:n) = (/ 2.5, 3.3, -1.3, 0.5 /)
  x1(1:n) = (/ 1.0, 2.0, 3.0, 4.0 /)
  call differ_matrix ( n, stencil, a )
  call r8mat_print ( n, n, a, '  Stencil matrix:' )
  b(1:n) = matmul ( a(1:n,1:n), x1(1:n) )
!
!  Set up and solve the DIFFER system.
!
  call differ_matrix ( n, stencil, a )
  x1(1:n) = b(1:n)
  call r8mat_fs ( n, a, x1, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST01 - Warning!'
    write ( *, '(a)' ) '  DIFFER system is singular.'
    return
  end if

  call r8vec_print ( n, x1, '  Solution of DIFFER system:' )
!
!  R8VM_SL solves the related Vandermonde system.
!  A simple transformation gives us the solution to the DIFFER system.
!
  job = 0
  call r8vm_sl ( n, stencil, b, x2, job, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST01 - Warning!'
    write ( *, '(a)' ) '  VANDERMONDE system is singular.'
    return
  end if

  call r8vec_print ( n, x2, '  Solution of VANDERMONDE system:' )

  x2(1:n) = x2(1:n) / stencil(1:n)
  call r8vec_print ( n, x2, '  Transformed solution of VANDERMONDE system:' )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests DIFFER_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:,:)
  real ( kind = 8 ) err
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  DIFFER_INVERSE returns the inverse of a DIFFER matrix;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N    Inverse error'

  seed = 123456789;

  do n = 2, 8

    write ( *, '(a)' ) ''

    allocate ( a(1:n,1:n) )
    allocate ( b(1:n,1:n) )
    allocate ( x(1:n) )

    do test = 1, 5
      call r8vec_uniform_01 ( n, seed, x )
      call differ_matrix ( n, x, a )
      call differ_inverse ( n, x, b )
      call inverse_error ( n, a, b, err )
      write ( *, '(2x,i2,2x,g14.6)' ) n, err
    end do

    deallocate ( a )
    deallocate ( b )
    deallocate ( x )

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests DIFFER_MATRIX.
!
!  Discussion:
!
!    Reproduce a specific example.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) df
  real ( kind = 8 ) dfdx
  real ( kind = 8 ) dx
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) order
  real ( kind = 8 ) stencil(n)
  real ( kind = 8 ) x0

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Reproduce a specific example.'
!
!  Compute the coefficients for a specific stencil.
!
  stencil(1:n) = (/ -3.0, -2.0, -1.0, 1.0 /)
  b(1:n) = 0.0
  order = 1
  b(order) = 1.0
  call differ_matrix ( n, stencil, a )
  c(1:n) = b(1:n)
  call r8mat_fs ( n, a, c, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST03 - Warning!'
    write ( *, '(a)' ) '  DIFFER system is singular.'
    return
  end if

  call r8vec_print ( n, c, '  Solution of DIFFER system:' )
!
!  Use the coefficients C to estimate the first derivative of EXP(X)
!  at X0, using a spacing of DX = 0.1.
!
  x0 = 1.3D+00
  dx = 0.1D+00
  df = 0.0D+00
  do i = 1, n
    df = df + c(i) * ( exp ( x0 + stencil(i) * dx ) - exp ( x0 ) )
  end do
  dfdx = df / dx

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  DFDX =         ', dfdx
  write ( *, '(a,g14.6)' ) '  d exp(x) /dx = ', exp ( x0 )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests DIFFER_FORWARD, DIFFER_BACKWARD, DIFFER_CENTRAL.
!
!  Discussion:
!
!    Evaluate the coefficients for uniformly spaced finite difference
!    approximations of derivatives.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: c(:)
  real ( kind = 8 ) h
  character ( len = 80 ) label
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  integer ( kind = 4 ) p
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  DIFFER_FORWARD,'
  write ( *, '(a)' ) '  DIFFER_BACKWARD, and'
  write ( *, '(a)' ) '  DIFFER_CENTRAL produce coefficients for difference'
  write ( *, '(a)' ) '  approximations of the O-th derivative,'
  write ( *, '(a)' ) '  with error of order H^P, for a uniform spacing of H.'

  h = 1.0D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a)' ) '  Use a spacing of H = ', h, ' for all examples.'
!
!  Forward difference approximation to the third derivative with error of O(h).
!
  o = 3
  p = 1
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  call differ_forward ( h, o, p, c, x )
  write ( label, '(a,i2,a,i2)' ) &
    '  Forward difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Backward difference approximation to the third derivative with error of O(h).
!
  o = 3
  p = 1
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  call differ_backward ( h, o, p, c, x )
  write ( label, '(a,i2,a,i2)' ) &
    '  Backward difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Central difference approximation to the third derivative with error of O(h^2).
!
  o = 3
  p = 2
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  call differ_central ( h, o, p, c, x )
  write ( label, '(a,i2,a,i2)' ) &
    '  Central difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Central difference approximation to the third derivative with error of O(h^4).
!
  o = 3
  p = 4
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  call differ_central ( h, o, p, c, x )
  write ( label, '(a,i2,a,i2)' ) &
    '  Central difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Forward difference approximation to the fourth derivative with error of O(h).
!
  o = 4
  p = 1
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  call differ_forward ( h, o, p, c, x )
  write ( label, '(a,i2,a,i2)' ) &
    '  Forward difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Backward difference approximation to the fourth derivative with error of O(h).
!
  o = 4
  p = 1
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  call differ_backward ( h, o, p, c, x )
  write ( label, '(a,i2,a,i2)' ) &
    '  Backward difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Central difference approximation to the fourth derivative with error of O(h^3).
!
  o = 4
  p = 3
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  call differ_central ( h, o, p, c, x )
  write ( label, '(a,i2,a,i2)' ) &
    '  Central difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests DIFFER_STENCIL.
!
!  Discussion:
!
!    Use DIFFER_STENCIL to reproduce forward, backward and central differences.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: c(:)
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  character ( len = 80 ) label
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  integer ( kind = 4 ) p
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  DIFFER_STENCIL produces coefficients for difference'
  write ( *, '(a)' ) '  approximations of the O-th derivative,'
  write ( *, '(a)' ) '  using arbitrarily spaced data, with maximum spacing H'
  write ( *, '(a)' ) '  with error of order H^P.'
!
!  Let X0 = 0.
!
  x0 = 0.0D+00
  h = 1.0D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  For all tests, let X0 = ', x0
  write ( *, '(a,g14.6,a)' ) '  and use a uniformly spacing of ', h
  write ( *, '(a)' ) '  so we can compare with previous results.'
!
!  Forward difference approximation to the third derivative with error of O(h).
!
  o = 3
  p = 1
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  do i = 1, n
    x(i) = real ( i - 1, kind = 8 ) * h
  end do
  call differ_stencil ( x0, o, p, x, c )
  write ( label, '(a,i2,a,i2)' ) &
    '  Forward difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Backward difference approximation to the third derivative with error of O(h).
!
  o = 3
  p = 1
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  do i = 1, n
    x(i) = - real ( n - i, kind = 8 ) * h
  end do

  call differ_stencil ( x0, o, p, x, c )
  write ( label, '(a,i2,a,i2)' ) &
    '  Backward difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Central difference approximation to the third derivative with error of O(h^2).
!
  o = 3
  p = 2
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  do i = 1, n
    x(i) = real ( - n - 1 + 2 * i, kind = 8 ) * h / 2.0D+00
  end do
  call differ_stencil ( x0, o, p, x, c )
  write ( label, '(a,i2,a,i2)' ) &
    '  Central difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Central difference approximation to the third derivative with error of O(h^4).
!
  o = 3
  p = 4
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  do i = 1, n
    x(i) = real ( - n - 1 + 2 * i, kind = 8 ) * h / 2.0D+00
  end do
  call differ_stencil ( x0, o, p, x, c )
  write ( label, '(a,i2,a,i2)' ) &
    '  Central difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Forward difference approximation to the fourth derivative with error of O(h).
!
  o = 4
  p = 1
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  do i = 1, n
    x(i) = real ( i - 1, kind = 8 ) * h
  end do
  call differ_stencil ( x0, o, p, x, c )
  write ( label, '(a,i2,a,i2)' ) &
    '  Forward difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Backward difference approximation to the fourth derivative with error of O(h).
!
  o = 4
  p = 1
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  do i = 1, n
    x(i) = - real ( n - i, kind = 8 ) * h
  end do
  call differ_stencil ( x0, o, p, x, c )
  write ( label, '(a,i2,a,i2)' ) &
    '  Backward difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )
!
!  Central difference approximation to the fourth derivative with error of O(h^3).
!
  o = 4
  p = 3
  n = o + p
  allocate ( c(1:n) )
  allocate ( x(1:n) )
  do i = 1, n
    x(i) = real ( - n - 1 + 2 * i, kind = 8 ) * h / 2.0D+00
  end do
  call differ_stencil ( x0, o, p, x, c )
  write ( label, '(a,i2,a,i2)' ) &
    '  Central difference coefficients, O = ', o, ', P = ', p
  call r8vec2_print ( n, x, c, label )
  deallocate ( c )
  deallocate ( x )

  return
end

