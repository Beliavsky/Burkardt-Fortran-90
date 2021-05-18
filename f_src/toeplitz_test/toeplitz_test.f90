program main

!*****************************************************************************80
!
!! MAIN is the main program for TOEPLITZ_TEST.
!
!  Discussion:
!
!    TOEPLITZ_TEST tests the TOEPLITZ library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TOEPLITZ_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the TOEPLITZ library.'

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
  write ( *, '(a)' ) 'TOEPLITZ_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests C4CI_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  complex ( kind = 4 ) a(n)
  integer ( kind = 4 ) seed
  complex ( kind = 4 ) x(n)
  complex ( kind = 4 ) x2(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  C4CI_SL solves a complex circulant system.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  seed = 123456789
  call c4ci_random ( n, seed, a )

  call c4ci_print ( n, a, '  The circulant matrix:' )
!
!  Set the desired solution.
!
  call c4vec_indicator ( n, x )
!
!  Compute the corresponding right hand side.
!
  call c4ci_mxv ( n, a, x, x2 )
!
!  Solve the linear system.
!
  call c4ci_sl ( n, a, x2 )

  call c4vec_print ( n, x2, '  Solution:' )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests C4TO_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  complex ( kind = 4 ) a(2*n-1)
  complex ( kind = 4 ) b(n)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) seed
  complex ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  C4TO_SL solves a complex Toeplitz system.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  seed = 123456789
  call c4to_random ( n, seed, a )

  call c4to_print ( n, a, '  The Toeplitz matrix:' )

  do job = 0, 1
!
!  Set the desired solution.
!
    call c4vec_indicator ( n, x )

    if ( job == 0 ) then
      call c4vec_print ( n, x, '  Desired solution:' )
    else
      call c4vec_print ( n, x, '  Desired solution to transposed system:' )
    end if
!
!  Compute the corresponding right hand side.
!
    if ( job == 0 ) then
      call c4to_mxv ( n, a, x, b )
    else
      call c4to_vxm ( n, a, x, b )
    end if

    if ( job == 0 ) then
      call c4vec_print ( n, b, '  Right Hand Side:' )
    else
      call c4vec_print ( n, b, '  Right Hand Side of transposed system:' )
    end if
!
!  Solve the linear system.
!
    call c4to_sl ( n, a, b, x, job )

    if ( job == 0 ) then
      call c4vec_print ( n, x, '  Solution:' )
    else
      call c4vec_print ( n, x, '  Solution to transposed system:' )
    end if

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests C8CI_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  complex ( kind = 8 ) a(n)
  integer ( kind = 4 ) seed
  complex ( kind = 8 ) x(n)
  complex ( kind = 8 ) x2(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  C8CI_SL solves a complex circulant system.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  seed = 123456789
  call c8ci_random ( n, seed, a )

  call c8ci_print ( n, a, '  The circulant matrix:' )
!
!  Set the desired solution.
!
  call c8vec_indicator ( n, x )
!
!  Compute the corresponding right hand side.
!
  call c8ci_mxv ( n, a, x, x2 )
!
!  Solve the linear system.
!
  call c8ci_sl ( n, a, x2 )

  call c8vec_print ( n, x2, '  Solution:' )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests C8TO_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  complex ( kind = 8 ) a(2*n-1)
  complex ( kind = 8 ) b(n)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) seed
  complex ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  C8TO_SL solves a complex Toeplitz system.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  seed = 123456789
  call c8to_random ( n, seed, a )

  call c8to_print ( n, a, '  The Toeplitz matrix:' )

  do job = 0, 1
!
!  Set the desired solution.
!
    call c8vec_indicator ( n, x )

    if ( job == 0 ) then
      call c8vec_print ( n, x, '  Desired solution:' )
    else
      call c8vec_print ( n, x, '  Desired solution to transposed system:' )
    end if
!
!  Compute the corresponding right hand side.
!
    if ( job == 0 ) then
      call c8to_mxv ( n, a, x, b )
    else
      call c8to_vxm ( n, a, x, b )
    end if

    if ( job == 0 ) then
      call c8vec_print ( n, b, '  Right Hand Side:' )
    else
      call c8vec_print ( n, b, '  Right Hand Side of transposed system:' )
    end if
!
!  Solve the linear system.
!
    call c8to_sl ( n, a, b, x, job )

    if ( job == 0 ) then
      call c8vec_print ( n, x, '  Solution:' )
    else
      call c8vec_print ( n, x, '  Solution to transposed system:' )
    end if

  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests R4TO_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 4 ) a(2*n-1)
  real ( kind = 4 ) b(n)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) seed
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  R4TO_SL solves a real Toeplitz system.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  seed = 123456789
  call r4to_random ( n, seed, a )

  call r4to_print ( n, a, '  The Toeplitz matrix:' )

  do job = 0, 1
!
!  Set the desired solution.
!
    call r4vec_indicator ( n, x )
!
!  Compute the corresponding right hand side.
!
    if ( job == 0 ) then
      call r4to_mxv ( n, a, x, b )
    else
      call r4to_vxm ( n, a, x, b )
    end if
!
!  Solve the linear system.
!
    call r4to_sl ( n, a, b, x, job )

    if ( job == 0 ) then
      call r4vec_print ( n, x, '  Solution:' )
    else
      call r4vec_print ( n, x, '  Solution to transposed system:' )
    end if

  end do

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 tests R4BTO_MXV, R4BTO_VXM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: l = 3
  integer ( kind = 4 ), parameter :: m = 2

  real ( kind = 4 ), dimension ( m, m, l ) ::  a1 = reshape ( (/ &
    1.0E+00, 5.0E+00, 2.0E+00, 5.0E+00, &
    3.0E+00, 6.0E+00, 4.0E+00, 6.0E+00, &
    5.0E+00, 7.0E+00, 6.0E+00, 7.0E+00 /), (/ m, m, l /) )

  real ( kind = 4 ), dimension ( m, m, l-1 ) :: a2 = reshape ( (/ &
    7.0E+00, 8.0E+00, 8.0E+00, 8.0E+00, &
    9.0E+00, 9.0E+00, 0.0E+00, 9.0E+00 /), (/ m, m, l-1 /) )
  real ( kind = 4 ) b(m,l)
  real ( kind = 4 ) x(m,l)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST06'
  write ( *, '(a)' ) '  For a real block Toeplitz matrix,'
  write ( *, '(a)' ) '  R4BTO_MXV computes A * x.'
  write ( *, '(a)' ) '  R4BTO_VXM computes x * A.'

  call r4bto_print ( m, l, a1, a2, '  The block Toeplitz matrix:' )

  call r4vec_indicator ( m*l, x )

  call r4vec_print ( m*l, x, '  The vector x:' )

  call r4bto_mxv ( m, l, a1, a2, x, b )

  call r4vec_print ( m*l, b, '  The product A*x:' )

  call r4bto_vxm ( m, l, a1, a2, x, b )

  call r4vec_print ( m*l, b, '  The product x*A:' )

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! TEST07 tests R4BTO_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ), parameter :: l = 3
  integer ( kind = 4 ), parameter :: n = m * l

  real ( kind = 4 ), dimension ( m, m, l ) ::  a1 = reshape ( (/ &
    9.0E+00, 2.0E+00, 1.0E+00, 8.0E+00, &
    3.0E+00, 6.0E+00, 4.0E+00, 6.0E+00, &
    5.0E+00, 7.0E+00, 6.0E+00, 7.0E+00 /), (/ m, m, l /) )

  real ( kind = 4 ), dimension ( m, m, l-1 ) :: a2 = reshape ( (/ &
    7.0E+00, 8.0E+00, 8.0E+00, 8.0E+00, &
    9.0E+00, 9.0E+00, 0.0E+00, 9.0E+00 /), (/ m, m, l-1 /) )
  real ( kind = 4 ) b(n)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST07'
  write ( *, '(a)' ) '  R4BTO_SL solves a block Toeplitz system.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n

  call r4bto_print ( m, l, a1, a2, '  The block Toeplitz matrix:' )
!
!  Set the desired solution.
!
  call r4vec_indicator ( n, x )
!
!  Compute the right hand side.
!
  call r4bto_mxv ( m, l, a1, a2, x, b )

  call r4vec_print ( n, b, '  Right hand side:' )

  call r4bto_sl ( m, l, a1, a2, b, x )

  call r4vec_print ( n, x, '  Computed solution:' )

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 tests R8TO_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(2*n-1)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST08'
  write ( *, '(a)' ) '  R8TO_SL solves a real Toeplitz system.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  seed = 123456789
  call r8to_random ( n, seed, a )

  call r8to_print ( n, a, '  The Toeplitz matrix:' )

  do job = 0, 1
!
!  Set the desired solution.
!
    call r8vec_indicator ( n, x )
!
!  Compute the corresponding right hand side.
!
    if ( job == 0 ) then
      call r8to_mxv ( n, a, x, b )
    else
      call r8to_vxm ( n, a, x, b )
    end if
!
!  Solve the linear system.
!
    call r8to_sl ( n, a, b, x, job )

    if ( job == 0 ) then
      call r8vec_print ( n, x, '  Solution:' )
    else
      call r8vec_print ( n, x, '  Solution to transposed system:' )
    end if

  end do

  return
end
subroutine test09 ( )

!*****************************************************************************80
!
!! TEST09 tests R8BTO_MXV, R8BTO_VXM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: l = 3
  integer ( kind = 4 ), parameter :: m = 2

  real ( kind = 8 ), dimension ( m, m, l ) ::  a1 = reshape ( (/ &
    1.0D+00, 5.0D+00, 2.0D+00, 5.0D+00, &
    3.0D+00, 6.0D+00, 4.0D+00, 6.0D+00, &
    5.0D+00, 7.0D+00, 6.0D+00, 7.0D+00 /), (/ m, m, l /) )

  real ( kind = 8 ), dimension ( m, m, l-1 ) :: a2 = reshape ( (/ &
    7.0D+00, 8.0D+00, 8.0D+00, 8.0D+00, &
    9.0D+00, 9.0D+00, 0.0D+00, 9.0D+00 /), (/ m, m, l-1 /) )
  real ( kind = 8 ) b(m,l)
  real ( kind = 8 ) x(m,l)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST09'
  write ( *, '(a)' ) '  For a real block Toeplitz matrix,'
  write ( *, '(a)' ) '  R8BTO_MXV computes A * x.'
  write ( *, '(a)' ) '  R8BTO_VXM computes x * A.'

  call r8bto_print ( m, l, a1, a2, '  The block Toeplitz matrix:' )

  call r8vec_indicator ( m*l, x )

  call r8vec_print ( m*l, x, '  The vector x:' )

  call r8bto_mxv ( m, l, a1, a2, x, b )

  call r8vec_print ( m*l, b, '  The product A*x:' )

  call r8bto_vxm ( m, l, a1, a2, x, b )

  call r8vec_print ( m*l, b, '  The product x*A:' )

  return
end
subroutine test10 ( )

!*****************************************************************************80
!
!! TEST10 tests R8BTO_SL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ), parameter :: l = 3
  integer ( kind = 4 ), parameter :: n = m * l

  real ( kind = 8 ), dimension ( m, m, l ) ::  a1 = reshape ( (/ &
    9.0D+00, 2.0D+00, 1.0D+00, 8.0D+00, &
    3.0D+00, 6.0D+00, 4.0D+00, 6.0D+00, &
    5.0D+00, 7.0D+00, 6.0D+00, 7.0D+00 /), (/ m, m, l /) )

  real ( kind = 8 ), dimension ( m, m, l-1 ) :: a2 = reshape ( (/ &
    7.0D+00, 8.0D+00, 8.0D+00, 8.0D+00, &
    9.0D+00, 9.0D+00, 0.0D+00, 9.0D+00 /), (/ m, m, l-1 /) )
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST10'
  write ( *, '(a)' ) '  R8BTO_SL solves a block Toeplitz system.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix order N = ', n

  call r8bto_print ( m, l, a1, a2, '  The block Toeplitz matrix:' )
!
!  Set the desired solution.
!
  call r8vec_indicator ( n, x )
!
!  Compute the right hand side.
!
  call r8bto_mxv ( m, l, a1, a2, x, b )

  call r8vec_print ( n, b, '  Right hand side:' )

  call r8bto_sl ( m, l, a1, a2, b, x )

  call r8vec_print ( n, x, '  Computed solution:' )

  return
end
