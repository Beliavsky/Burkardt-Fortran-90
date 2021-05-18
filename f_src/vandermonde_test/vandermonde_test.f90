program main

!*****************************************************************************80
!
!! MAIN is the main program for VANDERMONDE_TEST.
!
!  Discussion:
!
!    VANDERMONDE_TEST tests the VANDERMONDE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2014
!
!  Author:
!
!    John Burkardt
!
  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VANDERMONDE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the VANDERMONDE library.'

  call bivand1_test ( )
  call bivand2_test ( )
  call dvand_test ( )
  call dvandprg_test ( )
  call pvand_test ( )
  call pvandprg_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VANDERMONDE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine bivand1_test ( )

!*****************************************************************************80
!
!! BIVAND1_TEST tests BIVAND1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), parameter :: n2 = ( n * ( n + 1 ) ) / 2

  real ( kind = 8 ) a(n2,n2)
  real ( kind = 8 ), dimension ( n ) :: alpha = (/ &
    1.0, 2.0, 3.0 /)
  real ( kind = 8 ), dimension ( n ) :: beta = (/ &
    10.0, 20.0, 30.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BIVAND1_TEST:'
  write ( *, '(a)' ) '  Compute a bidimensional Vandermonde matrix'
  write ( *, '(a)' ) '  associated with polynomials of total degree < N.'

  call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )
  call r8vec_print ( n, beta, '  Vandermonde vector BETA:' )

  call bivand1 ( n, alpha, beta, a )

  call r8mat_print ( n2, n2, a, '  Bidimensional Vandermonde matrix:' )

  return
end
subroutine bivand2_test ( )

!*****************************************************************************80
!
!! BIVAND2_TEST tests BIVAND2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), parameter :: n2 = n * n

  real ( kind = 8 ) a(n2,n2)
  real ( kind = 8 ), dimension ( n ) :: alpha = (/ &
    1.0, 2.0, 3.0 /)
  real ( kind = 8 ), dimension ( n ) :: beta = (/ &
    10.0, 20.0, 30.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BIVAND2_TEST:'
  write ( *, '(a)' ) '  Compute a bidimensional Vandermonde matrix'
  write ( *, '(a)' ) '  associated with polynomials of maximum degree < N.'

  call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )
  call r8vec_print ( n, beta, '  Vandermonde vector BETA:' )

  call bivand2 ( n, alpha, beta, a )

  call r8mat_print ( n2, n2, a, '  Bidimensional Vandermonde matrix:' )

  return
end
subroutine dvand_test ( )

!*****************************************************************************80
!
!! DVAND_TEST tests DVAND.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ), dimension ( n ) :: alpha1 = (/ &
    0.0, 1.0, 2.0, 3.0, 4.0 /)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( n ) :: x1 = (/ &
    5.0, 3.0, 4.0, 1.0, 2.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DVAND_TEST:'
  write ( *, '(a)' ) '  Solve a Vandermonde linear system A''*x=b'

  do test = 1, 2

    if ( test == 1 ) then
      alpha(1:n) = alpha1(1:n)
    else if ( test == 2 ) then
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, alpha )
    end if

    call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )

    call vand1 ( n, alpha, a )

    x(1:n) = x1(1:n)
    b = matmul ( transpose ( a ), x )
    call r8vec_print ( n, b, '  Right hand side B:' )

    call dvand ( n, alpha, b, x )
    call r8vec_print ( n, x, '  Solution X:' )

  end do

  return
end
subroutine dvandprg_test ( )

!*****************************************************************************80
!
!! DVANDPRG_TEST tests DVANDPRG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ), dimension ( n ) :: alpha1 = (/ &
    0.0, 1.0, 2.0, 3.0, 4.0 /)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) m(n)
  integer ( kind = 4 ) nsub
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( n ) :: x1 = (/ &
    5.0, 3.0, 4.0, 1.0, 2.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DVANDPRG_TEST:'
  write ( *, '(a)' ) '  Solve a Vandermonde linear system A''*x=b'
  write ( *, '(a)' ) '  progressively.'
  write ( *, '(a)' ) '  First we use ALPHA = 0, 1, 2, 3, 4.'
  write ( *, '(a)' ) '  Then we choose ALPHA at random.'

  do test = 1, 2

    if ( test == 1 ) then
      alpha(1:n) = alpha1(1:n)
    else if ( test == 2 ) then
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, alpha )
    end if

    call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )

    call vand1 ( n, alpha, a )

    x(1:n) = x1(1:n)
    b = matmul ( transpose ( a ), x )
    call r8vec_print ( n, b, '  Right hand side B:' )

    x(1:n) = 0.0D+00

    do nsub = 1, n
      call dvandprg ( nsub, alpha, b, x, c, m )
      call r8vec_print ( nsub, x, '  Solution X:' )
    end do

  end do

  return
end
subroutine pvand_test ( )

!*****************************************************************************80
!
!! PVAND_TEST tests PVAND.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ), dimension ( n ) :: alpha1 = (/ &
    0.0, 1.0, 2.0, 3.0, 4.0 /)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( n ) :: x1 = (/ &
    5.0, 3.0, 4.0, 1.0, 2.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PVAND_TEST:'
  write ( *, '(a)' ) '  Solve a Vandermonde linear system A*x=b'

  do test = 1, 2

    if ( test == 1 ) then
      alpha(1:n) = alpha1(1:n)
    else if ( test == 2 ) then
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, alpha )
    end if

    call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )

    call vand1 ( n, alpha, a )

    x(1:n) = x1(1:n)
    b = matmul ( a, x )
    call r8vec_print ( n, b, '  Right hand side B:' )

    call pvand ( n, alpha, b, x )
    call r8vec_print ( n, x, '  Solution X:' )

  end do

  return
end
subroutine pvandprg_test ( )

!*****************************************************************************80
!
!! PVANDPRG_TEST tests PVANDPRG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ), dimension ( n ) :: alpha1 = (/ &
    0.0, 1.0, 2.0, 3.0, 4.0 /)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) m(n)
  integer ( kind = 4 ) nsub
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( n ) :: x1 = (/ &
    5.0, 3.0, 4.0, 1.0, 2.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PVANDPRG_TEST:'
  write ( *, '(a)' ) '  Solve a Vandermonde linear system A*x=b'
  write ( *, '(a)' ) '  progressively.'
  write ( *, '(a)' ) '  First we use ALPHA = 0, 1, 2, 3, 4.'
  write ( *, '(a)' ) '  Then we choose ALPHA at random.'

  do test = 1, 2

    if ( test == 1 ) then
      alpha(1:n) = alpha1(1:n)
    else if ( test == 2 ) then
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, alpha )
    end if

    call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )

    call vand1 ( n, alpha, a )

    x(1:n) = x1(1:n)
    b = matmul ( a, x )
    call r8vec_print ( n, b, '  Right hand side B:' )

    x(1:n) = 0.0D+00
    do nsub = 1, n
      call pvandprg ( nsub, alpha, b, x, c, m )
      call r8vec_print ( nsub, x, '  Solution X:' )
    end do

  end do

  return
end

