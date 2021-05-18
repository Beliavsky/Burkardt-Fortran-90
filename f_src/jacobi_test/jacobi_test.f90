program main

!*****************************************************************************80
!
!! MAIN is the main program for JACOBI_TEST.
!
!  Discussion:
!
!    JACOBI_TEST tests the JACOBI library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JACOBI_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the JACOBI library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JACOBI_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!  Purpose:
!
!    JACOBI_TEST01 tests JACOBI1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: b(:)
  real ( kind = 8 ) d_max
  real ( kind = 8 ) e_max
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ) r_max
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: x_exact(:)
  real ( kind = 8 ), allocatable :: x_new(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JACOBI_TEST01:'
  write ( *, '(a)' ) '  Try the Jacobi iteration on the second difference matrix'

  it_max = 2000
  n = 33
  tol = 1.0D-05

  allocate ( a(1:n,1:n) )
  allocate ( b(1:n) )
  allocate ( r(1:n) )
  allocate ( x(1:n) )
  allocate ( x_exact(1:n) )
  allocate ( x_new(1:n) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  Matrix order N = ', n
  write ( *, '(a,i6)' ) '  Maximum number of iterations = ', it_max
!
!  Set the matrix A.
!
  call dif2 ( n, n, a )
!
!  Determine the right hand side vector B.
!
  do i = 1, n
    t = real ( i - 1, kind = 8 ) / real ( n - 1, kind = 8 )
    x_exact(i) = exp ( t ) * ( t - 1 ) * t
  end do

  b = matmul ( a(1:n,1:n), x_exact(1:n) )
!
!  Set the initial estimate for the solution.
!
  it = 0

  x(1:n) = 0.0D+00

  r = matmul ( a(1:n,1:n), x(1:n) )
  r(1:n) = b(1:n) - r(1:n)
  r_max = maxval ( r );

  e_max = maxval ( abs ( x - x_exact ) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       I    Resid           X-Error         X-Change'
  write ( *, '(a)' ) ' '

  write ( *, '(2x,i6,2x,g14.6,2x,g14.6)' ) it, r_max, e_max
!
!  Carry out the iteration.
!
  do it = 1, it_max

    call jacobi1 ( n, a, b, x, x_new )

    r = matmul ( a(1:n,1:n), x(1:n) )
    r(1:n) = b(1:n) - r(1:n)
    r_max = maxval ( abs ( r ) )
!
!  Compute the average point motion.
!
    d_max = maxval ( abs ( x - x_new ) )
!
!  Compute the average point motion.
!
    e_max = maxval ( abs ( x - x_exact ) )
!
!  Update the solution
!
    x(1:n) = x_new(1:n)

    write ( *, '(2x,i6,2x,g14.6,2x,g14.6,2x,g14.6)' ) it, r_max, e_max, d_max

    if ( r_max <= tol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Convergence criterion satisifed on step ', it
      exit
    end if

  end do

  call r8vec_print ( n, x, "  Estimated solution:" )

  deallocate ( a )
  deallocate ( b )
  deallocate ( r )
  deallocate ( x )
  deallocate ( x_exact )
  deallocate ( x_new )

  return
end
