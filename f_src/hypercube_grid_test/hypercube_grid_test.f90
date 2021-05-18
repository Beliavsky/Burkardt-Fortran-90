program main 

!*****************************************************************************80
!
!! MAIN is the main program for HYPERCUBE_GRID_TEST.
!
!  Discussion:
!
!    HYPERCUBE_GRID_TEST tests HYPERCUBE_GRID.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HYPERCUBE_GRID_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the HYPERCUBE_GRID library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HYPERCUBE_GRID_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests HYPERCUBE_GRID on a two dimensional example.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2

  real ( kind = 8 ), dimension ( m ) :: a = (/ 0.0D+00, 0.0D+00 /)
  real ( kind = 8 ), dimension ( m ) :: b = (/ 1.0D+00, 10.0D+00 /)
  integer ( kind = 4 ), dimension ( m ) :: c = (/ 2, 4 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4vec_product
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( m ) :: ns = (/ 4, 5 /)
  real ( kind = 8 ), allocatable :: x(:,:)

  n = i4vec_product ( m, ns )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Create a grid using HYPERCUBE_GRID.'
  write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
  write ( *, '(a,i4)' ) '  Number of grid points N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I    NS     C      A         B'
  write ( *, '(a)' ) ''
  do i = 1, m
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) i, ns(i), c(i), a(i), b(i)
  end do

  allocate ( x(1:m,1:n) )
  call hypercube_grid ( m, n, ns, a, b, c, x )
  call r8mat_transpose_print ( m, n, x, '  Grid points:' )
  deallocate ( x )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests HYPERCUBE_GRID on a five dimensional example.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5

  real ( kind = 8 ), dimension ( m ) :: a = (/ &
    0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00 /)
  real ( kind = 8 ), dimension ( m ) :: b = (/ &
    1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00 /)
  integer ( kind = 4 ), dimension ( m ) :: c = (/ &
    1, 2, 3, 4, 5 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4vec_product
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( m ) :: ns = (/ &
    2, 2, 2, 2, 2 /)
  real ( kind = 8 ), allocatable :: x(:,:)

  n = i4vec_product ( m, ns )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Create a grid using HYPERCUBE_GRID.'
  write ( *, '(a)' ) '  Use a two point grid in each dimension.'
  write ( *, '(a)' ) '  Use a different centering option in each dimension.'
  write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
  write ( *, '(a,i4)' ) '  Number of grid points N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I    NS     C      A         B'
  write ( *, '(a)' ) ''
  do i = 1, m
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) i, ns(i), c(i), a(i), b(i)
  end do

  allocate ( x(1:m,1:n) )
  call hypercube_grid ( m, n, ns, a, b, c, x )
  call r8mat_transpose_print ( m, n, x, '  Grid points:' )
  deallocate ( x )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests HYPERCUBE_GRID on a three dimensional example.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3

  real ( kind = 8 ), dimension ( m ) :: a = (/ -1.0D+00, -1.0D+00, -1.0D+00 /)
  real ( kind = 8 ), dimension ( m ) :: b = (/ +1.0D+00, +1.0D+00, +1.0D+00 /)
  integer ( kind = 4 ), dimension ( m ) :: c = (/ 1, 1, 1 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4vec_product
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( m ) :: ns = (/ 3, 3, 3 /)
  real ( kind = 8 ), allocatable :: x(:,:)

  n = i4vec_product ( m, ns )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Create a grid using HYPERCUBE_GRID.'
  write ( *, '(a)' ) '  Use the same parameters in every dimension.'
  write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
  write ( *, '(a,i4)' ) '  Number of grid points N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I    NS     C      A         B'
  write ( *, '(a)' ) ''
  do i = 1, m
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) i, ns(i), c(i), a(i), b(i)
  end do

  allocate ( x(1:m,1:n) )
  call hypercube_grid ( m, n, ns, a, b, c, x )
  call r8mat_transpose_print ( m, n, x, '  Grid points:' )
  deallocate ( x )

  return
end
