program main 

!*****************************************************************************80
!
!! MAIN is the main program for SQUARE_GRID_TEST.
!
!  Discussion:
!
!    SQUARE_GRID_TEST tests SQUARE_GRID.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SQUARE_GRID_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SQUARE_GRID library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SQUARE_GRID_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests SQUARE_GRID using the same parameters for all dimensions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), dimension ( 2 ) :: a = (/ -1.0D+00, -1.0D+00 /)
  real ( kind = 8 ), dimension ( 2 ) :: b = (/ +1.0D+00, +1.0D+00 /)
  integer ( kind = 4 ), dimension ( 2 ) :: c = (/ 1, 1 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( 2 ) :: ns = (/ 3, 3 /)
  real ( kind = 8 ), allocatable :: x(:,:)

  n = ns(1) * ns(2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Create a grid using SQUARE_GRID.'
  write ( *, '(a)' ) '  Use the same parameters in every dimension.'
  write ( *, '(a,i4)' ) '  Number of grid points N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I    NS     C      A         B'
  write ( *, '(a)' ) ''
  do i = 1, 2
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) i, ns(i), c(i), a(i), b(i)
  end do

  allocate ( x(1:2,1:n) )
  call square_grid ( n, ns, a, b, c, x )
  call r8mat_transpose_print ( 2, n, x, '  Grid points:' )
  deallocate ( x )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 uses a different number of points in each coordinate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), dimension ( 2 ) :: a = (/ 0.0D+00, 0.0D+00 /)
  real ( kind = 8 ), dimension ( 2 ) :: b = (/ 1.0D+00, 1.0D+00 /)
  integer ( kind = 4 ), dimension ( 2 ) :: c = (/ 2, 2 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( 2 ) :: ns = (/ 4, 2 /)
  real ( kind = 8 ), allocatable :: x(:,:)

  n = ns(1) * ns(2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Create a grid using SQUARE_GRID.'
  write ( *, '(a)' ) '  Use a different number of points in each dimension.'
  write ( *, '(a,i4)' ) '  Number of grid points N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I    NS     C      A         B'
  write ( *, '(a)' ) ''
  do i = 1, 2
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) i, ns(i), c(i), a(i), b(i)
  end do

  allocate ( x(1:2,1:n) )
  call square_grid ( n, ns, a, b, c, x )
  call r8mat_transpose_print ( 2, n, x, '  Grid points:' )
  deallocate ( x )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 uses a square with different sizes in each dimension.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), dimension ( 2 ) :: a = (/  0.0D+00, -2.0D+00 /)
  real ( kind = 8 ), dimension ( 2 ) :: b = (/ 10.0D+00, +2.0D+00 /)
  integer ( kind = 4 ), dimension ( 2 ) :: c = (/ 3, 4 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( 2 ) :: ns = (/ 3, 3 /)
  real ( kind = 8 ), allocatable :: x(:,:)

  n = ns(1) * ns(2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Use a different physical size in each dimension.'
  write ( *, '(a,i4)' ) '  Number of grid points N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I    NS     C      A         B'
  write ( *, '(a)' ) ''
  do i = 1, 2
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) i, ns(i), c(i), a(i), b(i)
  end do

  allocate ( x(1:2,1:n) )
  call square_grid ( n, ns, a, b, c, x )
  call r8mat_transpose_print ( 2, n, x, '  Grid points:' )
  deallocate ( x )

  return
end

