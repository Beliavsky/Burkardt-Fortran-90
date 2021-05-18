program main 

!*****************************************************************************80
!
!! MAIN is the main program for CUBE_GRID_TEST.
!
!  Discussion:
!
!    CUBE_GRID_TEST tests the CUBE_GRID library.
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
  write ( *, '(a)' ) 'CUBE_GRID_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the CUBE_GRID library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CUBE_GRID_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests CUBE_GRID using the same parameters for all dimensions.
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

  real ( kind = 8 ), dimension ( 3 ) :: a = (/ -1.0D+00, -1.0D+00, -1.0D+00 /)
  real ( kind = 8 ), dimension ( 3 ) :: b = (/ +1.0D+00, +1.0D+00, +1.0D+00 /)
  integer ( kind = 4 ), dimension ( 3 ) :: c = (/ 1, 1, 1 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( 3 ) :: ns = (/ 3, 3, 3 /)
  real ( kind = 8 ), allocatable :: x(:,:)

  n = ns(1) * ns(2) * ns(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Create a grid using CUBE_GRID.'
  write ( *, '(a)' ) '  Use the same parameters in every dimension.'
  write ( *, '(a,i4)' ) '  Number of grid points N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I    NS     C      A         B'
  write ( *, '(a)' ) ''
  do i = 1, 3
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) i, ns(i), c(i), a(i), b(i)
  end do

  allocate ( x(1:3,1:n) )
  call cube_grid ( n, ns, a, b, c, x )
  call r8mat_transpose_print ( 3, n, x, '  Grid points:' )
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

  real ( kind = 8 ), dimension ( 3 ) :: a = (/ 0.0D+00, 0.0D+00, 0.0D+00 /)
  real ( kind = 8 ), dimension ( 3 ) :: b = (/ 1.0D+00, 1.0D+00, 1.0D+00 /)
  integer ( kind = 4 ), dimension ( 3 ) :: c = (/ 2, 2, 2 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( 3 ) :: ns = (/ 4, 2, 3 /)
  real ( kind = 8 ), allocatable :: x(:,:)

  n = ns(1) * ns(2) * ns(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Create a grid using CUBE_GRID.'
  write ( *, '(a)' ) '  Use a different number of points in each dimension.'
  write ( *, '(a,i4)' ) '  Number of grid points N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I    NS     C      A         B'
  write ( *, '(a)' ) ''
  do i = 1, 3
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) i, ns(i), c(i), a(i), b(i)
  end do

  allocate ( x(1:3,1:n) )
  call cube_grid ( n, ns, a, b, c, x )
  call r8mat_transpose_print ( 3, n, x, '  Grid points:' )
  deallocate ( x )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 uses a cube with different sizes in each dimension.
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

  real ( kind = 8 ), dimension ( 3 ) :: a = (/  0.0D+00, -2.0D+00, 50.0D+00 /)
  real ( kind = 8 ), dimension ( 3 ) :: b = (/ 10.0D+00, +2.0D+00, 51.0D+00 /)
  integer ( kind = 4 ), dimension ( 3 ) :: c = (/ 3, 4, 5 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( 3 ) :: ns = (/ 3, 3, 3 /)
  real ( kind = 8 ), allocatable :: x(:,:)

  n = ns(1) * ns(2) * ns(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Use a different physical size in each dimension.'
  write ( *, '(a,i4)' ) '  Number of grid points N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I    NS     C      A         B'
  write ( *, '(a)' ) ''
  do i = 1, 3
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) i, ns(i), c(i), a(i), b(i)
  end do

  allocate ( x(1:3,1:n) )
  call cube_grid ( n, ns, a, b, c, x )
  call r8mat_transpose_print ( 3, n, x, '  Grid points:' )
  deallocate ( x )

  return
end

