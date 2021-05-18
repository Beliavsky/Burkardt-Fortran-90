program main

!*****************************************************************************80
!
!! MAIN is the main program for POLYGON_GRID_TEST.
!
!  Discussion:
!
!    POLYGON_GRID_TEST tests the POLYGON_GRID library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYGON_GRID_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the POLYGON_GRID library.'

  call polygon_grid_count_test ( )

  call polygon_grid_points_test01 ( )
  call polygon_grid_points_test02 ( )
  call polygon_grid_points_test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYGON_GRID_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine polygon_grid_count_test ( )

!*****************************************************************************80
!
!! POLYGON_GRID_COUNT_TEST tests POLYGON_GRID_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ng
  integer ( kind = 4 ) nv

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYGON_GRID_COUNT_TEST:'
  write ( *, '(a)' ) '  POLYGON_GRID_COUNT counts NG, the number of points in'
  write ( *, '(a)' ) '  a grid defined with N+1 points on each side of a'
  write ( *, '(a)' ) '  polygon of NV vertices.'

  do nv = 3, 5
    write ( *, '(a)' ) ''
    write ( *, '(a,i2)' ) '  Polygonal vertex count NV = ', nv
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '   N     NG'
    write ( *, '(a)' ) ''
    do n = 0, 5
      call polygon_grid_count ( n, nv, ng )
      write ( *, '(2x,i2,2x,i5)' ) n, ng
    end do
  end do

  return
end
subroutine polygon_grid_points_test01 ( )

!*****************************************************************************80
!
!! POLYGON_GRID_POINTS_TEST01 tests POLYGON_GRID_POINTS
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nv = 3

  character ( len = 255 ) filename
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ng
  character ( len = 255 ) prefix
  real ( kind = 8 ), dimension ( 2, nv ) :: v = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    0.5D+00, 0.86602540378443860D+00 /), (/ 2, nv /) )
  real ( kind = 8 ), allocatable :: xg(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYGON_GRID_POINTS_TEST01:'
  write ( *, '(a)' ) '  POLYGON_GRID_POINTS returns grid points for a polygon'
  write ( *, '(a)' ) '  of NV vertices, with N+1 points on a side'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  For this test, the polygon is a triangle.'

  call r8mat_transpose_print ( 2, nv, v, '  Polygon vertices:' )
!
!  Count the grid points.
!
  n = 5
  call polygon_grid_count ( n, nv, ng )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  N = ', n
  write ( *, '(a,i4)' ) '  Number of grid points will be NG = ', ng
!
!  Compute the grid points.
!
  allocate ( xg(1:2,1:ng) )

  call polygon_grid_points ( n, nv, v, ng, xg )

  call r8mat_transpose_print ( 2, ng, xg, '  The grid point array:' )
!
!  Display the points.
!
  prefix = 'triangle'

  call polygon_grid_display ( n, nv, v, ng, xg, prefix )
!
!  Write the points to a file.
!
  filename = 'triangle.xy'

  call r8mat_write ( filename, 2, ng, xg )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Data written to the file "' // trim ( filename ) // '"'

  deallocate ( xg )

  return
end
subroutine polygon_grid_points_test02 ( )

!*****************************************************************************80
!
!! POLYGON_GRID_POINTS_TEST02 tests POLYGON_GRID_POINTS
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nv = 4

  character ( len = 255 ) filename
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ng
  character ( len = 255 ) prefix
  real ( kind = 8 ), dimension ( 2, nv ) :: v = reshape ( (/ &
    1.0D+00, 1.0D+00, &
    2.0D+00, 0.0D+00, &
    4.0D+00, 3.0D+00, &
    0.0D+00, 5.0D+00 /), (/ 2, nv /) )
  real ( kind = 8 ), allocatable :: xg(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYGON_GRID_POINTS_TEST02:'
  write ( *, '(a)' ) '  POLYGON_GRID_POINTS returns grid points for a polygon'
  write ( *, '(a)' ) '  of NV vertices, with N+1 points on a side'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  For this test, the polygon is a convex quadrilateral'
  write ( *, '(a)' ) '  with sides of varying length.'
!
!  Define the polygon.
!
  call r8mat_transpose_print ( 2, nv, v, '  Polygon vertices:' )
!
!  Count the grid points.
!
  n = 7
  call polygon_grid_count ( n, nv, ng )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  N = ', n
  write ( *, '(a,i4)' ) '  Number of grid points will be NG = ', ng
!
!  Compute the grid points.
!
  allocate ( xg(1:2,1:ng) )

  call polygon_grid_points ( n, nv, v, ng, xg )

  call r8mat_transpose_print ( 2, ng, xg, '  The grid point array:' )
!
!  Display the points.
!
  prefix = 'quad'

  call polygon_grid_display ( n, nv, v, ng, xg, prefix )
!
!  Write the points to a file.
!
  filename = 'quad.xy'

  call r8mat_write ( filename, 2, ng, xg )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Data written to the file "' // trim ( filename ) // '"'

  deallocate ( xg )

  return
end
subroutine polygon_grid_points_test03 ( )

!*****************************************************************************80
!
!! POLYGON_GRID_POINTS_TEST03 tests POLYGON_GRID_POINTS
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nv = 6

  character ( len = 255 ) filename
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ng
  character ( len = 255 ) prefix
  real ( kind = 8 ), dimension ( 2, nv ) :: v = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    2.0D+00, 0.0D+00, &
    2.0D+00, 1.0D+00, &
    1.0D+00, 1.0D+00, &
    1.0D+00, 2.0D+00, &
    0.0D+00, 2.0D+00 /), (/ 2, nv /) )
  real ( kind = 8 ), allocatable :: xg(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYGON_GRID_POINTS_TEST03:'
  write ( *, '(a)' ) '  POLYGON_GRID_POINTS returns grid points for a polygon'
  write ( *, '(a)' ) '  of NV vertices, with N+1 points on a side'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  For this test, the polygon is nonconvex and six sided.'
  write ( *, '(a)' ) '  Two degenerate triangles are created, and some grid points'
  write ( *, '(a)' ) '  are generated several times.'
!
!  Define the polygon.
!
  call r8mat_transpose_print ( 2, nv, v, '  Polygon vertices:' )
!
!  Count the grid points.
!
  n = 5
  call polygon_grid_count ( n, nv, ng )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  N = ', n
  write ( *, '(a,i4)' ) '  Number of grid points will be NG = ', ng
!
!  Compute the grid points.
!
  allocate ( xg(1:2,1:ng) )

  call polygon_grid_points ( n, nv, v, ng, xg )

  call r8mat_transpose_print ( 2, ng, xg, '  The grid point array:' )
!
!  Display the points.
!
  prefix = 'ell'

  call polygon_grid_display ( n, nv, v, ng, xg, prefix )
!
!  Write the points to a file.
!
  filename = 'ell.xy'

  call r8mat_write ( filename, 2, ng, xg )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Data written to the file "' // trim ( filename ) // '"'

  deallocate ( xg )

  return
end

