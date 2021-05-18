program main

!*****************************************************************************80
!
!! MAIN is the main program for SPHERE_GRID_TEST.
!
!  Discussion:
!
!    SPHERE_GRID_TEST tests the SPHERE_GRID library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPHERE_GRID_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SPHERE_GRID library.'
 
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
  call test11 ( )
  call test12 ( )
  call test13 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPHERE_GRID_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests SPHERE_GRID_ICOS_NUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) edge_num
  integer ( kind = 4 ) face_num
  integer ( kind = 4 ) factor
  integer ( kind = 4 ) factor_log
  integer ( kind = 4 ) point_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  SPHERE_ICOS_POINT_NUM determines the size'
  write ( *, '(a)' ) '  (number of vertices, edges and faces) in a grid'
  write ( *, '(a)' ) '  on a sphere, made by subdividing an initial'
  write ( *, '(a)' ) '  projected icosahedron.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  N determines the number of subdivisions of each'
  write ( *, '(a)' ) '  edge of the icosahedral faces.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         N         V         E         F'
  write ( *, '(a)' ) '  --------  --------  --------  --------'
  write ( *, '(a)' ) ' '

  do factor = 1, 20
    call sphere_icos_point_num ( factor, point_num )
    call sphere_icos_edge_num ( factor, edge_num )
    call sphere_icos_face_num ( factor, face_num )
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) &
      factor, point_num, edge_num, face_num
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Repeat, but using N constrained by doubling:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         N         V         E         F'
  write ( *, '(a)' ) '  --------  --------  --------  --------'
  write ( *, '(a)' ) ' '

  factor = 1
  do factor_log = 0, 10
    call sphere_icos_point_num ( factor, point_num )
    call sphere_icos_edge_num ( factor, edge_num )
    call sphere_icos_face_num ( factor, face_num )
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) &
      factor, point_num, edge_num, face_num
    factor = factor * 2
  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests SPHERE_ICOS1_POINTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) edge_num
  integer ( kind = 4 ) factor
  character ( len = 80 ) filename
  integer ( kind = 4 ) node
  integer ( kind = 4 ) node_num
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: node_xyz
  integer ( kind = 4 ) triangle_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  SPHERE_ICOS_POINT_NUM "sizes" a grid generated'
  write ( *, '(a)' ) '  on an icosahedron and projected to a sphere.'
  write ( *, '(a)' ) '  SPHERE_ICOS1_POINTS creates the grid points.'

  factor = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Sizing factor =       ', factor

  call sphere_icos_point_num ( factor, node_num, edge_num, triangle_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of vertices =  ', node_num
  write ( *, '(a,i8)' ) '  Number of edges =     ', edge_num
  write ( *, '(a,i8)' ) '  Number of faces =     ', triangle_num

  allocate ( node_xyz(1:3,1:node_num) )

  call sphere_icos1_points ( factor, node_num, node_xyz )

  call r8mat_transpose_print_some ( 3, node_num, node_xyz, 1, 1, 3, 20, &
    '  Initial part of NODE_XYZ array:' )
!
!  Write the nodes to a file.
!
  write ( filename, '(a,i2.2,a)' ) 'sphere_icos1_points_f', factor, '.xyz'

  call r8mat_write ( filename, 3, node_num, node_xyz )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Wrote grid nodes to "' // trim ( filename ) // '".'

  deallocate ( node_xyz )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests SPHERE_ICOS2_POINTS
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) edge_num
  integer ( kind = 4 ) factor
  character ( len = 80 ) filename
  integer ( kind = 4 ) node
  integer ( kind = 4 ) node_num
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: node_xyz
  integer ( kind = 4 ) triangle_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  SPHERE_ICOS_POINT_NUM "sizes" a grid generated'
  write ( *, '(a)' ) '  on an icosahedron and projected to a sphere.'
  write ( *, '(a)' ) '  SPHERE_ICOS2_POINTS creates the grid.'

  factor = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Sizing factor FACTOR = ', factor

  call sphere_icos_point_num ( factor, node_num, edge_num, triangle_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of nodes =     ', node_num
  write ( *, '(a,i8)' ) '  Number of edges =     ', edge_num
  write ( *, '(a,i8)' ) '  Number of triangles = ', triangle_num

  allocate ( node_xyz(1:3,1:node_num) )

  call sphere_icos2_points ( factor, node_num, node_xyz )

  call r8mat_transpose_print_some ( 3, node_num, node_xyz, 1, 1, 3, 20, &
    '  Initial part of NODE_XYZ array:' )
!
!  Write the nodes to a file.
!
  write ( filename, '(a,i2.2,a)' ) 'sphere_icos2_points_f', factor, '.xyz'

  call r8mat_write ( filename, 3, node_num, node_xyz )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Wrote grid nodes to "' // trim ( filename ) // '".'

  deallocate ( node_xyz )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests SPHERE_LL_POINTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 October 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: lat_num = 3
  integer ( kind = 4 ), parameter :: long_num = 4

  real ( kind = 8 ), dimension ( 3 ) :: pc = (/ &
    0.0D+00, 0.0D+00, 0.0D+00 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) node_num
  real ( kind = 8 ) node_xyz(3,2+lat_num*long_num)
  real ( kind = 8 ) :: r = 10.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  SPHERE_LL_POINTS produces latitude/longitude'
  write ( *, '(a)' ) '  points on a sphere in 3D.'

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Radius = ', r

  call r8vec_print ( 3, pc, '  Center:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The number of latitudes =  ', lat_num
  write ( *, '(a,i8)' ) '  The number of longitudes = ', long_num

  call sphere_ll_point_num ( lat_num, long_num, node_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The number of grid points is ', node_num

  call sphere_ll_points ( r, pc, lat_num, long_num, node_num, node_xyz )

  k = 1
  write ( *, '(2x,i8,3g14.6)' ) k, node_xyz(1:3,k)

  do i = 1, lat_num
    write ( *, '(a)' ) ' '
    do j = 0, long_num - 1
      k = k + 1
      write ( *, '(2x,i8,3g14.6)' ) k, node_xyz(1:3,k)
    end do
  end do

  k = k + 1
  write ( *, '(a)' ) ' '
  write ( *, '(2x,i8,3g14.6)' ) k, node_xyz(1:3,k)

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests SPHERE_SPIRALPOINTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: node_num = 500

  character ( len = 80 ) filename
  real ( kind = 8 ), dimension ( 3 ) :: center_xyz = (/ &
    0.0D+00, 0.0D+00, 0.0D+00 /)
  integer   ( kind = 4 ) node
  real ( kind = 8 ) node_xyz(3,node_num)
  real ( kind = 8 ) :: r = 1.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  SPHERE_SPIRALPOINTS produces a spiral of'
  write ( *, '(a)' ) '  points on a sphere in 3D.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Radius = ', r

  call r8vec_print ( 3, center_xyz, '  Center:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The number of spiral points is ', node_num

  call sphere_spiralpoints ( r, center_xyz, node_num, node_xyz )

  call r8mat_transpose_print_some ( 3, node_num, node_xyz, 1, 1, 3, 10, &
    '  The spiral points:' )
!
!  Write the nodes to a file.
!
  write ( filename, '(a,i4.4,a)' ) 'sphere_grid_spiral_n', node_num, '.xyz'

  call r8mat_write ( filename, 3, node_num, node_xyz )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Wrote grid nodes to "' // trim ( filename ) // '".'

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 tests SPHERE_LL_LINES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: line_max = 1000

  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: lat_num = 3
  integer ( kind = 4 ) line(2,line_max)
  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) :: long_num = 4

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST06'
  write ( *, '(a)' ) '  SPHERE_LL_LINES computes latitude/longitude lines'
  write ( *, '(a)' ) '  on a sphere in 3D.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of latitudes is  ', lat_num
  write ( *, '(a,i8)' ) '  Number of longitudes is ', long_num

  call sphere_ll_line_num ( lat_num, long_num, line_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of line segments is ', line_num

  call sphere_ll_lines ( lat_num, long_num, line_num, line )

  call i4mat_transpose_print ( 2, line_num, line, '  Grid line vertices:' )

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! TEST07 tests SPHERE_GRID_Q4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: lat_num = 3
  integer ( kind = 4 ), parameter :: long_num = 4

  integer ( kind = 4 ), parameter :: rectangle_num = lat_num * long_num

  integer ( kind = 4 ) rectangle_node(4,rectangle_num)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST07'
  write ( *, '(a)' ) '  SPHERE_GRID_Q4 computes a grid'
  write ( *, '(a)' ) '  of Q4 rectangular elements on a sphere in 3D.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of latitudes is      ', lat_num
  write ( *, '(a,i8)' ) '  Number of longitudes is     ', long_num
  write ( *, '(a,i8)' ) '  The number of rectangles is ', rectangle_num

  call sphere_grid_q4 ( lat_num, long_num, rectangle_node )

  call i4mat_transpose_print ( 4, rectangle_num, rectangle_node, &
    '  Rectangle vertices:' )

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 tests SPHERE_GRID_T3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: lat_num = 3
  integer ( kind = 4 ), parameter :: long_num = 4

  integer ( kind = 4 ) triangle_num
  integer ( kind = 4 ) triangle_node(3,2*(lat_num+1)*long_num)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST08'
  write ( *, '(a)' ) '  SPHERE_GRID_T3 computes a grid'
  write ( *, '(a)' ) '  of T3 triangular elements on a sphere in 3D.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of latitudes is  ', lat_num
  write ( *, '(a,i8)' ) '  Number of longitudes is ', long_num

  call sphere_grid_t3 ( lat_num, long_num, triangle_node )

  triangle_num = 2 * ( lat_num + 1 ) * long_num

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The number of triangles is ', triangle_num

  call i4mat_transpose_print ( 3, triangle_num, triangle_node, &
    '  Triangle vertices:' )

  return
end
subroutine test09 ( )

!*****************************************************************************80
!
!! TEST09 tests SPHERE_UNIT_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 80 ) filename
  integer ( kind = 4 ) node
  integer ( kind = 4 ) node_num
  real ( kind = 8 ), allocatable :: node_xyz(:,: )
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST09'
  write ( *, '(a)' ) '  For the unit sphere in 3 dimensions:'
  write ( *, '(a)' ) '  SPHERE_UNIT_SAMPLE does a random sampling.'

  node_num = 1000

  allocate ( node_xyz(1:3,1:node_num) )

  call sphere_unit_sample ( node_num, seed, node_xyz )

  call r8mat_transpose_print_some ( 3, node_num, node_xyz, 1, 1, 3, 10, &
    '  First 10 values:' )
!
!  Write the nodes to a file.
!
  write ( filename, '(a,i6.6,a)' ) 'sphere_sample_n', node_num, '.xyz'

  call r8mat_write ( filename, 3, node_num, node_xyz )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Wrote grid nodes to "' // trim ( filename ) // '".'

  deallocate ( node_xyz )

  return
end
subroutine test10 ( )

!*****************************************************************************80
!
!! TEST10 tests SPHERE_CUBED_POINTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 October 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 80 ) filename
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  real ( kind = 8 ), allocatable :: xyz(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST10'
  write ( *, '(a)' ) '  SPHERE_CUBED_POINTS computes points on a cubed sphere grid.'

  n = 10
  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of divisions on each face = ', n

  call sphere_cubed_point_num ( n, ns );
  write ( *, '(a,i6)' ) '  Total number of points = ', ns

  allocate ( xyz(1:3,1:ns) )

  call sphere_cubed_points ( n, ns, xyz )

  call r8mat_transpose_print_some ( 3, ns, xyz, 1, 1, 3, 20, &
    '  Initial part of XYZ array:' )
!
!  Write the nodes to a file.
!
  write ( filename, '(a,i6.6,a)' ) 'sphere_cubed_f', n, '.xyz'

  call r8mat_write ( filename, 3, ns, xyz )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Wrote grid nodes to "' // trim ( filename ) // '".'

  deallocate ( xyz )

  return
end
subroutine test11 ( )

!*****************************************************************************80
!
!! TEST11 tests SPHERE_LL_LINES and SPHERE_LL_POINTS.
!
!  Discussion:
!
!    The MATLAB version of this routine displays the computed results
!    graphically.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) lat_num
  integer ( kind = 4 ), allocatable :: line_data(:,:)
  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) long_num
  real ( kind = 8 ) margin
  integer ( kind = 4 ) node_num
  real ( kind = 8 ), allocatable :: node_xyz(:,:)
  real ( kind = 8 ) pc(3)
  real ( kind = 8 ) r
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ) xyz_range(3)
  real ( kind = 8 ) xyz_max(3)
  real ( kind = 8 ) xyz_min(3)
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min
  real ( kind = 8 ) z_max
  real ( kind = 8 ) z_min

  lat_num = 22
  long_num = 24
  pc(1:3) = (/ 0.0D+00, 0.0D+00, 0.0D+00 /)
  r = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST11'
  write ( *, '(a)' ) '  SPHERE_LL_POINTS computes points on a'
  write ( *, '(a)' ) '  latitude/longitude grid'
  write ( *, '(a)' ) '  SPHERE_LL_LINES computes lines on a'
  write ( *, '(a)' ) '  latitude/longitude grid'
  write ( *, '(a)' ) '  Use this information to draw a globe with a grid.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of latitudes is  ', lat_num
  write ( *, '(a,i6)' ) '  Number of longitudes is ', long_num
!
!  Get points.
!
  call sphere_ll_point_num ( lat_num, long_num, node_num )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  The number of grid points is ', node_num

  allocate ( node_xyz(1:3,1:node_num) )
  call sphere_ll_points ( r, pc, lat_num, long_num, node_num, node_xyz )
!
!  Get lines.
!
  call sphere_ll_line_num ( lat_num, long_num, line_num )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of line segments is ', line_num

  allocate ( line_data(1:2,1:line_num) )
  call sphere_ll_lines ( lat_num, long_num, line_num, line_data )
!
!  Get the scale.
!
  xyz_min(1) = minval ( node_xyz(1,1:node_num) )
  xyz_max(1) = maxval ( node_xyz(1,1:node_num) )

  xyz_min(2) = minval ( node_xyz(2,1:node_num) )
  xyz_max(2) = maxval ( node_xyz(2,1:node_num) )

  xyz_min(3) = minval ( node_xyz(3,1:node_num) )
  xyz_max(3) = maxval ( node_xyz(3,1:node_num) )

  xyz_range(1:3) = xyz_max(1:3) - xyz_min(1:3);

  margin = 0.025D+00 * maxval ( xyz_range(1:3) )

  x_min = xyz_min(1) - margin;
  x_max = xyz_max(1) + margin;
  y_min = xyz_min(2) - margin;
  y_max = xyz_max(2) + margin;
  z_min = xyz_min(3) - margin;
  z_max = xyz_max(3) + margin;
!
!  Draw just the points.
!
! figure ( )
! clf
! hold on
! point_size = 50;
! point_color = [ 0.0, 1.0, 0.0 ];
! scatter3 ( node_xyz(1,:), node_xyz(2,:), node_xyz(3,:), point_size, 'k', 'filled' );
!
!  We want to include a sphere in the picture.  But if the sphere is close to
!  the correct radius, lines that should lie on the surface because they are
!  curved will actually tunnel through the surface because we draw them straight.
!
! [ x, y, z ] = sphere ( 40 );
! x = 0.95 * r * x;
! y = 0.95 * r * y;
! z = 0.95 * r * z;
!
! c = z ./ z;
! surf ( x, y, z, c, 'EdgeColor', 'None' );
!
! axis equal
! grid on
! xlabel ( '--X axis--' )
! ylabel ( '--Y axis--' )
! zlabel ( '--Z axis--' )
! view ( 3 )
! hold off
!
! filename = 'sphere_ll_points.png';
! print ( '-dpng', filename );
! fprintf ( 1, '\n' );
! fprintf ( 1, '  Saving plot as "%s".\n', filename );
!
!  Draw the grid (points and lines)
!  As usual, trying to get Matlab's graphics routines to cooperate is
!  a nightmare.
!
! clf
! hold on
! point_size = 50;
! point_color = [ 0.0, 1.0, 0.0 ];
! scatter3 ( node_xyz(1,:), node_xyz(2,:), node_xyz(3,:), point_size, 'k', 'filled' );
!
! for i = 1 : line_num
!   p = [ line_data(1,i), line_data(2,i) ];
!   line ( node_xyz(1,p), node_xyz(2,p), node_xyz(3,p), 'LineWidth', 2 );
! end
!
!  We want to include a sphere in the picture.  But if the sphere is close to
!  the correct radius, lines that should lie on the surface because they are
!  curved will actually tunnel through the surface because we draw them straight.
!
! [ x, y, z ] = sphere ( 40 );
! x = 0.95 * r * x;
! y = 0.95 * r * y;
! z = 0.95 * r * z;
! c = z ./ z;
! surf ( x, y, z, c, 'EdgeColor', 'None' );\
!
! axis equal
!
! grid on
! xlabel ( '--X axis--' )
! ylabel ( '--Y axis--' )
! zlabel ( '--Z axis--' )
! view ( 3 )
! hold off
!
! filename = 'sphere_ll_grid.png';
! print ( '-dpng', filename );
! fprintf ( 1, '\n' );
! fprintf ( 1, '  Saving plot as "%s".\n', filename );
 
  deallocate ( node_xyz )

  return
end
subroutine test12 ( )

!*****************************************************************************80
!
!! TEST12 is just a dummy for now, but should be copied from MATLAB version.
!
  return
end
subroutine test13 ( )

!*****************************************************************************80
!
!! TEST13 is just a dummy for now, but should be copied from MATLAB version.
!
  return
end

