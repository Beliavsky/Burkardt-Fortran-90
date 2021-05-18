program main

!*****************************************************************************80
!
!! MAIN is the main program for POLYGON_TRIANGULATE_TEST.
!
!  Discussion:
!
!    POLYGON_TRIANGULATE_TEST tests the POLYGON_TRIANGULATE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYGON_TRIANGULATE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the POLYGON_TRIANGULATE library.'

  call test01 ( )

  call test02 ( 'comb' )
  call test02 ( 'hand' )
  call test02 ( 'i18' )

  call test03 ( 'comb' )
  call test03 ( 'hand' )
  call test03 ( 'i18' )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYGON_TRIANGULATE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests the "comb_10" polygon.
!
!  Discussion:
!
!    There are N-3 triangles in the triangulation.
!
!    For the first N-2 triangles, the first edge listed is always an
!    internal diagonal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) triangles(3,n-2)
  real ( kind = 8 ), dimension ( n ) :: x = (/ &
    8.0D+00, 7.0D+00, 6.0D+00, 5.0D+00, 4.0D+00, &
    3.0D+00, 2.0D+00, 1.0D+00, 0.0D+00, 4.0D+00 /)
  real ( kind = 8 ), dimension ( n ) :: y = (/ &
    0.0D+00, 10.0D+00,  0.0D+00, 10.0D+00,  0.0D+00, &
   10.0D+00,  0.0D+00, 10.0D+00,  0.0D+00, -2.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Triangulate the comb_10 polygon.'

  call polygon_triangulate ( n, x, y, triangles )

  call i4mat_transpose_print ( 3, n - 2, triangles, '  Triangles' )

  return
end
subroutine test02 ( prefix )

!*****************************************************************************80
!
!! TEST02 triangulates a polygon described in a file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    John Burkardt.
!
  implicit none

  integer ( kind = 4 ) dim_num
  character ( len = 255 ) element_filename
  integer ( kind = 4 ) n
  character ( len = 255 ) node_filename
  character ( len = * ) prefix
  integer ( kind = 4 ) triangle_num
  integer ( kind = 4 ), allocatable :: triangles(:,:)
  real ( kind = 8 ), allocatable :: xy(:,:)
!
!  Create filenames.
!
  node_filename = trim ( prefix ) // '_nodes.txt'
  element_filename = trim ( prefix ) // '_elements.txt'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Read polygon coordinates in "' &
    // trim ( node_filename ) // '"'
!
!  Read the node coordinates.
!
  call r8mat_header_read ( node_filename, dim_num, n )

  allocate ( xy(1:2,1:n) )

  call r8mat_data_read ( node_filename, 2, n, xy )
!
!  Get the triangulation.
!
  allocate ( triangles(1:3,1:n-2) )

  call polygon_triangulate ( n, xy(1,:), xy(2,:), triangles )
!
!  Write the triangulation to a file.
!
  triangle_num = n - 2
  call i4mat_write ( element_filename, 3, triangle_num, triangles )

  write ( *, '(a)' ) '  Write triangulation to "' &
    // trim ( element_filename ) // '"'
 
  deallocate ( triangles )
  deallocate ( xy )

  return
end
subroutine test03 ( prefix )

!*****************************************************************************80
!
!! TEST03 plots a triangulation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2014
!
!  Author:
!
!    John Burkardt.
!
  implicit none

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) diagonal_filename
  integer ( kind = 4 ) diagonal_unit
  integer ( kind = 4 ) dim_num
  character ( len = 255 ) edge_filename
  integer ( kind = 4 ) edge_unit
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) node
  character ( len = 255 ) node_filename
  character ( len = 255 ) plot_filename
  character ( len = * ) prefix
  integer ( kind = 4 ), allocatable :: triangles(:,:)
  real ( kind = 8 ), allocatable :: xy(:,:)
!
!  Create filenames.
!
  node_filename = trim ( prefix ) // '_nodes.txt'
  edge_filename = trim ( prefix ) // '_edges.txt'
  diagonal_filename = trim ( prefix ) // '_diagonals.txt'
  command_filename = trim ( prefix ) // '_commands.txt'
  plot_filename = trim ( prefix ) // '.png'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Read node coordinates in "' // trim ( node_filename ) // '"'
!
!  Read the node coordinates.
!
  call r8mat_header_read ( node_filename, dim_num, n )

  allocate ( xy(1:2,1:n) )

  call r8mat_data_read ( node_filename, 2, n, xy )
!
!  Get the triangulation.
!
  allocate ( triangles(1:3,1:n-2) )

  call polygon_triangulate ( n, xy(1,:), xy(2,:), triangles )
!
!  Plot the edges.
!
  call get_unit ( edge_unit )
  open ( unit = edge_unit, file = edge_filename, status = 'replace' )

  do j = 1, n + 1
    j2 = mod ( j - 1, n ) + 1
    write ( edge_unit, '(g14.6,2x,g14.6)' ) xy(1,j2), xy(2,j2)
  end do

  close ( unit = edge_unit )
!
!  Plot the diagonals.
!
  call get_unit ( diagonal_unit )
  open ( unit = diagonal_unit, file = diagonal_filename, status = 'replace' )

  do j = 1, n - 3
    do i = 1, 2
      node = triangles(i,j)
      write ( diagonal_unit, '(g14.6,2x,g14.6)' ) xy(1,node), xy(2,node)
    end do
    write ( diagonal_unit, '(1x)' )
  end do

  close ( unit = diagonal_unit )
!
!  Write the GNUPLOT command file.
!
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set nokey'
  write ( command_unit, '(a)' ) 'set size ratio 1'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
  write ( command_unit, '(a)' ) &
    'set title "Edges (green) and Diagonals (red)"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( edge_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "green",\'
  write ( command_unit, '(a)' ) '     "' // trim ( diagonal_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "red",\'
  write ( command_unit, '(a)' ) '     "' // trim ( node_filename ) // &
    '" using 1:2 with points pt 7 ps 2 lc rgb "black"'

  close ( unit = command_unit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Write edges to "' &
    // trim ( edge_filename ) // '"'
  write ( *, '(a)' ) '  Write diagonals to "' &
    // trim ( diagonal_filename ) // '"'
  write ( *, '(a)' ) '  Write gnuplot commands to "' &
    // trim ( command_filename ) // '"'

  return
end

