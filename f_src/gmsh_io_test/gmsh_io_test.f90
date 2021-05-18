program main

!*****************************************************************************80
!
!! MAIN is the main program for GMSH_IO_TEST.
!
!  Discussion:
!
!    GMSH_IO_TEST tests the GMSH_IO library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GMSH_IO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the GMSH_IO library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GMSH_IO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 gets the example 2D data and writes it to a file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: element_node(:,:)
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order
  character ( len = 255 ) gmsh_filename
  integer ( kind = 4 ) m
  integer ( kind = 4 ) node_num
  real ( kind = 8 ), allocatable :: node_x(:,:)

  gmsh_filename = 'example_2d.msh'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  Get example 2D data, write to a file.'
!
!  Get sizes.
!
  call gmsh_mesh2d_node_size_example ( node_num, m )

  call gmsh_mesh2d_element_size_example ( element_num, element_order  )
!
!  Print the sizes.
!
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of nodes = ', node_num
  write ( *, '(a,i4)' ) '  Spatial dimension = ', m
  write ( *, '(a,i4)' ) '  Number of elements = ', element_num
  write ( *, '(a,i4)' ) '  Order of elements = ', element_order
!
!  Allocate memory.
!
  allocate ( node_x(1:m,1:node_num) )
  allocate ( element_node(1:element_order,1:element_num) )
!
!  Get the data.
!
  call gmsh_mesh2d_node_data_example ( node_num, m, node_x )

  call gmsh_mesh2d_element_data_example ( element_num, element_order, &
    element_node )
!
!  Print some of the data.
!
  call r8mat_transpose_print_some ( m, node_num, node_x, &
    1, 1, m, 10, '  Coordinates for first 10 nodes:' )

  call i4mat_transpose_print_some ( element_order, element_num, element_node, &
    1, 1, element_order, 10, '  Node connectivity of first 10 elements:' )
!
!  Write the GMSH file.
!
  call gmsh_mesh2d_write ( gmsh_filename, m, node_num, node_x, &
    element_order, element_num, element_node )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Wrote example data to file "' // &
    trim ( gmsh_filename ) // '"'
!
!  Clean up.
!
  deallocate ( element_node )
  deallocate ( node_x )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 reads the example data from a file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 October 2014
!
!  Author:
!
!   John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: element_node(:,:)
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order
  character ( len = 255 ) gmsh_filename
  integer ( kind = 4 ) m
  integer ( kind = 4 ) node_num
  real ( kind = 8 ), allocatable :: node_x(:,:)

  gmsh_filename = 'example_2d.msh'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  Read data from a file.'
!
!  Get the data size.
!
  call gmsh_size_read ( gmsh_filename, node_num, m, element_num, &
    element_order )
!
!  Print the sizes.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Node data read from file "' // trim ( gmsh_filename ) // '"'
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of nodes = ', node_num
  write ( *, '(a,i4)' ) '  Spatial dimension = ', m
  write ( *, '(a,i4)' ) '  Number of elements = ', element_num
  write ( *, '(a,i4)' ) '  Element order = ', element_order
!
!  Allocate memory.
!
  allocate ( node_x(1:m,1:node_num) )
  allocate ( element_node(1:element_order,1:element_num) )
!
!  Get the data.
!
  call gmsh_data_read ( gmsh_filename, m, node_num, node_x, &
    element_order, element_num, element_node )
!
!  Print some of the data.
!
  call r8mat_transpose_print_some ( m, node_num, node_x, &
    1, 1, m, 10, '  Coordinates for first 10 nodes:' )
!
  call i4mat_transpose_print_some ( element_order, element_num, element_node, &
    1, 1, element_order, 10, '  Connectivity for first 10 elements:' )
!
!  Clean up.
!
  deallocate ( element_node )
  deallocate ( node_x )

  return
end

