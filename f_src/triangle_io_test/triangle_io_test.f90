program main

!*****************************************************************************80
!
!! MAIN is the main program for TRIANGLE_IO_TEST.
!
!  Discussion:
!
!    TRIANGLE_IO_TEST tests the TRIANGLE_IO library.
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
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_IO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the TRIANGLE_IO library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_IO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 gets the example node data and writes it to a file.
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

  real ( kind = 8 ), allocatable :: node_att(:,:)
  integer ( kind = 4 ) node_att_num
  real ( kind = 8 ), allocatable :: node_coord(:,:)
  integer ( kind = 4 ) node_dim
  character ( len = 255 ) node_filename
  integer ( kind = 4 ), allocatable :: node_marker(:,:)
  integer ( kind = 4 ) node_marker_num
  integer ( kind = 4 ) node_num

  node_filename = 'example.node'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  Get example node data, write to a triangle node file.'
!
!  Get node example size.
!
  call triangle_node_size_example ( node_num, node_dim, node_att_num, &
    node_marker_num )
!
!  Print the sizes.
!
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of nodes = ', node_num
  write ( *, '(a,i4)' ) '  Spatial dimension = ', node_dim
  write ( *, '(a,i4)' ) '  Number of node attributes = ', node_att_num
  write ( *, '(a,i4)' ) '  Number of node markers = ', node_marker_num
!
!  Allocate memory for node data.
!
  allocate ( node_coord(1:node_dim,1:node_num) )
  allocate ( node_att(1:node_att_num,1:node_num) )
  allocate ( node_marker(1:node_marker_num,node_num) )
!
!  Get the node data.
!
  call triangle_node_data_example ( node_num, node_dim, node_att_num, &
    node_marker_num, node_coord, node_att, node_marker )
!
!  Print some of the data.
!
  call r8mat_transpose_print_some ( node_dim, node_num, node_coord, &
    1, 1, node_dim, 10, '  Coordinates for first 10 nodes:' )

  call r8mat_transpose_print_some ( node_att_num, node_num, node_att, &
    1, 1, node_att_num, 10, '  Attributes for first 10 nodes:' )

  call i4mat_transpose_print_some ( node_marker_num, node_num, node_marker, &
    1, 1, node_marker_num, 10, '  Markers for first 10 nodes:' ) 
!
!  Write the node information to node file.
!
  call triangle_node_write ( node_filename, node_num, node_dim, node_att_num, &
    node_marker_num, node_coord, node_att, node_marker )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Node data written to file "' // trim ( node_filename ) // '"'
!
!  Clean up.
!
  deallocate ( node_att )
  deallocate ( node_coord )
  deallocate ( node_marker )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 gets the example element data and writes it to a file.
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

  real ( kind = 8 ), allocatable :: element_att(:,:)
  integer ( kind = 4 ) element_att_num
  character ( len = 255 ) element_filename
  integer ( kind = 4 ), allocatable :: element_node(:,:)
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order

  element_filename = 'element.ele'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  Get example element data, write to a triangle element file.'
!
!  Get element example size.
!
  call triangle_element_size_example ( element_num, element_order, &
    element_att_num )
!
!  Print the sizes.
!
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of elements = ', element_num
  write ( *, '(a,i4)' ) '  Order of elements = ', element_order
  write ( *, '(a,i4)' ) '  Number of element attributes = ', element_att_num
!
!  Allocate memory.
!
  allocate ( element_node(1:element_order,1:element_num) )
  allocate ( element_att(1:element_att_num,1:element_num ) )
!
!  Get the data.
!
  call triangle_element_data_example ( element_num, element_order, &
    element_att_num, element_node, element_att )
!
!  Print some of the data.
!
  call i4mat_transpose_print_some ( element_order, element_num, element_node, &
    1, 1, element_order, 10, '  Node connectivity of first 10 elements:' )

  call r8mat_transpose_print_some ( element_att_num, element_num, element_att, &
    1, 1, element_att_num, 10, '  Attributes for first 10 elements:' ) 
!
!  Write the node information to node file.
!
  call triangle_element_write ( element_filename, element_num, element_order, &
    element_att_num, element_node, element_att )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Element data written to file "' // trim ( element_filename ) // '"'
!
!  Clean up.
!
  deallocate ( element_att )
  deallocate ( element_node )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 reads the example node data from a file.
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
!   John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: node_att(:,:)
  integer ( kind = 4 ) node_att_num
  real ( kind = 8 ), allocatable :: node_coord(:,:)
  integer ( kind = 4 ) node_dim
  character ( len = 255 ) node_filename
  integer ( kind = 4 ), allocatable :: node_marker(:,:)
  integer ( kind = 4 ) node_marker_num
  integer ( kind = 4 ) node_num

  node_filename = 'example.node'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03:'
  write ( *, '(a)' ) '  Read node data from a node file.'
!
!  Get the data size.
!
  call triangle_node_size_read ( node_filename, node_num, node_dim, &
    node_att_num, node_marker_num )
!
!  Print the sizes.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Node data read from file "' // trim ( node_filename ) // '"'
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of nodes = ', node_num
  write ( *, '(a,i4)' ) '  Spatial dimension = ', node_dim
  write ( *, '(a,i4)' ) '  Number of node attributes = ', node_att_num
  write ( *, '(a,i4)' ) '  Number of node markers = ', node_marker_num
!
!  Allocate memory.
!
  allocate ( node_coord(1:node_dim,1:node_num) )
  allocate ( node_att(1:node_att_num,1:node_num) )
  allocate ( node_marker(1:node_marker_num,node_num) )
!
!  Get the data.
!
  call triangle_node_data_read ( node_filename, node_num, node_dim, &
    node_att_num, node_marker_num, node_coord, node_att, node_marker )
!
!  Print some of the data.
!
  call r8mat_transpose_print_some ( node_dim, node_num, node_coord, &
    1, 1, node_dim, 10, '  Coordinates for first 10 nodes:' )

  call r8mat_transpose_print_some ( node_att_num, node_num, node_att, &
    1, 1, node_att_num, 10, '  Attributes for first 10 nodes:' )

  call i4mat_transpose_print_some ( node_marker_num, node_num, node_marker, &
    1, 1, node_marker_num, 10, '  Markers for first 10 nodes:' ) 
!
!  Clean up.
!
  deallocate ( node_att )
  deallocate ( node_coord )
  deallocate ( node_marker )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 reads the example element data from a file.
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

  real ( kind = 8 ), allocatable :: element_att(:,:)
  integer ( kind = 4 ) element_att_num
  character ( len = 255 ) element_filename
  integer ( kind = 4 ), allocatable :: element_node(:,:)
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order

  element_filename = 'example.ele'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST04:'
  write ( *, '(a)' ) '  Read element data from an element file.'
!
!  Get data size.
!
  call triangle_element_size_read ( element_filename, element_num, &
    element_order, element_att_num )
!
!  Print the sizes.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Element data read from file "' // trim ( element_filename ) // '"'
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of elements = ', element_num
  write ( *, '(a,i4)' ) '  Element order = ', element_order
  write ( *, '(a,i4)' ) '  Number of element attributes = ', element_att_num
!
!  Allocate memory.
!
  allocate ( element_node(1:element_order,1:element_num) )
  allocate ( element_att(1:element_att_num,1:element_num ) )
!
!  Get the data.
!
  call triangle_element_data_read ( element_filename, element_num, &
    element_order, element_att_num, element_node, element_att )
!
!  Print some of the data.
!
  call i4mat_transpose_print_some ( element_order, element_num, element_node, &
    1, 1, element_order, 10, '  Connectivity for first 10 elements:' )

  call r8mat_transpose_print_some ( element_att_num, element_num, element_att, &
    1, 1, element_att_num, 10, '  Attributes for first 10 elements:' )
!
!  Clean up.
!
  deallocate ( element_att )
  deallocate ( element_node )

  return
end
