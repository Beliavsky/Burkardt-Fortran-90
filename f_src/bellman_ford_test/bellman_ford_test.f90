program main

!*****************************************************************************80
!
!! MAIN is the main program for BELLMAN_FORD_TEST.
!
!  Discussion:
!
!    BELLMAN_FORD_TEST tests the BELLMAN_FORD library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BELLMAN_FORD_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BELLMAN_FORD library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BELLMAN_FORD_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 runs a simple test.
!
!  Discussion:
!
!    The correct distances are { 0, -6, -2, 3, 0, 0 }.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: e_num = 10
  integer ( kind = 4 ), parameter :: v_num = 6

  integer ( kind = 4 ), dimension (2,e_num) :: e = reshape ( (/ &
    2, 1, &
    5, 2, &
    2, 3, &
    3, 5, &
    5, 1, &
    3, 6, &
    6, 1, &
    4, 3, &
    6, 4, &
    4, 1 /), (/ 2, e_num /) )
  real ( kind = 8 ), dimension ( e_num ) :: e_weight = (/ &
    -3.0, &
     6.0, &
    -4.0, &
    -1.0, &
     4.0, &
    -2.0, &
     2.0, &
     8.0, &
    -3.0, &
     3.0 /)
  integer ( kind = 4 ) predecessor(v_num)
  integer ( kind = 4 ) source
  real ( kind = 8 ) v_weight(v_num)
  
  source = 1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  Bellman-Ford shortest path algorithm.'

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of vertices = ', v_num
  write ( *, '(a,i4)' ) '  Number of edges = ', e_num
  write ( *, '(a,i4)' ) '  The reference vertex is ', source

  call i4mat_transpose_print ( 2, e_num, e, '  The edge array:' )
  call r8vec_print ( e_num, e_weight, '  The edge weights:' )

  call bellman_ford ( v_num, e_num, source, e, e_weight, v_weight, &
    predecessor )

  call r8vec_print ( v_num, v_weight, "  The shortest distances:" )

  call i4vec_print ( v_num, predecessor, &
    '  The vertex predecessor parents for the shortest paths:' )

  return
end
