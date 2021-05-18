program main

!*****************************************************************************80
!
!! MAIN is the main program for WEDGE_GRID_TEST.
!
!  Discussion:
!
!    WEDGE_GRID_TEST tests the WEDGE_GRID library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WEDGE_GRID_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the WEDGE_GRID library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WEDGE_GRID_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests WEDGE_GRID.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ), allocatable :: g(:,:)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) ng
  character ( len = 255 ) output_filename
  integer ( kind = 4 ) output_unit

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  WEDGE_GRID can define a grid of points'
  write ( *, '(a)' ) '  with N+1 points on a side'
  write ( *, '(a)' ) '  over the interior of the unit wedge in 3D.'

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Grid order N = ', n

  call wedge_grid_size ( n, ng )

  write ( *, '(a,i6)' ) '  Grid count NG = ', ng

  allocate ( g(1:3,1:ng) )

  call wedge_grid ( n, ng, g )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     J      X                Y               Z'
  write ( *, '(a)' ) ' '
  do j = 1, ng
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) j, g(1:3,j)
  end do

  output_filename = 'wedge_grid.xy'

  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_filename, status = 'replace' )
  do j = 1, ng
    write ( output_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) g(1:3,j)
  end do
  close ( unit = output_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Data written to "' // trim ( output_filename ) // '".'

  deallocate ( g )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests WEDGE_GRID_PLOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ), allocatable :: g(:,:)
  character ( len = 255 ) header
  integer ( kind = 4 ) ng

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  WEDGE_GRID_PLOT can create GNUPLOT data files'
  write ( *, '(a)' ) '  for displaying a wedge grid.'

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Grid order N = ', n

  call wedge_grid_size ( n, ng )

  write ( *, '(a,i6)' ) '  Grid count NG = ', ng

  allocate ( g(1:3,1:ng) )

  call wedge_grid ( n, ng, g )

  header = 'wedge'

  call wedge_grid_plot ( n, ng, g, header )

  deallocate ( g )

  return
end

