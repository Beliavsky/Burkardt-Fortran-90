program main

!*****************************************************************************80
!
!! MAIN is the main program for DISK_GRID_TEST.
!
!  Discussion:
!
!    DISK_GRID_TEST tests the DISK_GRID library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DISK_GRID_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the DISK_GRID library.'

  call disk_grid_test01 ( )
  call disk_grid_test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DISK_GRID_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine disk_grid_test01 ( )

!*****************************************************************************80
!
!! DISK_GRID_TEST01 tests DISK_GRID.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) c(2)
  real ( kind = 8 ), allocatable :: cg(:,:)
  character ( len = 80 ) boundary_filename
  integer ( kind = 4 ) boundary_unit
  character ( len = 80 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 80 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = 80 ) filename
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ng
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) t

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  DISK_GRID can define a grid of points'
  write ( *, '(a)' ) '  with N+1 points on a horizontal or vertical radius,'
  write ( *, '(a)' ) '  based on any disk.'

  n = 20
  r = 2.0D+00
  c(1) = 1.0D+00
  c(2) = 5.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  We use N = ', n
  write ( *, '(a,g14.6)' ) '  Radius R = ', r
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  Center C = (', c(1), ',', c(2), ')'

  call disk_grid_count ( n, r, c, ng )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of grid points will be ', ng

  allocate ( cg(1:2,1:ng) )

  call disk_grid ( n, r, c, ng, cg )

  call r82vec_print_part ( ng, cg, 20, '  Part of the grid point array:' )
!
!  Write the coordinate data to file.
!
  filename = 'disk_grid_test01.xy'

  call r8mat_write ( filename, 2, ng, cg )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Data written to the file "' // trim ( filename ) // '".'
!
!  Create graphics data files.
!
  call get_unit ( boundary_unit )
  boundary_filename = 'disk_grid_test01_boundary.txt'
  open ( unit = boundary_unit, file = boundary_filename, status = 'replace' )
  do i = 0, 50
    t = 2.0D+00 * pi * real ( i, kind = 8 ) / 50.0D+00
    write ( boundary_unit, '(2x,g14.6,2x,g14.6)' ) &
      c(1) + r * cos ( t ), c(2) + r * sin ( t )
  end do
  close ( unit = boundary_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created boundary file "' // trim ( boundary_filename ) // '".'

  call get_unit ( data_unit )
  data_filename = 'disk_grid_test01_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, ng
    write ( data_unit, '(2x,g14.6,2x,g14.6)' ) cg(1:2,i)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'
!
!  Create graphics command file.
!
  call get_unit ( command_unit )
  command_filename = 'disk_grid_test01_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "disk_grid_test01.png"'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set title "Disk Grid"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set key off'
  write ( command_unit, '(a)' ) 'set size ratio -1'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 with points lt 3 pt 3,\'
  write ( command_unit, '(a)' ) '    "' // trim ( boundary_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "black"'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  deallocate ( cg )

  return
end
subroutine disk_grid_test02 ( )

!*****************************************************************************80
!
!! DISK_GRID_TEST02 tests DISK_GRID_FIBONACCI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) c(2)
  character ( len = 80 ) boundary_filename
  integer ( kind = 4 ) boundary_unit
  character ( len = 80 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 80 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = 80 ) filename
  real ( kind = 8 ), allocatable :: g(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) t

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  DISK_GRID_FIBONACCI can define a grid of N points'
  write ( *, '(a)' ) '  based on a Fibonacci spiral inside a disk.'

  n = 1000
  r = 2.0D+00
  c(1) = 1.0D+00
  c(2) = 5.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  We use N = ', n
  write ( *, '(a,g14.6)' ) '  Radius R = ', r
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  Center C = (', c(1), ',', c(2), ')'

  allocate ( g(1:2,1:n) )

  call disk_grid_fibonacci ( n, r, c, g )

  call r82vec_print_part ( n, g, 20, '  Part of the grid point array:' )
!
!  Write the coordinate data to a file.
!
  filename = 'disk_grid_test02.xy'

  call r8mat_write ( filename, 2, n, g )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Coordinate data written to the file "' // trim ( filename ) // '".'
!
!  Create graphics data files.
!
  call get_unit ( boundary_unit )
  boundary_filename = 'disk_grid_test02_boundary.txt'
  open ( unit = boundary_unit, file = boundary_filename, status = 'replace' )
  do i = 0, 50
    t = 2.0D+00 * pi * real ( i, kind = 8 ) / 50.0D+00
    write ( boundary_unit, '(2x,g14.6,2x,g14.6)' ) &
      c(1) + r * cos ( t ), c(2) + r * sin ( t )
  end do
  close ( unit = boundary_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created boundary file "' // trim ( boundary_filename ) // '".'

  call get_unit ( data_unit )
  data_filename = 'disk_grid_test02_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, n
    write ( data_unit, '(2x,g14.6,2x,g14.6)' ) g(1:2,i)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'
!
!  Create graphics command file.
!
  call get_unit ( command_unit )
  command_filename = 'disk_grid_test02_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "disk_grid_test02.png"'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set title "Fibonacci Grid"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set key off'
  write ( command_unit, '(a)' ) 'set size ratio -1'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 with points lt 3 pt 3,\'
  write ( command_unit, '(a)' ) '    "' // trim ( boundary_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "black"'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  deallocate ( g )

  return
end
