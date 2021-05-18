program main

!*****************************************************************************80
!
!! MAIN is the main program for SPHERE_CUBED_GRID_TEST.
!
!  Discussion:
!
!    SPHERE_CUBED_GRID_TEST tests the SPHERE_CUBED_GRID library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_CUBED_GRID_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SPHERE_CUBED_GRID library.'

  call sphere_cubed_grid_point_count_test ( )
  call sphere_cubed_grid_points_test ( )
  call sphere_cubed_grid_points_face_test ( )
  call sphere_cubed_grid_points_display_test ( )

  call sphere_cubed_grid_ijk_to_xyz_test ( )
  call sphere_cubed_grid_line_count_test ( )
  call sphere_cubed_grid_lines_test ( )
  call sphere_cubed_grid_lines_display_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_CUBED_GRID_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine sphere_cubed_grid_point_count_test ( )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_POINT_COUNT_TEST tests SPHERE_CUBED_GRID_POINT_COUNT_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) point_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_CUBED_GRID_POINT_COUNT_TEST'
  write ( *, '(a)' ) '  SPHERE_CUBED_GRID_POINT_COUNT counts points on a cubed sphere grid.'
  write ( *, '(a)' ) '  Each square face is divided into NxN subfaces,'
  write ( *, '(a)' ) '  and there are 6 faces.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        N      POINT_COUNT'
  write ( *, '(a)' ) ''
  do n = 1, 10
    call sphere_cubed_grid_point_count ( n, point_num )
    write ( *, '(2x,i8,2x,i8)' ) n, point_num
  end do

  return
end
subroutine sphere_cubed_grid_points_test ( )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_POINTS_TEST tests SPHERE_CUBED_GRID_POINTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  real ( kind = 8 ), allocatable :: xyz(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_CUBED_GRID_POINTS_TEST'
  write ( *, '(a)' ) '  SPHERE_CUBED_GRID_POINTS computes points on a cubed sphere grid.'

  n = 10
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of divisions on each face = ', n

  call sphere_cubed_grid_point_count ( n, ns )
  write ( *, '(a,i4)' ) '  Total number of points = ', ns

  allocate ( xyz(1:3,1:ns) )
  call sphere_cubed_grid_points ( n, ns, xyz )

  call r8mat_transpose_print_some ( 3, ns, xyz, 1, 1, 3, 20, &
    '  Initial part of XYZ array:' )

  deallocate ( xyz )

  return
end
subroutine sphere_cubed_grid_points_face_test ( )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_POINTS_FACE_TEST tests SPHERE_CUBED_GRID_POINTS_FACE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) k2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  integer ( kind = 4 ) ns2
  real ( kind = 8 ), allocatable :: xyz(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_CUBED_GRID_POINTS_FACE_TEST'
  write ( *, '(a)' ) '  SPHERE_CUBED_GRID_POINTS_FACE computes points associated'
  write ( *, '(a)' ) '  with one face of a cubed sphere grid.'

  n = 3
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of divisions on each face = ', n

  call sphere_cubed_grid_point_count ( n, ns )
  write ( *, '(a,i4)' ) '  Total number of points = ', ns

  allocate ( xyz(1:3,1:ns) )

  ns2 = 0
  i1 = 0
  j1 = 0
  k1 = 0
  i2 = n
  j2 = n
  k2 = 0
!
!  Bottom face.
!
  call sphere_cubed_grid_points_face ( n, i1, j1, k1, i2, j2, k2, ns2, xyz ) 

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Current number of points = ', ns2

  call r8mat_transpose_print ( ns2, 3, xyz,'  XYZ array after call for bottom face:' )
!
!  Compute one more face, but skip points already generated.
!
  i1 = 0
  j1 = 0
  k1 = 1
  i2 = 0
  j2 = n - 1
  k2 = n - 1

  call sphere_cubed_grid_points_face ( n, i1, j1, k1, i2, j2, k2, ns2, xyz )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Current number of points = ', ns2

  call r8mat_transpose_print ( ns2, 3, xyz,'  XYZ array after call for a side face face:' )

  deallocate ( xyz )

  return
end
subroutine sphere_cubed_grid_points_display_test ( )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_POINTS_DISPLAY_TEST tests SPHERE_CUBED_GRID_POINTS_DISPLAY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  character ( len = 255 ) prefix
  real ( kind = 8 ), allocatable :: xyz(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_CUBED_GRID_POINTS_DISPLAY_TEST'
  write ( *, '(a)' ) '  SPHERE_CUBED_GRID_POINTS_DISPLAY_TEST displays points'
  write ( *, '(a)' ) '  on a cubed sphere grid.'

  n = 10
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of divisions on each face = ', n

  call sphere_cubed_grid_point_count ( n, ns )
  write ( *, '(a,i4)' ) '  Total number of points = ', ns

  allocate ( xyz(1:3,1:ns) )

  call sphere_cubed_grid_points ( n, ns, xyz )

  prefix = 'sphere_cubed_grid_points'

  call sphere_cubed_grid_points_display ( ns, xyz, prefix )

  deallocate ( xyz )

  return
end
subroutine sphere_cubed_grid_ijk_to_xyz_test ( )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_IJK_TO_XYZ_TEST tests SPHERE_CUBED_GRID_IJK_TO_XYZ.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ) xyz(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_CUBED_GRID_IJK_TO_XYZ_TEST:'
  write ( *, '(a)' ) '  SPHERE_CUBED_GRID_IJK_TO_XYZ returns the XYZ coordinates'
  write ( *, '(a)' ) '  of a point on the surface of the cubed sphere,'
  write ( *, '(a)' ) '  given its (I,J,K) indices.' 

  n = 3

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Using grid parameter N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I     J     K        X           Y           Z'
  write ( *, '(a)' ) ''

  do i = 0, n
    do j = 0, n
      do k = 0, n
        if ( i == 0 .or. i == n .or. &
             j == 0 .or. j == n .or. &
             k == 0 .or. k == n ) then
          call sphere_cubed_grid_ijk_to_xyz ( n, i, j, k, xyz )
          write ( *, '(2x,i4,2x,i4,2x,i4,2x,f10.4,2x,f10.4,2x,f10.4)' ) &
            i, j, k, xyz(1), xyz(2), xyz(3)
        end if
      end do
    end do
  end do

  return
end
subroutine sphere_cubed_grid_line_count_test ( )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_LINE_COUNT_TEST tests SPHERE_CUBED_GRID_LINE_COUNT_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) line_count
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_CUBED_GRID_LINE_COUNT_TEST'
  write ( *, '(a)' ) '  SPHERE_CUBED_GRID_LINE_COUNT counts lines on a cubed sphere grid.'
  write ( *, '(a)' ) '  Each square face is divided into NxN subfaces,'
  write ( *, '(a)' ) '  and there are 6 faces.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        N      LINE_COUNT'
  write ( *, '(a)' ) ''
  do n = 1, 10
    call sphere_cubed_grid_line_count ( n, line_count )
    write ( *, '(2x,i8,2x,i8)' ) n, line_count
  end do

  return
end
subroutine sphere_cubed_grid_lines_test ( )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_LINES_TEST tests SPHERE_CUBED_GRID_LINES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ), allocatable :: line_data(:,:,:)
  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) n
  integer ( kind = 4 ) point_num
  real ( kind = 8 ), allocatable :: xyz(:,:)

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_CUBED_GRID_LINES_TEST'
  write ( *, '(a)' ) '  SPHERE_CUBED_GRID_LINES defines the lines'
  write ( *, '(a)' ) '  on a cubed sphere grid.'
  write ( *, '(a,i2,a,i2,a)' ) '  Each cube face is divided into ', n, ' by ', n, ' subfaces'

  call sphere_cubed_grid_point_count ( n, point_num )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The number of points is ', point_num

  allocate ( xyz(1:3,1:point_num) )

  call sphere_cubed_grid_points ( n, point_num, xyz )

  call sphere_cubed_grid_line_count ( n, line_num )

  write ( *, '(a,i4)' ) '  The number of grid lines is ', line_num

  allocate ( line_data(1:3,1:2,1:line_num) )

  call sphere_cubed_grid_lines ( n, line_num, line_data )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Line     Start              End'
  write ( *, '(a)' ) '  Index    X    Y   Z         X  Y   Z'
  write ( *, '(a)' ) ''

  do i = 1, min ( 10, line_num )
    write ( *, '(a)' ) ''
    write ( *, '(2x,i4,2x,f10.4,2x,f10.4,2x,f10.4,2x,f10.4,2x,f10.4,2x,f10.4)' ) &
      i, line_data(1:3,1,i), line_data(1:3,2,i)
  end do

  deallocate ( line_data )
  deallocate ( xyz )

  return
end
subroutine sphere_cubed_grid_lines_display_test ( )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_LINES_DISPLAY_TEST tests SPHERE_CUBED_GRID_LINES_DISPLAY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: line_data(:,:,:)
  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) n
  character ( len = 255 ) prefix

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_CUBED_GRID_LINES_DISPLAY_TEST'
  write ( *, '(a)' ) '  SPHERE_CUBED_GRID_LINES_DISPLAY displays the lines'
  write ( *, '(a)' ) '  on a cubed sphere grid.'
  write ( *, '(a,i2,a,i2,a)' ) '  Each cube face is divided into ', n, ' by ', n, ' subfaces'

  call sphere_cubed_grid_line_count ( n, line_num )

  write ( *, '(a,i4)' ) '  The number of grid lines is ', line_num

  allocate ( line_data(1:3,1:2,1:line_num) )

  call sphere_cubed_grid_lines ( n, line_num, line_data )

  prefix = 'sphere_cubed_grid_lines'

  call sphere_cubed_grid_lines_display ( line_num, line_data, prefix )

  deallocate ( line_data )

  return
end
